--  Glib.Object tests for object identity, generic user data, signals,
--  properties, references, and weak notifications. There is no focused C
--  GObject test to port; these cases exercise the corresponding Ada API.

with Ada.Command_Line;
with Interfaces.C;
with Interfaces.C.Strings;
with System;

with Glib;                   use Glib;
with Glib.Object;            use Glib.Object;
with Glib.Test;              use Glib.Test;
with Glib.Types;
with Gtk.Buildable;
with Gtk.Main;
with Gtk.Orientable;
with Gtk.Switch;
with Gtk.Widget;
with Gtkada.Types;

procedure Main is

   use type Glib.Object.GObject;
   use type Glib.GType;
   use type Glib.GQuark;
   use type Glib.Signal_Id;
   use type System.Address;

   procedure Destroy_Closure_Data (Data : in out Integer);
   package Integer_Data is new Glib.Object.User_Data (Integer);
   package Integer_Closure is new Glib.Object.User_Data_Closure
     (Integer, Destroy_Closure_Data);

   --  The callbacks record observable cleanup without depending on timing.
   Destroyed_Count : Natural := 0;
   Destroyed_Value : Integer := 0;
   Weak_Count      : Natural := 0;
   Notify_Count    : Natural := 0;
   Closure_Count   : Natural := 0;

   procedure On_Data_Destroyed (Data : Integer);
   procedure On_Weak_Notify
     (Data : System.Address; Where_The_Object_Was : System.Address);
   pragma Convention (C, On_Weak_Notify);
   procedure On_Notify
     (Object : System.Address; Spec : System.Address; Data : System.Address);
   pragma Convention (C, On_Notify);

   function New_C_Object (Typ : GType) return System.Address;
   pragma Import (C, New_C_Object, "ada_g_object_new");

   function Signal_Connect
     (Object        : System.Address;
      Detailed_Name : Interfaces.C.Strings.chars_ptr;
      Handler       : System.Address;
      Data          : System.Address;
      Destroy       : System.Address;
      Flags         : Interfaces.C.int) return Glib.Gulong;
   pragma Import (C, Signal_Connect, "g_signal_connect_data");

   Test_Class : aliased Ada_GObject_Class := Uninitialized_Class;
   --  A private runtime type gives signal introspection a stable test signal.
   Empty_Class : aliased Ada_GObject_Class := Uninitialized_Class;
   --  A second runtime type verifies empty signal lists on valid object types.
   Signal_Names : Interfaces.C.Strings.chars_ptr_array (1 .. 1);
   Signal_Parameters : Signal_Parameter_Types (1 .. 1, 1 .. 1) :=
     (1 => (1 => GType_Int));
   Signal_Returns : Signal_Return_Types (1 .. 1) := (1 => GType_None);

   -----------------------
   -- On_Data_Destroyed --
   -----------------------

   procedure On_Data_Destroyed (Data : Integer) is
   begin
      Destroyed_Count := Destroyed_Count + 1;
      Destroyed_Value := Data;
   end On_Data_Destroyed;

   -------------------
   -- On_Weak_Notify --
   -------------------

   procedure On_Weak_Notify
     (Data : System.Address; Where_The_Object_Was : System.Address) is
   begin
      --  A live object address proves this callback ran at final unref.
      if Where_The_Object_Was /= System.Null_Address then
         Weak_Count := Weak_Count + 1;
      end if;
   end On_Weak_Notify;

   ----------------
   -- On_Notify --
   ----------------

   procedure On_Notify
     (Object : System.Address; Spec : System.Address; Data : System.Address) is
   begin
      --  The signal callback counts a property notification emitted by Notify.
      if Object /= System.Null_Address and then Spec /= System.Null_Address then
         Notify_Count := Notify_Count + 1;
      end if;
   end On_Notify;

   --------------------------
   -- Destroy_Closure_Data --
   --------------------------

   procedure Destroy_Closure_Data (Data : in out Integer) is
   begin
      Closure_Count := Closure_Count + 1;
      Data := 0;
   end Destroy_Closure_Data;

   --------------------
   -- Test_Conversion --
   --------------------

   procedure Test_Conversion with Convention => C;
   procedure Test_Conversion is
      Raw  : constant System.Address := New_C_Object (GType_Object);
      Fast_Raw : constant System.Address := New_C_Object (GType_Object);
      Stub : GObject_Record;
      Ada_Object : GObject;
      Fast_Object : GObject;
   begin
      --  A C-created object has no GtkAda wrapper until conversion is asked.
      Assert_True (Get_User_Data_Or_Null (System.Null_Address) = null);
      Assert_True (Get_User_Data_Or_Null (Raw) = null);
      Assert_True (Get_User_Data (System.Null_Address, Stub) = null);

      --  Run the fast fallback first so its one-time GtkAda quark setup is tested.
      Assert_True (Get_User_Data_Fast (System.Null_Address, Stub) = null);
      Fast_Object := Get_User_Data_Fast (Fast_Raw, Stub);
      Assert_True (Fast_Object /= null and then Is_Created (Fast_Object.all));
      Unref (Fast_Object);

      Ada_Object := Get_User_Data (Raw, Stub);
      Assert_True (Ada_Object /= null and then Is_Created (Ada_Object.all));
      Assert_True (Get_User_Data_Or_Null (Raw) = Ada_Object);
      Assert_True (Get_User_Data (Raw, Stub) = Ada_Object);
      Assert_True (Get_Type (Ada_Object) = GType_Object);

      --  The object owns its Ada wrapper; final unref frees both together.
      Unref (Ada_Object);
   end Test_Conversion;

   -------------------
   -- Test_Fast_Cast --
   -------------------

   procedure Test_Fast_Cast with Convention => C;
   procedure Test_Fast_Cast is
      Raw  : constant System.Address := New_C_Object (GType_Object);
      Stub : GObject_Record;
      Ada_Object : GObject;
   begin
      --  Fast conversion creates the requested stub and caches it on the C object.
      Assert_True (Get_User_Data_Fast (System.Null_Address, Stub) = null);
      Ada_Object := Get_User_Data_Fast (Raw, Stub);
      Assert_True (Ada_Object /= null and then Is_Created (Ada_Object.all));
      Assert_True (Get_User_Data_Fast (Raw, Stub) = Ada_Object);
      Unref (Ada_Object);
   end Test_Fast_Cast;

   ------------------
   -- Test_Lifecycle --
   ------------------

   procedure Test_Lifecycle with Convention => C;
   procedure Test_Lifecycle is
      Object : GObject := new GObject_Record;
      No_Object : GObject := null;
   begin
      --  Initialize creates the base GObject; Ref and Ref_Sink balance extra refs.
      Assert_True (not Is_Created (Object.all));
      Assert_True (Convert (No_Object) = System.Null_Address);
      Assert_True (Get_Object_Or_Null (null) = System.Null_Address);
      Initialize (Object.all'Access);
      Assert_True (Is_Created (Object.all));
      Assert_True (Get_Object (Object) /= System.Null_Address);
      Assert_True (Get_Object_Or_Null (Object) = Get_Object (Object));
      Assert_True (Get_Type (Object) = GType_Object);

      Ref (Object);
      Unref (Object);
      Ref_Sink (Object);
      Unref (Object);
      Unref (Object);
      Object := null;
   end Test_Lifecycle;

   -----------------
   -- Test_User_Data --
   -----------------

   procedure Test_User_Data with Convention => C;
   procedure Test_User_Data is
      Object : GObject := new GObject_Record;
      Key     : constant Glib.GQuark := Quark_From_String ("gtkada-test-data");
      Caught  : Boolean := False;
      Quark_Caught : Boolean := False;
   begin
      --  Use the generic package with both string keys and quark keys.
      Initialize (Object.all'Access);
      Destroyed_Count := 0;
      Assert_True (not Integer_Data.Is_Set (Object));
      Assert_True (Integer_Data.Get (Object, "missing", 17) = 17);
      begin
         declare
            Ignored : constant Integer := Integer_Data.Get (Object, "missing");
            pragma Unreferenced (Ignored);
         begin
            null;
         end;
      exception
         when Gtkada.Types.Data_Error =>
            Caught := True;
      end;
      Assert_True (Caught);

      Integer_Data.Set
        (Object, 21, "string-key", On_Data_Destroyed'Access);
      Assert_True (Integer_Data.Is_Set (Object, "string-key"));
      Assert_True (Integer_Data.Get (Object, "string-key") = 21);
      Assert_True (Integer_Data.Get (Object, "string-key", 99) = 21);
      Integer_Data.Set
        (Object, 22, "string-key", On_Data_Destroyed'Access);
      Assert_True (Destroyed_Count = 1 and then Destroyed_Value = 21);
      Assert_True (Integer_Data.Get (Object, "string-key") = 22);
      Integer_Data.Remove (Object, "string-key");
      Assert_True (not Integer_Data.Is_Set (Object, "string-key"));
      Assert_True (Destroyed_Count = 2 and then Destroyed_Value = 22);

      --  Quark overloads exercise the qdata path and its default/missing cases.
      Assert_True (Integer_Data.Get (Object, Key, 31) = 31);
      begin
         declare
            Ignored : constant Integer := Integer_Data.Get (Object, Key);
            pragma Unreferenced (Ignored);
         begin
            null;
         end;
      exception
         when Gtkada.Types.Data_Error =>
            Quark_Caught := True;
      end;
      Assert_True (Quark_Caught);
      Integer_Data.Set (Object, 32, Key, On_Data_Destroyed'Access);
      Assert_True (Integer_Data.Get (Object, Key) = 32);
      Assert_True (Integer_Data.Get (Object, Key, 99) = 32);
      Integer_Data.Remove (Object, Key);
      Assert_True (Integer_Data.Get (Object, Key, 33) = 33);
      Assert_True (Destroyed_Count = 3 and then Destroyed_Value = 32);
      Unref (Object);
      Object := null;
   end Test_User_Data;

   ------------------
   -- Test_Weak_Refs --
   ------------------

   procedure Test_Weak_Refs with Convention => C;
   procedure Test_Weak_Refs is
      Object : GObject := new GObject_Record;
   begin
      --  Removing a weak notification suppresses it; final unref invokes one.
      Weak_Count := 0;
      Initialize (Object.all'Access);
      Weak_Ref (Object, On_Weak_Notify'Unrestricted_Access);
      Weak_Unref (Object, On_Weak_Notify'Unrestricted_Access);
      Unref (Object);
      Object := null;
      Assert_True (Weak_Count = 0);

      Object := new GObject_Record;
      Initialize (Object.all'Access);
      Weak_Ref (Object, On_Weak_Notify'Unrestricted_Access);
      Unref (Object);
      Object := null;
      Assert_True (Weak_Count = 1);
   end Test_Weak_Refs;

   -------------------
   -- Test_Closure --
   -------------------

   procedure Test_Closure with Convention => C;
   procedure Test_Closure is
      Data_Address : constant System.Address :=
        Integer_Closure.Build (System.Null_Address, 41);
      Internal : constant Integer_Closure.Internal_Data_Access :=
        Integer_Closure.Convert (Data_Address);
      use type Integer_Closure.Internal_Data_Access;
   begin
      --  Build returns C-compatible closure storage; Free_Data owns its cleanup.
      Closure_Count := 0;
      Assert_True (Internal /= null and then Internal.Data.all = 41);
      Integer_Closure.Free_Data (Data_Address);
      Assert_True (Closure_Count = 1);
   end Test_Closure;

   ----------------------
   -- Test_Signal_Query --
   ----------------------

   procedure Test_Signal_Query with Convention => C;
   procedure Test_Signal_Query is
      Created : Boolean;
      Empty_Created : Boolean;
      Object  : GObject := new GObject_Record;
      Info    : constant GInterface_Info_Access := new GInterface_Info;
      Id      : Glib.Signal_Id;
      Description : Signal_Query;
   begin
      --  Register an object type with no signals so GLib returns a null array.
      Empty_Created := Initialize_Class_Record
        (Ancestor => GType_Object,
         Class_Record => Empty_Class'Access,
         Type_Name => "GtkAdaCoverageEmptySignals");
      Assert_True (Empty_Created);
      declare
         No_Signal_Ids : constant Signal_Id_Array :=
           List_Ids (Empty_Class.The_Type);
      begin
         --  The empty result exercises Gtkada.C's null-array conversion.
         Assert_True (No_Signal_Ids'Length = 0);
      end;

      --  Register a signal with one integer parameter and inspect its metadata.
      Signal_Names (1) := Interfaces.C.Strings.New_String ("coverage-ping");
      Created := Initialize_Class_Record
        (Ancestor => GType_Object,
         Class_Record => Test_Class'Access,
         Type_Name => "GtkAdaCoverageGlibObject",
         Signals => Signal_Names,
         Parameters => Signal_Parameters,
         Returns => Signal_Returns);
      Interfaces.C.Strings.Free (Signal_Names (1));
      Assert_True (Created);
      Assert_True
        (not Initialize_Class_Record
           (Ancestor => GType_Object,
            Class_Record => Test_Class'Access,
            Type_Name => "GtkAdaCoverageGlibObject",
            Signals => No_Signals));
      --  GtkBuildable has no interface properties to implement on this type.
      Add_Interface (Test_Class, Gtk.Buildable.Get_Type, Info);
      G_New (Object.all'Access, Test_Class);
      Assert_True (Get_Type (Object) = Test_Class.The_Type);
      Assert_True (Glib.Types.Is_A (Test_Class.The_Type, Gtk.Buildable.Get_Type));
      Id := Lookup (Test_Class.The_Type, "coverage-ping");
      Assert_True (Id /= Null_Signal_Id);
      Glib.Object.Query (Id, Description);
      Assert_True (Glib.Object.Id (Description) = Id);
      Assert_True (Glib.Object.Signal_Name (Description) = "coverage-ping");
      Assert_True (Return_Type (Description) = GType_None);
      declare
         Params : constant GType_Array := Glib.Object.Params (Description);
         Ids    : constant Signal_Id_Array := List_Ids (Test_Class.The_Type);
      begin
         Assert_True
           (Params'Length = 1 and then Params (Params'First) = GType_Int);
         Assert_True (Ids'Length = 1 and then Ids (Ids'First) = Id);
      end;
      Assert_True (Lookup (Test_Class.The_Type, "missing-signal") = Null_Signal_Id);
      Unref (Object);
      Object := null;
   end Test_Signal_Query;

   ------------------------
   -- Test_Introspection --
   ------------------------

   procedure Test_Introspection with Convention => C;
   procedure Test_Introspection is
      Class : constant GObject_Class := Glib.Types.Class_Ref (Gtk.Switch.Get_Type);
      Object_Properties : constant Param_Spec_Array :=
        Class_List_Properties (Class);
      Vtable : constant Interface_Vtable :=
        Glib.Types.Default_Interface_Ref (Gtk.Orientable.Get_Type);
      Interface_Properties : constant Param_Spec_Array :=
        Interface_List_Properties (Vtable);
   begin
      --  GtkSwitch and GtkOrientable expose stable inherited/interface properties.
      Assert_True (Object_Properties'Length > 0);
      Assert_True (Interface_Properties'Length > 0);
      Glib.Types.Class_Unref (Gtk.Switch.Get_Type);
   end Test_Introspection;

   ---------------------
   -- Test_Bind_Notify --
   ---------------------

   procedure Test_Bind_Notify with Convention => C;
   procedure Test_Bind_Notify is
      Source : Gtk.Switch.Gtk_Switch;
      Target : Gtk.Switch.Gtk_Switch;
      Binding : G_Binding;
      Name : Interfaces.C.Strings.chars_ptr;
   begin
      --  Gtk widgets need GTK initialized even though the test never displays them.
      Gtk.Main.Init;
      Gtk.Switch.Gtk_New (Source);
      Gtk.Switch.Gtk_New (Target);
      Ref_Sink (GObject_Record (Source.all)'Access);
      Ref_Sink (GObject_Record (Target.all)'Access);
      Gtk.Switch.Set_Active (Source, False);
      Gtk.Switch.Set_Active (Target, True);

      --  Sync_Create copies the source value and later changes follow the binding.
      Binding := Glib.Object.Bind_Property
        (Source, "active", Target, "active", Binding_Sync_Create);
      Assert_True (Binding /= null and then not Gtk.Switch.Get_Active (Target));
      Gtk.Switch.Set_Active (Source, True);
      Assert_True (Gtk.Switch.Get_Active (Target));

      --  Observe the notify signal so Notify's explicit emission has a checked effect.
      Name := Interfaces.C.Strings.New_String ("notify::active");
      Notify_Count := 0;
      declare
         Connected : constant Glib.Gulong := Signal_Connect
           (Get_Object (Source), Name, On_Notify'Address,
            System.Null_Address, System.Null_Address, 0);
      begin
         Interfaces.C.Strings.Free (Name);
         Assert_True (Connected /= 0);
      end;
      Notify (GObject_Record (Source.all)'Access, "active");
      Assert_True (Notify_Count = 1);

      Unbind (Binding);
      Binding := null;
      Gtk.Switch.Set_Active (Source, False);
      Assert_True (Gtk.Switch.Get_Active (Target));

      --  Exercise the procedure overload; the binding follows the same property.
      Glib.Object.Bind_Property (Source, "active", Target, "active");
      Gtk.Switch.Set_Active (Source, True);
      Assert_True (Gtk.Switch.Get_Active (Target));
      Unref (GObject (Source));
      Unref (GObject (Target));
   end Test_Bind_Notify;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/glib-object/conversion", Test_Conversion'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib-object/fast-cast", Test_Fast_Cast'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib-object/lifecycle", Test_Lifecycle'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib-object/user-data", Test_User_Data'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib-object/weak-refs", Test_Weak_Refs'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib-object/closure", Test_Closure'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib-object/signal-query", Test_Signal_Query'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib-object/introspection", Test_Introspection'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib-object/bind-notify", Test_Bind_Notify'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
