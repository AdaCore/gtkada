--  Tests the typed and slot-based GtkATContext state-change signal adapters.
--  Gtk has no C test for GtkATContext's Ada signal wrappers, so this focused
--  test constructs a context and emits its signal directly, like label.adb.

with Ada.Command_Line;
with Glib;
with Glib.Test;
with Glib.Object;
with Gtk.Atcontext;
with Gtk.Label; use Gtk.Label;
with Gtk.Main;
with Gtkada.Types;
with System;

procedure Atcontext_Test is
   use type System.Address;
   use type Glib.GType;
   use type Gtk.Atcontext.Gtk_Atcontext;

   Typed_Count : Natural := 0;
   Slot_Count  : Natural := 0;
   Callback_Order : Natural := 0;
   Typed_Order : Natural := 0;
   Slot_Order  : Natural := 0;
   Typed_Object : System.Address := System.Null_Address;
   Slot_Object  : System.Address := System.Null_Address;

   --  GtkATContext is abstract. Register a minimal concrete subtype using
   --  its queried C sizes so its inherited state-change signal can be tested.
   --  Keep the C field order, including the type-name pointer, for g_type_query.
   type GType_Query is record
      Type_Id      : Glib.GType;
      Type_Name    : Gtkada.Types.Chars_Ptr;
      Class_Size   : Glib.Guint;
      Instance_Size : Glib.Guint;
   end record
   with Convention => C;

   procedure Query_Type
     (Type_Id : Glib.GType;
      Query   : access GType_Query)
   with Import, Convention => C, External_Name => "g_type_query";

   function Register_Static_Type
     (Parent_Type  : Glib.GType;
      Type_Name    : Gtkada.Types.Chars_Ptr;
      Class_Size   : Glib.Guint;
      Class_Init   : System.Address;
      Instance_Size : Glib.Guint;
      Instance_Init : System.Address;
      Flags        : Glib.Guint) return Glib.GType
   with Import, Convention => C,
        External_Name => "g_type_register_static_simple";

   --  GtkATContext's signal has no payload, so the variadic emitter only
   --  needs the object and signal name as its fixed arguments.
   procedure Emit_State_Change
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Typed_State_Change
     (Self : access Gtk.Atcontext.Gtk_Atcontext_Record'Class);
   procedure Slot_State_Change
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Test_State_Change with Convention => C;

   --  Use a unique type name because GLib keeps registered types process-wide.
   function New_Concrete_Context_Type return Glib.GType is
      Query : aliased GType_Query;
      Name  : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("GtkAdaAtcontextCoverageTest");
      Result : Glib.GType;
   begin
      Query_Type (Gtk.Atcontext.Get_Type, Query'Access);
      Result := Register_Static_Type
        (Gtk.Atcontext.Get_Type, Name,
         Query.Class_Size, System.Null_Address,
         Query.Instance_Size, System.Null_Address, 0);
      Gtkada.Types.Free (Name);
      return Result;
   end New_Concrete_Context_Type;

   --  Each callback records the object it receives so the test verifies the
   --  typed marshaller and the slot-object marshaller pass the expected data.
   procedure Typed_State_Change
     (Self : access Gtk.Atcontext.Gtk_Atcontext_Record'Class) is
   begin
      Typed_Count := Typed_Count + 1;
      Callback_Order := Callback_Order + 1;
      Typed_Order := Callback_Order;
      Typed_Object := Self.Get_Object;
   end Typed_State_Change;

   procedure Slot_State_Change
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Slot_Count := Slot_Count + 1;
      Callback_Order := Callback_Order + 1;
      Slot_Order := Callback_Order;
      Slot_Object := Self.Get_Object;
   end Slot_State_Change;

   --  GTK_A11Y=none makes gtk_at_context_create return null. The test-only
   --  subtype provides a real GtkATContext instance without a backend.
   procedure Test_State_Change is
      Label      : constant Gtk.Label.Gtk_Label :=
        Gtk.Label.Gtk_Label_New ("accessible label");
      Context    : Gtk.Atcontext.Gtk_Atcontext;
      Name       : Gtkada.Types.Chars_Ptr;
      Context_Type : Glib.GType;
   begin
      Label.Ref_Sink;
      Context_Type := New_Concrete_Context_Type;
      Glib.Test.Assert_True (Context_Type /= 0);
      Context := new Gtk.Atcontext.Gtk_Atcontext_Record;
      Glib.Object.G_New (Context, Context_Type);
      Glib.Test.Assert_True (Context /= null);
      Glib.Test.Assert_True (Context.Is_Created);

      Context.On_State_Change
        (Typed_State_Change'Unrestricted_Access, After => False);
      Context.On_State_Change
        (Slot_State_Change'Unrestricted_Access, Label, After => True);

      Name := Gtkada.Types.New_String ("state-change");
      Emit_State_Change (Context.Get_Object, Name);
      Gtkada.Types.Free (Name);

      Glib.Test.Assert_Cmpuint_Eq (Glib.Guint (Typed_Count), 1);
      Glib.Test.Assert_Cmpuint_Eq (Glib.Guint (Slot_Count), 1);
      --  The slot connection is an after-handler, so it follows the typed one.
      Glib.Test.Assert_Cmpuint_Eq (Glib.Guint (Typed_Order), 1);
      Glib.Test.Assert_Cmpuint_Eq (Glib.Guint (Slot_Order), 2);
      Glib.Test.Assert_True (Typed_Object = Context.Get_Object);
      Glib.Test.Assert_True (Slot_Object = Label.Get_Object);

      Context.Unref;
      Label.Unref;
   end Test_State_Change;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/atcontext/state-change", Test_State_Change'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Atcontext_Test;
