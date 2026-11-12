--  Exercise Gtk.Builder_List_Item_Factory's byte and resource constructors,
--  idempotent initializers, and source and scope accessors, plus
--  Gtk.Builder_CScope construction and callback registration. No matching C
--  testsuite case exists for these GTK units.

with Ada.Command_Line;
with System;

with Glib;        use Glib;
with Glib.Bytes;  use Glib.Bytes;
with Glib.Error;
with Glib.Object;
with Glib.Resource;
with Glib.Test;   use Glib.Test;

with Gtk.Builder;
with Gtk.Builder_CScope;
with Gtk.Builder_List_Item_Factory;
with Gtk.Builder_Scope;
with Gtk.Main;

procedure Main is
   use type Glib.GType;
   use type Glib.Error.GError;
   use type Glib.Resource.Gresource;
   use type Gtk.Builder_CScope.Gtk_Builder_C_Scope;
   use type Gtk.Builder_List_Item_Factory.Gtk_Builder_List_Item_Factory;
   use type Gtk.Builder_Scope.Gtk_Builder_Scope;
   use type System.Address;

   UI_Definition : constant UTF8_String :=
     "<interface><template class=""GtkListItem"">" &
     "<property name=""child""><object class=""GtkLabel""/>" &
     "</property></template></interface>";
   Resource_Path : constant UTF8_String :=
     "/org/gtkada/coverage/builder-list-item-factory/list_item.ui";

   function To_Bytes (Text : String) return Guint8_Array;
   procedure Test_Bytes_Constructors with Convention => C;
   procedure Test_Resource_Constructors with Convention => C;
   procedure Test_C_Scope_Callbacks with Convention => C;

   --  GtkBuilder stores a C closure after resolving this registered name.
   procedure Registered_Callback;

   procedure Unref_Closure (Closure : System.Address);
   pragma Import (C, Unref_Closure, "g_closure_unref");

   procedure Registered_Callback is
   begin
      null;
   end Registered_Callback;

   --  GBytes copies the UTF-8 bytes so the test data stays valid independently
   --  of the Ada string's lifetime and address.
   function To_Bytes (Text : String) return Guint8_Array is
      Result : Guint8_Array (1 .. Text'Length);
      Index  : Positive := Result'First;
   begin
      for Character of Text loop
         Result (Index) := Guint8 (Standard.Character'Pos (Character));
         Index := Index + 1;
      end loop;
      return Result;
   end To_Bytes;

   --  Both constructor forms expose the original bytes and builder scope.
   procedure Test_Bytes_Constructors is
      Scope : constant Gtk.Builder_CScope.Gtk_Builder_C_Scope :=
        Gtk.Builder_CScope.Gtk_Builder_C_Scope_New;
      Scope_Interface : constant Gtk.Builder_Scope.Gtk_Builder_Scope :=
        Gtk.Builder_CScope."+" (Scope);
      Data : constant Gbytes :=
        Gbytes_New (To_Bytes (UI_Definition), Gsize (UI_Definition'Length));
      By_Function : constant Gtk.Builder_List_Item_Factory.Gtk_Builder_List_Item_Factory :=
        Gtk.Builder_List_Item_Factory.Gtk_Builder_List_Item_Factory_New_From_Bytes
          (Scope_Interface, Data);
      By_Procedure : Gtk.Builder_List_Item_Factory.Gtk_Builder_List_Item_Factory;
      Initialized : constant Gtk.Builder_List_Item_Factory.Gtk_Builder_List_Item_Factory :=
        new Gtk.Builder_List_Item_Factory.Gtk_Builder_List_Item_Factory_Record;
      Replacement : constant Gbytes :=
        Gbytes_New (To_Bytes ("ignored"), 7);
   begin
      Assert_True (Gtk.Builder_List_Item_Factory.Get_Type /= 0);
      Assert_True (Glib.Object.Is_Created (By_Function.all));
      Assert_True (By_Function.Get_Bytes.Get_Object = Data.Get_Object);
      Assert_Cmpstr_Eq (By_Function.Get_Resource, "");
      Assert_True (By_Function.Get_Scope = Scope_Interface);

      --  The out-parameter constructor reaches its separate generated path.
      Gtk.Builder_List_Item_Factory.Gtk_New_From_Bytes
        (By_Procedure, Scope_Interface, Data);
      Assert_True (Glib.Object.Is_Created (By_Procedure.all));
      Assert_True (By_Procedure.Get_Bytes.Get_Object = Data.Get_Object);
      Assert_True (By_Procedure.Get_Scope = Scope_Interface);

      --  A fresh wrapper initializes once; repeating Initialize must preserve
      --  the object and its original bytes.
      Gtk.Builder_List_Item_Factory.Initialize_From_Bytes
        (Initialized, Scope_Interface, Data);
      declare
         Original_Object : constant System.Address := Initialized.Get_Object;
      begin
         --  The constructor guard is a no-op: neither wrapper identity nor
         --  the stored byte source may change on a second initialization.
         Gtk.Builder_List_Item_Factory.Initialize_From_Bytes
           (Initialized, Scope_Interface, Replacement);
         Assert_True (Initialized.Get_Object = Original_Object);
         Assert_True (Initialized.Get_Bytes.Get_Object = Data.Get_Object);
      end;

      Glib.Object.Unref (Glib.Object.GObject (Initialized));
      Glib.Object.Unref (Glib.Object.GObject (By_Procedure));
      Glib.Object.Unref (Glib.Object.GObject (By_Function));
      Glib.Object.Unref (Glib.Object.GObject (Scope));
      Unref (Replacement);
      Unref (Data);
   end Test_Bytes_Constructors;

   --  The local resource fixture allows real resource-backed constructors
   --  without depending on installed application data or user files.
   procedure Test_Resource_Constructors is
      Error : Glib.Error.GError;
      Resource : Glib.Resource.Gresource :=
        Glib.Resource.Load ("list_item.gresource", Error);
      Scope : constant Gtk.Builder_CScope.Gtk_Builder_C_Scope :=
        Gtk.Builder_CScope.Gtk_Builder_C_Scope_New;
      Scope_Interface : constant Gtk.Builder_Scope.Gtk_Builder_Scope :=
        Gtk.Builder_CScope."+" (Scope);
      By_Function : Gtk.Builder_List_Item_Factory.Gtk_Builder_List_Item_Factory;
      By_Procedure : Gtk.Builder_List_Item_Factory.Gtk_Builder_List_Item_Factory;
      Initialized : constant Gtk.Builder_List_Item_Factory.Gtk_Builder_List_Item_Factory :=
        new Gtk.Builder_List_Item_Factory.Gtk_Builder_List_Item_Factory_Record;
   begin
      Assert_True (Error = null and Resource /= Glib.Resource.Null_Gresource);
      Resource.Register;

      --  Resource-backed factories keep the path and the exact scope used
      --  for later instantiation of each list item.
      By_Function :=
        Gtk.Builder_List_Item_Factory.Gtk_Builder_List_Item_Factory_New_From_Resource
          (Scope_Interface, Resource_Path);
      Assert_True (Glib.Object.Is_Created (By_Function.all));
      Assert_Cmpstr_Eq (By_Function.Get_Resource, Resource_Path);
      Assert_True (By_Function.Get_Scope = Scope_Interface);

      --  Exercise the procedure constructor and its resource-path conversion.
      Gtk.Builder_List_Item_Factory.Gtk_New_From_Resource
        (By_Procedure, Scope_Interface, Resource_Path);
      Assert_Cmpstr_Eq (By_Procedure.Get_Resource, Resource_Path);
      Assert_True (By_Procedure.Get_Scope = Scope_Interface);

      --  Resource factories are mutually exclusive with embedded bytes, so
      --  their useful source accessor is the resource path.
      --  Both the first initialization and the already-created guard are
      --  covered; the latter must leave the selected resource unchanged.
      Gtk.Builder_List_Item_Factory.Initialize_From_Resource
        (Initialized, Scope_Interface, Resource_Path);
      Gtk.Builder_List_Item_Factory.Initialize_From_Resource
        (Initialized, Scope_Interface, "/ignored-after-initialize.ui");
      Assert_Cmpstr_Eq (Initialized.Get_Resource, Resource_Path);

      Glib.Object.Unref (Glib.Object.GObject (Initialized));
      Glib.Object.Unref (Glib.Object.GObject (By_Procedure));
      Glib.Object.Unref (Glib.Object.GObject (By_Function));
      Glib.Object.Unref (Glib.Object.GObject (Scope));
      Resource.Unregister;
      Glib.Resource.Unref (Resource);
   end Test_Resource_Constructors;

   ---------------------------
   -- Test_C_Scope_Callbacks --
   ---------------------------

   procedure Test_C_Scope_Callbacks is
      Scope : Gtk.Builder_CScope.Gtk_Builder_C_Scope;
      Builder : constant Gtk.Builder.Gtk_Builder :=
        Gtk.Builder.Gtk_Builder_New;
      Error : Glib.Error.GError;
      Closure : System.Address;
   begin
      --  Exercise Gtk_New's allocation path and Initialize's already-created
      --  guard without replacing the underlying GObject.
      Gtk.Builder_CScope.Gtk_New (Scope);
      Assert_True (Glib.Object.Is_Created (Scope.all));
      declare
         Original_Object : constant System.Address := Scope.Get_Object;
      begin
         Gtk.Builder_CScope.Initialize (Scope);
         Assert_True (Scope.Get_Object = Original_Object);
      end;

      Assert_True (Gtk.Builder_CScope.Get_Type /= 0);
      Builder.Set_Scope (Gtk.Builder_CScope."+" (Scope));

      --  A successful closure lookup verifies that the supplied Ada callback
      --  address was stored under the Builder-visible function name.
      Scope.Add_Callback_Symbol
        ("registered_callback", Registered_Callback'Unrestricted_Access);
      Closure := Builder.Create_Closure
        ("registered_callback", 0, null, Error);
      Assert_True (Error = null);
      Assert_Nonnull (Closure);
      Unref_Closure (Closure);

      Glib.Object.Unref (Glib.Object.GObject (Builder));
      Glib.Object.Unref (Glib.Object.GObject (Scope));
   end Test_C_Scope_Callbacks;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gtk-builder-list-item-factory/bytes-constructors",
      Test_Bytes_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtk-builder-list-item-factory/resource-constructors",
      Test_Resource_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtk-builder-list-item-factory/c-scope-callbacks",
      Test_C_Scope_Callbacks'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
