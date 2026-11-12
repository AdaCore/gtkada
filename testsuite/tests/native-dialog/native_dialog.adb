--  Exercises Gtk.Native_Dialog through GtkFileChooserNative, a concrete
--  subclass still available in GTK 4.22 but not exposed by this GtkAda tree.
--  The local constructor import lets the test cover the public base-class
--  methods and response marshallers without changing the generated bindings.

with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C;
with System;

with Glib;
with Glib.Object; use Glib.Object;
with Glib.Properties;
with Glib.Test; use Glib.Test;
with Gtk.Label; use Gtk.Label;
with Gtk.Main;
with Gtk.Native_Dialog; use Gtk.Native_Dialog;
with Gtk.Window; use Gtk.Window;
with Gtkada.Bindings;
with Gtkada.Types; use Gtkada.Types;

procedure Native_Dialog is

   use type Glib.Object.GObject;
   use type Gtk.Native_Dialog.Gtk_Native_Dialog;
   use type Gtk.Window.Gtk_Window;
   use type System.Address;

   --  This GTK constructor returns GtkFileChooserNative, which derives from
   --  GtkNativeDialog; action zero is GTK_FILE_CHOOSER_ACTION_OPEN.
   function New_File_Chooser_Native
     (Title        : Chars_Ptr;
      Parent       : System.Address;
      Action       : Interfaces.C.int;
      Accept_Label : Chars_Ptr;
      Cancel_Label : Chars_Ptr) return System.Address
   with Import, Convention => C,
        External_Name => "gtk_file_chooser_native_new";

   --  Emit a response directly so no user interaction or portal is needed.
   procedure Emit_Response
     (Object      : System.Address;
      Name        : Chars_Ptr;
      Response_Id : Glib.Gint)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   Typed_Calls       : Natural := 0;
   Slot_Calls        : Natural := 0;
   Typed_Response_Id : Glib.Gint := 0;
   Slot_Response_Id  : Glib.Gint := 0;
   Exception_Calls   : Natural := 0;
   Typed_Object      : System.Address := System.Null_Address;
   Slot_Object       : System.Address := System.Null_Address;

   procedure On_Typed_Response
     (Self : access Gtk_Native_Dialog_Record'Class;
      Response_Id : Glib.Gint);

   procedure On_Slot_Response
     (Self : access Glib.Object.GObject_Record'Class;
      Response_Id : Glib.Gint);

   procedure On_Typed_Response_Raising
     (Self : access Gtk_Native_Dialog_Record'Class;
      Response_Id : Glib.Gint);

   procedure On_Slot_Response_Raising
     (Self : access Glib.Object.GObject_Record'Class;
      Response_Id : Glib.Gint);

   procedure Capture_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Test_Properties_And_Visibility with Convention => C;
   procedure Test_Response_Overloads with Convention => C;

   --  GtkAda creates and caches the base-class wrapper when converting this
   --  raw GObject pointer, using the Gtk.Native_Dialog type-conversion hook.
   function New_Dialog return Gtk_Native_Dialog is
      Title        : Chars_Ptr := New_String ("Initial title");
      Accept_Label : Chars_Ptr := New_String ("Choose");
      Cancel_Label : Chars_Ptr := New_String ("Dismiss");
      Raw          : System.Address;
      Stub         : Gtk_Native_Dialog_Record;
      Result       : Gtk_Native_Dialog;
   begin
      Raw := New_File_Chooser_Native
        (Title, System.Null_Address, 0, Accept_Label, Cancel_Label);
      Free (Title);
      Free (Accept_Label);
      Free (Cancel_Label);
      Result := Gtk_Native_Dialog (Get_User_Data (Raw, Stub));
      Assert_True (Result /= null);
      return Result;
   end New_Dialog;

   -----------------------------
   -- Test_Properties_And_Visibility --
   -----------------------------

   procedure Test_Properties_And_Visibility is
      Dialog : constant Gtk_Native_Dialog := New_Dialog;
      Parent : constant Gtk_Window := Gtk_Window_New;
   begin
      --  Methods and GObject properties must expose the same modal and title.
      Assert_False (Dialog.Get_Modal);
      Dialog.Set_Modal (True);
      Assert_True (Dialog.Get_Modal);
      Assert_True
        (Glib.Properties.Get_Property
           (Dialog, Gtk.Native_Dialog.Modal_Property));
      Glib.Properties.Set_Property
        (Dialog, Gtk.Native_Dialog.Modal_Property, False);
      Assert_False (Dialog.Get_Modal);

      Assert_Cmpstr_Eq (Dialog.Get_Title, "Initial title");
      Dialog.Set_Title ("Updated title");
      Assert_Cmpstr_Eq (Dialog.Get_Title, "Updated title");
      Assert_Cmpstr_Eq
        (Glib.Properties.Get_Property
           (Dialog, Gtk.Native_Dialog.Title_Property),
         "Updated title");
      Glib.Properties.Set_Property
        (Dialog, Gtk.Native_Dialog.Title_Property, "Property title");
      Assert_Cmpstr_Eq (Dialog.Get_Title, "Property title");

      --  The transient parent is a borrowed window; GtkAda returns its cached
      --  wrapper, while passing null clears the native association.
      Assert_True (Dialog.Get_Transient_For = null);
      Dialog.Set_Transient_For (Parent);
      Assert_True (Dialog.Get_Transient_For = Parent);
      Assert_True
        (Glib.Properties.Get_Property
           (Dialog, Gtk.Native_Dialog.Transient_For_Property).Get_Object =
         Get_Object (Parent));
      Glib.Properties.Set_Property
        (Dialog, Gtk.Native_Dialog.Transient_For_Property, null);
      Assert_True (Dialog.Get_Transient_For = null);
      Dialog.Set_Transient_For (null);

      --  Show and hide are synchronous state changes even when the dialog
      --  backend is native; hiding immediately avoids requesting any choice.
      Assert_False (Dialog.Get_Visible);
      Assert_False
        (Glib.Properties.Get_Property
           (Dialog, Gtk.Native_Dialog.Visible_Property));
      Dialog.Show;
      Assert_True (Dialog.Get_Visible);
      Assert_True
        (Glib.Properties.Get_Property
           (Dialog, Gtk.Native_Dialog.Visible_Property));
      Dialog.Hide;
      Assert_False (Dialog.Get_Visible);
      Assert_False
        (Glib.Properties.Get_Property
           (Dialog, Gtk.Native_Dialog.Visible_Property));

      --  Destroy releases native resources but not the caller's object ref.
      Dialog.Destroy;
      Assert_False (Dialog.Get_Visible);
      Unref (Dialog);
      Parent.Destroy;
   end Test_Properties_And_Visibility;

   --------------------------
   -- Test_Response_Overloads --
   --------------------------

   procedure Test_Response_Overloads is
      Dialog      : constant Gtk_Native_Dialog := New_Dialog;
      Slot        : constant Gtk_Label := Gtk_Label_New ("response slot");
      Signal_Name : Chars_Ptr := New_String ("response");
      Expected    : constant Glib.Gint := 37;
   begin
      --  The typed handler receives GtkNativeDialog; the slot form receives
      --  the supplied GObject slot, and both callbacks receive the response.
      Typed_Calls := 0;
      Slot_Calls := 0;
      Typed_Response_Id := 0;
      Slot_Response_Id := 0;
      Exception_Calls := 0;
      Typed_Object := System.Null_Address;
      Slot_Object := System.Null_Address;
      Dialog.On_Response (On_Typed_Response'Unrestricted_Access);
      Dialog.On_Response
        (On_Slot_Response'Unrestricted_Access, Slot => Slot, After => True);
      --  Each marshaller must catch a callback exception before returning to
      --  GLib and pass it to GtkAda's configured exception handler.
      Dialog.On_Response (On_Typed_Response_Raising'Unrestricted_Access);
      Dialog.On_Response
        (On_Slot_Response_Raising'Unrestricted_Access, Slot => Slot);
      Gtkada.Bindings.Set_On_Exception
        (Capture_Exception'Unrestricted_Access);

      Emit_Response (Get_Object (Dialog), Signal_Name, Expected);
      Assert_Cmpuint_Eq (Glib.Guint (Typed_Calls), 1);
      Assert_Cmpuint_Eq (Glib.Guint (Slot_Calls), 1);
      Assert_Cmpint_Eq (Typed_Response_Id, Expected);
      Assert_Cmpint_Eq (Slot_Response_Id, Expected);
      Assert_True (Typed_Object = Get_Object (Dialog));
      Assert_True (Slot_Object = Get_Object (Slot));
      Assert_Cmpuint_Eq (Glib.Guint (Exception_Calls), 2);

      Free (Signal_Name);
      Dialog.Destroy;
      Unref (Dialog);
   end Test_Response_Overloads;

   -----------------------
   -- On_Typed_Response --
   -----------------------

   procedure On_Typed_Response
     (Self : access Gtk_Native_Dialog_Record'Class;
      Response_Id : Glib.Gint) is
   begin
      Typed_Calls := Typed_Calls + 1;
      Typed_Response_Id := Response_Id;
      Typed_Object := Get_Object (Self);
   end On_Typed_Response;

   ----------------------
   -- On_Slot_Response --
   ----------------------

   procedure On_Slot_Response
     (Self : access Glib.Object.GObject_Record'Class;
      Response_Id : Glib.Gint) is
   begin
      Slot_Calls := Slot_Calls + 1;
      Slot_Response_Id := Response_Id;
      Slot_Object := Get_Object (Self);
   end On_Slot_Response;

   -------------------------------
   -- On_Typed_Response_Raising --
   -------------------------------

   procedure On_Typed_Response_Raising
     (Self : access Gtk_Native_Dialog_Record'Class;
      Response_Id : Glib.Gint) is
      pragma Unreferenced (Self, Response_Id);
   begin
      raise Program_Error with "typed native dialog response callback";
   end On_Typed_Response_Raising;

   ------------------------------
   -- On_Slot_Response_Raising --
   ------------------------------

   procedure On_Slot_Response_Raising
     (Self : access Glib.Object.GObject_Record'Class;
      Response_Id : Glib.Gint) is
      pragma Unreferenced (Self, Response_Id);
   begin
      raise Program_Error with "slot native dialog response callback";
   end On_Slot_Response_Raising;

   -----------------------
   -- Capture_Exception --
   -----------------------

   procedure Capture_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Exception_Calls := Exception_Calls + 1;
   end Capture_Exception;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/native-dialog/properties-visibility",
      Test_Properties_And_Visibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/native-dialog/response-overloads",
      Test_Response_Overloads'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Native_Dialog;
