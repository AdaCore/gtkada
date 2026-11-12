--  Exercises Gtk.Event_Controller_Key constructors, IM-context accessors,
--  and both callback forms for every public signal. The related C coverage
--  is testsuite/c_tests/imcontext.c; its surrounding-text tests use private
--  GtkText key-controller accessors that GtkAda does not expose.

with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C.Strings;
with System;

with Gdk.Enums;                 use Gdk.Enums;
with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Test;                 use Glib.Test;
with Gtk.Event_Controller_Key;  use Gtk.Event_Controller_Key;
with Gtk.IM_Context;            use Gtk.IM_Context;
with Gtk.IM_Context_Simple;     use Gtk.IM_Context_Simple;
with Gtk.Main;
with Gtkada.Bindings;

use type System.Address;

procedure Event_Controller_Key is
   use Interfaces.C.Strings;

   Keyval  : constant Guint := 16#61#;
   Keycode : constant Guint := 38;
   State   : constant Gdk_Modifier_Type := Gdk_Control_Mask;

   Current_Controller : Gtk_Event_Controller_Key := null;
   Expected_Slot      : System.Address := System.Null_Address;

   --  Separate counts prove both callback overloads ran for each signal.
   Im_Update_Typed  : Natural := 0;
   Im_Update_Slot   : Natural := 0;
   Key_Pressed_Typed : Natural := 0;
   Key_Pressed_Slot  : Natural := 0;
   Key_Released_Typed : Natural := 0;
   Key_Released_Slot  : Natural := 0;
   Modifiers_Typed : Natural := 0;
   Modifiers_Slot  : Natural := 0;
   Caught_Exceptions : Natural := 0;

   --  These imports call GLib's variadic emitter with fixed Ada profiles.
   --  Modifier flags are guint-sized, and gboolean results are written through
   --  the final pointer, as required by g_signal_emit_by_name.
   procedure Emit_Void
     (Instance : System.Address; Name : chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Key_Boolean
     (Instance : System.Address;
      Name     : chars_ptr;
      Key      : Guint;
      Code     : Guint;
      Modifiers : Gdk_Modifier_Type;
      Result   : access Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Key_Void
     (Instance : System.Address;
      Name     : chars_ptr;
      Key      : Guint;
      Code     : Guint;
      Modifiers : Gdk_Modifier_Type)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Modifiers
     (Instance : System.Address;
      Name     : chars_ptr;
      Modifiers : Gdk_Modifier_Type;
      Result   : access Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Test_Constructors_And_IM_Context with Convention => C;
   procedure Test_Signal_Handlers with Convention => C;
   procedure Test_Callback_Exception_Containment with Convention => C;

   --  The generated marshallers must route Ada exceptions to this hook rather
   --  than letting them unwind through GLib's C signal emitter.
   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Caught_Exceptions := Caught_Exceptions + 1;
   end Record_Exception;

   --  Typed callback forms receive the key controller itself. These helpers
   --  check that the marshaller supplied the signal's original object.
   procedure Check_Controller
     (Self : access Gtk_Event_Controller_Key_Record'Class);

   procedure Check_Controller
     (Self : access Gtk_Event_Controller_Key_Record'Class) is
   begin
      Assert_True (Self /= null);
      Assert_True (Get_Object (Self) = Get_Object (Current_Controller));
   end Check_Controller;

   --  Slot callbacks receive the object passed as the signal connection's
   --  user data, rather than the controller which emitted the signal.
   procedure Check_Slot (Self : access GObject_Record'Class);

   procedure Check_Slot (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Assert_True (Get_Object (Self) = Expected_Slot);
   end Check_Slot;

   --  The no-argument signal exercises the Gtk-specific and GObject slot
   --  marshallers for the input-method update notification.
   procedure On_Im_Update
     (Self : access Gtk_Event_Controller_Key_Record'Class) is
   begin
      Check_Controller (Self);
      Im_Update_Typed := Im_Update_Typed + 1;
   end On_Im_Update;

   procedure On_Im_Update_Slot
     (Self : access GObject_Record'Class) is
   begin
      Check_Slot (Self);
      Im_Update_Slot := Im_Update_Slot + 1;
   end On_Im_Update_Slot;

   --  The key-pressed handlers also check conversion of all three arguments.
   function On_Key_Pressed
     (Self     : access Gtk_Event_Controller_Key_Record'Class;
      Key      : Guint;
      Code     : Guint;
      Modifiers : Gdk_Modifier_Type) return Boolean is
   begin
      Check_Controller (Self);
      Assert_Cmpuint_Eq (Key, Keyval);
      Assert_Cmpuint_Eq (Code, Keycode);
      Assert_Cmphex_Eq (Guint (Modifiers), Guint (State));
      Key_Pressed_Typed := Key_Pressed_Typed + 1;
      return False;
   end On_Key_Pressed;

   function On_Key_Pressed_Slot
     (Self      : access GObject_Record'Class;
      Key       : Guint;
      Code      : Guint;
      Modifiers : Gdk_Modifier_Type) return Boolean is
   begin
      Check_Slot (Self);
      Assert_Cmpuint_Eq (Key, Keyval);
      Assert_Cmpuint_Eq (Code, Keycode);
      Assert_Cmphex_Eq (Guint (Modifiers), Guint (State));
      Key_Pressed_Slot := Key_Pressed_Slot + 1;
      return True;
   end On_Key_Pressed_Slot;

   --  Key release has the same arguments but a void return value.
   procedure On_Key_Released
     (Self      : access Gtk_Event_Controller_Key_Record'Class;
      Key       : Guint;
      Code      : Guint;
      Modifiers : Gdk_Modifier_Type) is
   begin
      Check_Controller (Self);
      Assert_Cmpuint_Eq (Key, Keyval);
      Assert_Cmpuint_Eq (Code, Keycode);
      Assert_Cmphex_Eq (Guint (Modifiers), Guint (State));
      Key_Released_Typed := Key_Released_Typed + 1;
   end On_Key_Released;

   procedure On_Key_Released_Slot
     (Self      : access GObject_Record'Class;
      Key       : Guint;
      Code      : Guint;
      Modifiers : Gdk_Modifier_Type) is
   begin
      Check_Slot (Self);
      Assert_Cmpuint_Eq (Key, Keyval);
      Assert_Cmpuint_Eq (Code, Keycode);
      Assert_Cmphex_Eq (Guint (Modifiers), Guint (State));
      Key_Released_Slot := Key_Released_Slot + 1;
   end On_Key_Released_Slot;

   --  Modifier callbacks validate the flags argument and return convention.
   function On_Modifiers
     (Self      : access Gtk_Event_Controller_Key_Record'Class;
      Modifiers : Gdk_Modifier_Type) return Boolean is
   begin
      Check_Controller (Self);
      Assert_Cmphex_Eq (Guint (Modifiers), Guint (State));
      Modifiers_Typed := Modifiers_Typed + 1;
      return False;
   end On_Modifiers;

   function On_Modifiers_Slot
     (Self      : access GObject_Record'Class;
      Modifiers : Gdk_Modifier_Type) return Boolean is
   begin
      Check_Slot (Self);
      Assert_Cmphex_Eq (Guint (Modifiers), Guint (State));
      Modifiers_Slot := Modifiers_Slot + 1;
      return True;
   end On_Modifiers_Slot;

   --  These deliberately failing handlers check the two distinct marshaller
   --  shapes: a typed void signal and a GObject slot signal with arguments.
   procedure Raise_On_Im_Update
     (Self : access Gtk_Event_Controller_Key_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "expected im-update callback exception";
   end Raise_On_Im_Update;

   procedure Raise_On_Key_Released_Slot
     (Self      : access GObject_Record'Class;
      Key       : Guint;
      Code      : Guint;
      Modifiers : Gdk_Modifier_Type) is
      pragma Unreferenced (Self, Key, Code, Modifiers);
   begin
      raise Program_Error with "expected key-released callback exception";
   end Raise_On_Key_Released_Slot;

   --  Both constructors initialize new objects. Reinitializing either object
   --  checks GtkAda's documented no-op path for an already-created instance.
   procedure Test_Constructors_And_IM_Context is
      Controller : constant Gtk_Event_Controller_Key :=
        Gtk_Event_Controller_Key_New;
      Other      : Gtk_Event_Controller_Key;
      Context    : constant Gtk_IM_Context_Simple :=
        Gtk_IM_Context_Simple_New;
   begin
      --  Constructors have distinct Ada entry points but should create the
      --  same GTK object type and safely tolerate a repeated initialization.
      Gtk_New (Other);
      Gtk.Event_Controller_Key.Initialize (Controller);
      Gtk.Event_Controller_Key.Initialize (Other);

      --  Round-trip a real GtkIMContextSimple through the public controller
      --  accessors, then clear it to cover the nullable setter argument.
      Controller.Set_Im_Context (Context);
      Assert_True
        (Get_Object (Controller.Get_Im_Context) = Get_Object (Context));
      Controller.Set_Im_Context (null);
      Assert_True (Controller.Get_Im_Context = null);

      --  Release each owned GObject reference after its final use.
      Unref (Controller);
      Unref (Other);
      Unref (Context);
   end Test_Constructors_And_IM_Context;

   --  Emit each signal with its declared C argument types so both callback
   --  overloads and the generated Gtk-specific/GObject marshallers run.
   procedure Test_Signal_Handlers is
      Controller : constant Gtk_Event_Controller_Key :=
        Gtk_Event_Controller_Key_New;
      Slot       : constant Gtk_IM_Context_Simple :=
        Gtk_IM_Context_Simple_New;
      Name       : chars_ptr;
      Result     : aliased Gboolean := 0;
   begin
      Current_Controller := Controller;
      Expected_Slot := Get_Object (Slot);

      --  Register every direct callback before its slot counterpart to keep
      --  emission order deterministic while checking each overload.
      --  The slot overload's After flag is set to cover post-default handlers.
      Controller.On_Im_Update (On_Im_Update'Unrestricted_Access);
      Controller.On_Im_Update
        (On_Im_Update_Slot'Unrestricted_Access, Slot, After => True);
      Controller.On_Key_Pressed (On_Key_Pressed'Unrestricted_Access);
      Controller.On_Key_Pressed
        (On_Key_Pressed_Slot'Unrestricted_Access, Slot, After => True);
      Controller.On_Key_Released (On_Key_Released'Unrestricted_Access);
      Controller.On_Key_Released
        (On_Key_Released_Slot'Unrestricted_Access, Slot, After => True);
      Controller.On_Modifiers (On_Modifiers'Unrestricted_Access);
      Controller.On_Modifiers
        (On_Modifiers_Slot'Unrestricted_Access, Slot, After => True);

      Name := New_String (String (Signal_Im_Update));
      Emit_Void (Get_Object (Controller), Name);
      --  The signal name is copied into the C call and can now be freed.
      Free (Name);
      Assert_Cmpint_Eq (Gint (Im_Update_Typed), 1);
      Assert_Cmpint_Eq (Gint (Im_Update_Slot), 1);

      Name := New_String (String (Signal_Key_Pressed));
      Result := 0;
      --  This is a synthetic signal emission: it checks argument marshalling,
      --  but it does not manufacture a native key event for Forward.
      Emit_Key_Boolean
        (Get_Object (Controller), Name, Keyval, Keycode, State, Result'Access);
      Free (Name);
      Assert_Cmpint_Eq (Gint (Key_Pressed_Typed), 1);
      Assert_Cmpint_Eq (Gint (Key_Pressed_Slot), 1);

      Name := New_String (String (Signal_Key_Released));
      Emit_Key_Void (Get_Object (Controller), Name, Keyval, Keycode, State);
      Free (Name);
      Assert_Cmpint_Eq (Gint (Key_Released_Typed), 1);
      Assert_Cmpint_Eq (Gint (Key_Released_Slot), 1);

      Name := New_String (String (Signal_Modifiers));
      Result := 0;
      Emit_Modifiers (Get_Object (Controller), Name, State, Result'Access);
      Free (Name);
      Assert_Cmpint_Eq (Gint (Modifiers_Typed), 1);
      Assert_Cmpint_Eq (Gint (Modifiers_Slot), 1);

      Unref (Controller);
      Unref (Slot);
      --  Do not retain wrapper pointers to finalized objects between cases.
      Current_Controller := null;
      Expected_Slot := System.Null_Address;
   end Test_Signal_Handlers;

   --  Callback failures are captured by Gtkada's exception hook and do not
   --  escape through g_signal_emit_by_name into C.
   procedure Test_Callback_Exception_Containment is
      Controller : constant Gtk_Event_Controller_Key :=
        Gtk_Event_Controller_Key_New;
      Slot       : constant Gtk_IM_Context_Simple :=
        Gtk_IM_Context_Simple_New;
      Name       : chars_ptr;
   begin
      Caught_Exceptions := 0;
      Controller.On_Im_Update
        (Raise_On_Im_Update'Unrestricted_Access);
      Name := New_String (String (Signal_Im_Update));
      Emit_Void (Get_Object (Controller), Name);
      Free (Name);

      Controller.On_Key_Released
        (Raise_On_Key_Released_Slot'Unrestricted_Access, Slot);
      Name := New_String (String (Signal_Key_Released));
      Emit_Key_Void (Get_Object (Controller), Name, Keyval, Keycode, State);
      Free (Name);
      Assert_Cmpint_Eq (Gint (Caught_Exceptions), 2);

      Unref (Controller);
      Unref (Slot);
   end Test_Callback_Exception_Containment;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Gtkada.Bindings.Set_On_Exception (Record_Exception'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/event-controller-key/constructors-and-im-context",
      Test_Constructors_And_IM_Context'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/event-controller-key/signals",
      Test_Signal_Handlers'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/event-controller-key/callback-exceptions",
      Test_Callback_Exception_Containment'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Event_Controller_Key;
