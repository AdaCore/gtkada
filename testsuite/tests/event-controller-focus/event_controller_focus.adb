--  Exercises Gtk.Event_Controller_Focus construction, focus state, and both
--  typed and GObject-slot signal callbacks. GTK has no matching C test to port;
--  GLib signals are emitted directly so callbacks need no physical focus event.

with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C.Strings;
with System;

with Glib;                        use Glib;
with Glib.Object;                 use Glib.Object;
with Glib.Properties;             use Glib.Properties;
with Glib.Test;                   use Glib.Test;
with Gtk.Event_Controller_Focus;  use Gtk.Event_Controller_Focus;
with Gtk.Main;
with Gtkada.Bindings;

use type System.Address;

procedure Event_Controller_Focus is
   use Interfaces.C.Strings;

   Current_Controller : Gtk_Event_Controller_Focus := null;
   Expected_Slot      : System.Address := System.Null_Address;

   Enter_Typed_Count  : Natural := 0;
   Enter_Slot_Count   : Natural := 0;
   Leave_Typed_Count  : Natural := 0;
   Leave_Slot_Count   : Natural := 0;
   Caught_Exceptions  : Natural := 0;

   --  The GLib signal emitter is variadic; each wrapper fixes its arguments
   --  to the instance and signal name for the no-argument focus signals.
   procedure Emit_Void (Object : System.Address; Name : chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Test_Constructors_And_State with Convention => C;
   procedure Test_Signal_Handlers with Convention => C;
   procedure Test_Callback_Exception_Containment with Convention => C;

   --  GtkAda catches callback exceptions before they can unwind through GLib.
   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Caught_Exceptions := Caught_Exceptions + 1;
   end Record_Exception;

   --  Typed callbacks receive the controller that emitted the focus signal.
   procedure Check_Controller
     (Self : access Gtk_Event_Controller_Focus_Record'Class);

   procedure Check_Controller
     (Self : access Gtk_Event_Controller_Focus_Record'Class) is
   begin
      Assert_True (Self /= null);
      Assert_True (Get_Object (Self) = Get_Object (Current_Controller));
   end Check_Controller;

   --  Slot callbacks receive the user-data object passed to On_Enter/On_Leave.
   procedure Check_Slot (Self : access GObject_Record'Class);

   procedure Check_Slot (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Assert_True (Get_Object (Self) = Expected_Slot);
   end Check_Slot;

   --  Enter and leave have no signal arguments, so these callbacks check the
   --  two generated marshaller profiles and count exactly one invocation each.
   procedure On_Enter_Typed
     (Self : access Gtk_Event_Controller_Focus_Record'Class) is
   begin
      Check_Controller (Self);
      Enter_Typed_Count := Enter_Typed_Count + 1;
   end On_Enter_Typed;

   procedure On_Enter_Slot (Self : access GObject_Record'Class) is
   begin
      Check_Slot (Self);
      Enter_Slot_Count := Enter_Slot_Count + 1;
   end On_Enter_Slot;

   procedure On_Leave_Typed
     (Self : access Gtk_Event_Controller_Focus_Record'Class) is
   begin
      Check_Controller (Self);
      Leave_Typed_Count := Leave_Typed_Count + 1;
   end On_Leave_Typed;

   procedure On_Leave_Slot (Self : access GObject_Record'Class) is
   begin
      Check_Slot (Self);
      Leave_Slot_Count := Leave_Slot_Count + 1;
   end On_Leave_Slot;

   --  Deliberately failing handlers exercise each marshaller's exception
   --  containment path; they are expected to be reported through the hook.
   procedure Raise_Typed
     (Self : access Gtk_Event_Controller_Focus_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "expected typed focus callback exception";
   end Raise_Typed;

   procedure Raise_Slot (Self : access GObject_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "expected slot focus callback exception";
   end Raise_Slot;

   --  Both constructors create controllers. Reinitializing each one checks
   --  Initialize's documented no-op behavior for an already-created object.
   procedure Test_Constructors_And_State is
      By_Function : constant Gtk_Event_Controller_Focus :=
        Gtk_Event_Controller_Focus_New;
      By_Procedure : Gtk_Event_Controller_Focus;
   begin
      Gtk_New (By_Procedure);
      Gtk.Event_Controller_Focus.Initialize (By_Function);
      Gtk.Event_Controller_Focus.Initialize (By_Procedure);

      Assert_True (Get_Type /= GType_Invalid);
      Assert_True (By_Function.Get_Type = Get_Type);
      --  Detached controllers have no focused widget, both as methods and properties.
      Assert_False (By_Function.Contains_Focus);
      Assert_False (By_Function.Is_Focus);
      Assert_False (Get_Property (By_Function, Contains_Focus_Property));
      Assert_False (Get_Property (By_Function, Is_Focus_Property));
      Assert_Cmpstr_Eq (String (Signal_Enter), "enter");
      Assert_Cmpstr_Eq (String (Signal_Leave), "leave");

      Unref (By_Function);
      Unref (By_Procedure);
   end Test_Constructors_And_State;

   --  Synchronous emissions exercise both overloads of each public signal
   --  registration without needing a window manager or physical focus change.
   procedure Test_Signal_Handlers is
      Controller : constant Gtk_Event_Controller_Focus :=
        Gtk_Event_Controller_Focus_New;
      Slot : constant Gtk_Event_Controller_Focus :=
        Gtk_Event_Controller_Focus_New;
      Name : chars_ptr;
   begin
      Current_Controller := Controller;
      Expected_Slot := Get_Object (Slot);
      Enter_Typed_Count := 0;
      Enter_Slot_Count := 0;
      Leave_Typed_Count := 0;
      Leave_Slot_Count := 0;

      Controller.On_Enter (On_Enter_Typed'Unrestricted_Access);
      --  The slot differs from the emitter so callbacks can verify user data.
      Controller.On_Enter
        (On_Enter_Slot'Unrestricted_Access, Slot, After => True);
      Controller.On_Leave (On_Leave_Typed'Unrestricted_Access);
      Controller.On_Leave
        (On_Leave_Slot'Unrestricted_Access, Slot, After => True);

      --  Signal names allocated for the C call are freed after each emission.
      Name := New_String (String (Signal_Enter));
      Emit_Void (Get_Object (Controller), Name);
      Free (Name);
      Name := New_String (String (Signal_Leave));
      Emit_Void (Get_Object (Controller), Name);
      Free (Name);

      Assert_Cmpuint_Eq (Guint (Enter_Typed_Count), 1);
      Assert_Cmpuint_Eq (Guint (Enter_Slot_Count), 1);
      Assert_Cmpuint_Eq (Guint (Leave_Typed_Count), 1);
      Assert_Cmpuint_Eq (Guint (Leave_Slot_Count), 1);

      Unref (Controller);
      Unref (Slot);
      Current_Controller := null;
      Expected_Slot := System.Null_Address;
   end Test_Signal_Handlers;

   --  A failing typed and slot callback for each signal should be caught by
   --  its generated marshaller and delivered to Gtkada's exception hook.
   procedure Test_Callback_Exception_Containment is
      Controller : constant Gtk_Event_Controller_Focus :=
        Gtk_Event_Controller_Focus_New;
      Slot : constant Gtk_Event_Controller_Focus :=
        Gtk_Event_Controller_Focus_New;
      Name : chars_ptr;
   begin
      Caught_Exceptions := 0;
      --  Each signal dispatches once to both callback profiles below.
      Controller.On_Enter (Raise_Typed'Unrestricted_Access);
      Controller.On_Enter (Raise_Slot'Unrestricted_Access, Slot, After => True);
      Controller.On_Leave (Raise_Typed'Unrestricted_Access);
      Controller.On_Leave (Raise_Slot'Unrestricted_Access, Slot, After => True);

      Name := New_String (String (Signal_Enter));
      Emit_Void (Get_Object (Controller), Name);
      Free (Name);
      Name := New_String (String (Signal_Leave));
      Emit_Void (Get_Object (Controller), Name);
      Free (Name);
      Assert_Cmpuint_Eq (Guint (Caught_Exceptions), 4);

      Unref (Controller);
      Unref (Slot);
   end Test_Callback_Exception_Containment;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Gtkada.Bindings.Set_On_Exception (Record_Exception'Unrestricted_Access);
   Add_Func
     ("/event-controller-focus/constructors-state",
      Test_Constructors_And_State'Unrestricted_Access);
   Add_Func
     ("/event-controller-focus/signals",
      Test_Signal_Handlers'Unrestricted_Access);
   Add_Func
     ("/event-controller-focus/callback-exceptions",
      Test_Callback_Exception_Containment'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Event_Controller_Focus;
