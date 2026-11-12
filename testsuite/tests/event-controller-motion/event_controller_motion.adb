--  Exercises Gtk.Event_Controller_Motion construction, pointer-state queries,
--  both Boolean properties, and every typed and GObject-slot signal handler.
--  GTK has no matching event-controller-motion C test to port, so signals are
--  emitted directly to check GtkAda's generated callback marshallers.

with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C.Strings;
with System;

with Glib;                         use Glib;
with Glib.Object;                  use Glib.Object;
with Glib.Properties;              use Glib.Properties;
with Glib.Test;                    use Glib.Test;
with Gtk.Event_Controller_Motion;  use Gtk.Event_Controller_Motion;
with Gtk.Main;
with Gtkada.Bindings;

use type System.Address;

procedure Event_Controller_Motion is
   use Interfaces.C.Strings;

   Pointer_X : constant Gdouble := 3.5;
   Pointer_Y : constant Gdouble := 7.25;

   Current_Controller : Gtk_Event_Controller_Motion := null;
   Expected_Slot      : System.Address := System.Null_Address;

   Enter_Typed_Count  : Natural := 0;
   Enter_Slot_Count   : Natural := 0;
   Leave_Typed_Count  : Natural := 0;
   Leave_Slot_Count   : Natural := 0;
   Motion_Typed_Count : Natural := 0;
   Motion_Slot_Count  : Natural := 0;
   Caught_Exceptions  : Natural := 0;

   --  The fixed Ada profiles emit the exact argument lists of these GLib
   --  signals while avoiding dependence on a physical pointer event.
   procedure Emit_Void (Object : System.Address; Name : chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Coordinates
     (Object : System.Address;
      Name   : chars_ptr;
      X, Y   : Gdouble)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Test_Constructors_And_Pointer_State with Convention => C;
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

   --  Concrete callbacks receive the controller that emitted the signal.
   --  Comparing object handles checks that the generated marshaller preserves
   --  the emitting instance when converting the raw C parameter back to Ada.
   procedure Check_Controller
     (Self : access Gtk_Event_Controller_Motion_Record'Class);

   procedure Check_Controller
     (Self : access Gtk_Event_Controller_Motion_Record'Class) is
   begin
      Assert_True (Self /= null);
      Assert_True (Get_Object (Self) = Get_Object (Current_Controller));
   end Check_Controller;

   --  Slot callbacks instead receive the user-data object supplied at connect.
   --  This is intentionally a second controller, distinct from the emitter,
   --  so the test detects accidental use of the signal instance as slot data.
   procedure Check_Slot (Self : access GObject_Record'Class);

   procedure Check_Slot (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Assert_True (Get_Object (Self) = Expected_Slot);
   end Check_Slot;

   --  Enter reports the pointer coordinates to both callback overloads.
   --  These checks cover both the Gtk-specific and generic GObject callback
   --  signatures while validating the two double-valued signal arguments.
   procedure On_Enter_Typed
     (Self : access Gtk_Event_Controller_Motion_Record'Class;
      X, Y : Gdouble) is
   begin
      Check_Controller (Self);
      Assert_Cmpfloat_Eq (X, Pointer_X);
      Assert_Cmpfloat_Eq (Y, Pointer_Y);
      Enter_Typed_Count := Enter_Typed_Count + 1;
   end On_Enter_Typed;

   procedure On_Enter_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble) is
   begin
      Check_Slot (Self);
      Assert_Cmpfloat_Eq (X, Pointer_X);
      Assert_Cmpfloat_Eq (Y, Pointer_Y);
      Enter_Slot_Count := Enter_Slot_Count + 1;
   end On_Enter_Slot;

   --  Leave has no coordinates, but still exercises both callback shapes.
   --  Its no-argument signature selects the other pair of generated
   --  marshallers, independently of the coordinate-bearing signals.
   procedure On_Leave_Typed
     (Self : access Gtk_Event_Controller_Motion_Record'Class) is
   begin
      Check_Controller (Self);
      Leave_Typed_Count := Leave_Typed_Count + 1;
   end On_Leave_Typed;

   procedure On_Leave_Slot (Self : access GObject_Record'Class) is
   begin
      Check_Slot (Self);
      Leave_Slot_Count := Leave_Slot_Count + 1;
   end On_Leave_Slot;

   --  Motion carries the same coordinate pair and uses separate registrations.
   --  Verifying this signal separately ensures its public On_Motion overloads
   --  connect under the motion name rather than accidentally under enter.
   procedure On_Motion_Typed
     (Self : access Gtk_Event_Controller_Motion_Record'Class;
      X, Y : Gdouble) is
   begin
      Check_Controller (Self);
      Assert_Cmpfloat_Eq (X, Pointer_X);
      Assert_Cmpfloat_Eq (Y, Pointer_Y);
      Motion_Typed_Count := Motion_Typed_Count + 1;
   end On_Motion_Typed;

   procedure On_Motion_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble) is
   begin
      Check_Slot (Self);
      Assert_Cmpfloat_Eq (X, Pointer_X);
      Assert_Cmpfloat_Eq (Y, Pointer_Y);
      Motion_Slot_Count := Motion_Slot_Count + 1;
   end On_Motion_Slot;

   --  Raising from each marshaller shape covers its exception containment.
   --  GtkAda must report these failures through the configured exception hook
   --  because Ada exceptions cannot unwind safely through GLib's C frames.
   procedure Raise_Coordinates_Typed
     (Self : access Gtk_Event_Controller_Motion_Record'Class;
      X, Y : Gdouble) is
      pragma Unreferenced (Self, X, Y);
   begin
      raise Program_Error with "expected typed coordinate callback exception";
   end Raise_Coordinates_Typed;

   procedure Raise_Coordinates_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble) is
      pragma Unreferenced (Self, X, Y);
   begin
      raise Program_Error with "expected slot coordinate callback exception";
   end Raise_Coordinates_Slot;

   procedure Raise_Void_Typed
     (Self : access Gtk_Event_Controller_Motion_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "expected typed void callback exception";
   end Raise_Void_Typed;

   procedure Raise_Void_Slot (Self : access GObject_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "expected slot void callback exception";
   end Raise_Void_Slot;

   --  Both constructors create an object; repeated Initialize is documented
   --  as a no-op, and detached controllers report no pointer over them.
   procedure Test_Constructors_And_Pointer_State is
      By_Function : constant Gtk_Event_Controller_Motion :=
        Gtk_Event_Controller_Motion_New;
      By_Procedure : Gtk_Event_Controller_Motion;
   begin
      Gtk_New (By_Procedure);
      Gtk.Event_Controller_Motion.Initialize (By_Function);
      Gtk.Event_Controller_Motion.Initialize (By_Procedure);

      --  A newly created, detached controller has no pointer crossing state.
      Assert_False (By_Function.Contains_Pointer);
      Assert_False (By_Function.Is_Pointer);
      Assert_False (Get_Property (By_Function, Contains_Pointer_Property));
      Assert_False (Get_Property (By_Function, Is_Pointer_Property));
      Assert_Cmpstr_Eq (String (Signal_Enter), "enter");
      Assert_Cmpstr_Eq (String (Signal_Leave), "leave");
      Assert_Cmpstr_Eq (String (Signal_Motion), "motion");

      Unref (By_Function);
      Unref (By_Procedure);
   end Test_Constructors_And_Pointer_State;

   --  Emit all three documented signals and verify typed/slot callback inputs.
   procedure Test_Signal_Handlers is
      Controller : constant Gtk_Event_Controller_Motion :=
        Gtk_Event_Controller_Motion_New;
      Slot       : constant Gtk_Event_Controller_Motion :=
        Gtk_Event_Controller_Motion_New;
      Name : chars_ptr;
   begin
      Current_Controller := Controller;
      Expected_Slot := Get_Object (Slot);
      --  Reset shared observations so this test remains independent of order.
      Enter_Typed_Count := 0;
      Enter_Slot_Count := 0;
      Leave_Typed_Count := 0;
      Leave_Slot_Count := 0;
      Motion_Typed_Count := 0;
      Motion_Slot_Count := 0;

      Controller.On_Enter (On_Enter_Typed'Unrestricted_Access);
      Controller.On_Enter (On_Enter_Slot'Unrestricted_Access, Slot, After => True);
      Controller.On_Leave (On_Leave_Typed'Unrestricted_Access);
      Controller.On_Leave (On_Leave_Slot'Unrestricted_Access, Slot, After => True);
      Controller.On_Motion (On_Motion_Typed'Unrestricted_Access);
      Controller.On_Motion (On_Motion_Slot'Unrestricted_Access, Slot, After => True);

      --  Each emission is synchronous, so the allocated signal name can be
      --  freed as soon as GLib returns from the emitter.
      --  The name allocation stays alive through each synchronous GLib emission.
      Name := New_String (String (Signal_Enter));
      Emit_Coordinates
        (Get_Object (Controller), Name, Pointer_X, Pointer_Y);
      Free (Name);
      Name := New_String (String (Signal_Leave));
      Emit_Void (Get_Object (Controller), Name);
      Free (Name);
      Name := New_String (String (Signal_Motion));
      Emit_Coordinates
        (Get_Object (Controller), Name, Pointer_X, Pointer_Y);
      Free (Name);

      Assert_Cmpuint_Eq (Guint (Enter_Typed_Count), 1);
      Assert_Cmpuint_Eq (Guint (Enter_Slot_Count), 1);
      Assert_Cmpuint_Eq (Guint (Leave_Typed_Count), 1);
      Assert_Cmpuint_Eq (Guint (Leave_Slot_Count), 1);
      Assert_Cmpuint_Eq (Guint (Motion_Typed_Count), 1);
      Assert_Cmpuint_Eq (Guint (Motion_Slot_Count), 1);

      Unref (Controller);
      Unref (Slot);
      Current_Controller := null;
      Expected_Slot := System.Null_Address;
   end Test_Signal_Handlers;

   --  Exceptions from typed and slot handlers are captured for both marshaller
   --  signatures instead of escaping through GLib's C signal emitter.
   procedure Test_Callback_Exception_Containment is
      Controller : constant Gtk_Event_Controller_Motion :=
        Gtk_Event_Controller_Motion_New;
      Slot       : constant Gtk_Event_Controller_Motion :=
        Gtk_Event_Controller_Motion_New;
      Name : chars_ptr;
   begin
      Caught_Exceptions := 0;
      Controller.On_Enter (Raise_Coordinates_Typed'Unrestricted_Access);
      Controller.On_Motion
        (Raise_Coordinates_Slot'Unrestricted_Access, Slot);
      Controller.On_Leave (Raise_Void_Typed'Unrestricted_Access);
      Controller.On_Leave (Raise_Void_Slot'Unrestricted_Access, Slot, After => True);

      --  One emission per failing callback should yield exactly four hook calls.
      Name := New_String (String (Signal_Enter));
      Emit_Coordinates (Get_Object (Controller), Name, Pointer_X, Pointer_Y);
      Free (Name);
      Name := New_String (String (Signal_Motion));
      Emit_Coordinates (Get_Object (Controller), Name, Pointer_X, Pointer_Y);
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
     ("/event-controller-motion/constructors-pointer-state",
      Test_Constructors_And_Pointer_State'Unrestricted_Access);
   Add_Func
     ("/event-controller-motion/signals",
      Test_Signal_Handlers'Unrestricted_Access);
   Add_Func
     ("/event-controller-motion/callback-exceptions",
      Test_Callback_Exception_Containment'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Event_Controller_Motion;
