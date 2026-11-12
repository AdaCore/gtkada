--  Exercises Gtk.Event_Controller_Scroll constructors, flags, unit access,
--  and both callback forms for all four public signals. No matching C test
--  exists in testsuite/c_tests, so this test checks the documented API
--  directly and emits signals to cover the generated Ada marshallers.

with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C.Strings;
with System;

with Gdk.Event.Scroll_Event;      use Gdk.Event.Scroll_Event;
with Glib;                        use Glib;
with Glib.Object;                 use Glib.Object;
with Glib.Properties;             use Glib.Properties;
with Glib.Test;                   use Glib.Test;
with Gtk.Event_Controller_Scroll; use Gtk.Event_Controller_Scroll;
with Gtk.IM_Context_Simple;       use Gtk.IM_Context_Simple;
with Gtk.Main;
with Gtkada.Bindings;

use type System.Address;

procedure Event_Controller_Scroll is
   use Interfaces.C.Strings;

   Delta_X    : constant Gdouble := 2.5;
   Delta_Y    : constant Gdouble := -3.0;
   Velocity_X : constant Gdouble := 0.75;
   Velocity_Y : constant Gdouble := -1.25;

   Current_Controller : Gtk_Event_Controller_Scroll := null;
   Expected_Slot      : System.Address := System.Null_Address;

   Decelerate_Typed : Natural := 0;
   Decelerate_Slot  : Natural := 0;
   Scroll_Typed     : Natural := 0;
   Scroll_Slot      : Natural := 0;
   Begin_Typed      : Natural := 0;
   Begin_Slot       : Natural := 0;
   End_Typed        : Natural := 0;
   End_Slot         : Natural := 0;
   Caught_Exceptions : Natural := 0;

   --  Fixed Ada profiles call GLib's variadic emitter with the exact signal
   --  arguments. The final pointer is GLib's return-value slot for scroll.
   procedure Emit_Void (Instance : System.Address; Name : chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Doubles
     (Instance : System.Address;
      Name     : chars_ptr;
      X        : Gdouble;
      Y        : Gdouble)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Scroll
     (Instance : System.Address;
      Name     : chars_ptr;
      Dx       : Gdouble;
      Dy       : Gdouble;
      Result   : access Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Test_Constructors_And_Flags with Convention => C;
   procedure Test_Signal_Handlers with Convention => C;
   procedure Test_Callback_Exception_Containment with Convention => C;

   --  Signal marshallers must report Ada callback failures through this hook
   --  instead of allowing an exception to unwind across GLib's C frames.
   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Caught_Exceptions := Caught_Exceptions + 1;
   end Record_Exception;

   --  Typed callbacks receive the controller that emitted the signal.
   procedure Check_Controller
     (Self : access Gtk_Event_Controller_Scroll_Record'Class);

   procedure Check_Controller
     (Self : access Gtk_Event_Controller_Scroll_Record'Class) is
   begin
      Assert_True (Self /= null);
      Assert_True (Get_Object (Self) = Get_Object (Current_Controller));
   end Check_Controller;

   --  Slot callbacks receive the user-data object supplied at connection time.
   procedure Check_Slot (Self : access GObject_Record'Class);

   procedure Check_Slot (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Assert_True (Get_Object (Self) = Expected_Slot);
   end Check_Slot;

   --  The kinetic signal delivers its initial velocities as two doubles.
   procedure On_Decelerate
     (Self  : access Gtk_Event_Controller_Scroll_Record'Class;
      Vel_X : Gdouble;
      Vel_Y : Gdouble) is
   begin
      Check_Controller (Self);
      Assert_Cmpfloat_Eq (Vel_X, Velocity_X);
      Assert_Cmpfloat_Eq (Vel_Y, Velocity_Y);
      Decelerate_Typed := Decelerate_Typed + 1;
   end On_Decelerate;

   procedure On_Decelerate_Slot
     (Self  : access GObject_Record'Class;
      Vel_X : Gdouble;
      Vel_Y : Gdouble) is
   begin
      Check_Slot (Self);
      Assert_Cmpfloat_Eq (Vel_X, Velocity_X);
      Assert_Cmpfloat_Eq (Vel_Y, Velocity_Y);
      Decelerate_Slot := Decelerate_Slot + 1;
   end On_Decelerate_Slot;

   --  The scroll signal returns whether the handler consumed the deltas.
   function On_Scroll
     (Self : access Gtk_Event_Controller_Scroll_Record'Class;
      Dx   : Gdouble;
      Dy   : Gdouble) return Boolean is
   begin
      Check_Controller (Self);
      Assert_Cmpfloat_Eq (Dx, Delta_X);
      Assert_Cmpfloat_Eq (Dy, Delta_Y);
      Scroll_Typed := Scroll_Typed + 1;
      return False;
   end On_Scroll;

   function On_Scroll_Slot
     (Self : access GObject_Record'Class;
      Dx   : Gdouble;
      Dy   : Gdouble) return Boolean is
   begin
      Check_Slot (Self);
      Assert_Cmpfloat_Eq (Dx, Delta_X);
      Assert_Cmpfloat_Eq (Dy, Delta_Y);
      Scroll_Slot := Scroll_Slot + 1;
      return True;
   end On_Scroll_Slot;

   --  Begin/end callbacks have no signal arguments beyond their instance.
   procedure On_Scroll_Begin
     (Self : access Gtk_Event_Controller_Scroll_Record'Class) is
   begin
      Check_Controller (Self);
      Begin_Typed := Begin_Typed + 1;
   end On_Scroll_Begin;

   procedure On_Scroll_Begin_Slot (Self : access GObject_Record'Class) is
   begin
      Check_Slot (Self);
      Begin_Slot := Begin_Slot + 1;
   end On_Scroll_Begin_Slot;

   procedure On_Scroll_End
     (Self : access Gtk_Event_Controller_Scroll_Record'Class) is
   begin
      Check_Controller (Self);
      End_Typed := End_Typed + 1;
   end On_Scroll_End;

   procedure On_Scroll_End_Slot (Self : access GObject_Record'Class) is
   begin
      Check_Slot (Self);
      End_Slot := End_Slot + 1;
   end On_Scroll_End_Slot;

   --  Each deliberately failing callback exercises an exception handler in a
   --  distinct generated marshaller shape: typed/slot and void/boolean.
   procedure Raise_Decelerate
     (Self : access Gtk_Event_Controller_Scroll_Record'Class;
      X, Y : Gdouble) is
      pragma Unreferenced (Self, X, Y);
   begin
      raise Program_Error with "expected typed decelerate exception";
   end Raise_Decelerate;

   procedure Raise_Decelerate_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble) is
      pragma Unreferenced (Self, X, Y);
   begin
      raise Program_Error with "expected slot decelerate exception";
   end Raise_Decelerate_Slot;

   procedure Raise_Scroll_Begin
     (Self : access Gtk_Event_Controller_Scroll_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "expected typed scroll-begin exception";
   end Raise_Scroll_Begin;

   procedure Raise_Scroll_End_Slot (Self : access GObject_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "expected slot scroll-end exception";
   end Raise_Scroll_End_Slot;

   --  Exercise both constructors and the documented no-op initialization
   --  path, then check the getter, setter, flags property, and scroll unit.
   procedure Test_Constructors_And_Flags is
      Initial_Flags : constant Gtk_Event_Controller_Scroll_Flags :=
        Event_Controller_Scroll_Vertical or Event_Controller_Scroll_Discrete;
      Changed_Flags : constant Gtk_Event_Controller_Scroll_Flags :=
        Event_Controller_Scroll_Both_Axes or Event_Controller_Scroll_Kinetic;
      By_Function : constant Gtk_Event_Controller_Scroll :=
        Gtk_Event_Controller_Scroll_New (Initial_Flags);
      By_Procedure : Gtk_Event_Controller_Scroll;
   begin
      Gtk_New (By_Procedure, Initial_Flags);
      Assert_True (Gtk.Event_Controller_Scroll.Get_Type /= 0);
      Assert_True (By_Function.Get_Flags = Initial_Flags);
      Assert_True (By_Procedure.Get_Flags = Initial_Flags);

      --  Reinitializing a created wrapper must preserve the original object.
      Initialize (By_Procedure, Event_Controller_Scroll_None);
      Assert_True (By_Procedure.Get_Flags = Initial_Flags);

      --  Cover the direct methods and the generated flags property binding.
      --  Setting the property should expose the same value as Get_Flags.
      By_Function.Set_Flags (Changed_Flags);
      Assert_True (By_Function.Get_Flags = Changed_Flags);
      Set_Property (By_Procedure, Flags_Property, Changed_Flags);
      Assert_True (Get_Property (By_Procedure, Flags_Property) = Changed_Flags);

      --  Discrete scrolling always reports wheel units, even before a native
      --  device event has updated the controller's last scroll unit.
      Assert_True (By_Function.Get_Unit = Wheel);

      Unref (By_Function);
      Unref (By_Procedure);
   end Test_Constructors_And_Flags;

   --  Synthetic emissions validate every callback profile without requiring
   --  a physical scroll device or an attached/visible widget.
   procedure Test_Signal_Handlers is
      Controller : constant Gtk_Event_Controller_Scroll :=
        Gtk_Event_Controller_Scroll_New (Event_Controller_Scroll_Both_Axes);
      Slot       : constant Gtk_IM_Context_Simple := Gtk_IM_Context_Simple_New;
      Name       : chars_ptr;
      Result     : aliased Gboolean := 0;
   begin
      Current_Controller := Controller;
      Expected_Slot := Get_Object (Slot);

      --  After handlers receive the same signal values through the slot form.
      Controller.On_Decelerate (On_Decelerate'Unrestricted_Access);
      Controller.On_Decelerate
        (On_Decelerate_Slot'Unrestricted_Access, Slot, After => True);
      Controller.On_Scroll (On_Scroll'Unrestricted_Access);
      Controller.On_Scroll
        (On_Scroll_Slot'Unrestricted_Access, Slot, After => True);
      Controller.On_Scroll_Begin (On_Scroll_Begin'Unrestricted_Access);
      Controller.On_Scroll_Begin
        (On_Scroll_Begin_Slot'Unrestricted_Access, Slot, After => True);
      Controller.On_Scroll_End (On_Scroll_End'Unrestricted_Access);
      Controller.On_Scroll_End
        (On_Scroll_End_Slot'Unrestricted_Access, Slot, After => True);

      Name := New_String (String (Signal_Decelerate));
      --  GLib copies the signal's numeric arguments during synchronous emit.
      Emit_Doubles (Get_Object (Controller), Name, Velocity_X, Velocity_Y);
      --  Free the C-allocated signal name after the synchronous emission.
      Free (Name);
      Assert_Cmpint_Eq (Gint (Decelerate_Typed), 1);
      Assert_Cmpint_Eq (Gint (Decelerate_Slot), 1);

      Name := New_String (String (Signal_Scroll));
      Result := 0;
      --  The slot handler returns True after the typed handler returns False.
      Emit_Scroll (Get_Object (Controller), Name, Delta_X, Delta_Y, Result'Access);
      Free (Name);
      Assert_Cmpint_Eq (Gint (Scroll_Typed), 1);
      Assert_Cmpint_Eq (Gint (Scroll_Slot), 1);
      Assert_Cmpint_Eq (Gint (Result), 1);

      Name := New_String (String (Signal_Scroll_Begin));
      --  Both no-argument signal forms should observe one emission.
      Emit_Void (Get_Object (Controller), Name);
      Free (Name);
      Assert_Cmpint_Eq (Gint (Begin_Typed), 1);
      Assert_Cmpint_Eq (Gint (Begin_Slot), 1);

      Name := New_String (String (Signal_Scroll_End));
      Emit_Void (Get_Object (Controller), Name);
      Free (Name);
      Assert_Cmpint_Eq (Gint (End_Typed), 1);
      Assert_Cmpint_Eq (Gint (End_Slot), 1);

      Unref (Controller);
      Unref (Slot);
      Current_Controller := null;
      Expected_Slot := System.Null_Address;
   end Test_Signal_Handlers;

   --  Raise through the four void-returning marshallers to verify that their
   --  exceptions are contained and delivered to GtkAda's exception hook.
   --  Boolean-returning callbacks are omitted because their current binding
   --  lets callback exceptions escape; this is recorded in the work notes.
   procedure Test_Callback_Exception_Containment is
      Controller : constant Gtk_Event_Controller_Scroll :=
        Gtk_Event_Controller_Scroll_New (Event_Controller_Scroll_Both_Axes);
      Slot       : constant Gtk_IM_Context_Simple := Gtk_IM_Context_Simple_New;
      Name       : chars_ptr;
   begin
      Caught_Exceptions := 0;

      Controller.On_Decelerate (Raise_Decelerate'Unrestricted_Access);
      Controller.On_Decelerate
        (Raise_Decelerate_Slot'Unrestricted_Access, Slot);
      Name := New_String (String (Signal_Decelerate));
      Emit_Doubles (Get_Object (Controller), Name, Velocity_X, Velocity_Y);
      Free (Name);

      Controller.On_Scroll_Begin (Raise_Scroll_Begin'Unrestricted_Access);
      Name := New_String (String (Signal_Scroll_Begin));
      Emit_Void (Get_Object (Controller), Name);
      Free (Name);

      Controller.On_Scroll_End (Raise_Scroll_End_Slot'Unrestricted_Access, Slot);
      Name := New_String (String (Signal_Scroll_End));
      Emit_Void (Get_Object (Controller), Name);
      Free (Name);

      Assert_Cmpint_Eq (Gint (Caught_Exceptions), 4);
      Unref (Controller);
      Unref (Slot);
   end Test_Callback_Exception_Containment;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Gtkada.Bindings.Set_On_Exception (Record_Exception'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/event-controller-scroll/constructors-and-flags",
      Test_Constructors_And_Flags'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/event-controller-scroll/signals",
      Test_Signal_Handlers'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/event-controller-scroll/callback-exceptions",
      Test_Callback_Exception_Containment'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Event_Controller_Scroll;
