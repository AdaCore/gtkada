--  Exercises the gesture bindings and Gtk.Widget.Add_Controller.
--
--  Driving real pointer input is out of reach here, so what is checked is
--  the part that the bindings are responsible for: that a gesture can be
--  built, configured, attached to a widget and detached again, that the
--  controller reports the widget it was attached to, and that a gesture
--  which has seen no input reports itself inactive. Signal tests cover
--  the null event sequence used for mouse input and opaque touch tokens.
--  Long-press cases additionally cover construction, delay-factor access and
--  both callback forms for the ::cancelled and ::pressed signals.
--  Click cases also exercise both constructors, all four signals, both
--  callback forms and exception routing for every generated click marshaller.
--  Pan cases check orientation construction/property access and both callback
--  forms for ::pan, including exception routing through the generated handlers.
--  Zoom cases check both constructors, the inactive scale delta, Initialize's
--  idempotence, and both callback forms for ::scale-changed.
--  Drag cases also check both constructors, idle coordinate queries, and both
--  callback forms for each drag signal, including callback exception routing.
--  Single-gesture checks cover idle input queries and exclusive/touch-only
--  settings, whose setters have no direct-input dependency.

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with System;
with Gdk.Device;
with Gdk.Event;          use Gdk.Event;
with Gdk.Rectangle;      use Gdk.Rectangle;
with Glib.Object;        use Glib.Object;
with Glib.Values;        use Glib.Values;
with Gtk.Arguments;
with Glib;               use Glib;
with Glib.Test;          use Glib.Test;
with Gtk.Drawing_Area;   use Gtk.Drawing_Area;
with Gtk.Event_Controller; use Gtk.Event_Controller;
with Gtk.Gesture;        use Gtk.Gesture;
with Gtk.Gesture_Click;  use Gtk.Gesture_Click;
with Gtk.Gesture_Drag;   use Gtk.Gesture_Drag;
with Gtk.Gesture_Long_Press; use Gtk.Gesture_Long_Press;
with Gtk.Gesture_Pan; use Gtk.Gesture_Pan;
with Gtk.Gesture_Rotate; use Gtk.Gesture_Rotate;
with Gtk.Gesture_Swipe; use Gtk.Gesture_Swipe;
with Gtk.Gesture_Single; use Gtk.Gesture_Single;
with Gtk.Gesture_Zoom;  use Gtk.Gesture_Zoom;
with Gtk.Enums;          use Gtk.Enums;
with Glib.Properties;
with Gtkada.Bindings;
with Gtk.Main;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.Window;         use Gtk.Window;

procedure Gesture is

   use type System.Address;
   use type Gdk.Gdk_Device;
   use type Gesture_List.Glist;

   function Address_To_Sequence_Proxy is new Ada.Unchecked_Conversion
     (System.Address, Glib.C_Proxy);

   Button_Secondary : constant Guint := 3;
   --  GDK_BUTTON_SECONDARY; the GDK_BUTTON_* constants are not bound.

   procedure Test_Drag
   with Convention => C;

   procedure Test_Drag_Signals with Convention => C;
   procedure Test_Drag_Callback_Exceptions with Convention => C;

   procedure Test_Click
   with Convention => C;

   procedure Test_Single
   with Convention => C;

   procedure Test_Remove
   with Convention => C;

   procedure Test_Sequence_Conversion with Convention => C;
   procedure Test_Mouse_Sequence_Signals with Convention => C;
   procedure Test_Long_Press with Convention => C;
   procedure Test_Long_Press_Signals with Convention => C;
   procedure Test_Long_Press_Callback_Exceptions with Convention => C;
   procedure Test_Swipe with Convention => C;
   procedure Test_Swipe_Signals with Convention => C;
   procedure Test_Swipe_Callback_Exceptions with Convention => C;
   procedure Test_Pan with Convention => C;
   procedure Test_Pan_Signals with Convention => C;
   procedure Test_Pan_Callback_Exceptions with Convention => C;
   procedure Test_Rotate with Convention => C;
   procedure Test_Rotate_Signals with Convention => C;
   procedure Test_Zoom with Convention => C;
   procedure Test_Zoom_Signals with Convention => C;
   procedure Test_Zoom_Callback_Exceptions with Convention => C;
   procedure Test_Click_Signals with Convention => C;
   procedure Test_Click_Callback_Exceptions with Convention => C;
   procedure Test_Base_Queries with Convention => C;
   procedure Test_Base_Grouping with Convention => C;
   procedure Test_Base_Signals with Convention => C;
   procedure Test_Base_Callback_Exceptions with Convention => C;

   Typed_Calls, Slot_Calls : Natural := 0;
   Long_Press : Gtk_Gesture_Long_Press;
   Swipe : Gtk_Gesture_Swipe;
   Pan_Signal_Gesture : Gtk_Gesture_Pan;
   Drag_Signal_Gesture : Gtk_Gesture_Drag;
   --  Retain the emitter so both callback forms can validate the source.
   Rotate_Signal_Gesture : Gtk_Gesture_Rotate;
   Zoom_Signal_Gesture : Gtk_Gesture_Zoom;
   Signal_Click : Gtk_Gesture_Click;
   Cancelled_Typed_Calls : Natural := 0;
   Cancelled_Slot_Calls  : Natural := 0;
   Pressed_Typed_Calls   : Natural := 0;
   Pressed_Slot_Calls    : Natural := 0;
   Swipe_Typed_Calls : Natural := 0;
   Swipe_Slot_Calls : Natural := 0;
   Pan_Typed_Calls : Natural := 0;
   Pan_Slot_Calls : Natural := 0;
   Drag_Typed_Calls : Natural := 0;
   Drag_Slot_Calls : Natural := 0;
   --  Independent counters make both connections observable in the test.
   Rotate_Typed_Calls : Natural := 0;
   Rotate_Slot_Calls : Natural := 0;
   Zoom_Typed_Calls : Natural := 0;
   Zoom_Slot_Calls : Natural := 0;
   Callback_Exception_Calls : Natural := 0;
   Click_Pressed_Typed_Calls  : Natural := 0;
   Click_Pressed_Slot_Calls   : Natural := 0;
   Click_Released_Typed_Calls : Natural := 0;
   Click_Released_Slot_Calls  : Natural := 0;
   Click_Stopped_Typed_Calls  : Natural := 0;
   Click_Stopped_Slot_Calls   : Natural := 0;
   Click_Unpaired_Typed_Calls : Natural := 0;
   Click_Unpaired_Slot_Calls  : Natural := 0;
   Base_Gesture : Gtk_Gesture;
   Base_Sequence : Gdk_Event_Sequence;
   Base_Typed_Calls : Natural := 0;
   Base_Slot_Calls : Natural := 0;

   --  Emit signals directly so callback tests do not depend on pointer input.
   procedure Emit_Void
     (Object : System.Address;
      Name   : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Coordinates
     (Object : System.Address;
      Name   : Interfaces.C.Strings.chars_ptr;
      X, Y   : Gdouble)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   --  The Zoom signal carries one scale value.
   procedure Emit_Scale
     (Object : System.Address;
      Name   : Interfaces.C.Strings.chars_ptr;
      Scale  : Gdouble)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   --  ::pan carries an enum and a double after the signal name.
   procedure Emit_Pan
     (Object : System.Address;
      Name   : Interfaces.C.Strings.chars_ptr;
      Direction : Gtk_Pan_Direction;
      Offset : Gdouble)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   --  The pressed and released signals take an integer and two coordinates.
   procedure Emit_Click_Press
     (Object : System.Address;
      Name   : Interfaces.C.Strings.chars_ptr;
      Count  : Gint;
      X, Y   : Gdouble)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   --  The unpaired-release signal also accepts a nullable sequence pointer.
   procedure Emit_Click_Unpaired
     (Object   : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr;
      X, Y     : Gdouble;
      Button   : Guint;
      Sequence : System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   --  Base gesture signals carry a nullable sequence pointer; state-changed
   --  additionally carries the GtkEventSequenceState enum.
   procedure Emit_Sequence
     (Object : System.Address;
      Name : Interfaces.C.Strings.chars_ptr;
      Sequence : System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Sequence_State
     (Object : System.Address;
      Name : Interfaces.C.Strings.chars_ptr;
      Sequence : System.Address;
      State : Gtk_Event_Sequence_State)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure On_Cancelled_Typed
     (Self : access Gtk_Gesture_Long_Press_Record'Class);
   procedure On_Cancelled_Slot
     (Self : access GObject_Record'Class);
   procedure On_Pressed_Typed
     (Self : access Gtk_Gesture_Long_Press_Record'Class;
      X, Y : Gdouble);
   procedure On_Pressed_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble);
   procedure On_Swipe_Typed
     (Self : access Gtk_Gesture_Swipe_Record'Class;
      X, Y : Gdouble);
   procedure On_Swipe_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble);
   procedure Raise_Swipe_Typed
     (Self : access Gtk_Gesture_Swipe_Record'Class;
      X, Y : Gdouble);
   procedure Raise_Swipe_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble);
   procedure On_Pan_Typed
     (Self : access Gtk_Gesture_Pan_Record'Class;
      Direction : Gtk_Pan_Direction;
      Offset : Gdouble);
   procedure On_Pan_Slot
     (Self : access GObject_Record'Class;
      Direction : Gtk_Pan_Direction;
      Offset : Gdouble);
   procedure Raise_Pan_Typed
     (Self : access Gtk_Gesture_Pan_Record'Class;
      Direction : Gtk_Pan_Direction;
      Offset : Gdouble);
   procedure Raise_Pan_Slot
     (Self : access GObject_Record'Class;
      Direction : Gtk_Pan_Direction;
      Offset : Gdouble);
   --  These distinct receiver types select GtkAda's typed and slot overloads.
   procedure On_Rotate_Typed
     (Self : access Gtk_Gesture_Rotate_Record'Class;
      Angle, Angle_Delta : Gdouble);
   --  The slot overload receives the explicitly supplied object instead.
   procedure On_Rotate_Slot
     (Self : access GObject_Record'Class;
      Angle, Angle_Delta : Gdouble);
   --  These callbacks verify the scale value and recover the emitter or slot.
   procedure On_Zoom_Typed
     (Self : access Gtk_Gesture_Zoom_Record'Class;
      Scale : Gdouble);
   procedure On_Zoom_Slot
     (Self : access GObject_Record'Class;
      Scale : Gdouble);
   procedure Raise_Rotate_Typed
     (Self : access Gtk_Gesture_Rotate_Record'Class;
      Angle, Angle_Delta : Gdouble);
   procedure Raise_Rotate_Slot
     (Self : access GObject_Record'Class;
      Angle, Angle_Delta : Gdouble);
   --  Raising callbacks exercise exception handling in both Zoom marshallers.
   procedure Raise_Zoom_Typed
     (Self : access Gtk_Gesture_Zoom_Record'Class;
      Scale : Gdouble);
   procedure Raise_Zoom_Slot
     (Self : access GObject_Record'Class;
      Scale : Gdouble);
   procedure Raise_Cancelled_Typed
     (Self : access Gtk_Gesture_Long_Press_Record'Class);
   procedure Raise_Cancelled_Slot
     (Self : access GObject_Record'Class);
   procedure Raise_Pressed_Typed
     (Self : access Gtk_Gesture_Long_Press_Record'Class;
      X, Y : Gdouble);
   procedure Raise_Pressed_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble);

   --  Click signal callbacks check reconstructed parameters and slot data.
   procedure On_Click_Pressed_Typed
     (Self : access Gtk_Gesture_Click_Record'Class;
      Count : Gint;
      X, Y : Gdouble);
   procedure On_Click_Pressed_Slot
     (Self : access GObject_Record'Class;
      Count : Gint;
      X, Y : Gdouble);
   procedure On_Click_Released_Typed
     (Self : access Gtk_Gesture_Click_Record'Class;
      Count : Gint;
      X, Y : Gdouble);
   procedure On_Click_Released_Slot
     (Self : access GObject_Record'Class;
      Count : Gint;
      X, Y : Gdouble);
   procedure On_Click_Stopped_Typed
     (Self : access Gtk_Gesture_Click_Record'Class);
   procedure On_Click_Stopped_Slot
     (Self : access GObject_Record'Class);
   procedure On_Click_Unpaired_Typed
     (Self : access Gtk_Gesture_Click_Record'Class;
      X, Y : Gdouble;
      Button : Guint;
      Sequence : Gdk_Event_Sequence);
   procedure On_Click_Unpaired_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble;
      Button : Guint;
      Sequence : Gdk_Event_Sequence);

   --  These handlers deliberately fail so each click marshaller's exception
   --  handler can be checked through the configured GtkAda exception hook.
   procedure Raise_Click_Pressed_Typed
     (Self : access Gtk_Gesture_Click_Record'Class;
      Count : Gint;
      X, Y : Gdouble);
   procedure Raise_Click_Pressed_Slot
     (Self : access GObject_Record'Class;
      Count : Gint;
      X, Y : Gdouble);
   procedure Raise_Click_Stopped_Typed
     (Self : access Gtk_Gesture_Click_Record'Class);
   procedure Raise_Click_Stopped_Slot
     (Self : access GObject_Record'Class);
   procedure Raise_Click_Unpaired_Typed
     (Self : access Gtk_Gesture_Click_Record'Class;
      X, Y : Gdouble;
      Button : Guint;
      Sequence : Gdk_Event_Sequence);
   procedure Raise_Click_Unpaired_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble;
      Button : Guint;
      Sequence : Gdk_Event_Sequence);

   --  A callback exception must be routed to GtkAda's hook instead of
   --  unwinding through the C signal dispatcher.
   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   --  The same signatures are used by begin, cancel, end and update.
   procedure On_Base_Sequence
     (Self : access Gtk_Gesture_Record'Class;
      Sequence : Gdk_Event_Sequence);
   procedure On_Base_Sequence_Slot
     (Self : access GObject_Record'Class;
      Sequence : Gdk_Event_Sequence);
   procedure On_Base_State
     (Self : access Gtk_Gesture_Record'Class;
      Sequence : Gdk_Event_Sequence;
      State : Gtk_Event_Sequence_State);
   procedure On_Base_State_Slot
     (Self : access GObject_Record'Class;
      Sequence : Gdk_Event_Sequence;
      State : Gtk_Event_Sequence_State);

   --  Drag's three signals share one typed and one slot callback signature.
   procedure On_Drag_Typed
     (Self : access Gtk_Gesture_Drag_Record'Class;
      X, Y : Gdouble);
   procedure On_Drag_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble);
   procedure Raise_Drag_Typed
     (Self : access Gtk_Gesture_Drag_Record'Class;
      X, Y : Gdouble);
   procedure Raise_Drag_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble);

   --  Throwing handlers cover the generated marshallers' exception branches.
   procedure Raise_Base_Sequence
     (Self : access Gtk_Gesture_Record'Class;
      Sequence : Gdk_Event_Sequence);
   procedure Raise_Base_Sequence_Slot
     (Self : access GObject_Record'Class;
      Sequence : Gdk_Event_Sequence);
   procedure Raise_Base_State
     (Self : access Gtk_Gesture_Record'Class;
      Sequence : Gdk_Event_Sequence;
      State : Gtk_Event_Sequence_State);
   procedure Raise_Base_State_Slot
     (Self : access GObject_Record'Class;
      Sequence : Gdk_Event_Sequence;
      State : Gtk_Event_Sequence_State);

   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Callback_Exception_Calls := Callback_Exception_Calls + 1;
   end Record_Callback_Exception;

   --  Each handler validates both the emitting object and converted sequence.
   procedure On_Base_Sequence
     (Self : access Gtk_Gesture_Record'Class;
      Sequence : Gdk_Event_Sequence) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Base_Gesture));
      Assert_True (Sequence = Base_Sequence);
      Base_Typed_Calls := Base_Typed_Calls + 1;
   end On_Base_Sequence;

   --  Slot callbacks receive the explicit slot object instead of the gesture.
   procedure On_Base_Sequence_Slot
     (Self : access GObject_Record'Class;
      Sequence : Gdk_Event_Sequence) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Base_Gesture));
      Assert_True (Sequence = Base_Sequence);
      Base_Slot_Calls := Base_Slot_Calls + 1;
   end On_Base_Sequence_Slot;

   --  State callbacks also check the enum conversion performed by the binding.
   procedure On_Base_State
     (Self : access Gtk_Gesture_Record'Class;
      Sequence : Gdk_Event_Sequence;
      State : Gtk_Event_Sequence_State) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Base_Gesture));
      Assert_True (Sequence = Base_Sequence);
      Assert_True (State = Event_Sequence_Claimed);
      Base_Typed_Calls := Base_Typed_Calls + 1;
   end On_Base_State;

   --  Verify the slot overload receives the same sequence and state values.
   procedure On_Base_State_Slot
     (Self : access GObject_Record'Class;
      Sequence : Gdk_Event_Sequence;
      State : Gtk_Event_Sequence_State) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Base_Gesture));
      Assert_True (Sequence = Base_Sequence);
      Assert_True (State = Event_Sequence_Claimed);
      Base_Slot_Calls := Base_Slot_Calls + 1;
   end On_Base_State_Slot;

   --  Each raising callback has a separate signature for one marshaller form.
   procedure Raise_Base_Sequence
     (Self : access Gtk_Gesture_Record'Class;
      Sequence : Gdk_Event_Sequence) is
      pragma Unreferenced (Self, Sequence);
   begin
      raise Program_Error with "expected base gesture callback exception";
   end Raise_Base_Sequence;

   procedure Raise_Base_Sequence_Slot
     (Self : access GObject_Record'Class;
      Sequence : Gdk_Event_Sequence) is
      pragma Unreferenced (Self, Sequence);
   begin
      raise Program_Error with "expected base gesture callback exception";
   end Raise_Base_Sequence_Slot;

   procedure Raise_Base_State
     (Self : access Gtk_Gesture_Record'Class;
      Sequence : Gdk_Event_Sequence;
      State : Gtk_Event_Sequence_State) is
      pragma Unreferenced (Self, Sequence, State);
   begin
      raise Program_Error with "expected base gesture callback exception";
   end Raise_Base_State;

   procedure Raise_Base_State_Slot
     (Self : access GObject_Record'Class;
      Sequence : Gdk_Event_Sequence;
      State : Gtk_Event_Sequence_State) is
      pragma Unreferenced (Self, Sequence, State);
   begin
      raise Program_Error with "expected base gesture callback exception";
   end Raise_Base_State_Slot;

   --  The callbacks check the converted coordinates and the receiver object.
   procedure On_Drag_Typed
     (Self : access Gtk_Gesture_Drag_Record'Class;
      X, Y : Gdouble) is
   begin
      Assert_True
        (Get_Object (Self) = Get_Object (Drag_Signal_Gesture));
      Assert_Cmpfloat_Eq (X, 12.5);
      Assert_Cmpfloat_Eq (Y, 34.25);
      Drag_Typed_Calls := Drag_Typed_Calls + 1;
   end On_Drag_Typed;

   --  Slot callbacks receive the explicitly supplied GObject as their object.
   procedure On_Drag_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble) is
   begin
      Assert_True
        (Get_Object (Self) = Get_Object (Drag_Signal_Gesture));
      Assert_Cmpfloat_Eq (X, 12.5);
      Assert_Cmpfloat_Eq (Y, 34.25);
      Drag_Slot_Calls := Drag_Slot_Calls + 1;
   end On_Drag_Slot;

   --  Raising callbacks cover the exception path in each generated
   --  marshaller while GtkAda's exception hook keeps it inside Ada.
   procedure Raise_Drag_Typed
     (Self : access Gtk_Gesture_Drag_Record'Class;
      X, Y : Gdouble) is
      pragma Unreferenced (Self, X, Y);
   begin
      raise Program_Error with "expected drag callback exception";
   end Raise_Drag_Typed;

   procedure Raise_Drag_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble) is
      pragma Unreferenced (Self, X, Y);
   begin
      raise Program_Error with "expected drag callback exception";
   end Raise_Drag_Slot;

   --  Each callback checks the emitting object and values reconstructed by
   --  the generated marshaller before counting the invocation.
   procedure On_Cancelled_Typed
     (Self : access Gtk_Gesture_Long_Press_Record'Class) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Long_Press));
      Cancelled_Typed_Calls := Cancelled_Typed_Calls + 1;
   end On_Cancelled_Typed;

   procedure On_Cancelled_Slot
     (Self : access GObject_Record'Class) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Long_Press));
      Cancelled_Slot_Calls := Cancelled_Slot_Calls + 1;
   end On_Cancelled_Slot;

   procedure On_Pressed_Typed
     (Self : access Gtk_Gesture_Long_Press_Record'Class;
      X, Y : Gdouble) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Long_Press));
      Assert_Cmpfloat_Eq (X, 12.5);
      Assert_Cmpfloat_Eq (Y, 34.25);
      Pressed_Typed_Calls := Pressed_Typed_Calls + 1;
   end On_Pressed_Typed;

   procedure On_Pressed_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Long_Press));
      Assert_Cmpfloat_Eq (X, 12.5);
      Assert_Cmpfloat_Eq (Y, 34.25);
      Pressed_Slot_Calls := Pressed_Slot_Calls + 1;
   end On_Pressed_Slot;

   --  Swipe's typed signal callback must recover the concrete gesture and its
   --  two velocity values from the GValues supplied by GObject.
   procedure On_Swipe_Typed
     (Self : access Gtk_Gesture_Swipe_Record'Class;
      X, Y : Gdouble) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Swipe));
      Assert_Cmpfloat_Eq (X, 120.5);
      Assert_Cmpfloat_Eq (Y, -45.25);
      Swipe_Typed_Calls := Swipe_Typed_Calls + 1;
   end On_Swipe_Typed;

   --  The slot overload receives its explicit GObject data and the same
   --  velocity payload, through the generic-object generated marshaller.
   procedure On_Swipe_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Swipe));
      Assert_Cmpfloat_Eq (X, 120.5);
      Assert_Cmpfloat_Eq (Y, -45.25);
      Swipe_Slot_Calls := Swipe_Slot_Calls + 1;
   end On_Swipe_Slot;

   --  Raising in each callback form covers the generated exception routing
   --  without allowing Ada exceptions to unwind through the C signal code.
   procedure Raise_Swipe_Typed
     (Self : access Gtk_Gesture_Swipe_Record'Class;
      X, Y : Gdouble) is
      pragma Unreferenced (Self, X, Y);
   begin
      raise Program_Error with "expected swipe callback exception";
   end Raise_Swipe_Typed;

   procedure Raise_Swipe_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble) is
      pragma Unreferenced (Self, X, Y);
   begin
      raise Program_Error with "expected swipe callback exception";
   end Raise_Swipe_Slot;

   --  The typed callback verifies the pan object and both decoded arguments.
   procedure On_Pan_Typed
     (Self : access Gtk_Gesture_Pan_Record'Class;
      Direction : Gtk_Pan_Direction;
      Offset : Gdouble) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Pan_Signal_Gesture));
      Assert_True (Direction = Pan_Direction_Left);
      Assert_Cmpfloat_Eq (Offset, 17.25);
      Pan_Typed_Calls := Pan_Typed_Calls + 1;
   end On_Pan_Typed;

   --  The slot form must receive the explicit GObject supplied to On_Pan.
   procedure On_Pan_Slot
     (Self : access GObject_Record'Class;
      Direction : Gtk_Pan_Direction;
      Offset : Gdouble) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Pan_Signal_Gesture));
      Assert_True (Direction = Pan_Direction_Left);
      Assert_Cmpfloat_Eq (Offset, 17.25);
      Pan_Slot_Calls := Pan_Slot_Calls + 1;
   end On_Pan_Slot;

   --  Failing handlers exercise each generated pan marshaller's exception
   --  block, which reports the exception through GtkAda's configured hook.
   procedure Raise_Pan_Typed
     (Self : access Gtk_Gesture_Pan_Record'Class;
      Direction : Gtk_Pan_Direction;
      Offset : Gdouble) is
      pragma Unreferenced (Self, Direction, Offset);
   begin
      raise Program_Error with "expected pan callback exception";
   end Raise_Pan_Typed;

   procedure Raise_Pan_Slot
     (Self : access GObject_Record'Class;
      Direction : Gtk_Pan_Direction;
      Offset : Gdouble) is
      pragma Unreferenced (Self, Direction, Offset);
   begin
      raise Program_Error with "expected pan callback exception";
   end Raise_Pan_Slot;

   --  These four callbacks deliberately fail to cover the generated
   --  marshallers' exception handlers for both signatures and slot forms.
   procedure Raise_Cancelled_Typed
     (Self : access Gtk_Gesture_Long_Press_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "expected long-press callback exception";
   end Raise_Cancelled_Typed;

   procedure Raise_Cancelled_Slot
     (Self : access GObject_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "expected long-press callback exception";
   end Raise_Cancelled_Slot;

   procedure Raise_Pressed_Typed
     (Self : access Gtk_Gesture_Long_Press_Record'Class;
      X, Y : Gdouble) is
      pragma Unreferenced (Self, X, Y);
   begin
      raise Program_Error with "expected long-press callback exception";
   end Raise_Pressed_Typed;

   procedure Raise_Pressed_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble) is
      pragma Unreferenced (Self, X, Y);
   begin
      raise Program_Error with "expected long-press callback exception";
   end Raise_Pressed_Slot;

   --  The typed form must recover the concrete GtkGestureClick object.
   --  Count and coordinates check every non-object argument of ::pressed.
   procedure On_Click_Pressed_Typed
     (Self : access Gtk_Gesture_Click_Record'Class;
      Count : Gint;
      X, Y : Gdouble) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Signal_Click));
      Assert_Cmpint_Eq (Count, 2);
      Assert_Cmpfloat_Eq (X, 12.5);
      Assert_Cmpfloat_Eq (Y, 34.25);
      Click_Pressed_Typed_Calls := Click_Pressed_Typed_Calls + 1;
   end On_Click_Pressed_Typed;

   --  The slot form receives the explicit GObject passed at connection time.
   procedure On_Click_Pressed_Slot
     (Self : access GObject_Record'Class;
      Count : Gint;
      X, Y : Gdouble) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Signal_Click));
      Assert_Cmpint_Eq (Count, 2);
      Assert_Cmpfloat_Eq (X, 12.5);
      Assert_Cmpfloat_Eq (Y, 34.25);
      Click_Pressed_Slot_Calls := Click_Pressed_Slot_Calls + 1;
   end On_Click_Pressed_Slot;

   --  ::released has the same payload shape, but its own connection wrapper.
   procedure On_Click_Released_Typed
     (Self : access Gtk_Gesture_Click_Record'Class;
      Count : Gint;
      X, Y : Gdouble) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Signal_Click));
      Assert_Cmpint_Eq (Count, 2);
      Assert_Cmpfloat_Eq (X, 12.5);
      Assert_Cmpfloat_Eq (Y, 34.25);
      Click_Released_Typed_Calls := Click_Released_Typed_Calls + 1;
   end On_Click_Released_Typed;

   --  Check that the release slot callback gets the same emitted values.
   procedure On_Click_Released_Slot
     (Self : access GObject_Record'Class;
      Count : Gint;
      X, Y : Gdouble) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Signal_Click));
      Assert_Cmpint_Eq (Count, 2);
      Assert_Cmpfloat_Eq (X, 12.5);
      Assert_Cmpfloat_Eq (Y, 34.25);
      Click_Released_Slot_Calls := Click_Released_Slot_Calls + 1;
   end On_Click_Released_Slot;

   --  ::stopped has no payload; this checks its typed callback conversion.
   procedure On_Click_Stopped_Typed
     (Self : access Gtk_Gesture_Click_Record'Class) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Signal_Click));
      Click_Stopped_Typed_Calls := Click_Stopped_Typed_Calls + 1;
   end On_Click_Stopped_Typed;

   --  The slot overload must still use the registered GObject as callback data.
   procedure On_Click_Stopped_Slot
     (Self : access GObject_Record'Class) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Signal_Click));
      Click_Stopped_Slot_Calls := Click_Stopped_Slot_Calls + 1;
   end On_Click_Stopped_Slot;

   --  The typed unpaired-release marshaller converts four values, including
   --  a NULL sequence representing a mouse event in this direct emission.
   procedure On_Click_Unpaired_Typed
     (Self : access Gtk_Gesture_Click_Record'Class;
      X, Y : Gdouble;
      Button : Guint;
      Sequence : Gdk_Event_Sequence) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Signal_Click));
      Assert_Cmpfloat_Eq (X, 4.5);
      Assert_Cmpfloat_Eq (Y, 8.25);
      Assert_Cmpuint_Eq (Button, 3);
      Assert_True (Sequence = null);
      Click_Unpaired_Typed_Calls := Click_Unpaired_Typed_Calls + 1;
   end On_Click_Unpaired_Typed;

   --  The slot version checks that the nullable sequence survives conversion.
   procedure On_Click_Unpaired_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble;
      Button : Guint;
      Sequence : Gdk_Event_Sequence) is
   begin
      Assert_True (Get_Object (Self) = Get_Object (Signal_Click));
      Assert_Cmpfloat_Eq (X, 4.5);
      Assert_Cmpfloat_Eq (Y, 8.25);
      Assert_Cmpuint_Eq (Button, 3);
      Assert_True (Sequence = null);
      Click_Unpaired_Slot_Calls := Click_Unpaired_Slot_Calls + 1;
   end On_Click_Unpaired_Slot;

   --  Trigger the generated marshaller exception handlers without letting an
   --  Ada exception cross the C signal dispatcher.
   --  Use the same signature for both ::pressed and ::released callbacks.
   procedure Raise_Click_Pressed_Typed
     (Self : access Gtk_Gesture_Click_Record'Class;
      Count : Gint;
      X, Y : Gdouble) is
      pragma Unreferenced (Self, Count, X, Y);
   begin
      raise Program_Error with "expected click callback exception";
   end Raise_Click_Pressed_Typed;

   --  Its slot counterpart covers the generic-object signal marshaller.
   procedure Raise_Click_Pressed_Slot
     (Self : access GObject_Record'Class;
      Count : Gint;
      X, Y : Gdouble) is
      pragma Unreferenced (Self, Count, X, Y);
   begin
      raise Program_Error with "expected click callback exception";
   end Raise_Click_Pressed_Slot;

   --  These no-argument handlers exercise the ::stopped exception paths.
   procedure Raise_Click_Stopped_Typed
     (Self : access Gtk_Gesture_Click_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "expected click callback exception";
   end Raise_Click_Stopped_Typed;

   --  The slot variant covers the stopped marshaller's generic callback.
   procedure Raise_Click_Stopped_Slot
     (Self : access GObject_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "expected click callback exception";
   end Raise_Click_Stopped_Slot;

   --  The multi-argument callbacks make its exception path run after decoding
   --  coordinates, button number and a nullable event sequence.
   procedure Raise_Click_Unpaired_Typed
     (Self : access Gtk_Gesture_Click_Record'Class;
      X, Y : Gdouble;
      Button : Guint;
      Sequence : Gdk_Event_Sequence) is
      pragma Unreferenced (Self, X, Y, Button, Sequence);
   begin
      raise Program_Error with "expected click callback exception";
   end Raise_Click_Unpaired_Typed;

   --  The generic-object form has a distinct marshaller and exception block.
   procedure Raise_Click_Unpaired_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble;
      Button : Guint;
      Sequence : Gdk_Event_Sequence) is
      pragma Unreferenced (Self, X, Y, Button, Sequence);
   begin
      raise Program_Error with "expected click callback exception";
   end Raise_Click_Unpaired_Slot;

   procedure On_Sequence
     (Self : access Gtk_Gesture_Record'Class;
      Sequence : Gdk_Event_Sequence)
   is
      pragma Unreferenced (Self);
   begin
      Assert_True (Sequence = null);
      Typed_Calls := Typed_Calls + 1;
   end On_Sequence;

   procedure On_Slot_Sequence
     (Self : access GObject_Record'Class;
      Sequence : Gdk_Event_Sequence)
   is
      pragma Unreferenced (Self);
   begin
      Assert_True (Sequence = null);
      Slot_Calls := Slot_Calls + 1;
   end On_Slot_Sequence;

   procedure Test_Sequence_Conversion is
      Value : aliased GValue;
      Token : aliased Gint := 42;
      function To_Proxy is new Ada.Unchecked_Conversion
        (System.Address, C_Proxy);
   begin
      Init (Value, GType_Pointer);
      Set_Proxy (Value, null);
      Assert_True
        (Gtk.Arguments.Unchecked_To_Gdk_Event_Sequence
           (C_GValues (Value'Address), 0) = null);

      --  A sequence is the pointer itself, not the data it points to.
      Set_Proxy (Value, To_Proxy (Token'Address));
      Assert_True
        (Gtk.Arguments.Unchecked_To_Gdk_Event_Sequence
           (C_GValues (Value'Address), 0) =
         Gdk_Event_Sequence (C_Proxy'(To_Proxy (Token'Address))));
      Unset (Value);
   end Test_Sequence_Conversion;

   procedure Test_Mouse_Sequence_Signals is
      Drag : constant Gtk_Gesture_Drag := Gtk_Gesture_Drag_New;
      G    : constant Gtk_Gesture := Gtk_Gesture (Drag);

      procedure Emit
        (Instance : System.Address;
         Name : Interfaces.C.Strings.chars_ptr;
         Sequence : Gdk_Event_Sequence)
      with Import, Convention => C_Variadic_2,
           External_Name => "g_signal_emit_by_name";

      procedure Emit_Null (Signal_Name : String) is
         Name : Interfaces.C.Strings.chars_ptr :=
           Interfaces.C.Strings.New_String (Signal_Name);
      begin
         Emit (Get_Object (G), Name, null);
         Interfaces.C.Strings.Free (Name);
      end Emit_Null;
   begin
      Typed_Calls := 0;
      Slot_Calls := 0;
      G.On_End (On_Sequence'Unrestricted_Access);
      G.On_End (On_Slot_Sequence'Unrestricted_Access, Slot => G);
      G.On_Cancel (On_Sequence'Unrestricted_Access);
      G.On_Cancel (On_Slot_Sequence'Unrestricted_Access, Slot => G);

      --  Mouse releases and cancellations use NULL rather than a touch token.
      Emit_Null ("end");
      Emit_Null ("cancel");
      Assert_Cmpint_Eq (Gint (Typed_Calls), 2);
      Assert_Cmpint_Eq (Gint (Slot_Calls), 2);
      G.Unref;
   end Test_Mouse_Sequence_Signals;

   ---------------
   -- Test_Drag --
   ---------------

   procedure Test_Drag is
      Window : constant Gtk_Window := Gtk_Window_New;
      Area   : constant Gtk_Drawing_Area := Gtk_Drawing_Area_New;
      Drag   : constant Gtk_Gesture_Drag := Gtk_Gesture_Drag_New;
      By_Procedure : Gtk_Gesture_Drag;
      Manual : constant Gtk_Gesture_Drag := new Gtk_Gesture_Drag_Record;
      Original_Object : System.Address;
      X, Y : aliased Gdouble := 0.0;
   begin
      --  Exercise both public constructor styles and the idempotent guard on
      --  Initialize for a caller-allocated Ada wrapper record.
      Gtk_New (By_Procedure);
      Gtk.Gesture_Drag.Initialize (Manual);
      Original_Object := Get_Object (Manual);
      Gtk.Gesture_Drag.Initialize (Manual);
      Assert_True (Get_Object (Drag) /= System.Null_Address);
      Assert_True (Get_Object (By_Procedure) /= System.Null_Address);
      Assert_True (Get_Object (Manual) = Original_Object);

      --  Without a pointer sequence both documented coordinate queries are
      --  inactive and return False; real input is intentionally not required.
      Assert_False (Drag.Get_Offset (X'Access, Y'Access));
      Assert_False (Drag.Get_Start_Point (X'Access, Y'Access));

      Window.Set_Child (Area);

      --  0 means "any button".
      Gtk_Gesture_Single (Drag).Set_Button (0);
      Assert_Cmpuint_Eq (Gtk_Gesture_Single (Drag).Get_Button, 0);

      --  Not attached yet, so it belongs to no widget.
      Assert_True (Gtk_Event_Controller (Drag).Get_Widget = null);

      Area.Add_Controller (Drag);
      Assert_True
        (Gtk_Event_Controller (Drag).Get_Widget = Gtk_Widget (Area));

      --  Nothing has been pressed, so the gesture is idle.
      Assert_True (not Gtk_Gesture (Drag).Is_Active);
      Assert_True (not Gtk_Gesture (Drag).Is_Recognized);
   end Test_Drag;

   --  Direct signal emissions check all three generated connectors and both
   --  marshallers while keeping the coordinates deterministic.
   procedure Test_Drag_Signals is
      Begin_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (String (Signal_Drag_Begin));
      End_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (String (Signal_Drag_End));
      Update_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (String (Signal_Drag_Update));
   begin
      Drag_Signal_Gesture := Gtk_Gesture_Drag_New;
      Drag_Typed_Calls := 0;
      Drag_Slot_Calls := 0;

      Drag_Signal_Gesture.On_Drag_Begin
        (On_Drag_Typed'Unrestricted_Access);
      Drag_Signal_Gesture.On_Drag_Begin
        (On_Drag_Slot'Unrestricted_Access,
         Slot => Drag_Signal_Gesture,
         After => True);
      Drag_Signal_Gesture.On_Drag_End
        (On_Drag_Typed'Unrestricted_Access);
      Drag_Signal_Gesture.On_Drag_End
        (On_Drag_Slot'Unrestricted_Access, Slot => Drag_Signal_Gesture);
      Drag_Signal_Gesture.On_Drag_Update
        (On_Drag_Typed'Unrestricted_Access);
      Drag_Signal_Gesture.On_Drag_Update
        (On_Drag_Slot'Unrestricted_Access, Slot => Drag_Signal_Gesture);

      --  The same payload shape is used by begin coordinates and end/update
      --  offsets, so each handler checks both doubles through its marshaller.
      Emit_Coordinates
        (Get_Object (Drag_Signal_Gesture), Begin_Name, 12.5, 34.25);
      Emit_Coordinates
        (Get_Object (Drag_Signal_Gesture), End_Name, 12.5, 34.25);
      Emit_Coordinates
        (Get_Object (Drag_Signal_Gesture), Update_Name, 12.5, 34.25);
      Assert_Cmpint_Eq (Gint (Drag_Typed_Calls), 3);
      Assert_Cmpint_Eq (Gint (Drag_Slot_Calls), 3);

      Interfaces.C.Strings.Free (Begin_Name);
      Interfaces.C.Strings.Free (End_Name);
      Interfaces.C.Strings.Free (Update_Name);
   end Test_Drag_Signals;

   --  A failing typed handler and slot handler exercise both generated drag
   --  marshaller exception blocks and are observed by the configured hook.
   procedure Test_Drag_Callback_Exceptions is
      Begin_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (String (Signal_Drag_Begin));
   begin
      Drag_Signal_Gesture := Gtk_Gesture_Drag_New;
      Callback_Exception_Calls := 0;
      Drag_Signal_Gesture.On_Drag_Begin
        (Raise_Drag_Typed'Unrestricted_Access);
      Drag_Signal_Gesture.On_Drag_Begin
        (Raise_Drag_Slot'Unrestricted_Access,
         Slot => Drag_Signal_Gesture);

      Emit_Coordinates
        (Get_Object (Drag_Signal_Gesture), Begin_Name, 12.5, 34.25);
      Assert_Cmpint_Eq (Gint (Callback_Exception_Calls), 2);
      Interfaces.C.Strings.Free (Begin_Name);
   end Test_Drag_Callback_Exceptions;

   ----------------
   -- Test_Click --
   ----------------

   procedure Test_Click is
      Window : constant Gtk_Window := Gtk_Window_New;
      Area   : constant Gtk_Drawing_Area := Gtk_Drawing_Area_New;
      Click  : constant Gtk_Gesture_Click := Gtk_Gesture_Click_New;
      By_Procedure : Gtk_Gesture_Click;
      Manual : constant Gtk_Gesture_Click := new Gtk_Gesture_Click_Record;
      Original_Object : System.Address;
   begin
      --  Exercise Gtk_New and Initialize as well as the function constructor.
      Gtk_New (By_Procedure);
      Gtk.Gesture_Click.Initialize (Manual);
      Original_Object := Get_Object (Manual);
      --  The second initialization is a no-op for an already-created wrapper.
      Gtk.Gesture_Click.Initialize (Manual);
      Assert_True (Get_Object (By_Procedure) /= System.Null_Address);
      Assert_True (Get_Object (Manual) = Original_Object);

      Window.Set_Child (Area);

      Gtk_Gesture_Single (Click).Set_Button (Button_Secondary);
      Assert_Cmpuint_Eq
        (Gtk_Gesture_Single (Click).Get_Button, Button_Secondary);

      Area.Add_Controller (Click);
      Assert_True
        (Gtk_Event_Controller (Click).Get_Widget = Gtk_Widget (Area));

      --  A gesture that has seen no press has no sequences to report.
      Assert_Cmpuint_Eq
        (Gdk_Event_Sequence_List.Length (Gtk_Gesture (Click).Get_Sequences),
         0);
   end Test_Click;

   --  With no input sequence, the current button is zero and the current
   --  sequence is null; the property accessors can be checked by round-trip.
   procedure Test_Single is
      Click : constant Gtk_Gesture_Click := Gtk_Gesture_Click_New;
      Single : constant Gtk_Gesture_Single := Gtk_Gesture_Single (Click);
   begin
      Assert_Cmpuint_Eq (Single.Get_Current_Button, 0);
      Assert_True (Single.Get_Current_Sequence = null);

      --  Exercise both boolean values to check the getter and setter bindings.
      Assert_False (Single.Get_Exclusive);
      Single.Set_Exclusive (True);
      Assert_True (Single.Get_Exclusive);
      Single.Set_Exclusive (False);
      Assert_False (Single.Get_Exclusive);

      Assert_False (Single.Get_Touch_Only);
      Single.Set_Touch_Only (True);
      Assert_True (Single.Get_Touch_Only);
      Single.Set_Touch_Only (False);
      Assert_False (Single.Get_Touch_Only);
   end Test_Single;

   --  Emitting each signal directly keeps these checks independent of a real
   --  pointer device while still exercising the generated C marshallers.
   procedure Test_Click_Signals is
      Pressed_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("pressed");
      Released_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("released");
      Stopped_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("stopped");
      Unpaired_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("unpaired-release");
   begin
      Signal_Click := Gtk_Gesture_Click_New;
      --  Start from zero so each callback count proves this emission's effect.
      Click_Pressed_Typed_Calls := 0;
      Click_Pressed_Slot_Calls := 0;
      Click_Released_Typed_Calls := 0;
      Click_Released_Slot_Calls := 0;
      Click_Stopped_Typed_Calls := 0;
      Click_Stopped_Slot_Calls := 0;
      Click_Unpaired_Typed_Calls := 0;
      Click_Unpaired_Slot_Calls := 0;

      --  Register both callback styles for every GtkGestureClick signal.
      --  The slot is the gesture itself, so callbacks can verify user data.
      Signal_Click.On_Pressed
        (On_Click_Pressed_Typed'Unrestricted_Access);
      Signal_Click.On_Pressed
        (On_Click_Pressed_Slot'Unrestricted_Access,
         Slot => Signal_Click,
         After => True);
      Signal_Click.On_Released
        (On_Click_Released_Typed'Unrestricted_Access);
      Signal_Click.On_Released
        (On_Click_Released_Slot'Unrestricted_Access,
         Slot => Signal_Click);
      Signal_Click.On_Stopped
        (On_Click_Stopped_Typed'Unrestricted_Access);
      Signal_Click.On_Stopped
        (On_Click_Stopped_Slot'Unrestricted_Access,
         Slot => Signal_Click,
         After => True);
      Signal_Click.On_Unpaired_Release
        (On_Click_Unpaired_Typed'Unrestricted_Access);
      Signal_Click.On_Unpaired_Release
        (On_Click_Unpaired_Slot'Unrestricted_Access,
         Slot => Signal_Click);

      --  Use a count of two and known coordinates to verify argument decoding.
      Emit_Click_Press
        (Get_Object (Signal_Click), Pressed_Name, 2, 12.5, 34.25);
      --  Released has the same signature, but a separate binding wrapper.
      Emit_Click_Press
        (Get_Object (Signal_Click), Released_Name, 2, 12.5, 34.25);
      --  Emitting stopped directly tests the callback without waiting on time.
      Emit_Void (Get_Object (Signal_Click), Stopped_Name);
      --  The NULL sequence models mouse input and checks nullable conversion.
      Emit_Click_Unpaired
        (Get_Object (Signal_Click), Unpaired_Name, 4.5, 8.25, 3,
         System.Null_Address);

      --  Every typed and slot overload must have run exactly once per signal.
      Assert_Cmpint_Eq (Gint (Click_Pressed_Typed_Calls), 1);
      Assert_Cmpint_Eq (Gint (Click_Pressed_Slot_Calls), 1);
      Assert_Cmpint_Eq (Gint (Click_Released_Typed_Calls), 1);
      Assert_Cmpint_Eq (Gint (Click_Released_Slot_Calls), 1);
      Assert_Cmpint_Eq (Gint (Click_Stopped_Typed_Calls), 1);
      Assert_Cmpint_Eq (Gint (Click_Stopped_Slot_Calls), 1);
      Assert_Cmpint_Eq (Gint (Click_Unpaired_Typed_Calls), 1);
      Assert_Cmpint_Eq (Gint (Click_Unpaired_Slot_Calls), 1);

      Interfaces.C.Strings.Free (Pressed_Name);
      Interfaces.C.Strings.Free (Released_Name);
      Interfaces.C.Strings.Free (Stopped_Name);
      Interfaces.C.Strings.Free (Unpaired_Name);
   end Test_Click_Signals;

   --  Exceptions from every typed and slot callback are caught by the
   --  generated marshaller and passed to the configured hook.
   procedure Test_Click_Callback_Exceptions is
      Pressed_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("pressed");
      Released_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("released");
      Stopped_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("stopped");
      Unpaired_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("unpaired-release");
   begin
      Signal_Click := Gtk_Gesture_Click_New;
      --  This test counts hook calls, not the assertions in normal callbacks.
      Callback_Exception_Calls := 0;

      --  Both press-related signal marshallers share the same callback shape.
      Signal_Click.On_Pressed
        (Raise_Click_Pressed_Typed'Unrestricted_Access);
      Signal_Click.On_Pressed
        (Raise_Click_Pressed_Slot'Unrestricted_Access,
         Slot => Signal_Click);
      Signal_Click.On_Released
        (Raise_Click_Pressed_Typed'Unrestricted_Access);
      Signal_Click.On_Released
        (Raise_Click_Pressed_Slot'Unrestricted_Access,
         Slot => Signal_Click);
      Signal_Click.On_Stopped
        (Raise_Click_Stopped_Typed'Unrestricted_Access);
      Signal_Click.On_Stopped
        (Raise_Click_Stopped_Slot'Unrestricted_Access,
         Slot => Signal_Click);
      Signal_Click.On_Unpaired_Release
        (Raise_Click_Unpaired_Typed'Unrestricted_Access);
      Signal_Click.On_Unpaired_Release
        (Raise_Click_Unpaired_Slot'Unrestricted_Access,
         Slot => Signal_Click);

      --  Each of the four signals invokes both failing callbacks once.
      Emit_Click_Press
        (Get_Object (Signal_Click), Pressed_Name, 2, 12.5, 34.25);
      Emit_Click_Press
        (Get_Object (Signal_Click), Released_Name, 2, 12.5, 34.25);
      Emit_Void (Get_Object (Signal_Click), Stopped_Name);
      Emit_Click_Unpaired
        (Get_Object (Signal_Click), Unpaired_Name, 4.5, 8.25, 3,
         System.Null_Address);
      Assert_Cmpint_Eq (Gint (Callback_Exception_Calls), 8);

      Interfaces.C.Strings.Free (Pressed_Name);
      Interfaces.C.Strings.Free (Released_Name);
      Interfaces.C.Strings.Free (Stopped_Name);
      Interfaces.C.Strings.Free (Unpaired_Name);
   end Test_Click_Callback_Exceptions;

   -----------------
   -- Test_Remove --
   -----------------

   procedure Test_Remove is
      Window : constant Gtk_Window := Gtk_Window_New;
      Area   : constant Gtk_Drawing_Area := Gtk_Drawing_Area_New;
      Drag   : constant Gtk_Gesture_Drag := Gtk_Gesture_Drag_New;
   begin
      Window.Set_Child (Area);

      Area.Add_Controller (Drag);
      Assert_True
        (Gtk_Event_Controller (Drag).Get_Widget = Gtk_Widget (Area));

      --  Add_Controller took ownership, so removing it is also the last
      --  reference: Drag must not be touched after this point.
      Area.Remove_Controller (Drag);
   end Test_Remove;

   --  Check both constructors, Initialize's idempotent guard, the accessor
   --  pair, and the public GObject property binding.
   procedure Test_Long_Press is
      --  The function form returns an already initialized gesture object.
      By_Function : constant Gtk_Gesture_Long_Press :=
        Gtk_Gesture_Long_Press_New;
      By_Procedure : Gtk_Gesture_Long_Press;
      --  Initialize also supports a caller-allocated Ada wrapper record.
      Manual : constant Gtk_Gesture_Long_Press :=
        new Gtk_Gesture_Long_Press_Record;
   begin
      Gtk_New (By_Procedure);
      Gtk.Gesture_Long_Press.Initialize (Manual);
      Gtk.Gesture_Long_Press.Initialize (Manual);

      Assert_True (By_Function /= null);
      Assert_True (By_Procedure /= null);
      Assert_True (Manual /= null);
      Assert_Cmpfloat_Eq (By_Function.Get_Delay_Factor, 1.0);

      --  Check the method updates the same delay-factor value it reads.
      By_Function.Set_Delay_Factor (1.5);
      Assert_Cmpfloat_Eq (By_Function.Get_Delay_Factor, 1.5);

      --  The property binding must expose the same value through GObject.
      Glib.Properties.Set_Property
        (By_Function, Delay_Factor_Property, 1.75);
      Assert_Cmpfloat_Eq
        (Glib.Properties.Get_Property
           (By_Function, Delay_Factor_Property), 1.75);
   end Test_Long_Press;

   --  Direct emission exercises both typed and slot callback overloads and
   --  checks the x/y coordinates sent by ::pressed.
   procedure Test_Long_Press_Signals is
      Cancelled_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("cancelled");
      Pressed_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("pressed");
   begin
      Long_Press := Gtk_Gesture_Long_Press_New;
      Cancelled_Typed_Calls := 0;
      Cancelled_Slot_Calls := 0;
      Pressed_Typed_Calls := 0;
      Pressed_Slot_Calls := 0;

      --  The slot forms receive this gesture as their explicit user data.
      Long_Press.On_Cancelled (On_Cancelled_Typed'Unrestricted_Access);
      Long_Press.On_Cancelled
        (On_Cancelled_Slot'Unrestricted_Access, Slot => Long_Press);
      Long_Press.On_Pressed (On_Pressed_Typed'Unrestricted_Access);
      --  After=True checks the alternative connection ordering option.
      Long_Press.On_Pressed
        (On_Pressed_Slot'Unrestricted_Access,
         Slot => Long_Press,
         After => True);

      --  The handlers verify these coordinates, not just that they ran.
      Emit_Void (Get_Object (Long_Press), Cancelled_Name);
      Emit_Coordinates
        (Get_Object (Long_Press), Pressed_Name, 12.5, 34.25);

      Assert_Cmpint_Eq (Gint (Cancelled_Typed_Calls), 1);
      Assert_Cmpint_Eq (Gint (Cancelled_Slot_Calls), 1);
      Assert_Cmpint_Eq (Gint (Pressed_Typed_Calls), 1);
      Assert_Cmpint_Eq (Gint (Pressed_Slot_Calls), 1);

      Interfaces.C.Strings.Free (Cancelled_Name);
      Interfaces.C.Strings.Free (Pressed_Name);
   end Test_Long_Press_Signals;

   --  Trigger one exception in each of the four marshallers and check that
   --  GtkAda's configured exception hook receives each one.
   procedure Test_Long_Press_Callback_Exceptions is
      Cancelled_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("cancelled");
      Pressed_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("pressed");
   begin
      Long_Press := Gtk_Gesture_Long_Press_New;
      Callback_Exception_Calls := 0;

      Long_Press.On_Cancelled (Raise_Cancelled_Typed'Unrestricted_Access);
      Long_Press.On_Cancelled
        (Raise_Cancelled_Slot'Unrestricted_Access, Slot => Long_Press);
      Long_Press.On_Pressed (Raise_Pressed_Typed'Unrestricted_Access);
      Long_Press.On_Pressed
        (Raise_Pressed_Slot'Unrestricted_Access, Slot => Long_Press);

      Emit_Void (Get_Object (Long_Press), Cancelled_Name);
      Emit_Coordinates
        (Get_Object (Long_Press), Pressed_Name, 12.5, 34.25);
      Assert_Cmpint_Eq (Gint (Callback_Exception_Calls), 4);

      Interfaces.C.Strings.Free (Cancelled_Name);
      Interfaces.C.Strings.Free (Pressed_Name);
   end Test_Long_Press_Callback_Exceptions;

   --  Swipe starts with no recognized input, so its documented velocity
   --  query returns False; both constructor styles and Initialize are checked.
   procedure Test_Swipe is
      By_Function : constant Gtk_Gesture_Swipe := Gtk_Gesture_Swipe_New;
      By_Procedure : Gtk_Gesture_Swipe;
      Manual : constant Gtk_Gesture_Swipe :=
        new Gtk_Gesture_Swipe_Record;
      Original_Object : System.Address;
      Velocity_X, Velocity_Y : Gdouble := 0.0;
   begin
      Gtk_New (By_Procedure);
      Gtk.Gesture_Swipe.Initialize (Manual);
      Original_Object := Get_Object (Manual);
      --  Reinitializing an existing wrapper must preserve its GObject.
      Gtk.Gesture_Swipe.Initialize (Manual);

      Assert_True (Get_Object (By_Function) /= System.Null_Address);
      Assert_True (Get_Object (By_Procedure) /= System.Null_Address);
      Assert_True (Get_Object (Manual) = Original_Object);
      Assert_True (Gtk.Gesture_Swipe.Signal_Swipe = "swipe");
      Assert_False
        (By_Function.Get_Velocity (Velocity_X, Velocity_Y));
   end Test_Swipe;

   --  Emit ::swipe directly to check both generated callback marshallers and
   --  verify they decode the velocity values passed by GObject.
   procedure Test_Swipe_Signals is
      Swipe_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (String (Signal_Swipe));
   begin
      Swipe := Gtk_Gesture_Swipe_New;
      Swipe_Typed_Calls := 0;
      Swipe_Slot_Calls := 0;

      Swipe.On_Swipe (On_Swipe_Typed'Unrestricted_Access);
      --  After=True also checks the alternate signal connection ordering.
      Swipe.On_Swipe
        (On_Swipe_Slot'Unrestricted_Access,
         Slot => Swipe,
         After => True);
      Emit_Coordinates (Get_Object (Swipe), Swipe_Name, 120.5, -45.25);

      Assert_Cmpint_Eq (Gint (Swipe_Typed_Calls), 1);
      Assert_Cmpint_Eq (Gint (Swipe_Slot_Calls), 1);
      Interfaces.C.Strings.Free (Swipe_Name);
   end Test_Swipe_Signals;

   --  Each generated swipe marshaller must route a callback exception to the
   --  registered GtkAda exception hook rather than across the C dispatcher.
   procedure Test_Swipe_Callback_Exceptions is
      Swipe_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (String (Signal_Swipe));
   begin
      Swipe := Gtk_Gesture_Swipe_New;
      Callback_Exception_Calls := 0;
      Swipe.On_Swipe (Raise_Swipe_Typed'Unrestricted_Access);
      Swipe.On_Swipe
        (Raise_Swipe_Slot'Unrestricted_Access, Slot => Swipe);

      Emit_Coordinates (Get_Object (Swipe), Swipe_Name, 120.5, -45.25);
      Assert_Cmpint_Eq (Gint (Callback_Exception_Calls), 2);
      Interfaces.C.Strings.Free (Swipe_Name);
   end Test_Swipe_Callback_Exceptions;

   --  Both constructor styles and Initialize are exercised, including its
   --  documented no-op when a manually allocated wrapper is already created.
   procedure Test_Pan is
      By_Function : constant Gtk_Gesture_Pan :=
        Gtk_Gesture_Pan_New (Orientation_Horizontal);
      By_Procedure : Gtk_Gesture_Pan;
      Manual : constant Gtk_Gesture_Pan :=
        new Gtk_Gesture_Pan_Record;
   begin
      Gtk_New (By_Procedure, Orientation_Vertical);
      Gtk.Gesture_Pan.Initialize (Manual, Orientation_Horizontal);
      Gtk.Gesture_Pan.Initialize (Manual, Orientation_Vertical);

      Assert_True (By_Function.Get_Orientation = Orientation_Horizontal);
      Assert_True (By_Procedure.Get_Orientation = Orientation_Vertical);
      --  Re-initialization must preserve the first object's orientation.
      Assert_True (Manual.Get_Orientation = Orientation_Horizontal);

      By_Function.Set_Orientation (Orientation_Vertical);
      Assert_True (By_Function.Get_Orientation = Orientation_Vertical);
      --  The explicit GObject property follows the same state as the methods.
      Set_Property
        (By_Function, Orientation_Property, Orientation_Horizontal);
      Assert_True
        (Get_Property (By_Function, Orientation_Property)
         = Orientation_Horizontal);
   end Test_Pan;

   --  A direct emission tests the enum/double decoding and both On_Pan
   --  overloads without relying on a physical pointer gesture.
   procedure Test_Pan_Signals is
      Pan_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (String (Signal_Pan));
   begin
      Pan_Signal_Gesture := Gtk_Gesture_Pan_New (Orientation_Horizontal);
      Pan_Typed_Calls := 0;
      Pan_Slot_Calls := 0;

      Pan_Signal_Gesture.On_Pan (On_Pan_Typed'Unrestricted_Access);
      Pan_Signal_Gesture.On_Pan
        (On_Pan_Slot'Unrestricted_Access,
         Slot => Pan_Signal_Gesture,
         After => True);
      Emit_Pan
        (Get_Object (Pan_Signal_Gesture), Pan_Name,
         Pan_Direction_Left, 17.25);

      Assert_Cmpint_Eq (Gint (Pan_Typed_Calls), 1);
      Assert_Cmpint_Eq (Gint (Pan_Slot_Calls), 1);
      Interfaces.C.Strings.Free (Pan_Name);
   end Test_Pan_Signals;

   --  The exception hook must receive errors from both generated pan
   --  marshallers, rather than letting them unwind through GObject.
   procedure Test_Pan_Callback_Exceptions is
      Pan_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (String (Signal_Pan));
   begin
      Pan_Signal_Gesture := Gtk_Gesture_Pan_New (Orientation_Horizontal);
      Callback_Exception_Calls := 0;
      Pan_Signal_Gesture.On_Pan (Raise_Pan_Typed'Unrestricted_Access);
      Pan_Signal_Gesture.On_Pan
        (Raise_Pan_Slot'Unrestricted_Access, Slot => Pan_Signal_Gesture);

      Emit_Pan
        (Get_Object (Pan_Signal_Gesture), Pan_Name,
         Pan_Direction_Left, 17.25);
      Assert_Cmpint_Eq (Gint (Callback_Exception_Calls), 2);
      Interfaces.C.Strings.Free (Pan_Name);
   end Test_Pan_Callback_Exceptions;

   --  Construction and the inactive query cover the rotate-specific API
   --  without requiring two physical touch points.
   procedure Test_Rotate is
      By_Function : constant Gtk_Gesture_Rotate := Gtk_Gesture_Rotate_New;
      By_Procedure : Gtk_Gesture_Rotate;
      Manual : constant Gtk_Gesture_Rotate :=
        new Gtk_Gesture_Rotate_Record;
      Original_Object : System.Address;
   begin
      Gtk_New (By_Procedure);
      Gtk.Gesture_Rotate.Initialize (Manual);
      Original_Object := Get_Object (Manual);
      --  Reinitializing an existing wrapper is documented as a no-op.
      Gtk.Gesture_Rotate.Initialize (Manual);

      Assert_True (Get_Object (By_Function) /= System.Null_Address);
      Assert_True (Get_Object (By_Procedure) /= System.Null_Address);
      Assert_True (Get_Object (Manual) = Original_Object);
      Assert_Cmpfloat_Eq (By_Function.Get_Angle_Delta, 0.0);
   end Test_Rotate;

   --  Direct signal emissions check the typed and slot adapters and the
   --  marshallers' exception handling without synthesizing touch input.
   procedure Test_Rotate_Signals is
      Rotate_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (String (Signal_Angle_Changed));
   begin
      Rotate_Signal_Gesture := Gtk_Gesture_Rotate_New;
      Rotate_Typed_Calls := 0;
      Rotate_Slot_Calls := 0;

      Rotate_Signal_Gesture.On_Angle_Changed
        (On_Rotate_Typed'Unrestricted_Access);
      Rotate_Signal_Gesture.On_Angle_Changed
        (On_Rotate_Slot'Unrestricted_Access,
         Slot => Rotate_Signal_Gesture,
         After => True);
      Emit_Coordinates
        (Get_Object (Rotate_Signal_Gesture), Rotate_Name, 1.25, -0.5);

      Assert_Cmpint_Eq (Gint (Rotate_Typed_Calls), 1);
      Assert_Cmpint_Eq (Gint (Rotate_Slot_Calls), 1);

      Callback_Exception_Calls := 0;
      Rotate_Signal_Gesture.On_Angle_Changed
        (Raise_Rotate_Typed'Unrestricted_Access);
      Rotate_Signal_Gesture.On_Angle_Changed
        (Raise_Rotate_Slot'Unrestricted_Access,
         Slot => Rotate_Signal_Gesture);
      Emit_Coordinates
        (Get_Object (Rotate_Signal_Gesture), Rotate_Name, 1.25, -0.5);
      Assert_Cmpint_Eq (Gint (Callback_Exception_Calls), 2);
      Interfaces.C.Strings.Free (Rotate_Name);
   end Test_Rotate_Signals;

   --  Check both constructors and Initialize's documented no-op on an
   --  existing object, then query the idle gesture's documented scale of 1.
   procedure Test_Zoom is
      By_Function : constant Gtk_Gesture_Zoom := Gtk_Gesture_Zoom_New;
      By_Procedure : Gtk_Gesture_Zoom;
      Manual : constant Gtk_Gesture_Zoom := new Gtk_Gesture_Zoom_Record;
      Original_Object : System.Address;
   begin
      Gtk_New (By_Procedure);
      Gtk.Gesture_Zoom.Initialize (Manual);
      Original_Object := Get_Object (Manual);
      Gtk.Gesture_Zoom.Initialize (Manual);

      Assert_True (Get_Object (By_Function) /= System.Null_Address);
      Assert_True (Get_Object (By_Procedure) /= System.Null_Address);
      Assert_True (Get_Object (Manual) = Original_Object);
      Assert_Cmpfloat_Eq (By_Function.Get_Scale_Delta, 1.0);
      Assert_Cmpfloat_Eq (By_Procedure.Get_Scale_Delta, 1.0);
      Assert_True (Signal_Scale_Changed = "scale-changed");
   end Test_Zoom;

   --  Emit ::scale-changed directly so generated callbacks are checked without
   --  synthesizing a two-finger gesture or depending on a touch device.
   procedure Test_Zoom_Signals is
      Zoom_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (String (Signal_Scale_Changed));
   begin
      Zoom_Signal_Gesture := Gtk_Gesture_Zoom_New;
      Zoom_Typed_Calls := 0;
      Zoom_Slot_Calls := 0;

      Zoom_Signal_Gesture.On_Scale_Changed
        (On_Zoom_Typed'Unrestricted_Access);
      Zoom_Signal_Gesture.On_Scale_Changed
        (On_Zoom_Slot'Unrestricted_Access,
         Slot => Zoom_Signal_Gesture,
         After => True);
      Emit_Scale (Get_Object (Zoom_Signal_Gesture), Zoom_Name, 1.75);

      Assert_Cmpint_Eq (Gint (Zoom_Typed_Calls), 1);
      Assert_Cmpint_Eq (Gint (Zoom_Slot_Calls), 1);
      Interfaces.C.Strings.Free (Zoom_Name);
   end Test_Zoom_Signals;

   --  Check that exceptions from typed and slot callbacks reach GtkAda's
   --  configured hook rather than unwinding through GObject's signal code.
   procedure Test_Zoom_Callback_Exceptions is
      Zoom_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (String (Signal_Scale_Changed));
   begin
      Zoom_Signal_Gesture := Gtk_Gesture_Zoom_New;
      Callback_Exception_Calls := 0;
      Zoom_Signal_Gesture.On_Scale_Changed
        (Raise_Zoom_Typed'Unrestricted_Access);
      Zoom_Signal_Gesture.On_Scale_Changed
        (Raise_Zoom_Slot'Unrestricted_Access,
         Slot => Zoom_Signal_Gesture);

      Emit_Scale (Get_Object (Zoom_Signal_Gesture), Zoom_Name, 1.75);
      Assert_Cmpint_Eq (Gint (Callback_Exception_Calls), 2);
      Interfaces.C.Strings.Free (Zoom_Name);
   end Test_Zoom_Callback_Exceptions;

   --  Validate that each marshaller reconstructs its object and both radians.
   procedure On_Rotate_Typed
     (Self : access Gtk_Gesture_Rotate_Record'Class;
      Angle, Angle_Delta : Gdouble) is
   begin
      Assert_True
        (Get_Object (Self) = Get_Object (Rotate_Signal_Gesture));
      Assert_Cmpfloat_Eq (Angle, 1.25);
      Assert_Cmpfloat_Eq (Angle_Delta, -0.5);
      Rotate_Typed_Calls := Rotate_Typed_Calls + 1;
   end On_Rotate_Typed;

   --  A slot callback receives the supplied GObject as its first argument.
   procedure On_Rotate_Slot
     (Self : access GObject_Record'Class;
      Angle, Angle_Delta : Gdouble) is
   begin
      Assert_True
        (Get_Object (Self) = Get_Object (Rotate_Signal_Gesture));
      Assert_Cmpfloat_Eq (Angle, 1.25);
      Assert_Cmpfloat_Eq (Angle_Delta, -0.5);
      Rotate_Slot_Calls := Rotate_Slot_Calls + 1;
   end On_Rotate_Slot;

   --  Raising callbacks exercise the exception branches in both marshallers.
   procedure Raise_Rotate_Typed
     (Self : access Gtk_Gesture_Rotate_Record'Class;
      Angle, Angle_Delta : Gdouble) is
      pragma Unreferenced (Self, Angle, Angle_Delta);
   begin
      raise Program_Error with "expected rotate gesture callback exception";
   end Raise_Rotate_Typed;

   procedure Raise_Rotate_Slot
     (Self : access GObject_Record'Class;
      Angle, Angle_Delta : Gdouble) is
      pragma Unreferenced (Self, Angle, Angle_Delta);
   begin
      raise Program_Error with "expected rotate gesture callback exception";
   end Raise_Rotate_Slot;

   --  The typed callback receives the concrete Zoom gesture and emitted scale.
   procedure On_Zoom_Typed
     (Self : access Gtk_Gesture_Zoom_Record'Class;
      Scale : Gdouble) is
   begin
      Assert_True
        (Get_Object (Self) = Get_Object (Zoom_Signal_Gesture));
      Assert_Cmpfloat_Eq (Scale, 1.75);
      Zoom_Typed_Calls := Zoom_Typed_Calls + 1;
   end On_Zoom_Typed;

   --  The slot callback receives the explicit GObject passed at connection.
   procedure On_Zoom_Slot
     (Self : access GObject_Record'Class;
      Scale : Gdouble) is
   begin
      Assert_True
        (Get_Object (Self) = Get_Object (Zoom_Signal_Gesture));
      Assert_Cmpfloat_Eq (Scale, 1.75);
      Zoom_Slot_Calls := Zoom_Slot_Calls + 1;
   end On_Zoom_Slot;

   --  Distinct signatures make both generated Zoom exception branches run.
   procedure Raise_Zoom_Typed
     (Self : access Gtk_Gesture_Zoom_Record'Class;
      Scale : Gdouble) is
      pragma Unreferenced (Self, Scale);
   begin
      raise Program_Error with "expected zoom callback exception";
   end Raise_Zoom_Typed;

   procedure Raise_Zoom_Slot
     (Self : access GObject_Record'Class;
      Scale : Gdouble) is
      pragma Unreferenced (Self, Scale);
   begin
      raise Program_Error with "expected zoom callback exception";
   end Raise_Zoom_Slot;

   --  Empty gestures still exercise the documented no-current-sequence
   --  results of the base GtkGesture query wrappers.
   procedure Test_Base_Queries is
      Window : constant Gtk_Window := Gtk_Window_New;
      Area : constant Gtk_Drawing_Area := Gtk_Drawing_Area_New;
      Drag : constant Gtk_Gesture_Drag := Gtk_Gesture_Drag_New;
      G : constant Gtk_Gesture := Gtk_Gesture (Drag);
      Token : aliased Gint := 17;
      Sequence : constant Gdk_Event_Sequence :=
        Gdk_Event_Sequence (Address_To_Sequence_Proxy (Token'Address));
      Null_Gesture : constant Gtk_Gesture :=
        Gtk.Gesture.Convert (System.Null_Address);
      Null_Sequence : constant Gdk_Event_Sequence :=
        Gtk.Gesture.Convert (System.Null_Address);
      Rect : Gdk_Rectangle;
      X, Y : aliased Gdouble := 0.0;
      Group_List : Gesture_List.Glist := G.Get_Group;
      Sequence_List : Gdk_Event_Sequence_List.Glist := G.Get_Sequences;
   begin
      --  Event-controller queries are made after attaching to a real widget,
      --  as GTK initializes gesture state when the controller is installed.
      Window.Set_Child (Area);
      Area.Add_Controller (Drag);

      --  Check both object and opaque-sequence conversion helpers, including
      --  their null cases, without dereferencing the token pointer.
      Assert_True (Gtk.Gesture.Convert (G) = Get_Object (G));
      Assert_True
        (Get_Object (Gtk.Gesture.Convert (Get_Object (G))) = Get_Object (G));
      Assert_True (Null_Gesture = null);
      Assert_True (Gtk.Gesture.Convert (Sequence) = Token'Address);
      Assert_True (Gtk.Gesture.Convert (Token'Address) = Sequence);
      Assert_True (Null_Sequence = null);

      --  With no event in progress, GTK reports empty geometry and device
      --  queries, and the Ada wrappers still copy the out parameters back.
      Assert_False (G.Get_Bounding_Box (Rect));
      Assert_True (G.Get_Device = null);

      --  Last-event, point and state queries use the documented pointer form;
      --  no pointer event has supplied coordinates or an active sequence.
      --  A sequence the gesture has never handled has no last event.
      Assert_True (Gdk.Event.Is_Null (G.Get_Last_Event (Sequence)));

      --  The center getter is omitted: on an idle gesture the GTK runtime
      --  emits a fatal GDK event-type critical. Its normal contract applies
      --  to active input, which cannot be forged by direct signal emission.
      Assert_True (G.Get_Last_Updated_Sequence = null);
      Assert_False (G.Get_Point (null, X'Access, Y'Access));
      Assert_True (G.Get_Sequence_State (null) = Event_Sequence_None);
      Assert_False (G.Handles_Sequence (null));

      --  State changes cannot target an absent sequence; the group and
      --  sequence lists expose the idle gesture and its empty event set.
      Assert_False
        (G.Set_Sequence_State (null, Event_Sequence_Denied));
      Assert_False (G.Set_State (Event_Sequence_Denied));
      Assert_Cmpuint_Eq (Gesture_List.Length (Group_List), 1);
      Assert_Cmpuint_Eq (Gdk_Event_Sequence_List.Length (Sequence_List), 0);
      Gesture_List.Free (Group_List);
      Gdk_Event_Sequence_List.Free (Sequence_List);
   end Test_Base_Queries;

   --  Grouping requires both controllers to belong to the same widget; the
   --  returned lists are freed because GTK documents them as caller-owned.
   procedure Test_Base_Grouping is
      Area : constant Gtk_Drawing_Area := Gtk_Drawing_Area_New;
      First : constant Gtk_Gesture_Drag := Gtk_Gesture_Drag_New;
      Second : constant Gtk_Gesture_Drag := Gtk_Gesture_Drag_New;
      G1 : constant Gtk_Gesture := Gtk_Gesture (First);
      G2 : constant Gtk_Gesture := Gtk_Gesture (Second);
      Group_List : Gesture_List.Glist;
      Cursor : Gesture_List.Glist;
      Item : Gtk_Gesture;
      Found_First, Found_Second : Boolean := False;
   begin
      Area.Add_Controller (First);
      Area.Add_Controller (Second);
      Assert_False (G1.Is_Grouped_With (G2));

      --  A group contains both attached gestures and is visible from either.
      G1.Group (G2);
      Assert_True (G1.Is_Grouped_With (G2));
      Assert_True (G2.Is_Grouped_With (G1));
      Group_List := G1.Get_Group;
      Assert_Cmpuint_Eq (Gesture_List.Length (Group_List), 2);

      --  Iteration checks the object conversions performed for list elements.
      Cursor := Group_List;
      while Cursor /= Gesture_List.Null_List loop
         Item := Gesture_List.Get_Data (Cursor);
         Found_First := Found_First or else
           Get_Object (Item) = Get_Object (G1);
         Found_Second := Found_Second or else
           Get_Object (Item) = Get_Object (G2);
         Cursor := Gesture_List.Next (Cursor);
      end loop;
      Assert_True (Found_First and Found_Second);
      Gesture_List.Free (Group_List);

      --  Ungroup restores each gesture's singleton group.
      G1.Ungroup;
      Assert_False (G1.Is_Grouped_With (G2));
      Group_List := G1.Get_Group;
      Assert_Cmpuint_Eq (Gesture_List.Length (Group_List), 1);
      Gesture_List.Free (Group_List);
   end Test_Base_Grouping;

   --  Direct signal emissions validate the base GtkGesture typed and slot
   --  connectors, including nullable sequence and enum conversion.
   procedure Test_Base_Signals is
      Drag : constant Gtk_Gesture_Drag := Gtk_Gesture_Drag_New;
      Token : aliased Gint := 23;
      Begin_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("begin");
      Cancel_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("cancel");
      End_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("end");
      Update_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("update");
      State_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("sequence-state-changed");
   begin
      Base_Gesture := Gtk_Gesture (Drag);
      Base_Sequence :=
        Gdk_Event_Sequence (Address_To_Sequence_Proxy (Token'Address));
      Base_Typed_Calls := 0;
      Base_Slot_Calls := 0;

      --  One callback pair can serve all sequence-only signals because their
      --  generated GtkAda signatures are identical.
      Base_Gesture.On_Begin (On_Base_Sequence'Unrestricted_Access);
      Base_Gesture.On_Begin
        (On_Base_Sequence_Slot'Unrestricted_Access, Slot => Base_Gesture);
      Base_Gesture.On_Cancel (On_Base_Sequence'Unrestricted_Access);
      Base_Gesture.On_Cancel
        (On_Base_Sequence_Slot'Unrestricted_Access,
         Slot => Base_Gesture, After => True);
      Base_Gesture.On_End (On_Base_Sequence'Unrestricted_Access);
      Base_Gesture.On_End
        (On_Base_Sequence_Slot'Unrestricted_Access, Slot => Base_Gesture);
      Base_Gesture.On_Update (On_Base_Sequence'Unrestricted_Access);
      Base_Gesture.On_Update
        (On_Base_Sequence_Slot'Unrestricted_Access,
         Slot => Base_Gesture, After => True);
      Base_Gesture.On_Sequence_State_Changed
        (On_Base_State'Unrestricted_Access);
      Base_Gesture.On_Sequence_State_Changed
        (On_Base_State_Slot'Unrestricted_Access, Slot => Base_Gesture);

      --  Emitting signals directly avoids depending on real pointer input.
      Emit_Sequence
        (Get_Object (Base_Gesture), Begin_Name,
         Gtk.Gesture.Convert (Base_Sequence));
      Emit_Sequence
        (Get_Object (Base_Gesture), Cancel_Name,
         Gtk.Gesture.Convert (Base_Sequence));
      Emit_Sequence
        (Get_Object (Base_Gesture), End_Name,
         Gtk.Gesture.Convert (Base_Sequence));
      Emit_Sequence
        (Get_Object (Base_Gesture), Update_Name,
         Gtk.Gesture.Convert (Base_Sequence));
      Emit_Sequence_State
        (Get_Object (Base_Gesture), State_Name,
         Gtk.Gesture.Convert (Base_Sequence), Event_Sequence_Claimed);
      Assert_Cmpint_Eq (Gint (Base_Typed_Calls), 5);
      Assert_Cmpint_Eq (Gint (Base_Slot_Calls), 5);

      Interfaces.C.Strings.Free (Begin_Name);
      Interfaces.C.Strings.Free (Cancel_Name);
      Interfaces.C.Strings.Free (End_Name);
      Interfaces.C.Strings.Free (Update_Name);
      Interfaces.C.Strings.Free (State_Name);
   end Test_Base_Signals;

   --  Exceptions are raised once through each of the four base-signal
   --  marshallers so GtkAda's exception hook handles every callback form.
   procedure Test_Base_Callback_Exceptions is
      Drag : constant Gtk_Gesture_Drag := Gtk_Gesture_Drag_New;
      Begin_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("begin");
      State_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("sequence-state-changed");
   begin
      Base_Gesture := Gtk_Gesture (Drag);
      Callback_Exception_Calls := 0;
      Base_Gesture.On_Begin (Raise_Base_Sequence'Unrestricted_Access);
      Base_Gesture.On_Begin
        (Raise_Base_Sequence_Slot'Unrestricted_Access,
         Slot => Base_Gesture);
      Base_Gesture.On_Sequence_State_Changed
        (Raise_Base_State'Unrestricted_Access);
      Base_Gesture.On_Sequence_State_Changed
        (Raise_Base_State_Slot'Unrestricted_Access,
         Slot => Base_Gesture);

      --  These emissions reach both typed and slot exception handlers.
      Emit_Sequence
        (Get_Object (Base_Gesture), Begin_Name, System.Null_Address);
      Emit_Sequence_State
        (Get_Object (Base_Gesture), State_Name, System.Null_Address,
         Event_Sequence_Claimed);
      Assert_Cmpint_Eq (Gint (Callback_Exception_Calls), 4);

      Interfaces.C.Strings.Free (Begin_Name);
      Interfaces.C.Strings.Free (State_Name);
   end Test_Base_Callback_Exceptions;

begin
   Glib.Test.Init;

   --  Widgets cannot be created before GTK is initialized.
   Gtk.Main.Init;
   Gtkada.Bindings.Set_On_Exception
     (Record_Callback_Exception'Unrestricted_Access);

   Glib.Test.Add_Func ("/gesture/drag", Test_Drag'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/drag-signals", Test_Drag_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/drag-callback-exceptions",
      Test_Drag_Callback_Exceptions'Unrestricted_Access);
   Glib.Test.Add_Func ("/gesture/click", Test_Click'Unrestricted_Access);
   Glib.Test.Add_Func ("/gesture/single", Test_Single'Unrestricted_Access);
   Glib.Test.Add_Func ("/gesture/remove", Test_Remove'Unrestricted_Access);

   Glib.Test.Add_Func
     ("/gesture/sequence-conversion",
      Test_Sequence_Conversion'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/mouse-sequence-signals",
      Test_Mouse_Sequence_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/long-press",
      Test_Long_Press'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/long-press-signals",
      Test_Long_Press_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/long-press-callback-exceptions",
      Test_Long_Press_Callback_Exceptions'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/swipe", Test_Swipe'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/swipe-signals", Test_Swipe_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/swipe-callback-exceptions",
      Test_Swipe_Callback_Exceptions'Unrestricted_Access);
   Glib.Test.Add_Func ("/gesture/pan", Test_Pan'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/pan-signals", Test_Pan_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/pan-callback-exceptions",
      Test_Pan_Callback_Exceptions'Unrestricted_Access);
   Glib.Test.Add_Func ("/gesture/rotate", Test_Rotate'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/rotate-signals",
      Test_Rotate_Signals'Unrestricted_Access);
   Glib.Test.Add_Func ("/gesture/zoom", Test_Zoom'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/zoom-signals", Test_Zoom_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/zoom-callback-exceptions",
      Test_Zoom_Callback_Exceptions'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/click-signals",
      Test_Click_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/click-callback-exceptions",
      Test_Click_Callback_Exceptions'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/base-queries", Test_Base_Queries'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/base-grouping", Test_Base_Grouping'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/base-signals", Test_Base_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture/base-callback-exceptions",
      Test_Base_Callback_Exceptions'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Gesture;
