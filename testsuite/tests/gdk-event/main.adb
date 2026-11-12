--  Exercises Gdk.Event ownership, typed casts, type predicates, and accessors,
--  including pad-event values read from synthetic events.
--  Synthetic event records follow the helper construction used by
--  testsuite/c_tests/gestures.c; GTK 4 intentionally has no public event
--  constructor, so the test mirrors the private event prefix from GDK.

with Ada.Command_Line;
with Interfaces.C;
with System;
with System.Address_To_Access_Conversions;

with Gdk.Device;
with Gdk.Device_Tool;
with Gdk.Display;
with Gdk.Enums;
with Gdk.Event;
with Gdk.Event.Button_Event;
with Gdk.Event.Crossing_Event;
with Gdk.Event.Delete_Event;
with Gdk.Event.DND_Event;
with Gdk.Event.Focus_Event;
with Gdk.Event.Grab_Broken_Event;
with Gdk.Event.Key_Event;
with Gdk.Event.Motion_Event;
with Gdk.Event.Pad_Event;
with Gdk.Event.Proximity_Event;
with Gdk.Event.Scroll_Event;
with Gdk.Event.Touch_Event;
with Gdk.Event.Touchpad_Event;
with Gdk.Drop;
with Gdk.Key_Match;
with Gdk;
with Gdk.Seat;
with Gdk.Surface;
with Glib;
with Glib.Object;
with Glib.Test;
with Gtk.Main;
with Gtk.Shortcut_Trigger;
with Gtk.Window;

procedure Main is
   use type Interfaces.C.unsigned;
   use type Interfaces.C.unsigned_short;
   use type System.Address;
   use type Gdk.Event.Gdk_Event_Sequence;
   use type Gdk.Drop.Gdk_Drop;
   use type Gdk.Event.Gdk_Event_Type;
   use type Gdk.Event.Scroll_Event.Gdk_Scroll_Direction;
   use type Gdk.Event.Scroll_Event.Gdk_Scroll_Relative_Direction;
   use type Gdk.Event.Scroll_Event.Gdk_Scroll_Unit;
   use type Gdk.Event.Touchpad_Event.Gdk_Touchpad_Gesture_Phase;
   use type Gdk.Event.Crossing_Event.Gdk_Crossing_Mode;
   use type Gdk.Event.Crossing_Event.Gdk_Notify_Type;
   use type Gdk.Key_Match.Gdk_Key_Match;
   use type Glib.Gdouble;
   use type Gdk.Enums.Gdk_Modifier_Type;
   use type Glib.GType;
   use type Gdk.Device_Tool.Gdk_Device_Tool;
   use type Gdk.Gdk_Device;
   use type Gdk.Gdk_Display;
   use type Gdk.Gdk_Seat;
   use type Gdk.Gdk_Surface;

   --  These fields match the common prefix of GdkEvent in gdkeventsprivate.h.
   --  C convention supplies the pointer alignment and padding used by GDK.
   type Event_Base_Record is record
      Class     : System.Address;
      Ref_Count : Interfaces.C.int;
      Event_Type : Gdk.Event.Gdk_Event_Type;
      Surface   : System.Address;
      Device    : System.Address;
      Time      : Interfaces.C.unsigned;
      Flags     : Interfaces.C.unsigned_short;
   end record
     with Convention => C;

   --  The motion layout is the common GdkEvent prefix plus the fields from
   --  GdkMotionEvent. Only these fields are initialized by this test.
   type Motion_Event_Record is record
      Parent : Event_Base_Record;
      State  : Interfaces.C.unsigned;
      X      : Glib.Gdouble;
      Y      : Glib.Gdouble;
      Axes   : System.Address;
      Tool   : System.Address;
      History : System.Address;
   end record
     with Convention => C;

   --  Scroll fields follow the common GdkEvent prefix in gdkeventsprivate.h.
   --  The public API has getters but no constructor or setters for these data.
   type Scroll_Event_Record is record
      Parent             : Event_Base_Record;
      --  The state and direction precede the delta payload in GDK's C layout.
      State              : Interfaces.C.unsigned;
      Direction          : Gdk.Event.Scroll_Event.Gdk_Scroll_Direction;
      Delta_X            : Glib.Gdouble;
      Delta_Y            : Glib.Gdouble;
      Is_Stop            : Interfaces.C.int;
      Tool               : System.Address;
      History            : System.Address;
      Unit               : Gdk.Event.Scroll_Event.Gdk_Scroll_Unit;
      Relative_Direction :
        Gdk.Event.Scroll_Event.Gdk_Scroll_Relative_Direction;
   end record
     with Convention => C;

   --  Button layout is used only to check the platform context-menu helper.
   type Button_Event_Record is record
      Parent : Event_Base_Record;
      State  : Interfaces.C.unsigned;
      Button : Interfaces.C.unsigned;
      X      : Glib.Gdouble;
      Y      : Glib.Gdouble;
      Axes   : System.Address;
      Tool   : System.Address;
   end record
     with Convention => C;

   --  GDK keeps events read-only and has no public constructor, so the test
   --  mirrors this private record layout to install deterministic values.
   --  Crossing fields follow GdkEvent in gdkeventsprivate.h. The getters
   --  tested below read mode, detail, and focus from these offsets.
   type Crossing_Event_Record is record
      Parent        : Event_Base_Record;
      State         : Interfaces.C.unsigned;
      Mode          : Gdk.Event.Crossing_Event.Gdk_Crossing_Mode;
      X             : Glib.Gdouble;
      Y             : Glib.Gdouble;
      Detail        : Gdk.Event.Crossing_Event.Gdk_Notify_Type;
      Focus         : Interfaces.C.int;
      Child_Surface : System.Address;
   end record
     with Convention => C;

   --  The grab-broken layout follows GdkEvent in gdkeventsprivate.h. These
   --  fields let the accessors return a known grab surface and implicit flag.
   type Grab_Broken_Event_Record is record
      Parent       : Event_Base_Record;
      Keyboard     : Interfaces.C.int;
      Implicit     : Interfaces.C.int;
      Grab_Surface : System.Address;
   end record
     with Convention => C;

   --  GdkTranslatedKey stores the key selected by the layout and the
   --  modifiers consumed while translating it.
   type Translated_Key_Record is record
      Keyval   : Interfaces.C.unsigned;
      Consumed : Gdk.Enums.Gdk_Modifier_Type;
      Layout   : Interfaces.C.unsigned;
      Level    : Interfaces.C.unsigned;
   end record
     with Convention => C;

   type Translated_Key_Array is array (0 .. 1) of Translated_Key_Record
     with Convention => C;

   --  This prefix follows GdkKeyEvent in gdkeventsprivate.h. The optional
   --  compose-sequence pointer comes after the translated keys and is unused.
   type Key_Event_Record is record
      Parent          : Event_Base_Record;
      State           : Gdk.Enums.Gdk_Modifier_Type;
      Keycode         : Interfaces.C.unsigned;
      Key_Is_Modifier : Interfaces.C.int;
      Translated      : Translated_Key_Array;
   end record
     with Convention => C;

   --  Touchpad fields follow the common event prefix in GDK's private event
   --  layout; the test sets these so every touchpad getter has known data.
   type Touchpad_Event_Record is record
      Parent      : Event_Base_Record;
      --  GDK stores the sequence and modifier state before gesture data.
      Sequence    : System.Address;
      State       : Interfaces.C.unsigned;
      --  Phase and finger count are byte-sized fields in GdkTouchpadEvent.
      Phase       : Interfaces.C.signed_char;
      N_Fingers   : Interfaces.C.signed_char;
      --  Pointer coordinates precede deltas in the private C layout.
      X           : Glib.Gdouble;
      Y           : Glib.Gdouble;
      Delta_X     : Glib.Gdouble;
      Delta_Y     : Glib.Gdouble;
      Angle_Delta : Glib.Gdouble;
      Scale       : Glib.Gdouble;
   end record
     with Convention => C;

   --  Touch fields follow the common event prefix in GDK's private layout.
   --  Only the emulation flag is needed to check the touch-event accessor.
   type Touch_Event_Record is record
      Parent          : Event_Base_Record;
      Sequence        : System.Address;
      State           : Interfaces.C.unsigned;
      X               : Glib.Gdouble;
      Y               : Glib.Gdouble;
      Axes             : System.Address;
      Touch_Emulating  : Interfaces.C.int;
   end record
     with Convention => C;

   --  GDK stores pad-event fields after the common event prefix. This private
   --  layout lets the test supply deterministic button, axis, group, and mode
   --  values because GdkEvent has no public constructor or field setters.
   type Pad_Event_Record is record
      Parent : Event_Base_Record;
      Group  : Interfaces.C.unsigned;
      Mode   : Interfaces.C.unsigned;
      Button : Interfaces.C.unsigned;
      Index  : Interfaces.C.unsigned;
      Value  : Glib.Gdouble;
   end record
     with Convention => C;

   package Base_Pointers is new
     System.Address_To_Access_Conversions (Event_Base_Record);
   package Motion_Pointers is new
     System.Address_To_Access_Conversions (Motion_Event_Record);
   package Scroll_Event_Pointers is new
     System.Address_To_Access_Conversions (Scroll_Event_Record);
   package Button_Pointers is new
     System.Address_To_Access_Conversions (Button_Event_Record);
   package Crossing_Event_Pointers is new
     System.Address_To_Access_Conversions (Crossing_Event_Record);
   package Grab_Broken_Event_Pointers is new
     System.Address_To_Access_Conversions (Grab_Broken_Event_Record);
   package Key_Pointers is new
     System.Address_To_Access_Conversions (Key_Event_Record);
   package Touchpad_Event_Pointers is new
     System.Address_To_Access_Conversions (Touchpad_Event_Record);
   package Touch_Event_Pointers is new
     System.Address_To_Access_Conversions (Touch_Event_Record);
   package Pad_Event_Pointers is new
     System.Address_To_Access_Conversions (Pad_Event_Record);

   function Create_Instance (Type_Id : Glib.GType) return System.Address
     with Import, Convention => C,
       External_Name => "g_type_create_instance";
   procedure Event_Unref (Event : System.Address)
     with Import, Convention => C,
       External_Name => "gdk_event_unref";
   function Object_Ref (Object : System.Address) return System.Address
     with Import, Convention => C,
       External_Name => "g_object_ref";

   Display : Gdk.Display.Gdk_Display;
   Seat    : Gdk.Gdk_Seat;
   Device  : Gdk.Gdk_Device;
   Window  : Gtk.Window.Gtk_Window;
   Surface : Gdk.Surface.Gdk_Surface;

   --  Each synthetic event owns references to its surface and device just as
   --  a backend-created event does. Finalization therefore follows normal GDK
   --  ownership rules instead of leaving borrowed pointers in the event.
   function New_Event
     (Type_Id    : Glib.GType;
      Event_Type : Gdk.Event.Gdk_Event_Type)
      return Gdk.Event.Gdk_Event
   is
      Raw : constant System.Address := Create_Instance (Type_Id);
      Base : constant Base_Pointers.Object_Pointer :=
        Base_Pointers.To_Pointer (Raw);
      Result : Gdk.Event.Gdk_Event;
   begin
      Base.all.Event_Type := Event_Type;
      Base.all.Surface := Object_Ref (Glib.Object.Get_Object (Surface));
      Base.all.Device := Object_Ref (Glib.Object.Get_Object (Device));
      Base.all.Time := 12345;
      Result := Gdk.Event.From_Object (Raw);
      Event_Unref (Raw);
      return Result;
   end New_Event;

   --  A motion event needs known coordinates for position and gesture math.
   procedure Set_Motion
     (Event : Gdk.Event.Gdk_Event;
      X, Y  : Glib.Gdouble) is
      Data : constant Motion_Pointers.Object_Pointer :=
        Motion_Pointers.To_Pointer (Gdk.Event.Get_Object (Event));
   begin
      Data.all.State := 0;
      Data.all.X := X;
      Data.all.Y := Y;
   end Set_Motion;

   --  Initialize the fields read by scroll-event accessors with deterministic
   --  values, including a smooth event that carries nonzero deltas.
   procedure Set_Scroll_Event
     (Event              : Gdk.Event.Gdk_Event;
      Direction          : Gdk.Event.Scroll_Event.Gdk_Scroll_Direction;
      Delta_X, Delta_Y   : Glib.Gdouble;
      Is_Stop            : Boolean;
      Unit               : Gdk.Event.Scroll_Event.Gdk_Scroll_Unit;
      Relative_Direction :
        Gdk.Event.Scroll_Event.Gdk_Scroll_Relative_Direction) is
      Data : constant Scroll_Event_Pointers.Object_Pointer :=
        Scroll_Event_Pointers.To_Pointer (Gdk.Event.Get_Object (Event));
   begin
      Data.all.State := 0;
      Data.all.Direction := Direction;
      Data.all.Delta_X := Delta_X;
      Data.all.Delta_Y := Delta_Y;
      Data.all.Is_Stop := (if Is_Stop then 1 else 0);
      --  The test events do not own tablet tools or compressed history.
      Data.all.Tool := System.Null_Address;
      Data.all.History := System.Null_Address;
      Data.all.Unit := Unit;
      Data.all.Relative_Direction := Relative_Direction;
   end Set_Scroll_Event;

   --  The secondary button follows the usual Linux context-menu convention.
   procedure Set_Button
     (Event  : Gdk.Event.Gdk_Event;
      Number : Interfaces.C.unsigned) is
      Data : constant Button_Pointers.Object_Pointer :=
        Button_Pointers.To_Pointer (Gdk.Event.Get_Object (Event));
   begin
      Data.all.State := 0;
      Data.all.Button := Number;
      Data.all.X := 4.0;
      Data.all.Y := 5.0;
   end Set_Button;

   --  The grab surface is separately referenced because GDK owns it for the
   --  event lifetime; the accessor returns that same surface as a borrowed
   --  GtkAda wrapper.
   procedure Set_Grab_Broken_Event
     (Event    : Gdk.Event.Gdk_Event;
      Implicit : Boolean) is
      Data : constant Grab_Broken_Event_Pointers.Object_Pointer :=
        Grab_Broken_Event_Pointers.To_Pointer
          (Gdk.Event.Get_Object (Event));
   begin
      Data.all.Keyboard := 0;
      Data.all.Implicit := (if Implicit then 1 else 0);
      Data.all.Grab_Surface :=
        Object_Ref (Glib.Object.Get_Object (Surface));
   end Set_Grab_Broken_Event;

   --  Fill both translations so the regular event accessors and shortcut
   --  matching see the same deterministic key without consulting input.
   procedure Set_Key_Event
     (Event        : Gdk.Event.Gdk_Event;
      Is_Modifier  : Boolean) is
      Data : constant Key_Pointers.Object_Pointer :=
        Key_Pointers.To_Pointer (Gdk.Event.Get_Object (Event));
      Key_A : constant Interfaces.C.unsigned :=
        Interfaces.C.unsigned (Character'Pos ('a'));
   begin
      Data.all.State := Gdk.Enums.Gdk_Control_Mask;
      Data.all.Keycode := 38;
      Data.all.Key_Is_Modifier := (if Is_Modifier then 1 else 0);
      Data.all.Translated (0) :=
        (Keyval   => Key_A,
         Consumed => Gdk.Enums.Gdk_Shift_Mask,
         Layout   => 2,
         Level    => 1);
      Data.all.Translated (1) :=
        (Keyval   => Key_A,
         Consumed => Gdk.Enums.Gdk_No_Modifier_Mask,
         Layout   => 2,
         Level    => 1);
   end Set_Key_Event;

   --  Install gesture data with values that distinguish deltas, phase,
   --  finger count, pinch angle, and pinch scale.
   procedure Set_Touchpad_Event (Event : Gdk.Event.Gdk_Event) is
      Data : constant Touchpad_Event_Pointers.Object_Pointer :=
        Touchpad_Event_Pointers.To_Pointer (Gdk.Event.Get_Object (Event));
   begin
      Data.all.Sequence := System.Null_Address;
      Data.all.State := 0;
      Data.all.Phase := 1;
      Data.all.N_Fingers := 3;
      Data.all.X := 4.0;
      Data.all.Y := 5.0;
      Data.all.Delta_X := 1.25;
      Data.all.Delta_Y := -2.5;
      Data.all.Angle_Delta := 0.75;
      Data.all.Scale := 1.5;
   end Set_Touchpad_Event;

   --  Install pad fields for all three accessor families. Pad events have no
   --  public setters, so the helper mirrors GDK's private C record layout.
   procedure Set_Pad_Event
     (Event  : Gdk.Event.Gdk_Event;
      Group  : Interfaces.C.unsigned;
      Mode   : Interfaces.C.unsigned;
      Button : Interfaces.C.unsigned;
      Index  : Interfaces.C.unsigned;
      Value  : Glib.Gdouble) is
      Data : constant Pad_Event_Pointers.Object_Pointer :=
        Pad_Event_Pointers.To_Pointer (Gdk.Event.Get_Object (Event));
   begin
      Data.all.Group := Group;
      Data.all.Mode := Mode;
      Data.all.Button := Button;
      Data.all.Index := Index;
      Data.all.Value := Value;
   end Set_Pad_Event;

   --  Set deterministic crossing fields; an absent child surface is valid.
   procedure Set_Crossing_Event
     (Event : Gdk.Event.Gdk_Event;
      Mode  : Gdk.Event.Crossing_Event.Gdk_Crossing_Mode;
      Detail : Gdk.Event.Crossing_Event.Gdk_Notify_Type;
      Focus : Boolean) is
      Data : constant Crossing_Event_Pointers.Object_Pointer :=
        Crossing_Event_Pointers.To_Pointer (Gdk.Event.Get_Object (Event));
   begin
      Data.all.State := 0;
      Data.all.Mode := Mode;
      Data.all.X := 1.0;
      Data.all.Y := 2.0;
      Data.all.Detail := Detail;
      Data.all.Focus := (if Focus then 1 else 0);
      Data.all.Child_Surface := System.Null_Address;
   end Set_Crossing_Event;

   --  Set the private C flag read by Gdk.Touch_Event's public getter.
   procedure Set_Touch_Event
     (Event : Gdk.Event.Gdk_Event;
      Emulating_Pointer : Boolean) is
      Data : constant Touch_Event_Pointers.Object_Pointer :=
        Touch_Event_Pointers.To_Pointer (Gdk.Event.Get_Object (Event));
   begin
      Data.all.Touch_Emulating := (if Emulating_Pointer then 1 else 0);
   end Set_Touch_Event;

   procedure Test_Ownership with Convention => C;
   procedure Test_Typed_Events with Convention => C;
   procedure Test_Event_Accessors with Convention => C;
   procedure Test_Scroll_Event_Accessors with Convention => C;
   procedure Test_Key_Event_Accessors with Convention => C;
   procedure Test_Touchpad_Event_Accessors with Convention => C;
   procedure Test_Touch_Event_Accessors with Convention => C;
   procedure Test_Pad_Event_Accessors with Convention => C;
   procedure Test_Crossing_Event_Accessors with Convention => C;
   procedure Test_Grab_Broken_Event_Accessors with Convention => C;

   --  Check null initialization, raw pointer access, same-object assignment,
   --  replacement, clearing, and Ada controlled-copy reference management.
   procedure Test_Ownership is
      First : Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Motion_Event.Get_Type, Gdk.Event.Motion_Notify);
      Second : Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Button_Event.Get_Type, Gdk.Event.Button_Press);
      Empty : Gdk.Event.Gdk_Event;
      Copy  : Gdk.Event.Gdk_Event;
      Raw   : constant System.Address := Gdk.Event.Get_Object (First);
   begin
      Glib.Test.Assert_True (Gdk.Event.Is_Null (Empty));
      Glib.Test.Assert_False (Gdk.Event.Is_Null (First));
      Glib.Test.Assert_True (Gdk.Event.Get_Object (First) = Raw);

      --  The copy exercises Adjust; changing the original must leave the copy
      --  holding its own reference to the first event.
      Copy := First;
      Glib.Test.Assert_True (Gdk.Event.Get_Object (Copy) = Raw);
      Gdk.Event.Set_Object (First, Raw);
      Glib.Test.Assert_True (Gdk.Event.Get_Object (First) = Raw);
      Gdk.Event.Set_Object (First, Gdk.Event.Get_Object (Second));
      Glib.Test.Assert_True
        (Gdk.Event.Get_Object (First) = Gdk.Event.Get_Object (Second));
      Glib.Test.Assert_True (Gdk.Event.Get_Object (Copy) = Raw);
      Gdk.Event.Set_Object (First, System.Null_Address);
      Glib.Test.Assert_True (Gdk.Event.Is_Null (First));
   end Test_Ownership;

   --  Each event subtype exercises its GType predicate and matching cast.
   --  Matching casts must retain the exact event pointer in the typed handle.
   procedure Test_Typed_Events is
   begin
      --  Button events cover the pointer-button subtype and its Ada view.
      declare
         Event : constant Gdk.Event.Gdk_Event := New_Event
           (Gdk.Event.Button_Event.Get_Type, Gdk.Event.Button_Press);
         Typed : constant Gdk.Event.Button_Event.Gdk_Button_Event :=
           Event.As_Button_Event;
      begin
         Glib.Test.Assert_True (Event.Is_Button_Event);
         Glib.Test.Assert_True
           (Gdk.Event.Get_Object (Typed) = Gdk.Event.Get_Object (Event));
      end;
      --  Crossing events are the enter/leave notifications for a surface.
      declare
         Event : constant Gdk.Event.Gdk_Event := New_Event
           (Gdk.Event.Crossing_Event.Get_Type, Gdk.Event.Enter_Notify);
         Typed : constant Gdk.Event.Crossing_Event.Gdk_Crossing_Event :=
           Event.As_Crossing_Event;
      begin
         Glib.Test.Assert_True (Event.Is_Crossing_Event);
         Glib.Test.Assert_True
           (Gdk.Event.Get_Object (Typed) = Gdk.Event.Get_Object (Event));
      end;
      --  Delete events represent a request to close or hide a surface.
      declare
         Event : constant Gdk.Event.Gdk_Event := New_Event
           (Gdk.Event.Delete_Event.Get_Type, Gdk.Event.Delete);
         Typed : constant Gdk.Event.Delete_Event.Gdk_Delete_Event :=
           Event.As_Delete_Event;
      begin
         Glib.Test.Assert_True (Event.Is_Delete_Event);
         Glib.Test.Assert_True
           (Gdk.Event.Get_Object (Typed) = Gdk.Event.Get_Object (Event));
      end;
      --  DND events are distinct from ordinary pointer motion and buttons.
      declare
         Event : constant Gdk.Event.Gdk_Event := New_Event
           (Gdk.Event.DND_Event.Get_Type, Gdk.Event.Drag_Enter);
         Typed : constant Gdk.Event.DND_Event.Gdk_DND_Event :=
           Event.As_DND_Event;
      begin
         Glib.Test.Assert_True (Event.Is_DND_Event);
         Glib.Test.Assert_True
           (Gdk.Event.Get_Object (Typed) = Gdk.Event.Get_Object (Event));
         --  A freshly created synthetic DND event has no associated drop.
         Glib.Test.Assert_True
           (Gdk.Event.DND_Event.Get_Drop (Event) = null);
      end;
      --  Focus events report a surface's keyboard-focus transition.
      declare
         Event : constant Gdk.Event.Gdk_Event := New_Event
           (Gdk.Event.Focus_Event.Get_Type, Gdk.Event.Focus_Change);
         Typed : constant Gdk.Event.Focus_Event.Gdk_Focus_Event :=
           Event.As_Focus_Event;
      begin
         Glib.Test.Assert_True (Event.Is_Focus_Event);
         --  A fresh synthetic event has a clear focus-entering flag.
         Glib.Test.Assert_False (Gdk.Event.Focus_Event.Get_In (Event));
         Glib.Test.Assert_True
           (Gdk.Event.Get_Object (Typed) = Gdk.Event.Get_Object (Event));
      end;
      --  Grab-broken events report when an active input grab is lost.
      declare
         Event : constant Gdk.Event.Gdk_Event := New_Event
           (Gdk.Event.Grab_Broken_Event.Get_Type, Gdk.Event.Grab_Broken);
         Typed : constant Gdk.Event.Grab_Broken_Event.Gdk_Grab_Broken_Event :=
           Event.As_Grab_Broken_Event;
      begin
         Glib.Test.Assert_True (Event.Is_Grab_Broken_Event);
         Glib.Test.Assert_True
           (Gdk.Event.Get_Object (Typed) = Gdk.Event.Get_Object (Event));
      end;
      --  Key events represent keyboard input, including press and release.
      declare
         Event : constant Gdk.Event.Gdk_Event := New_Event
           (Gdk.Event.Key_Event.Get_Type, Gdk.Event.Key_Press);
         Typed : constant Gdk.Event.Key_Event.Gdk_Key_Event :=
           Event.As_Key_Event;
      begin
         Glib.Test.Assert_True (Event.Is_Key_Event);
         Glib.Test.Assert_True
           (Gdk.Event.Get_Object (Typed) = Gdk.Event.Get_Object (Event));
      end;
      --  Motion events carry pointer coordinates and modifier state.
      declare
         Event : constant Gdk.Event.Gdk_Event := New_Event
           (Gdk.Event.Motion_Event.Get_Type, Gdk.Event.Motion_Notify);
         Typed : constant Gdk.Event.Motion_Event.Gdk_Motion_Event :=
           Event.As_Motion_Event;
      begin
         Glib.Test.Assert_True (Event.Is_Motion_Event);
         Glib.Test.Assert_True
           (Gdk.Event.Get_Object (Typed) = Gdk.Event.Get_Object (Event));
      end;
      --  Pad events cover tablet-pad buttons, rings, strips, and dials.
      declare
         Event : constant Gdk.Event.Gdk_Event := New_Event
           (Gdk.Event.Pad_Event.Get_Type, Gdk.Event.Pad_Button_Press);
         Typed : constant Gdk.Event.Pad_Event.Gdk_Pad_Event :=
           Event.As_Pad_Event;
      begin
         Glib.Test.Assert_True (Event.Is_Pad_Event);
         Glib.Test.Assert_True
           (Gdk.Event.Get_Object (Typed) = Gdk.Event.Get_Object (Event));
      end;
      --  Proximity events identify a tool entering or leaving a tablet area.
      declare
         Event : constant Gdk.Event.Gdk_Event := New_Event
           (Gdk.Event.Proximity_Event.Get_Type, Gdk.Event.Proximity_In);
         Typed : constant Gdk.Event.Proximity_Event.Gdk_Proximity_Event :=
           Event.As_Proximity_Event;
      begin
         Glib.Test.Assert_True (Event.Is_Proximity_Event);
         Glib.Test.Assert_True
           (Gdk.Event.Get_Object (Typed) = Gdk.Event.Get_Object (Event));
      end;
      --  Scroll events carry wheel or surface-scroll information.
      declare
         Event : constant Gdk.Event.Gdk_Event := New_Event
           (Gdk.Event.Scroll_Event.Get_Type, Gdk.Event.Scroll);
         Typed : constant Gdk.Event.Scroll_Event.Gdk_Scroll_Event :=
           Event.As_Scroll_Event;
      begin
         Glib.Test.Assert_True (Event.Is_Scroll_Event);
         Glib.Test.Assert_True
           (Gdk.Event.Get_Object (Typed) = Gdk.Event.Get_Object (Event));
      end;
      --  Touch events group updates belonging to a single touch sequence.
      declare
         Event : constant Gdk.Event.Gdk_Event := New_Event
           (Gdk.Event.Touch_Event.Get_Type, Gdk.Event.Touch_Begin);
         Typed : constant Gdk.Event.Touch_Event.Gdk_Touch_Event :=
           Event.As_Touch_Event;
      begin
         Glib.Test.Assert_True (Event.Is_Touch_Event);
         Glib.Test.Assert_True
           (Gdk.Event.Get_Object (Typed) = Gdk.Event.Get_Object (Event));
      end;
      --  Touchpad events carry gesture phases such as swipe or pinch.
      declare
         Event : constant Gdk.Event.Gdk_Event := New_Event
           (Gdk.Event.Touchpad_Event.Get_Type, Gdk.Event.Touchpad_Swipe);
         Typed : constant Gdk.Event.Touchpad_Event.Gdk_Touchpad_Event :=
           Event.As_Touchpad_Event;
      begin
         Glib.Test.Assert_True (Event.Is_Touchpad_Event);
         Glib.Test.Assert_True
           (Gdk.Event.Get_Object (Typed) = Gdk.Event.Get_Object (Event));
      end;
   end Test_Typed_Events;

   --  Check both values of the implicit-grab flag and the borrowed grab
   --  surface returned from a synthetic grab-broken event.
   procedure Test_Grab_Broken_Event_Accessors is
      Implicit_Event : constant Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Grab_Broken_Event.Get_Type, Gdk.Event.Grab_Broken);
      Explicit_Event : constant Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Grab_Broken_Event.Get_Type, Gdk.Event.Grab_Broken);
   begin
      Set_Grab_Broken_Event (Implicit_Event, True);
      Set_Grab_Broken_Event (Explicit_Event, False);

      Glib.Test.Assert_True
        (Gdk.Event.Grab_Broken_Event.Get_Implicit (Implicit_Event));
      Glib.Test.Assert_True
        (not Gdk.Event.Grab_Broken_Event.Get_Implicit (Explicit_Event));
      Glib.Test.Assert_True
        (Gdk.Event.Grab_Broken_Event.Get_Grab_Surface (Implicit_Event) =
           Surface);
      Glib.Test.Assert_True
        (Gdk.Event.Grab_Broken_Event.Get_Grab_Surface (Explicit_Event) =
           Surface);
   end Test_Grab_Broken_Event_Accessors;

   --  Crossing accessors return the mode and notify detail stored in the
   --  event, and translate the C gboolean focus flag to Ada Boolean.
   procedure Test_Crossing_Event_Accessors is
      Focused : constant Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Crossing_Event.Get_Type, Gdk.Event.Enter_Notify);
      Unfocused : constant Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Crossing_Event.Get_Type, Gdk.Event.Leave_Notify);
   begin
      --  Exercise distinct values, including both truth values for focus.
      --  Enter and leave are both valid crossing types for the bound getters.
      Set_Crossing_Event
        (Focused, Gdk.Event.Crossing_Event.Gtk_Grab,
         Gdk.Event.Crossing_Event.Nonlinear_Virtual, True);
      Set_Crossing_Event
        (Unfocused, Gdk.Event.Crossing_Event.Touch_End,
         Gdk.Event.Crossing_Event.Inferior, False);

      Glib.Test.Assert_True
        (Gdk.Event.Crossing_Event.Get_Mode (Focused) =
           Gdk.Event.Crossing_Event.Gtk_Grab);
      Glib.Test.Assert_True
        (Gdk.Event.Crossing_Event.Get_Detail (Focused) =
           Gdk.Event.Crossing_Event.Nonlinear_Virtual);
      Glib.Test.Assert_True (Gdk.Event.Crossing_Event.Get_Focus (Focused));
      Glib.Test.Assert_True
        (Gdk.Event.Crossing_Event.Get_Mode (Unfocused) =
           Gdk.Event.Crossing_Event.Touch_End);
      Glib.Test.Assert_True
        (Gdk.Event.Crossing_Event.Get_Detail (Unfocused) =
           Gdk.Event.Crossing_Event.Inferior);
      Glib.Test.Assert_False
        (Gdk.Event.Crossing_Event.Get_Focus (Unfocused));
   end Test_Crossing_Event_Accessors;

   --  Check known motion coordinates, derived gesture values, and metadata.
   --  A button event also covers the documented context-menu convention.
   procedure Test_Event_Accessors is
      First : Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Motion_Event.Get_Type, Gdk.Event.Motion_Notify);
      Second : Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Motion_Event.Get_Type, Gdk.Event.Motion_Notify);
      Button : Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Button_Event.Get_Type, Gdk.Event.Button_Press);
      Missing_Position : constant Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Delete_Event.Get_Type, Gdk.Event.Delete);
      X, Y, Value : Glib.Gdouble := 0.0;
      Distance, Angle : Glib.Gdouble := 0.0;
   begin
      Set_Motion (First, 1.0, 2.0);
      Set_Motion (Second, 4.0, 2.0);
      Set_Button (Button, 3);

      Glib.Test.Assert_True (Gdk.Event.Get_Type /= Glib.GType_Invalid);
      --  The typed event enum and timestamp should preserve their C values.
      Glib.Test.Assert_True
        (Gdk.Event.Get_Event_Type (First) = Gdk.Event.Motion_Notify);
      Glib.Test.Assert_Cmpuint_Eq
        (Glib.Guint (Gdk.Event.Get_Time (First)), 12345);
      Glib.Test.Assert_True
        (Gdk.Event.Get_Position (First, X, Y));
      Glib.Test.Assert_Cmpfloat_Eq (X, 1.0);
      Glib.Test.Assert_Cmpfloat_Eq (Y, 2.0);
      Glib.Test.Assert_False
        (Gdk.Event.Get_Position (Missing_Position, X, Y));
      Glib.Test.Assert_True
        (Gdk.Event.Get_Center (First, Second, X, Y));
      Glib.Test.Assert_Cmpfloat_Eq (X, 2.5);
      Glib.Test.Assert_Cmpfloat_Eq (Y, 2.0);
      Glib.Test.Assert_True
        (Gdk.Event.Get_Distance (First, Second, Distance));
      Glib.Test.Assert_Cmpfloat_Eq (Distance, 3.0);
      Glib.Test.Assert_True (Gdk.Event.Get_Angle (First, Second, Angle));
      Glib.Test.Assert_Cmpfloat_Eq (Angle, 0.0);
      Glib.Test.Assert_False
        (Gdk.Event.Get_Center (First, Missing_Position, X, Y));

      --  Motion has no extra axis data or touch sequence in this mouse event.
      Glib.Test.Assert_False (Gdk.Event.Get_Axis (First, 0, Value));
      Glib.Test.Assert_True
        (Gdk.Event.Get_Modifier_State (First) =
           Gdk.Enums.Gdk_Modifier_Type'(0));
      Glib.Test.Assert_False (Gdk.Event.Get_Pointer_Emulated (First));
      --  Mouse motion has no touch sequence, so its identity token is null.
      Glib.Test.Assert_True
        (Gdk.Event.Get_Event_Sequence (First) =
           null);

      --  Event metadata follows the strong surface/device references installed
      --  by New_Event; the mouse event has no tablet tool attached.
      Glib.Test.Assert_True
        (Glib.Object.Get_Object (Gdk.Event.Get_Surface (First)) =
           Glib.Object.Get_Object (Surface));
      Glib.Test.Assert_True
        (Glib.Object.Get_Object (Gdk.Event.Get_Device (First)) =
           Glib.Object.Get_Object (Device));
      Glib.Test.Assert_True
        (Glib.Object.Get_Object (Gdk.Event.Get_Display (First)) =
           Glib.Object.Get_Object (Display));
      Glib.Test.Assert_True (Gdk.Event.Get_Device_Tool (First) = null);
      Glib.Test.Assert_True
        (Glib.Object.Get_Object (Gdk.Event.Get_Seat (First)) =
           Glib.Object.Get_Object (Seat));

      --  A secondary button opens a context menu; motion does not.
      --  The button-event accessor returns the number installed above.
      Glib.Test.Assert_Cmpuint_Eq
        (Gdk.Event.Button_Event.Get_Button (Button), 3);
      Glib.Test.Assert_True (Gdk.Event.Triggers_Context_Menu (Button));
      Glib.Test.Assert_False (Gdk.Event.Triggers_Context_Menu (First));
   end Test_Event_Accessors;

   --  Scroll getters preserve the deltas, direction, unit, relative-motion
   --  direction, and stop marker supplied by the backend event.
   procedure Test_Scroll_Event_Accessors is
      Event : Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Scroll_Event.Get_Type, Gdk.Event.Scroll);
      Stop_Event : Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Scroll_Event.Get_Type, Gdk.Event.Scroll);
      Delta_X, Delta_Y : Glib.Gdouble := 0.0;
   begin
      --  Smooth direction is the documented case that reports scroll deltas.
      Set_Scroll_Event
        (Event,
         Direction => Gdk.Event.Scroll_Event.Smooth,
         Delta_X => 1.5,
         Delta_Y => -2.25,
         Is_Stop => False,
         Unit => Gdk.Event.Scroll_Event.Surface,
         Relative_Direction => Gdk.Event.Scroll_Event.Inverted);
      Gdk.Event.Scroll_Event.Get_Deltas (Event, Delta_X, Delta_Y);
      Glib.Test.Assert_Cmpfloat_Eq (Delta_X, 1.5);
      Glib.Test.Assert_Cmpfloat_Eq (Delta_Y, -2.25);
      Glib.Test.Assert_True
        (Gdk.Event.Scroll_Event.Get_Direction (Event) =
           Gdk.Event.Scroll_Event.Smooth);
      --  Check the two enum-valued metadata fields independently as well.
      Glib.Test.Assert_True
        (Gdk.Event.Scroll_Event.Get_Relative_Direction (Event) =
           Gdk.Event.Scroll_Event.Inverted);
      Glib.Test.Assert_True
        (Gdk.Event.Scroll_Event.Get_Unit (Event) =
           Gdk.Event.Scroll_Event.Surface);
      Glib.Test.Assert_False (Gdk.Event.Scroll_Event.Is_Stop (Event));

      --  A stop event has no movement deltas and reports its stop marker.
      Set_Scroll_Event
        (Stop_Event,
         Direction => Gdk.Event.Scroll_Event.Smooth,
         Delta_X => 0.0,
         Delta_Y => 0.0,
         Is_Stop => True,
         Unit => Gdk.Event.Scroll_Event.Wheel,
         Relative_Direction => Gdk.Event.Scroll_Event.Identical);
      Gdk.Event.Scroll_Event.Get_Deltas (Stop_Event, Delta_X, Delta_Y);
      Glib.Test.Assert_Cmpfloat_Eq (Delta_X, 0.0);
      Glib.Test.Assert_Cmpfloat_Eq (Delta_Y, 0.0);
      Glib.Test.Assert_True (Gdk.Event.Scroll_Event.Is_Stop (Stop_Event));
   end Test_Scroll_Event_Accessors;

   --  Check key translations, modifier status, and the shortcut results that
   --  are also exercised by testsuite/c_tests/shortcuts.c:test_trigger_trigger.
   procedure Test_Key_Event_Accessors is
      Event : Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Key_Event.Get_Type, Gdk.Event.Key_Press);
      Modifier_Event : Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Key_Event.Get_Type, Gdk.Event.Key_Release);
      --  This is the shortcut used by the corresponding C test's key event.
      Trigger : constant Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger :=
        Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Parse_String
          ("<Control>a");
      Mismatch : constant Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger :=
        Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Parse_String
          ("<Control>b");
      Keyval    : Glib.Guint := 0;
      Modifiers : Gdk.Enums.Gdk_Modifier_Type :=
        Gdk.Enums.Gdk_No_Modifier_Mask;
      Key_A : constant Glib.Guint :=
        Glib.Guint (Character'Pos ('a'));
   begin
      Set_Key_Event (Event, Is_Modifier => False);
      Set_Key_Event (Modifier_Event, Is_Modifier => True);

      Glib.Test.Assert_True
        (Gdk.Event.Key_Event.Get_Consumed_Modifiers (Event) =
           Gdk.Enums.Gdk_Shift_Mask);
      Glib.Test.Assert_Cmpuint_Eq
        (Gdk.Event.Key_Event.Get_Keycode (Event), 38);
      Glib.Test.Assert_Cmpuint_Eq
        (Gdk.Event.Key_Event.Get_Keyval (Event), Key_A);
      Glib.Test.Assert_Cmpuint_Eq
        (Gdk.Event.Key_Event.Get_Layout (Event), 2);
      Glib.Test.Assert_Cmpuint_Eq
        (Gdk.Event.Key_Event.Get_Level (Event), 1);
      Glib.Test.Assert_False (Gdk.Event.Key_Event.Is_Modifier (Event));
      Glib.Test.Assert_True
        (Gdk.Event.Key_Event.Is_Modifier (Modifier_Event));

      --  The event's no-lock translation is lowercase 'a' with Control active.
      Glib.Test.Assert_True
        (Gdk.Event.Key_Event.Get_Match (Event, Keyval, Modifiers));
      Glib.Test.Assert_Cmpuint_Eq (Keyval, Key_A);
      Glib.Test.Assert_True (Modifiers = Gdk.Enums.Gdk_Control_Mask);
      Glib.Test.Assert_True
        (Gdk.Event.Key_Event.Matches
           (Event, Key_A, Gdk.Enums.Gdk_Control_Mask) =
             Gdk.Key_Match.Gdk_Key_Match_Exact);
      --  A different active modifier must not match the Control event.
      Glib.Test.Assert_True
        (Gdk.Event.Key_Event.Matches
           (Event, Key_A, Gdk.Enums.Gdk_Shift_Mask) =
             Gdk.Key_Match.Gdk_Key_Match_None);

      --  GtkShortcutTrigger delegates to the same event matching rules.
      --  Matching the translated Control+A key is exact; Control+B misses.
      Glib.Test.Assert_True
        (Trigger.Trigger (Event, Enable_Mnemonics => False) =
           Gdk.Key_Match.Gdk_Key_Match_Exact);
      Glib.Test.Assert_True
        (Mismatch.Trigger (Event, Enable_Mnemonics => True) =
           Gdk.Key_Match.Gdk_Key_Match_None);
      Glib.Object.Unref (Glib.Object.GObject (Trigger));
      Glib.Object.Unref (Glib.Object.GObject (Mismatch));
   end Test_Key_Event_Accessors;

   --  Check all values extracted from a synthetic three-finger pinch event.
   procedure Test_Touchpad_Event_Accessors is
      Event : Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Touchpad_Event.Get_Type, Gdk.Event.Touchpad_Pinch);
      Delta_X, Delta_Y : Glib.Gdouble := 0.0;
   begin
      Set_Touchpad_Event (Event);

      --  Swipe motion deltas remain available on a pinch event as well.
      Gdk.Event.Touchpad_Event.Get_Deltas (Event, Delta_X, Delta_Y);
      Glib.Test.Assert_Cmpfloat_Eq (Delta_X, 1.25);
      Glib.Test.Assert_Cmpfloat_Eq (Delta_Y, -2.5);
      Glib.Test.Assert_True
        (Gdk.Event.Touchpad_Event.Get_Gesture_Phase (Event) =
           Gdk.Event.Touchpad_Event.Phase_Update);
      Glib.Test.Assert_Cmpuint_Eq
        (Gdk.Event.Touchpad_Event.Get_N_Fingers (Event), 3);
      --  Pinch-specific getters report angle and relative scale separately.
      Glib.Test.Assert_Cmpfloat_Eq
        (Gdk.Event.Touchpad_Event.Get_Pinch_Angle_Delta (Event), 0.75);
      Glib.Test.Assert_Cmpfloat_Eq
        (Gdk.Event.Touchpad_Event.Get_Pinch_Scale (Event), 1.5);
   end Test_Touchpad_Event_Accessors;

   --  Check both documented results for whether a touch event emulates a
   --  pointer event, using synthetic records so no touch hardware is needed.
   procedure Test_Touch_Event_Accessors is
      Emulating_Event : constant Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Touch_Event.Get_Type, Gdk.Event.Touch_Begin);
      Touch_Only_Event : constant Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Touch_Event.Get_Type, Gdk.Event.Touch_Update);
   begin
      Set_Touch_Event (Emulating_Event, True);
      Set_Touch_Event (Touch_Only_Event, False);
      Glib.Test.Assert_True
        (Gdk.Event.Touch_Event.Get_Emulating_Pointer (Emulating_Event));
      Glib.Test.Assert_False
        (Gdk.Event.Touch_Event.Get_Emulating_Pointer (Touch_Only_Event));
   end Test_Touch_Event_Accessors;

   --  Verify values returned by the button, axis, and group/mode pad getters.
   --  Each event uses the event type required by its corresponding GDK API.
   procedure Test_Pad_Event_Accessors is
      Button_Event : Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Pad_Event.Get_Type, Gdk.Event.Pad_Button_Press);
      Axis_Event : Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Pad_Event.Get_Type, Gdk.Event.Pad_Ring);
      Group_Event : Gdk.Event.Gdk_Event := New_Event
        (Gdk.Event.Pad_Event.Get_Type, Gdk.Event.Pad_Group_Mode);
      Index : Glib.Guint := 0;
      Value : Glib.Gdouble := 0.0;
      Group : Glib.Guint := 0;
      Mode  : Glib.Guint := 0;
   begin
      Set_Pad_Event (Button_Event, 2, 5, 7, 0, 0.0);
      Set_Pad_Event (Axis_Event, 3, 4, 0, 6, 0.625);
      Set_Pad_Event (Group_Event, 8, 9, 0, 0, 0.0);

      Glib.Test.Assert_Cmpuint_Eq
        (Gdk.Event.Pad_Event.Get_Button (Button_Event), 7);

      Gdk.Event.Pad_Event.Get_Axis_Value (Axis_Event, Index, Value);
      Glib.Test.Assert_Cmpuint_Eq (Index, 6);
      Glib.Test.Assert_Cmpfloat_Eq (Value, 0.625);

      Gdk.Event.Pad_Event.Get_Group_Mode (Group_Event, Group, Mode);
      Glib.Test.Assert_Cmpuint_Eq (Group, 8);
      Glib.Test.Assert_Cmpuint_Eq (Mode, 9);
   end Test_Pad_Event_Accessors;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   --  A presented native surface and default pointer provide valid event
   --  context for the metadata accessors, while all event coordinates remain
   --  deterministic and independent of physical input.
   Display := Gdk.Display.Get_Default;
   Seat := Display.Get_Default_Seat;
   Device := Seat.Get_Pointer;
   Window := Gtk.Window.Gtk_Window_New;
   Window.Present;
   Surface := Window.Get_Surface;
   Glib.Test.Assert_True (Display /= null);
   Glib.Test.Assert_True (Seat /= null);
   Glib.Test.Assert_True (Device /= null);
   Glib.Test.Assert_True (Surface /= null);

   Glib.Test.Add_Func
     ("/gdk-event/ownership", Test_Ownership'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-event/typed-events", Test_Typed_Events'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-event/grab-broken-event-accessors",
      Test_Grab_Broken_Event_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-event/accessors", Test_Event_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-event/scroll-event-accessors",
      Test_Scroll_Event_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-event/key-event-accessors",
      Test_Key_Event_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-event/touchpad-event-accessors",
      Test_Touchpad_Event_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-event/touch-event-accessors",
      Test_Touch_Event_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-event/pad-event-accessors",
      Test_Pad_Event_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-event/crossing-event-accessors",
      Test_Crossing_Event_Accessors'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
