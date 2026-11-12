--  Exercises Gtk.Event_Controller_Legacy construction, the inherited base
--  controller API, and both event signal callback forms. The related legacy
--  propagation cases are in
--  testsuite/c_tests/gestures.c (test_mixed and test_early_exit); the focused
--  signal emissions below avoid depending on physical or queued pointer input.

with Ada.Command_Line;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with System.Address_To_Access_Conversions;

with Gdk.Event;
with Gdk.Event.Motion_Event;
with Gdk.Enums;
with Gtk.Enums;
with Glib;
with Glib.Object; use Glib.Object;
with Glib.Test; use Glib.Test;
with Gtk.Event_Controller;        use Gtk.Event_Controller;
with Gtk.Event_Controller_Legacy; use Gtk.Event_Controller_Legacy;
with Gtk.Main;
with Gtk.Widget;

use type Glib.GType;
use type Gdk.Enums.Gdk_Modifier_Type;
use type Gdk.Gdk_Device;
use type System.Address;
use type Gtk.Enums.Gtk_Propagation_Limit;
use type Gtk.Enums.Gtk_Propagation_Phase;
use type Gtk.Widget.Gtk_Widget;

procedure Event_Controller_Legacy is
   use Interfaces.C.Strings;

   --  These fields match the common GdkEvent prefix used by the event tests.
   type Event_Base_Record is record
      Class      : System.Address;
      Ref_Count  : Interfaces.C.int;
      Event_Type : Gdk.Event.Gdk_Event_Type;
      Surface    : System.Address;
      Device     : System.Address;
      Time       : Interfaces.C.unsigned;
      Flags      : Interfaces.C.unsigned_short;
   end record
     with Convention => C;

   package Event_Pointers is new
     System.Address_To_Access_Conversions (Event_Base_Record);

   --  GTK 4 has no public event constructor; create a motion event instance,
   --  wrap it with GtkAda's reference-counted handle, and release the raw ref.
   function Create_Instance (Type_Id : Glib.GType) return System.Address
     with Import, Convention => C,
       External_Name => "g_type_create_instance";
   procedure Event_Unref (Event : System.Address)
     with Import, Convention => C,
       External_Name => "gdk_event_unref";

   function New_Event return Gdk.Event.Gdk_Event is
      Raw  : constant System.Address :=
        Create_Instance (Gdk.Event.Motion_Event.Get_Type);
      Base : constant Event_Pointers.Object_Pointer :=
        Event_Pointers.To_Pointer (Raw);
      Result : Gdk.Event.Gdk_Event;
   begin
      --  Initialize the event tag before the runtime reference is released.
      Base.all.Event_Type := Gdk.Event.Motion_Notify;
      Base.all.Time := 12345;
      Result := Gdk.Event.From_Object (Raw);
      Event_Unref (Raw);
      return Result;
   end New_Event;

   --  g_signal_emit_by_name is variadic; the signal takes a GdkEvent pointer
   --  and writes its gboolean return value through the last argument.
   procedure Emit_Event
     (Instance : System.Address;
      Name     : chars_ptr;
      Event    : System.Address;
      Result   : access Glib.Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   Current_Controller : Gtk_Event_Controller_Legacy := null;
   Expected_Slot      : System.Address := System.Null_Address;
   Expected_Event     : System.Address := System.Null_Address;
   Typed_Calls        : Natural := 0;
   Slot_Calls         : Natural := 0;

   procedure Test_Constructors with Convention => C;
   procedure Test_Base_Controller with Convention => C;
   procedure Test_Signal_Handlers with Convention => C;
   procedure Test_Boolean_Return with Convention => C;

   --  Typed handlers receive the emitting controller and the exact event.
   function On_Event_Typed
     (Self  : access Gtk_Event_Controller_Legacy_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean is
   begin
      --  The typed conversion must preserve both the emitter and event handle.
      Assert_True (Self /= null);
      Assert_True (Get_Object (Self) = Get_Object (Current_Controller));
      Assert_True (Gdk.Event.Get_Object (Event) = Expected_Event);
      Typed_Calls := Typed_Calls + 1;
      return False;
   end On_Event_Typed;

   --  The slot overload receives its user-data object, not the emitter.
   function On_Event_Slot
     (Self  : access GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean is
   begin
      --  The generic callback must receive the connected slot object as data.
      Assert_True (Self /= null);
      Assert_True (Get_Object (Self) = Expected_Slot);
      Assert_True (Gdk.Event.Get_Object (Event) = Expected_Event);
      Slot_Calls := Slot_Calls + 1;
      return False;
   end On_Event_Slot;

   --  A true return checks that the Boolean result survives the marshaller.
   function Stop_Event
     (Self  : access Gtk_Event_Controller_Legacy_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean is
   begin
      Assert_True (Self /= null);
      Assert_True (Gdk.Event.Get_Object (Event) = Expected_Event);
      return True;
   end Stop_Event;

   --  Both construction entry points work, and Initialize is idempotent.
   procedure Test_Constructors is
      By_Function : constant Gtk_Event_Controller_Legacy :=
        Gtk_Event_Controller_Legacy_New;
      By_Procedure : Gtk_Event_Controller_Legacy;
   begin
      --  Reinitializing either constructor result must leave its GType intact.
      Gtk_New (By_Procedure);
      Gtk.Event_Controller_Legacy.Initialize (By_Function);
      Gtk.Event_Controller_Legacy.Initialize (By_Procedure);
      Assert_True (Gtk.Event_Controller_Legacy.Get_Type /= Glib.GType_Invalid);
      Assert_True
        (By_Function.Get_Type = Gtk.Event_Controller_Legacy.Get_Type);
      Assert_Cmpstr_Eq (String (Signal_Event), "event");
      Unref (By_Function);
      Unref (By_Procedure);
   end Test_Constructors;

   --  The idle event getters return empty values, and base properties round-trip.
   procedure Test_Base_Controller is
      Controller : constant Gtk_Event_Controller_Legacy :=
        Gtk_Event_Controller_Legacy_New;
      Base       : constant Gtk_Event_Controller :=
        Gtk_Event_Controller (Controller);
      Event      : constant Gdk.Event.Gdk_Event := Base.Get_Current_Event;
   begin
      --  With no event being dispatched, GTK documents null/zero event state.
      Assert_True (Gdk.Event.Get_Object (Event) = System.Null_Address);
      Assert_True (Base.Get_Current_Event_Device = null);
      Assert_True
        (Base.Get_Current_Event_State = Gdk.Enums.Gdk_No_Modifier_Mask);
      Assert_Cmpuint_Eq (Glib.Guint (Base.Get_Current_Event_Time), 0);
      Assert_True (Base.Get_Widget = null);

      --  Check both C-string paths: a name is copied, and empty clears it.
      Assert_Cmpstr_Eq (Base.Get_Name, "");
      Base.Set_Name ("legacy-controller-test");
      Assert_Cmpstr_Eq (Base.Get_Name, "legacy-controller-test");
      Base.Set_Name ("");
      Assert_Cmpstr_Eq (Base.Get_Name, "");

      --  Round-trip every legal propagation value through GTK's base API.
      for Limit in Gtk.Enums.Gtk_Propagation_Limit loop
         Base.Set_Propagation_Limit (Limit);
         Assert_True (Base.Get_Propagation_Limit = Limit);
      end loop;
      for Phase in Gtk.Enums.Gtk_Propagation_Phase loop
         Base.Set_Propagation_Phase (Phase);
         Assert_True (Base.Get_Propagation_Phase = Phase);
      end loop;

      --  Reset on an idle controller leaves it with no current event.
      Base.Reset;
      Assert_True
        (Gdk.Event.Get_Object (Base.Get_Current_Event) = System.Null_Address);
      Assert_True (Base.Get_Current_Event_Device = null);
      Assert_True
        (Base.Get_Current_Event_State = Gdk.Enums.Gdk_No_Modifier_Mask);
      Assert_Cmpuint_Eq (Glib.Guint (Base.Get_Current_Event_Time), 0);
      Unref (Controller);
   end Test_Base_Controller;

   --  Exercise both signal registration overloads and their false results.
   procedure Test_Signal_Handlers is
      Controller : constant Gtk_Event_Controller_Legacy :=
        Gtk_Event_Controller_Legacy_New;
      Slot       : constant Gtk_Event_Controller_Legacy :=
        Gtk_Event_Controller_Legacy_New;
      Event      : constant Gdk.Event.Gdk_Event := New_Event;
      Name       : chars_ptr := New_String (String (Signal_Event));
      Result     : aliased Glib.Gboolean := 1;
   begin
      Current_Controller := Controller;
      Expected_Slot := Get_Object (Slot);
      Expected_Event := Gdk.Event.Get_Object (Event);
      Typed_Calls := 0;
      Slot_Calls := 0;
      Controller.On_Event (On_Event_Typed'Unrestricted_Access);
      Controller.On_Event
        (On_Event_Slot'Unrestricted_Access, Slot, After => True);

      --  One emission synchronously calls both handlers with the same event.
      Emit_Event
        (Get_Object (Controller), Name, Expected_Event, Result'Access);
      Free (Name);
      Assert_Cmpuint_Eq (Glib.Guint (Typed_Calls), 1);
      Assert_Cmpuint_Eq (Glib.Guint (Slot_Calls), 1);
      Assert_Cmpint_Eq (Glib.Gint (Result), 0);

      Unref (Controller);
      Unref (Slot);
      Current_Controller := null;
      Expected_Slot := System.Null_Address;
      Expected_Event := System.Null_Address;
   end Test_Signal_Handlers;

   --  A true typed callback result is returned through the signal marshaller.
   procedure Test_Boolean_Return is
      Controller : constant Gtk_Event_Controller_Legacy :=
        Gtk_Event_Controller_Legacy_New;
      Event      : constant Gdk.Event.Gdk_Event := New_Event;
      Name       : chars_ptr := New_String (String (Signal_Event));
      Result     : aliased Glib.Gboolean := 0;
   begin
      Expected_Event := Gdk.Event.Get_Object (Event);
      Controller.On_Event (Stop_Event'Unrestricted_Access);
      --  A single typed handler returns true through GLib's gboolean result.
      Emit_Event
        (Get_Object (Controller), Name, Expected_Event, Result'Access);
      Assert_Cmpint_Eq (Glib.Gint (Result), 1);
      Unref (Controller);
      Free (Name);
      Expected_Event := System.Null_Address;
   end Test_Boolean_Return;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func
     ("/event-controller-legacy/constructors",
      Test_Constructors'Unrestricted_Access);
   Add_Func
     ("/event-controller-legacy/base-controller",
      Test_Base_Controller'Unrestricted_Access);
   Add_Func
     ("/event-controller-legacy/signals",
      Test_Signal_Handlers'Unrestricted_Access);
   Add_Func
     ("/event-controller-legacy/boolean-return",
      Test_Boolean_Return'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Event_Controller_Legacy;
