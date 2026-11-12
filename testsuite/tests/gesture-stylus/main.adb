--  Tests Gtk.Gesture_Stylus construction, configuration, queries and signals.
--  The signal cases use explicit GObject emission so they need no tablet or
--  physical stylus, while the query cases check the no-current-event results.

with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with Gdk.Device;
with Gdk.Device_Tool;
with Gdk.Enums;
with Glib;
with Glib.Object;
with Glib.Properties;
with Glib.Test;
with Gtk.Drawing_Area;
with Gtk.Gesture_Stylus;
with Gtk.Main;
with Gtkada.Bindings;

procedure Main is
   use Glib;
   use Glib.Test;
   use type System.Address;
   use type Gtk.Gesture_Stylus.Gtk_Gesture_Stylus;
   use type Gdk.Device.Gdk_Time_Coord_Array_Access;
   use type Gdk.Device_Tool.Gdk_Device_Tool;

   --  Shared references let the callbacks verify their object and slot
   --  arguments without introducing a window or input-device dependency.
   Stylus : Gtk.Gesture_Stylus.Gtk_Gesture_Stylus;
   Slot   : Gtk.Drawing_Area.Gtk_Drawing_Area;
   Typed_Calls : Natural := 0;
   Slot_Calls  : Natural := 0;
   Check_No_Event_Queries : Boolean := False;
   Callback_Exception_Calls : Natural := 0;

   --  The first two arguments are fixed; GObject signal values are varargs.
   procedure Emit_Signal
     (Object : System.Address;
      Name   : Interfaces.C.Strings.chars_ptr;
      X      : Gdouble;
      Y      : Gdouble)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Typed_Callback
     (Self : access Gtk.Gesture_Stylus.Gtk_Gesture_Stylus_Record'Class;
      X, Y : Gdouble);

   procedure Slot_Callback
     (Self : access Glib.Object.GObject_Record'Class;
      X, Y : Gdouble);

   procedure Test_Construct_And_Property
   with Convention => C;

   procedure Test_Signals_And_Queries
   with Convention => C;

   --  Both callback signatures must receive the emitting coordinates. The
   --  slot form also verifies that GtkAda forwards the supplied GObject.
   procedure Typed_Callback
     (Self : access Gtk.Gesture_Stylus.Gtk_Gesture_Stylus_Record'Class;
      X, Y : Gdouble)
   is
   begin
      Assert_True (Glib.Object.Get_Object (Self) =
                   Glib.Object.Get_Object (Stylus));
      Assert_Cmpfloat_Eq (X, 12.5);
      Assert_Cmpfloat_Eq (Y, 34.25);
      Typed_Calls := Typed_Calls + 1;

      --  GTK documents these queries for use inside a stylus signal handler.
      --  The manually emitted signal has no current GDK event, so each API
      --  must report that no axis, tool or motion backlog is available.
      if Check_No_Event_Queries then
         declare
            Axes : constant Gdk.Enums.Gdk_Axis_Use_Array :=
              (Gdk.Enums.Gdk_Axis_X,
               Gdk.Enums.Gdk_Axis_Pressure,
               Gdk.Enums.Gdk_Axis_Ignore);
            Values : Gdouble_Array (0 .. 1);
            Axis_Value : Gdouble;
            Backlog : Gdk.Device.Gdk_Time_Coord_Array_Access;
         begin
            Assert_False
              (Stylus.Get_Axis
                 (Gdk.Enums.Gdk_Axis_Pressure, Axis_Value));
            Assert_False (Stylus.Get_Axes (Axes, Values));
            Assert_False (Stylus.Get_Device_Tool /= null);
            Assert_False (Stylus.Get_Backlog (Backlog));
            Assert_True (Backlog = null);
         end;
      end if;
   end Typed_Callback;

   procedure Slot_Callback
     (Self : access Glib.Object.GObject_Record'Class;
      X, Y : Gdouble)
   is
   begin
      Assert_True (Glib.Object.Get_Object (Self) =
                   Glib.Object.Get_Object (Slot));
      Assert_Cmpfloat_Eq (X, 12.5);
      Assert_Cmpfloat_Eq (Y, 34.25);
      Slot_Calls := Slot_Calls + 1;
   end Slot_Callback;

   --  Exceptions raised by Ada callbacks must be contained at the C boundary
   --  and delivered to GtkAda's configured exception handler.
   procedure Raise_Typed_Callback
     (Self : access Gtk.Gesture_Stylus.Gtk_Gesture_Stylus_Record'Class;
      X, Y : Gdouble);
   procedure Raise_Slot_Callback
     (Self : access Glib.Object.GObject_Record'Class;
      X, Y : Gdouble);
   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   --  These two callbacks deliberately fail so each generated marshaller's
   --  exception guard is tested for both supported callback signatures.
   procedure Raise_Typed_Callback
     (Self : access Gtk.Gesture_Stylus.Gtk_Gesture_Stylus_Record'Class;
      X, Y : Gdouble)
   is
      pragma Unreferenced (Self, X, Y);
   begin
      raise Program_Error with "expected typed callback exception";
   end Raise_Typed_Callback;

   procedure Raise_Slot_Callback
     (Self : access Glib.Object.GObject_Record'Class;
      X, Y : Gdouble)
   is
      pragma Unreferenced (Self, X, Y);
   begin
      raise Program_Error with "expected slot callback exception";
   end Raise_Slot_Callback;

   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (Occurrence);
   begin
      Callback_Exception_Calls := Callback_Exception_Calls + 1;
   end Record_Callback_Exception;

   --  Exercise both constructor forms, idempotent Initialize, and the public
   --  property together with its method accessors.
   procedure Test_Construct_And_Property is
      By_Function : constant Gtk.Gesture_Stylus.Gtk_Gesture_Stylus :=
        Gtk.Gesture_Stylus.Gtk_Gesture_Stylus_New;
      By_Procedure : Gtk.Gesture_Stylus.Gtk_Gesture_Stylus;
   begin
      Assert_True (Gtk.Gesture_Stylus.Get_Type /= GType_Invalid);
      Gtk.Gesture_Stylus.Gtk_New (By_Procedure);
      Gtk.Gesture_Stylus.Initialize (By_Procedure);

      --  Verify GTK's default first, then exercise both boolean property values
      --  through the methods and the generated property binding.
      Assert_True (By_Function.Get_Stylus_Only);
      By_Function.Set_Stylus_Only (True);
      Assert_True (By_Function.Get_Stylus_Only);
      Assert_True
        (Glib.Properties.Get_Property
           (By_Function, Gtk.Gesture_Stylus.Stylus_Only_Property));

      Glib.Properties.Set_Property
        (By_Function, Gtk.Gesture_Stylus.Stylus_Only_Property, False);
      Assert_False (By_Function.Get_Stylus_Only);
      By_Procedure.Set_Stylus_Only (True);
      Assert_True (By_Procedure.Get_Stylus_Only);
   end Test_Construct_And_Property;

   --  Emit each signal with known values to run every typed and slot signal
   --  connector and marshaller without relying on hardware-generated events.
   procedure Test_Signals_And_Queries is
      Name : Interfaces.C.Strings.chars_ptr;
      --  The signal name is allocated for GObject's NUL-terminated C API and
      --  released immediately after synchronous delivery.
      procedure Emit (Signal : String) is
      begin
         Name := Interfaces.C.Strings.New_String (Signal);
         Emit_Signal
           (Glib.Object.Get_Object (Stylus), Name, 12.5, 34.25);
         Interfaces.C.Strings.Free (Name);
      end Emit;
   begin
      Stylus := Gtk.Gesture_Stylus.Gtk_Gesture_Stylus_New;
      Slot := Gtk.Drawing_Area.Gtk_Drawing_Area_New;
      Typed_Calls := 0;
      Slot_Calls := 0;

      --  Register typed and slot callbacks on each of the four stylus signals;
      --  After=True also exercises the generated after-handler connection.
      Stylus.On_Down (Typed_Callback'Unrestricted_Access, After => True);
      Stylus.On_Down (Slot_Callback'Unrestricted_Access, Slot => Slot);
      Stylus.On_Motion (Typed_Callback'Unrestricted_Access);
      Stylus.On_Motion (Slot_Callback'Unrestricted_Access, Slot => Slot);
      Stylus.On_Proximity (Typed_Callback'Unrestricted_Access);
      Stylus.On_Proximity (Slot_Callback'Unrestricted_Access, Slot => Slot);
      Stylus.On_Up (Typed_Callback'Unrestricted_Access);
      Stylus.On_Up (Slot_Callback'Unrestricted_Access, Slot => Slot);

      Check_No_Event_Queries := True;
      Emit ("motion");
      Check_No_Event_Queries := False;
      Emit ("down");
      Emit ("proximity");
      Emit ("up");
      Assert_Cmpuint_Eq (Guint (Typed_Calls), 4);
      Assert_Cmpuint_Eq (Guint (Slot_Calls), 4);

      --  Raising from either callback signature must not unwind through C.
      Callback_Exception_Calls := 0;
      Gtkada.Bindings.Set_On_Exception
        (Record_Callback_Exception'Unrestricted_Access);
      Stylus.On_Down (Raise_Typed_Callback'Unrestricted_Access);
      Stylus.On_Down
        (Raise_Slot_Callback'Unrestricted_Access, Slot => Slot);
      Emit ("down");
      Assert_Cmpuint_Eq (Guint (Callback_Exception_Calls), 2);

   end Test_Signals_And_Queries;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gesture-stylus/construction-property",
      Test_Construct_And_Property'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gesture-stylus/signals-queries",
      Test_Signals_And_Queries'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
