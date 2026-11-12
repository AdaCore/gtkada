--  Exercises Gdk.Seat accessors and signal marshallers with the default Xvfb
--  seat. No C tests directly exercise the gdk-seat API.

with Ada.Command_Line;
with Ada.Exceptions;
with System;

with Gdk;
with Gdk.Device;
with Gdk.Device_Tool;
with Gdk.Display;
with Gdk.Seat;
with Glib;
with Glib.Object;
with Glib.Test;
with Gtk.Main;
with Gtkada.Bindings;
with Gtkada.Types;

procedure Main is
   use Glib;
   use Glib.Test;
   use type Gdk.Gdk_Device;
   use type Gdk.Gdk_Display;
   use type Gdk.Device_Tool.Gdk_Device_Tool;
   use type Gdk.Seat.Seat_Capabilities;
   use type System.Address;

   Display : Gdk.Gdk_Display;
   Seat    : Gdk.Gdk_Seat;
   Pointer_Device  : Gdk.Gdk_Device;
   Keyboard_Device : Gdk.Gdk_Device;
   Signal_Tool     : Gdk.Device_Tool.Gdk_Device_Tool;

   Device_Calls, Device_Slot_Calls : Natural := 0;
   Tool_Calls, Tool_Slot_Calls     : Natural := 0;
   Callback_Exception_Calls        : Natural := 0;
   Raise_In_Callbacks               : Boolean := False;

   procedure Test_Accessors with Convention => C;
   procedure Test_Signals with Convention => C;

   --  A generic GObject instance is enough as a valid payload for manually
   --  emitted GdkDeviceTool signals, following the gdk-device test's idiom.
   function New_C_Object (Typ : GType) return System.Address;
   pragma Import (C, New_C_Object, "ada_g_object_new");

   procedure Emit_Device
     (Instance : System.Address; Name : Gtkada.Types.Chars_Ptr;
      Device   : System.Address);
   pragma Import (C, Emit_Device, "g_signal_emit_by_name");

   procedure Emit_Tool
     (Instance : System.Address; Name : Gtkada.Types.Chars_Ptr;
      Tool     : System.Address);
   pragma Import (C, Emit_Tool, "g_signal_emit_by_name");

   procedure On_Device
     (Self   : access Gdk.Seat.Gdk_Seat_Record'Class;
      Device : not null access Gdk.Device.Gdk_Device_Record'Class);
   procedure On_Device_Slot
     (Self   : access Glib.Object.GObject_Record'Class;
      Device : not null access Gdk.Device.Gdk_Device_Record'Class);
   procedure On_Tool
     (Self : access Gdk.Seat.Gdk_Seat_Record'Class;
      Tool : not null access Gdk.Device_Tool.Gdk_Device_Tool_Record'Class);
   procedure On_Tool_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Tool : not null access Gdk.Device_Tool.Gdk_Device_Tool_Record'Class);
   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   --  Device callbacks are shared by added and removed signals so each
   --  overload is checked against both signal names.
   --  Identity checks ensure both generated marshallers and slot forwarding
   --  deliver the actual seat, payload and connected slot.
   procedure On_Device
     (Self   : access Gdk.Seat.Gdk_Seat_Record'Class;
      Device : not null access Gdk.Device.Gdk_Device_Record'Class) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) =
                   Glib.Object.Get_Object (Seat));
      Assert_True (Glib.Object.Get_Object (Device) =
                   Glib.Object.Get_Object (Pointer_Device));
      if Raise_In_Callbacks then
         raise Program_Error with "expected seat device callback exception";
      end if;
      Device_Calls := Device_Calls + 1;
   end On_Device;

   procedure On_Device_Slot
     (Self   : access Glib.Object.GObject_Record'Class;
      Device : not null access Gdk.Device.Gdk_Device_Record'Class) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) =
                   Glib.Object.Get_Object (Display));
      Assert_True (Glib.Object.Get_Object (Device) =
                   Glib.Object.Get_Object (Pointer_Device));
      if Raise_In_Callbacks then
         raise Program_Error with "expected seat device slot exception";
      end if;
      Device_Slot_Calls := Device_Slot_Calls + 1;
   end On_Device_Slot;

   --  Tool callbacks use a valid but synthetic tool because Xvfb has no
   --  tablet device that could supply a physical one.
   procedure On_Tool
     (Self : access Gdk.Seat.Gdk_Seat_Record'Class;
      Tool : not null access Gdk.Device_Tool.Gdk_Device_Tool_Record'Class) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) =
                   Glib.Object.Get_Object (Seat));
      Assert_True (Glib.Object.Get_Object (Tool) =
                   Glib.Object.Get_Object (Signal_Tool));
      if Raise_In_Callbacks then
         raise Program_Error with "expected seat tool callback exception";
      end if;
      Tool_Calls := Tool_Calls + 1;
   end On_Tool;

   procedure On_Tool_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      Tool : not null access Gdk.Device_Tool.Gdk_Device_Tool_Record'Class) is
   begin
      Assert_True (Glib.Object.Get_Object (Self) =
                   Glib.Object.Get_Object (Display));
      Assert_True (Glib.Object.Get_Object (Tool) =
                   Glib.Object.Get_Object (Signal_Tool));
      if Raise_In_Callbacks then
         raise Program_Error with "expected seat tool slot exception";
      end if;
      Tool_Slot_Calls := Tool_Slot_Calls + 1;
   end On_Tool_Slot;

   --  GtkAda catches callback exceptions before they can unwind through C.
   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Callback_Exception_Calls := Callback_Exception_Calls + 1;
   end Record_Callback_Exception;

   --  The Xvfb seat supplies a keyboard and pointer; its relationships and
   --  filtered device lists are stable even when the host has no hardware.
   procedure Test_Accessors is
      Devices  : Gdk.Device.Device_List.Glist;
      Tools    : Gdk.Device_Tool.Device_Tool_List.Glist;
      Seats    : Gdk.Seat.Seat_List.Glist;
      Converted_Seat : Gdk.Gdk_Seat;
      Capabilities   : constant Gdk.Seat.Seat_Capabilities :=
        Seat.Get_Capabilities;
   begin
      Assert_True (Seat.Get_Display = Display);
      Assert_True (Pointer_Device /= null);
      Assert_True (Keyboard_Device /= null);
      Assert_True ((Capabilities and Gdk.Seat.Pointer) = Gdk.Seat.Pointer);
      Assert_True ((Capabilities and Gdk.Seat.Keyboard) = Gdk.Seat.Keyboard);

      --  A seat's display accessor returns the same borrowed display used to
      --  obtain the default seat above.
      --  Both capability filters return the corresponding default devices.
      Devices := Seat.Get_Devices (Gdk.Seat.Pointer);
      Assert_True (Gdk.Device.Device_List.Length (Devices) > 0);
      Gdk.Device.Device_List.Free (Devices);
      Devices := Seat.Get_Devices (Gdk.Seat.Keyboard);
      Assert_True (Gdk.Device.Device_List.Length (Devices) > 0);
      Gdk.Device.Device_List.Free (Devices);

      --  The list owns only its GList nodes; the devices remain owned by GDK.
      --  Xvfb has no tablet tools, but the getter still returns a valid empty
      --  list; this covers the returned-list wrapper without hardware needs.
      Tools := Seat.Get_Tools;
      Assert_Cmpuint_Eq (Gdk.Device_Tool.Device_Tool_List.Length (Tools), 0);
      Gdk.Device_Tool.Device_Tool_List.Free (Tools);

      --  Appending also validates the seat-to-address conversion used by
      --  generic GObject lists, and Get_Data performs the reverse conversion.
      --  Round-trip through the generic seat list exercises both generated
      --  object/address conversion hooks and verifies borrowed object identity.
      Gdk.Seat.Seat_List.Append (Seats, Seat);
      Converted_Seat := Gdk.Seat.Seat_List.Get_Data (Seats);
      Assert_True (Glib.Object.Get_Object (Converted_Seat) =
                   Glib.Object.Get_Object (Seat));
      Gdk.Seat.Seat_List.Free (Seats);
   end Test_Accessors;

   --  Direct emission is deterministic under Xvfb, which cannot produce real
   --  hot-plug or tablet events. Both typed and slot callback overloads run.
   procedure Test_Signals is
      Added_Name   : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("device-added");
      Removed_Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("device-removed");
      Tool_Added_Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("tool-added");
      Tool_Removed_Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("tool-removed");
      Seat_Object : constant System.Address := Glib.Object.Get_Object (Seat);
      Device_Object : constant System.Address :=
        Glib.Object.Get_Object (Pointer_Device);
   begin
      Signal_Tool := Gdk.Device_Tool.Convert
        (New_C_Object (Gdk.Device_Tool.Get_Type));
      Assert_True (Signal_Tool /= null);

      --  The slot forms verify that the second callback argument remains the
      --  device or tool while the connected GObject is passed as Self.
      Seat.On_Device_Added (On_Device'Unrestricted_Access);
      Seat.On_Device_Added
        (On_Device_Slot'Unrestricted_Access, Display, After => True);
      Seat.On_Device_Removed (On_Device'Unrestricted_Access, After => True);
      Seat.On_Device_Removed (On_Device_Slot'Unrestricted_Access, Display);
      Seat.On_Tool_Added (On_Tool'Unrestricted_Access);
      Seat.On_Tool_Added (On_Tool_Slot'Unrestricted_Access, Display);
      Seat.On_Tool_Removed (On_Tool'Unrestricted_Access, After => True);
      Seat.On_Tool_Removed (On_Tool_Slot'Unrestricted_Access, Display,
                            After => True);

      Emit_Device (Seat_Object, Added_Name, Device_Object);
      Emit_Device (Seat_Object, Removed_Name, Device_Object);
      Emit_Tool (Seat_Object, Tool_Added_Name,
                 Glib.Object.Get_Object (Signal_Tool));
      Emit_Tool (Seat_Object, Tool_Removed_Name,
                 Glib.Object.Get_Object (Signal_Tool));

      Assert_Cmpuint_Eq (Guint (Device_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Device_Slot_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Tool_Calls), 2);
      Assert_Cmpuint_Eq (Guint (Tool_Slot_Calls), 2);

      --  One device and one tool emission reach both callbacks for that
      --  signal family, so four exceptions are expected in this second pass.
      --  Repeat emissions with raising callbacks to cover each marshaller's
      --  exception branch and check that GtkAda contains every exception.
      Gtkada.Bindings.Set_On_Exception
        (Record_Callback_Exception'Unrestricted_Access);
      Raise_In_Callbacks := True;
      Emit_Device (Seat_Object, Added_Name, Device_Object);
      Emit_Tool (Seat_Object, Tool_Added_Name,
                 Glib.Object.Get_Object (Signal_Tool));
      Raise_In_Callbacks := False;
      Assert_Cmpuint_Eq (Guint (Callback_Exception_Calls), 4);

      Gtkada.Types.Free (Added_Name);
      Gtkada.Types.Free (Removed_Name);
      Gtkada.Types.Free (Tool_Added_Name);
      Gtkada.Types.Free (Tool_Removed_Name);
      Glib.Object.Unref (Glib.Object.GObject (Signal_Tool));
   end Test_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Display := Gdk.Display.Get_Default;
   Seat := Display.Get_Default_Seat;
   Pointer_Device := Seat.Get_Pointer;
   Keyboard_Device := Seat.Get_Keyboard;

   Add_Func ("/gdk-seat/accessors", Test_Accessors'Unrestricted_Access);
   Add_Func ("/gdk-seat/signals", Test_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
