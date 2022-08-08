------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  <description>
--  The Gdk.Device.Gdk_Device object represents a single input device, such as
--  a keyboard, a mouse, a touchpad, etc.
--
--  See the Gdk.Device_Manager.Gdk_Device_Manager documentation for more
--  information about the various kinds of master and slave devices, and their
--  relationships.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Display;             use Gdk.Display;
with Gdk.Event;               use Gdk.Event;
with Gdk.Screen;              use Gdk.Screen;
with Gdk.Types;               use Gdk.Types;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Glist;              use Glib.Glist;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;

package Gdk.Device is

   type Gdk_Device_Record is new GObject_Record with null record;
   type Gdk_Device is access all Gdk_Device_Record'Class;

   type Gdk_Device_Type is (
      Gdk_Device_Type_Master,
      Gdk_Device_Type_Slave,
      Gdk_Device_Type_Floating);
   pragma Convention (C, Gdk_Device_Type);
   --  Indicates the device type. See [above][GdkDeviceManager.description]
   --  for more information about the meaning of these device types.

   type Gdk_Axis_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_Axis_Flags);
   --  Flags describing the current capabilities of a device/tool.

   Gdk_Axis_Flag_X : constant Gdk_Axis_Flags := 2;
   Gdk_Axis_Flag_Y : constant Gdk_Axis_Flags := 4;
   Gdk_Axis_Flag_Pressure : constant Gdk_Axis_Flags := 8;
   Gdk_Axis_Flag_Xtilt : constant Gdk_Axis_Flags := 16;
   Gdk_Axis_Flag_Ytilt : constant Gdk_Axis_Flags := 32;
   Gdk_Axis_Flag_Wheel : constant Gdk_Axis_Flags := 64;
   Gdk_Axis_Flag_Distance : constant Gdk_Axis_Flags := 128;
   Gdk_Axis_Flag_Rotation : constant Gdk_Axis_Flags := 256;
   Gdk_Axis_Flag_Slider : constant Gdk_Axis_Flags := 512;

   function Convert (R : Gdk.Device.Gdk_Device) return System.Address;
   function Convert (R : System.Address) return Gdk.Device.Gdk_Device;
   package Device_List is new Generic_List (Gdk.Device.Gdk_Device);

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Device_Type_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Device_Type);
   type Property_Gdk_Device_Type is new Gdk_Device_Type_Properties.Property;

   package Gdk_Axis_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Axis_Flags);
   type Property_Gdk_Axis_Flags is new Gdk_Axis_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_device_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Associated_Device
      (Self : not null access Gdk_Device_Record) return Gdk_Device;
   --  Returns the associated device to Device, if Device is of type
   --  Gdk.Device.Gdk_Device_Type_Master, it will return the paired pointer or
   --  keyboard.
   --  If Device is of type Gdk.Device.Gdk_Device_Type_Slave, it will return
   --  the master device to which Device is attached to.
   --  If Device is of type Gdk.Device.Gdk_Device_Type_Floating, null will be
   --  returned, as there is no associated device.
   --  Since: gtk+ 3.0

   function Get_Axes
      (Self : not null access Gdk_Device_Record) return Gdk_Axis_Flags;
   --  Returns the axes currently available on the device.
   --  Since: gtk+ 3.22

   function Get_Axis_Use
      (Self  : not null access Gdk_Device_Record;
       Index : Guint) return Gdk_Axis_Use;
   --  Returns the axis use for Index_.
   --  Since: gtk+ 2.20
   --  "index_": the index of the axis.

   procedure Set_Axis_Use
      (Self  : not null access Gdk_Device_Record;
       Index : Guint;
       GUse  : Gdk_Axis_Use);
   --  Specifies how an axis of a device is used.
   --  "index_": the index of the axis
   --  "use": specifies how the axis is used

   function Get_Device_Type
      (Self : not null access Gdk_Device_Record) return Gdk_Device_Type;
   --  Returns the device type for Device.
   --  Since: gtk+ 3.0

   function Get_Display
      (Self : not null access Gdk_Device_Record)
       return Gdk.Display.Gdk_Display;
   --  Returns the Gdk.Display.Gdk_Display to which Device pertains.
   --  Since: gtk+ 3.0

   function Get_Has_Cursor
      (Self : not null access Gdk_Device_Record) return Boolean;
   --  Determines whether the pointer follows device motion. This is not
   --  meaningful for keyboard devices, which don't have a pointer.
   --  Since: gtk+ 2.20

   function Get_Last_Event_Window
      (Self : not null access Gdk_Device_Record) return Gdk.Gdk_Window;
   --  Gets information about which window the given pointer device is in,
   --  based on events that have been received so far from the display server.
   --  If another application has a pointer grab, or this application has a
   --  grab with owner_events = False, null may be returned even if the pointer
   --  is physically over one of this application's windows.
   --  Since: gtk+ 3.12

   function Get_Mode
      (Self : not null access Gdk_Device_Record) return Gdk_Input_Mode;
   --  Determines the mode of the device.
   --  Since: gtk+ 2.20

   function Set_Mode
      (Self : not null access Gdk_Device_Record;
       Mode : Gdk_Input_Mode) return Boolean;
   --  Sets a the mode of an input device. The mode controls if the device is
   --  active and whether the device's range is mapped to the entire screen or
   --  to a single window.
   --  Note: This is only meaningful for floating devices, master devices (and
   --  slaves connected to these) drive the pointer cursor, which is not
   --  limited by the input mode.
   --  "mode": the input mode.

   function Get_N_Axes
      (Self : not null access Gdk_Device_Record) return Glib.Gint;
   --  Returns the number of axes the device currently has.
   --  Since: gtk+ 3.0

   function Get_N_Keys
      (Self : not null access Gdk_Device_Record) return Glib.Gint;
   --  Returns the number of keys the device currently has.
   --  Since: gtk+ 2.24

   function Get_Name
      (Self : not null access Gdk_Device_Record) return UTF8_String;
   --  Determines the name of the device.
   --  Since: gtk+ 2.20

   procedure Get_Position_Double
      (Self   : not null access Gdk_Device_Record;
       Screen : out Gdk.Screen.Gdk_Screen;
       X      : out Gdouble;
       Y      : out Gdouble);
   --  Gets the current location of Device in double precision. As a slave
   --  device's coordinates are those of its master pointer, this function may
   --  not be called on devices of type Gdk.Device.Gdk_Device_Type_Slave,
   --  unless there is an ongoing grab on them. See Gdk.Device.Grab.
   --  Since: gtk+ 3.10
   --  "screen": location to store the Gdk.Screen.Gdk_Screen the Device is on,
   --  or null.
   --  "x": location to store root window X coordinate of Device, or null.
   --  "y": location to store root window Y coordinate of Device, or null.

   function Get_Product_Id
      (Self : not null access Gdk_Device_Record) return UTF8_String;
   --  Returns the product ID of this device, or null if this information
   --  couldn't be obtained. This ID is retrieved from the device, and is thus
   --  constant for it. See Gdk.Device.Get_Vendor_Id for more information.
   --  Since: gtk+ 3.16

   function Get_Seat
      (Self : not null access Gdk_Device_Record) return Glib.Object.GObject;
   --  Returns the Gdk.Seat.Gdk_Seat the device belongs to.
   --  Since: gtk+ 3.20

   function Get_Source
      (Self : not null access Gdk_Device_Record) return Gdk_Input_Source;
   --  Determines the type of the device.
   --  Since: gtk+ 2.20

   procedure Get_State
      (Self   : not null access Gdk_Device_Record;
       Window : Gdk.Gdk_Window;
       Axes   : Gdouble_Array;
       Mask   : out Gdk.Types.Gdk_Modifier_Type);
   --  Gets the current state of a pointer device relative to Window. As a
   --  slave device's coordinates are those of its master pointer, this
   --  function may not be called on devices of type
   --  Gdk.Device.Gdk_Device_Type_Slave, unless there is an ongoing grab on
   --  them. See Gdk.Device.Grab.
   --  "window": a Gdk.Gdk_Window.
   --  "axes": an array of doubles to store the values of the axes of Device
   --  in, or null.
   --  "mask": location to store the modifiers, or null.

   function Get_Vendor_Id
      (Self : not null access Gdk_Device_Record) return UTF8_String;
   --  Returns the vendor ID of this device, or null if this information
   --  couldn't be obtained. This ID is retrieved from the device, and is thus
   --  constant for it.
   --  This function, together with Gdk.Device.Get_Product_Id, can be used to
   --  eg. compose Gsettings.Gsettings paths to store settings for this device.
   --  |[<!-- language="C" --> static GSettings * get_device_settings
   --  (GdkDevice *device) { const gchar *vendor, *product; GSettings
   --  *settings; GdkDevice *device; gchar *path;
   --  vendor = gdk_device_get_vendor_id (device); product =
   --  gdk_device_get_product_id (device);
   --  path = g_strdup_printf ("/org/example/app/devices/%s:%s/", vendor,
   --  product); settings = g_settings_new_with_path (DEVICE_SCHEMA, path);
   --  g_free (path);
   --  return settings; } ]|
   --  Since: gtk+ 3.16

   function Get_Window_At_Position_Double
      (Self  : not null access Gdk_Device_Record;
       Win_X : access Gdouble;
       Win_Y : access Gdouble) return Gdk.Gdk_Window;
   --  Obtains the window underneath Device, returning the location of the
   --  device in Win_X and Win_Y in double precision. Returns null if the
   --  window tree under Device is not known to GDK (for example, belongs to
   --  another application).
   --  As a slave device coordinates are those of its master pointer, This
   --  function may not be called on devices of type
   --  Gdk.Device.Gdk_Device_Type_Slave, unless there is an ongoing grab on
   --  them, see Gdk.Device.Grab.
   --  Since: gtk+ 3.0
   --  "win_x": return location for the X coordinate of the device location,
   --  relative to the window origin, or null.
   --  "win_y": return location for the Y coordinate of the device location,
   --  relative to the window origin, or null.

   function Grab
      (Self           : not null access Gdk_Device_Record;
       Window         : Gdk.Gdk_Window;
       Grab_Ownership : Gdk_Grab_Ownership;
       Owner_Events   : Boolean;
       Event_Mask     : Gdk.Event.Gdk_Event_Mask;
       Cursor         : Gdk.Gdk_Cursor;
       Time           : Guint32) return Gdk_Grab_Status;
   pragma Obsolescent (Grab);
   --  Grabs the device so that all events coming from this device are passed
   --  to this application until the device is ungrabbed with
   --  Gdk.Device.Ungrab, or the window becomes unviewable. This overrides any
   --  previous grab on the device by this client.
   --  Note that Device and Window need to be on the same display.
   --  Device grabs are used for operations which need complete control over
   --  the given device events (either pointer or keyboard). For example in
   --  GTK+ this is used for Drag and Drop operations, popup menus and such.
   --  Note that if the event mask of an X window has selected both button
   --  press and button release events, then a button press event will cause an
   --  automatic pointer grab until the button is released. X does this
   --  automatically since most applications expect to receive button press and
   --  release events in pairs. It is equivalent to a pointer grab on the
   --  window with Owner_Events set to True.
   --  If you set up anything at the time you take the grab that needs to be
   --  cleaned up when the grab ends, you should handle the
   --  Gdk.Event.Gdk_Event_Grab_Broken events that are emitted when the grab
   --  ends unvoluntarily.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.20., 1
   --  "window": the Gdk.Gdk_Window which will own the grab (the grab window)
   --  "grab_ownership": specifies the grab ownership.
   --  "owner_events": if False then all device events are reported with
   --  respect to Window and are only reported if selected by Event_Mask. If
   --  True then pointer events for this application are reported as normal,
   --  but pointer events outside this application are reported with respect to
   --  Window and only if selected by Event_Mask. In either mode, unreported
   --  events are discarded.
   --  "event_mask": specifies the event mask, which is used in accordance
   --  with Owner_Events.
   --  "cursor": the cursor to display while the grab is active if the device
   --  is a pointer. If this is null then the normal cursors are used for
   --  Window and its descendants, and the cursor for Window is used elsewhere.
   --  "time_": the timestamp of the event which led to this pointer grab.
   --  This usually comes from the Gdk.Event.Gdk_Event struct, though
   --  GDK_CURRENT_TIME can be used if the time isn't known.

   procedure Set_Key
      (Self      : not null access Gdk_Device_Record;
       Index     : Guint;
       Keyval    : Guint;
       Modifiers : Gdk.Types.Gdk_Modifier_Type);
   --  Specifies the X key event to generate when a macro button of a device
   --  is pressed.
   --  "index_": the index of the macro button to set
   --  "keyval": the keyval to generate
   --  "modifiers": the modifiers to set

   procedure Ungrab
      (Self : not null access Gdk_Device_Record;
       Time : Guint32);
   pragma Obsolescent (Ungrab);
   --  Release any grab on Device.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.20., 1
   --  "time_": a timestap (e.g. GDK_CURRENT_TIME).

   procedure Warp
      (Self   : not null access Gdk_Device_Record;
       Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class;
       X      : Glib.Gint;
       Y      : Glib.Gint);
   --  Warps Device in Display to the point X,Y on the screen Screen, unless
   --  the device is confined to a window by a grab, in which case it will be
   --  moved as far as allowed by the grab. Warping the pointer creates events
   --  as if the user had moved the mouse instantaneously to the destination.
   --  Note that the pointer should normally be under the control of the user.
   --  This function was added to cover some rare use cases like keyboard
   --  navigation support for the color picker in the
   --  Gtk.Color_Selection_Dialog.Gtk_Color_Selection_Dialog.
   --  Since: gtk+ 3.0
   --  "screen": the screen to warp Device to.
   --  "x": the X coordinate of the destination.
   --  "y": the Y coordinate of the destination.

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Get_Window_At_Position
     (Self   : not null access Gdk_Device_Record;
      Win_X  : out Gint;
      Win_Y  : out Gint;
      Window : out Gdk.Gdk_Window);
   --  Obtains the window underneath Device, returning the location of the
   --  device in Win_X and Win_Y. Returns null if the window tree under Device
   --  is not known to GDK (for example, belongs to another application).
   --  As a slave device coordinates are those of its master pointer, This
   --  function may not be called on devices of type
   --  Gdk.Device.Gdk_Device_Type_Slave, unless there is an ongoing grab on
   --  them, see Gdk.Device.Grab.
   --  Since: gtk+ 3.0
   --  "win_x": return location for the X coordinate of the device location,
   --  relative to the window origin, or null.
   --  "win_y": return location for the Y coordinate of the device location,
   --  relative to the window origin, or null.

   procedure Set_Device
     (Event   : Gdk.Event.Gdk_Event;
      Device  : not null access Gdk_Device_Record);
   --  Sets the device for Event to Device. The event must
   --  have been allocated by GTK+, for instance, by gdk_event_copy().

   procedure Set_Source_Device
     (Event   : Gdk.Event.Gdk_Event;
      Device  : not null access Gdk_Device_Record);
   --  Sets the slave device for Event to Device.
   --  The event must have been allocated by GTK+,
   --  for instance by gdk_event_copy().

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Associated_Device_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Device
   --  Associated pointer or keyboard with this device, if any. Devices of
   --  type GDK_DEVICE_TYPE_MASTER always come in keyboard/pointer pairs. Other
   --  device types will have a null associated device.

   Axes_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Axis_Flags
   --  The axes currently available for this device.

   Device_Manager_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Device_Manager
   --  The Gdk.Device_Manager.Gdk_Device_Manager the Gdk.Device.Gdk_Device
   --  pertains to.

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display
   --  The Gdk.Display.Gdk_Display the Gdk.Device.Gdk_Device pertains to.

   Has_Cursor_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the device is represented by a cursor on the screen. Devices of
   --  type Gdk.Device.Gdk_Device_Type_Master will have True here.

   Input_Mode_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Input_Mode

   Input_Source_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Input_Source
   --  Source type for the device.

   N_Axes_Property : constant Glib.Properties.Property_Uint;
   --  Number of axes in the device.

   Name_Property : constant Glib.Properties.Property_String;
   --  The device name.

   Num_Touches_Property : constant Glib.Properties.Property_Uint;
   --  The maximal number of concurrent touches on a touch device. Will be 0
   --  if the device is not a touch device or if the number of touches is
   --  unknown.

   Product_Id_Property : constant Glib.Properties.Property_String;
   --  Product ID of this device, see Gdk.Device.Get_Product_Id.

   Seat_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Seat
   --  Gdk.Seat.Gdk_Seat of this device.

   The_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Device_Type
   --  Device role in the device manager.

   Tool_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Device_Tool

   Vendor_Id_Property : constant Glib.Properties.Property_String;
   --  Vendor ID of this device, see Gdk.Device.Get_Vendor_Id.

   -------------
   -- Signals --
   -------------

   Signal_Changed : constant Glib.Signal_Name := "changed";
   --  The ::changed signal is emitted either when the Gdk.Device.Gdk_Device
   --  has changed the number of either axes or keys. For example In X this
   --  will normally happen when the slave device routing events through the
   --  master device changes (for example, user switches from the USB mouse to
   --  a tablet), in that case the master device will change to reflect the new
   --  slave device axes and keys.
   --    procedure Handler (Self : access Gdk_Device_Record'Class)

   type Cb_Gdk_Device_GObject_Void is not null access procedure
     (Self : access Gdk_Device_Record'Class;
      Tool : not null access Glib.Object.GObject_Record'Class);

   type Cb_GObject_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Tool : not null access Glib.Object.GObject_Record'Class);

   Signal_Tool_Changed : constant Glib.Signal_Name := "tool-changed";
   procedure On_Tool_Changed
      (Self  : not null access Gdk_Device_Record;
       Call  : Cb_Gdk_Device_GObject_Void;
       After : Boolean := False);
   procedure On_Tool_Changed
      (Self  : not null access Gdk_Device_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::tool-changed signal is emitted on pen/eraser Gdk_Devices whenever
   --  tools enter or leave proximity.

private
   Vendor_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("vendor-id");
   Tool_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("tool");
   The_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("type");
   Seat_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("seat");
   Product_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("product-id");
   Num_Touches_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("num-touches");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   N_Axes_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-axes");
   Input_Source_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("input-source");
   Input_Mode_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("input-mode");
   Has_Cursor_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-cursor");
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
   Device_Manager_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("device-manager");
   Axes_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("axes");
   Associated_Device_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("associated-device");
end Gdk.Device;
