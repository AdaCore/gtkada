------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Represents an input device, such as a keyboard, mouse or touchpad.
--
--  See the [classGdk.Seat] documentation for more information about the
--  various kinds of devices, and their relationships.

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Gdk.Device_Tool;         use Gdk.Device_Tool;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Pango.Enums;             use Pango.Enums;

package Gdk.Device is

   type Gdk_Device_Record is new GObject_Record with null record;
   subtype Gdk_Device is Gdk.Gdk_Device;

   type Gdk_Input_Source is (
      Gdk_Source_Mouse,
      Gdk_Source_Pen,
      Gdk_Source_Keyboard,
      Gdk_Source_Touchscreen,
      Gdk_Source_Touchpad,
      Gdk_Source_Trackpoint,
      Gdk_Source_Tablet_Pad);
   pragma Convention (C, Gdk_Input_Source);
   --  An enumeration describing the type of an input device in general terms.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Input_Source_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Input_Source);
   type Property_Gdk_Input_Source is new Gdk_Input_Source_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_device_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Active_Layout_Index
      (Self : not null access Gdk_Device_Record) return Glib.Gint;
   --  Retrieves the index of the active layout of the keyboard.
   --  If there is no valid active layout for the `GdkDevice`, this function
   --  will return -1;
   --  This is only relevant for keyboard devices.
   --  Since: gtk+ 4.18
   --  @return The layout index of the active layout or -1.

   function Get_Caps_Lock_State
      (Self : not null access Gdk_Device_Record) return Boolean;
   --  Retrieves whether the Caps Lock modifier of the keyboard is locked.
   --  This is only relevant for keyboard devices.
   --  @return True if Caps Lock is on for Device

   function Get_Device_Tool
      (Self : not null access Gdk_Device_Record)
       return Gdk.Device_Tool.Gdk_Device_Tool;
   --  Retrieves the current tool for Device.
   --  @return the `GdkDeviceTool`

   function Get_Direction
      (Self : not null access Gdk_Device_Record)
       return Pango.Enums.Direction;
   --  Returns the direction of effective layout of the keyboard.
   --  This is only relevant for keyboard devices.
   --  The direction of a layout is the direction of the majority of its
   --  symbols. See [funcPango.unichar_direction].
   --  @return Pango.Enums.Pango_Direction_Ltr or
   --  Pango.Enums.Pango_Direction_Rtl if it can determine the direction.
   --  Pango.Enums.Pango_Direction_Neutral otherwise

   function Get_Display
      (Self : not null access Gdk_Device_Record) return Gdk.Gdk_Display;
   --  Returns the `GdkDisplay` to which Device pertains.
   --  @return a `GdkDisplay`

   function Get_Has_Cursor
      (Self : not null access Gdk_Device_Record) return Boolean;
   --  Determines whether the pointer follows device motion.
   --  This is not meaningful for keyboard devices, which don't have a
   --  pointer.
   --  @return True if the pointer follows device motion

   function Get_Layout_Names
      (Self : not null access Gdk_Device_Record)
       return GNAT.Strings.String_List;
   --  Retrieves the names of the layouts of the keyboard.
   --  This is only relevant for keyboard devices.
   --  Since: gtk+ 4.18
   --  @return null-terminated array of strings of layouts,

   function Get_Name
      (Self : not null access Gdk_Device_Record) return UTF8_String;
   --  The name of the device, suitable for showing in a user interface.
   --  @return a name

   function Get_Num_Lock_State
      (Self : not null access Gdk_Device_Record) return Boolean;
   --  Retrieves whether the Num Lock modifier of the keyboard is locked.
   --  This is only relevant for keyboard devices.
   --  @return True if Num Lock is on for Device

   function Get_Num_Touches
      (Self : not null access Gdk_Device_Record) return Guint;
   --  Retrieves the number of touch points associated to Device.
   --  @return the number of touch points

   function Get_Product_Id
      (Self : not null access Gdk_Device_Record) return UTF8_String;
   --  Returns the product ID of this device.
   --  This ID is retrieved from the device, and does not change. See
   --  [methodGdk.Device.get_vendor_id] for more information.
   --  @return the product ID

   function Get_Scroll_Lock_State
      (Self : not null access Gdk_Device_Record) return Boolean;
   --  Retrieves whether the Scroll Lock modifier of the keyboard is locked.
   --  This is only relevant for keyboard devices.
   --  @return True if Scroll Lock is on for Device

   function Get_Source
      (Self : not null access Gdk_Device_Record) return Gdk_Input_Source;
   --  Determines the type of the device.
   --  @return a `GdkInputSource`

   function Get_Surface_At_Position
      (Self  : not null access Gdk_Device_Record;
       Win_X : access Gdouble;
       Win_Y : access Gdouble) return Gdk.Gdk_Surface;
   --  Obtains the surface underneath Device, returning the location of the
   --  device in Win_X and Win_Y.
   --  Returns null if the surface tree under Device is not known to GDK (for
   --  example, belongs to another application).
   --  @param Win_X return location for the X coordinate of the device
   --  location relative to the surface origin
   --  @param Win_Y return location for the Y coordinate of the device
   --  location relative to the surface origin
   --  @return the `GdkSurface` under the device position

   function Get_Timestamp
      (Self : not null access Gdk_Device_Record) return Guint32;
   --  Returns the timestamp of the last activity for this device.
   --  In practice, this means the timestamp of the last event that was
   --  received from the OS for this device. (GTK may occasionally produce
   --  events for a device that are not received from the OS, and will not
   --  update the timestamp).
   --  Since: gtk+ 4.2
   --  @return the timestamp of the last activity for this device

   function Get_Vendor_Id
      (Self : not null access Gdk_Device_Record) return UTF8_String;
   --  Returns the vendor ID of this device.
   --  This ID is retrieved from the device, and does not change.
   --  This function, together with [methodGdk.Device.get_product_id], can be
   --  used to eg. compose `GSettings` paths to store settings for this device.
   --  ```c static GSettings * get_device_settings (GdkDevice *device) { const
   --  char *vendor, *product; GSettings *settings; GdkDevice *device; char
   --  *path;
   --  vendor = gdk_device_get_vendor_id (device); product =
   --  gdk_device_get_product_id (device);
   --  path = g_strdup_printf ("/org/example/app/devices/%s:%s/", vendor,
   --  product); settings = g_settings_new_with_path (DEVICE_SCHEMA, path);
   --  g_free (path);
   --  return settings; } ```
   --  @return the vendor ID

   function Has_Bidi_Layouts
      (Self : not null access Gdk_Device_Record) return Boolean;
   --  Determines if layouts for both right-to-left and left-to-right
   --  languages are in use on the keyboard.
   --  This is only relevant for keyboard devices.
   --  @return True if there are layouts with both directions, False otherwise

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Layout_Names_Property : constant Glib.Properties.Property_String :=
   Glib.Properties.Build ("layout-names");--  Unknown type: unspecified

   Active_Layout_Index_Property : constant Glib.Properties.Property_Int;
   --  The index of the keyboard active layout of a `GdkDevice`.
   --
   --  Will be -1 if there is no valid active layout.
   --
   --  This is only relevant for keyboard devices.

   Caps_Lock_State_Property : constant Glib.Properties.Property_Boolean;
   --  Whether Caps Lock is on.
   --
   --  This is only relevant for keyboard devices.

   Direction_Property : constant Pango.Enums.Property_Direction;
   --  Type: Pango.Enums.Direction
   --  The direction of the current layout.
   --
   --  This is only relevant for keyboard devices.

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display
   --  The `GdkDisplay` the `GdkDevice` pertains to.

   Has_Bidi_Layouts_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the device has both right-to-left and left-to-right layouts.
   --
   --  This is only relevant for keyboard devices.

   Has_Cursor_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the device is represented by a cursor on the screen.

   Modifier_State_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gdk.Types.Gdk_Modifier_Type
   --  The current modifier state of the device.
   --
   --  This is only relevant for keyboard devices.

   N_Axes_Property : constant Glib.Properties.Property_Uint;
   --  Number of axes in the device.

   Name_Property : constant Glib.Properties.Property_String;
   --  The device name.

   Num_Lock_State_Property : constant Glib.Properties.Property_Boolean;
   --  Whether Num Lock is on.
   --
   --  This is only relevant for keyboard devices.

   Num_Touches_Property : constant Glib.Properties.Property_Uint;
   --  The maximal number of concurrent touches on a touch device.
   --
   --  Will be 0 if the device is not a touch device or if the number of
   --  touches is unknown.

   Product_Id_Property : constant Glib.Properties.Property_String;
   --  Product ID of this device.
   --
   --  See [methodGdk.Device.get_product_id].

   Scroll_Lock_State_Property : constant Glib.Properties.Property_Boolean;
   --  Whether Scroll Lock is on.
   --
   --  This is only relevant for keyboard devices.

   Seat_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Seat
   --  `GdkSeat` of this device.

   Source_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Input_Source
   --  Source type for the device.

   Tool_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Device_Tool
   --  The `GdkDeviceTool` that is currently used with this device.

   Vendor_Id_Property : constant Glib.Properties.Property_String;
   --  Vendor ID of this device.
   --
   --  See [methodGdk.Device.get_vendor_id].

   -------------
   -- Signals --
   -------------

   type Cb_Gdk_Device_Void is not null access procedure (Self : access Gdk_Device_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gdk_Device_Record;
       Call  : Cb_Gdk_Device_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gdk_Device_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted either when the number of either axes or keys changes.
   --
   --  On X11 this will normally happen when the physical device routing
   --  events through the logical device changes (for example, user switches
   --  from the USB mouse to a tablet); in that case the logical device will
   --  change to reflect the axes and keys on the new physical device.

   type Cb_Gdk_Device_Gdk_Device_Tool_Void is not null access procedure
     (Self : access Gdk_Device_Record'Class;
      Tool : not null access Gdk.Device_Tool.Gdk_Device_Tool_Record'Class);

   type Cb_GObject_Gdk_Device_Tool_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Tool : not null access Gdk.Device_Tool.Gdk_Device_Tool_Record'Class);

   Signal_Tool_Changed : constant Glib.Signal_Name := "tool-changed";
   procedure On_Tool_Changed
      (Self  : not null access Gdk_Device_Record;
       Call  : Cb_Gdk_Device_Gdk_Device_Tool_Void;
       After : Boolean := False);
   procedure On_Tool_Changed
      (Self  : not null access Gdk_Device_Record;
       Call  : Cb_GObject_Gdk_Device_Tool_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted on pen/eraser devices whenever tools enter or leave proximity.

private
   Vendor_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("vendor-id");
   Tool_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("tool");
   Source_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("source");
   Seat_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("seat");
   Scroll_Lock_State_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("scroll-lock-state");
   Product_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("product-id");
   Num_Touches_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("num-touches");
   Num_Lock_State_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("num-lock-state");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   N_Axes_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-axes");
   Modifier_State_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("modifier-state");
   Has_Cursor_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-cursor");
   Has_Bidi_Layouts_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-bidi-layouts");
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
   Direction_Property : constant Pango.Enums.Property_Direction :=
     Pango.Enums.Build ("direction");
   Caps_Lock_State_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("caps-lock-state");
   Active_Layout_Index_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("active-layout-index");
end Gdk.Device;
