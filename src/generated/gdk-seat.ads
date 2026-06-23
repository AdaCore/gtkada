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

--  Represents a collection of input devices that belong to a user.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Device;              use Gdk.Device;
with Gdk.Device_Tool;         use Gdk.Device_Tool;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;

package Gdk.Seat is

   type Gdk_Seat_Record is new GObject_Record with null record;
   subtype Gdk_Seat is Gdk.Gdk_Seat;

   type Seat_Capabilities is mod 2 ** Integer'Size;
   pragma Convention (C, Seat_Capabilities);
   --  Flags describing the seat capabilities.

   None : constant Seat_Capabilities := 0;
   Pointer : constant Seat_Capabilities := 1;
   Touch : constant Seat_Capabilities := 2;
   Tablet_Stylus : constant Seat_Capabilities := 4;
   Keyboard : constant Seat_Capabilities := 8;
   Tablet_Pad : constant Seat_Capabilities := 16;
   All_Pointing : constant Seat_Capabilities := 7;
   All_Capabilities : constant Seat_Capabilities := 31;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Seat_Capabilities_Properties is
      new Generic_Internal_Discrete_Property (Seat_Capabilities);
   type Property_Seat_Capabilities is new Seat_Capabilities_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_seat_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Capabilities
      (Self : not null access Gdk_Seat_Record) return Seat_Capabilities;
   --  Returns the capabilities this `GdkSeat` currently has.
   --  @return the seat capabilities

   function Get_Devices
      (Self         : not null access Gdk_Seat_Record;
       Capabilities : Seat_Capabilities) return Gdk.Device.Device_List.Glist;
   --  Returns the devices that match the given capabilities.
   --  @param Capabilities capabilities to get devices for

   function Get_Display
      (Self : not null access Gdk_Seat_Record) return Gdk.Gdk_Display;
   --  Returns the `GdkDisplay` this seat belongs to.
   --  @return a `GdkDisplay`. This object is owned by GTK and must not be
   --  freed.

   function Get_Keyboard
      (Self : not null access Gdk_Seat_Record) return Gdk.Gdk_Device;
   --  Returns the device that routes keyboard events.
   --  @return a `GdkDevice` with keyboard capabilities. This object is owned
   --  by GTK and must not be freed.

   function Get_Pointer
      (Self : not null access Gdk_Seat_Record) return Gdk.Gdk_Device;
   --  Returns the device that routes pointer events.
   --  @return a `GdkDevice` with pointer capabilities. This object is owned
   --  by GTK and must not be freed.

   function Get_Tools
      (Self : not null access Gdk_Seat_Record)
       return Gdk.Device_Tool.Device_Tool_List.Glist;
   --  Returns all `GdkDeviceTools` that are known to the application.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display
   --  `GdkDisplay` of this seat.

   -------------
   -- Signals --
   -------------

   type Cb_Gdk_Seat_Gdk_Device_Void is not null access procedure
     (Self   : access Gdk_Seat_Record'Class;
      Device : not null access Gdk.Device.Gdk_Device_Record'Class);

   type Cb_GObject_Gdk_Device_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Device : not null access Gdk.Device.Gdk_Device_Record'Class);

   Signal_Device_Added : constant Glib.Signal_Name := "device-added";
   procedure On_Device_Added
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_Gdk_Seat_Gdk_Device_Void;
       After : Boolean := False);
   procedure On_Device_Added
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_GObject_Gdk_Device_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a new input device is related to this seat.

   Signal_Device_Removed : constant Glib.Signal_Name := "device-removed";
   procedure On_Device_Removed
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_Gdk_Seat_Gdk_Device_Void;
       After : Boolean := False);
   procedure On_Device_Removed
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_GObject_Gdk_Device_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when an input device is removed (e.g. unplugged).

   type Cb_Gdk_Seat_Gdk_Device_Tool_Void is not null access procedure
     (Self : access Gdk_Seat_Record'Class;
      Tool : not null access Gdk.Device_Tool.Gdk_Device_Tool_Record'Class);

   type Cb_GObject_Gdk_Device_Tool_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Tool : not null access Gdk.Device_Tool.Gdk_Device_Tool_Record'Class);

   Signal_Tool_Added : constant Glib.Signal_Name := "tool-added";
   procedure On_Tool_Added
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_Gdk_Seat_Gdk_Device_Tool_Void;
       After : Boolean := False);
   procedure On_Tool_Added
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_GObject_Gdk_Device_Tool_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever a new tool is made known to the seat.
   --
   --  The tool may later be assigned to a device (i.e. on proximity with a
   --  tablet). The device will emit the [signalGdk.Device::tool-changed]
   --  signal accordingly.
   --
   --  A same tool may be used by several devices.

   Signal_Tool_Removed : constant Glib.Signal_Name := "tool-removed";
   procedure On_Tool_Removed
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_Gdk_Seat_Gdk_Device_Tool_Void;
       After : Boolean := False);
   procedure On_Tool_Removed
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_GObject_Gdk_Device_Tool_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever a tool is no longer known to this Seat.

private
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
end Gdk.Seat;
