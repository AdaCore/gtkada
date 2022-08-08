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
--  The Gdk.Seat.Gdk_Seat object represents a collection of input devices that
--  belong to a user.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Device;              use Gdk.Device;
with Gdk.Event;               use Gdk.Event;
with Gdk.Types;               use Gdk.Types;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Glist;              use Glib.Glist;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;

package Gdk.Seat is

   type Gdk_Seat_Record is new GObject_Record with null record;
   type Gdk_Seat is access all Gdk_Seat_Record'Class;

   type Gdk_Seat_Capabilities is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_Seat_Capabilities);
   --  Flags describing the seat capabilities.

   Gdk_Seat_Capability_None : constant Gdk_Seat_Capabilities := 0;
   Gdk_Seat_Capability_Pointer : constant Gdk_Seat_Capabilities := 1;
   Gdk_Seat_Capability_Touch : constant Gdk_Seat_Capabilities := 2;
   Gdk_Seat_Capability_Tablet_Stylus : constant Gdk_Seat_Capabilities := 4;
   Gdk_Seat_Capability_Keyboard : constant Gdk_Seat_Capabilities := 8;
   Gdk_Seat_Capability_All_Pointing : constant Gdk_Seat_Capabilities := 7;
   Gdk_Seat_Capability_All : constant Gdk_Seat_Capabilities := 15;

   function Convert (R : Gdk.Seat.Gdk_Seat) return System.Address;
   function Convert (R : System.Address) return Gdk.Seat.Gdk_Seat;
   package Gdk_Seat_List is new Generic_List (Gdk.Seat.Gdk_Seat);

   ---------------
   -- Callbacks --
   ---------------

   type Gdk_Seat_Grab_Prepare_Func is access procedure
     (Seat   : not null access Gdk_Seat_Record'Class;
      Window : Gdk.Gdk_Window);
   --  Type of the callback used to set up Window so it can be grabbed. A
   --  typical action would be ensuring the window is visible, although there's
   --  room for other initialization actions.
   --  Since: gtk+ 3.20
   --  "seat": the Gdk.Seat.Gdk_Seat being grabbed
   --  "window": the Gdk.Gdk_Window being grabbed

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Seat_Capabilities_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Seat_Capabilities);
   type Property_Gdk_Seat_Capabilities is new Gdk_Seat_Capabilities_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_seat_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Capabilities
      (Self : not null access Gdk_Seat_Record) return Gdk_Seat_Capabilities;
   --  Returns the capabilities this Gdk.Seat.Gdk_Seat currently has.
   --  Since: gtk+ 3.20

   function Get_Display
      (Self : not null access Gdk_Seat_Record) return Glib.Object.GObject;
   --  Returns the Gdk.Display.Gdk_Display this seat belongs to.

   function Get_Keyboard
      (Self : not null access Gdk_Seat_Record) return Gdk.Device.Gdk_Device;
   --  Returns the master device that routes keyboard events.
   --  Since: gtk+ 3.20

   function Get_Pointer
      (Self : not null access Gdk_Seat_Record) return Gdk.Device.Gdk_Device;
   --  Returns the master device that routes pointer events.
   --  Since: gtk+ 3.20

   function Get_Slaves
      (Self         : not null access Gdk_Seat_Record;
       Capabilities : Gdk_Seat_Capabilities)
       return Gdk.Device.Device_List.Glist;
   --  Returns the slave devices that match the given capabilities.
   --  Since: gtk+ 3.20
   --  "capabilities": capabilities to get devices for

   function Grab
      (Self         : not null access Gdk_Seat_Record;
       Window       : Gdk.Gdk_Window;
       Capabilities : Gdk_Seat_Capabilities;
       Owner_Events : Boolean;
       Cursor       : Gdk.Gdk_Cursor;
       Event        : Gdk.Event.Gdk_Event;
       Prepare_Func : Gdk_Seat_Grab_Prepare_Func) return Gdk_Grab_Status;
   --  Grabs the seat so that all events corresponding to the given
   --  Capabilities are passed to this application until the seat is ungrabbed
   --  with Gdk.Seat.Ungrab, or the window becomes hidden. This overrides any
   --  previous grab on the seat by this client.
   --  As a rule of thumb, if a grab is desired over
   --  Gdk.Seat.Gdk_Seat_Capability_Pointer, all other "pointing" capabilities
   --  (eg. Gdk.Seat.Gdk_Seat_Capability_Touch) should be grabbed too, so the
   --  user is able to interact with all of those while the grab holds, you
   --  should thus use Gdk.Seat.Gdk_Seat_Capability_All_Pointing most commonly.
   --  Grabs are used for operations which need complete control over the
   --  events corresponding to the given capabilities. For example in GTK+ this
   --  is used for Drag and Drop operations, popup menus and such.
   --  Note that if the event mask of a Gdk.Gdk_Window has selected both
   --  button press and button release events, or touch begin and touch end,
   --  then a press event will cause an automatic grab until the button is
   --  released, equivalent to a grab on the window with Owner_Events set to
   --  True. This is done because most applications expect to receive paired
   --  press and release events.
   --  If you set up anything at the time you take the grab that needs to be
   --  cleaned up when the grab ends, you should handle the
   --  Gdk.Event.Gdk_Event_Grab_Broken events that are emitted when the grab
   --  ends unvoluntarily.
   --  Since: gtk+ 3.20
   --  "window": the Gdk.Gdk_Window which will own the grab
   --  "capabilities": capabilities that will be grabbed
   --  "owner_events": if False then all device events are reported with
   --  respect to Window and are only reported if selected by Event_Mask. If
   --  True then pointer events for this application are reported as normal,
   --  but pointer events outside this application are reported with respect to
   --  Window and only if selected by Event_Mask. In either mode, unreported
   --  events are discarded.
   --  "cursor": the cursor to display while the grab is active. If this is
   --  null then the normal cursors are used for Window and its descendants,
   --  and the cursor for Window is used elsewhere.
   --  "event": the event that is triggering the grab, or null if none is
   --  available.
   --  "prepare_func": function to prepare the window to be grabbed, it can be
   --  null if Window is visible before this call.

   procedure Ungrab (Self : not null access Gdk_Seat_Record);
   --  Releases a grab added through Gdk.Seat.Grab.
   --  Since: gtk+ 3.20

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display
   --  Gdk.Display.Gdk_Display of this seat.

   -------------
   -- Signals --
   -------------

   type Cb_Gdk_Seat_GObject_Void is not null access procedure
     (Self   : access Gdk_Seat_Record'Class;
      Device : not null access Glib.Object.GObject_Record'Class);

   type Cb_GObject_GObject_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Device : not null access Glib.Object.GObject_Record'Class);

   Signal_Device_Added : constant Glib.Signal_Name := "device-added";
   procedure On_Device_Added
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_Gdk_Seat_GObject_Void;
       After : Boolean := False);
   procedure On_Device_Added
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::device-added signal is emitted when a new input device is related
   --  to this seat.

   Signal_Device_Removed : constant Glib.Signal_Name := "device-removed";
   procedure On_Device_Removed
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_Gdk_Seat_GObject_Void;
       After : Boolean := False);
   procedure On_Device_Removed
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::device-removed signal is emitted when an input device is removed
   --  (e.g. unplugged).

   Signal_Tool_Added : constant Glib.Signal_Name := "tool-added";
   procedure On_Tool_Added
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_Gdk_Seat_GObject_Void;
       After : Boolean := False);
   procedure On_Tool_Added
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::tool-added signal is emitted whenever a new tool is made known to
   --  the seat. The tool may later be assigned to a device (i.e. on proximity
   --  with a tablet). The device will emit the
   --  Gdk.Device.Gdk_Device::tool-changed signal accordingly.
   --
   --  A same tool may be used by several devices.

   Signal_Tool_Removed : constant Glib.Signal_Name := "tool-removed";
   procedure On_Tool_Removed
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_Gdk_Seat_GObject_Void;
       After : Boolean := False);
   procedure On_Tool_Removed
      (Self  : not null access Gdk_Seat_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted whenever a tool is no longer known to this Seat.

private
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
end Gdk.Seat;
