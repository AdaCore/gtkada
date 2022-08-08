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
--  In addition to a single pointer and keyboard for user interface input, GDK
--  contains support for a variety of input devices, including graphics
--  tablets, touchscreens and multiple pointers/keyboards interacting
--  simultaneously with the user interface. Such input devices often have
--  additional features, such as sub-pixel positioning information and
--  additional device-dependent information.
--
--  In order to query the device hierarchy and be aware of changes in the
--  device hierarchy (such as virtual devices being created or removed, or
--  physical devices being plugged or unplugged), GDK provides
--  Gdk.Device_Manager.Gdk_Device_Manager.
--
--  By default, and if the platform supports it, GDK is aware of multiple
--  keyboard/pointer pairs and multitouch devices. This behavior can be changed
--  by calling gdk_disable_multidevice before Gdk.Display.Open. There should
--  rarely be a need to do that though, since GDK defaults to a compatibility
--  mode in which it will emit just one enter/leave event pair for all devices
--  on a window. To enable per-device enter/leave events and other
--  multi-pointer interaction features, Gdk.Window.Set_Support_Multidevice must
--  be called on Gdk_Windows (or Gtk.Widget.Set_Support_Multidevice on
--  widgets). window. See the Gdk.Window.Set_Support_Multidevice documentation
--  for more information.
--
--  On X11, multi-device support is implemented through XInput 2. Unless
--  gdk_disable_multidevice is called, the XInput 2
--  Gdk.Device_Manager.Gdk_Device_Manager implementation will be used as the
--  input source. Otherwise either the core or XInput 1 implementations will be
--  used.
--
--  For simple applications that don't have any special interest in input
--  devices, the so-called "client pointer" provides a reasonable approximation
--  to a simple setup with a single pointer and keyboard. The device that has
--  been set as the client pointer can be accessed via
--  Gdk.Device_Manager.Get_Client_Pointer.
--
--  Conceptually, in multidevice mode there are 2 device types. Virtual
--  devices (or master devices) are represented by the pointer cursors and
--  keyboard foci that are seen on the screen. Physical devices (or slave
--  devices) represent the hardware that is controlling the virtual devices,
--  and thus have no visible cursor on the screen.
--
--  Virtual devices are always paired, so there is a keyboard device for every
--  pointer device. Associations between devices may be inspected through
--  Gdk.Device.Get_Associated_Device.
--
--  There may be several virtual devices, and several physical devices could
--  be controlling each of these virtual devices. Physical devices may also be
--  "floating", which means they are not attached to any virtual device.
--
--  # Master and slave devices
--
--  |[ carlosSacarino:~$ xinput list ⎡ Virtual core pointer id=2 [master
--  pointer (3)] ⎜ ↳ Virtual core XTEST pointer id=4 [slave pointer (2)] ⎜ ↳
--  Wacom ISDv4 E6 Pen stylus id=10 [slave pointer (2)] ⎜ ↳ Wacom ISDv4 E6
--  Finger touch id=11 [slave pointer (2)] ⎜ ↳ SynPS/2 Synaptics TouchPad id=13
--  [slave pointer (2)] ⎜ ↳ TPPS/2 IBM TrackPoint id=14 [slave pointer (2)] ⎜ ↳
--  Wacom ISDv4 E6 Pen eraser id=16 [slave pointer (2)] ⎣ Virtual core keyboard
--  id=3 [master keyboard (2)] ↳ Virtual core XTEST keyboard id=5 [slave
--  keyboard (3)] ↳ Power Button id=6 [slave keyboard (3)] ↳ Video Bus id=7
--  [slave keyboard (3)] ↳ Sleep Button id=8 [slave keyboard (3)] ↳ Integrated
--  Camera id=9 [slave keyboard (3)] ↳ AT Translated Set 2 keyboard id=12
--  [slave keyboard (3)] ↳ ThinkPad Extra Buttons id=15 [slave keyboard (3)] ]|
--
--  By default, GDK will automatically listen for events coming from all
--  master devices, setting the Gdk.Device.Gdk_Device for all events coming
--  from input devices. Events containing device information are
--  GDK_MOTION_NOTIFY, GDK_BUTTON_PRESS, GDK_2_BUTTON_PRESS,
--  GDK_3_BUTTON_PRESS, GDK_BUTTON_RELEASE, GDK_SCROLL, GDK_KEY_PRESS,
--  GDK_KEY_RELEASE, GDK_ENTER_NOTIFY, GDK_LEAVE_NOTIFY, GDK_FOCUS_CHANGE,
--  GDK_PROXIMITY_IN, GDK_PROXIMITY_OUT, GDK_DRAG_ENTER, GDK_DRAG_LEAVE,
--  GDK_DRAG_MOTION, GDK_DRAG_STATUS, GDK_DROP_START, GDK_DROP_FINISHED and
--  GDK_GRAB_BROKEN. When dealing with an event on a master device, it is
--  possible to get the source (slave) device that the event originated from
--  via gdk_event_get_source_device.
--
--  On a standard session, all physical devices are connected by default to
--  the "Virtual Core Pointer/Keyboard" master devices, hence routing all
--  events through these. This behavior is only modified by device grabs, where
--  the slave device is temporarily detached for as long as the grab is held,
--  and more permanently by user modifications to the device hierarchy.
--
--  On certain application specific setups, it may make sense to detach a
--  physical device from its master pointer, and mapping it to an specific
--  window. This can be achieved by the combination of Gdk.Device.Grab and
--  Gdk.Device.Set_Mode.
--
--  In order to listen for events coming from devices other than a virtual
--  device, Gdk.Window.Set_Device_Events must be called. Generally, this
--  function can be used to modify the event mask for any given device.
--
--  Input devices may also provide additional information besides X/Y. For
--  example, graphics tablets may also provide pressure and X/Y tilt
--  information. This information is device-dependent, and may be queried
--  through gdk_device_get_axis. In multidevice mode, virtual devices will
--  change axes in order to always represent the physical device that is
--  routing events through it. Whenever the physical device changes, the
--  Gdk.Device.Gdk_Device:n-axes property will be notified, and
--  gdk_device_list_axes will return the new device axes.
--
--  Devices may also have associated "keys" or macro buttons. Such keys can be
--  globally set to map into normal X keyboard events. The mapping is set using
--  Gdk.Device.Set_Key.
--
--  In GTK+ 3.20, a new Gdk.Seat.Gdk_Seat object has been introduced that
--  supersedes Gdk.Device_Manager.Gdk_Device_Manager and should be preferred in
--  newly written code.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Device;      use Gdk.Device;
with Gdk.Display;     use Gdk.Display;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gdk.Device_Manager is

   type Gdk_Device_Manager_Record is new GObject_Record with null record;
   type Gdk_Device_Manager is access all Gdk_Device_Manager_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_device_manager_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Client_Pointer
      (Self : not null access Gdk_Device_Manager_Record)
       return Gdk.Device.Gdk_Device;
   pragma Obsolescent (Get_Client_Pointer);
   --  Returns the client pointer, that is, the master pointer that acts as
   --  the core pointer for this application. In X11, window managers may
   --  change this depending on the interaction pattern under the presence of
   --  several pointers.
   --  You should use this function seldomly, only in code that isn't
   --  triggered by a Gdk.Event.Gdk_Event and there aren't other means to get a
   --  meaningful Gdk.Device.Gdk_Device to operate on.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.20, 1

   function Get_Display
      (Self : not null access Gdk_Device_Manager_Record)
       return Gdk.Display.Gdk_Display;
   --  Gets the Gdk.Display.Gdk_Display associated to Device_Manager.
   --  Since: gtk+ 3.0

   function List_Devices
      (Self     : not null access Gdk_Device_Manager_Record;
       The_Type : Gdk.Device.Gdk_Device_Type)
       return Gdk.Device.Device_List.Glist;
   pragma Obsolescent (List_Devices);
   --  Returns the list of devices of type Type currently attached to
   --  Device_Manager.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.20, 1
   --  "type": device type to get.

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get_Device_Manager
     (Self : not null access Gdk.Display.Gdk_Display_Record'Class)
   return Gdk.Device_Manager.Gdk_Device_Manager;
   --  Returns the Gdk.Device_Manager.Gdk_Device_Manager associated to
   --  Display.
   --  Since: gtk+ 3.0

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display

   -------------
   -- Signals --
   -------------

   Signal_Device_Added : constant Glib.Signal_Name := "device-added";
   --  The ::device-added signal is emitted either when a new master pointer
   --  is created, or when a slave (Hardware) input device is plugged in.
   --    procedure Handler
   --       (Self   : access Gdk_Device_Manager_Record'Class;
   --        Device : Device)

   Signal_Device_Changed : constant Glib.Signal_Name := "device-changed";
   --  The ::device-changed signal is emitted whenever a device has changed in
   --  the hierarchy, either slave devices being disconnected from their master
   --  device or connected to another one, or master devices being added or
   --  removed a slave device.
   --
   --  If a slave device is detached from all master devices
   --  (gdk_device_get_associated_device returns null), its
   --  Gdk.Device.Gdk_Device_Type will change to
   --  Gdk.Device.Gdk_Device_Type_Floating, if it's attached, it will change to
   --  Gdk.Device.Gdk_Device_Type_Slave.
   --    procedure Handler
   --       (Self   : access Gdk_Device_Manager_Record'Class;
   --        Device : Device)

   Signal_Device_Removed : constant Glib.Signal_Name := "device-removed";
   --  The ::device-removed signal is emitted either when a master pointer is
   --  removed, or when a slave (Hardware) input device is unplugged.
   --    procedure Handler
   --       (Self   : access Gdk_Device_Manager_Record'Class;
   --        Device : Device)

private
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
end Gdk.Device_Manager;
