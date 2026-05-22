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

--  A representation of a workstation.
--
--  Their purpose are two-fold:
--
--  - To manage and provide information about input devices (pointers,
--  keyboards, etc) - To manage and provide information about output devices
--  (monitors, projectors, etc)
--
--  Most of the input device handling has been factored out into separate
--  [classGdk.Seat] objects. Every display has a one or more seats, which can
--  be accessed with [methodGdk.Display.get_default_seat] and
--  [methodGdk.Display.list_seats].
--
--  Output devices are represented by [classGdk.Monitor] objects, which can be
--  accessed with [methodGdk.Display.get_monitor_at_surface] and similar APIs.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Dmabuf_Formats; use Gdk.Dmabuf_Formats;
with Glib;               use Glib;
with Glib.List_Model;    use Glib.List_Model;
with Glib.Object;        use Glib.Object;
with Glib.Properties;    use Glib.Properties;
with Glib.Values;        use Glib.Values;

package Gdk.Display is

   type Gdk_Display_Record is new GObject_Record with null record;
   subtype Gdk_Display is Gdk.Gdk_Display;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_display_get_type");

   -------------
   -- Methods --
   -------------

   procedure Beep (Self : not null access Gdk_Display_Record);
   --  Emits a short beep on Display

   procedure Close (Self : not null access Gdk_Display_Record);
   --  Closes the connection to the windowing system for the given display.
   --  This cleans up associated resources.

   function Device_Is_Grabbed
      (Self   : not null access Gdk_Display_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Boolean;
   --  Returns True if there is an ongoing grab on Device for Display.
   --  @param Device a `GdkDevice`
   --  @return True if there is a grab in effect for Device.

   procedure Flush (Self : not null access Gdk_Display_Record);
   --  Flushes any requests queued for the windowing system.
   --  This happens automatically when the main loop blocks waiting for new
   --  events, but if your application is drawing without returning control to
   --  the main loop, you may need to call this function explicitly. A common
   --  case where this function needs to be called is when an application is
   --  executing drawing commands from a thread other than the thread where the
   --  main loop is running.
   --  This is most useful for X11. On windowing systems where requests are
   --  handled synchronously, this function will do nothing.

   function Get_Dmabuf_Formats
      (Self : not null access Gdk_Display_Record)
       return Gdk.Dmabuf_Formats.Gdk_Dmabuf_Formats;
   --  Returns the dma-buf formats that are supported on this display.
   --  GTK may use OpenGL or Vulkan to support some formats. Calling this
   --  function will then initialize them if they aren't yet.
   --  The formats returned by this function can be used for negotiating
   --  buffer formats with producers such as v4l, pipewire or GStreamer.
   --  To learn more about dma-bufs, see [classGdk.DmabufTextureBuilder].
   --  This function is threadsafe. It can be called from any thread.
   --  Since: gtk+ 4.14
   --  @return a `GdkDmabufFormats` object

   function Get_Monitors
      (Self : not null access Gdk_Display_Record)
       return Glib.List_Model.Glist_Model;
   --  Gets the list of monitors associated with this display.
   --  Subsequent calls to this function will always return the same list for
   --  the same display.
   --  You can listen to the GListModel::items-changed signal on this list to
   --  monitor changes to the monitor of this display.
   --  @return a `GListModel` of `GdkMonitor`

   function Get_Name
      (Self : not null access Gdk_Display_Record) return UTF8_String;
   --  Gets the name of the display.
   --  @return a string representing the display name. This string is owned by
   --  GDK and should not be modified or freed.

   function Get_Setting
      (Self  : not null access Gdk_Display_Record;
       Name  : UTF8_String;
       Value : in out Glib.Values.GValue) return Boolean;
   --  Retrieves a desktop-wide setting such as double-click time for the
   --  Display.
   --  @param Name the name of the setting
   --  @param Value location to store the value of the setting
   --  @return True if the setting existed and a value was stored in Value,
   --  False otherwise

   function Get_Startup_Notification_Id
      (Self : not null access Gdk_Display_Record) return UTF8_String;
   pragma Obsolescent (Get_Startup_Notification_Id);
   --  Gets the startup notification ID for a Wayland display, or null if no
   --  ID has been defined.
   --  Deprecated since 4.10, 1
   --  @return the startup notification ID for Display

   function Is_Closed
      (Self : not null access Gdk_Display_Record) return Boolean;
   --  Finds out if the display has been closed.
   --  @return True if the display is closed.

   function Is_Composited
      (Self : not null access Gdk_Display_Record) return Boolean;
   --  Returns whether surfaces can reasonably be expected to have their alpha
   --  channel drawn correctly on the screen.
   --  Check [methodGdk.Display.is_rgba] for whether the display supports an
   --  alpha channel.
   --  On X11 this function returns whether a compositing manager is
   --  compositing on Display.
   --  On modern displays, this value is always True.
   --  @return Whether surfaces with RGBA visuals can reasonably be expected
   --  to have their alpha channels drawn correctly on the screen.

   function Is_Rgba
      (Self : not null access Gdk_Display_Record) return Boolean;
   --  Returns whether surfaces on this Display are created with an alpha
   --  channel.
   --  Even if a True is returned, it is possible that the surface's alpha
   --  channel won't be honored when displaying the surface on the screen: in
   --  particular, for X an appropriate windowing manager and compositing
   --  manager must be running to provide appropriate display. Use
   --  [methodGdk.Display.is_composited] to check if that is the case.
   --  On modern displays, this value is always True.
   --  @return True if surfaces are created with an alpha channel or False if
   --  the display does not support this functionality.

   procedure Notify_Startup_Complete
      (Self       : not null access Gdk_Display_Record;
       Startup_Id : UTF8_String);
   pragma Obsolescent (Notify_Startup_Complete);
   --  Indicates to the GUI environment that the application has finished
   --  loading, using a given identifier.
   --  GTK will call this function automatically for
   --  [GtkWindow](../gtk4/class.Window.html) with custom startup-notification
   --  identifier unless
   --  [gtk_window_set_auto_startup_notification](../gtk4/method.Window.set_auto_startup_notification.html)
   --  is called to disable that feature.
   --  Deprecated since 4.10, 1
   --  @param Startup_Id a startup-notification identifier, for which
   --  notification process should be completed

   function Prepare_Gl
      (Self : not null access Gdk_Display_Record) return Boolean;
   --  Checks that OpenGL is available for Self and ensures that it is
   --  properly initialized. When this fails, an Error will be set describing
   --  the error and this function returns False.
   --  Note that even if this function succeeds, creating a `GdkGLContext` may
   --  still fail.
   --  This function is idempotent. Calling it multiple times will just return
   --  the same value or error.
   --  You never need to call this function, GDK will call it automatically as
   --  needed. But you can use it as a check when setting up code that might
   --  make use of OpenGL.
   --  Since: gtk+ 4.4
   --  @return True if the display supports OpenGL

   function Supports_Input_Shapes
      (Self : not null access Gdk_Display_Record) return Boolean;
   --  Returns True if the display supports input shapes.
   --  This means that [methodGdk.Surface.set_input_region] can be used to
   --  modify the input shape of surfaces on Display.
   --  On modern displays, this value is always True.
   --  @return True if surfaces with modified input shape are supported

   function Supports_Shadow_Width
      (Self : not null access Gdk_Display_Record) return Boolean;
   --  Returns whether it's possible for a surface to draw outside of the
   --  window area.
   --  If True is returned the application decides if it wants to draw
   --  shadows. If False is returned, the compositor decides if it wants to
   --  draw shadows.
   --  Since: gtk+ 4.14
   --  @return True if surfaces can draw shadows or False if the display does
   --  not support this functionality.

   procedure Sync (Self : not null access Gdk_Display_Record);
   --  Flushes any requests queued for the windowing system and waits until
   --  all requests have been handled.
   --  This is often used for making sure that the display is synchronized
   --  with the current state of the program. Calling [methodGdk.Display.sync]
   --  before [methodGdkx11.Display.error_trap_pop] makes sure that any errors
   --  generated from earlier requests are handled before the error trap is
   --  removed.
   --  This is most useful for X11. On windowing systems where requests are
   --  handled synchronously, this function will do nothing.

   ---------------
   -- Functions --
   ---------------

   function Get_Default return Gdk.Gdk_Display;
   --  Gets the default `GdkDisplay`.
   --  This is a convenience function for:
   --  gdk_display_manager_get_default_display (gdk_display_manager_get ())
   --  @return a `GdkDisplay`, or null if there is no default display

   function Open (Display_Name : UTF8_String := "") return Gdk.Gdk_Display;
   --  Opens a display.
   --  If opening the display fails, `NULL` is returned.
   --  @param Display_Name the name of the display to open
   --  @return a `GdkDisplay`

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Composited_Property : constant Glib.Properties.Property_Boolean;
   --  True if the display properly composites the alpha channel.

   Dmabuf_Formats_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Dmabuf_Formats
   --  The dma-buf formats that are supported on this display

   Input_Shapes_Property : constant Glib.Properties.Property_Boolean;
   --  True if the display supports input shapes.

   Rgba_Property : constant Glib.Properties.Property_Boolean;
   --  True if the display supports an alpha channel.

   Shadow_Width_Property : constant Glib.Properties.Property_Boolean;
   --  True if the display supports extensible frames.

   -------------
   -- Signals --
   -------------

   type Cb_Gdk_Display_Boolean_Void is not null access procedure
     (Self     : access Gdk_Display_Record'Class;
      Is_Error : Boolean);

   type Cb_GObject_Boolean_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Is_Error : Boolean);

   Signal_Closed : constant Glib.Signal_Name := "closed";
   procedure On_Closed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_Boolean_Void;
       After : Boolean := False);
   procedure On_Closed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the connection to the windowing system for Display is
   --  closed.

   type Cb_Gdk_Display_Void is not null access procedure (Self : access Gdk_Display_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Opened : constant Glib.Signal_Name := "opened";
   procedure On_Opened
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_Void;
       After : Boolean := False);
   procedure On_Opened
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the connection to the windowing system for Display is
   --  opened.

   Signal_Seat_Added : constant Glib.Signal_Name := "seat-added";
   --  Emitted whenever a new seat is made known to the windowing system.
   --    procedure Handler
   --       (Self : access Gdk_Display_Record'Class;
   --        Seat : Seat)

   Signal_Seat_Removed : constant Glib.Signal_Name := "seat-removed";
   --  Emitted whenever a seat is removed by the windowing system.
   --    procedure Handler
   --       (Self : access Gdk_Display_Record'Class;
   --        Seat : Seat)

   type Cb_Gdk_Display_UTF8_String_Void is not null access procedure
     (Self    : access Gdk_Display_Record'Class;
      Setting : UTF8_String);

   type Cb_GObject_UTF8_String_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Setting : UTF8_String);

   Signal_Setting_Changed : constant Glib.Signal_Name := "setting-changed";
   procedure On_Setting_Changed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Setting_Changed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever a setting changes its value.

private
   Shadow_Width_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("shadow-width");
   Rgba_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("rgba");
   Input_Shapes_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("input-shapes");
   Dmabuf_Formats_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("dmabuf-formats");
   Composited_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("composited");
end Gdk.Display;
