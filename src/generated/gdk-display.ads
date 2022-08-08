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
--  Gdk.Display.Gdk_Display objects purpose are two fold:
--
--  - To manage and provide information about input devices (pointers and
--  keyboards)
--
--  - To manage and provide information about the available Gdk_Screens
--
--  GdkDisplay objects are the GDK representation of an X Display, which can
--  be described as a workstation consisting of a keyboard, a pointing device
--  (such as a mouse) and one or more screens. It is used to open and keep
--  track of various GdkScreen objects currently instantiated by the
--  application. It is also used to access the keyboard(s) and mouse pointer(s)
--  of the display.
--
--  Most of the input device handling has been factored out into the separate
--  Gdk.Device_Manager.Gdk_Device_Manager object. Every display has a device
--  manager, which you can obtain using gdk_display_get_device_manager.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Event;   use Gdk.Event;
with Gdk.Monitor; use Gdk.Monitor;
with Gdk.Types;   use Gdk.Types;
with Glib;        use Glib;
with Glib.Object; use Glib.Object;

package Gdk.Display is

   type Gdk_Display_Record is new GObject_Record with null record;
   type Gdk_Display is access all Gdk_Display_Record'Class;

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
   --  Since: gtk+ 2.2

   procedure Close (Self : not null access Gdk_Display_Record);
   --  Closes the connection to the windowing system for the given display,
   --  and cleans up associated resources.
   --  Since: gtk+ 2.2

   procedure Flush (Self : not null access Gdk_Display_Record);
   --  Flushes any requests queued for the windowing system; this happens
   --  automatically when the main loop blocks waiting for new events, but if
   --  your application is drawing without returning control to the main loop,
   --  you may need to call this function explicitly. A common case where this
   --  function needs to be called is when an application is executing drawing
   --  commands from a thread other than the thread where the main loop is
   --  running.
   --  This is most useful for X11. On windowing systems where requests are
   --  handled synchronously, this function will do nothing.
   --  Since: gtk+ 2.4

   function Get_Default_Cursor_Size
      (Self : not null access Gdk_Display_Record) return Guint;
   --  Returns the default size to use for cursors on Display.
   --  Since: gtk+ 2.4

   function Get_Default_Group
      (Self : not null access Gdk_Display_Record) return Gdk.Gdk_Window;
   --  Returns the default group leader window for all toplevel windows on
   --  Display. This window is implicitly created by GDK. See
   --  Gdk.Window.Set_Group.
   --  Since: gtk+ 2.4

   function Get_Default_Seat
      (Self : not null access Gdk_Display_Record) return Glib.Object.GObject;
   --  Returns the default Gdk.Seat.Gdk_Seat for this display.
   --  Since: gtk+ 3.20

   function Get_Event
      (Self : not null access Gdk_Display_Record) return Gdk.Event.Gdk_Event;
   --  Gets the next Gdk.Event.Gdk_Event to be processed for Display, fetching
   --  events from the windowing system if necessary.
   --  Since: gtk+ 2.2

   procedure Get_Maximal_Cursor_Size
      (Self   : not null access Gdk_Display_Record;
       Width  : out Guint;
       Height : out Guint);
   --  Gets the maximal size to use for cursors on Display.
   --  Since: gtk+ 2.4
   --  "width": the return location for the maximal cursor width
   --  "height": the return location for the maximal cursor height

   function Get_Monitor
      (Self        : not null access Gdk_Display_Record;
       Monitor_Num : Glib.Gint) return Gdk.Monitor.Gdk_Monitor;
   --  Gets a monitor associated with this display.
   --  Since: gtk+ 3.22
   --  "monitor_num": number of the monitor

   function Get_Monitor_At_Point
      (Self : not null access Gdk_Display_Record;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Gdk.Monitor.Gdk_Monitor;
   --  Gets the monitor in which the point (X, Y) is located, or a nearby
   --  monitor if the point is not in any monitor.
   --  Since: gtk+ 3.22
   --  "x": the x coordinate of the point
   --  "y": the y coordinate of the point

   function Get_Monitor_At_Window
      (Self   : not null access Gdk_Display_Record;
       Window : Gdk.Gdk_Window) return Gdk.Monitor.Gdk_Monitor;
   --  Gets the monitor in which the largest area of Window resides, or a
   --  monitor close to Window if it is outside of all monitors.
   --  Since: gtk+ 3.22
   --  "window": a Gdk.Gdk_Window

   function Get_N_Monitors
      (Self : not null access Gdk_Display_Record) return Glib.Gint;
   --  Gets the number of monitors that belong to Display.
   --  The returned number is valid until the next emission of the
   --  Gdk.Display.Gdk_Display::monitor-added or
   --  Gdk.Display.Gdk_Display::monitor-removed signal.
   --  Since: gtk+ 3.22

   function Get_N_Screens
      (Self : not null access Gdk_Display_Record) return Glib.Gint;
   pragma Obsolescent (Get_N_Screens);
   --  Gets the number of screen managed by the Display.
   --  Since: gtk+ 2.2
   --  Deprecated since 3.10, 1

   function Get_Name
      (Self : not null access Gdk_Display_Record) return UTF8_String;
   --  Gets the name of the display.
   --  Since: gtk+ 2.2

   function Get_Primary_Monitor
      (Self : not null access Gdk_Display_Record)
       return Gdk.Monitor.Gdk_Monitor;
   --  Gets the primary monitor for the display.
   --  The primary monitor is considered the monitor where the "main desktop"
   --  lives. While normal application windows typically allow the window
   --  manager to place the windows, specialized desktop applications such as
   --  panels should place themselves on the primary monitor.
   --  Since: gtk+ 3.22

   function Has_Pending
      (Self : not null access Gdk_Display_Record) return Boolean;
   --  Returns whether the display has events that are waiting to be
   --  processed.
   --  Since: gtk+ 3.0

   function Is_Closed
      (Self : not null access Gdk_Display_Record) return Boolean;
   --  Finds out if the display has been closed.
   --  Since: gtk+ 2.22

   procedure Keyboard_Ungrab
      (Self : not null access Gdk_Display_Record;
       Time : Guint32);
   pragma Obsolescent (Keyboard_Ungrab);
   --  Release any keyboard grab
   --  Since: gtk+ 2.2
   --  Deprecated since 3.0, 1
   --  "time_": a timestap (e.g GDK_CURRENT_TIME).

   function List_Seats
      (Self : not null access Gdk_Display_Record)
       return Glib.Object.Object_Simple_List.Glist;
   --  Returns the list of seats known to Display.
   --  Since: gtk+ 3.20

   procedure Notify_Startup_Complete
      (Self       : not null access Gdk_Display_Record;
       Startup_Id : UTF8_String);
   --  Indicates to the GUI environment that the application has finished
   --  loading, using a given identifier.
   --  GTK+ will call this function automatically for Gtk.Window.Gtk_Window
   --  with custom startup-notification identifier unless
   --  Gtk.Window.Set_Auto_Startup_Notification is called to disable that
   --  feature.
   --  Since: gtk+ 3.0
   --  "startup_id": a startup-notification identifier, for which notification
   --  process should be completed

   function Peek_Event
      (Self : not null access Gdk_Display_Record) return Gdk.Event.Gdk_Event;
   --  Gets a copy of the first Gdk.Event.Gdk_Event in the Display's event
   --  queue, without removing the event from the queue. (Note that this
   --  function will not get more events from the windowing system. It only
   --  checks the events that have already been moved to the GDK event queue.)
   --  Since: gtk+ 2.2

   function Pointer_Is_Grabbed
      (Self : not null access Gdk_Display_Record) return Boolean;
   pragma Obsolescent (Pointer_Is_Grabbed);
   --  Test if the pointer is grabbed.
   --  Since: gtk+ 2.2
   --  Deprecated since 3.0, 1

   procedure Pointer_Ungrab
      (Self : not null access Gdk_Display_Record;
       Time : Guint32);
   pragma Obsolescent (Pointer_Ungrab);
   --  Release any pointer grab.
   --  Since: gtk+ 2.2
   --  Deprecated since 3.0, 1
   --  "time_": a timestap (e.g. GDK_CURRENT_TIME).

   procedure Put_Event
      (Self  : not null access Gdk_Display_Record;
       Event : Gdk.Event.Gdk_Event);
   --  Appends a copy of the given event onto the front of the event queue for
   --  Display.
   --  Since: gtk+ 2.2
   --  "event": a Gdk.Event.Gdk_Event.

   function Request_Selection_Notification
      (Self      : not null access Gdk_Display_Record;
       Selection : Gdk.Types.Gdk_Atom) return Boolean;
   --  Request Gdk.Event.Gdk_Event_Owner_Change events for ownership changes
   --  of the selection named by the given atom.
   --  Since: gtk+ 2.6
   --  "selection": the Gdk.Types.Gdk_Atom naming the selection for which
   --  ownership change notification is requested

   procedure Set_Double_Click_Distance
      (Self     : not null access Gdk_Display_Record;
       Distance : Guint);
   --  Sets the double click distance (two clicks within this distance count
   --  as a double click and result in a GDK_2_BUTTON_PRESS event). See also
   --  Gdk.Display.Set_Double_Click_Time. Applications should not set this, it
   --  is a global user-configured setting.
   --  Since: gtk+ 2.4
   --  "distance": distance in pixels

   procedure Set_Double_Click_Time
      (Self : not null access Gdk_Display_Record;
       Msec : Guint);
   --  Sets the double click time (two clicks within this time interval count
   --  as a double click and result in a GDK_2_BUTTON_PRESS event).
   --  Applications should not set this, it is a global user-configured
   --  setting.
   --  Since: gtk+ 2.2
   --  "msec": double click time in milliseconds (thousandths of a second)

   function Supports_Clipboard_Persistence
      (Self : not null access Gdk_Display_Record) return Boolean;
   --  Returns whether the speicifed display supports clipboard persistance;
   --  i.e. if it's possible to store the clipboard data after an application
   --  has quit. On X11 this checks if a clipboard daemon is running.
   --  Since: gtk+ 2.6

   function Supports_Composite
      (Self : not null access Gdk_Display_Record) return Boolean;
   pragma Obsolescent (Supports_Composite);
   --  Returns True if Gdk.Window.Set_Composited can be used to redirect
   --  drawing on the window using compositing.
   --  Currently this only works on X11 with XComposite and XDamage extensions
   --  available.
   --  Since: gtk+ 2.12
   --  Deprecated since 3.16, 1

   function Supports_Cursor_Alpha
      (Self : not null access Gdk_Display_Record) return Boolean;
   --  Returns True if cursors can use an 8bit alpha channel on Display.
   --  Otherwise, cursors are restricted to bilevel alpha (i.e. a mask).
   --  Since: gtk+ 2.4

   function Supports_Cursor_Color
      (Self : not null access Gdk_Display_Record) return Boolean;
   --  Returns True if multicolored cursors are supported on Display.
   --  Otherwise, cursors have only a forground and a background color.
   --  Since: gtk+ 2.4

   function Supports_Input_Shapes
      (Self : not null access Gdk_Display_Record) return Boolean;
   --  Returns True if gdk_window_input_shape_combine_mask can be used to
   --  modify the input shape of windows on Display.
   --  Since: gtk+ 2.10

   function Supports_Selection_Notification
      (Self : not null access Gdk_Display_Record) return Boolean;
   --  Returns whether Gdk.Event.Gdk_Event_Owner_Change events will be sent
   --  when the owner of a selection changes.
   --  Since: gtk+ 2.6

   function Supports_Shapes
      (Self : not null access Gdk_Display_Record) return Boolean;
   --  Returns True if gdk_window_shape_combine_mask can be used to create
   --  shaped windows on Display.
   --  Since: gtk+ 2.10

   procedure Sync (Self : not null access Gdk_Display_Record);
   --  Flushes any requests queued for the windowing system and waits until
   --  all requests have been handled. This is often used for making sure that
   --  the display is synchronized with the current state of the program.
   --  Calling Gdk.Display.Sync before gdk_error_trap_pop makes sure that any
   --  errors generated from earlier requests are handled before the error trap
   --  is removed.
   --  This is most useful for X11. On windowing systems where requests are
   --  handled synchronously, this function will do nothing.
   --  Since: gtk+ 2.2

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Store_Clipboard
     (Display          : not null access Gdk_Display_Record;
      Clipboard_Window : Gdk.Gdk_Window;
      Time             : Guint32;
      Targets          : Gdk.Types.Gdk_Atom_Array);
   --  Issues a request to the clipboard manager to store the clipboard data.
   --  On X11, this is a special program that works according to the
   --  freedesktop clipboard specification, available at <ulink
   --  url="http://www.freedesktop.org/Standards/clipboard-manager-spec">
   --  http://www.freedesktop.org/Standards/clipboard-manager-spec</ulink>>.
   --  Since: gtk+ 2.6
   --  "clipboard_window": a Gdk.Gdk_Window belonging to the clipboard owner
   --  "time_": a timestamp
   --  "targets": an array of targets that should be saved, or null if all
   --  available targets should be saved.
   --  "n_targets": length of the Targets array

   procedure Get_Window_At_Pointer
     (Display : access Gdk_Display_Record;
      Win_X   : out Glib.Gint;
      Win_Y   : out Glib.Gint;
      Win     : out Gdk.Gdk_Window);
   --  Obtains the window underneath the mouse pointer, returning the location
   --  of that window in Win_X, Win_Y. Returns nullif the window
   --  under the mouse pointer is not known to GDK (for example, belongs to
   --  another application).
   --  (Win_X, Win_Y) are relative to the origin of the window under the
   --  pointer.
   --
   --  Obsolescent: use Gdk.Device.Get_Window_At_Position instead.

   ---------------
   -- Functions --
   ---------------

   function Get_Default return Gdk_Display;
   --  Gets the default Gdk.Display.Gdk_Display. This is a convenience
   --  function for: `gdk_display_manager_get_default_display
   --  (gdk_display_manager_get ())`.
   --  Since: gtk+ 2.2

   function Open (Display_Name : UTF8_String) return Gdk_Display;
   --  Opens a display.
   --  Since: gtk+ 2.2
   --  "display_name": the name of the display to open

   function Open_Default_Libgtk_Only return Gdk_Display;
   pragma Obsolescent (Open_Default_Libgtk_Only);
   --  Opens the default display specified by command line arguments or
   --  environment variables, sets it as the default display, and returns it.
   --  gdk_parse_args must have been called first. If the default display has
   --  previously been set, simply returns that. An internal function that
   --  should not be used by applications.
   --  Deprecated since 3.16, 1

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
   --  The ::closed signal is emitted when the connection to the windowing
   --  system for Display is closed.

   type Cb_Gdk_Display_GObject_Void is not null access procedure
     (Self    : access Gdk_Display_Record'Class;
      Monitor : not null access Glib.Object.GObject_Record'Class);

   type Cb_GObject_GObject_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Monitor : not null access Glib.Object.GObject_Record'Class);

   Signal_Monitor_Added : constant Glib.Signal_Name := "monitor-added";
   procedure On_Monitor_Added
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_GObject_Void;
       After : Boolean := False);
   procedure On_Monitor_Added
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::monitor-added signal is emitted whenever a monitor is added.

   Signal_Monitor_Removed : constant Glib.Signal_Name := "monitor-removed";
   procedure On_Monitor_Removed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_GObject_Void;
       After : Boolean := False);
   procedure On_Monitor_Removed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::monitor-removed signal is emitted whenever a monitor is removed.

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
   --  The ::opened signal is emitted when the connection to the windowing
   --  system for Display is opened.

   Signal_Seat_Added : constant Glib.Signal_Name := "seat-added";
   procedure On_Seat_Added
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_GObject_Void;
       After : Boolean := False);
   procedure On_Seat_Added
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::seat-added signal is emitted whenever a new seat is made known to
   --  the windowing system.

   Signal_Seat_Removed : constant Glib.Signal_Name := "seat-removed";
   procedure On_Seat_Removed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_Gdk_Display_GObject_Void;
       After : Boolean := False);
   procedure On_Seat_Removed
      (Self  : not null access Gdk_Display_Record;
       Call  : Cb_GObject_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::seat-removed signal is emitted whenever a seat is removed by the
   --  windowing system.

end Gdk.Display;
