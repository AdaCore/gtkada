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
--  A GtkWindow is a toplevel window which can contain other widgets. Windows
--  normally have decorations that are under the control of the windowing
--  system and allow the user to manipulate the window (resize it, move it,
--  close it,...).
--
--  # GtkWindow as GtkBuildable
--
--  The GtkWindow implementation of the Gtk.Buildable.Gtk_Buildable interface
--  supports a custom <accel-groups> element, which supports any number of
--  <group> elements representing the Gtk.Accel_Group.Gtk_Accel_Group objects
--  you want to add to your window (synonymous with Gtk.Window.Add_Accel_Group.
--
--  It also supports the <initial-focus> element, whose name property names
--  the widget to receive the focus when the window is mapped.
--
--  An example of a UI definition fragment with accel groups: |[ <object
--  class="GtkWindow"> <accel-groups> <group name="accelgroup1"/>
--  </accel-groups> <initial-focus name="thunderclap"/> </object>
--
--  ...
--
--  <object class="GtkAccelGroup" id="accelgroup1"/> ]|
--  The GtkWindow implementation of the Gtk.Buildable.Gtk_Buildable interface
--  supports setting a child as the titlebar by specifying "titlebar" as the
--  "type" attribute of a <child> element.
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> window.background ├── decoration ├── <titlebar
--  child>.titlebar [.default-decoration] ╰── <child> ]|
--
--  GtkWindow has a main CSS node with name window and style class
--  .background, and a subnode with name decoration.
--
--  Style classes that are typically used with the main CSS node are .csd
--  (when client-side decorations are in use), .solid-csd (for client-side
--  decorations without invisible borders), .ssd (used by mutter when rendering
--  server-side decorations). GtkWindow also represents window states with the
--  following style classes on the main node: .tiled, .maximized, .fullscreen.
--  Specialized types of window often add their own discriminating style
--  classes, such as .popup or .tooltip.
--
--  GtkWindow adds the .titlebar and .default-decoration style classes to the
--  widget that is added as a titlebar child.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Device;      use Gdk.Device;
with Gdk.Event;       use Gdk.Event;
with Gdk.Pixbuf;      use Gdk.Pixbuf;
with Gdk.Rectangle;   use Gdk.Rectangle;
with Gdk.Screen;      use Gdk.Screen;
with Gdk.Types;       use Gdk.Types;
with Gdk.Window;      use Gdk.Window;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Window is

   type Gtk_Window_Record is new Gtk_Bin_Record with null record;
   type Gtk_Window is access all Gtk_Window_Record'Class;

   type Gtk_Window_Group_Record is new GObject_Record with null record;
   type Gtk_Window_Group is access all Gtk_Window_Group_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Window   : out Gtk_Window;
       The_Type : Gtk.Enums.Gtk_Window_Type := Gtk.Enums.Window_Toplevel);
   procedure Initialize
      (Window   : not null access Gtk_Window_Record'Class;
       The_Type : Gtk.Enums.Gtk_Window_Type := Gtk.Enums.Window_Toplevel);
   --  Creates a new Gtk.Window.Gtk_Window, which is a toplevel window that
   --  can contain other widgets. Nearly always, the type of the window should
   --  be GTK_WINDOW_TOPLEVEL. If you're implementing something like a popup
   --  menu from scratch (which is a bad idea, just use Gtk.Menu.Gtk_Menu), you
   --  might use GTK_WINDOW_POPUP. GTK_WINDOW_POPUP is not for dialogs, though
   --  in some other toolkits dialogs are called "popups". In GTK+,
   --  GTK_WINDOW_POPUP means a pop-up menu or pop-up tooltip. On X11, popup
   --  windows are not controlled by the [window manager][gtk-X11-arch].
   --  If you simply want an undecorated window (no window borders), use
   --  Gtk.Window.Set_Decorated, don't use GTK_WINDOW_POPUP.
   --  All top-level windows created by Gtk.Window.Gtk_New are stored in an
   --  internal top-level window list. This list can be obtained from
   --  Gtk.Window.List_Toplevels. Due to Gtk+ keeping a reference to the window
   --  internally, Gtk.Window.Gtk_New does not return a reference to the
   --  caller.
   --  To delete a Gtk.Window.Gtk_Window, call Gtk.Widget.Destroy.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "type": type of window

   function Gtk_Window_New
      (The_Type : Gtk.Enums.Gtk_Window_Type := Gtk.Enums.Window_Toplevel)
       return Gtk_Window;
   --  Creates a new Gtk.Window.Gtk_Window, which is a toplevel window that
   --  can contain other widgets. Nearly always, the type of the window should
   --  be GTK_WINDOW_TOPLEVEL. If you're implementing something like a popup
   --  menu from scratch (which is a bad idea, just use Gtk.Menu.Gtk_Menu), you
   --  might use GTK_WINDOW_POPUP. GTK_WINDOW_POPUP is not for dialogs, though
   --  in some other toolkits dialogs are called "popups". In GTK+,
   --  GTK_WINDOW_POPUP means a pop-up menu or pop-up tooltip. On X11, popup
   --  windows are not controlled by the [window manager][gtk-X11-arch].
   --  If you simply want an undecorated window (no window borders), use
   --  Gtk.Window.Set_Decorated, don't use GTK_WINDOW_POPUP.
   --  All top-level windows created by Gtk.Window.Gtk_New are stored in an
   --  internal top-level window list. This list can be obtained from
   --  Gtk.Window.List_Toplevels. Due to Gtk+ keeping a reference to the window
   --  internally, Gtk.Window.Gtk_New does not return a reference to the
   --  caller.
   --  To delete a Gtk.Window.Gtk_Window, call Gtk.Widget.Destroy.
   --  "type": type of window

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_window_get_type");

   procedure Gtk_New (Window_Group : out Gtk_Window_Group);
   procedure Initialize
      (Window_Group : not null access Gtk_Window_Group_Record'Class);
   --  Creates a new Gtk.Window.Gtk_Window_Group object. Grabs added with
   --  Gtk.Widget.Grab_Add only affect windows within the same
   --  Gtk.Window.Gtk_Window_Group.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Window_Group_New return Gtk_Window_Group;
   --  Creates a new Gtk.Window.Gtk_Window_Group object. Grabs added with
   --  Gtk.Widget.Grab_Add only affect windows within the same
   --  Gtk.Window.Gtk_Window_Group.

   function Group_Get_Type return Glib.GType;
   pragma Import (C, Group_Get_Type, "gtk_window_group_get_type");

   -------------
   -- Methods --
   -------------

   function Activate_Default
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Activates the default widget for the window, unless the current focused
   --  widget has been configured to receive the default action (see
   --  Gtk.Widget.Set_Receives_Default), in which case the focused widget is
   --  activated.

   function Activate_Focus
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Activates the current focused widget within the window.

   function Activate_Key
      (Window : not null access Gtk_Window_Record;
       Event  : Gdk.Event.Gdk_Event_Key) return Boolean;
   --  Activates mnemonics and accelerators for this Gtk.Window.Gtk_Window.
   --  This is normally called by the default ::key_press_event handler for
   --  toplevel windows, however in some cases it may be useful to call this
   --  directly when overriding the standard key handling for a toplevel
   --  window.
   --  Since: gtk+ 2.4
   --  "event": a Gdk.Event.Gdk_Event_Key

   procedure Add_Accel_Group
      (Window      : not null access Gtk_Window_Record;
       Accel_Group : not null access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class);
   --  Associate Accel_Group with Window, such that calling
   --  Gtk.Accel_Group.Accel_Groups_Activate on Window will activate
   --  accelerators in Accel_Group.
   --  "accel_group": a Gtk.Accel_Group.Gtk_Accel_Group

   procedure Add_Mnemonic
      (Window : not null access Gtk_Window_Record;
       Keyval : Gdk.Types.Gdk_Key_Type;
       Target : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a mnemonic to this window.
   --  "keyval": the mnemonic
   --  "target": the widget that gets activated by the mnemonic

   procedure Begin_Move_Drag
      (Window    : not null access Gtk_Window_Record;
       Button    : Glib.Gint;
       Root_X    : Glib.Gint;
       Root_Y    : Glib.Gint;
       Timestamp : Guint32);
   --  Starts moving a window. This function is used if an application has
   --  window movement grips. When GDK can support it, the window movement will
   --  be done using the standard mechanism for the [window
   --  manager][gtk-X11-arch] or windowing system. Otherwise, GDK will try to
   --  emulate window movement, potentially not all that well, depending on the
   --  windowing system.
   --  "button": mouse button that initiated the drag
   --  "root_x": X position where the user clicked to initiate the drag, in
   --  root window coordinates
   --  "root_y": Y position where the user clicked to initiate the drag
   --  "timestamp": timestamp from the click event that initiated the drag

   procedure Begin_Resize_Drag
      (Window    : not null access Gtk_Window_Record;
       Edge      : Gdk.Window.Gdk_Window_Edge;
       Button    : Glib.Gint;
       Root_X    : Glib.Gint;
       Root_Y    : Glib.Gint;
       Timestamp : Guint32);
   --  Starts resizing a window. This function is used if an application has
   --  window resizing controls. When GDK can support it, the resize will be
   --  done using the standard mechanism for the [window manager][gtk-X11-arch]
   --  or windowing system. Otherwise, GDK will try to emulate window resizing,
   --  potentially not all that well, depending on the windowing system.
   --  "edge": position of the resize control
   --  "button": mouse button that initiated the drag
   --  "root_x": X position where the user clicked to initiate the drag, in
   --  root window coordinates
   --  "root_y": Y position where the user clicked to initiate the drag
   --  "timestamp": timestamp from the click event that initiated the drag

   procedure Close (Window : not null access Gtk_Window_Record);
   --  Requests that the window is closed, similar to what happens when a
   --  window manager close button is clicked.
   --  This function can be used with close buttons in custom titlebars.
   --  Since: gtk+ 3.10

   procedure Deiconify (Window : not null access Gtk_Window_Record);
   --  Asks to deiconify (i.e. unminimize) the specified Window. Note that you
   --  shouldn't assume the window is definitely deiconified afterward, because
   --  other entities (e.g. the user or [window manager][gtk-X11-arch])) could
   --  iconify it again before your code which assumes deiconification gets to
   --  run.
   --  You can track iconification via the "window-state-event" signal on
   --  Gtk.Widget.Gtk_Widget.

   procedure Fullscreen (Window : not null access Gtk_Window_Record);
   --  Asks to place Window in the fullscreen state. Note that you shouldn't
   --  assume the window is definitely full screen afterward, because other
   --  entities (e.g. the user or [window manager][gtk-X11-arch]) could
   --  unfullscreen it again, and not all window managers honor requests to
   --  fullscreen windows. But normally the window will end up fullscreen. Just
   --  don't write code that crashes if not.
   --  You can track the fullscreen state via the "window-state-event" signal
   --  on Gtk.Widget.Gtk_Widget.
   --  Since: gtk+ 2.2

   procedure Fullscreen_On_Monitor
      (Window  : not null access Gtk_Window_Record;
       Screen  : not null access Gdk.Screen.Gdk_Screen_Record'Class;
       Monitor : Glib.Gint);
   --  Asks to place Window in the fullscreen state. Note that you shouldn't
   --  assume the window is definitely full screen afterward.
   --  You can track the fullscreen state via the "window-state-event" signal
   --  on Gtk.Widget.Gtk_Widget.
   --  Since: gtk+ 3.18
   --  "screen": a Gdk.Screen.Gdk_Screen to draw to
   --  "monitor": which monitor to go fullscreen on

   function Get_Accept_Focus
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Gets the value set by Gtk.Window.Set_Accept_Focus.
   --  Since: gtk+ 2.4

   procedure Set_Accept_Focus
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Windows may set a hint asking the desktop environment not to receive
   --  the input focus. This function sets this hint.
   --  Since: gtk+ 2.4
   --  "setting": True to let this window receive input focus

   function Get_Attached_To
      (Window : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Fetches the attach widget for this window. See
   --  Gtk.Window.Set_Attached_To.
   --  Since: gtk+ 3.4

   procedure Set_Attached_To
      (Window        : not null access Gtk_Window_Record;
       Attach_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Marks Window as attached to Attach_Widget. This creates a logical
   --  binding between the window and the widget it belongs to, which is used
   --  by GTK+ to propagate information such as styling or accessibility to
   --  Window as if it was a children of Attach_Widget.
   --  Examples of places where specifying this relation is useful are for
   --  instance a Gtk.Menu.Gtk_Menu created by a Gtk.Combo_Box.Gtk_Combo_Box, a
   --  completion popup window created by Gtk.GEntry.Gtk_Entry or a typeahead
   --  search entry created by Gtk.Tree_View.Gtk_Tree_View.
   --  Note that this function should not be confused with
   --  Gtk.Window.Set_Transient_For, which specifies a window manager relation
   --  between two toplevels instead.
   --  Passing null for Attach_Widget detaches the window.
   --  Since: gtk+ 3.4
   --  "attach_widget": a Gtk.Widget.Gtk_Widget, or null

   function Get_Decorated
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether the window has been set to have decorations such as a
   --  title bar via Gtk.Window.Set_Decorated.

   procedure Set_Decorated
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  By default, windows are decorated with a title bar, resize controls,
   --  etc. Some [window managers][gtk-X11-arch] allow GTK+ to disable these
   --  decorations, creating a borderless window. If you set the decorated
   --  property to False using this function, GTK+ will do its best to convince
   --  the window manager not to decorate the window. Depending on the system,
   --  this function may not have any effect when called on a window that is
   --  already visible, so you should call it before calling Gtk.Widget.Show.
   --  On Windows, this function always works, since there's no window manager
   --  policy involved.
   --  "setting": True to decorate the window

   procedure Get_Default_Size
      (Window : not null access Gtk_Window_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   --  Gets the default size of the window. A value of -1 for the width or
   --  height indicates that a default size has not been explicitly set for
   --  that dimension, so the "natural" size of the window will be used.
   --  "width": location to store the default width, or null
   --  "height": location to store the default height, or null

   procedure Set_Default_Size
      (Window : not null access Gtk_Window_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint);
   --  Sets the default size of a window. If the window's "natural" size (its
   --  size request) is larger than the default, the default will be ignored.
   --  More generally, if the default size does not obey the geometry hints for
   --  the window (gtk_window_set_geometry_hints can be used to set these
   --  explicitly), the default size will be clamped to the nearest permitted
   --  size.
   --  Unlike Gtk.Widget.Set_Size_Request, which sets a size request for a
   --  widget and thus would keep users from shrinking the window, this
   --  function only sets the initial size, just as if the user had resized the
   --  window themselves. Users can still shrink the window again as they
   --  normally would. Setting a default size of -1 means to use the "natural"
   --  default size (the size request of the window).
   --  For more control over a window's initial size and how resizing works,
   --  investigate Gtk.Window.Set_Geometry_Hints.
   --  For some uses, Gtk.Window.Resize is a more appropriate function.
   --  Gtk.Window.Resize changes the current size of the window, rather than
   --  the size to be used on initial display. Gtk.Window.Resize always affects
   --  the window itself, not the geometry widget.
   --  The default size of a window only affects the first time a window is
   --  shown; if a window is hidden and re-shown, it will remember the size it
   --  had prior to hiding, rather than using the default size.
   --  Windows can't actually be 0x0 in size, they must be at least 1x1, but
   --  passing 0 for Width and Height is OK, resulting in a 1x1 default size.
   --  If you use this function to reestablish a previously saved window size,
   --  note that the appropriate size to save is the one returned by
   --  Gtk.Window.Get_Size. Using the window allocation directly will not work
   --  in all circumstances and can lead to growing or shrinking windows.
   --  "width": width in pixels, or -1 to unset the default width
   --  "height": height in pixels, or -1 to unset the default height

   function Get_Default_Widget
      (Window : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the default widget for Window. See Gtk.Window.Set_Default for
   --  more details.
   --  Since: gtk+ 2.14

   function Get_Deletable
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether the window has been set to have a close button via
   --  Gtk.Window.Set_Deletable.
   --  Since: gtk+ 2.10

   procedure Set_Deletable
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  By default, windows have a close button in the window frame. Some
   --  [window managers][gtk-X11-arch] allow GTK+ to disable this button. If
   --  you set the deletable property to False using this function, GTK+ will
   --  do its best to convince the window manager not to show a close button.
   --  Depending on the system, this function may not have any effect when
   --  called on a window that is already visible, so you should call it before
   --  calling Gtk.Widget.Show.
   --  On Windows, this function always works, since there's no window manager
   --  policy involved.
   --  Since: gtk+ 2.10
   --  "setting": True to decorate the window as deletable

   function Get_Destroy_With_Parent
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether the window will be destroyed with its transient parent.
   --  See gtk_window_set_destroy_with_parent ().

   procedure Set_Destroy_With_Parent
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  If Setting is True, then destroying the transient parent of Window will
   --  also destroy Window itself. This is useful for dialogs that shouldn't
   --  persist beyond the lifetime of the main window they're associated with,
   --  for example.
   --  "setting": whether to destroy Window with its transient parent

   function Get_Focus
      (Window : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the current focused widget within the window. Note that this
   --  is the widget that would have the focus if the toplevel window focused;
   --  if the toplevel window is not focused then `gtk_widget_has_focus
   --  (widget)` will not be True for the widget.

   procedure Set_Focus
      (Window : not null access Gtk_Window_Record;
       Focus  : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  If Focus is not the current focus widget, and is focusable, sets it as
   --  the focus widget for the window. If Focus is null, unsets the focus
   --  widget for this window. To set the focus to a particular widget in the
   --  toplevel, it is usually more convenient to use Gtk.Widget.Grab_Focus
   --  instead of this function.
   --  "focus": widget to be the new focus widget, or null to unset any focus
   --  widget for the toplevel window.

   function Get_Focus_On_Map
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Gets the value set by Gtk.Window.Set_Focus_On_Map.
   --  Since: gtk+ 2.6

   procedure Set_Focus_On_Map
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Windows may set a hint asking the desktop environment not to receive
   --  the input focus when the window is mapped. This function sets this hint.
   --  Since: gtk+ 2.6
   --  "setting": True to let this window receive input focus on map

   function Get_Focus_Visible
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Gets the value of the Gtk.Window.Gtk_Window:focus-visible property.
   --  Since: gtk+ 3.2

   procedure Set_Focus_Visible
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Sets the Gtk.Window.Gtk_Window:focus-visible property.
   --  Since: gtk+ 3.2
   --  "setting": the new value

   function Get_Gravity
      (Window : not null access Gtk_Window_Record)
       return Gdk.Window.Gdk_Gravity;
   --  Gets the value set by Gtk.Window.Set_Gravity.

   procedure Set_Gravity
      (Window  : not null access Gtk_Window_Record;
       Gravity : Gdk.Window.Gdk_Gravity);
   --  Window gravity defines the meaning of coordinates passed to
   --  Gtk.Window.Move. See Gtk.Window.Move and Gdk.Window.Gdk_Gravity for more
   --  details.
   --  The default window gravity is GDK_GRAVITY_NORTH_WEST which will
   --  typically "do what you mean."
   --  "gravity": window gravity

   function Get_Group
      (Window : not null access Gtk_Window_Record) return Gtk_Window_Group;
   --  Returns the group for Window or the default group, if Window is null or
   --  if Window does not have an explicit window group.
   --  Since: gtk+ 2.10

   function Get_Has_Resize_Grip
      (Window : not null access Gtk_Window_Record) return Boolean;
   pragma Obsolescent (Get_Has_Resize_Grip);
   --  Determines whether the window may have a resize grip.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.14, 1

   procedure Set_Has_Resize_Grip
      (Window : not null access Gtk_Window_Record;
       Value  : Boolean);
   pragma Obsolescent (Set_Has_Resize_Grip);
   --  Sets whether Window has a corner resize grip.
   --  Note that the resize grip is only shown if the window is actually
   --  resizable and not maximized. Use Gtk.Window.Resize_Grip_Is_Visible to
   --  find out if the resize grip is currently shown.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.14, 1
   --  "value": True to allow a resize grip

   function Get_Hide_Titlebar_When_Maximized
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether the window has requested to have its titlebar hidden
   --  when maximized. See gtk_window_set_hide_titlebar_when_maximized ().
   --  Since: gtk+ 3.4

   procedure Set_Hide_Titlebar_When_Maximized
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  If Setting is True, then Window will request that it's titlebar should
   --  be hidden when maximized. This is useful for windows that don't convey
   --  any information other than the application name in the titlebar, to put
   --  the available screen space to better use. If the underlying window
   --  system does not support the request, the setting will not have any
   --  effect.
   --  Note that custom titlebars set with Gtk.Window.Set_Titlebar are not
   --  affected by this. The application is in full control of their content
   --  and visibility anyway.
   --  Since: gtk+ 3.4
   --  "setting": whether to hide the titlebar when Window is maximized

   function Get_Icon
      (Window : not null access Gtk_Window_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Gets the value set by Gtk.Window.Set_Icon (or if you've called
   --  Gtk.Window.Set_Icon_List, gets the first icon in the icon list).

   procedure Set_Icon
      (Window : not null access Gtk_Window_Record;
       Icon   : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Sets up the icon representing a Gtk.Window.Gtk_Window. This icon is
   --  used when the window is minimized (also known as iconified). Some window
   --  managers or desktop environments may also place it in the window frame,
   --  or display it in other contexts. On others, the icon is not used at all,
   --  so your mileage may vary.
   --  The icon should be provided in whatever size it was naturally drawn;
   --  that is, don't scale the image before passing it to GTK+. Scaling is
   --  postponed until the last minute, when the desired final size is known,
   --  to allow best quality.
   --  If you have your icon hand-drawn in multiple sizes, use
   --  Gtk.Window.Set_Icon_List. Then the best size will be used.
   --  This function is equivalent to calling Gtk.Window.Set_Icon_List with a
   --  1-element list.
   --  See also Gtk.Window.Set_Default_Icon_List to set the icon for all
   --  windows in your application in one go.
   --  "icon": icon image, or null

   function Get_Icon_List
      (Window : not null access Gtk_Window_Record)
       return Glib.Object.Object_Simple_List.Glist;
   --  Retrieves the list of icons set by Gtk.Window.Set_Icon_List. The list
   --  is copied, but the reference count on each member won't be incremented.

   procedure Set_Icon_List
      (Window : not null access Gtk_Window_Record;
       List   : Glib.Object.Object_Simple_List.Glist);
   --  Sets up the icon representing a Gtk.Window.Gtk_Window. The icon is used
   --  when the window is minimized (also known as iconified). Some window
   --  managers or desktop environments may also place it in the window frame,
   --  or display it in other contexts. On others, the icon is not used at all,
   --  so your mileage may vary.
   --  Gtk.Window.Set_Icon_List allows you to pass in the same icon in several
   --  hand-drawn sizes. The list should contain the natural sizes your icon is
   --  available in; that is, don't scale the image before passing it to GTK+.
   --  Scaling is postponed until the last minute, when the desired final size
   --  is known, to allow best quality.
   --  By passing several sizes, you may improve the final image quality of
   --  the icon, by reducing or eliminating automatic image scaling.
   --  Recommended sizes to provide: 16x16, 32x32, 48x48 at minimum, and
   --  larger images (64x64, 128x128) if you have them.
   --  See also Gtk.Window.Set_Default_Icon_List to set the icon for all
   --  windows in your application in one go.
   --  Note that transient windows (those who have been set transient for
   --  another window using Gtk.Window.Set_Transient_For) will inherit their
   --  icon from their transient parent. So there's no need to explicitly set
   --  the icon on transient windows.
   --  "list": list of Gdk.Pixbuf.Gdk_Pixbuf

   function Get_Icon_Name
      (Window : not null access Gtk_Window_Record) return UTF8_String;
   --  Returns the name of the themed icon for the window, see
   --  Gtk.Window.Set_Icon_Name.
   --  Since: gtk+ 2.6

   procedure Set_Icon_Name
      (Window : not null access Gtk_Window_Record;
       Name   : UTF8_String := "");
   --  Sets the icon for the window from a named themed icon. See the docs for
   --  Gtk.Icon_Theme.Gtk_Icon_Theme for more details. On some platforms, the
   --  window icon is not used at all.
   --  Note that this has nothing to do with the WM_ICON_NAME property which
   --  is mentioned in the ICCCM.
   --  Since: gtk+ 2.6
   --  "name": the name of the themed icon

   function Get_Mnemonic_Modifier
      (Window : not null access Gtk_Window_Record)
       return Gdk.Types.Gdk_Modifier_Type;
   --  Returns the mnemonic modifier for this window. See
   --  Gtk.Window.Set_Mnemonic_Modifier.

   procedure Set_Mnemonic_Modifier
      (Window   : not null access Gtk_Window_Record;
       Modifier : Gdk.Types.Gdk_Modifier_Type);
   --  Sets the mnemonic modifier for this window.
   --  "modifier": the modifier mask used to activate mnemonics on this
   --  window.

   function Get_Mnemonics_Visible
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Gets the value of the Gtk.Window.Gtk_Window:mnemonics-visible property.
   --  Since: gtk+ 2.20

   procedure Set_Mnemonics_Visible
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Sets the Gtk.Window.Gtk_Window:mnemonics-visible property.
   --  Since: gtk+ 2.20
   --  "setting": the new value

   function Get_Modal
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether the window is modal. See Gtk.Window.Set_Modal.

   procedure Set_Modal
      (Window : not null access Gtk_Window_Record;
       Modal  : Boolean := True);
   --  Sets a window modal or non-modal. Modal windows prevent interaction
   --  with other windows in the same application. To keep modal dialogs on top
   --  of main application windows, use Gtk.Window.Set_Transient_For to make
   --  the dialog transient for the parent; most [window
   --  managers][gtk-X11-arch] will then disallow lowering the dialog below the
   --  parent.
   --  "modal": whether the window is modal

   procedure Get_Position
      (Window : not null access Gtk_Window_Record;
       Root_X : out Glib.Gint;
       Root_Y : out Glib.Gint);
   --  This function returns the position you need to pass to Gtk.Window.Move
   --  to keep Window in its current position. This means that the meaning of
   --  the returned value varies with window gravity. See Gtk.Window.Move for
   --  more details.
   --  The reliability of this function depends on the windowing system
   --  currently in use. Some windowing systems, such as Wayland, do not
   --  support a global coordinate system, and thus the position of the window
   --  will always be (0, 0). Others, like X11, do not have a reliable way to
   --  obtain the geometry of the decorations of a window if they are provided
   --  by the window manager. Additionally, on X11, window manager have been
   --  known to mismanage window gravity, which result in windows moving even
   --  if you use the coordinates of the current position as returned by this
   --  function.
   --  If you haven't changed the window gravity, its gravity will be
   --  GDK_GRAVITY_NORTH_WEST. This means that Gtk.Window.Get_Position gets the
   --  position of the top-left corner of the window manager frame for the
   --  window. Gtk.Window.Move sets the position of this same top-left corner.
   --  If a window has gravity GDK_GRAVITY_STATIC the window manager frame is
   --  not relevant, and thus Gtk.Window.Get_Position will always produce
   --  accurate results. However you can't use static gravity to do things like
   --  place a window in a corner of the screen, because static gravity ignores
   --  the window manager decorations.
   --  Ideally, this function should return appropriate values if the window
   --  has client side decorations, assuming that the windowing system supports
   --  global coordinates.
   --  In practice, saving the window position should not be left to
   --  applications, as they lack enough knowledge of the windowing system and
   --  the window manager state to effectively do so. The appropriate way to
   --  implement saving the window position is to use a platform-specific
   --  protocol, wherever that is available.
   --  "root_x": return location for X coordinate of gravity-determined
   --  reference point, or null
   --  "root_y": return location for Y coordinate of gravity-determined
   --  reference point, or null

   procedure Set_Position
      (Window   : not null access Gtk_Window_Record;
       Position : Gtk.Enums.Gtk_Window_Position);
   --  Sets a position constraint for this window. If the old or new
   --  constraint is Gtk.Enums.Win_Pos_Center_Always, this will also cause the
   --  window to be repositioned to satisfy the new constraint.
   --  "position": a position constraint.

   function Get_Resizable
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Gets the value set by Gtk.Window.Set_Resizable.

   procedure Set_Resizable
      (Window    : not null access Gtk_Window_Record;
       Resizable : Boolean);
   --  Sets whether the user can resize a window. Windows are user resizable
   --  by default.
   --  "resizable": True if the user can resize this window

   procedure Get_Resize_Grip_Area
      (Window    : not null access Gtk_Window_Record;
       Rect      : out Gdk.Rectangle.Gdk_Rectangle;
       retrieved : out Boolean);
   pragma Obsolescent (Get_Resize_Grip_Area);
   --  If a window has a resize grip, this will retrieve the grip position,
   --  width and height into the specified Gdk.Rectangle.Gdk_Rectangle.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.14, 1
   --  "rect": a pointer to a Gdk.Rectangle.Gdk_Rectangle which we should
   --  store the resize grip area

   function Get_Role
      (Window : not null access Gtk_Window_Record) return UTF8_String;
   --  Returns the role of the window. See Gtk.Window.Set_Role for further
   --  explanation.

   procedure Set_Role
      (Window : not null access Gtk_Window_Record;
       Role   : UTF8_String);
   --  This function is only useful on X11, not with other GTK+ targets.
   --  In combination with the window title, the window role allows a [window
   --  manager][gtk-X11-arch] to identify "the same" window when an application
   --  is restarted. So for example you might set the "toolbox" role on your
   --  app's toolbox window, so that when the user restarts their session, the
   --  window manager can put the toolbox back in the same place.
   --  If a window already has a unique title, you don't need to set the role,
   --  since the WM can use the title to identify the window when restoring the
   --  session.
   --  "role": unique identifier for the window to be used when restoring a
   --  session

   function Get_Screen
      (Window : not null access Gtk_Window_Record)
       return Gdk.Screen.Gdk_Screen;
   --  Returns the Gdk.Screen.Gdk_Screen associated with Window.
   --  Since: gtk+ 2.2

   procedure Set_Screen
      (Window : not null access Gtk_Window_Record;
       Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class);
   --  Sets the Gdk.Screen.Gdk_Screen where the Window is displayed; if the
   --  window is already mapped, it will be unmapped, and then remapped on the
   --  new screen.
   --  Since: gtk+ 2.2
   --  "screen": a Gdk.Screen.Gdk_Screen.

   procedure Get_Size
      (Window : not null access Gtk_Window_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   --  Obtains the current size of Window.
   --  If Window is not visible on screen, this function return the size GTK+
   --  will suggest to the [window manager][gtk-X11-arch] for the initial
   --  window size (but this is not reliably the same as the size the window
   --  manager will actually select). See: Gtk.Window.Set_Default_Size.
   --  Depending on the windowing system and the window manager constraints,
   --  the size returned by this function may not match the size set using
   --  Gtk.Window.Resize; additionally, since Gtk.Window.Resize may be
   --  implemented as an asynchronous operation, GTK+ cannot guarantee in any
   --  way that this code:
   --  |[<!-- language="C" --> // width and height are set elsewhere
   --  gtk_window_resize (window, width, height);
   --  int new_width, new_height; gtk_window_get_size (window, &new_width,
   --  &new_height); ]|
   --  will result in `new_width` and `new_height` matching `width` and
   --  `height`, respectively.
   --  This function will return the logical size of the
   --  Gtk.Window.Gtk_Window, excluding the widgets used in client side
   --  decorations; there is, however, no guarantee that the result will be
   --  completely accurate because client side decoration may include widgets
   --  that depend on the user preferences and that may not be visibile at the
   --  time you call this function.
   --  The dimensions returned by this function are suitable for being stored
   --  across sessions; use Gtk.Window.Set_Default_Size to restore them when
   --  before showing the window.
   --  To avoid potential race conditions, you should only call this function
   --  in response to a size change notification, for instance inside a handler
   --  for the Gtk.Widget.Gtk_Widget::size-allocate signal, or inside a handler
   --  for the Gtk.Widget.Gtk_Widget::configure-event signal:
   --  |[<!-- language="C" --> static void on_size_allocate (GtkWidget
   --  *widget, GtkAllocation *allocation) { int new_width, new_height;
   --  gtk_window_get_size (GTK_WINDOW (widget), &new_width, &new_height);
   --  ... } ]|
   --  Note that, if you connect to the Gtk.Widget.Gtk_Widget::size-allocate
   --  signal, you should not use the dimensions of the Gtk_Allocation passed
   --  to the signal handler, as the allocation may contain client side
   --  decorations added by GTK+, depending on the windowing system in use.
   --  If you are getting a window size in order to position the window on the
   --  screen, you should, instead, simply set the window's semantic type with
   --  Gtk.Window.Set_Type_Hint, which allows the window manager to e.g. center
   --  dialogs. Also, if you set the transient parent of dialogs with
   --  Gtk.Window.Set_Transient_For window managers will often center the
   --  dialog over its parent window. It's much preferred to let the window
   --  manager handle these cases rather than doing it yourself, because all
   --  apps will behave consistently and according to user or system
   --  preferences, if the window manager handles it. Also, the window manager
   --  can take into account the size of the window decorations and border that
   --  it may add, and of which GTK+ has no knowledge. Additionally,
   --  positioning windows in global screen coordinates may not be allowed by
   --  the windowing system. For more information, see:
   --  Gtk.Window.Set_Position.
   --  "width": return location for width, or null
   --  "height": return location for height, or null

   function Get_Skip_Pager_Hint
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Gets the value set by Gtk.Window.Set_Skip_Pager_Hint.
   --  Since: gtk+ 2.2

   procedure Set_Skip_Pager_Hint
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Windows may set a hint asking the desktop environment not to display
   --  the window in the pager. This function sets this hint. (A "pager" is any
   --  desktop navigation tool such as a workspace switcher that displays a
   --  thumbnail representation of the windows on the screen.)
   --  Since: gtk+ 2.2
   --  "setting": True to keep this window from appearing in the pager

   function Get_Skip_Taskbar_Hint
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Gets the value set by Gtk.Window.Set_Skip_Taskbar_Hint
   --  Since: gtk+ 2.2

   procedure Set_Skip_Taskbar_Hint
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Windows may set a hint asking the desktop environment not to display
   --  the window in the task bar. This function sets this hint.
   --  Since: gtk+ 2.2
   --  "setting": True to keep this window from appearing in the task bar

   function Get_Title
      (Window : not null access Gtk_Window_Record) return UTF8_String;
   --  Retrieves the title of the window. See Gtk.Window.Set_Title.

   procedure Set_Title
      (Window : not null access Gtk_Window_Record;
       Title  : UTF8_String);
   --  Sets the title of the Gtk.Window.Gtk_Window. The title of a window will
   --  be displayed in its title bar; on the X Window System, the title bar is
   --  rendered by the [window manager][gtk-X11-arch], so exactly how the title
   --  appears to users may vary according to a user's exact configuration. The
   --  title should help a user distinguish this window from other windows they
   --  may have open. A good title might include the application name and
   --  current document filename, for example.
   --  "title": title of the window

   function Get_Titlebar
      (Window : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the custom titlebar that has been set with
   --  Gtk.Window.Set_Titlebar.
   --  Since: gtk+ 3.16

   procedure Set_Titlebar
      (Window   : not null access Gtk_Window_Record;
       Titlebar : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets a custom titlebar for Window.
   --  A typical widget used here is Gtk.Header_Bar.Gtk_Header_Bar, as it
   --  provides various features expected of a titlebar while allowing the
   --  addition of child widgets to it.
   --  If you set a custom titlebar, GTK+ will do its best to convince the
   --  window manager not to put its own titlebar on the window. Depending on
   --  the system, this function may not work for a window that is already
   --  visible, so you set the titlebar before calling Gtk.Widget.Show.
   --  Since: gtk+ 3.10
   --  "titlebar": the widget to use as titlebar

   function Get_Transient_For
      (Window : not null access Gtk_Window_Record) return Gtk_Window;
   --  Fetches the transient parent for this window. See
   --  Gtk.Window.Set_Transient_For.

   procedure Set_Transient_For
      (Window : not null access Gtk_Window_Record;
       Parent : access Gtk_Window_Record'Class);
   --  Dialog windows should be set transient for the main application window
   --  they were spawned from. This allows [window managers][gtk-X11-arch] to
   --  e.g. keep the dialog on top of the main window, or center the dialog
   --  over the main window. gtk_dialog_new_with_buttons and other convenience
   --  functions in GTK+ will sometimes call Gtk.Window.Set_Transient_For on
   --  your behalf.
   --  Passing null for Parent unsets the current transient window.
   --  On Wayland, this function can also be used to attach a new
   --  GTK_WINDOW_POPUP to a GTK_WINDOW_TOPLEVEL parent already mapped on
   --  screen so that the GTK_WINDOW_POPUP will be created as a
   --  subsurface-based window GDK_WINDOW_SUBSURFACE which can be positioned at
   --  will relatively to the GTK_WINDOW_TOPLEVEL surface.
   --  On Windows, this function puts the child window on top of the parent,
   --  much as the window manager would have done on X.
   --  "parent": parent window, or null

   function Get_Type_Hint
      (Window : not null access Gtk_Window_Record)
       return Gdk.Window.Gdk_Window_Type_Hint;
   --  Gets the type hint for this window. See Gtk.Window.Set_Type_Hint.

   procedure Set_Type_Hint
      (Window : not null access Gtk_Window_Record;
       Hint   : Gdk.Window.Gdk_Window_Type_Hint);
   --  By setting the type hint for the window, you allow the window manager
   --  to decorate and handle the window in a way which is suitable to the
   --  function of the window in your application.
   --  This function should be called before the window becomes visible.
   --  gtk_dialog_new_with_buttons and other convenience functions in GTK+
   --  will sometimes call Gtk.Window.Set_Type_Hint on your behalf.
   --  "hint": the window type

   function Get_Urgency_Hint
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Gets the value set by Gtk.Window.Set_Urgency_Hint
   --  Since: gtk+ 2.8

   procedure Set_Urgency_Hint
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Windows may set a hint asking the desktop environment to draw the users
   --  attention to the window. This function sets this hint.
   --  Since: gtk+ 2.8
   --  "setting": True to mark this window as urgent

   function Get_Window_Type
      (Window : not null access Gtk_Window_Record)
       return Gtk.Enums.Gtk_Window_Type;
   --  Gets the type of the window. See Gtk.Enums.Gtk_Window_Type.
   --  Since: gtk+ 2.20

   function Has_Group
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether Window has an explicit window group.

   function Has_Toplevel_Focus
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether the input focus is within this GtkWindow. For real
   --  toplevel windows, this is identical to Gtk.Window.Is_Active, but for
   --  embedded windows, like Gtk.Plug.Gtk_Plug, the results will differ.
   --  Since: gtk+ 2.4

   procedure Iconify (Window : not null access Gtk_Window_Record);
   --  Asks to iconify (i.e. minimize) the specified Window. Note that you
   --  shouldn't assume the window is definitely iconified afterward, because
   --  other entities (e.g. the user or [window manager][gtk-X11-arch]) could
   --  deiconify it again, or there may not be a window manager in which case
   --  iconification isn't possible, etc. But normally the window will end up
   --  iconified. Just don't write code that crashes if not.
   --  It's permitted to call this function before showing a window, in which
   --  case the window will be iconified before it ever appears onscreen.
   --  You can track iconification via the "window-state-event" signal on
   --  Gtk.Widget.Gtk_Widget.

   function Is_Active
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether the window is part of the current active toplevel.
   --  (That is, the toplevel window receiving keystrokes.) The return value is
   --  True if the window is active toplevel itself, but also if it is, say, a
   --  Gtk.Plug.Gtk_Plug embedded in the active toplevel. You might use this
   --  function if you wanted to draw a widget differently in an active window
   --  from a widget in an inactive window. See Gtk.Window.Has_Toplevel_Focus
   --  Since: gtk+ 2.4

   function Is_Maximized
      (Window : not null access Gtk_Window_Record) return Boolean;
   --  Retrieves the current maximized state of Window.
   --  Note that since maximization is ultimately handled by the window
   --  manager and happens asynchronously to an application request, you
   --  shouldn't assume the return value of this function changing immediately
   --  (or at all), as an effect of calling Gtk.Window.Maximize or
   --  Gtk.Window.Unmaximize.
   --  Since: gtk+ 3.12

   procedure Maximize (Window : not null access Gtk_Window_Record);
   --  Asks to maximize Window, so that it becomes full-screen. Note that you
   --  shouldn't assume the window is definitely maximized afterward, because
   --  other entities (e.g. the user or [window manager][gtk-X11-arch]) could
   --  unmaximize it again, and not all window managers support maximization.
   --  But normally the window will end up maximized. Just don't write code
   --  that crashes if not.
   --  It's permitted to call this function before showing a window, in which
   --  case the window will be maximized when it appears onscreen initially.
   --  You can track maximization via the "window-state-event" signal on
   --  Gtk.Widget.Gtk_Widget, or by listening to notifications on the
   --  Gtk.Window.Gtk_Window:is-maximized property.

   function Mnemonic_Activate
      (Window   : not null access Gtk_Window_Record;
       Keyval   : Gdk.Types.Gdk_Key_Type;
       Modifier : Gdk.Types.Gdk_Modifier_Type) return Boolean;
   --  Activates the targets associated with the mnemonic.
   --  "keyval": the mnemonic
   --  "modifier": the modifiers

   procedure Move
      (Window : not null access Gtk_Window_Record;
       X      : Glib.Gint;
       Y      : Glib.Gint);
   --  Asks the [window manager][gtk-X11-arch] to move Window to the given
   --  position. Window managers are free to ignore this; most window managers
   --  ignore requests for initial window positions (instead using a
   --  user-defined placement algorithm) and honor requests after the window
   --  has already been shown.
   --  Note: the position is the position of the gravity-determined reference
   --  point for the window. The gravity determines two things: first, the
   --  location of the reference point in root window coordinates; and second,
   --  which point on the window is positioned at the reference point.
   --  By default the gravity is GDK_GRAVITY_NORTH_WEST, so the reference
   --  point is simply the X, Y supplied to Gtk.Window.Move. The top-left
   --  corner of the window decorations (aka window frame or border) will be
   --  placed at X, Y. Therefore, to position a window at the top left of the
   --  screen, you want to use the default gravity (which is
   --  GDK_GRAVITY_NORTH_WEST) and move the window to 0,0.
   --  To position a window at the bottom right corner of the screen, you
   --  would set GDK_GRAVITY_SOUTH_EAST, which means that the reference point
   --  is at X + the window width and Y + the window height, and the
   --  bottom-right corner of the window border will be placed at that
   --  reference point. So, to place a window in the bottom right corner you
   --  would first set gravity to south east, then write: `gtk_window_move
   --  (window, gdk_screen_width () - window_width, gdk_screen_height () -
   --  window_height)` (note that this example does not take multi-head
   --  scenarios into account).
   --  The [Extended Window Manager Hints
   --  Specification](http://www.freedesktop.org/Standards/wm-spec) has a nice
   --  table of gravities in the "implementation notes" section.
   --  The Gtk.Window.Get_Position documentation may also be relevant.
   --  "x": X coordinate to move window to
   --  "y": Y coordinate to move window to

   function Parse_Geometry
      (Window   : not null access Gtk_Window_Record;
       Geometry : UTF8_String) return Boolean;
   pragma Obsolescent (Parse_Geometry);
   --  Parses a standard X Window System geometry string - see the manual page
   --  for X (type "man X") for details on this. Gtk.Window.Parse_Geometry does
   --  work on all GTK+ ports including Win32 but is primarily intended for an
   --  X environment.
   --  If either a size or a position can be extracted from the geometry
   --  string, Gtk.Window.Parse_Geometry returns True and calls
   --  Gtk.Window.Set_Default_Size and/or Gtk.Window.Move to resize/move the
   --  window.
   --  If Gtk.Window.Parse_Geometry returns True, it will also set the
   --  GDK_HINT_USER_POS and/or GDK_HINT_USER_SIZE hints indicating to the
   --  window manager that the size/position of the window was user-specified.
   --  This causes most window managers to honor the geometry.
   --  Note that for Gtk.Window.Parse_Geometry to work as expected, it has to
   --  be called when the window has its "final" size, i.e. after calling
   --  Gtk.Widget.Show_All on the contents and Gtk.Window.Set_Geometry_Hints on
   --  the window. |[<!-- language="C" --> include <gtk/gtk.h>
   --  static void fill_with_content (GtkWidget *vbox) { // fill with
   --  content... }
   --  int main (int argc, char *argv[]) { GtkWidget *window, *vbox;
   --  GdkGeometry size_hints = { 100, 50, 0, 0, 100, 50, 10, 10, 0.0, 0.0,
   --  GDK_GRAVITY_NORTH_WEST };
   --  gtk_init (&argc, &argv);
   --  window = gtk_window_new (GTK_WINDOW_TOPLEVEL); vbox = gtk_box_new
   --  (GTK_ORIENTATION_VERTICAL, 0);
   --  gtk_container_add (GTK_CONTAINER (window), vbox); fill_with_content
   --  (vbox); gtk_widget_show_all (vbox);
   --  gtk_window_set_geometry_hints (GTK_WINDOW (window), NULL, &size_hints,
   --  GDK_HINT_MIN_SIZE | GDK_HINT_BASE_SIZE | GDK_HINT_RESIZE_INC);
   --  if (argc > 1) { gboolean res; res = gtk_window_parse_geometry
   --  (GTK_WINDOW (window), argv[1]); if (! res) fprintf (stderr, "Failed to
   --  parse "%s"\n", argv[1]); }
   --  gtk_widget_show_all (window); gtk_main ();
   --  return 0; } ]|
   --  Deprecated since 3.20, 1
   --  "geometry": geometry string

   procedure Present (Window : not null access Gtk_Window_Record);
   --  Presents a window to the user. This function should not be used as when
   --  it is called, it is too late to gather a valid timestamp to allow focus
   --  stealing prevention to work correctly.

   procedure Present_With_Time
      (Window    : not null access Gtk_Window_Record;
       Timestamp : Guint32);
   --  Presents a window to the user. This may mean raising the window in the
   --  stacking order, deiconifying it, moving it to the current desktop,
   --  and/or giving it the keyboard focus, possibly dependent on the user's
   --  platform, window manager, and preferences.
   --  If Window is hidden, this function calls Gtk.Widget.Show as well.
   --  This function should be used when the user tries to open a window
   --  that's already open. Say for example the preferences dialog is currently
   --  open, and the user chooses Preferences from the menu a second time; use
   --  Gtk.Window.Present to move the already-open dialog where the user can
   --  see it.
   --  Presents a window to the user in response to a user interaction. The
   --  timestamp should be gathered when the window was requested to be shown
   --  (when clicking a link for example), rather than once the window is ready
   --  to be shown.
   --  Since: gtk+ 2.8
   --  "timestamp": the timestamp of the user interaction (typically a button
   --  or key press event) which triggered this call

   function Propagate_Key_Event
      (Window : not null access Gtk_Window_Record;
       Event  : Gdk.Event.Gdk_Event_Key) return Boolean;
   --  Propagate a key press or release event to the focus widget and up the
   --  focus container chain until a widget handles Event. This is normally
   --  called by the default ::key_press_event and ::key_release_event handlers
   --  for toplevel windows, however in some cases it may be useful to call
   --  this directly when overriding the standard key handling for a toplevel
   --  window.
   --  Since: gtk+ 2.4
   --  "event": a Gdk.Event.Gdk_Event_Key

   procedure Remove_Accel_Group
      (Window      : not null access Gtk_Window_Record;
       Accel_Group : not null access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class);
   --  Reverses the effects of Gtk.Window.Add_Accel_Group.
   --  "accel_group": a Gtk.Accel_Group.Gtk_Accel_Group

   procedure Remove_Mnemonic
      (Window : not null access Gtk_Window_Record;
       Keyval : Gdk.Types.Gdk_Key_Type;
       Target : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a mnemonic from this window.
   --  "keyval": the mnemonic
   --  "target": the widget that gets activated by the mnemonic

   procedure Reshow_With_Initial_Size
      (Window : not null access Gtk_Window_Record);
   pragma Obsolescent (Reshow_With_Initial_Size);
   --  Hides Window, then reshows it, resetting the default size and position
   --  of the window. Used by GUI builders only.
   --  Deprecated since 3.10, 1

   procedure Resize
      (Window : not null access Gtk_Window_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint);
   --  Resizes the window as if the user had done so, obeying geometry
   --  constraints. The default geometry constraint is that windows may not be
   --  smaller than their size request; to override this constraint, call
   --  Gtk.Widget.Set_Size_Request to set the window's request to a smaller
   --  value.
   --  If Gtk.Window.Resize is called before showing a window for the first
   --  time, it overrides any default size set with
   --  Gtk.Window.Set_Default_Size.
   --  Windows may not be resized smaller than 1 by 1 pixels.
   --  When using client side decorations, GTK+ will do its best to adjust the
   --  given size so that the resulting window size matches the requested size
   --  without the title bar, borders and shadows added for the client side
   --  decorations, but there is no guarantee that the result will be totally
   --  accurate because these widgets added for client side decorations depend
   --  on the theme and may not be realized or visible at the time
   --  Gtk.Window.Resize is issued.
   --  If the GtkWindow has a titlebar widget (see Gtk.Window.Set_Titlebar),
   --  then typically, Gtk.Window.Resize will compensate for the height of the
   --  titlebar widget only if the height is known when the resulting GtkWindow
   --  configuration is issued. For example, if new widgets are added after the
   --  GtkWindow configuration and cause the titlebar widget to grow in height,
   --  this will result in a window content smaller that specified by
   --  Gtk.Window.Resize and not a larger window.
   --  "width": width in pixels to resize the window to
   --  "height": height in pixels to resize the window to

   function Resize_Grip_Is_Visible
      (Window : not null access Gtk_Window_Record) return Boolean;
   pragma Obsolescent (Resize_Grip_Is_Visible);
   --  Determines whether a resize grip is visible for the specified window.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.14, 1

   procedure Resize_To_Geometry
      (Window : not null access Gtk_Window_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint);
   pragma Obsolescent (Resize_To_Geometry);
   --  Like Gtk.Window.Resize, but Width and Height are interpreted in terms
   --  of the base size and increment set with gtk_window_set_geometry_hints.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.20, 1
   --  "width": width in resize increments to resize the window to
   --  "height": height in resize increments to resize the window to

   procedure Set_Default
      (Window         : not null access Gtk_Window_Record;
       Default_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  The default widget is the widget that's activated when the user presses
   --  Enter in a dialog (for example). This function sets or unsets the
   --  default widget for a Gtk.Window.Gtk_Window. When setting (rather than
   --  unsetting) the default widget it's generally easier to call
   --  Gtk.Widget.Grab_Default on the widget. Before making a widget the
   --  default widget, you must call Gtk.Widget.Set_Can_Default on the widget
   --  you'd like to make the default.
   --  "default_widget": widget to be the default, or null to unset the
   --  default widget for the toplevel

   procedure Set_Default_Geometry
      (Window : not null access Gtk_Window_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint);
   pragma Obsolescent (Set_Default_Geometry);
   --  Like Gtk.Window.Set_Default_Size, but Width and Height are interpreted
   --  in terms of the base size and increment set with
   --  gtk_window_set_geometry_hints.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.20, 1
   --  "width": width in resize increments, or -1 to unset the default width
   --  "height": height in resize increments, or -1 to unset the default
   --  height

   procedure Set_Geometry_Hints
      (Window          : not null access Gtk_Window_Record;
       Geometry_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       Geometry        : Gdk.Window.Gdk_Geometry;
       Geom_Mask       : Gdk.Window.Gdk_Window_Hints);
   --  This function sets up hints about how a window can be resized by the
   --  user. You can set a minimum and maximum size; allowed resize increments
   --  (e.g. for xterm, you can only resize by the size of a character); aspect
   --  ratios; and more. See the Gdk.Window.Gdk_Geometry struct.
   --  "geometry_widget": widget the geometry hints used to be applied to or
   --  null. Since 3.20 this argument is ignored and GTK behaves as if null was
   --  set.
   --  "geometry": struct containing geometry information or null
   --  "geom_mask": mask indicating which struct fields should be paid
   --  attention to

   procedure Set_Has_User_Ref_Count
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Tells GTK+ whether to drop its extra reference to the window when
   --  Gtk.Widget.Destroy is called.
   --  This function is only exported for the benefit of language bindings
   --  which may need to keep the window alive until their wrapper object is
   --  garbage collected. There is no justification for ever calling this
   --  function in an application.
   --  Since: gtk+ 3.0
   --  "setting": the new value

   function Set_Icon_From_File
      (Window   : not null access Gtk_Window_Record;
       Filename : UTF8_String) return Boolean;
   --  Sets the icon for Window. Warns on failure if Err is null.
   --  This function is equivalent to calling Gtk.Window.Set_Icon with a
   --  pixbuf created by loading the image from Filename.
   --  Since: gtk+ 2.2
   --  "filename": location of icon file

   procedure Set_Keep_Above
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Asks to keep Window above, so that it stays on top. Note that you
   --  shouldn't assume the window is definitely above afterward, because other
   --  entities (e.g. the user or [window manager][gtk-X11-arch]) could not
   --  keep it above, and not all window managers support keeping windows
   --  above. But normally the window will end kept above. Just don't write
   --  code that crashes if not.
   --  It's permitted to call this function before showing a window, in which
   --  case the window will be kept above when it appears onscreen initially.
   --  You can track the above state via the "window-state-event" signal on
   --  Gtk.Widget.Gtk_Widget.
   --  Note that, according to the [Extended Window Manager Hints
   --  Specification](http://www.freedesktop.org/Standards/wm-spec), the above
   --  state is mainly meant for user preferences and should not be used by
   --  applications e.g. for drawing attention to their dialogs.
   --  Since: gtk+ 2.4
   --  "setting": whether to keep Window above other windows

   procedure Set_Keep_Below
      (Window  : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Asks to keep Window below, so that it stays in bottom. Note that you
   --  shouldn't assume the window is definitely below afterward, because other
   --  entities (e.g. the user or [window manager][gtk-X11-arch]) could not
   --  keep it below, and not all window managers support putting windows
   --  below. But normally the window will be kept below. Just don't write code
   --  that crashes if not.
   --  It's permitted to call this function before showing a window, in which
   --  case the window will be kept below when it appears onscreen initially.
   --  You can track the below state via the "window-state-event" signal on
   --  Gtk.Widget.Gtk_Widget.
   --  Note that, according to the [Extended Window Manager Hints
   --  Specification](http://www.freedesktop.org/Standards/wm-spec), the above
   --  state is mainly meant for user preferences and should not be used by
   --  applications e.g. for drawing attention to their dialogs.
   --  Since: gtk+ 2.4
   --  "setting": whether to keep Window below other windows

   procedure Set_Startup_Id
      (Window     : not null access Gtk_Window_Record;
       Startup_Id : UTF8_String);
   --  Startup notification identifiers are used by desktop environment to
   --  track application startup, to provide user feedback and other features.
   --  This function changes the corresponding property on the underlying
   --  GdkWindow. Normally, startup identifier is managed automatically and you
   --  should only use this function in special cases like transferring focus
   --  from other processes. You should use this function before calling
   --  Gtk.Window.Present or any equivalent function generating a window map
   --  event.
   --  This function is only useful on X11, not with other GTK+ targets.
   --  Since: gtk+ 2.12
   --  "startup_id": a string with startup-notification identifier

   procedure Set_Wmclass
      (Window        : not null access Gtk_Window_Record;
       Wmclass_Name  : UTF8_String;
       Wmclass_Class : UTF8_String);
   pragma Obsolescent (Set_Wmclass);
   --  Don't use this function. It sets the X Window System "class" and "name"
   --  hints for a window. According to the ICCCM, you should always set these
   --  to the same value for all windows in an application, and GTK+ sets them
   --  to that value by default, so calling this function is sort of pointless.
   --  However, you may want to call Gtk.Window.Set_Role on each window in your
   --  application, for the benefit of the session manager. Setting the role
   --  allows the window manager to restore window positions when loading a
   --  saved session.
   --  Deprecated since 3.22, 1
   --  "wmclass_name": window name hint
   --  "wmclass_class": window class hint

   procedure Stick (Window : not null access Gtk_Window_Record);
   --  Asks to stick Window, which means that it will appear on all user
   --  desktops. Note that you shouldn't assume the window is definitely stuck
   --  afterward, because other entities (e.g. the user or [window
   --  manager][gtk-X11-arch] could unstick it again, and some window managers
   --  do not support sticking windows. But normally the window will end up
   --  stuck. Just don't write code that crashes if not.
   --  It's permitted to call this function before showing a window.
   --  You can track stickiness via the "window-state-event" signal on
   --  Gtk.Widget.Gtk_Widget.

   procedure Unfullscreen (Window : not null access Gtk_Window_Record);
   --  Asks to toggle off the fullscreen state for Window. Note that you
   --  shouldn't assume the window is definitely not full screen afterward,
   --  because other entities (e.g. the user or [window manager][gtk-X11-arch])
   --  could fullscreen it again, and not all window managers honor requests to
   --  unfullscreen windows. But normally the window will end up restored to
   --  its normal state. Just don't write code that crashes if not.
   --  You can track the fullscreen state via the "window-state-event" signal
   --  on Gtk.Widget.Gtk_Widget.
   --  Since: gtk+ 2.2

   procedure Unmaximize (Window : not null access Gtk_Window_Record);
   --  Asks to unmaximize Window. Note that you shouldn't assume the window is
   --  definitely unmaximized afterward, because other entities (e.g. the user
   --  or [window manager][gtk-X11-arch]) could maximize it again, and not all
   --  window managers honor requests to unmaximize. But normally the window
   --  will end up unmaximized. Just don't write code that crashes if not.
   --  You can track maximization via the "window-state-event" signal on
   --  Gtk.Widget.Gtk_Widget.

   procedure Unstick (Window : not null access Gtk_Window_Record);
   --  Asks to unstick Window, which means that it will appear on only one of
   --  the user's desktops. Note that you shouldn't assume the window is
   --  definitely unstuck afterward, because other entities (e.g. the user or
   --  [window manager][gtk-X11-arch]) could stick it again. But normally the
   --  window will end up stuck. Just don't write code that crashes if not.
   --  You can track stickiness via the "window-state-event" signal on
   --  Gtk.Widget.Gtk_Widget.

   procedure Add_Window
      (Window_Group : not null access Gtk_Window_Group_Record;
       Window       : not null access Gtk_Window_Record'Class);
   --  Adds a window to a Gtk.Window.Gtk_Window_Group.
   --  "window": the Gtk.Window.Gtk_Window to add

   function Get_Current_Device_Grab
      (Window_Group : not null access Gtk_Window_Group_Record;
       Device       : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the current grab widget for Device, or null if none.
   --  Since: gtk+ 3.0
   --  "device": a Gdk.Device.Gdk_Device

   function Get_Current_Grab
      (Window_Group : not null access Gtk_Window_Group_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the current grab widget of the given group, see
   --  Gtk.Widget.Grab_Add.
   --  Since: gtk+ 2.22

   function List_Windows
      (Window_Group : not null access Gtk_Window_Group_Record)
       return Gtk.Widget.Widget_List.Glist;
   --  Returns a list of the Gtk_Windows that belong to Window_Group.
   --  Since: gtk+ 2.14

   procedure Remove_Window
      (Window_Group : not null access Gtk_Window_Group_Record;
       Window       : not null access Gtk_Window_Record'Class);
   --  Removes a window from a Gtk.Window.Gtk_Window_Group.
   --  "window": the Gtk.Window.Gtk_Window to remove

   ---------------
   -- Functions --
   ---------------

   function Get_Default_Icon_List return Glib.Object.Object_Simple_List.Glist;
   --  Gets the value set by Gtk.Window.Set_Default_Icon_List. The list is a
   --  copy and should be freed with g_list_free, but the pixbufs in the list
   --  have not had their reference count incremented.

   procedure Set_Default_Icon_List
      (List : Glib.Object.Object_Simple_List.Glist);
   --  Sets an icon list to be used as fallback for windows that haven't had
   --  Gtk.Window.Set_Icon_List called on them to set up a window-specific icon
   --  list. This function allows you to set up the icon for all windows in
   --  your app at once.
   --  See Gtk.Window.Set_Icon_List for more details.
   --  "list": a list of Gdk.Pixbuf.Gdk_Pixbuf

   function Get_Default_Icon_Name return UTF8_String;
   --  Returns the fallback icon name for windows that has been set with
   --  Gtk.Window.Set_Default_Icon_Name. The returned string is owned by GTK+
   --  and should not be modified. It is only valid until the next call to
   --  Gtk.Window.Set_Default_Icon_Name.
   --  Since: gtk+ 2.16

   procedure Set_Default_Icon_Name (Name : UTF8_String);
   --  Sets an icon to be used as fallback for windows that haven't had
   --  Gtk.Window.Set_Icon_List called on them from a named themed icon, see
   --  Gtk.Window.Set_Icon_Name.
   --  Since: gtk+ 2.6
   --  "name": the name of the themed icon

   function List_Toplevels return Gtk.Widget.Widget_List.Glist;
   --  Returns a list of all existing toplevel windows. The widgets in the
   --  list are not individually referenced. If you want to iterate through the
   --  list and perform actions involving callbacks that might destroy the
   --  widgets, you must call `g_list_foreach (result, (GFunc)g_object_ref,
   --  NULL)` first, and then unref all the widgets afterwards.

   procedure Set_Auto_Startup_Notification (Setting : Boolean);
   --  By default, after showing the first Gtk.Window.Gtk_Window, GTK+ calls
   --  gdk_notify_startup_complete. Call this function to disable the automatic
   --  startup notification. You might do this if your first window is a splash
   --  screen, and you want to delay notification until after your real main
   --  window has been shown, for example.
   --  In that example, you would disable startup notification temporarily,
   --  show your splash screen, then re-enable it so that showing the main
   --  window would automatically result in notification.
   --  Since: gtk+ 2.2
   --  "setting": True to automatically do startup notification

   procedure Set_Default_Icon
      (Icon : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Sets an icon to be used as fallback for windows that haven't had
   --  Gtk.Window.Set_Icon called on them from a pixbuf.
   --  Since: gtk+ 2.4
   --  "icon": the icon

   function Set_Default_Icon_From_File
      (Filename : UTF8_String) return Boolean;
   --  Sets an icon to be used as fallback for windows that haven't had
   --  Gtk.Window.Set_Icon_List called on them from a file on disk. Warns on
   --  failure if Err is null.
   --  Since: gtk+ 2.2
   --  "filename": location of icon file

   procedure Set_Interactive_Debugging (Enable : Boolean);
   --  Opens or closes the [interactive debugger][interactive-debugging],
   --  which offers access to the widget hierarchy of the application and to
   --  useful debugging tools.
   --  Since: gtk+ 3.14
   --  "enable": True to enable interactive debugging

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Accept_Focus_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the window should receive the input focus.

   Application_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Application.Gtk_Application
   --  The Gtk.Application.Gtk_Application associated with the window.
   --
   --  The application will be kept alive for at least as long as it has any
   --  windows associated with it (see Glib.Application.Hold for a way to keep
   --  it alive without windows).
   --
   --  Normally, the connection between the application and the window will
   --  remain until the window is destroyed, but you can explicitly remove it
   --  by setting the :application property to null.

   Attached_To_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The widget to which this window is attached. See
   --  Gtk.Window.Set_Attached_To.
   --
   --  Examples of places where specifying this relation is useful are for
   --  instance a Gtk.Menu.Gtk_Menu created by a Gtk.Combo_Box.Gtk_Combo_Box, a
   --  completion popup window created by Gtk.GEntry.Gtk_Entry or a typeahead
   --  search entry created by Gtk.Tree_View.Gtk_Tree_View.

   Decorated_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the window should be decorated by the window manager.

   Default_Height_Property : constant Glib.Properties.Property_Int;

   Default_Width_Property : constant Glib.Properties.Property_Int;

   Deletable_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the window frame should have a close button.

   Destroy_With_Parent_Property : constant Glib.Properties.Property_Boolean;

   Focus_On_Map_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the window should receive the input focus when mapped.

   Focus_Visible_Property : constant Glib.Properties.Property_Boolean;
   --  Whether 'focus rectangles' are currently visible in this window.
   --
   --  This property is maintained by GTK+ based on user input and should not
   --  be set by applications.

   Gravity_Property : constant Gdk.Window.Property_Gdk_Gravity;
   --  Type: Gdk.Window.Gdk_Gravity
   --  The window gravity of the window. See Gtk.Window.Move and
   --  Gdk.Window.Gdk_Gravity for more details about window gravity.

   Has_Resize_Grip_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the window has a corner resize grip.
   --
   --  Note that the resize grip is only shown if the window is actually
   --  resizable and not maximized. Use
   --  Gtk.Window.Gtk_Window:resize-grip-visible to find out if the resize grip
   --  is currently shown.

   Has_Toplevel_Focus_Property : constant Glib.Properties.Property_Boolean;

   Hide_Titlebar_When_Maximized_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the titlebar should be hidden during maximization.

   Icon_Property : constant Glib.Properties.Property_Object;
   --  Type: Gdk.Pixbuf.Gdk_Pixbuf

   Icon_Name_Property : constant Glib.Properties.Property_String;
   --  The :icon-name property specifies the name of the themed icon to use as
   --  the window icon. See Gtk.Icon_Theme.Gtk_Icon_Theme for more details.

   Is_Active_Property : constant Glib.Properties.Property_Boolean;

   Is_Maximized_Property : constant Glib.Properties.Property_Boolean;

   Mnemonics_Visible_Property : constant Glib.Properties.Property_Boolean;
   --  Whether mnemonics are currently visible in this window.
   --
   --  This property is maintained by GTK+ based on user input, and should not
   --  be set by applications.

   Modal_Property : constant Glib.Properties.Property_Boolean;

   Resizable_Property : constant Glib.Properties.Property_Boolean;

   Resize_Grip_Visible_Property : constant Glib.Properties.Property_Boolean;
   --  Whether a corner resize grip is currently shown.

   Role_Property : constant Glib.Properties.Property_String;

   Screen_Property : constant Glib.Properties.Property_Object;
   --  Type: Gdk.Screen.Gdk_Screen

   Skip_Pager_Hint_Property : constant Glib.Properties.Property_Boolean;

   Skip_Taskbar_Hint_Property : constant Glib.Properties.Property_Boolean;

   Startup_Id_Property : constant Glib.Properties.Property_String;
   --  Flags: write
   --  The :startup-id is a write-only property for setting window's startup
   --  notification identifier. See Gtk.Window.Set_Startup_Id for more details.

   The_Type_Property : constant Gtk.Enums.Property_Gtk_Window_Type;

   Title_Property : constant Glib.Properties.Property_String;

   Transient_For_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk_Window
   --  The transient parent of the window. See Gtk.Window.Set_Transient_For
   --  for more details about transient windows.

   Type_Hint_Property : constant Gdk.Window.Property_Gdk_Window_Type_Hint;
   --  Type: Gdk.Window.Gdk_Window_Type_Hint

   Urgency_Hint_Property : constant Glib.Properties.Property_Boolean;

   Window_Position_Property : constant Gtk.Enums.Property_Gtk_Window_Position;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Window_Void is not null access procedure (Self : access Gtk_Window_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate_Default : constant Glib.Signal_Name := "activate-default";
   procedure On_Activate_Default
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_Gtk_Window_Void;
       After : Boolean := False);
   procedure On_Activate_Default
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::activate-default signal is a [keybinding
   --  signal][GtkBindingSignal] which gets emitted when the user activates the
   --  default widget of Window.

   Signal_Activate_Focus : constant Glib.Signal_Name := "activate-focus";
   procedure On_Activate_Focus
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_Gtk_Window_Void;
       After : Boolean := False);
   procedure On_Activate_Focus
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::activate-focus signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user activates the currently focused widget
   --  of Window.

   type Cb_Gtk_Window_Boolean_Boolean is not null access function
     (Self   : access Gtk_Window_Record'Class;
      Toggle : Boolean) return Boolean;

   type Cb_GObject_Boolean_Boolean is not null access function
     (Self   : access Glib.Object.GObject_Record'Class;
      Toggle : Boolean) return Boolean;

   Signal_Enable_Debugging : constant Glib.Signal_Name := "enable-debugging";
   procedure On_Enable_Debugging
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_Gtk_Window_Boolean_Boolean;
       After : Boolean := False);
   procedure On_Enable_Debugging
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::enable-debugging signal is a [keybinding
   --  signal][GtkBindingSignal] which gets emitted when the user enables or
   --  disables interactive debugging. When Toggle is True, interactive
   --  debugging is toggled on or off, when it is False, the debugger will be
   --  pointed at the widget under the pointer.
   --
   --  The default bindings for this signal are Ctrl-Shift-I and Ctrl-Shift-D.
   -- 
   --  Callback parameters:
   --    --  "toggle": toggle the debugger
   --    --  Returns True if the key binding was handled

   Signal_Keys_Changed : constant Glib.Signal_Name := "keys-changed";
   procedure On_Keys_Changed
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_Gtk_Window_Void;
       After : Boolean := False);
   procedure On_Keys_Changed
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::keys-changed signal gets emitted when the set of accelerators or
   --  mnemonics that are associated with Window changes.

   type Cb_Gtk_Window_Gtk_Widget_Void is not null access procedure
     (Self   : access Gtk_Window_Record'Class;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   type Cb_GObject_Gtk_Widget_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   Signal_Set_Focus : constant Glib.Signal_Name := "set-focus";
   procedure On_Set_Focus
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_Gtk_Window_Gtk_Widget_Void;
       After : Boolean := False);
   procedure On_Set_Focus
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted whenever the currently focused widget in this
   --  window changes.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Window_Record, Gtk_Window);
   function "+"
     (Widget : access Gtk_Window_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Window
   renames Implements_Gtk_Buildable.To_Object;

private
   Window_Position_Property : constant Gtk.Enums.Property_Gtk_Window_Position :=
     Gtk.Enums.Build ("window-position");
   Urgency_Hint_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("urgency-hint");
   Type_Hint_Property : constant Gdk.Window.Property_Gdk_Window_Type_Hint :=
     Gdk.Window.Build ("type-hint");
   Transient_For_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("transient-for");
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   The_Type_Property : constant Gtk.Enums.Property_Gtk_Window_Type :=
     Gtk.Enums.Build ("type");
   Startup_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("startup-id");
   Skip_Taskbar_Hint_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("skip-taskbar-hint");
   Skip_Pager_Hint_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("skip-pager-hint");
   Screen_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("screen");
   Role_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("role");
   Resize_Grip_Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("resize-grip-visible");
   Resizable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("resizable");
   Modal_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("modal");
   Mnemonics_Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("mnemonics-visible");
   Is_Maximized_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-maximized");
   Is_Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-active");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   Icon_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("icon");
   Hide_Titlebar_When_Maximized_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("hide-titlebar-when-maximized");
   Has_Toplevel_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-toplevel-focus");
   Has_Resize_Grip_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-resize-grip");
   Gravity_Property : constant Gdk.Window.Property_Gdk_Gravity :=
     Gdk.Window.Build ("gravity");
   Focus_Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("focus-visible");
   Focus_On_Map_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("focus-on-map");
   Destroy_With_Parent_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("destroy-with-parent");
   Deletable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("deletable");
   Default_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("default-width");
   Default_Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("default-height");
   Decorated_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("decorated");
   Attached_To_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("attached-to");
   Application_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("application");
   Accept_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("accept-focus");
end Gtk.Window;
