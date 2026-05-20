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

--  A toplevel window which can contain other widgets.
--
--  <picture> <source srcset="window-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkWindow" src="window.png"> </picture>
--  Windows normally have decorations that are under the control of the
--  windowing system and allow the user to manipulate the window (resize it,
--  move it, close it,...).
--
--  # GtkWindow as GtkBuildable
--
--  The `GtkWindow` implementation of the [ifaceGtk.Buildable] interface
--  supports setting a child as the titlebar by specifying "titlebar" as the
--  "type" attribute of a `<child>` element.
--
--  # Shortcuts and Gestures
--
--  `GtkWindow` supports the following keyboard shortcuts:
--
--  - <kbd>F10</kbd> activates the menubar, if present. - <kbd>Alt</kbd> makes
--  the mnemonics visible while pressed.
--
--  The following signals have default keybindings:
--
--  - [signalGtk.Window::activate-default] -
--  [signalGtk.Window::activate-focus] - [signalGtk.Window::enable-debugging]
--
--  # Actions
--
--  `GtkWindow` defines a set of built-in actions:
--
--  - `default.activate` activates the default widget. - `window.minimize`
--  minimizes the window. - `window.toggle-maximized` maximizes or restores the
--  window. - `window.close` closes the window.
--
--  # CSS nodes
--
--  ``` window.background [.csd / .solid-csd / .ssd] [.maximized / .fullscreen
--  / .tiled] ├── <child> ╰── <titlebar child>.titlebar [.default-decoration]
--  ```
--
--  `GtkWindow` has a main CSS node with name window and style class
--  .background.
--
--  Style classes that are typically used with the main CSS node are .csd
--  (when client-side decorations are in use), .solid-csd (for client-side
--  decorations without invisible borders), .ssd (used by mutter when rendering
--  server-side decorations). GtkWindow also represents window states with the
--  following style classes on the main node: .maximized, .fullscreen, .tiled
--  (when supported, also .tiled-top, .tiled-left, .tiled-right,
--  .tiled-bottom).
--
--  `GtkWindow` subclasses often add their own discriminating style classes,
--  such as .dialog, .popup or .tooltip.
--
--  Generally, some CSS properties don't make sense on the toplevel window
--  node, such as margins or padding. When client-side decorations without
--  invisible borders are in use (i.e. the .solid-csd style class is added to
--  the main window node), the CSS border of the toplevel window is used for
--  resize drags. In the .csd case, the shadow area outside of the window can
--  be used to resize it.
--
--  `GtkWindow` adds the .titlebar and .default-decoration style classes to
--  the widget that is added as a titlebar child.
--
--  # Accessibility
--
--  `GtkWindow` uses the [enumGtk.AccessibleRole.window] role.
--
--  From GTK 4.12 to 4.18, it used the [enumGtk.AccessibleRole.application]
--  role.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.List_Model;       use Glib.List_Model;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Native;            use Gtk.Native;
with Gtk.Root;              use Gtk.Root;
with Gtk.Shortcut_Manager;  use Gtk.Shortcut_Manager;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Window is

   type Gtk_Window_Record is new Gtk_Widget_Record with null record;
   type Gtk_Window is access all Gtk_Window_Record'Class;

   type Gtk_Window_Group_Record is new GObject_Record with null record;
   type Gtk_Window_Group is access all Gtk_Window_Group_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Window);
   procedure Initialize (Self : not null access Gtk_Window_Record'Class);
   --  Creates a new `GtkWindow`.
   --  To get an undecorated window (without window borders), use
   --  [methodGtk.Window.set_decorated].
   --  All top-level windows created by this function are stored in an
   --  internal top-level window list. This list can be obtained from
   --  [funcGtk.Window.list_toplevels]. Due to GTK keeping a reference to the
   --  window internally, this function does not return a reference to the
   --  caller.
   --  To delete a `GtkWindow`, call [methodGtk.Window.destroy].
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Window_New return Gtk_Window;
   --  Creates a new `GtkWindow`.
   --  To get an undecorated window (without window borders), use
   --  [methodGtk.Window.set_decorated].
   --  All top-level windows created by this function are stored in an
   --  internal top-level window list. This list can be obtained from
   --  [funcGtk.Window.list_toplevels]. Due to GTK keeping a reference to the
   --  window internally, this function does not return a reference to the
   --  caller.
   --  To delete a `GtkWindow`, call [methodGtk.Window.destroy].

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_window_get_type");

   procedure Gtk_New (Window_Group : out Gtk_Window_Group);
   procedure Initialize
      (Window_Group : not null access Gtk_Window_Group_Record'Class);
   --  Creates a new `GtkWindowGroup` object.
   --  Modality of windows only affects windows within the same
   --  `GtkWindowGroup`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Window_Group_New return Gtk_Window_Group;
   --  Creates a new `GtkWindowGroup` object.
   --  Modality of windows only affects windows within the same
   --  `GtkWindowGroup`.

   function Group_Get_Type return Glib.GType;
   pragma Import (C, Group_Get_Type, "gtk_window_group_get_type");

   -------------
   -- Methods --
   -------------

   procedure Close (Self : not null access Gtk_Window_Record);
   --  Requests that the window is closed.
   --  This is similar to what happens when a window manager close button is
   --  clicked.
   --  This function can be used with close buttons in custom titlebars.

   procedure Destroy (Self : not null access Gtk_Window_Record);
   --  Drops the internal reference GTK holds on toplevel windows.

   procedure Fullscreen (Self : not null access Gtk_Window_Record);
   --  Asks to place the window in the fullscreen state.
   --  Note that you shouldn't assume the window is definitely fullscreen
   --  afterward, because other entities (e.g. the user or window manager)
   --  unfullscreen it again, and not all window managers honor requests to
   --  fullscreen windows.
   --  If a window is not explicitly fullscreened or unfullscreened before it
   --  is shown, the initial state is at the window managers discretion.
   --  You can track the result of this operation via the
   --  [propertyGdk.Toplevel:state] property, or by listening to notifications
   --  of the [propertyGtk.Window:fullscreened] property.

   function Get_Child
      (Self : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget of the window.
   --  @return the child widget of Window

   procedure Set_Child
      (Self  : not null access Gtk_Window_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child widget of the window.
   --  @param Child the child widget

   function Get_Decorated
      (Self : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether the window has been set to have decorations.
   --  @return true if the window has been set to have decorations

   procedure Set_Decorated
      (Self    : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Sets whether the window should be decorated.
   --  By default, windows are decorated with a title bar, resize controls,
   --  etc. Some window managers allow GTK to disable these decorations,
   --  creating a borderless window. If you set the decorated property to false
   --  using this function, GTK will do its best to convince the window manager
   --  not to decorate the window. Depending on the system, this function may
   --  not have any effect when called on a window that is already visible, so
   --  you should call it before calling [methodGtk.Widget.show].
   --  On Windows, this function always works, since there's no window manager
   --  policy involved.
   --  @param Setting true to decorate the window

   procedure Get_Default_Size
      (Self   : not null access Gtk_Window_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   --  Gets the default size of the window.
   --  A value of 0 for the width or height indicates that a default size has
   --  not been explicitly set for that dimension, so the "natural" size of the
   --  window will be used.
   --  This function is the recommended way for [saving window state across
   --  restarts of
   --  applications](https://developer.gnome.org/documentation/tutorials/save-state.html).
   --  @param Width location to store the default width
   --  @param Height location to store the default height

   procedure Set_Default_Size
      (Self   : not null access Gtk_Window_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint);
   --  Sets the default size of a window.
   --  The default size of a window is the size that will be used if no other
   --  constraints apply.
   --  The default size will be updated whenever the window is resized to
   --  reflect the new size, unless the window is forced to a size, like when
   --  it is maximized or fullscreened.
   --  If the window's minimum size request is larger than the default, the
   --  default will be ignored.
   --  Setting the default size to a value <= 0 will cause it to be ignored
   --  and the natural size request will be used instead. It is possible to do
   --  this while the window is showing to "reset" it to its initial size.
   --  Unlike [methodGtk.Widget.set_size_request], which sets a size request
   --  for a widget and thus would keep users from shrinking the window, this
   --  function only sets the initial size, just as if the user had resized the
   --  window themselves. Users can still shrink the window again as they
   --  normally would. Setting a default size of -1 means to use the "natural"
   --  default size (the size request of the window).
   --  If you use this function to reestablish a previously saved window size,
   --  note that the appropriate size to save is the one returned by
   --  [methodGtk.Window.get_default_size]. Using the window allocation
   --  directly will not work in all circumstances and can lead to growing or
   --  shrinking windows.
   --  @param Width width in pixels, or -1 to unset the default width
   --  @param Height height in pixels, or -1 to unset the default height

   function Get_Default_Widget
      (Self : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the default widget for Window.
   --  @return the default widget

   procedure Set_Default_Widget
      (Self           : not null access Gtk_Window_Record;
       Default_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the default widget.
   --  The default widget is the widget that is activated when the user
   --  presses <kbd>Enter</kbd> in a dialog (for example).
   --  @param Default_Widget widget to be the default

   function Get_Deletable
      (Self : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether the window has been set to have a close button.
   --  @return true if the window has been set to have a close button

   procedure Set_Deletable
      (Self    : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Sets whether the window should be deletable.
   --  By default, windows have a close button in the window frame. Some
   --  window managers allow GTK to disable this button. If you set the
   --  deletable property to false using this function, GTK will do its best to
   --  convince the window manager not to show a close button. Depending on the
   --  system, this function may not have any effect when called on a window
   --  that is already visible, so you should call it before calling
   --  [methodGtk.Widget.show].
   --  On Windows, this function always works, since there's no window manager
   --  policy involved.
   --  @param Setting true to decorate the window as deletable

   function Get_Destroy_With_Parent
      (Self : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether the window will be destroyed with its transient parent.
   --  @return true if the window will be destroyed with its transient parent

   procedure Set_Destroy_With_Parent
      (Self    : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Sets whether to destroy the window when the transient parent is
   --  destroyed.
   --  This is useful for dialogs that shouldn't persist beyond the lifetime
   --  of the main window they are associated with, for example.
   --  @param Setting whether to destroy the window with its transient parent

   function Get_Focus_Visible
      (Self : not null access Gtk_Window_Record) return Boolean;
   --  Gets whether "focus rectangles" are supposed to be visible.
   --  @return true if "focus rectangles" are supposed to be visible in this
   --  window

   procedure Set_Focus_Visible
      (Self    : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Sets whether "focus rectangles" are supposed to be visible.
   --  This property is maintained by GTK based on user input, and should not
   --  be set by applications.
   --  @param Setting the new value

   function Get_Gravity
      (Self : not null access Gtk_Window_Record)
       return Gtk.Enums.Gtk_Window_Gravity;
   --  Returns the gravity that is used when changing the window size
   --  programmatically.
   --  Since: gtk+ 4.20
   --  @return the gravity

   procedure Set_Gravity
      (Self    : not null access Gtk_Window_Record;
       Gravity : Gtk.Enums.Gtk_Window_Gravity);
   --  Sets the gravity that is used when changing the window size
   --  programmatically.
   --  Since: gtk+ 4.20
   --  @param Gravity the new gravity

   function Get_Group
      (Self : not null access Gtk_Window_Record) return Gtk_Window_Group;
   --  Returns the group for the window.
   --  If the window has no group, then the default group is returned.
   --  @return the window group for Window or the default group

   function Get_Handle_Menubar_Accel
      (Self : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether this window reacts to <kbd>F10</kbd> presses by
   --  activating a menubar it contains.
   --  Since: gtk+ 4.2
   --  @return true if the window handles <kbd>F10</kbd>

   procedure Set_Handle_Menubar_Accel
      (Self                 : not null access Gtk_Window_Record;
       Handle_Menubar_Accel : Boolean);
   --  Sets whether this window should react to <kbd>F10</kbd> presses by
   --  activating a menubar it contains.
   --  Since: gtk+ 4.2
   --  @param Handle_Menubar_Accel true to make Window handle <kbd>F10</kbd>

   function Get_Hide_On_Close
      (Self : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether the window will be hidden instead of destroyed when the
   --  close button is clicked.
   --  @return true if the window will be hidden

   procedure Set_Hide_On_Close
      (Self    : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Sets whether clicking the close button will hide the window instead of
   --  destroying it.
   --  @param Setting whether to hide the window when it is closed

   function Get_Icon_Name
      (Self : not null access Gtk_Window_Record) return UTF8_String;
   --  Returns the name of the themed icon for the window.
   --  @return the icon name

   procedure Set_Icon_Name
      (Self : not null access Gtk_Window_Record;
       Name : UTF8_String := "");
   --  Sets the icon for the window from a named themed icon.
   --  See the docs for [classGtk.IconTheme] for more details. On some
   --  platforms, the window icon is not used at all.
   --  Note that this has nothing to do with the WM_ICON_NAME property which
   --  is mentioned in the ICCCM.
   --  @param Name the name of the themed icon

   function Get_Mnemonics_Visible
      (Self : not null access Gtk_Window_Record) return Boolean;
   --  Gets whether mnemonics are supposed to be visible.
   --  @return true if mnemonics are supposed to be visible in this window

   procedure Set_Mnemonics_Visible
      (Self    : not null access Gtk_Window_Record;
       Setting : Boolean);
   --  Sets whether mnemonics are supposed to be visible.
   --  This property is maintained by GTK based on user input, and should not
   --  be set by applications.
   --  @param Setting the new value

   function Get_Modal
      (Self : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether the window is modal.
   --  @return true if the window is set to be modal and establishes a grab
   --  when shown

   procedure Set_Modal
      (Self  : not null access Gtk_Window_Record;
       Modal : Boolean);
   --  Sets a window modal or non-modal.
   --  Modal windows prevent interaction with other windows in the same
   --  application. To keep modal dialogs on top of main application windows,
   --  use [methodGtk.Window.set_transient_for] to make the dialog transient
   --  for the parent; most window managers will then disallow lowering the
   --  dialog below the parent.
   --  @param Modal whether the window is modal

   function Get_Resizable
      (Self : not null access Gtk_Window_Record) return Boolean;
   --  Gets whether the user can resize the window.
   --  @return true if the user can resize the window

   procedure Set_Resizable
      (Self      : not null access Gtk_Window_Record;
       Resizable : Boolean);
   --  Sets whether the user can resize a window.
   --  Windows are user resizable by default.
   --  @param Resizable true if the user can resize this window

   function Get_Title
      (Self : not null access Gtk_Window_Record) return UTF8_String;
   --  Retrieves the title of the window.
   --  @return the title

   procedure Set_Title
      (Self  : not null access Gtk_Window_Record;
       Title : UTF8_String := "");
   --  Sets the title of the window.
   --  The title of a window will be displayed in its title bar; on the X
   --  Window System, the title bar is rendered by the window manager so
   --  exactly how the title appears to users may vary according to a user's
   --  exact configuration. The title should help a user distinguish this
   --  window from other windows they may have open. A good title might include
   --  the application name and current document filename, for example.
   --  Passing `NULL` does the same as setting the title to an empty string.
   --  @param Title title of the window

   function Get_Titlebar
      (Self : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the titlebar that has been set with
   --  [methodGtk.Window.set_titlebar].
   --  @return the titlebar

   procedure Set_Titlebar
      (Self     : not null access Gtk_Window_Record;
       Titlebar : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets a custom titlebar for the window.
   --  A typical widget used here is [classGtk.HeaderBar], as it provides
   --  various features expected of a titlebar while allowing the addition of
   --  child widgets to it.
   --  If you set a custom titlebar, GTK will do its best to convince the
   --  window manager not to put its own titlebar on the window. Depending on
   --  the system, this function may not work for a window that is already
   --  visible, so you set the titlebar before calling [methodGtk.Widget.show].
   --  @param Titlebar the widget to use as titlebar

   function Get_Transient_For
      (Self : not null access Gtk_Window_Record) return Gtk_Window;
   --  Fetches the transient parent for this window.
   --  @return the transient parent

   procedure Set_Transient_For
      (Self   : not null access Gtk_Window_Record;
       Parent : access Gtk_Window_Record'Class);
   --  Sets a transient parent for the window.
   --  Dialog windows should be set transient for the main application window
   --  they were spawned from. This allows window managers to e.g. keep the
   --  dialog on top of the main window, or center the dialog over the main
   --  window. [ctorGtk.Dialog.new_with_buttons] and other convenience
   --  functions in GTK will sometimes call this function on your behalf.
   --  Passing `NULL` for Parent unsets the current transient window.
   --  On Windows, this function puts the child window on top of the parent,
   --  much as the window manager would have done on X.
   --  @param Parent parent window

   function Has_Group
      (Self : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether the window has an explicit window group.
   --  @return true if Window has an explicit window group

   function Is_Active
      (Self : not null access Gtk_Window_Record) return Boolean;
   --  Returns whether the window is part of the current active toplevel.
   --  The active toplevel is the window receiving keystrokes.
   --  The return value is True if the window is active toplevel itself. You
   --  might use this function if you wanted to draw a widget differently in an
   --  active window from a widget in an inactive window.
   --  @return true if the window part of the current active window.

   function Is_Fullscreen
      (Self : not null access Gtk_Window_Record) return Boolean;
   --  Retrieves the current fullscreen state of the window.
   --  Note that since fullscreening is ultimately handled by the window
   --  manager and happens asynchronously to an application request, you
   --  shouldn't assume the return value of this function changing immediately
   --  (or at all), as an effect of calling [methodGtk.Window.fullscreen] or
   --  [methodGtk.Window.unfullscreen].
   --  If the window isn't yet mapped, the value returned will whether the
   --  initial requested state is fullscreen.
   --  @return whether the window is fullscreen

   function Is_Maximized
      (Self : not null access Gtk_Window_Record) return Boolean;
   --  Retrieves the current maximized state of the window.
   --  Note that since maximization is ultimately handled by the window
   --  manager and happens asynchronously to an application request, you
   --  shouldn't assume the return value of this function changing immediately
   --  (or at all), as an effect of calling [methodGtk.Window.maximize] or
   --  [methodGtk.Window.unmaximize].
   --  If the window isn't yet mapped, the value returned will whether the
   --  initial requested state is maximized.
   --  @return whether the window is maximized

   function Is_Suspended
      (Self : not null access Gtk_Window_Record) return Boolean;
   --  Retrieves the current suspended state of the window.
   --  A window being suspended means it's currently not visible to the user,
   --  for example by being on a inactive workspace, minimized, obstructed.
   --  Since: gtk+ 4.12
   --  @return whether the window is suspended

   procedure Maximize (Self : not null access Gtk_Window_Record);
   --  Asks to maximize the window, so that it fills the screen.
   --  Note that you shouldn't assume the window is definitely maximized
   --  afterward, because other entities (e.g. the user or window manager)
   --  could unmaximize it again, and not all window managers support
   --  maximization.
   --  It's permitted to call this function before showing a window, in which
   --  case the window will be maximized when it appears onscreen initially.
   --  If a window is not explicitly maximized or unmaximized before it is
   --  shown, the initial state is at the window managers discretion. For
   --  example, it might decide to maximize a window that almost fills the
   --  screen.
   --  You can track the result of this operation via the
   --  [propertyGdk.Toplevel:state] property, or by listening to notifications
   --  on the [propertyGtk.Window:maximized] property.

   procedure Minimize (Self : not null access Gtk_Window_Record);
   --  Asks to minimize the window.
   --  Note that you shouldn't assume the window is definitely minimized
   --  afterward, because the windowing system might not support this
   --  functionality; other entities (e.g. the user or the window manager)
   --  could unminimize it again, or there may not be a window manager in which
   --  case minimization isn't possible, etc.
   --  It's permitted to call this function before showing a window, in which
   --  case the window will be minimized before it ever appears onscreen.
   --  You can track result of this operation via the
   --  [propertyGdk.Toplevel:state] property.

   procedure Present (Self : not null access Gtk_Window_Record);
   --  Presents a window to the user.
   --  This may mean raising the window in the stacking order, unminimizing
   --  it, moving it to the current desktop and/or giving it the keyboard focus
   --  (possibly dependent on the user's platform, window manager and
   --  preferences).
   --  If Window is hidden, this function also makes it visible.

   procedure Present_With_Time
      (Self      : not null access Gtk_Window_Record;
       Timestamp : Guint32);
   pragma Obsolescent (Present_With_Time);
   --  Presents a window to the user in response to an user interaction.
   --  See [methodGtk.Window.present] for more details.
   --  The timestamp should be gathered when the window was requested to be
   --  shown (when clicking a link for example), rather than once the window is
   --  ready to be shown.
   --  Deprecated since 4.14, 1
   --  @param Timestamp the timestamp of the user interaction (typically a
   --  button or key press event) which triggered this call

   procedure Set_Startup_Id
      (Self       : not null access Gtk_Window_Record;
       Startup_Id : UTF8_String);
   --  Sets the startup notification ID.
   --  Startup notification identifiers are used by desktop environment to
   --  track application startup, to provide user feedback and other features.
   --  This function changes the corresponding property on the underlying
   --  `GdkSurface`.
   --  Normally, startup identifier is managed automatically and you should
   --  only use this function in special cases like transferring focus from
   --  other processes. You should use this function before calling
   --  [methodGtk.Window.present] or any equivalent function generating a
   --  window map event.
   --  This function is only useful on Wayland or X11, not with other GDK
   --  backends.
   --  @param Startup_Id a string with startup-notification identifier

   procedure Unfullscreen (Self : not null access Gtk_Window_Record);
   --  Asks to remove the fullscreen state for the window, and return to its
   --  previous state.
   --  Note that you shouldn't assume the window is definitely not fullscreen
   --  afterward, because other entities (e.g. the user or window manager)
   --  could fullscreen it again, and not all window managers honor requests to
   --  unfullscreen windows; normally the window will end up restored to its
   --  normal state. Just don't write code that crashes if not.
   --  If a window is not explicitly fullscreened or unfullscreened before it
   --  is shown, the initial state is at the window managers discretion.
   --  You can track the result of this operation via the
   --  [propertyGdk.Toplevel:state] property, or by listening to notifications
   --  of the [propertyGtk.Window:fullscreened] property.

   procedure Unmaximize (Self : not null access Gtk_Window_Record);
   --  Asks to unmaximize the window.
   --  Note that you shouldn't assume the window is definitely unmaximized
   --  afterward, because other entities (e.g. the user or window manager)
   --  maximize it again, and not all window managers honor requests to
   --  unmaximize.
   --  If a window is not explicitly maximized or unmaximized before it is
   --  shown, the initial state is at the window managers discretion. For
   --  example, it might decide to maximize a window that almost fills the
   --  screen.
   --  You can track the result of this operation via the
   --  [propertyGdk.Toplevel:state] property, or by listening to notifications
   --  on the [propertyGtk.Window:maximized] property.

   procedure Unminimize (Self : not null access Gtk_Window_Record);
   --  Asks to unminimize the window.
   --  Note that you shouldn't assume the window is definitely unminimized
   --  afterward, because the windowing system might not support this
   --  functionality; other entities (e.g. the user or the window manager)
   --  could minimize it again, or there may not be a window manager in which
   --  case minimization isn't possible, etc.
   --  You can track result of this operation via the
   --  [propertyGdk.Toplevel:state] property.

   procedure Add_Window
      (Window_Group : not null access Gtk_Window_Group_Record;
       Window       : not null access Gtk_Window_Record'Class);
   --  Adds a window to a `GtkWindowGroup`.
   --  @param Window the `GtkWindow` to add

   function List_Windows
      (Window_Group : not null access Gtk_Window_Group_Record)
       return Gtk.Widget.Widget_List.Glist;
   --  Returns a list of the `GtkWindows` that belong to Window_Group.

   procedure Remove_Window
      (Window_Group : not null access Gtk_Window_Group_Record;
       Window       : not null access Gtk_Window_Record'Class);
   --  Removes a window from a `GtkWindowGroup`.
   --  @param Window the `GtkWindow` to remove

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Announce
      (Self     : not null access Gtk_Window_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Window_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Window_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Window_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Window_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Window_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Window_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Window_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Window_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Window_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Window_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Window_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Window_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Window_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Window_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   procedure Get_Surface_Transform
      (Self : not null access Gtk_Window_Record;
       X    : out Gdouble;
       Y    : out Gdouble);

   procedure Realize (Self : not null access Gtk_Window_Record);

   procedure Unrealize (Self : not null access Gtk_Window_Record);

   function Get_Focus
      (Self : not null access Gtk_Window_Record)
       return Gtk.Widget.Gtk_Widget;

   procedure Set_Focus
      (Self  : not null access Gtk_Window_Record;
       Focus : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------
   -- Functions --
   ---------------

   function Get_Default_Icon_Name return UTF8_String;
   --  Returns the fallback icon name for windows.
   --  The returned string is owned by GTK and should not be modified. It is
   --  only valid until the next call to
   --  [funcGtk.Window.set_default_icon_name].
   --  @return the fallback icon name for windows

   procedure Set_Default_Icon_Name (Name : UTF8_String);
   --  Sets an icon to be used as fallback.
   --  The fallback icon is used for windows that haven't had
   --  [methodGtk.Window.set_icon_name] called on them.
   --  @param Name the name of the themed icon

   function Get_Toplevels return Glib.List_Model.Glist_Model;
   --  Returns the list of all existing toplevel windows.
   --  If you want to iterate through the list and perform actions involving
   --  callbacks that might destroy the widgets or add new ones, be aware that
   --  the list of toplevels will change and emit the "items-changed" signal.
   --  @return the list of toplevel widgets

   function List_Toplevels return Gtk.Widget.Widget_List.Glist;
   --  Returns the list of all existing toplevel windows.
   --  The widgets in the list are not individually referenced. If you want to
   --  iterate through the list and perform actions involving callbacks that
   --  might destroy the widgets, you must call `g_list_foreach (result,
   --  (GFunc)g_object_ref, NULL)` first, and then unref all the widgets
   --  afterwards.

   procedure Set_Auto_Startup_Notification (Setting : Boolean);
   --  Sets whether the window should request startup notification.
   --  By default, after showing the first window, GTK calls
   --  [methodGdk.Toplevel.set_startup_id]. Call this function to disable the
   --  automatic startup notification. You might do this if your first window
   --  is a splash screen, and you want to delay notification until after your
   --  real main window has been shown, for example.
   --  In that example, you would disable startup notification temporarily,
   --  show your splash screen, then re-enable it so that showing the main
   --  window would automatically result in notification.
   --  @param Setting true to automatically do startup notification

   procedure Set_Interactive_Debugging (Enable : Boolean);
   --  Opens or closes the [interactive
   --  debugger](running.htmlinteractive-debugging).
   --  The debugger offers access to the widget hierarchy of the application
   --  and to useful debugging tools.
   --  This function allows applications that already use
   --  <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>I</kbd> (or
   --  <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>D</kbd>) for their own key
   --  shortcuts to add a different shortcut to open the Inspector.
   --  If you are not overriding the default key shortcuts for the Inspector,
   --  you should not use this function.
   --  @param Enable true to enable interactive debugging

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Application_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Application.Gtk_Application
   --  The `GtkApplication` associated with the window.
   --
   --  The application will be kept alive for at least as long as it has any
   --  windows associated with it (see Glib.Application.Hold for a way to keep
   --  it alive without windows).
   --
   --  Normally, the connection between the application and the window will
   --  remain until the window is destroyed, but you can explicitly remove it
   --  by setting the this property to `NULL`.

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.

   Decorated_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the window should have a frame (also known as *decorations*).

   Default_Height_Property : constant Glib.Properties.Property_Int;
   --  The default height of the window.

   Default_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The default widget.

   Default_Width_Property : constant Glib.Properties.Property_Int;
   --  The default width of the window.

   Deletable_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the window frame should have a close button.

   Destroy_With_Parent_Property : constant Glib.Properties.Property_Boolean;
   --  If this window should be destroyed when the parent is destroyed.

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gdk.Display
   --  The display that will display this window.

   Focus_Visible_Property : constant Glib.Properties.Property_Boolean;
   --  Whether 'focus rectangles' are currently visible in this window.
   --
   --  This property is maintained by GTK based on user input and should not
   --  be set by applications.

   Focus_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The focus widget.

   Fullscreened_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the window is fullscreen.
   --
   --  Setting this property is the equivalent of calling
   --  [methodGtk.Window.fullscreen] or [methodGtk.Window.unfullscreen]; either
   --  operation is asynchronous, which means you will need to connect to the
   --  ::notify signal in order to know whether the operation was successful.

   Gravity_Property : constant Gtk.Enums.Property_Gtk_Window_Gravity;
   --  The gravity to use when resizing the window programmatically.
   --
   --  Gravity describes which point of the window we want to keep fixed
   --  (meaning that the window will grow in the opposite direction). For
   --  example, a gravity of `GTK_WINDOW_GRAVITY_TOP_RIGHT` means that we want
   --  the to fix top right corner of the window.

   Handle_Menubar_Accel_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the window frame should handle <kbd>F10</kbd> for activating
   --  menubars.

   Hide_On_Close_Property : constant Glib.Properties.Property_Boolean;
   --  If this window should be hidden instead of destroyed when the user
   --  clicks the close button.

   Icon_Name_Property : constant Glib.Properties.Property_String;
   --  Specifies the name of the themed icon to use as the window icon.
   --
   --  See [classGtk.IconTheme] for more details.

   Is_Active_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the toplevel is the currently active window.

   Maximized_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the window is maximized.
   --
   --  Setting this property is the equivalent of calling
   --  [methodGtk.Window.maximize] or [methodGtk.Window.unmaximize]; either
   --  operation is asynchronous, which means you will need to connect to the
   --  ::notify signal in order to know whether the operation was successful.

   Mnemonics_Visible_Property : constant Glib.Properties.Property_Boolean;
   --  Whether mnemonics are currently visible in this window.
   --
   --  This property is maintained by GTK based on user input, and should not
   --  be set by applications.

   Modal_Property : constant Glib.Properties.Property_Boolean;
   --  If true, the window is modal.

   Resizable_Property : constant Glib.Properties.Property_Boolean;
   --  If true, users can resize the window.

   Startup_Id_Property : constant Glib.Properties.Property_String;
   --  Flags: write
   --  A write-only property for setting window's startup notification
   --  identifier.

   Suspended_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the window is suspended.
   --
   --  See [methodGtk.Window.is_suspended] for details about what suspended
   --  means.

   Title_Property : constant Glib.Properties.Property_String;
   --  The title of the window.

   Titlebar_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The titlebar widget.

   Transient_For_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk_Window
   --  The transient parent of the window.

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
   --  Emitted when the user activates the default widget.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The keybindings for this signal are all forms of the <kbd>Enter</kbd>
   --  key.

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
   --  Emitted when the user activates the currently focused widget of Window.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default binding for this signal is <kbd>␣</kbd>.

   type Cb_Gtk_Window_Boolean is not null access function
     (Self : access Gtk_Window_Record'Class) return Boolean;

   type Cb_GObject_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class)
   return Boolean;

   Signal_Close_Request : constant Glib.Signal_Name := "close-request";
   procedure On_Close_Request
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_Gtk_Window_Boolean;
       After : Boolean := False);
   procedure On_Close_Request
      (Self  : not null access Gtk_Window_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user clicks on the close button of the window.
   -- 
   --  Callback parameters:

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
   --  Emitted when the user enables or disables interactive debugging.
   --
   --  When Toggle is true, interactive debugging is toggled on or off, when
   --  it is false, the debugger will be pointed at the widget under the
   --  pointer.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default bindings for this signal are
   --  <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>I</kbd> and
   --  <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>D</kbd>.
   -- 
   --  Callback parameters:
   --    --  @param Toggle toggle the debugger

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
   --  Emitted when the set of accelerators or mnemonics that are associated
   --  with the window changes.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Accessible"
   --
   --  - "ConstraintTarget"
   --
   --  - "Native"
   --
   --  - "Root"
   --
   --  - "ShortcutManager"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Window_Record, Gtk_Window);
   function "+"
     (Widget : access Gtk_Window_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Window
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Window_Record, Gtk_Window);
   function "+"
     (Widget : access Gtk_Window_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Window
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Native is new Glib.Types.Implements
     (Gtk.Native.Gtk_Native, Gtk_Window_Record, Gtk_Window);
   function "+"
     (Widget : access Gtk_Window_Record'Class)
   return Gtk.Native.Gtk_Native
   renames Implements_Gtk_Native.To_Interface;
   function "-"
     (Interf : Gtk.Native.Gtk_Native)
   return Gtk_Window
   renames Implements_Gtk_Native.To_Object;

   package Implements_Gtk_Root is new Glib.Types.Implements
     (Gtk.Root.Gtk_Root, Gtk_Window_Record, Gtk_Window);
   function "+"
     (Widget : access Gtk_Window_Record'Class)
   return Gtk.Root.Gtk_Root
   renames Implements_Gtk_Root.To_Interface;
   function "-"
     (Interf : Gtk.Root.Gtk_Root)
   return Gtk_Window
   renames Implements_Gtk_Root.To_Object;

   package Implements_Gtk_Shortcut_Manager is new Glib.Types.Implements
     (Gtk.Shortcut_Manager.Gtk_Shortcut_Manager, Gtk_Window_Record, Gtk_Window);
   function "+"
     (Widget : access Gtk_Window_Record'Class)
   return Gtk.Shortcut_Manager.Gtk_Shortcut_Manager
   renames Implements_Gtk_Shortcut_Manager.To_Interface;
   function "-"
     (Interf : Gtk.Shortcut_Manager.Gtk_Shortcut_Manager)
   return Gtk_Window
   renames Implements_Gtk_Shortcut_Manager.To_Object;

private
   Transient_For_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("transient-for");
   Titlebar_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("titlebar");
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Suspended_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("suspended");
   Startup_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("startup-id");
   Resizable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("resizable");
   Modal_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("modal");
   Mnemonics_Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("mnemonics-visible");
   Maximized_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("maximized");
   Is_Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-active");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   Hide_On_Close_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("hide-on-close");
   Handle_Menubar_Accel_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("handle-menubar-accel");
   Gravity_Property : constant Gtk.Enums.Property_Gtk_Window_Gravity :=
     Gtk.Enums.Build ("gravity");
   Fullscreened_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("fullscreened");
   Focus_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("focus-widget");
   Focus_Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("focus-visible");
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
   Destroy_With_Parent_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("destroy-with-parent");
   Deletable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("deletable");
   Default_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("default-width");
   Default_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("default-widget");
   Default_Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("default-height");
   Decorated_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("decorated");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
   Application_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("application");
end Gtk.Window;
