-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  This widget implements a top level window.
--  It is used as the base class for dialogs, ...
--
--  A window has both a default widget (to which events are sent if no other
--  widget has been selected and has the focus), and a focus widget (which
--  gets the events and overrides the default widget).
--
--  You can set many hints on the window (its minimum and maximum size, its
--  decoration, etc.) but these are only hints to the window manager, which
--  might not respect them.
--
--  A useful hint, respected by most window managers, can be used to force
--  some secondary windows to stay on top of the main window on the screen
--  (for instance, so that a smaller window can not be hidden by a bigger
--  one). See the function Set_Transient_For below.
--
--  A window can also be modal, i.e. grab all the mouse and keyboard events
--  in the application while it is displayed.
--
--  </description>
--  <c_version>1.3.11</c_version>

with Glib.Properties;
with Gdk.Types;
with Gdk.Window;
with Gtk.Accel_Group;
with Gtk.Bin;
with Gtk.Enums;
with Gtk.Widget;

package Gtk.Window is

   type Gtk_Window_Record is new Bin.Gtk_Bin_Record with private;
   type Gtk_Window is access all Gtk_Window_Record'Class;

   procedure Gtk_New
     (Window   : out Gtk_Window;
      The_Type : Gtk.Enums.Gtk_Window_Type := Gtk.Enums.Window_Toplevel);
   --  Create a new window.
   --  The_Type specifies the type of the window, and can be either a
   --  top level window, a dialog or a popup window. You will most often only
   --  need to use Window_Toplevel, the other types are mostly used internally
   --  by gtk+.
   --  A Popup window is used to display a temporary information window. It has
   --  no borders nor resizing handles.

   procedure Initialize
     (Window   : access Gtk_Window_Record'Class;
      The_Type : Gtk.Enums.Gtk_Window_Type);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Window.

   procedure Set_Title (Window : access Gtk_Window_Record; Title : String);
   --  Change the title of the window, as it appears in the title bar.
   --  Note that on some systems you might not be able to change it.

   function Get_Title (Window : access Gtk_Window_Record) return String;
   --  Return the title of the window, or "" if there is none

   procedure Set_Wmclass
     (Window        : access Gtk_Window_Record;
      Wmclass_Name  : String;
      Wmclass_Class : String);
   --  Don't use this function. It sets the X Window System "class" and
   --  "name" hints for a window. According to the ICCCM, you should
   --  always set these to the same value for all windows in an
   --  application, and GTK sets them to that value by default, so calling
   --  this function is sort of pointless. However, you may want to call
   --  Set_Role on each window in your application, for the
   --  benefit of the session manager. Setting the role allows the window
   --  manager to restore window positions when loading a saved session.

   procedure Set_Role (Window : access Gtk_Window_Record; Role : String);
   --  In combination with the window title, the window role allows a
   --  window manager to identify "the same" window when an application is
   --  restarted. So for example you might set the "toolbox" role on your
   --  app's toolbox window, so that when the user restarts their session,
   --  the window manager can put the toolbox back in the same place.
   --  If a window already has a unique title, you don't need to set the
   --  role, since the WM can use the title to identify the window when
   --  restoring the session.
   --  Role: unique identifier for the window to be used when restoring a
   --  session

   procedure Add_Accel_Group
     (Window      : access Gtk_Window_Record;
      Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group);
   --  Specify an accelerator group for the window.

   procedure Remove_Accel_Group
     (Window      : access Gtk_Window_Record;
      Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group);
   --  Remove the specified accelerator group for the window.

   procedure Set_Position
     (Window   : access Gtk_Window_Record;
      Position : Gtk.Enums.Gtk_Window_Position);
   --  Specify how the position of the window should be computed.
   --  If Position is Win_Pos_Center_Always or Win_Pos_Center, then the window
   --  is centered on the screen. In the first case, it is also recentered
   --  when the window is resized with Gtk.Widget.Set_Usize (ie except on
   --  user action).
   --  If Position is Win_Pos_Mouse, then the window is positioned so that it
   --  centered around the mouse.
   --  If Position is Win_Pos_None, no calculation is done. If
   --  Gtk.Widget.Set_Uposition as been called, it is respected. This is the
   --  default case.

   function Activate_Focus (Window : access Gtk_Window_Record) return Boolean;
   --  Call Gtk.Widget.Activate on the widget that currently has the focus in
   --  the window, ie sends an "activate" signal to that widget. Note that this
   --  signal does not really exists and is mapped to some widget-specific
   --  signal.
   --  Return True if the widget could be activated, False otherwise.
   --  The Focus widget is set through a signal "set_focus".

   function Activate_Default
     (Window : access Gtk_Window_Record) return Boolean;
   --  Activate the default widget in the window.
   --  In other words, send an "activate" signal to that widget. Note that
   --  this signal is a virtual one and is mapped to some widget specific
   --  signal.
   --  Return False is the widget could not be activated or if there was
   --  no default widget.
   --  You can set the default widget with the following calls:
   --
   --     Gtk.Widget.Set_Flags (Widget, Can_Default);
   --
   --     Gtk.Widget.Grab_Default (Widget);

   procedure Set_Transient_For
     (Window : access Gtk_Window_Record;
      Parent : access Gtk_Window_Record'Class);
   --  Specify that Window is a transient window.
   --  A transient window is a temporary window, like a popup menu or a
   --  dialog box). Parent is the toplevel window of the application to which
   --  Window belongs. A window that has set this can expect less decoration
   --  from the window manager (for instance no title bar and no borders).
   --  (see XSetTransientForHint(3) on Unix systems)
   --
   --  The main usage of this function is to force Window to be on top of
   --  Parent on the screen at all times. Most window managers respect this
   --  hint, even if this is not mandatory.

   procedure Set_Type_Hint
     (Window : access Gtk_Window_Record;
      Hint   : Gdk.Window.Gdk_Window_Type_Hint);
   --  allow the window manager to decorate and handle the window in a way
   --  which is suitable to the function of the window in your application.
   --  This function should be called before the window becomes visible.

   procedure Set_Destroy_With_Parent
     (Window  : access Gtk_Window_Record;
      Setting : Boolean := True);
   --  Set whether destroying the transient parent of Window will also destroy
   --  Window itself.
   --  This is useful for dialogs that shouldn't persist beyond the lifetime
   --  of the main window they're associated with, for example.

   procedure Set_Resizable
     (Window    : access Gtk_Window_Record;
      Resizable : Boolean := True);
   --  Set whether the user can resize a window.
   --  Windows are user resizable by default.

   --  <doc_ignore>
   procedure Set_Resizeable
     (Window    : access Gtk_Window_Record;
      Resizable : Boolean := True) renames Set_Resizable;
   --  This procedure is deprecated.
   --  </doc_ignore>

   function Get_Resizable (Window : access Gtk_Window_Record) return Boolean;
   --  Whether the user can resize a window.

   function Get_Resizeable (Window : access Gtk_Window_Record) return Boolean
     renames Get_Resizable;
   --  This function is deprecated.

   procedure Set_Gravity
     (Window  : access Gtk_Window_Record;
      Gravity : Gdk.Window.Gdk_Gravity);
   --  Window gravity defines the "reference point" to be used when
   --  positioning or resizing a window. Calls to
   --  Gtk.Widget.Set_UPosition will position a different point on the
   --  window depending on the window gravity. When the window changes size
   --  the reference point determined by the window's gravity will stay in
   --  a fixed location.
   --
   --  See Gdk_Gravity for full details. To briefly summarize,
   --  Gravity_North_West means that the reference point is the
   --  northwest (top left) corner of the window
   --  frame. Gravity_South_East would be the bottom right corner of
   --  the frame, and so on. If you want to position the window contents,
   --  rather than the window manager's frame, Gravity_Static moves
   --  the reference point to the northwest corner of the Gtk_Window
   --  itself.
   --
   --  The default window gravity is Gravity_North_West.

   function Get_Gravity
     (Window : access Gtk_Window_Record) return Gdk.Window.Gdk_Gravity;
   --  Return the value set by Set_Gravity.

   procedure Set_Geometry_Hints
     (Window          : access Gtk_Window_Record;
      Geometry_Widget : Gtk.Widget.Gtk_Widget;
      Geometry        : Gdk.Window.Gdk_Geometry;
      Geom_Mask       : Gdk.Window.Gdk_Window_Hints);
   --  Specify some geometry hints for the window.
   --  This includes its minimal and maximal sizes, ...
   --  These attributes are specified in Geometry.
   --  Geom_Mask indicates which of the fields in Geometry are set.
   --  Geometry_Widget can be null (and thus is not an access parameter). It
   --  adds some extra size to Geometry based on the actual size of
   --  Geometry_Widget (the extra amount is Window'Size - Geometry_Widget'Size)
   --
   --  Geometry.Base_* indicates the size that is used by the window manager
   --  to report the size: for instance, if Base_Width = 600 and actual width
   --  is 200, the window manager will indicate a width of -400.
   --
   --  If your window manager respects the hints (and its doesn't have to),
   --  then the user will never be able to resize the window to a size not
   --  in Geometry.Min_* .. Geometry.Max_*.
   --
   --  Geometry.*_Inc specifies by which amount the size will be multiplied.
   --  For instance, if Width_Inc = 50 and the size reported by the Window
   --  Manager is 2x3, then the actual width of the window is 100.
   --  Your window's size will always be a multiple of the *_Inc values.
   --
   --  Geometry.*_Aspect specifies the aspect ratio for the window. The window
   --  will always be resized so that the ratio between its width and its
   --  height remains in the range Min_Aspect .. Max_Aspect.

   procedure Set_Has_Frame (Window : access Gtk_Window_Record);
   --  If this function is called on a window before it is realized
   --  or showed it will have a "frame" window around widget-window.
   --  Using the signal frame_event you can receive all events targeted at the
   --  frame.
   --
   --  This function is used by the linux-fb port to implement managed
   --  windows, but it could concievably be used by X-programs that
   --  want to do their own window decorations.

   procedure Set_Frame_Dimensions
     (Window : access Gtk_Window_Record;
      Left   : Gint;
      Top    : Gint;
      Right  : Gint;
      Bottom : Gint);
   --  Change the size of the frame border.
   --  This has only an effect for windows with frames (see Set_Has_Frame).

   procedure Set_Decorated
     (Window  : access Gtk_Window_Record;
      Setting : Boolean := True);
   --  By default, windows are decorated with a title bar, resize
   --  controls, etc. Some window managers allow GtkAda to disable these
   --  decorations, creating a borderless window. If you set the decorated
   --  property to False using this function, GtkAda will do its best to
   --  convince the window manager not to decorate the window.

   procedure Set_Modal
     (Window : access Gtk_Window_Record;
      Modal  : Boolean := True);
   --  Define the window as being Modal.
   --  It will grab the input from the keyboard and the mouse while it is
   --  displayed and will release it when it is hidden. The grab is only in
   --  effect for the windows that belong to the same application, and will not
   --  affect other applications running on the same screen.
   --  In cunjunction with Gtk.Main.Main, this is the easiest way to show a
   --  dialog to which the user has to answer before the application can
   --  continue.

   function List_Toplevels return Gtk.Widget.Widget_List.Glist;
   --  Return a list of all existing toplevel windows.
   --  The widgets in the list are not individually referenced. If you want
   --  to iterate through the list and perform actions involving
   --  callbacks that might destroy the widgets, you must "ref"erence
   --  all the widgets in the list first and then unref all the widgets
   --  afterwards.

   procedure Present (Window : access Gtk_Window_Record);
   --  Present a window to the user.
   --  This may mean raising the window in the stacking order, deiconifying it,
   --  moving it to the current desktop, and/or giving it the keyboard focus,
   --  possibly dependent on the user's platform, window manager, and
   --  preferences.
   --
   --  If Window is hidden, this function calls Gtk.Widget.Show as well.
   --
   --  This function should be used when the user tries to open a window
   --  that's already open. Say for example the preferences dialog is
   --  currently open, and the user chooses Preferences from the menu
   --  a second time; use Present to move the already-open dialog
   --  where the user can see it.

   procedure Iconify (Window : access Gtk_Window_Record);
   --  Ask to iconify Window.
   --  Note that you shouldn't assume the window is definitely iconified
   --  afterward, because other entities (e.g. the user or window manager)
   --  could deiconify it again, or there may not be a window manager in which
   --  case iconification isn't possible, etc. But normally the window will end
   --  up iconified. Just don't write code that crashes if not.
   --
   --  It's permitted to call this function before showing a window,
   --  in which case the window will be iconified before it ever appears
   --  onscreen.
   --
   --  You can track iconification via the "window_state_event" signal
   --  on Gtk_Widget.

   procedure Deiconify (Window : access Gtk_Window_Record);
   --  Ask to deiconify Window.
   --  Note that you shouldn't assume the window is definitely deiconified
   --  afterward, because other entities (e.g. the user or window manager)
   --  could iconify it again before your code which assumes deiconification
   --  gets to run.
   --
   --  You can track iconification via the "window_state_event" signal
   --  on Gtk_Widget.

   procedure Stick (Window : access Gtk_Window_Record);
   --  Ask to stick Window, which means that it will appear on all user
   --  desktops. Note that you shouldn't assume the window is definitely
   --  stuck afterward, because other entities (e.g. the user or window
   --  manager) could unstick it again, and some window managers do not
   --  support sticking windows. But normally the window will end up
   --  stuck.
   --
   --  It's permitted to call this function before showing a window.
   --
   --  You can track stickiness via the "window_state_event" signal
   --  on Gtk_Widget.

   procedure Unstick (Window : access Gtk_Window_Record);
   --  Ask to unstick Window, which means that it will appear on only
   --  one of the user's desktops. Note that you shouldn't assume the
   --  window is definitely unstuck afterward, because other entities
   --  (e.g. the user or window manager) could stick it again. But
   --  normally the window will end up stuck.
   --
   --  You can track stickiness via the "window_state_event" signal
   --  on Gtk_Widget.

   procedure Maximize (Window : access Gtk_Window_Record);
   --  Ask to maximize Window, so that it becomes full-screen.
   --  Note that you shouldn't assume the window is definitely maximized
   --  afterward, because other entities (e.g. the user or window manager)
   --  could unmaximize it again, and not all window managers support
   --  maximization. But normally the window will end up maximized.
   --
   --  It's permitted to call this function before showing a window,
   --  in which case the window will be maximized when it appears onscreen
   --  initially.
   --
   --  You can track maximization via the "window_state_event" signal
   --  on Gtk_Widget.

   procedure Unmaximize (Window : access Gtk_Window_Record);
   --  Ask to unmaximize Window.
   --  Note that you shouldn't assume the window is definitely unmaximized
   --  afterward, because other entities (e.g. the user or window manager)
   --  could maximize it again, and not all window managers honor requests to
   --  unmaximize. But normally the window will end up unmaximized.
   --
   --  You can track maximization via the "window_state_event" signal
   --  on Gtk_Widget.

   function Get_Transient_For
     (Window : access Gtk_Window_Record) return Gtk_Window;
   --  Return the window for which this one is a temporary window.
   --  See Set_Transient_For below for more information on transient windows.
   --  null is returned if there is no such window.

   --  <doc_ignore>
   procedure Set_Policy
     (Window       : access Gtk_Window_Record;
      Allow_Shrink : Boolean;
      Allow_Grow   : Boolean;
      Auto_Shrink  : Boolean);
   --  Specify the behavior of the window with regards to size modifications.
   --  Default values when the window is created are:
   --    Allow_Shrink => False,
   --    Allow_Grow   => True,
   --    Auto_Shrink  => False.
   --
   --  If Allow_Shrink is False, then the minimum size of the window is
   --  calculated once depending on its children, and the window can never be
   --  smaller.
   --  If Allow_Grow is False, then the maximum size of the window is
   --  calculated once depending on its children, and the window can never be
   --  bigger.
   --  If Auto_Shrink if False, then the window is not shrinked when its
   --  content changes.
   --  pragma Deprecated (Set_Policy);
   --  </doc_ignore>

   procedure Set_Default_Size
     (Window : access Gtk_Window_Record; Width : Gint; Height : Gint);
   --  Sets the default size of a window. If the window's "natural" size (its
   --  size request) is larger than the default, the default will be
   --  ignored. More generally, if the default size does not obey the geometry
   --  hints for the window (Set_Geometry_Hints can be used to set these
   --  explicitly), the default size will be clamped to the nearest permitted
   --  size.
   --
   --  Unlike Gtk.Widget.Set_Size_Request, which sets a size request for a
   --  widget and thus would keep users from shrinking the window, this
   --  function only sets the initial size, just as if the user had resized the
   --  window themselves. Users can still shrink the window again as they
   --  normally would. Setting a default size of -1 means to use the "natural"
   --  default size (the size request of the window).
   --
   --  For more control over a window's initial size and how resizing works,
   --  investigate Set_Geometry_Hints.
   --
   --  For some uses, Resize is a more appropriate function.  Resize changes
   --  the current size of the window, rather than the size to be used on
   --  initial display. Resize always affects the window itself, not the
   --  geometry widget.
   --
   --  The default size of a window only affects the first time a window is
   --  shown; if a window is hidden and re-shown, it will remember the size it
   --  had prior to hiding, rather than using the default size.
   --
   --  Windows can't actually be 0x0 in size, they must be at least 1x1, but
   --  passing 0 for @width and @height is OK, resulting in a 1x1 default size.
   --
   --  This has no effect on Popup windows (set in call to Gtk_New).

   procedure Resize
     (Window : access Gtk_Window_Record;
      Width, Height : Gint);
   --  Resize the window as if the user had done so, obeying geometry
   --  constraints. The default geometry constraint is that windows may
   --  not be smaller than their size request; to override this
   --  constraint, call Gtk.Widget.Set_Size_Request to set the window's
   --  request to a smaller value.
   --
   --  If Resize is called before showing a window for the -- first time, it
   --  overrides any default size set with -- Set_Default_Size.
   --
   --  Windows may not be resized smaller than 1 by 1 pixels. However, as a
   --  special case, if both Width and Height are set to -1, the best requested
   --  size is recomputed for the window, and used.

   procedure Add_Mnemonic
     (Window : access Gtk_Window_Record;
      Keyval : Gdk.Types.Gdk_Key_Type;
      Target : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Add a mnemonic to this window.
   --  Target will receive the "activate" signal when Keyval is pressed inside
   --  the window.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Type_Property
   --    Type:  Gtk_Window_Type
   --    Flags: read-write (construct only)
   --    Descr: The type of the window
   --    See also:  Gtk_New
   --
   --  - Name:  Title_Property
   --    Type:  String
   --    Flags: read-write
   --    Descr: The title of the window
   --    See also:  Set_Title and Get_Title
   --
   --  - Name:  Auto_Shrink_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: If TRUE, the window automatically shrinks to its size request
   --           anytime a resize occurs. Don't use this feature, it makes no
   --           sense.
   --    See also:  Set_Policy
   --
   --  - Name:  Allow_Shrink_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: If TRUE, the window has no mimimum size. Don't use this
   --           feature, it makes no sense
   --    See also:  Set_Policy
   --
   --  - Name:  Allow_Grow_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: If TRUE, users can expand the window beyond its minimum size.
   --    See also:  Set_Policy
   --
   --  - Name:  Modal_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: If TRUE, the window is modal (other windows are not usable
   --           while this one is up)
   --    See also:  Set_Modal
   --
   --  - Name:  Win_Pos_Property
   --    Type:  Gtk_Window_Position
   --    Flags: read-write
   --    Descr: The initial position of the window.
   --    See also:  Set_Position
   --
   --  - Name:  Default_Width_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: The default width of the window, or 0 to use the size request.
   --    See also:  Set_Default_Size
   --
   --  - Name:  Default_Height_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: The default height of the window, or 0 to use the size request.
   --    See also:  Set_Default_Size
   --
   --  - Name:  Destroy_With_Parent_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: If this window should be destroyed when the parent is destroyed
   --    See also:  Set_Destroy_With_Parent
   --
   --  </properties>

   Type_Property             : constant Gtk.Enums.Property_Gtk_Window_Type;
   Title_Property            : constant Glib.Properties.Property_String;
   Auto_Shrink_Property      : constant Glib.Properties.Property_Boolean;
   Allow_Shrink_Property     : constant Glib.Properties.Property_Boolean;
   Allow_Grow_Property       : constant Glib.Properties.Property_Boolean;
   Modal_Property            : constant Glib.Properties.Property_Boolean;
   Win_Pos_Property          : constant Gtk.Enums.Property_Gtk_Window_Position;
   Default_Width_Property    : constant Glib.Properties.Property_Int;
   Default_Height_Property   : constant Glib.Properties.Property_Int;
   Destroy_With_Parent_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "set_focus"
   --    procedure Handler (Window : access Gtk_Window_Record'Class;
   --                       Widget : access Gtk_Widget_Record'Class);
   --
   --    Called when the widget that has the focus has changed.
   --    This widget gets all keyboard events that happen in the window.
   --    You should not block the emission of this signal, since most of
   --    the work is done in the default handler.
   --
   --  - "frame_event"
   --    function Handler
   --      (Window : access Gtk_Window_Record'Class;
   --       Event  : Gdk.Event.Gdk_Event) return Boolean;
   --
   --    If this function is called on a window before it is realized
   --    or showed it will have a "frame" window around widget-window.
   --    Called when the "frame" window set around a window receives events.
   --    This is mainly used by the linux-fb port to implement managed
   --    windows, but it could concievably be used by X-programs that
   --    want to do their own window decorations.
   --  </signals>

private
   type Gtk_Window_Record is new Bin.Gtk_Bin_Record with null record;

   Type_Property              : constant Gtk.Enums.Property_Gtk_Window_Type :=
     Gtk.Enums.Build ("type");
   Title_Property               : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Auto_Shrink_Property         : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("auto_shrink");
   Allow_Shrink_Property        : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("allow_shrink");
   Allow_Grow_Property          : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("allow_grow");
   Modal_Property               : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("modal");
   Win_Pos_Property       : constant Gtk.Enums.Property_Gtk_Window_Position :=
     Gtk.Enums.Build ("window_position");
   Default_Width_Property       : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("default_width");
   Default_Height_Property      : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("default_height");
   Destroy_With_Parent_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("destroy_with_parent");

   pragma Import (C, Get_Type, "gtk_window_get_type");
end Gtk.Window;

--  <example>
--  <include>../examples/documentation/banner.adb</include>
--  </example>

--  missing:
--  Get_Role
--  Set_Focus
--  Get_Focus
--  Get_Type_Hint
--  Get_Destroy_With_Parent
--  Get_Has_Frame
--  Get_Frame_Dimensions
--  Get_Decorated
--  Set_Icon_List
--  Get_Icon_List
--  Set_Icon
--  Get_Icon
--  Set_Default_Icon_List
--  Get_Default_Icon_List
--  Get_Modal
--  Remove_Mnemonic
--  Mnemonic_Activate
--  Set_Mnemonic_Modifier
--  Get_Mnemonic_Modifier
--  Begin_Resize_Drag
--  Begin_Move_Drag
--  Get_Default_Size
--  Resize
--  Get_Size
--  Move
--  Get_Position
--  Parse_Geometry
--  Reshow_With_Initial_Size
--  Group_Get_Type
--  Group_New
--  Group_Add_Window
--  Group_Remove_Window
