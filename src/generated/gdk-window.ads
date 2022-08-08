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


pragma Warnings (Off, "*is already use-visible*");
with Cairo;                   use Cairo;
with Cairo.Region;            use Cairo.Region;
with Gdk.Color;               use Gdk.Color;
with Gdk.Device;              use Gdk.Device;
with Gdk.Display;             use Gdk.Display;
with Gdk.Drawing_Context;     use Gdk.Drawing_Context;
with Gdk.Event;               use Gdk.Event;
with Gdk.Frame_Clock;         use Gdk.Frame_Clock;
with Gdk.GLContext;           use Gdk.GLContext;
with Gdk.RGBA;                use Gdk.RGBA;
with Gdk.Rectangle;           use Gdk.Rectangle;
with Gdk.Screen;              use Gdk.Screen;
with Gdk.Types;               use Gdk.Types;
with Gdk.Visual;              use Gdk.Visual;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Glist;              use Glib.Glist;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;

package Gdk.Window is

   type Gdk_Window_Class is (
      Input_Output,
      Input_Only);
   pragma Convention (C, Gdk_Window_Class);
   --  Gdk_Input_Output windows are the standard kind of window you might
   --  expect. Such windows receive events and are also displayed on screen.
   --  Gdk_Input_Only windows are invisible; they are usually placed above
   --  other windows in order to trap or filter the events. You can't draw on
   --  Gdk_Input_Only windows.

   type Gdk_Window_Edge is (
      Window_Edge_North_West,
      Window_Edge_North,
      Window_Edge_North_East,
      Window_Edge_West,
      Window_Edge_East,
      Window_Edge_South_West,
      Window_Edge_South,
      Window_Edge_South_East);
   pragma Convention (C, Gdk_Window_Edge);
   --  Determines a window edge or corner.

   type Gdk_Window_Type is (
      Window_Root,
      Window_Toplevel,
      Window_Child,
      Window_Temp,
      Window_Foreign,
      Window_Offscreen,
      Window_Subsurface);
   pragma Convention (C, Gdk_Window_Type);
   --  Describes the kind of window.

   type Gdk_Window_Attributes_Type is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_Window_Attributes_Type);
   --  Used to indicate which fields in the Gdk_Window_Attr struct should be
   --  honored. For example, if you filled in the "cursor" and "x" fields of
   --  Gdk_Window_Attr, pass "Gdk_Wa_X | Gdk_Wa_Cursor" to gdk_window_new.
   --  Fields in Gdk_Window_Attr not covered by a bit in this enum are
   --  required; for example, the Width/Height, Wclass, and Window_Type fields
   --  are required, they have no corresponding flag in
   --  Gdk.Window.Gdk_Window_Attributes_Type.

   Wa_Title : constant Gdk_Window_Attributes_Type := 2;
   Wa_X : constant Gdk_Window_Attributes_Type := 4;
   Wa_Y : constant Gdk_Window_Attributes_Type := 8;
   Wa_Cursor : constant Gdk_Window_Attributes_Type := 16;
   Wa_Visual : constant Gdk_Window_Attributes_Type := 32;
   Wa_Wmclass : constant Gdk_Window_Attributes_Type := 64;
   Wa_Noredir : constant Gdk_Window_Attributes_Type := 128;
   Wa_Type_Hint : constant Gdk_Window_Attributes_Type := 256;

   type Gdk_Window_Hints is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_Window_Hints);
   --  Used to indicate which fields of a Gdk.Window.Gdk_Geometry struct
   --  should be paid attention to. Also, the presence/absence of Gdk_Hint_Pos,
   --  Gdk_Hint_User_Pos, and Gdk_Hint_User_Size is significant, though they
   --  don't directly refer to Gdk.Window.Gdk_Geometry fields.
   --  Gdk_Hint_User_Pos will be set automatically by Gtk.Window.Gtk_Window if
   --  you call Gtk.Window.Move. Gdk_Hint_User_Pos and Gdk_Hint_User_Size
   --  should be set if the user specified a size/position using a --geometry
   --  command-line argument; Gtk.Window.Parse_Geometry automatically sets
   --  these flags.

   Hint_Pos : constant Gdk_Window_Hints := 1;
   Hint_Min_Size : constant Gdk_Window_Hints := 2;
   Hint_Max_Size : constant Gdk_Window_Hints := 4;
   Hint_Base_Size : constant Gdk_Window_Hints := 8;
   Hint_Aspect : constant Gdk_Window_Hints := 16;
   Hint_Resize_Inc : constant Gdk_Window_Hints := 32;
   Hint_Win_Gravity : constant Gdk_Window_Hints := 64;
   Hint_User_Pos : constant Gdk_Window_Hints := 128;
   Hint_User_Size : constant Gdk_Window_Hints := 256;

   type Gdk_Window_Type_Hint is (
      Window_Type_Hint_Normal,
      Window_Type_Hint_Dialog,
      Window_Type_Hint_Menu,
      Window_Type_Hint_Toolbar,
      Window_Type_Hint_Splashscreen,
      Window_Type_Hint_Utility,
      Window_Type_Hint_Dock,
      Window_Type_Hint_Desktop,
      Window_Type_Hint_Dropdown_Menu,
      Window_Type_Hint_Popup_Menu,
      Window_Type_Hint_Tooltip,
      Window_Type_Hint_Notification,
      Window_Type_Hint_Combo,
      Window_Type_Hint_Dnd);
   pragma Convention (C, Gdk_Window_Type_Hint);
   --  These are hints for the window manager that indicate what type of
   --  function the window has. The window manager can use this when
   --  determining decoration and behaviour of the window. The hint must be set
   --  before mapping the window.
   --
   --  See the [Extended Window Manager
   --  Hints](http://www.freedesktop.org/Standards/wm-spec) specification for
   --  more details about window types.

   type Gdk_WMDecoration is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_WMDecoration);
   --  These are hints originally defined by the Motif toolkit. The window
   --  manager can use them when determining how to decorate the window. The
   --  hint must be set before mapping the window.

   Decor_All : constant Gdk_WMDecoration := 1;
   Decor_Border : constant Gdk_WMDecoration := 2;
   Decor_Resizeh : constant Gdk_WMDecoration := 4;
   Decor_Title : constant Gdk_WMDecoration := 8;
   Decor_Menu : constant Gdk_WMDecoration := 16;
   Decor_Minimize : constant Gdk_WMDecoration := 32;
   Decor_Maximize : constant Gdk_WMDecoration := 64;

   type Gdk_WMFunction is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_WMFunction);
   --  These are hints originally defined by the Motif toolkit. The window
   --  manager can use them when determining the functions to offer for the
   --  window. The hint must be set before mapping the window.

   Func_All : constant Gdk_WMFunction := 1;
   Func_Resize : constant Gdk_WMFunction := 2;
   Func_Move : constant Gdk_WMFunction := 4;
   Func_Minimize : constant Gdk_WMFunction := 8;
   Func_Maximize : constant Gdk_WMFunction := 16;
   Func_Close : constant Gdk_WMFunction := 32;

   type Gdk_Gravity is (
      Gravity_North_West,
      Gravity_North,
      Gravity_North_East,
      Gravity_West,
      Gravity_Center,
      Gravity_East,
      Gravity_South_West,
      Gravity_South,
      Gravity_South_East,
      Gravity_Static);
   pragma Convention (C, Gdk_Gravity);
   --  Defines the reference point of a window and the meaning of coordinates
   --  passed to Gtk.Window.Move. See Gtk.Window.Move and the "implementation
   --  notes" section of the [Extended Window Manager
   --  Hints](http://www.freedesktop.org/Standards/wm-spec) specification for
   --  more details.

   for Gdk_Gravity use (
      Gravity_North_West => 1,
      Gravity_North => 2,
      Gravity_North_East => 3,
      Gravity_West => 4,
      Gravity_Center => 5,
      Gravity_East => 6,
      Gravity_South_West => 7,
      Gravity_South => 8,
      Gravity_South_East => 9,
      Gravity_Static => 10);

   type Gdk_Fullscreen_Mode is (
      On_Current_Monitor,
      On_All_Monitors);
   pragma Convention (C, Gdk_Fullscreen_Mode);
   --  Indicates which monitor (in a multi-head setup) a window should span
   --  over when in fullscreen mode.

   type Gdk_Anchor_Hints is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_Anchor_Hints);
   --  Positioning hints for aligning a window relative to a rectangle.
   --
   --  These hints determine how the window should be positioned in the case
   --  that the window would fall off-screen if placed in its ideal position.
   --
   --  For example, Gdk.Anchor_Flip_X will replace Gdk.Gravity_North_West with
   --  Gdk.Gravity_North_East and vice versa if the window extends beyond the
   --  left or right edges of the monitor.
   --
   --  If Gdk.Anchor_Slide_X is set, the window can be shifted horizontally to
   --  fit on-screen. If Gdk.Anchor_Resize_X is set, the window can be shrunken
   --  horizontally to fit.
   --
   --  In general, when multiple flags are set, flipping should take
   --  precedence over sliding, which should take precedence over resizing.

   Anchor_Flip_X : constant Gdk_Anchor_Hints := 1;
   Anchor_Flip_Y : constant Gdk_Anchor_Hints := 2;
   Anchor_Slide_X : constant Gdk_Anchor_Hints := 4;
   Anchor_Slide_Y : constant Gdk_Anchor_Hints := 8;
   Anchor_Resize_X : constant Gdk_Anchor_Hints := 16;
   Anchor_Resize_Y : constant Gdk_Anchor_Hints := 32;
   Anchor_Flip : constant Gdk_Anchor_Hints := 3;
   Anchor_Slide : constant Gdk_Anchor_Hints := 12;
   Anchor_Resize : constant Gdk_Anchor_Hints := 48;

   type Gdk_Geometry is record
      Min_Width : Glib.Gint := 0;
      Min_Height : Glib.Gint := 0;
      Max_Width : Glib.Gint := 0;
      Max_Height : Glib.Gint := 0;
      Base_Width : Glib.Gint := 0;
      Base_Height : Glib.Gint := 0;
      Width_Inc : Glib.Gint := 0;
      Height_Inc : Glib.Gint := 0;
      Min_Aspect : Gdouble;
      Max_Aspect : Gdouble;
      Win_Gravity : Gdk_Gravity;
   end record;
   pragma Convention (C, Gdk_Geometry);

   function From_Object_Free (B : access Gdk_Geometry) return Gdk_Geometry;
   pragma Inline (From_Object_Free);
   --  The Gdk.Window.Gdk_Geometry struct gives the window manager information
   --  about a window's geometry constraints. Normally you would set these on
   --  the GTK+ level using Gtk.Window.Set_Geometry_Hints.
   --  Gtk.Window.Gtk_Window then sets the hints on the Gdk.Gdk_Window it
   --  creates.
   --
   --  Gdk.Window.Set_Geometry_Hints expects the hints to be fully valid
   --  already and simply passes them to the window manager; in contrast,
   --  Gtk.Window.Set_Geometry_Hints performs some interpretation. For example,
   --  Gtk.Window.Gtk_Window will apply the hints to the geometry widget
   --  instead of the toplevel window, if you set a geometry widget. Also, the
   --  Min_Width/Min_Height/Max_Width/Max_Height fields may be set to -1, and
   --  Gtk.Window.Gtk_Window will substitute the size request of the window or
   --  geometry widget. If the minimum size hint is not provided,
   --  Gtk.Window.Gtk_Window will use its requisition as the minimum size. If
   --  the minimum size is provided and a geometry widget is set,
   --  Gtk.Window.Gtk_Window will take the minimum size as the minimum size of
   --  the geometry widget rather than the entire window. The base size is
   --  treated similarly.
   --
   --  The canonical use-case for Gtk.Window.Set_Geometry_Hints is to get a
   --  terminal widget to resize properly. Here, the terminal text area should
   --  be the geometry widget; Gtk.Window.Gtk_Window will then automatically
   --  set the base size to the size of other widgets in the terminal window,
   --  such as the menubar and scrollbar. Then, the Width_Inc and Height_Inc
   --  fields should be set to the size of one character in the terminal.
   --  Finally, the base size should be set to the size of one character. The
   --  net effect is that the minimum size of the terminal will have a 1x1
   --  character terminal area, and only terminal sizes on the "character grid"
   --  will be allowed.
   --
   --  Here's an example of how the terminal example would be implemented,
   --  assuming a terminal area widget called "terminal" and a toplevel window
   --  "toplevel":
   --
   --  |[<!-- language="C" --> GdkGeometry hints;
   --
   --  hints.base_width = terminal->char_width; hints.base_height =
   --  terminal->char_height; hints.min_width = terminal->char_width;
   --  hints.min_height = terminal->char_height; hints.width_inc =
   --  terminal->char_width; hints.height_inc = terminal->char_height;
   --
   --  gtk_window_set_geometry_hints (GTK_WINDOW (toplevel), GTK_WIDGET
   --  (terminal), &hints, GDK_HINT_RESIZE_INC | GDK_HINT_MIN_SIZE |
   --  GDK_HINT_BASE_SIZE); ]|
   --
   --  The other useful fields are the Min_Aspect and Max_Aspect fields; these
   --  contain a width/height ratio as a floating point number. If a geometry
   --  widget is set, the aspect applies to the geometry widget rather than the
   --  entire window. The most common use of these hints is probably to set
   --  Min_Aspect and Max_Aspect to the same value, thus forcing the window to
   --  keep a constant aspect ratio.

   function Convert (R : Gdk.Gdk_Window) return System.Address;
   function Convert (R : System.Address) return Gdk.Gdk_Window;
   package Gdk_Window_List is new Generic_List (Gdk.Gdk_Window);

   ---------------
   -- Callbacks --
   ---------------

   type Gdk_Window_Child_Func is access function (Window : Gdk.Gdk_Window) return Boolean;
   --  A function of this type is passed to
   --  Gdk.Window.Invalidate_Maybe_Recurse. It gets called for each child of
   --  the window to determine whether to recursively invalidate it or now.
   --  "window": a Gdk.Gdk_Window

   type Gdk_Window_Invalidate_Handler_Func is access procedure
     (Window : Gdk.Gdk_Window;
      Region : Cairo.Region.Cairo_Region);
   --  Whenever some area of the window is invalidated (directly in the window
   --  or in a child window) this gets called with Region in the coordinate
   --  space of Window. You can use Region to just keep track of the dirty
   --  region, or you can actually change Region in case you are doing display
   --  tricks like showing a child in multiple places.
   --  Since: gtk+ 3.10
   --  "window": a Gdk.Gdk_Window
   --  "region": a cairo_region_t

   pragma Convention (C, Gdk_Window_Invalidate_Handler_Func);

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Window_Class_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Window_Class);
   type Property_Gdk_Window_Class is new Gdk_Window_Class_Properties.Property;

   package Gdk_Window_Edge_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Window_Edge);
   type Property_Gdk_Window_Edge is new Gdk_Window_Edge_Properties.Property;

   package Gdk_Window_Type_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Window_Type);
   type Property_Gdk_Window_Type is new Gdk_Window_Type_Properties.Property;

   package Gdk_Window_Attributes_Type_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Window_Attributes_Type);
   type Property_Gdk_Window_Attributes_Type is new Gdk_Window_Attributes_Type_Properties.Property;

   package Gdk_Window_Hints_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Window_Hints);
   type Property_Gdk_Window_Hints is new Gdk_Window_Hints_Properties.Property;

   package Gdk_Window_Type_Hint_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Window_Type_Hint);
   type Property_Gdk_Window_Type_Hint is new Gdk_Window_Type_Hint_Properties.Property;

   package Gdk_WMDecoration_Properties is
      new Generic_Internal_Discrete_Property (Gdk_WMDecoration);
   type Property_Gdk_WMDecoration is new Gdk_WMDecoration_Properties.Property;

   package Gdk_WMFunction_Properties is
      new Generic_Internal_Discrete_Property (Gdk_WMFunction);
   type Property_Gdk_WMFunction is new Gdk_WMFunction_Properties.Property;

   package Gdk_Gravity_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Gravity);
   type Property_Gdk_Gravity is new Gdk_Gravity_Properties.Property;

   package Gdk_Fullscreen_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Fullscreen_Mode);
   type Property_Gdk_Fullscreen_Mode is new Gdk_Fullscreen_Mode_Properties.Property;

   package Gdk_Anchor_Hints_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Anchor_Hints);
   type Property_Gdk_Anchor_Hints is new Gdk_Anchor_Hints_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New
      (Self            : out Gdk_Window;
       Parent          : Gdk.Gdk_Window;
       Attributes      : Gdk.Gdk_Window_Attr;
       Attributes_Mask : Gdk_Window_Attributes_Type);
   --  Creates a new Gdk.Gdk_Window using the attributes from Attributes. See
   --  Gdk_Window_Attr and Gdk.Window.Gdk_Window_Attributes_Type for more
   --  details. Note: to use this on displays other than the default display,
   --  Parent must be specified.
   --  "parent": a Gdk.Gdk_Window, or null to create the window as a child of
   --  the default root window for the default display.
   --  "attributes": attributes of the new window
   --  "attributes_mask": mask indicating which fields in Attributes are valid

   function Gdk_Window_New
      (Parent          : Gdk.Gdk_Window;
       Attributes      : Gdk.Gdk_Window_Attr;
       Attributes_Mask : Gdk_Window_Attributes_Type) return Gdk_Window;
   --  Creates a new Gdk.Gdk_Window using the attributes from Attributes. See
   --  Gdk_Window_Attr and Gdk.Window.Gdk_Window_Attributes_Type for more
   --  details. Note: to use this on displays other than the default display,
   --  Parent must be specified.
   --  "parent": a Gdk.Gdk_Window, or null to create the window as a child of
   --  the default root window for the default display.
   --  "attributes": attributes of the new window
   --  "attributes_mask": mask indicating which fields in Attributes are valid

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_window_get_type");

   -------------
   -- Methods --
   -------------

   procedure Beep (Self : Gdk.Gdk_Window);
   pragma Import (C, Beep, "gdk_window_beep");
   --  Emits a short beep associated to Window in the appropriate display, if
   --  supported. Otherwise, emits a short beep on the display just as
   --  Gdk.Display.Beep.
   --  Since: gtk+ 2.12

   function Begin_Draw_Frame
      (Self   : Gdk.Gdk_Window;
       Region : Cairo.Region.Cairo_Region)
       return Gdk.Drawing_Context.Gdk_Drawing_Context;
   --  Indicates that you are beginning the process of redrawing Region on
   --  Window, and provides you with a Gdk.Drawing_Context.Gdk_Drawing_Context.
   --  If Window is a top level Gdk.Gdk_Window, backed by a native window
   --  implementation, a backing store (offscreen buffer) large enough to
   --  contain Region will be created. The backing store will be initialized
   --  with the background color or background surface for Window. Then, all
   --  drawing operations performed on Window will be diverted to the backing
   --  store. When you call gdk_window_end_frame, the contents of the backing
   --  store will be copied to Window, making it visible on screen. Only the
   --  part of Window contained in Region will be modified; that is, drawing
   --  operations are clipped to Region.
   --  The net result of all this is to remove flicker, because the user sees
   --  the finished product appear all at once when you call
   --  Gdk.Window.End_Draw_Frame. If you draw to Window directly without
   --  calling Gdk.Window.Begin_Draw_Frame, the user may see flicker as
   --  individual drawing operations are performed in sequence.
   --  When using GTK+, the widget system automatically places calls to
   --  Gdk.Window.Begin_Draw_Frame and Gdk.Window.End_Draw_Frame around
   --  emissions of the `GtkWidget::draw` signal. That is, if you're drawing
   --  the contents of the widget yourself, you can assume that the widget has
   --  a cleared background, is already set as the clip region, and already has
   --  a backing store. Therefore in most cases, application code in GTK does
   --  not need to call Gdk.Window.Begin_Draw_Frame explicitly.
   --  Since: gtk+ 3.22
   --  "region": a Cairo region

   procedure Begin_Move_Drag
      (Self      : Gdk.Gdk_Window;
       Button    : Glib.Gint;
       Root_X    : Glib.Gint;
       Root_Y    : Glib.Gint;
       Timestamp : Guint32);
   pragma Import (C, Begin_Move_Drag, "gdk_window_begin_move_drag");
   --  Begins a window move operation (for a toplevel window).
   --  This function assumes that the drag is controlled by the client pointer
   --  device, use Gdk.Window.Begin_Move_Drag_For_Device to begin a drag with a
   --  different device.
   --  "button": the button being used to drag, or 0 for a keyboard-initiated
   --  drag
   --  "root_x": root window X coordinate of mouse click that began the drag
   --  "root_y": root window Y coordinate of mouse click that began the drag
   --  "timestamp": timestamp of mouse click that began the drag

   procedure Begin_Move_Drag_For_Device
      (Self      : Gdk.Gdk_Window;
       Device    : not null access Gdk.Device.Gdk_Device_Record'Class;
       Button    : Glib.Gint;
       Root_X    : Glib.Gint;
       Root_Y    : Glib.Gint;
       Timestamp : Guint32);
   --  Begins a window move operation (for a toplevel window). You might use
   --  this function to implement a "window move grip," for example. The
   --  function works best with window managers that support the [Extended
   --  Window Manager Hints](http://www.freedesktop.org/Standards/wm-spec) but
   --  has a fallback implementation for other window managers.
   --  Since: gtk+ 3.4
   --  "device": the device used for the operation
   --  "button": the button being used to drag, or 0 for a keyboard-initiated
   --  drag
   --  "root_x": root window X coordinate of mouse click that began the drag
   --  "root_y": root window Y coordinate of mouse click that began the drag
   --  "timestamp": timestamp of mouse click that began the drag

   procedure Begin_Paint_Rect
      (Self      : Gdk.Gdk_Window;
       Rectangle : Gdk.Rectangle.Gdk_Rectangle);
   pragma Import (C, Begin_Paint_Rect, "gdk_window_begin_paint_rect");
   pragma Obsolescent (Begin_Paint_Rect);
   --  A convenience wrapper around Gdk.Window.Begin_Paint_Region which
   --  creates a rectangular region for you. See Gdk.Window.Begin_Paint_Region
   --  for details.
   --  Deprecated since 3.22, 1
   --  "rectangle": rectangle you intend to draw to

   procedure Begin_Paint_Region
      (Self   : Gdk.Gdk_Window;
       Region : Cairo.Region.Cairo_Region);
   pragma Import (C, Begin_Paint_Region, "gdk_window_begin_paint_region");
   pragma Obsolescent (Begin_Paint_Region);
   --  Indicates that you are beginning the process of redrawing Region. A
   --  backing store (offscreen buffer) large enough to contain Region will be
   --  created. The backing store will be initialized with the background color
   --  or background surface for Window. Then, all drawing operations performed
   --  on Window will be diverted to the backing store. When you call
   --  Gdk.Window.End_Paint, the backing store will be copied to Window, making
   --  it visible onscreen. Only the part of Window contained in Region will be
   --  modified; that is, drawing operations are clipped to Region.
   --  The net result of all this is to remove flicker, because the user sees
   --  the finished product appear all at once when you call
   --  Gdk.Window.End_Paint. If you draw to Window directly without calling
   --  Gdk.Window.Begin_Paint_Region, the user may see flicker as individual
   --  drawing operations are performed in sequence. The clipping and
   --  background-initializing features of Gdk.Window.Begin_Paint_Region are
   --  conveniences for the programmer, so you can avoid doing that work
   --  yourself.
   --  When using GTK+, the widget system automatically places calls to
   --  Gdk.Window.Begin_Paint_Region and Gdk.Window.End_Paint around emissions
   --  of the expose_event signal. That is, if you're writing an expose event
   --  handler, you can assume that the exposed area in
   --  Gdk.Event.Gdk_Event_Expose has already been cleared to the window
   --  background, is already set as the clip region, and already has a backing
   --  store. Therefore in most cases, application code need not call
   --  Gdk.Window.Begin_Paint_Region. (You can disable the automatic calls
   --  around expose events on a widget-by-widget basis by calling
   --  Gtk.Widget.Set_Double_Buffered.)
   --  If you call this function multiple times before calling the matching
   --  Gdk.Window.End_Paint, the backing stores are pushed onto a stack.
   --  Gdk.Window.End_Paint copies the topmost backing store onscreen,
   --  subtracts the topmost region from all other regions in the stack, and
   --  pops the stack. All drawing operations affect only the topmost backing
   --  store in the stack. One matching call to Gdk.Window.End_Paint is
   --  required for each call to Gdk.Window.Begin_Paint_Region.
   --  Deprecated since 3.22, 1
   --  "region": region you intend to draw to

   procedure Begin_Resize_Drag
      (Self      : Gdk.Gdk_Window;
       Edge      : Gdk_Window_Edge;
       Button    : Glib.Gint;
       Root_X    : Glib.Gint;
       Root_Y    : Glib.Gint;
       Timestamp : Guint32);
   pragma Import (C, Begin_Resize_Drag, "gdk_window_begin_resize_drag");
   --  Begins a window resize operation (for a toplevel window).
   --  This function assumes that the drag is controlled by the client pointer
   --  device, use Gdk.Window.Begin_Resize_Drag_For_Device to begin a drag with
   --  a different device.
   --  "edge": the edge or corner from which the drag is started
   --  "button": the button being used to drag, or 0 for a keyboard-initiated
   --  drag
   --  "root_x": root window X coordinate of mouse click that began the drag
   --  "root_y": root window Y coordinate of mouse click that began the drag
   --  "timestamp": timestamp of mouse click that began the drag (use
   --  Gdk.Event.Get_Time)

   procedure Begin_Resize_Drag_For_Device
      (Self      : Gdk.Gdk_Window;
       Edge      : Gdk_Window_Edge;
       Device    : not null access Gdk.Device.Gdk_Device_Record'Class;
       Button    : Glib.Gint;
       Root_X    : Glib.Gint;
       Root_Y    : Glib.Gint;
       Timestamp : Guint32);
   --  Begins a window resize operation (for a toplevel window). You might use
   --  this function to implement a "window resize grip," for example; in fact
   --  Gtk.Status_Bar.Gtk_Status_Bar uses it. The function works best with
   --  window managers that support the [Extended Window Manager
   --  Hints](http://www.freedesktop.org/Standards/wm-spec) but has a fallback
   --  implementation for other window managers.
   --  Since: gtk+ 3.4
   --  "edge": the edge or corner from which the drag is started
   --  "device": the device used for the operation
   --  "button": the button being used to drag, or 0 for a keyboard-initiated
   --  drag
   --  "root_x": root window X coordinate of mouse click that began the drag
   --  "root_y": root window Y coordinate of mouse click that began the drag
   --  "timestamp": timestamp of mouse click that began the drag (use
   --  Gdk.Event.Get_Time)

   procedure Configure_Finished (Self : Gdk.Gdk_Window);
   pragma Import (C, Configure_Finished, "gdk_window_configure_finished");
   pragma Obsolescent (Configure_Finished);
   --  Does nothing, present only for compatiblity.
   --  Since: gtk+ 2.6
   --  Deprecated since 3.8, 1

   procedure Coords_From_Parent
      (Self     : Gdk.Gdk_Window;
       Parent_X : Gdouble;
       Parent_Y : Gdouble;
       X        : out Gdouble;
       Y        : out Gdouble);
   pragma Import (C, Coords_From_Parent, "gdk_window_coords_from_parent");
   --  Transforms window coordinates from a parent window to a child window,
   --  where the parent window is the normal parent as returned by
   --  Gdk.Window.Get_Parent for normal windows, and the window's embedder as
   --  returned by gdk_offscreen_window_get_embedder for offscreen windows.
   --  For normal windows, calling this function is equivalent to subtracting
   --  the return values of Gdk.Window.Get_Position from the parent
   --  coordinates. For offscreen windows however (which can be arbitrarily
   --  transformed), this function calls the GdkWindow::from-embedder: signal
   --  to translate the coordinates.
   --  You should always use this function when writing generic code that
   --  walks down a window hierarchy.
   --  See also: Gdk.Window.Coords_To_Parent
   --  Since: gtk+ 2.22
   --  "parent_x": X coordinate in parent's coordinate system
   --  "parent_y": Y coordinate in parent's coordinate system
   --  "x": return location for X coordinate in child's coordinate system
   --  "y": return location for Y coordinate in child's coordinate system

   procedure Coords_To_Parent
      (Self     : Gdk.Gdk_Window;
       X        : Gdouble;
       Y        : Gdouble;
       Parent_X : out Gdouble;
       Parent_Y : out Gdouble);
   pragma Import (C, Coords_To_Parent, "gdk_window_coords_to_parent");
   --  Transforms window coordinates from a child window to its parent window,
   --  where the parent window is the normal parent as returned by
   --  Gdk.Window.Get_Parent for normal windows, and the window's embedder as
   --  returned by gdk_offscreen_window_get_embedder for offscreen windows.
   --  For normal windows, calling this function is equivalent to adding the
   --  return values of Gdk.Window.Get_Position to the child coordinates. For
   --  offscreen windows however (which can be arbitrarily transformed), this
   --  function calls the GdkWindow::to-embedder: signal to translate the
   --  coordinates.
   --  You should always use this function when writing generic code that
   --  walks up a window hierarchy.
   --  See also: Gdk.Window.Coords_From_Parent
   --  Since: gtk+ 2.22
   --  "x": X coordinate in child's coordinate system
   --  "y": Y coordinate in child's coordinate system
   --  "parent_x": return location for X coordinate in parent's coordinate
   --  system, or null
   --  "parent_y": return location for Y coordinate in parent's coordinate
   --  system, or null

   function Create_Gl_Context
      (Self : Gdk.Gdk_Window) return Gdk.GLContext.Gdk_GLContext;
   --  Creates a new Gdk.GLContext.Gdk_GLContext matching the framebuffer
   --  format to the visual of the Gdk.Gdk_Window. The context is disconnected
   --  from any particular window or surface.
   --  If the creation of the Gdk.GLContext.Gdk_GLContext failed, Error will
   --  be set.
   --  Before using the returned Gdk.GLContext.Gdk_GLContext, you will need to
   --  call Gdk.GLContext.Make_Current or Gdk.GLContext.Realize.
   --  Since: gtk+ 3.16

   function Create_Similar_Image_Surface
      (Self   : Gdk.Gdk_Window;
       Format : Cairo.Cairo_Format;
       Width  : Glib.Gint;
       Height : Glib.Gint;
       Scale  : Glib.Gint) return Cairo.Cairo_Surface;
   pragma Import (C, Create_Similar_Image_Surface, "gdk_window_create_similar_image_surface");
   --  Create a new image surface that is efficient to draw on the given
   --  Window.
   --  Initially the surface contents are all 0 (transparent if contents have
   --  transparency, black otherwise.)
   --  The Width and Height of the new surface are not affected by the scaling
   --  factor of the Window, or by the Scale argument; they are the size of the
   --  surface in device pixels. If you wish to create an image surface capable
   --  of holding the contents of Window you can use:
   --  |[<!-- language="C" --> int scale = gdk_window_get_scale_factor
   --  (window); int width = gdk_window_get_width (window) * scale; int height
   --  = gdk_window_get_height (window) * scale;
   --  // format is set elsewhere cairo_surface_t *surface =
   --  gdk_window_create_similar_image_surface (window, format, width, height,
   --  scale); ]|
   --  Note that unlike cairo_surface_create_similar_image, the new surface's
   --  device scale is set to Scale, or to the scale factor of Window if Scale
   --  is 0.
   --  Since: gtk+ 3.10
   --  "format": the format for the new surface
   --  "width": width of the new surface
   --  "height": height of the new surface
   --  "scale": the scale of the new surface, or 0 to use same as Window

   function Create_Similar_Surface
      (Self    : Gdk.Gdk_Window;
       Content : Cairo.Cairo_Content;
       Width   : Glib.Gint;
       Height  : Glib.Gint) return Cairo.Cairo_Surface;
   pragma Import (C, Create_Similar_Surface, "gdk_window_create_similar_surface");
   --  Create a new surface that is as compatible as possible with the given
   --  Window. For example the new surface will have the same fallback
   --  resolution and font options as Window. Generally, the new surface will
   --  also use the same backend as Window, unless that is not possible for
   --  some reason. The type of the returned surface may be examined with
   --  cairo_surface_get_type.
   --  Initially the surface contents are all 0 (transparent if contents have
   --  transparency, black otherwise.)
   --  Since: gtk+ 2.22
   --  "content": the content for the new surface
   --  "width": width of the new surface
   --  "height": height of the new surface

   procedure Deiconify (Self : Gdk.Gdk_Window);
   pragma Import (C, Deiconify, "gdk_window_deiconify");
   --  Attempt to deiconify (unminimize) Window. On X11 the window manager may
   --  choose to ignore the request to deiconify. When using GTK+, use
   --  Gtk.Window.Deiconify instead of the Gdk.Gdk_Window variant. Or better
   --  yet, you probably want to use Gtk.Window.Present_With_Time, which raises
   --  the window, focuses it, unminimizes it, and puts it on the current
   --  desktop.

   procedure Destroy (Self : Gdk.Gdk_Window);
   pragma Import (C, Destroy, "gdk_window_destroy");
   --  Destroys the window system resources associated with Window and
   --  decrements Window's reference count. The window system resources for all
   --  children of Window are also destroyed, but the children's reference
   --  counts are not decremented.
   --  Note that a window will not be destroyed automatically when its
   --  reference count reaches zero. You must call this function yourself
   --  before that happens.

   procedure Destroy_Notify (Self : Gdk.Gdk_Window);
   pragma Import (C, Destroy_Notify, "gdk_window_destroy_notify");

   procedure Enable_Synchronized_Configure (Self : Gdk.Gdk_Window);
   pragma Import (C, Enable_Synchronized_Configure, "gdk_window_enable_synchronized_configure");
   pragma Obsolescent (Enable_Synchronized_Configure);
   --  Does nothing, present only for compatiblity.
   --  Since: gtk+ 2.6
   --  Deprecated since 3.8, 1

   procedure End_Draw_Frame
      (Self    : Gdk.Gdk_Window;
       Context : not null access Gdk.Drawing_Context.Gdk_Drawing_Context_Record'Class);
   --  Indicates that the drawing of the contents of Window started with
   --  gdk_window_begin_frame has been completed.
   --  This function will take care of destroying the
   --  Gdk.Drawing_Context.Gdk_Drawing_Context.
   --  It is an error to call this function without a matching
   --  gdk_window_begin_frame first.
   --  Since: gtk+ 3.22
   --  "context": the Gdk.Drawing_Context.Gdk_Drawing_Context created by
   --  Gdk.Window.Begin_Draw_Frame

   procedure End_Paint (Self : Gdk.Gdk_Window);
   pragma Import (C, End_Paint, "gdk_window_end_paint");
   --  Indicates that the backing store created by the most recent call to
   --  Gdk.Window.Begin_Paint_Region should be copied onscreen and deleted,
   --  leaving the next-most-recent backing store or no backing store at all as
   --  the active paint region. See Gdk.Window.Begin_Paint_Region for full
   --  details.
   --  It is an error to call this function without a matching
   --  Gdk.Window.Begin_Paint_Region first.

   function Ensure_Native (Self : Gdk.Gdk_Window) return Boolean;
   --  Tries to ensure that there is a window-system native window for this
   --  GdkWindow. This may fail in some situations, returning False.
   --  Offscreen window and children of them can never have native windows.
   --  Some backends may not support native child windows.
   --  Since: gtk+ 2.18

   procedure Flush (Self : Gdk.Gdk_Window);
   pragma Import (C, Flush, "gdk_window_flush");
   pragma Obsolescent (Flush);
   --  This function does nothing.
   --  Since: gtk+ 2.18
   --  Deprecated since 3.14, 1

   procedure Focus (Self : Gdk.Gdk_Window; Timestamp : Guint32);
   pragma Import (C, Focus, "gdk_window_focus");
   --  Sets keyboard focus to Window. In most cases,
   --  Gtk.Window.Present_With_Time should be used on a Gtk.Window.Gtk_Window,
   --  rather than calling this function.
   --  "timestamp": timestamp of the event triggering the window focus

   procedure Freeze_Toplevel_Updates_Libgtk_Only (Self : Gdk.Gdk_Window);
   pragma Import (C, Freeze_Toplevel_Updates_Libgtk_Only, "gdk_window_freeze_toplevel_updates_libgtk_only");
   pragma Obsolescent (Freeze_Toplevel_Updates_Libgtk_Only);
   --  Temporarily freezes a window and all its descendants such that it won't
   --  receive expose events. The window will begin receiving expose events
   --  again when Gdk.Window.Thaw_Toplevel_Updates_Libgtk_Only is called. If
   --  Gdk.Window.Freeze_Toplevel_Updates_Libgtk_Only has been called more than
   --  once, Gdk.Window.Thaw_Toplevel_Updates_Libgtk_Only must be called an
   --  equal number of times to begin processing exposes.
   --  This function is not part of the GDK public API and is only for use by
   --  GTK+.
   --  Deprecated since 3.16, 1

   procedure Freeze_Updates (Self : Gdk.Gdk_Window);
   pragma Import (C, Freeze_Updates, "gdk_window_freeze_updates");
   --  Temporarily freezes a window such that it won't receive expose events.
   --  The window will begin receiving expose events again when
   --  Gdk.Window.Thaw_Updates is called. If Gdk.Window.Freeze_Updates has been
   --  called more than once, Gdk.Window.Thaw_Updates must be called an equal
   --  number of times to begin processing exposes.

   procedure Fullscreen (Self : Gdk.Gdk_Window);
   pragma Import (C, Fullscreen, "gdk_window_fullscreen");
   --  Moves the window into fullscreen mode. This means the window covers the
   --  entire screen and is above any panels or task bars.
   --  If the window was already fullscreen, then this function does nothing.
   --  On X11, asks the window manager to put Window in a fullscreen state, if
   --  the window manager supports this operation. Not all window managers
   --  support this, and some deliberately ignore it or don't have a concept of
   --  "fullscreen"; so you can't rely on the fullscreenification actually
   --  happening. But it will happen with most standard window managers, and
   --  GDK makes a best effort to get it to happen.
   --  Since: gtk+ 2.2

   procedure Fullscreen_On_Monitor
      (Self    : Gdk.Gdk_Window;
       Monitor : Glib.Gint);
   pragma Import (C, Fullscreen_On_Monitor, "gdk_window_fullscreen_on_monitor");
   --  Moves the window into fullscreen mode on the given monitor. This means
   --  the window covers the entire screen and is above any panels or task
   --  bars.
   --  If the window was already fullscreen, then this function does nothing.
   --  "monitor": Which monitor to display fullscreen on.

   procedure Geometry_Changed (Self : Gdk.Gdk_Window);
   pragma Import (C, Geometry_Changed, "gdk_window_geometry_changed");
   --  This function informs GDK that the geometry of an embedded offscreen
   --  window has changed. This is necessary for GDK to keep track of which
   --  offscreen window the pointer is in.
   --  Since: gtk+ 2.18

   function Get_Accept_Focus (Self : Gdk.Gdk_Window) return Boolean;
   --  Determines whether or not the desktop environment shuld be hinted that
   --  the window does not want to receive input focus.
   --  Since: gtk+ 2.22

   procedure Set_Accept_Focus
      (Self         : Gdk.Gdk_Window;
       Accept_Focus : Boolean);
   --  Setting Accept_Focus to False hints the desktop environment that the
   --  window doesn't want to receive input focus.
   --  On X, it is the responsibility of the window manager to interpret this
   --  hint. ICCCM-compliant window manager usually respect it.
   --  Since: gtk+ 2.4
   --  "accept_focus": True if the window should receive input focus

   function Get_Background_Pattern
      (Self : Gdk.Gdk_Window) return Cairo.Cairo_Pattern;
   pragma Import (C, Get_Background_Pattern, "gdk_window_get_background_pattern");
   pragma Obsolescent (Get_Background_Pattern);
   --  Gets the pattern used to clear the background on Window.
   --  Since: gtk+ 2.22
   --  Deprecated since 3.22, 1

   procedure Set_Background_Pattern
      (Self    : Gdk.Gdk_Window;
       Pattern : Cairo.Cairo_Pattern);
   pragma Import (C, Set_Background_Pattern, "gdk_window_set_background_pattern");
   pragma Obsolescent (Set_Background_Pattern);
   --  Sets the background of Window.
   --  A background of null means that the window won't have any background.
   --  On the X11 backend it's also possible to inherit the background from the
   --  parent window using gdk_x11_get_parent_relative_pattern.
   --  The windowing system will normally fill a window with its background
   --  when the window is obscured then exposed.
   --  Deprecated since 3.22, 1
   --  "pattern": a pattern to use, or null

   function Get_Children
      (Self : Gdk.Gdk_Window) return Gdk_Window_List.Glist;
   --  Gets the list of children of Window known to GDK. This function only
   --  returns children created via GDK, so for example it's useless when used
   --  with the root window; it only returns windows an application created
   --  itself.
   --  The returned list must be freed, but the elements in the list need not
   --  be.

   function Get_Clip_Region
      (Self : Gdk.Gdk_Window) return Cairo.Region.Cairo_Region;
   pragma Import (C, Get_Clip_Region, "gdk_window_get_clip_region");
   --  Computes the region of a window that potentially can be written to by
   --  drawing primitives. This region may not take into account other factors
   --  such as if the window is obscured by other windows, but no area outside
   --  of this region will be affected by drawing primitives.

   function Get_Composited (Self : Gdk.Gdk_Window) return Boolean;
   pragma Obsolescent (Get_Composited);
   --  Determines whether Window is composited.
   --  See Gdk.Window.Set_Composited.
   --  Since: gtk+ 2.22
   --  Deprecated since 3.16, 1

   procedure Set_Composited (Self : Gdk.Gdk_Window; Composited : Boolean);
   pragma Obsolescent (Set_Composited);
   --  Sets a Gdk.Gdk_Window as composited, or unsets it. Composited windows
   --  do not automatically have their contents drawn to the screen. Drawing is
   --  redirected to an offscreen buffer and an expose event is emitted on the
   --  parent of the composited window. It is the responsibility of the
   --  parent's expose handler to manually merge the off-screen content onto
   --  the screen in whatever way it sees fit.
   --  It only makes sense for child windows to be composited; see
   --  Gdk.Window.Set_Opacity if you need translucent toplevel windows.
   --  An additional effect of this call is that the area of this window is no
   --  longer clipped from regions marked for invalidation on its parent. Draws
   --  done on the parent window are also no longer clipped by the child.
   --  This call is only supported on some systems (currently, only X11 with
   --  new enough Xcomposite and Xdamage extensions). You must call
   --  Gdk.Display.Supports_Composite to check if setting a window as
   --  composited is supported before attempting to do so.
   --  Since: gtk+ 2.12
   --  Deprecated since 3.16, 1
   --  "composited": True to set the window as composited

   function Get_Cursor (Self : Gdk.Gdk_Window) return Gdk.Gdk_Cursor;
   pragma Import (C, Get_Cursor, "gdk_window_get_cursor");
   --  Retrieves a Gdk.Gdk_Cursor pointer for the cursor currently set on the
   --  specified Gdk.Gdk_Window, or null. If the return value is null then
   --  there is no custom cursor set on the specified window, and it is using
   --  the cursor for its parent window.
   --  Since: gtk+ 2.18

   procedure Set_Cursor (Self : Gdk.Gdk_Window; Cursor : Gdk.Gdk_Cursor);
   pragma Import (C, Set_Cursor, "gdk_window_set_cursor");
   --  Sets the default mouse pointer for a Gdk.Gdk_Window.
   --  Note that Cursor must be for the same display as Window.
   --  Use gdk_cursor_new_for_display or gdk_cursor_new_from_pixbuf to create
   --  the cursor. To make the cursor invisible, use Gdk.Blank_Cursor. Passing
   --  null for the Cursor argument to Gdk.Window.Set_Cursor means that Window
   --  will use the cursor of its parent window. Most windows should use this
   --  default.
   --  "cursor": a cursor

   procedure Get_Decorations
      (Self            : Gdk.Gdk_Window;
       Decorations     : out Gdk_WMDecoration;
       Has_Decorations : out Boolean);
   --  Returns the decorations set on the GdkWindow with
   --  Gdk.Window.Set_Decorations.
   --  "decorations": The window decorations will be written here

   procedure Set_Decorations
      (Self        : Gdk.Gdk_Window;
       Decorations : Gdk_WMDecoration);
   pragma Import (C, Set_Decorations, "gdk_window_set_decorations");
   --  "Decorations" are the features the window manager adds to a toplevel
   --  Gdk.Gdk_Window. This function sets the traditional Motif window manager
   --  hints that tell the window manager which decorations you would like your
   --  window to have. Usually you should use Gtk.Window.Set_Decorated on a
   --  Gtk.Window.Gtk_Window instead of using the GDK function directly.
   --  The Decorations argument is the logical OR of the fields in the
   --  Gdk.Window.Gdk_WMDecoration enumeration. If GDK_DECOR_ALL is included in
   --  the mask, the other bits indicate which decorations should be turned
   --  off. If GDK_DECOR_ALL is not included, then the other bits indicate
   --  which decorations should be turned on.
   --  Most window managers honor a decorations hint of 0 to disable all
   --  decorations, but very few honor all possible combinations of bits.
   --  "decorations": decoration hint mask

   function Get_Device_Cursor
      (Self   : Gdk.Gdk_Window;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Gdk.Gdk_Cursor;
   --  Retrieves a Gdk.Gdk_Cursor pointer for the Device currently set on the
   --  specified Gdk.Gdk_Window, or null. If the return value is null then
   --  there is no custom cursor set on the specified window, and it is using
   --  the cursor for its parent window.
   --  Since: gtk+ 3.0
   --  "device": a master, pointer Gdk.Device.Gdk_Device.

   procedure Set_Device_Cursor
      (Self   : Gdk.Gdk_Window;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class;
       Cursor : Gdk.Gdk_Cursor);
   --  Sets a specific Gdk.Gdk_Cursor for a given device when it gets inside
   --  Window. Use gdk_cursor_new_for_display or gdk_cursor_new_from_pixbuf to
   --  create the cursor. To make the cursor invisible, use Gdk.Blank_Cursor.
   --  Passing null for the Cursor argument to Gdk.Window.Set_Cursor means that
   --  Window will use the cursor of its parent window. Most windows should use
   --  this default.
   --  Since: gtk+ 3.0
   --  "device": a master, pointer Gdk.Device.Gdk_Device
   --  "cursor": a Gdk.Gdk_Cursor

   function Get_Device_Events
      (Self   : Gdk.Gdk_Window;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Gdk.Event.Gdk_Event_Mask;
   --  Returns the event mask for Window corresponding to an specific device.
   --  Since: gtk+ 3.0
   --  "device": a Gdk.Device.Gdk_Device.

   procedure Set_Device_Events
      (Self       : Gdk.Gdk_Window;
       Device     : not null access Gdk.Device.Gdk_Device_Record'Class;
       Event_Mask : Gdk.Event.Gdk_Event_Mask);
   --  Sets the event mask for a given device (Normally a floating device, not
   --  attached to any visible pointer) to Window. For example, an event mask
   --  including GDK_BUTTON_PRESS_MASK means the window should report button
   --  press events. The event mask is the bitwise OR of values from the
   --  Gdk.Event.Gdk_Event_Mask enumeration.
   --  See the [input handling overview][event-masks] for details.
   --  Since: gtk+ 3.0
   --  "device": Gdk.Device.Gdk_Device to enable events for.
   --  "event_mask": event mask for Window

   procedure Get_Device_Position
      (Self   : Gdk.Gdk_Window;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Mask   : out Gdk.Types.Gdk_Modifier_Type;
       Window : out Gdk.Gdk_Window);
   --  Obtains the current device position and modifier state. The position is
   --  given in coordinates relative to the upper left corner of Window.
   --  Use Gdk.Window.Get_Device_Position_Double if you need subpixel
   --  precision.
   --  Since: gtk+ 3.0
   --  "device": pointer Gdk.Device.Gdk_Device to query to.
   --  "x": return location for the X coordinate of Device, or null.
   --  "y": return location for the Y coordinate of Device, or null.
   --  "mask": return location for the modifier mask, or null.

   function Get_Device_Position_Double
      (Self   : Gdk.Gdk_Window;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class;
       X      : access Gdouble;
       Y      : access Gdouble;
       Mask   : access Gdk.Types.Gdk_Modifier_Type) return Gdk.Gdk_Window;
   --  Obtains the current device position in doubles and modifier state. The
   --  position is given in coordinates relative to the upper left corner of
   --  Window.
   --  Since: gtk+ 3.10
   --  "device": pointer Gdk.Device.Gdk_Device to query to.
   --  "x": return location for the X coordinate of Device, or null.
   --  "y": return location for the Y coordinate of Device, or null.
   --  "mask": return location for the modifier mask, or null.

   function Get_Display
      (Self : Gdk.Gdk_Window) return Gdk.Display.Gdk_Display;
   --  Gets the Gdk.Display.Gdk_Display associated with a Gdk.Gdk_Window.
   --  Since: gtk+ 2.24

   function Get_Effective_Parent
      (Self : Gdk.Gdk_Window) return Gdk.Gdk_Window;
   pragma Import (C, Get_Effective_Parent, "gdk_window_get_effective_parent");
   --  Obtains the parent of Window, as known to GDK. Works like
   --  Gdk.Window.Get_Parent for normal windows, but returns the window's
   --  embedder for offscreen windows.
   --  See also: gdk_offscreen_window_get_embedder
   --  Since: gtk+ 2.22

   function Get_Effective_Toplevel
      (Self : Gdk.Gdk_Window) return Gdk.Gdk_Window;
   pragma Import (C, Get_Effective_Toplevel, "gdk_window_get_effective_toplevel");
   --  Gets the toplevel window that's an ancestor of Window.
   --  Works like Gdk.Window.Get_Toplevel, but treats an offscreen window's
   --  embedder as its parent, using Gdk.Window.Get_Effective_Parent.
   --  See also: gdk_offscreen_window_get_embedder
   --  Since: gtk+ 2.22

   function Get_Event_Compression (Self : Gdk.Gdk_Window) return Boolean;
   --  Get the current event compression setting for this window.
   --  Since: gtk+ 3.12

   procedure Set_Event_Compression
      (Self              : Gdk.Gdk_Window;
       Event_Compression : Boolean);
   --  Determines whether or not extra unprocessed motion events in the event
   --  queue can be discarded. If True only the most recent event will be
   --  delivered.
   --  Some types of applications, e.g. paint programs, need to see all motion
   --  events and will benefit from turning off event compression.
   --  By default, event compression is enabled.
   --  Since: gtk+ 3.12
   --  "event_compression": True if motion events should be compressed

   function Get_Events
      (Self : Gdk.Gdk_Window) return Gdk.Event.Gdk_Event_Mask;
   pragma Import (C, Get_Events, "gdk_window_get_events");
   --  Gets the event mask for Window for all master input devices. See
   --  Gdk.Window.Set_Events.

   procedure Set_Events
      (Self       : Gdk.Gdk_Window;
       Event_Mask : Gdk.Event.Gdk_Event_Mask);
   pragma Import (C, Set_Events, "gdk_window_set_events");
   --  The event mask for a window determines which events will be reported
   --  for that window from all master input devices. For example, an event
   --  mask including GDK_BUTTON_PRESS_MASK means the window should report
   --  button press events. The event mask is the bitwise OR of values from the
   --  Gdk.Event.Gdk_Event_Mask enumeration.
   --  See the [input handling overview][event-masks] for details.
   --  "event_mask": event mask for Window

   function Get_Focus_On_Map (Self : Gdk.Gdk_Window) return Boolean;
   --  Determines whether or not the desktop environment should be hinted that
   --  the window does not want to receive input focus when it is mapped.
   --  Since: gtk+ 2.22

   procedure Set_Focus_On_Map
      (Self         : Gdk.Gdk_Window;
       Focus_On_Map : Boolean);
   --  Setting Focus_On_Map to False hints the desktop environment that the
   --  window doesn't want to receive input focus when it is mapped.
   --  focus_on_map should be turned off for windows that aren't triggered
   --  interactively (such as popups from network activity).
   --  On X, it is the responsibility of the window manager to interpret this
   --  hint. Window managers following the freedesktop.org window manager
   --  extension specification should respect it.
   --  Since: gtk+ 2.6
   --  "focus_on_map": True if the window should receive input focus when
   --  mapped

   function Get_Frame_Clock
      (Self : Gdk.Gdk_Window) return Gdk.Frame_Clock.Gdk_Frame_Clock;
   --  Gets the frame clock for the window. The frame clock for a window never
   --  changes unless the window is reparented to a new toplevel window.
   --  Since: gtk+ 3.8

   procedure Get_Frame_Extents
      (Self : Gdk.Gdk_Window;
       Rect : out Gdk.Rectangle.Gdk_Rectangle);
   pragma Import (C, Get_Frame_Extents, "gdk_window_get_frame_extents");
   --  Obtains the bounding box of the window, including window manager
   --  titlebar/borders if any. The frame position is given in root window
   --  coordinates. To get the position of the window itself (rather than the
   --  frame) in root window coordinates, use Gdk.Window.Get_Origin.
   --  "rect": rectangle to fill with bounding box of the window frame

   function Get_Fullscreen_Mode
      (Self : Gdk.Gdk_Window) return Gdk_Fullscreen_Mode;
   pragma Import (C, Get_Fullscreen_Mode, "gdk_window_get_fullscreen_mode");
   --  Obtains the Gdk.Window.Gdk_Fullscreen_Mode of the Window.
   --  Since: gtk+ 3.8

   procedure Set_Fullscreen_Mode
      (Self : Gdk.Gdk_Window;
       Mode : Gdk_Fullscreen_Mode);
   pragma Import (C, Set_Fullscreen_Mode, "gdk_window_set_fullscreen_mode");
   --  Specifies whether the Window should span over all monitors (in a
   --  multi-head setup) or only the current monitor when in fullscreen mode.
   --  The Mode argument is from the Gdk.Window.Gdk_Fullscreen_Mode
   --  enumeration. If GDK_FULLSCREEN_ON_ALL_MONITORS is specified, the
   --  fullscreen Window will span over all monitors from the
   --  Gdk.Screen.Gdk_Screen.
   --  On X11, searches through the list of monitors from the
   --  Gdk.Screen.Gdk_Screen the ones which delimit the 4 edges of the entire
   --  Gdk.Screen.Gdk_Screen and will ask the window manager to span the Window
   --  over these monitors.
   --  If the XINERAMA extension is not available or not usable, this function
   --  has no effect.
   --  Not all window managers support this, so you can't rely on the
   --  fullscreen window to span over the multiple monitors when
   --  GDK_FULLSCREEN_ON_ALL_MONITORS is specified.
   --  Since: gtk+ 3.8
   --  "mode": fullscreen mode

   procedure Get_Geometry
      (Self   : Gdk.Gdk_Window;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   pragma Import (C, Get_Geometry, "gdk_window_get_geometry");
   --  Any of the return location arguments to this function may be null, if
   --  you aren't interested in getting the value of that field.
   --  The X and Y coordinates returned are relative to the parent window of
   --  Window, which for toplevels usually means relative to the window
   --  decorations (titlebar, etc.) rather than relative to the root window
   --  (screen-size background window).
   --  On the X11 platform, the geometry is obtained from the X server, so
   --  reflects the latest position of Window; this may be out-of-sync with the
   --  position of Window delivered in the most-recently-processed
   --  Gdk.Event.Gdk_Event_Configure. Gdk.Window.Get_Position in contrast gets
   --  the position from the most recent configure event.
   --  Note: If Window is not a toplevel, it is much better to call
   --  Gdk.Window.Get_Position, Gdk.Window.Get_Width and Gdk.Window.Get_Height
   --  instead, because it avoids the roundtrip to the X server and because
   --  these functions support the full 32-bit coordinate space, whereas
   --  Gdk.Window.Get_Geometry is restricted to the 16-bit coordinates of X11.
   --  "x": return location for X coordinate of window (relative to its
   --  parent)
   --  "y": return location for Y coordinate of window (relative to its
   --  parent)
   --  "width": return location for width of window
   --  "height": return location for height of window

   function Get_Group (Self : Gdk.Gdk_Window) return Gdk.Gdk_Window;
   pragma Import (C, Get_Group, "gdk_window_get_group");
   --  Returns the group leader window for Window. See Gdk.Window.Set_Group.
   --  Since: gtk+ 2.4

   procedure Set_Group (Self : Gdk.Gdk_Window; Leader : Gdk.Gdk_Window);
   pragma Import (C, Set_Group, "gdk_window_set_group");
   --  Sets the group leader window for Window. By default, GDK sets the group
   --  leader for all toplevel windows to a global window implicitly created by
   --  GDK. With this function you can override this default.
   --  The group leader window allows the window manager to distinguish all
   --  windows that belong to a single application. It may for example allow
   --  users to minimize/unminimize all windows belonging to an application at
   --  once. You should only set a non-default group window if your application
   --  pretends to be multiple applications.
   --  "leader": group leader window, or null to restore the default group
   --  leader window

   function Get_Height (Self : Gdk.Gdk_Window) return Glib.Gint;
   pragma Import (C, Get_Height, "gdk_window_get_height");
   --  Returns the height of the given Window.
   --  On the X11 platform the returned size is the size reported in the
   --  most-recently-processed configure event, rather than the current size on
   --  the X server.
   --  Since: gtk+ 2.24

   function Get_Modal_Hint (Self : Gdk.Gdk_Window) return Boolean;
   --  Determines whether or not the window manager is hinted that Window has
   --  modal behaviour.
   --  Since: gtk+ 2.22

   procedure Set_Modal_Hint (Self : Gdk.Gdk_Window; Modal : Boolean);
   --  The application can use this hint to tell the window manager that a
   --  certain window has modal behaviour. The window manager can use this
   --  information to handle modal windows in a special way.
   --  You should only use this on windows for which you have previously
   --  called Gdk.Window.Set_Transient_For
   --  "modal": True if the window is modal, False otherwise.

   procedure Get_Origin
      (Self : Gdk.Gdk_Window;
       X    : out Glib.Gint;
       Y    : out Glib.Gint);
   pragma Import (C, Get_Origin, "gdk_window_get_origin");
   --  Obtains the position of a window in root window coordinates. (Compare
   --  with Gdk.Window.Get_Position and Gdk.Window.Get_Geometry which return
   --  the position of a window relative to its parent window.)
   --  "x": return location for X coordinate
   --  "y": return location for Y coordinate

   function Get_Parent (Self : Gdk.Gdk_Window) return Gdk.Gdk_Window;
   pragma Import (C, Get_Parent, "gdk_window_get_parent");
   --  Obtains the parent of Window, as known to GDK. Does not query the X
   --  server; thus this returns the parent as passed to gdk_window_new, not
   --  the actual parent. This should never matter unless you're using Xlib
   --  calls mixed with GDK calls on the X11 platform. It may also matter for
   --  toplevel windows, because the window manager may choose to reparent
   --  them.
   --  Note that you should use Gdk.Window.Get_Effective_Parent when writing
   --  generic code that walks up a window hierarchy, because
   --  Gdk.Window.Get_Parent will most likely not do what you expect if there
   --  are offscreen windows in the hierarchy.

   function Get_Pass_Through (Self : Gdk.Gdk_Window) return Boolean;
   --  Returns whether input to the window is passed through to the window
   --  below.
   --  See Gdk.Window.Set_Pass_Through for details
   --  Since: gtk+ 3.18

   procedure Set_Pass_Through
      (Self         : Gdk.Gdk_Window;
       Pass_Through : Boolean);
   --  Sets whether input to the window is passed through to the window below.
   --  The default value of this is False, which means that pointer events
   --  that happen inside the window are send first to the window, but if the
   --  event is not selected by the event mask then the event is sent to the
   --  parent window, and so on up the hierarchy.
   --  If Pass_Through is True then such pointer events happen as if the
   --  window wasn't there at all, and thus will be sent first to any windows
   --  below Window. This is useful if the window is used in a transparent
   --  fashion. In the terminology of the web this would be called
   --  "pointer-events: none".
   --  Note that a window with Pass_Through True can still have a subwindow
   --  without pass through, so you can get events on a subset of a window. And
   --  in that cases you would get the in-between related events such as the
   --  pointer enter/leave events on its way to the destination window.
   --  Since: gtk+ 3.18
   --  "pass_through": a boolean

   procedure Get_Pointer
      (Self   : Gdk.Gdk_Window;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Mask   : out Gdk.Types.Gdk_Modifier_Type;
       Window : out Gdk.Gdk_Window);
   pragma Obsolescent (Get_Pointer);
   --  Obtains the current pointer position and modifier state. The position
   --  is given in coordinates relative to the upper left corner of Window.
   --  Deprecated since 3.0, 1
   --  "x": return location for X coordinate of pointer or null to not return
   --  the X coordinate
   --  "y": return location for Y coordinate of pointer or null to not return
   --  the Y coordinate
   --  "mask": return location for modifier mask or null to not return the
   --  modifier mask

   procedure Get_Position
      (Self : Gdk.Gdk_Window;
       X    : out Glib.Gint;
       Y    : out Glib.Gint);
   pragma Import (C, Get_Position, "gdk_window_get_position");
   --  Obtains the position of the window as reported in the
   --  most-recently-processed Gdk.Event.Gdk_Event_Configure. Contrast with
   --  Gdk.Window.Get_Geometry which queries the X server for the current
   --  window position, regardless of which events have been received or
   --  processed.
   --  The position coordinates are relative to the window's parent window.
   --  "x": X coordinate of window
   --  "y": Y coordinate of window

   procedure Get_Root_Coords
      (Self   : Gdk.Gdk_Window;
       X      : Glib.Gint;
       Y      : Glib.Gint;
       Root_X : out Glib.Gint;
       Root_Y : out Glib.Gint);
   pragma Import (C, Get_Root_Coords, "gdk_window_get_root_coords");
   --  Obtains the position of a window position in root window coordinates.
   --  This is similar to Gdk.Window.Get_Origin but allows you to pass in any
   --  position in the window, not just the origin.
   --  Since: gtk+ 2.18
   --  "x": X coordinate in window
   --  "y": Y coordinate in window
   --  "root_x": return location for X coordinate
   --  "root_y": return location for Y coordinate

   procedure Get_Root_Origin
      (Self : Gdk.Gdk_Window;
       X    : out Glib.Gint;
       Y    : out Glib.Gint);
   pragma Import (C, Get_Root_Origin, "gdk_window_get_root_origin");
   --  Obtains the top-left corner of the window manager frame in root window
   --  coordinates.
   --  "x": return location for X position of window frame
   --  "y": return location for Y position of window frame

   function Get_Scale_Factor (Self : Gdk.Gdk_Window) return Glib.Gint;
   pragma Import (C, Get_Scale_Factor, "gdk_window_get_scale_factor");
   --  Returns the internal scale factor that maps from window coordiantes to
   --  the actual device pixels. On traditional systems this is 1, but on very
   --  high density outputs this can be a higher value (often 2).
   --  A higher value means that drawing is automatically scaled up to a
   --  higher resolution, so any code doing drawing will automatically look
   --  nicer. However, if you are supplying pixel-based data the scale value
   --  can be used to determine whether to use a pixel resource with higher
   --  resolution data.
   --  The scale of a window may change during runtime, if this happens a
   --  configure event will be sent to the toplevel window.
   --  Since: gtk+ 3.10

   function Get_Screen (Self : Gdk.Gdk_Window) return Gdk.Screen.Gdk_Screen;
   --  Gets the Gdk.Screen.Gdk_Screen associated with a Gdk.Gdk_Window.
   --  Since: gtk+ 2.24

   function Get_Source_Events
      (Self   : Gdk.Gdk_Window;
       Source : Gdk_Input_Source) return Gdk.Event.Gdk_Event_Mask;
   pragma Import (C, Get_Source_Events, "gdk_window_get_source_events");
   --  Returns the event mask for Window corresponding to the device class
   --  specified by Source.
   --  "source": a Gdk_Input_Source to define the source class.

   procedure Set_Source_Events
      (Self       : Gdk.Gdk_Window;
       Source     : Gdk_Input_Source;
       Event_Mask : Gdk.Event.Gdk_Event_Mask);
   pragma Import (C, Set_Source_Events, "gdk_window_set_source_events");
   --  Sets the event mask for any floating device (i.e. not attached to any
   --  visible pointer) that has the source defined as Source. This event mask
   --  will be applied both to currently existing, newly added devices after
   --  this call, and devices being attached/detached.
   --  Since: gtk+ 3.0
   --  "source": a Gdk_Input_Source to define the source class.
   --  "event_mask": event mask for Window

   function Get_State
      (Self : Gdk.Gdk_Window) return Gdk.Event.Gdk_Window_State;
   pragma Import (C, Get_State, "gdk_window_get_state");
   --  Gets the bitwise OR of the currently active window state flags, from
   --  the Gdk.Event.Gdk_Window_State enumeration.

   function Get_Support_Multidevice (Self : Gdk.Gdk_Window) return Boolean;
   --  Returns True if the window is aware of the existence of multiple
   --  devices.
   --  Since: gtk+ 3.0

   procedure Set_Support_Multidevice
      (Self                : Gdk.Gdk_Window;
       Support_Multidevice : Boolean);
   --  This function will enable multidevice features in Window.
   --  Multidevice aware windows will need to handle properly multiple, per
   --  device enter/leave events, device grabs and grab ownerships.
   --  Since: gtk+ 3.0
   --  "support_multidevice": True to enable multidevice support in Window.

   function Get_Toplevel (Self : Gdk.Gdk_Window) return Gdk.Gdk_Window;
   pragma Import (C, Get_Toplevel, "gdk_window_get_toplevel");
   --  Gets the toplevel window that's an ancestor of Window.
   --  Any window type but Gdk.Window_Child is considered a toplevel window,
   --  as is a Gdk.Window_Child window that has a root window as parent.
   --  Note that you should use Gdk.Window.Get_Effective_Toplevel when you
   --  want to get to a window's toplevel as seen on screen, because
   --  Gdk.Window.Get_Toplevel will most likely not do what you expect if there
   --  are offscreen windows in the hierarchy.

   function Get_Type_Hint
      (Self : Gdk.Gdk_Window) return Gdk_Window_Type_Hint;
   pragma Import (C, Get_Type_Hint, "gdk_window_get_type_hint");
   --  This function returns the type hint set for a window.
   --  Since: gtk+ 2.10

   procedure Set_Type_Hint
      (Self : Gdk.Gdk_Window;
       Hint : Gdk_Window_Type_Hint);
   pragma Import (C, Set_Type_Hint, "gdk_window_set_type_hint");
   --  The application can use this call to provide a hint to the window
   --  manager about the functionality of a window. The window manager can use
   --  this information when determining the decoration and behaviour of the
   --  window.
   --  The hint must be set before the window is mapped.
   --  "hint": A hint of the function this window will have

   function Get_Update_Area
      (Self : Gdk.Gdk_Window) return Cairo.Region.Cairo_Region;
   pragma Import (C, Get_Update_Area, "gdk_window_get_update_area");
   --  Transfers ownership of the update area from Window to the caller of the
   --  function. That is, after calling this function, Window will no longer
   --  have an invalid/dirty region; the update area is removed from Window and
   --  handed to you. If a window has no update area,
   --  Gdk.Window.Get_Update_Area returns null. You are responsible for calling
   --  cairo_region_destroy on the returned region if it's non-null.

   function Get_Visible_Region
      (Self : Gdk.Gdk_Window) return Cairo.Region.Cairo_Region;
   pragma Import (C, Get_Visible_Region, "gdk_window_get_visible_region");
   --  Computes the region of the Window that is potentially visible. This
   --  does not necessarily take into account if the window is obscured by
   --  other windows, but no area outside of this region is visible.

   function Get_Visual (Self : Gdk.Gdk_Window) return Gdk.Visual.Gdk_Visual;
   pragma Import (C, Get_Visual, "gdk_window_get_visual");
   --  Gets the Gdk.Visual.Gdk_Visual describing the pixel format of Window.
   --  Since: gtk+ 2.24

   function Get_Width (Self : Gdk.Gdk_Window) return Glib.Gint;
   pragma Import (C, Get_Width, "gdk_window_get_width");
   --  Returns the width of the given Window.
   --  On the X11 platform the returned size is the size reported in the
   --  most-recently-processed configure event, rather than the current size on
   --  the X server.
   --  Since: gtk+ 2.24

   function Get_Window_Type (Self : Gdk.Gdk_Window) return Gdk_Window_Type;
   pragma Import (C, Get_Window_Type, "gdk_window_get_window_type");
   --  Gets the type of the window. See Gdk.Window.Gdk_Window_Type.

   function Has_Native (Self : Gdk.Gdk_Window) return Boolean;
   --  Checks whether the window has a native window or not. Note that you can
   --  use Gdk.Window.Ensure_Native if a native window is needed.
   --  Since: gtk+ 2.22

   procedure Hide (Self : Gdk.Gdk_Window);
   pragma Import (C, Hide, "gdk_window_hide");
   --  For toplevel windows, withdraws them, so they will no longer be known
   --  to the window manager; for all windows, unmaps them, so they won't be
   --  displayed. Normally done automatically as part of Gtk.Widget.Hide.

   procedure Iconify (Self : Gdk.Gdk_Window);
   pragma Import (C, Iconify, "gdk_window_iconify");
   --  Asks to iconify (minimize) Window. The window manager may choose to
   --  ignore the request, but normally will honor it. Using Gtk.Window.Iconify
   --  is preferred, if you have a Gtk.Window.Gtk_Window widget.
   --  This function only makes sense when Window is a toplevel window.

   procedure Input_Shape_Combine_Region
      (Self         : Gdk.Gdk_Window;
       Shape_Region : Cairo.Region.Cairo_Region;
       Offset_X     : Glib.Gint;
       Offset_Y     : Glib.Gint);
   pragma Import (C, Input_Shape_Combine_Region, "gdk_window_input_shape_combine_region");
   --  Like Gdk.Window.Shape_Combine_Region, but the shape applies only to
   --  event handling. Mouse events which happen while the pointer position
   --  corresponds to an unset bit in the mask will be passed on the window
   --  below Window.
   --  An input shape is typically used with RGBA windows. The alpha channel
   --  of the window defines which pixels are invisible and allows for nicely
   --  antialiased borders, and the input shape controls where the window is
   --  "clickable".
   --  On the X11 platform, this requires version 1.1 of the shape extension.
   --  On the Win32 platform, this functionality is not present and the
   --  function does nothing.
   --  Since: gtk+ 2.10
   --  "shape_region": region of window to be non-transparent
   --  "offset_x": X position of Shape_Region in Window coordinates
   --  "offset_y": Y position of Shape_Region in Window coordinates

   procedure Invalidate_Maybe_Recurse
      (Self       : Gdk.Gdk_Window;
       Region     : Cairo.Region.Cairo_Region;
       Child_Func : Gdk_Window_Child_Func);
   --  Adds Region to the update area for Window. The update area is the
   --  region that needs to be redrawn, or "dirty region." The call
   --  Gdk.Window.Process_Updates sends one or more expose events to the
   --  window, which together cover the entire update area. An application
   --  would normally redraw the contents of Window in response to those expose
   --  events.
   --  GDK will call Gdk.Window.Process_All_Updates on your behalf whenever
   --  your program returns to the main loop and becomes idle, so normally
   --  there's no need to do that manually, you just need to invalidate regions
   --  that you know should be redrawn.
   --  The Child_Func parameter controls whether the region of each child
   --  window that intersects Region will also be invalidated. Only children
   --  for which Child_Func returns TRUE will have the area invalidated.
   --  "region": a cairo_region_t
   --  "child_func": function to use to decide if to recurse to a child, null
   --  means never recurse.

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Invalidate_Maybe_Recurse_User_Data is

      type Gdk_Window_Child_Func is access function
        (Window    : Gdk.Gdk_Window;
         User_Data : User_Data_Type) return Boolean;
      --  A function of this type is passed to
      --  Gdk.Window.Invalidate_Maybe_Recurse. It gets called for each child of
      --  the window to determine whether to recursively invalidate it or now.
      --  "window": a Gdk.Gdk_Window
      --  "user_data": user data

      procedure Invalidate_Maybe_Recurse
         (Self       : Gdk.Gdk_Window;
          Region     : Cairo.Region.Cairo_Region;
          Child_Func : Gdk_Window_Child_Func;
          User_Data  : User_Data_Type);
      --  Adds Region to the update area for Window. The update area is the
      --  region that needs to be redrawn, or "dirty region." The call
      --  Gdk.Window.Process_Updates sends one or more expose events to the
      --  window, which together cover the entire update area. An application
      --  would normally redraw the contents of Window in response to those
      --  expose events.
      --  GDK will call Gdk.Window.Process_All_Updates on your behalf whenever
      --  your program returns to the main loop and becomes idle, so normally
      --  there's no need to do that manually, you just need to invalidate
      --  regions that you know should be redrawn.
      --  The Child_Func parameter controls whether the region of each child
      --  window that intersects Region will also be invalidated. Only children
      --  for which Child_Func returns TRUE will have the area invalidated.
      --  "region": a cairo_region_t
      --  "child_func": function to use to decide if to recurse to a child,
      --  null means never recurse.
      --  "user_data": data passed to Child_Func

   end Invalidate_Maybe_Recurse_User_Data;

   procedure Invalidate_Rect
      (Self                : Gdk.Gdk_Window;
       Rect                : Gdk.Rectangle.Gdk_Rectangle;
       Invalidate_Children : Boolean);
   --  A convenience wrapper around Gdk.Window.Invalidate_Region which
   --  invalidates a rectangular region. See Gdk.Window.Invalidate_Region for
   --  details.
   --  "rect": rectangle to invalidate or null to invalidate the whole window
   --  "invalidate_children": whether to also invalidate child windows

   procedure Invalidate_Region
      (Self                : Gdk.Gdk_Window;
       Region              : Cairo.Region.Cairo_Region;
       Invalidate_Children : Boolean);
   --  Adds Region to the update area for Window. The update area is the
   --  region that needs to be redrawn, or "dirty region." The call
   --  Gdk.Window.Process_Updates sends one or more expose events to the
   --  window, which together cover the entire update area. An application
   --  would normally redraw the contents of Window in response to those expose
   --  events.
   --  GDK will call Gdk.Window.Process_All_Updates on your behalf whenever
   --  your program returns to the main loop and becomes idle, so normally
   --  there's no need to do that manually, you just need to invalidate regions
   --  that you know should be redrawn.
   --  The Invalidate_Children parameter controls whether the region of each
   --  child window that intersects Region will also be invalidated. If False,
   --  then the update area for child windows will remain unaffected. See
   --  gdk_window_invalidate_maybe_recurse if you need fine grained control
   --  over which children are invalidated.
   --  "region": a cairo_region_t
   --  "invalidate_children": True to also invalidate child windows

   function Is_Destroyed (Self : Gdk.Gdk_Window) return Boolean;
   --  Check to see if a window is destroyed..
   --  Since: gtk+ 2.18

   function Is_Input_Only (Self : Gdk.Gdk_Window) return Boolean;
   --  Determines whether or not the window is an input only window.
   --  Since: gtk+ 2.22

   function Is_Shaped (Self : Gdk.Gdk_Window) return Boolean;
   --  Determines whether or not the window is shaped.
   --  Since: gtk+ 2.22

   function Is_Viewable (Self : Gdk.Gdk_Window) return Boolean;
   --  Check if the window and all ancestors of the window are mapped. (This
   --  is not necessarily "viewable" in the X sense, since we only check as far
   --  as we have GDK window parents, not to the root window.)

   function Is_Visible (Self : Gdk.Gdk_Window) return Boolean;
   --  Checks whether the window has been mapped (with Gdk.Window.Show or
   --  Gdk.Window.Show_Unraised).

   procedure Lower (Self : Gdk.Gdk_Window);
   pragma Import (C, Lower, "gdk_window_lower");
   --  Lowers Window to the bottom of the Z-order (stacking order), so that
   --  other windows with the same parent window appear above Window. This is
   --  true whether or not the other windows are visible.
   --  If Window is a toplevel, the window manager may choose to deny the
   --  request to move the window in the Z-order, Gdk.Window.Lower only
   --  requests the restack, does not guarantee it.
   --  Note that Gdk.Window.Show raises the window again, so don't call this
   --  function before Gdk.Window.Show. (Try Gdk.Window.Show_Unraised.)

   procedure Mark_Paint_From_Clip
      (Self : Gdk.Gdk_Window;
       Cr   : Cairo.Cairo_Context);
   pragma Import (C, Mark_Paint_From_Clip, "gdk_window_mark_paint_from_clip");
   --  If you call this during a paint (e.g. between
   --  Gdk.Window.Begin_Paint_Region and Gdk.Window.End_Paint then GDK will
   --  mark the current clip region of the window as being drawn. This is
   --  required when mixing GL rendering via gdk_cairo_draw_from_gl and cairo
   --  rendering, as otherwise GDK has no way of knowing when something paints
   --  over the GL-drawn regions.
   --  This is typically called automatically by GTK+ and you don't need to
   --  care about this.
   --  Since: gtk+ 3.16
   --  "cr": a cairo_t

   procedure Maximize (Self : Gdk.Gdk_Window);
   pragma Import (C, Maximize, "gdk_window_maximize");
   --  Maximizes the window. If the window was already maximized, then this
   --  function does nothing.
   --  On X11, asks the window manager to maximize Window, if the window
   --  manager supports this operation. Not all window managers support this,
   --  and some deliberately ignore it or don't have a concept of "maximized";
   --  so you can't rely on the maximization actually happening. But it will
   --  happen with most standard window managers, and GDK makes a best effort
   --  to get it to happen.
   --  On Windows, reliably maximizes the window.

   procedure Merge_Child_Input_Shapes (Self : Gdk.Gdk_Window);
   pragma Import (C, Merge_Child_Input_Shapes, "gdk_window_merge_child_input_shapes");
   --  Merges the input shape masks for any child windows into the input shape
   --  mask for Window. i.e. the union of all input masks for Window and its
   --  children will become the new input mask for Window. See
   --  Gdk.Window.Input_Shape_Combine_Region.
   --  This function is distinct from Gdk.Window.Set_Child_Input_Shapes
   --  because it includes Window's input shape mask in the set of shapes to be
   --  merged.
   --  Since: gtk+ 2.10

   procedure Merge_Child_Shapes (Self : Gdk.Gdk_Window);
   pragma Import (C, Merge_Child_Shapes, "gdk_window_merge_child_shapes");
   --  Merges the shape masks for any child windows into the shape mask for
   --  Window. i.e. the union of all masks for Window and its children will
   --  become the new mask for Window. See Gdk.Window.Shape_Combine_Region.
   --  This function is distinct from Gdk.Window.Set_Child_Shapes because it
   --  includes Window's shape mask in the set of shapes to be merged.

   procedure Move (Self : Gdk.Gdk_Window; X : Glib.Gint; Y : Glib.Gint);
   pragma Import (C, Move, "gdk_window_move");
   --  Repositions a window relative to its parent window. For toplevel
   --  windows, window managers may ignore or modify the move; you should
   --  probably use Gtk.Window.Move on a Gtk.Window.Gtk_Window widget anyway,
   --  instead of using GDK functions. For child windows, the move will
   --  reliably succeed.
   --  If you're also planning to resize the window, use
   --  Gdk.Window.Move_Resize to both move and resize simultaneously, for a
   --  nicer visual effect.
   --  "x": X coordinate relative to window's parent
   --  "y": Y coordinate relative to window's parent

   procedure Move_Region
      (Self   : Gdk.Gdk_Window;
       Region : Cairo.Region.Cairo_Region;
       Dx     : Glib.Gint;
       Dy     : Glib.Gint);
   pragma Import (C, Move_Region, "gdk_window_move_region");
   --  Move the part of Window indicated by Region by Dy pixels in the Y
   --  direction and Dx pixels in the X direction. The portions of Region that
   --  not covered by the new position of Region are invalidated.
   --  Child windows are not moved.
   --  Since: gtk+ 2.8
   --  "region": The cairo_region_t to move
   --  "dx": Amount to move in the X direction
   --  "dy": Amount to move in the Y direction

   procedure Move_Resize
      (Self   : Gdk.Gdk_Window;
       X      : Glib.Gint;
       Y      : Glib.Gint;
       Width  : Glib.Gint;
       Height : Glib.Gint);
   pragma Import (C, Move_Resize, "gdk_window_move_resize");
   --  Equivalent to calling Gdk.Window.Move and Gdk.Window.Resize, except
   --  that both operations are performed at once, avoiding strange visual
   --  effects. (i.e. the user may be able to see the window first move, then
   --  resize, if you don't use Gdk.Window.Move_Resize.)
   --  "x": new X position relative to window's parent
   --  "y": new Y position relative to window's parent
   --  "width": new width
   --  "height": new height

   procedure Move_To_Rect
      (Self           : Gdk.Gdk_Window;
       Rect           : Gdk.Rectangle.Gdk_Rectangle;
       Rect_Anchor    : Gdk_Gravity;
       Window_Anchor  : Gdk_Gravity;
       Anchor_Hints   : Gdk_Anchor_Hints;
       Rect_Anchor_Dx : Glib.Gint;
       Rect_Anchor_Dy : Glib.Gint);
   pragma Import (C, Move_To_Rect, "gdk_window_move_to_rect");
   --  Moves Window to Rect, aligning their anchor points.
   --  Rect is relative to the top-left corner of the window that Window is
   --  transient for. Rect_Anchor and Window_Anchor determine anchor points on
   --  Rect and Window to pin together. Rect's anchor point can optionally be
   --  offset by Rect_Anchor_Dx and Rect_Anchor_Dy, which is equivalent to
   --  offsetting the position of Window.
   --  Anchor_Hints determines how Window will be moved if the anchor points
   --  cause it to move off-screen. For example, Gdk.Anchor_Flip_X will replace
   --  Gdk.Gravity_North_West with Gdk.Gravity_North_East and vice versa if
   --  Window extends beyond the left or right edges of the monitor.
   --  Connect to the Gdk.Gdk_Window::moved-to-rect signal to find out how it
   --  was actually positioned.
   --  Since: gtk+ 3.24
   --  "rect": the destination Gdk.Rectangle.Gdk_Rectangle to align Window
   --  with
   --  "rect_anchor": the point on Rect to align with Window's anchor point
   --  "window_anchor": the point on Window to align with Rect's anchor point
   --  "anchor_hints": positioning hints to use when limited on space
   --  "rect_anchor_dx": horizontal offset to shift Window, i.e. Rect's anchor
   --  point
   --  "rect_anchor_dy": vertical offset to shift Window, i.e. Rect's anchor
   --  point

   function Peek_Children
      (Self : Gdk.Gdk_Window) return Gdk_Window_List.Glist;
   --  Like Gdk.Window.Get_Children, but does not copy the list of children,
   --  so the list does not need to be freed.

   procedure Process_Updates
      (Self            : Gdk.Gdk_Window;
       Update_Children : Boolean);
   pragma Obsolescent (Process_Updates);
   --  Sends one or more expose events to Window. The areas in each expose
   --  event will cover the entire update area for the window (see
   --  Gdk.Window.Invalidate_Region for details). Normally GDK calls
   --  Gdk.Window.Process_All_Updates on your behalf, so there's no need to
   --  call this function unless you want to force expose events to be
   --  delivered immediately and synchronously (vs. the usual case, where GDK
   --  delivers them in an idle handler). Occasionally this is useful to
   --  produce nicer scrolling behavior, for example.
   --  Deprecated since 3.22, 1
   --  "update_children": whether to also process updates for child windows

   procedure Gdk_Raise (Self : Gdk.Gdk_Window);
   pragma Import (C, Gdk_Raise, "gdk_window_raise");
   --  Raises Window to the top of the Z-order (stacking order), so that other
   --  windows with the same parent window appear below Window. This is true
   --  whether or not the windows are visible.
   --  If Window is a toplevel, the window manager may choose to deny the
   --  request to move the window in the Z-order, Gdk.Window.Gdk_Raise only
   --  requests the restack, does not guarantee it.

   procedure Register_Dnd (Self : Gdk.Gdk_Window);
   pragma Import (C, Register_Dnd, "gdk_window_register_dnd");
   --  Registers a window as a potential drop destination.

   procedure Reparent
      (Self       : Gdk.Gdk_Window;
       New_Parent : Gdk.Gdk_Window;
       X          : Glib.Gint;
       Y          : Glib.Gint);
   pragma Import (C, Reparent, "gdk_window_reparent");
   --  Reparents Window into the given New_Parent. The window being reparented
   --  will be unmapped as a side effect.
   --  "new_parent": new parent to move Window into
   --  "x": X location inside the new parent
   --  "y": Y location inside the new parent

   procedure Resize
      (Self   : Gdk.Gdk_Window;
       Width  : Glib.Gint;
       Height : Glib.Gint);
   pragma Import (C, Resize, "gdk_window_resize");
   --  Resizes Window; for toplevel windows, asks the window manager to resize
   --  the window. The window manager may not allow the resize. When using
   --  GTK+, use Gtk.Window.Resize instead of this low-level GDK function.
   --  Windows may not be resized below 1x1.
   --  If you're also planning to move the window, use Gdk.Window.Move_Resize
   --  to both move and resize simultaneously, for a nicer visual effect.
   --  "width": new width of the window
   --  "height": new height of the window

   procedure Restack
      (Self    : Gdk.Gdk_Window;
       Sibling : Gdk.Gdk_Window;
       Above   : Boolean);
   --  Changes the position of Window in the Z-order (stacking order), so that
   --  it is above Sibling (if Above is True) or below Sibling (if Above is
   --  False).
   --  If Sibling is null, then this either raises (if Above is True) or
   --  lowers the window.
   --  If Window is a toplevel, the window manager may choose to deny the
   --  request to move the window in the Z-order, Gdk.Window.Restack only
   --  requests the restack, does not guarantee it.
   --  Since: gtk+ 2.18
   --  "sibling": a Gdk.Gdk_Window that is a sibling of Window, or null
   --  "above": a boolean

   procedure Scroll (Self : Gdk.Gdk_Window; Dx : Glib.Gint; Dy : Glib.Gint);
   pragma Import (C, Scroll, "gdk_window_scroll");
   --  Scroll the contents of Window, both pixels and children, by the given
   --  amount. Window itself does not move. Portions of the window that the
   --  scroll operation brings in from offscreen areas are invalidated. The
   --  invalidated region may be bigger than what would strictly be necessary.
   --  For X11, a minimum area will be invalidated if the window has no
   --  subwindows, or if the edges of the window's parent do not extend beyond
   --  the edges of the window. In other cases, a multi-step process is used to
   --  scroll the window which may produce temporary visual artifacts and
   --  unnecessary invalidations.
   --  "dx": Amount to scroll in the X direction
   --  "dy": Amount to scroll in the Y direction

   procedure Set_Background
      (Self  : Gdk.Gdk_Window;
       Color : Gdk.Color.Gdk_Color);
   pragma Import (C, Set_Background, "gdk_window_set_background");
   pragma Obsolescent (Set_Background);
   --  Sets the background color of Window.
   --  However, when using GTK+, influence the background of a widget using a
   --  style class or CSS — if you're an application — or with
   --  Gtk.Style_Context.Set_Background — if you're implementing a custom
   --  widget.
   --  Deprecated since 3.4, 1
   --  "color": a Gdk.Color.Gdk_Color

   procedure Set_Background_Rgba
      (Self : Gdk.Gdk_Window;
       Rgba : Gdk.RGBA.Gdk_RGBA);
   pragma Import (C, Set_Background_Rgba, "gdk_window_set_background_rgba");
   pragma Obsolescent (Set_Background_Rgba);
   --  Sets the background color of Window.
   --  See also Gdk.Window.Set_Background_Pattern.
   --  Deprecated since 3.22, 1
   --  "rgba": a Gdk.RGBA.Gdk_RGBA color

   procedure Set_Child_Input_Shapes (Self : Gdk.Gdk_Window);
   pragma Import (C, Set_Child_Input_Shapes, "gdk_window_set_child_input_shapes");
   --  Sets the input shape mask of Window to the union of input shape masks
   --  for all children of Window, ignoring the input shape mask of Window
   --  itself. Contrast with Gdk.Window.Merge_Child_Input_Shapes which includes
   --  the input shape mask of Window in the masks to be merged.
   --  Since: gtk+ 2.10

   procedure Set_Child_Shapes (Self : Gdk.Gdk_Window);
   pragma Import (C, Set_Child_Shapes, "gdk_window_set_child_shapes");
   --  Sets the shape mask of Window to the union of shape masks for all
   --  children of Window, ignoring the shape mask of Window itself. Contrast
   --  with Gdk.Window.Merge_Child_Shapes which includes the shape mask of
   --  Window in the masks to be merged.

   procedure Set_Functions
      (Self      : Gdk.Gdk_Window;
       Functions : Gdk_WMFunction);
   pragma Import (C, Set_Functions, "gdk_window_set_functions");
   --  Sets hints about the window management functions to make available via
   --  buttons on the window frame.
   --  On the X backend, this function sets the traditional Motif window
   --  manager hint for this purpose. However, few window managers do anything
   --  reliable or interesting with this hint. Many ignore it entirely.
   --  The Functions argument is the logical OR of values from the
   --  Gdk.Window.Gdk_WMFunction enumeration. If the bitmask includes
   --  GDK_FUNC_ALL, then the other bits indicate which functions to disable;
   --  if it doesn't include GDK_FUNC_ALL, it indicates which functions to
   --  enable.
   --  "functions": bitmask of operations to allow on Window

   procedure Set_Geometry_Hints
      (Self      : Gdk.Gdk_Window;
       Geometry  : Gdk_Geometry;
       Geom_Mask : Gdk_Window_Hints);
   pragma Import (C, Set_Geometry_Hints, "gdk_window_set_geometry_hints");
   --  Sets the geometry hints for Window. Hints flagged in Geom_Mask are set,
   --  hints not flagged in Geom_Mask are unset. To unset all hints, use a
   --  Geom_Mask of 0 and a Geometry of null.
   --  This function provides hints to the windowing system about acceptable
   --  sizes for a toplevel window. The purpose of this is to constrain user
   --  resizing, but the windowing system will typically (but is not required
   --  to) also constrain the current size of the window to the provided values
   --  and constrain programatic resizing via Gdk.Window.Resize or
   --  Gdk.Window.Move_Resize.
   --  Note that on X11, this effect has no effect on windows of type
   --  Gdk.Window_Temp or windows where override redirect has been turned on
   --  via Gdk.Window.Set_Override_Redirect since these windows are not
   --  resizable by the user.
   --  Since you can't count on the windowing system doing the constraints for
   --  programmatic resizes, you should generally call
   --  Gdk.Window.Constrain_Size yourself to determine appropriate sizes.
   --  "geometry": geometry hints
   --  "geom_mask": bitmask indicating fields of Geometry to pay attention to

   procedure Set_Icon_Name (Self : Gdk.Gdk_Window; Name : UTF8_String := "");
   --  Windows may have a name used while minimized, distinct from the name
   --  they display in their titlebar. Most of the time this is a bad idea from
   --  a user interface standpoint. But you can set such a name with this
   --  function, if you like.
   --  After calling this with a non-null Name, calls to Gdk.Window.Set_Title
   --  will not update the icon title.
   --  Using null for Name unsets the icon title; further calls to
   --  Gdk.Window.Set_Title will again update the icon title as well.
   --  Note that some platforms don't support window icons.
   --  "name": name of window while iconified (minimized)

   procedure Set_Invalidate_Handler
      (Self    : Gdk.Gdk_Window;
       Handler : Gdk_Window_Invalidate_Handler_Func);
   --  Registers an invalidate handler for a specific window. This will get
   --  called whenever a region in the window or its children is invalidated.
   --  This can be used to record the invalidated region, which is useful if
   --  you are keeping an offscreen copy of some region and want to keep it up
   --  to date. You can also modify the invalidated region in case you're doing
   --  some effect where e.g. a child widget appears in multiple places.
   --  Since: gtk+ 3.10
   --  "handler": a Gdk_Window_Invalidate_Handler_Func callback function

   procedure Set_Keep_Above (Self : Gdk.Gdk_Window; Setting : Boolean);
   --  Set if Window must be kept above other windows. If the window was
   --  already above, then this function does nothing.
   --  On X11, asks the window manager to keep Window above, if the window
   --  manager supports this operation. Not all window managers support this,
   --  and some deliberately ignore it or don't have a concept of "keep above";
   --  so you can't rely on the window being kept above. But it will happen
   --  with most standard window managers, and GDK makes a best effort to get
   --  it to happen.
   --  Since: gtk+ 2.4
   --  "setting": whether to keep Window above other windows

   procedure Set_Keep_Below (Self : Gdk.Gdk_Window; Setting : Boolean);
   --  Set if Window must be kept below other windows. If the window was
   --  already below, then this function does nothing.
   --  On X11, asks the window manager to keep Window below, if the window
   --  manager supports this operation. Not all window managers support this,
   --  and some deliberately ignore it or don't have a concept of "keep below";
   --  so you can't rely on the window being kept below. But it will happen
   --  with most standard window managers, and GDK makes a best effort to get
   --  it to happen.
   --  Since: gtk+ 2.4
   --  "setting": whether to keep Window below other windows

   procedure Set_Opacity (Self : Gdk.Gdk_Window; Opacity : Gdouble);
   pragma Import (C, Set_Opacity, "gdk_window_set_opacity");
   --  Set Window to render as partially transparent, with opacity 0 being
   --  fully transparent and 1 fully opaque. (Values of the opacity parameter
   --  are clamped to the [0,1] range.)
   --  For toplevel windows this depends on support from the windowing system
   --  that may not always be there. For instance, On X11, this works only on X
   --  screens with a compositing manager running. On Wayland, there is no
   --  per-window opacity value that the compositor would apply. Instead, use
   --  `gdk_window_set_opaque_region (window, NULL)` to tell the compositor
   --  that the entire window is (potentially) non-opaque, and draw your
   --  content with alpha, or use Gtk.Widget.Set_Opacity to set an overall
   --  opacity for your widgets.
   --  For child windows this function only works for non-native windows.
   --  For setting up per-pixel alpha topelevels, see
   --  Gdk.Screen.Get_Rgba_Visual, and for non-toplevels, see
   --  Gdk.Window.Set_Composited.
   --  Support for non-toplevel windows was added in 3.8.
   --  Since: gtk+ 2.12
   --  "opacity": opacity

   procedure Set_Opaque_Region
      (Self   : Gdk.Gdk_Window;
       Region : Cairo.Region.Cairo_Region);
   pragma Import (C, Set_Opaque_Region, "gdk_window_set_opaque_region");
   --  For optimisation purposes, compositing window managers may like to not
   --  draw obscured regions of windows, or turn off blending during for these
   --  regions. With RGB windows with no transparency, this is just the shape
   --  of the window, but with ARGB32 windows, the compositor does not know
   --  what regions of the window are transparent or not.
   --  This function only works for toplevel windows.
   --  GTK+ will update this property automatically if the Window background
   --  is opaque, as we know where the opaque regions are. If your window
   --  background is not opaque, please update this property in your
   --  Gtk.Widget.Gtk_Widget::style-updated handler.
   --  Since: gtk+ 3.10
   --  "region": a region, or null

   procedure Set_Override_Redirect
      (Self              : Gdk.Gdk_Window;
       Override_Redirect : Boolean);
   --  An override redirect window is not under the control of the window
   --  manager. This means it won't have a titlebar, won't be minimizable, etc.
   --  - it will be entirely under the control of the application. The window
   --  manager can't see the override redirect window at all.
   --  Override redirect should only be used for short-lived temporary
   --  windows, such as popup menus. Gtk.Menu.Gtk_Menu uses an override
   --  redirect window in its implementation, for example.
   --  "override_redirect": True if window should be override redirect

   procedure Set_Role (Self : Gdk.Gdk_Window; Role : UTF8_String);
   --  When using GTK+, typically you should use Gtk.Window.Set_Role instead
   --  of this low-level function.
   --  The window manager and session manager use a window's role to
   --  distinguish it from other kinds of window in the same application. When
   --  an application is restarted after being saved in a previous session, all
   --  windows with the same title and role are treated as interchangeable. So
   --  if you have two windows with the same title that should be distinguished
   --  for session management purposes, you should set the role on those
   --  windows. It doesn't matter what string you use for the role, as long as
   --  you have a different role for each non-interchangeable kind of window.
   --  "role": a string indicating its role

   procedure Set_Shadow_Width
      (Self   : Gdk.Gdk_Window;
       Left   : Glib.Gint;
       Right  : Glib.Gint;
       Top    : Glib.Gint;
       Bottom : Glib.Gint);
   pragma Import (C, Set_Shadow_Width, "gdk_window_set_shadow_width");
   --  Newer GTK+ windows using client-side decorations use extra geometry
   --  around their frames for effects like shadows and invisible borders.
   --  Window managers that want to maximize windows or snap to edges need to
   --  know where the extents of the actual frame lie, so that users don't feel
   --  like windows are snapping against random invisible edges.
   --  Note that this property is automatically updated by GTK+, so this
   --  function should only be used by applications which do not use GTK+ to
   --  create toplevel windows.
   --  Since: gtk+ 3.12
   --  "left": The left extent
   --  "right": The right extent
   --  "top": The top extent
   --  "bottom": The bottom extent

   procedure Set_Skip_Pager_Hint
      (Self        : Gdk.Gdk_Window;
       Skips_Pager : Boolean);
   --  Toggles whether a window should appear in a pager (workspace switcher,
   --  or other desktop utility program that displays a small thumbnail
   --  representation of the windows on the desktop). If a window's semantic
   --  type as specified with Gdk.Window.Set_Type_Hint already fully describes
   --  the window, this function should not be called in addition, instead you
   --  should allow the window to be treated according to standard policy for
   --  its semantic type.
   --  Since: gtk+ 2.2
   --  "skips_pager": True to skip the pager

   procedure Set_Skip_Taskbar_Hint
      (Self          : Gdk.Gdk_Window;
       Skips_Taskbar : Boolean);
   --  Toggles whether a window should appear in a task list or window list.
   --  If a window's semantic type as specified with Gdk.Window.Set_Type_Hint
   --  already fully describes the window, this function should not be called
   --  in addition, instead you should allow the window to be treated according
   --  to standard policy for its semantic type.
   --  Since: gtk+ 2.2
   --  "skips_taskbar": True to skip the taskbar

   procedure Set_Startup_Id
      (Self       : Gdk.Gdk_Window;
       Startup_Id : UTF8_String);
   --  When using GTK+, typically you should use Gtk.Window.Set_Startup_Id
   --  instead of this low-level function.
   --  Since: gtk+ 2.12
   --  "startup_id": a string with startup-notification identifier

   function Set_Static_Gravities
      (Self       : Gdk.Gdk_Window;
       Use_Static : Boolean) return Boolean;
   pragma Obsolescent (Set_Static_Gravities);
   --  Used to set the bit gravity of the given window to static, and flag it
   --  so all children get static subwindow gravity. This is used if you are
   --  implementing scary features that involve deep knowledge of the windowing
   --  system. Don't worry about it.
   --  Deprecated since 3.16, 1
   --  "use_static": True to turn on static gravity

   procedure Set_Title (Self : Gdk.Gdk_Window; Title : UTF8_String);
   --  Sets the title of a toplevel window, to be displayed in the titlebar.
   --  If you haven't explicitly set the icon name for the window (using
   --  Gdk.Window.Set_Icon_Name), the icon name will be set to Title as well.
   --  Title must be in UTF-8 encoding (as with all user-readable strings in
   --  GDK/GTK+). Title may not be null.
   --  "title": title of Window

   procedure Set_Transient_For
      (Self   : Gdk.Gdk_Window;
       Parent : Gdk.Gdk_Window);
   pragma Import (C, Set_Transient_For, "gdk_window_set_transient_for");
   --  Indicates to the window manager that Window is a transient dialog
   --  associated with the application window Parent. This allows the window
   --  manager to do things like center Window on Parent and keep Window above
   --  Parent.
   --  See Gtk.Window.Set_Transient_For if you're using Gtk.Window.Gtk_Window
   --  or Gtk.Dialog.Gtk_Dialog.
   --  "parent": another toplevel Gdk.Gdk_Window

   procedure Set_Urgency_Hint (Self : Gdk.Gdk_Window; Urgent : Boolean);
   --  Toggles whether a window needs the user's urgent attention.
   --  Since: gtk+ 2.8
   --  "urgent": True if the window is urgent

   procedure Set_User_Data
      (Self      : Gdk.Gdk_Window;
       User_Data : System.Address);
   pragma Import (C, Set_User_Data, "gdk_window_set_user_data");
   --  For most purposes this function is deprecated in favor of
   --  g_object_set_data. However, for historical reasons GTK+ stores the
   --  Gtk.Widget.Gtk_Widget that owns a Gdk.Gdk_Window as user data on the
   --  Gdk.Gdk_Window. So, custom widget implementations should use this
   --  function for that. If GTK+ receives an event for a Gdk.Gdk_Window, and
   --  the user data for the window is non-null, GTK+ will assume the user data
   --  is a Gtk.Widget.Gtk_Widget, and forward the event to that widget.
   --  "user_data": user data

   procedure Shape_Combine_Region
      (Self         : Gdk.Gdk_Window;
       Shape_Region : Cairo.Region.Cairo_Region;
       Offset_X     : Glib.Gint;
       Offset_Y     : Glib.Gint);
   pragma Import (C, Shape_Combine_Region, "gdk_window_shape_combine_region");
   --  Makes pixels in Window outside Shape_Region be transparent, so that the
   --  window may be nonrectangular.
   --  If Shape_Region is null, the shape will be unset, so the whole window
   --  will be opaque again. Offset_X and Offset_Y are ignored if Shape_Region
   --  is null.
   --  On the X11 platform, this uses an X server extension which is widely
   --  available on most common platforms, but not available on very old X
   --  servers, and occasionally the implementation will be buggy. On servers
   --  without the shape extension, this function will do nothing.
   --  This function works on both toplevel and child windows.
   --  "shape_region": region of window to be non-transparent
   --  "offset_x": X position of Shape_Region in Window coordinates
   --  "offset_y": Y position of Shape_Region in Window coordinates

   procedure Show (Self : Gdk.Gdk_Window);
   pragma Import (C, Show, "gdk_window_show");
   --  Like Gdk.Window.Show_Unraised, but also raises the window to the top of
   --  the window stack (moves the window to the front of the Z-order).
   --  This function maps a window so it's visible onscreen. Its opposite is
   --  Gdk.Window.Hide.
   --  When implementing a Gtk.Widget.Gtk_Widget, you should call this
   --  function on the widget's Gdk.Gdk_Window as part of the "map" method.

   procedure Show_Unraised (Self : Gdk.Gdk_Window);
   pragma Import (C, Show_Unraised, "gdk_window_show_unraised");
   --  Shows a Gdk.Gdk_Window onscreen, but does not modify its stacking
   --  order. In contrast, Gdk.Window.Show will raise the window to the top of
   --  the window stack.
   --  On the X11 platform, in Xlib terms, this function calls XMapWindow (it
   --  also updates some internal GDK state, which means that you can't really
   --  use XMapWindow directly on a GDK window).

   function Show_Window_Menu
      (Self  : Gdk.Gdk_Window;
       Event : Gdk.Event.Gdk_Event) return Boolean;
   --  Asks the windowing system to show the window menu. The window menu is
   --  the menu shown when right-clicking the titlebar on traditional windows
   --  managed by the window manager. This is useful for windows using
   --  client-side decorations, activating it with a right-click on the window
   --  decorations.
   --  Since: gtk+ 3.14
   --  "event": a Gdk.Event.Gdk_Event to show the menu for

   procedure Stick (Self : Gdk.Gdk_Window);
   pragma Import (C, Stick, "gdk_window_stick");
   --  "Pins" a window such that it's on all workspaces and does not scroll
   --  with viewports, for window managers that have scrollable viewports.
   --  (When using Gtk.Window.Gtk_Window, Gtk.Window.Stick may be more useful.)
   --  On the X11 platform, this function depends on window manager support,
   --  so may have no effect with many window managers. However, GDK will do
   --  the best it can to convince the window manager to stick the window. For
   --  window managers that don't support this operation, there's nothing you
   --  can do to force it to happen.

   procedure Thaw_Toplevel_Updates_Libgtk_Only (Self : Gdk.Gdk_Window);
   pragma Import (C, Thaw_Toplevel_Updates_Libgtk_Only, "gdk_window_thaw_toplevel_updates_libgtk_only");
   pragma Obsolescent (Thaw_Toplevel_Updates_Libgtk_Only);
   --  Thaws a window frozen with
   --  Gdk.Window.Freeze_Toplevel_Updates_Libgtk_Only.
   --  This function is not part of the GDK public API and is only for use by
   --  GTK+.
   --  Deprecated since 3.16, 1

   procedure Thaw_Updates (Self : Gdk.Gdk_Window);
   pragma Import (C, Thaw_Updates, "gdk_window_thaw_updates");
   --  Thaws a window frozen with Gdk.Window.Freeze_Updates.

   procedure Unfullscreen (Self : Gdk.Gdk_Window);
   pragma Import (C, Unfullscreen, "gdk_window_unfullscreen");
   --  Moves the window out of fullscreen mode. If the window was not
   --  fullscreen, does nothing.
   --  On X11, asks the window manager to move Window out of the fullscreen
   --  state, if the window manager supports this operation. Not all window
   --  managers support this, and some deliberately ignore it or don't have a
   --  concept of "fullscreen"; so you can't rely on the unfullscreenification
   --  actually happening. But it will happen with most standard window
   --  managers, and GDK makes a best effort to get it to happen.
   --  Since: gtk+ 2.2

   procedure Unmaximize (Self : Gdk.Gdk_Window);
   pragma Import (C, Unmaximize, "gdk_window_unmaximize");
   --  Unmaximizes the window. If the window wasn't maximized, then this
   --  function does nothing.
   --  On X11, asks the window manager to unmaximize Window, if the window
   --  manager supports this operation. Not all window managers support this,
   --  and some deliberately ignore it or don't have a concept of "maximized";
   --  so you can't rely on the unmaximization actually happening. But it will
   --  happen with most standard window managers, and GDK makes a best effort
   --  to get it to happen.
   --  On Windows, reliably unmaximizes the window.

   procedure Unstick (Self : Gdk.Gdk_Window);
   pragma Import (C, Unstick, "gdk_window_unstick");
   --  Reverse operation for Gdk.Window.Stick; see Gdk.Window.Stick, and
   --  Gtk.Window.Unstick.

   procedure Withdraw (Self : Gdk.Gdk_Window);
   pragma Import (C, Withdraw, "gdk_window_withdraw");
   --  Withdraws a window (unmaps it and asks the window manager to forget
   --  about it). This function is not really useful as Gdk.Window.Hide
   --  automatically withdraws toplevel windows before hiding them.

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get_User_Data (Window : Gdk_Window) return Glib.Object.GObject;
   --  Return the widget to which events are reported when they happen on
   --  Window. This is the widget that was set through the call to
   --  Set_User_data.

   procedure Set_User_Data
     (Window : Gdk_Window;
      Widget : access Glib.Object.GObject_Record'Class);
   --  Sets a special field in the window.
   --  All the events reported by the Xserver (or the Windows server) for
   --  Window will be redirected to Widget through the standard signals
   --  "expose_event", "button_press_event", ...
   --  You almost always need to call this function after creating a new
   --  Gdk_Window yourself, or you won't be able to handle the events.

   procedure Ref (Window : Gdk_Window);
   procedure Unref (Window : Gdk_Window);
   pragma Import (C, Ref, "g_object_ref");
   pragma Import (C, Unref, "g_object_unref");

   ---------------
   -- Functions --
   ---------------

   procedure At_Pointer
      (Win_X  : out Glib.Gint;
       Win_Y  : out Glib.Gint;
       Window : out Gdk.Gdk_Window);
   pragma Obsolescent (At_Pointer);
   --  Obtains the window underneath the mouse pointer, returning the location
   --  of that window in Win_X, Win_Y. Returns null if the window under the
   --  mouse pointer is not known to GDK (if the window belongs to another
   --  application and a Gdk.Gdk_Window hasn't been created for it with
   --  gdk_window_foreign_new)
   --  NOTE: For multihead-aware widgets or applications use
   --  gdk_display_get_window_at_pointer instead.
   --  Deprecated since 3.0, 1
   --  "win_x": return location for origin of the window under the pointer
   --  "win_y": return location for origin of the window under the pointer

   procedure Constrain_Size
      (Geometry   : Gdk_Geometry;
       Flags      : Gdk_Window_Hints;
       Width      : Glib.Gint;
       Height     : Glib.Gint;
       New_Width  : out Glib.Gint;
       New_Height : out Glib.Gint);
   pragma Import (C, Constrain_Size, "gdk_window_constrain_size");
   --  Constrains a desired width and height according to a set of geometry
   --  hints (such as minimum and maximum size).
   --  "geometry": a Gdk.Window.Gdk_Geometry structure
   --  "flags": a mask indicating what portions of Geometry are set
   --  "width": desired width of window
   --  "height": desired height of the window
   --  "new_width": location to store resulting width
   --  "new_height": location to store resulting height

   procedure Process_All_Updates;
   pragma Import (C, Process_All_Updates, "gdk_window_process_all_updates");
   pragma Obsolescent (Process_All_Updates);
   --  Calls Gdk.Window.Process_Updates for all windows (see Gdk.Gdk_Window)
   --  in the application.
   --  Deprecated since 3.22, 1

   procedure Set_Debug_Updates (Setting : Boolean);
   pragma Obsolescent (Set_Debug_Updates);
   --  With update debugging enabled, calls to Gdk.Window.Invalidate_Region
   --  clear the invalidated region of the screen to a noticeable color, and
   --  GDK pauses for a short time before sending exposes to windows during
   --  Gdk.Window.Process_Updates. The net effect is that you can see the
   --  invalid region for each window and watch redraws as they occur. This
   --  allows you to diagnose inefficiencies in your application.
   --  In essence, because the GDK rendering model prevents all flicker, if
   --  you are redrawing the same region 400 times you may never notice, aside
   --  from noticing a speed problem. Enabling update debugging causes GTK to
   --  flicker slowly and noticeably, so you can see exactly what's being
   --  redrawn when, in what order.
   --  The --gtk-debug=updates command line option passed to GTK+ programs
   --  enables this debug option at application startup time. That's usually
   --  more useful than calling Gdk.Window.Set_Debug_Updates yourself, though
   --  you might want to use this function to enable updates sometime after
   --  application startup time.
   --  Deprecated since 3.22, 1
   --  "setting": True to turn on update debugging

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Cursor_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Cursor
   --  The mouse pointer for a Gdk.Gdk_Window. See Gdk.Window.Set_Cursor and
   --  Gdk.Window.Get_Cursor for details.

   -------------
   -- Signals --
   -------------

   Signal_Create_Surface : constant Glib.Signal_Name := "create-surface";
   --  The ::create-surface signal is emitted when an offscreen window needs
   --  its surface (re)created, which happens either when the window is first
   --  drawn to, or when the window is being resized. The first signal handler
   --  that returns a non-null surface will stop any further signal emission,
   --  and its surface will be used.
   --
   --  Note that it is not possible to access the window's previous surface
   --  from within any callback of this signal. Calling
   --  gdk_offscreen_window_get_surface will lead to a crash.
   --    function Handler
   --       (Self   : Gdk_Window;
   --        Width  : Glib.Gint;
   --        Height : Glib.Gint) return Cairo.Cairo_Surface
   -- 
   --  Callback parameters:
   --    --  "width": the width of the offscreen surface to create
   --    --  "height": the height of the offscreen surface to create
   --    --  Returns the newly created cairo_surface_t for the offscreen window

   Signal_From_Embedder : constant Glib.Signal_Name := "from-embedder";
   --  The ::from-embedder signal is emitted to translate coordinates in the
   --  embedder of an offscreen window to the offscreen window.
   --
   --  See also Gdk.Gdk_Window::to-embedder.
   --    procedure Handler
   --       (Self        : Gdk_Window;
   --        Embedder_X  : Gdouble;
   --        Embedder_Y  : Gdouble;
   --        Offscreen_X : out System.Address;
   --        Offscreen_Y : out System.Address)
   -- 
   --  Callback parameters:
   --    --  "embedder_x": x coordinate in the embedder window
   --    --  "embedder_y": y coordinate in the embedder window
   --    --  "offscreen_x": return location for the x coordinate in the offscreen
   --    --  window
   --    --  "offscreen_y": return location for the y coordinate in the offscreen
   --    --  window

   Signal_Moved_To_Rect : constant Glib.Signal_Name := "moved-to-rect";
   --  Emitted when the position of Window is finalized after being moved to a
   --  destination rectangle.
   --
   --  Window might be flipped over the destination rectangle in order to keep
   --  it on-screen, in which case Flipped_X and Flipped_Y will be set to True
   --  accordingly.
   --
   --  Flipped_Rect is the ideal position of Window after any possible
   --  flipping, but before any possible sliding. Final_Rect is Flipped_Rect,
   --  but possibly translated in the case that flipping is still ineffective
   --  in keeping Window on-screen.
   --    procedure Handler
   --       (Self         : Gdk_Window;
   --        Flipped_Rect : System.Address;
   --        Final_Rect   : System.Address;
   --        Flipped_X    : Boolean;
   --        Flipped_Y    : Boolean)
   -- 
   --  Callback parameters:
   --    --  "flipped_rect": the position of Window after any possible flipping or
   --    --  null if the backend can't obtain it
   --    --  "final_rect": the final position of Window or null if the backend can't
   --    --  obtain it
   --    --  "flipped_x": True if the anchors were flipped horizontally
   --    --  "flipped_y": True if the anchors were flipped vertically

   Signal_Pick_Embedded_Child : constant Glib.Signal_Name := "pick-embedded-child";
   --  The ::pick-embedded-child signal is emitted to find an embedded child
   --  at the given position.
   --    function Handler
   --       (Self : Gdk_Window;
   --        X    : Gdouble;
   --        Y    : Gdouble) return Gtk.Window.Gtk_Window
   -- 
   --  Callback parameters:
   --    --  "x": x coordinate in the window
   --    --  "y": y coordinate in the window
   --    --  Returns the Gdk.Gdk_Window of the
   --     embedded child at X, Y, or null

   Signal_To_Embedder : constant Glib.Signal_Name := "to-embedder";
   --  The ::to-embedder signal is emitted to translate coordinates in an
   --  offscreen window to its embedder.
   --
   --  See also Gdk.Gdk_Window::from-embedder.
   --    procedure Handler
   --       (Self        : Gdk_Window;
   --        Offscreen_X : Gdouble;
   --        Offscreen_Y : Gdouble;
   --        Embedder_X  : out System.Address;
   --        Embedder_Y  : out System.Address)
   -- 
   --  Callback parameters:
   --    --  "offscreen_x": x coordinate in the offscreen window
   --    --  "offscreen_y": y coordinate in the offscreen window
   --    --  "embedder_x": return location for the x coordinate in the embedder
   --    --  window
   --    --  "embedder_y": return location for the y coordinate in the embedder
   --    --  window

private
   Cursor_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("cursor");
end Gdk.Window;
