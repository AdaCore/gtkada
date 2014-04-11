------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2014, AdaCore                     --
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
--  This package is a rewrite of Gtkada.Canvas, with hopefully more
--  capabilities and a cleaner API.
--
--  It provides a drawing area (canvas) on which items can be displayed and
--  linked together. It also supports interactive manipulation of those
--  items.
--
--  This package is organized around the concept of Model-View-Controller:
--    - The model is an item that gives access to all the items contained
--      in the canvas, although it need not necessarily own them. A default
--      model implementation is provided which indeed stores the items
--      internally, but it is possible to create a model which is a simple
--      wrapper around an application-specific API that would already have the
--      list of items.
--
--    - The view is in charge of representing the model, or a subset of it. It
--      is possible to have multiple views for a single model, each displaying
--      a different subset or a different part of the whole canvas.
--      When a view is put inside a Gtk_Scrolled_Window, it automatically
--      supports scrolling either via the scrollbars, or directly with the
--      mouse wheel or touchpad.
--
--    - The controller provides the user interaction in the canvas, and will
--      change the view and model properties when the user performs actions.
--
--  A view does not draw any background (image, grid,...). This is because
--  there are simply too many ways application want to take advantage of the
--  background. Instead, you should override the Draw_Internal primitive and
--  take advantage (optionally) of some of the helps in
--  Gtkada.Canvas_View.Views, which among other things provide ways to draw
--  grids.
--
--  Likewise, a view does not handle events by default (except for scrolling
--  when it is put in a Gtk_Scrolled_Window). This is also because applications
--  want to do widely different things (for some, clicking in the background
--  should open a menu, whereas others will want to let the user scroll by
--  dragging the mouse in the background -- likewise when clicking on items
--  for instance).
--
--  Differences with Gtkada.Canvas
--  ==============================
--
--  This package is organized around the concept of Model-View-Controller,
--  which provides a much more flexible approach. There is for instance no
--  need to duplicate the items in memory if you already have them available
--  somewhere else in your application.
--
--  Various settings that were set on an Interactive_Canvas (like the font for
--  annotations, arrow sizes,...) are now configured on each item or link
--  separately, which provides much more flexibility in what this canvas can
--  display.
--
--  The support for items is much richer: via a number of new primitive
--  operations, it is possible to control with more details the behavior of
--  items and where links should be attached to them.
--  More importantly, this package provides a ready-to-use set of predefined
--  items (rectangles, circles, text, polygons,...) which can be composited
--  and have automatic size computation. This makes it easier than before to
--  have an item that contains, for instance, a list of text fields, since
--  there is no need any more to compute the size of the text explicitly.
--
--  This package systematically use a Gdouble for coordinates (in any of the
--  coordinate systems), instead of the mix of Gint, Gdouble and Gfloat that
--  the Gtkada.Canvas is using. In fact, most of the time applications will
--  only have to deal with the item coordinate system (see below), and never
--  with the view coordinate system.
--
--  The behavior of snap-to-grid is different: whereas in Gtkada.Canvas it
--  forces items to always be aligned with the grid (with no way to have items
--  not aligned), the Canvas_View's effect is more subtle: basically, when an
--  item is moved closed enough to the grid, it will be aligned to the grid.
--  But if it is far from any grid line, you can drop it anywhere.
--
--  User interaction
--  ================
--
--  By default, limited user interaction is supported (scrolling when the view
--  is put in a Gtk_Scrolled_Window, including with mouse wheel and touchpad).
--
--  But of course it supports much more advanced interactions, like clicking
--  on items, moving them with the mouse or keyboard,...
--
--  For this, you need to connect to the "item_event" signal, and either
--  directly handle the signal (a simple click for instance), or set some
--  data in the details parameters, to enable dragging items or the background
--  of the canvas (for crolling).
--
--  The following has not been backported yet:
--  ==========================================
--  It also supports scrolling if put in a Gtk_Scrolled_Window.
--  The canvas will be scrolled (and the selected items moved) if an item is
--  selected and the mouse is dragged on a small area on the side of the canvas
--  or even directly outside of the canvas. Scrolling will continue until the
--  mouse is either released or moved back inside the canvas.
--
--  The scrolling speed will slightly increase over time if the mouse is kept
--  outside of the canvas. This makes the canvas much more comfortable to use
--  for the user.
--
--  The items can also react to mouse events: mouse clicks are transmitted to
--  the item if the mouse did not move more than a given amount of pixels.
--  To decide what their reaction should be, you should override the
--  On_Button_Click subprogram.
--
--  Items are selected automatically when they are clicked. If Control is
--  pressed at the same time, multiple items can be selected.
--  If the background is clicked (and control is not pressed), then all items
--  are unselected.
--  Pressing and dragging the mouse in the backgroudn draws a virtual box on
--  the screen. All the items fully included in this box when it is released
--  will be selected (this will replace the current selection if Control was
--  not pressed).
--
--  </description>
--  <group>Drawing</group>
--  <testgtk>create_canvas_view.adb</testgtk>

pragma Ada_2012;

with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Unchecked_Deallocation;
private with GNAT.Strings;
with Cairo;
with Gdk.Types;        use Gdk.Types;
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Handlers;
with Gtk.Widget;
with Gtkada.Style;     use Gtkada.Style;
with Pango.Layout;     use Pango.Layout;

package Gtkada.Canvas_View is

   -----------------
   -- Coordinates --
   -----------------
   --  There are multiple coordinate systems used in this API. Here is a full
   --  description:
   --
   --  - Model coordinates: these are the coordinates of items without
   --    considering canvas scrolling or zooming. These do not change when the
   --    view is zoomed or scrolled, and these are therefore the coordinates
   --    that are stored in the model.
   --    The drawing of links is done within this system.
   --    These coordinates are in general oriented so that x increases towards
   --    the right, and y increases towards the bottom of the screen. This
   --    can be changed by overriding Set_Transform below.
   --
   --  - View coordinates: these are the coordinates of items in the widget
   --    representing the view. They change when the view is scrolled or
   --    zoomed. These coordinates are mostly an implementation detail.
   --
   --  - Item coordinates: these are the coordinates relative to the
   --    top-left corner of an item as if it was displayed at a zoom level of
   --    100%. All drawing of items is done with this system, so that the
   --    same item can be displayed at different positions in the view
   --    without changing the drawing instructions.
   --    The drawing coordinates are automatically converted to the view
   --    coordinates by the use of a transformation matrix, which is done very
   --    efficiently on modern systems.
   --
   --  - Window coordinates
   --    These are rarely used, only when interfacing with gtk+ events. These
   --    are the coordinates relative to the Gdk_Window of the view.

   subtype Model_Coordinate  is Gdouble;
   subtype View_Coordinate   is Gdouble;
   subtype Item_Coordinate   is Gdouble;
   subtype Window_Coordinate is Gdouble;
   --  We use subtypes for convenience in your applications to avoid casts.

   type Model_Rectangle  is record
     X, Y, Width, Height : Model_Coordinate;
   end record;
   type View_Rectangle   is record
      X, Y, Width, Height : View_Coordinate;
   end record;
   type Item_Rectangle   is record
      X, Y, Width, Height : Item_Coordinate;
   end record;
   type Window_Rectangle is record
      X, Y, Width, Height : Window_Coordinate;
   end record;
   --  A rectangle in various coordinates

   type Model_Point is record
      X, Y : Model_Coordinate;
   end record;
   type View_Point  is record
      X, Y : View_Coordinate;
   end record;
   type Window_Point  is record
      X, Y : Window_Coordinate;
   end record;
   subtype Item_Point  is Gtkada.Style.Point;
   --  A point in various coordinates

   type Model_Point_Array is array (Natural range <>) of Model_Point;
   type Model_Point_Array_Access is access Model_Point_Array;

   subtype Item_Point_Array is Gtkada.Style.Point_Array;
   subtype Item_Point_Array_Access is Gtkada.Style.Point_Array_Access;

   No_Rectangle : constant Model_Rectangle := (0.0, 0.0, 0.0, 0.0);
   No_Point     : constant Model_Point := (Gdouble'First, Gdouble'First);

   function Point_In_Rect
     (Rect : Model_Rectangle; P : Model_Point) return Boolean;
   function Point_In_Rect
     (Rect : Item_Rectangle; P : Item_Point) return Boolean;
   --  Whether the point is in the rectangle

   function Intersects (Rect1, Rect2 : Model_Rectangle) return Boolean;
   function Intersects (Rect1, Rect2 : Item_Rectangle) return Boolean;
   --  Whether the two rectangles intersect.

   procedure Union
     (Rect1 : in out Model_Rectangle;
      Rect2 : Model_Rectangle);
   --  Store in Rect1 the minimum rectangle that contains both Rect1 and Rect2.

   ------------------
   -- Enumerations --
   ------------------

   type Side_Attachment is (Auto, Top, Right, Bottom, Left, No_Clipping);
   --  Which side of the toplevel item the link is attached to.
   --
   --  For toplevel items, this can be controlled by using the
   --  Anchor_Attachment's X and Y properties.
   --  But for nested item, this forces the link to start from the
   --  toplevel item's border. Here is an example:
   --        +----------+
   --        | +-+      |
   --        | |A|      |\
   --        | +-+      | \1
   --        |     B    |\ \
   --        +----------+ \ \
   --                     2\ +----------------+
   --                       \|       C        |
   --                        +----------------+
   --
   --  The link 1 is attached to the nested item A, and the side_attachment
   --  is set to Right. As a result, it always starts at the same height as A
   --  itself.
   --  The link 2 is also attached to A, but the side is set to Auto. So the
   --  canvas draws the shortest path from A to C (and clips the line to the
   --  border of B). So it is not as visible that 2 is linked to A.
   --
   --  The "No_Clipping" side should be used when a link is connected to
   --  another link, since in that case there is no notion of link.

   type Anchor_Attachment is record
      X, Y          : Glib.Gdouble := 0.5;
      Toplevel_Side : Side_Attachment;
   end record;
   Middle_Attachment : constant Anchor_Attachment := (0.5, 0.5, Auto);
   --  Where in the item the link is attached (0.5 means the middle, 0.0
   --  means left or top, and 1.0 means right or bottom).
   --  You can therefore force a link to always emerge from the right side of
   --  an item by setting X to 1.0 and Y to any value, for instance.
   --  See the description of Side_Attachment for an example on how to use
   --  Toplevel_Side.

   type Route_Style is (Orthogonal, Straight, Arc, Curve);
   --  This defines how a link is routed between its two ends.
   --  Curve is similar to orthogonal (links restricted to horizontal and
   --  vertical lines), but using a bezier curve.

   ------------------
   -- Draw context --
   ------------------

   type Draw_Context is record
      Cr     : Cairo.Cairo_Context;
      Layout : Pango.Layout.Pango_Layout := null;
   end record;
   --  Context to perform the actual drawing

   --------------------
   -- Abstract Items --
   --------------------

   type Abstract_Item_Record is interface;
   type Abstract_Item is access all Abstract_Item_Record'Class;
   --  These are all the elements that can be displayed on a canvas, including
   --  the boxes, the links between the boxes, any annotations on those links,
   --  and so on.
   --  Items can be grouped, so that toplevel items contain one or more
   --  other items. The toplevel items are the ones that are moved
   --  interactively by the user, and their contained items will be moved
   --  along.
   --  All primitive operations on items, except its position, are done in the
   --  Item's own coordinate systems so that it is easy to create new types of
   --  items without paying attention to any of its parents rotation or
   --  scaling, or the rotation and scaling of the view itself).
   --
   --  This interface is meant for use when you already have ways to store
   --  coordinates and sizes in your own data types, at which point you can
   --  implement a simpler wrapper for your data type that implements this
   --  interface. In general, though, it is better to extend the type
   --  Abstract_Item_Record which provides its own non-abstract handling for a
   --  number of subprograms below.

   package Items_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Abstract_Item);

   function Position
     (Self : not null access Abstract_Item_Record)
      return Gtkada.Style.Point is abstract;
   --  The coordinates of the item within its parent.
   --  If the item has no parent, the coordinates should be returned in model
   --  coordinates. These coordinates describe the origin (0,0) point of
   --  the item's coordinate system.

   procedure Set_Position
     (Self  : not null access Abstract_Item_Record;
      Pos   : Gtkada.Style.Point) is null;
   --  Used to change the position of an item (by default an item cannot be
   --  moved). You must call the model's Refresh_Layout after moving items.

   function Bounding_Box
     (Self : not null access Abstract_Item_Record)
      return Item_Rectangle is abstract;
   --  Returns the area occupied by the item.
   --  Any drawing for the item, including shadows for instance, must be
   --  within this area.
   --  This bounding box is always returned in the item's own coordinate
   --  system, so that it is not necessary to pay attention to the current
   --  scaling factor or rotation for the item, its parents or the canvas view.

   --
   --  The coordinates of the item are always the top-left corner of their
   --  bounding box. These coordinates are either relative to the item's
   --  toplevel container, or model coordinates for toplevel items.
   --
   --  The bounding box is also used for fast detection on whether the item
   --  might be clicked on by the user.

   procedure Draw
     (Self    : not null access Abstract_Item_Record;
      Context : Draw_Context) is abstract;
   --  Draw the item on the given cairo context.
   --  A transformation matrix has already been applied to Cr, so that all
   --  drawing should be done in item-coordinates for Self, so that (0,0) is
   --  the top-left corner of Self's bounding box.
   --  Do not call this procedure directly. Instead, call
   --  Translate_And_Draw_Item below.

   procedure Translate_And_Draw_Item
     (Self    : not null access Abstract_Item_Record'Class;
      Context : Draw_Context);
   --  Translate the transformation matrix and draw the item.
   --  This procedure should be used instead of calling Draw directly.

   function Contains
     (Self    : not null access Abstract_Item_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean is abstract;
   --  Should test whether Point is within the painted region for Self (i.e.
   --  whether Self should be selected when the user clicks on the point).
   --  For an item with holes, this function should return False when the
   --  point is inside one of the holes, for instance.

   procedure Destroy
     (Self : not null access Abstract_Item_Record) is null;
   --  Called when Self is no longer needed.
   --  Do not call directly.

   function Parent
     (Self : not null access Abstract_Item_Record)
      return Abstract_Item is (null);
   --  Return the item inside which Self is contained.
   --  null is returned for toplevel items, in which case the coordinates of
   --  the bounding box are model coordinats. Otherwise, the coordinates are
   --  relative to the returned item.

   function Link_Anchor_Point
     (Self   : not null access Abstract_Item_Record;
      Anchor : Anchor_Attachment)
      return Item_Point is abstract;
   --  Return the anchor point for links to or from this item. In general,
   --  this anchor point is in the middle of the item or depends on the
   --  Anchor parameter, and the link will automatically be clipped to one
   --  of the borders. The coordinates are absolute.
   --  This anchor point can be in the middle of an item, the link itself
   --  will be clipped with a call to Clip_Line_On_Top_Level

   function Clip_Line
     (Self   : not null access Abstract_Item_Record;
      P1, P2 : Item_Point) return Item_Point is abstract;
   --  Returns the intersection of the line from P1 to P2 with the border of
   --  the item. Drawing a line from this intersection point to P2 will not
   --  intersect the item.

   function Model_Bounding_Box
     (Self     : not null access Abstract_Item_Record'Class)
      return Model_Rectangle;
   --  Return the bounding box of Self always in model coordinates.
   --  As opposed to Bounding_Box, model coordinates are also returned
   --  for nested items.

   -----------
   -- Items --
   -----------

   type Canvas_Item_Record is abstract new Abstract_Item_Record with private;
   type Canvas_Item is access all Canvas_Item_Record'Class;
   --  An implementation of the Abstract_Item interface, which handles a
   --  number of the operations automatically. For instance, it will store the
   --  position of the item and its bounding box.
   --  It is easier to derive from this type when you want to create your own
   --  items, unless you want complete control of the data storage.

   overriding function Position
     (Self : not null access Canvas_Item_Record) return Gtkada.Style.Point;
   overriding function Contains
     (Self    : not null access Canvas_Item_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean;
   overriding function Link_Anchor_Point
     (Self   : not null access Canvas_Item_Record;
      Anchor : Anchor_Attachment)
      return Item_Point;
   overriding function Clip_Line
     (Self   : not null access Canvas_Item_Record;
      P1, P2 : Item_Point) return Item_Point;
      --  Provide a default implementation for a number of primitives.
   --  Intersects only checks whether the point is within the bounding box, so
   --  only works for rectangular items.

   overriding procedure Set_Position
     (Self  : not null access Canvas_Item_Record;
      Pos   : Gtkada.Style.Point);
   --  Sets the position of the item within its parent (or within the canvas
   --  view if Self has no parent).

   ------------------
   -- Canvas_Model --
   ------------------

   type Canvas_Model_Record
      is abstract new Glib.Object.GObject_Record with private;
   type Canvas_Model is access all Canvas_Model_Record'Class;
   --  A model is a common interface to query the list of items that should
   --  be displayed in the canvas. It does not assume anything regarding the
   --  actual storage of the items, so it is possible to create your own
   --  model implementation that simply query the rest of your application
   --  (or a database, or some other source of data) as needed, without
   --  duplicating the items.
   --
   --  This type is not an Ada interface because it needs to inherit from
   --  GObject so that it can send signals.
   --
   --  The interface does not provide support for adding items to the model:
   --  instead, this is expected to be done by the concrete implementations of
   --  the model, which must then send the signal "layout_changed".

   function Model_Get_Type return Glib.GType;
   pragma Convention (C, Model_Get_Type);
   --  Return the internal type

   procedure Initialize
     (Self : not null access Canvas_Model_Record'Class);
   --  Initialize the internal data so that signals can be sent.
   --  This procedure must always be called when you create a new model.

   procedure For_Each_Item
     (Self     : not null access Canvas_Model_Record;
      Callback : not null access procedure
        (Item : not null access Abstract_Item_Record'Class);
      In_Area  : Model_Rectangle := No_Rectangle) is abstract;
   --  Calls Callback for each item in the model, not including the links
   --  which are handled specially.
   --  Only the items that intersect In_Area should be returned for
   --  efficiency, although it is valid to return all items otherwise.

   function Bounding_Box
     (Self   : not null access Canvas_Model_Record;
      Margin : Model_Coordinate := 0.0)
      return Model_Rectangle;
   --  Returns the rectangle that encompasses all the items in the model.
   --  This is used by views to compute the maximum area that should be made
   --  visible.
   --  An extra margin is added to each side of the box.
   --  The default implementation is not efficient, since it will iterate all
   --  items one by one to compute the rectangle. No caching is done.

   procedure Refresh_Layout (Self : not null access Canvas_Model_Record);
   --  Refresh the layout of Self.
   --  This procedure should be called every time items are moved (because
   --  this impacts links to or from these items), or when they are added or
   --  removed (it could also impact the layout of links if they displays to
   --  avoid going underneath items).
   --  This procedure is also used to compute the size of items (see
   --  Container_Item below).
   --  The default implementation will simply iterate over all items, but it
   --  could be implemented more efficiently.
   --
   --  WARNING: this procedure must be called only once at least one view has
   --  been created for the model. This ensures that the necessary information
   --  for the layout of text has been retrieved from the view layer. If you
   --  do not have at least one view, all text will be hidden or displayed as
   --  ellipsis.
   --  In fact, this procedure is called automatically on the model the first
   --  time it is associated with a view.

   function Toplevel_Item_At
     (Self    : not null access Canvas_Model_Record;
      Point   : Model_Point;
      Context : Draw_Context) return Abstract_Item;
   --  Return the toplevel item at the specific coordinates (if any).
   --  The default implementation simply traverses the list of items, and
   --  calls Contains on each child.
   --  This function returns the topmost item

   procedure Layout_Changed
     (Self : not null access Canvas_Model_Record'Class);
   function On_Layout_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : not null access GObject_Record'Class);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id;
   Signal_Layout_Changed : constant Glib.Signal_Name := "layout_changed";
   --  Emits or handles the "layout_changed" signal.
   --  This signal must be emitted by models whenever new items are added,
   --  existing items are resized or removed, or any other event that impacts
   --  coordinates of any item in the model.
   --  It is recommended to emit this signal only once per batch of changes,

   procedure Item_Contents_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class);
   function On_Item_Contents_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class; Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id;
   Signal_Item_Contents_Changed : constant Glib.Signal_Name :=
     "item_contents_changed";
   --  This signal should be emitted instead of layout_changed when only the
   --  contents of an item (but not its size) has changed). This will only
   --  trigger the refresh of that specific item.

   ----------------
   -- List Model --
   ----------------

   type List_Canvas_Model_Record is new Canvas_Model_Record with private;
   type List_Canvas_Model is access all List_Canvas_Model_Record'Class;
   --  A very simple-minded concrete implementation for a model.
   --  Not efficient, do not use in your code.

   procedure Gtk_New (Self : out List_Canvas_Model);
   --  Create a new model

   procedure Add
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class);
   --  Add a new item to the model

   overriding procedure For_Each_Item
     (Self     : not null access List_Canvas_Model_Record;
      Callback : not null access procedure
        (Item : not null access Abstract_Item_Record'Class);
      In_Area  : Model_Rectangle := No_Rectangle);

   -----------------
   -- Canvas_View --
   -----------------

   View_Margin : constant View_Coordinate := 20.0;
   --  The number of blank pixels on each sides of the view. This avoids having
   --  items displays exactly next to the border of the view.

   type Canvas_View_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Canvas_View is access all Canvas_View_Record'Class;
   --  A view is a display of one particular part of the model, or a subset of
   --  it. Multiple views can be associated with a specific model, and will
   --  monitor changes to it view signals.
   --  The view automatically refreshes its display when its model changes.

   procedure Gtk_New
     (Self  : out Canvas_View;
      Model : access Canvas_Model_Record'Class := null);
   procedure Initialize
     (Self  : not null access Canvas_View_Record'Class;
      Model : access Canvas_Model_Record'Class := null);
   --  Create a new view which displays the model.
   --  A new reference to the model is created (and released when the view is
   --  destroyed), so that in general the code will look like:
   --       Model := new ....;
   --       Initialize (Model);
   --       Gtk_New (View, Model);
   --       Unref (Model);  --  unless you need to keep a handle on it too

   procedure Set_Model
      (Self  : not null access Canvas_View_Record'Class;
       Model : access Canvas_Model_Record'Class);
   --  Change the model, and redraw the whole draw.

   function Model
     (Self  : not null access Canvas_View_Record'Class)
      return Canvas_Model;
   --  Return the model

   function View_Get_Type return Glib.GType;
   pragma Convention (C, View_Get_Type);
   --  Return the internal type

   procedure Draw_Internal
     (Self    : not null access Canvas_View_Record;
      Context : Draw_Context;
      Area    : Model_Rectangle);
   --  Redraw either the whole view, or a specific part of it only.
   --  The transformation matrix has already been set on the context.
   --  This procedure can be overridden if you need to perform special
   --  operations, like drawing a grid for instance. See the various helper
   --  subprograms in Gtkada.Canvas_View.Views to do so.

   function Get_Visible_Area
     (Self : not null access Canvas_View_Record)
      return Model_Rectangle;
   --  Return the area of the model that is currently displayed in the view.
   --  This is in model coordinates (since the canvas coordinates are always
   --  from (0,0) to (Self.Get_Allocation_Width, Self.Get_Allocation_Height).

   procedure Set_Transform
     (Self   : not null access Canvas_View_Record;
      Cr     : Cairo.Cairo_Context;
      Item   : access Abstract_Item_Record'Class := null);
   --  Set the transformation matrix for the current settings (scrolling and
   --  zooming).
   --
   --  The effect is that any drawing on this context should now be done using
   --  the model coordinates, which will automatically be converted to the
   --  canvas_coordinates internally.
   --
   --  If Item is specified, all drawing becomes relative to that item
   --  instead of the position of the top-left corner of the view. All drawing
   --  to this context must then be done in item_coordinates, which will
   --  automatically be converted to canvas_coordinates internally.
   --
   --  This procedure does not need to be call directly in general, since the
   --  context passed to the Draw primitive of the item has already been set
   --  up appropriately.
   --
   --  The default coordinates follow the industry standard of having y
   --  increase downwards. This is sometimes unusual for mathematically-
   --  oriented people. One solution is to override this procedure in your
   --  own view, and call Cairo.Set_Scale as in:
   --      procedure Set_Transform (Self, Cr) is
   --          Set_Transform (Canvas_View_Record (Self.all)'Access, Cr);
   --          Cairo.Set_Scale (Cr, 1.0, -1.0);
   --  which will make y increase upwards instead.

   function View_To_Model
     (Self   : not null access Canvas_View_Record;
      Rect   : View_Rectangle) return Model_Rectangle;
   function View_To_Model
     (Self   : not null access Canvas_View_Record;
      P      : View_Point) return Model_Point;
   function Model_To_View
     (Self   : not null access Canvas_View_Record;
      Rect   : Model_Rectangle) return View_Rectangle;
   function Model_To_View
     (Self   : not null access Canvas_View_Record;
      P      : Model_Point) return View_Point;
   function Model_To_Window
     (Self   : not null access Canvas_View_Record;
      Rect   : Model_Rectangle) return Window_Rectangle;
   function Window_To_Model
     (Self   : not null access Canvas_View_Record;
      Rect   : Window_Rectangle) return Model_Rectangle;
   function Window_To_Model
     (Self   : not null access Canvas_View_Record;
      P      : Window_Point) return Model_Point;
   function Item_To_Model
     (Item   : not null access Abstract_Item_Record'Class;
      Rect   : Item_Rectangle) return Model_Rectangle;
   function Item_To_Model
     (Item   : not null access Abstract_Item_Record'Class;
      P      : Item_Point) return Model_Point;
   function Model_To_Item
     (Item   : not null access Abstract_Item_Record'Class;
      P      : Model_Point) return Item_Point;
   function Model_To_Item
     (Item   : not null access Abstract_Item_Record'Class;
      P      : Model_Rectangle) return Item_Rectangle;
   --  Conversion between the various coordinate systems.
   --  Calling these should seldom be needed, as Cairo uses a transformation
   --  matrix to automatically (and efficiently) do the transformation on
   --  your behalf. See the documentation for Set_Transform.

   procedure Set_Scale
     (Self      : not null access Canvas_View_Record;
      Scale     : Gdouble := 1.0;
      Center_On : Model_Point := No_Point);
   --  Changes the scaling factor for Self.
   --  This also scrols the view so that either Center_On or the current center
   --  of the view remains at the same location in the widget, as if the user
   --  was zooming towards that specific point.

   function Get_Scale
     (Self : not null access Canvas_View_Record) return Gdouble;
   --  Return the current scale

   procedure Scale_To_Fit
     (Self      : not null access Canvas_View_Record;
      Max_Scale : Gdouble := 4.0);
   --  Chose the scale and scroll position so that the whole model is visible.
   --  This procedure leaves a small margin on each sides of the model, since
   --  that looks nicer.
   --  This function can be called even before Self has got a size assigned by
   --  window manager, but the computation of the scale will be delayed until
   --  an actual size is known.

   No_Drag_Allowed : constant Model_Rectangle := (0.0, 0.0, 0.0, 0.0);
   Drag_Anywhere   : constant Model_Rectangle :=
     (Gdouble'First, Gdouble'First, Gdouble'Last, Gdouble'Last);
   --  Values for the Event_Details.Allowed_Drag_Area field

   type Canvas_Event_Type is
     (Button_Press, Button_Release, Start_Drag, In_Drag, End_Drag, Key_Press);
   --  The event types that are emitted for the Item_Event signal:
   --  * Button_Press is called when the user presses any mouse buttton either
   --    on an item or in the background.
   --    This event can also be used to start a drag event (by
   --    setting the Allowed_Drag_Area field of the Canvas_Event_Details).
   --    It can be used also to display contextual menus.
   --
   --  * Start_Drag is used after a user has pressed a mouse button, and the
   --    callback has enabled a drag area, and the mouse has moved by at least
   --    a small margin. It applies to either the item (and all other selected
   --    items, or to the background, for instance to scroll the canvas).
   --
   --  * In_Drag is used during an actual drag.
   --
   --  * End_Drag is used after a successfull drag, when the mouse is released.
   --
   --  * Button_Release is called when the mouse is released but no drag action
   --    too place. This is the event to use to modify the current selection,
   --    either by unselecting everything, adding the specific item to the
   --    selection,...
   --
   --  * Key_Press is used when the user types something on the keyboard while
   --    the canvas has the focus. It can be used to move items with the arrow
   --    keys, edit an item,...

   type Canvas_Event_Details is record
      Event_Type     : Canvas_Event_Type;
      Button         : Guint;
      State          : Gdk.Types.Gdk_Modifier_Type;
      Root_Point     : Gtkada.Style.Point;  --  coordinates in root window
      --  Attributes of the low-level event

      M_Point        : Model_Point;
      Toplevel_Item  : Abstract_Item;
      --  higher-level details on the event

      Allowed_Drag_Area : Model_Rectangle := No_Drag_Allowed;
   end record;
   type Event_Details_Access is not null access all Canvas_Event_Details;
   --  This record describes high-level aspects of user interaction with the
   --  canvas. In addition to some information about the gtk+ event, the
   --  following information:
   --  * State describes the state of the modifier keys (shift, alt, control)
   --    and can be used to activate different behavior in such cases.
   --  * M_Point indicates where in the model the user clicked. This is
   --    independent of the zoom level or current scrolling.
   --  * Toplevel_Item is the item that was clicked on. It is set to null if
   --    the user clicked in the background.
   --  * Allowed_Drag_Area should be modified by the callback when the event is
   --    a button_press event. It should be set to the area within which the
   --    item (and all currently selected items) can be moved. If you leave it
   --    to No_Drag_Allowed, the item cannot be moved.
   --    This field is ignored for events other than button_press, since it
   --    makes no sense for instance to start a drag on a button release.

   procedure Viewport_Changed
     (Self   : not null access Canvas_View_Record'Class);
   procedure On_Viewport_Changed
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access procedure
        (Self : not null access GObject_Record'Class);
      Slot : access GObject_Record'Class := null);
   Signal_Viewport_Changed : constant Glib.Signal_Name := "viewport_changed";
   --  This signal is emitted whenever the view is zoomed or scrolled.
   --  This can be used for instance to synchronize multiple views, or display
   --  a "mini-map" of the whole view.

   procedure Item_Event
     (Self    : not null access Canvas_View_Record'Class;
      Details : Event_Details_Access);
   procedure On_Item_Event
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access procedure
        (Self    : not null access GObject_Record'Class;
         Details : Event_Details_Access);
      Slot : access GObject_Record'Class := null);
   Signal_Item_Event : constant Glib.Signal_Name := "item_event";
   --  This signal is emitted whenever the user interacts with an item (button
   --  press or release, key events,...).
   --  It is recommended to connect to this signal rather than the lower-level
   --  Button_Press_Event, Button_Release_Event,... since most information is
   --  provided here in the form of the details parameter.

   ------------------------
   -- Object hierarchies --
   ------------------------
   --  The above declarations for Abstract_Item and Canvas_Item will let you
   --  create your own custom items. However, they will require the overriding
   --  of a number of subprograms to be useful.
   --  Instead, some predefined types of items are defined below, which can
   --  be combined into a hierarchy of items: toplevel items act as
   --  containers for one or more other objets. The size of items can be
   --  computed automatically, or forced when the item is created.
   --
   --  Children can be put at specific coordinates in their parents, or
   --  stacked vertically or horizontally.

   type Container_Item_Record is abstract new Canvas_Item_Record with private;
   type Container_Item is access all Container_Item_Record'Class;

   type Child_Layout_Strategy is (Horizontal_Stack, Vertical_Stack);
   procedure Set_Child_Layout
     (Self   : not null access Container_Item_Record'Class;
      Layout : Child_Layout_Strategy);
   --  How should the children of a container be organized: either one on top
   --  of another, or one next to another.

   type Margins is record
      Top, Right, Bottom, Left : Model_Coordinate;
   end record;
   No_Margins : constant Margins := (0.0, 0.0, 0.0, 0.0);

   type Alignment_Style is (Align_Start, Align_Center, Align_End);
   --  How an item should be aligned within its parent.
   --  When the parent stacks its children vertically, alignment is taken into
   --  account horizontally; and similarly when the parent orgranizes its
   --  children horizontally, the alignment is vertical.
   --
   --  When an item does not request a specific size along the alignment axis,
   --  it always uses the full width or height of its parent, so the alignment
   --  does not play a role.
   --
   --  However, when the item requests a size smaller than its parent's along
   --  the alignment axis, extra margin needs to be added, and they are added
   --  either to its left/top (when Align_Start), to both sides (when
   --  Align_Center), or to its right/bottom (when Align_End)..
   --
   --  Alignment does not apply to floating children.

   type Overflow_Style is (Overflow_Prevent, Overflow_Hide);
   --  An overflow situation occurs when an item's contents is larger than its
   --  contents.
   --  If Overflow_Prevent is true, an item will always request enough size to
   --  fit all its contents. There might still be cases where the parent item
   --  was set to a small size, though, and the overflow is hidden nonetheless.
   --  If Overflow_Hide is true, an item will request a minimal size, and
   --  simply hide the part of its contents that does not fit.

   procedure Add_Child
     (Self     : not null access Container_Item_Record'Class;
      Child    : not null access Container_Item_Record'Class;
      Align    : Alignment_Style := Align_Start;
      Margin   : Margins := No_Margins;
      Float    : Boolean := False;
      Overflow : Overflow_Style := Overflow_Prevent);
   --  Add a new child to the container.
   --  If the child's position is set, it is then interpreted as relative to
   --  Self. If the position is not specified, it will be computed
   --  automatically based on the container's policy (either below the previous
   --  child, or to its right).
   --  Margins are added to each size of the child.
   --
   --  A floating child does not participate in the stacking: it will still be
   --  displayed below or to the right of the previous child, but the next
   --  item will then be displayed at the same coordinate as the floating
   --  child.

   procedure Set_Min_Size
     (Self       : not null access Container_Item_Record;
      Min_Width  : Gdouble := 1.0;
      Min_Height : Gdouble := 1.0);
   --  Specify a minimal size for the item, along both axis.

   procedure Size_Request
     (Self    : not null access Container_Item_Record;
      Context : Draw_Context);
   --  Compute the ideal size for Self.
   --  It might be either a size specifically forced by the user, or computed
   --  from Self's children's own size_request.
   --  The size is stored internally in the object.
   --  The requested size must not include the margins that were defined in
   --  Add_Child.
   --  Self can modify its computed position (i.e. the position within its
   --  parent) as part of the size computation in this procedure.

   procedure Size_Allocate
     (Self  : not null access Container_Item_Record);
   --  Called once the size of the parent object has been decided (i.e. after
   --  all the calls to Size_Request).
   --  The parent must set its child's position and size, and then call
   --  Size_Allocate to let it know about the final size and position.
   --  This can be used to compute attributes that need the actual size of the
   --  item (gradients, centering or right-aligning objects,...)
   --  Alignments and margins are automatically handled by the parent.

   procedure Refresh_Layout
     (Self    : not null access Container_Item_Record;
      Context : Draw_Context);
   --  Recompute the layout for Self and its children, i.e. do the full size
   --  negocitation

   procedure For_Each_Child
     (Self     : not null access Container_Item_Record'Class;
      Callback : not null access procedure
        (Child : not null access Container_Item_Record'Class));
   --  Traverse all children of Self recursively, and calls Callback for each.

   procedure Draw_Children
     (Self    : not null access Container_Item_Record'Class;
      Context : Draw_Context);
   --  Display all the children of Self

   overriding function Position
     (Self : not null access Container_Item_Record) return Gtkada.Style.Point;
   overriding function Parent
     (Self : not null access Container_Item_Record)
      return Abstract_Item;
   overriding function Bounding_Box
     (Self : not null access Container_Item_Record)
      return Item_Rectangle;

   ----------------
   -- Rectangles --
   ----------------

   type Rect_Item_Record is new Container_Item_Record with private;
   type Rect_Item is access all Rect_Item_Record'Class;
   --  A predefined type object which displays itself as a rectangle or a
   --  rectangle with rounded corners.

   function Gtk_New_Rect
     (Style         : Gtkada.Style.Drawing_Style;
      Width, Height : Model_Coordinate := -1.0;
      Radius        : Model_Coordinate := 0.0)
      return Rect_Item;
   --  Create a new rectangle item.
   --  If either Width or Height are negative, they will be computed based on
   --  the children's requested size (if there are no children, a default size
   --  is used).

   overriding procedure Draw
     (Self    : not null access Rect_Item_Record;
      Context : Draw_Context);

   --------------
   -- Ellipses --
   --------------

   type Ellipse_Item_Record is new Container_Item_Record with private;
   type Ellipse_Item is access all Ellipse_Item_Record'Class;
   --  A predefined object that displays itself as a circle or an ellipse
   --  inscribed in a given rectangle.

   function Gtk_New_Ellipse
     (Style         : Gtkada.Style.Drawing_Style;
      Width, Height : Model_Coordinate := -1.0)
      return Ellipse_Item;
   --  Create a new ellipse item.
   --  If either Width or Height are negative, they will be computed based on
   --  the children's requested size (if there are no children, a default size
   --  is used).
   --  The ellipse is inscribed in the rectangle given by the item's position
   --  and the size passed in argument to this function.

   overriding procedure Draw
     (Self    : not null access Ellipse_Item_Record;
      Context : Draw_Context);
   overriding function Contains
     (Self    : not null access Ellipse_Item_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean;

   ---------------
   -- Polylines --
   ---------------

   type Polyline_Item_Record is new Container_Item_Record with private;
   type Polyline_Item is access all Polyline_Item_Record'Class;
   --  A predefine object that displays itself as a set of joined lines.
   --  This object can optionally contain children, and the polyline can thus
   --  be used to draw a polygon around those items

   function Gtk_New_Polyline
     (Style    : Gtkada.Style.Drawing_Style;
      Points   : Item_Point_Array;
      Close    : Boolean := False;
      Relative : Boolean := False)
      return Polyline_Item;
   --  Create a new polyline item.
   --  If Relative is true, then each point is relative to the previous one
   --  (i.e. its coordinates are the previous points's coordinate plus the
   --  offset given in points). The first point is of course in item
   --  coordinates.

   overriding procedure Draw
     (Self    : not null access Polyline_Item_Record;
      Context : Draw_Context);
   overriding procedure Destroy (Self : not null access Polyline_Item_Record);
   overriding procedure Size_Request
     (Self    : not null access Polyline_Item_Record;
      Context : Draw_Context);
   overriding function Contains
     (Self    : not null access Polyline_Item_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean;

   -----------
   -- Texts --
   -----------

   type Text_Item_Record is new Container_Item_Record with private;
   type Text_Item is access all Text_Item_Record'Class;
   --  A predefined object that displays itself as text.

   type Text_Arrow_Direction is
     (No_Text_Arrow,
      Up_Text_Arrow,
      Down_Text_Arrow,
      Left_Text_Arrow,
      Right_Text_Arrow);

   function Gtk_New_Text
     (Style    : Gtkada.Style.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow)
      return Text_Item;
   --  Create a new text item
   --
   --  Directed indicates whether the text should be followed (or preceded)
   --  by a directional arrow. This is used when displaying text along links,
   --  to help users read the meaning of the label.

   procedure Set_Directed
     (Self     : not null access Text_Item_Record;
      Directed : Text_Arrow_Direction := No_Text_Arrow);
   --  Change the direction of the arrow.
   --  In particular, this is done automatically when the text is used on a
   --  link.

   procedure Set_Text
     (Self : not null access Text_Item_Record;
      Text : String);
   --  Change the text displayed in the item.
   --  This does not force a refresh of the item, and it is likely that you
   --  will need to call the Model's Refresh_Layout method to properly
   --  recompute sizes of items and link paths.

   overriding procedure Draw
     (Self    : not null access Text_Item_Record;
      Context : Draw_Context);
   overriding procedure Destroy (Self : not null access Text_Item_Record);
   overriding procedure Size_Request
     (Self    : not null access Text_Item_Record;
      Context : Draw_Context);
   overriding procedure Size_Allocate
     (Self    : not null access Text_Item_Record);

   ----------------------
   -- Horizontal lines --
   ----------------------

   type Hr_Item_Record is new Container_Item_Record with private;
   type Hr_Item is access all Hr_Item_Record'Class;
   --  A predefined object that displays itself as a horizontal line with
   --  optional text in the middle. This thus looks like:
   --              ---- text ----

   function Gtk_New_Hr
     (Style   : Gtkada.Style.Drawing_Style;
      Text    : String := "")
      return Hr_Item;
   --  Create a new horizontal rule

   overriding procedure Draw
     (Self    : not null access Hr_Item_Record;
      Context : Draw_Context);
   overriding procedure Destroy (Self : not null access Hr_Item_Record);
   overriding procedure Size_Request
     (Self    : not null access Hr_Item_Record;
      Context : Draw_Context);

   ------------------
   -- Canvas links --
   ------------------

   type Canvas_Link_Record is new Abstract_Item_Record with private;
   type Canvas_Link is access all Canvas_Link_Record'Class;
   --  Special support is provided for links.
   --  These are a special kind of item, which provides automatic routing
   --  algorithms. They always join two items (including possibly two lines)

   function Gtk_New
     (From, To    : not null access Abstract_Item_Record'Class;
      Style       : Gtkada.Style.Drawing_Style;
      Routing     : Route_Style := Straight;
      Label       : access Container_Item_Record'Class := null;
      Anchor_From : Anchor_Attachment := Middle_Attachment;
      Label_From  : access Container_Item_Record'Class := null;
      Anchor_To   : Anchor_Attachment := Middle_Attachment;
      Label_To    : access Container_Item_Record'Class := null)
     return Canvas_Link;
   procedure Initialize
     (Link        : not null access Canvas_Link_Record'Class;
      From, To    : not null access Abstract_Item_Record'Class;
      Style       : Gtkada.Style.Drawing_Style;
      Routing     : Route_Style := Straight;
      Label       : access Container_Item_Record'Class := null;
      Anchor_From : Anchor_Attachment := Middle_Attachment;
      Label_From  : access Container_Item_Record'Class := null;
      Anchor_To   : Anchor_Attachment := Middle_Attachment;
      Label_To    : access Container_Item_Record'Class := null);
   --  Create a new link between the two items.
   --  This link is not automatically added to the model.
   --  Both items must belong to the same model.
   --
   --  The label is displayed approximately in the middle of the link.
   --  The Label_From is displayed next to the origin of the link, whereas
   --  Label_To is displayed next to the target of the link.
   --  These labels will generally be some Text_Item, but it might make sense
   --  to use more complex labels, for instance to draw something with a
   --  polyline item, or using an image.
   --
   --  If the Label is directed, the direction of the arrow will be changed
   --  automatically to match the layout of the link.

   procedure Set_Offset
     (Self    : not null access Canvas_Link_Record;
      Offset  : Gdouble);
   --  This only applies to arc links, and is used to specify the curve of the
   --  arc (this is basically the maximal distance between the straight line
   --  and the summit of the arc).
   --  Offset must not be 0.0

   procedure Refresh_Layout
     (Self    : not null access Canvas_Link_Record;
      Context : Draw_Context);
   --  Recompute the layout/routing for the link.
   --  This procedure should be called whenever any of the end objects changes
   --  side or position. The view will do this automatically the first time,
   --  but will not update links later on.

   procedure Set_Waypoints
     (Self     : not null access Canvas_Link_Record;
      Points   : Item_Point_Array;
      Relative : Boolean := False);
   --  Set explicit waypoints for the link, which forces the link to go through
   --  the given points.
   --  Relative should be true if all

   function Get_Style
     (Self : not null access Canvas_Link_Record) return Drawing_Style;
   --  Return the style used for the drawingo of this link

   function Get_Points
     (Self : not null access Canvas_Link_Record)
      return Item_Point_Array_Access;
   --  Return the computed points for the link.
   --  Do not free or store the result

   overriding procedure Destroy
     (Self : not null access Canvas_Link_Record);
   overriding function Bounding_Box
     (Self : not null access Canvas_Link_Record)
      return Item_Rectangle;
   overriding function Position
     (Self : not null access Canvas_Link_Record)
      return Gtkada.Style.Point;
   overriding procedure Draw
     (Self    : not null access Canvas_Link_Record;
      Context : Draw_Context);
   overriding function Contains
     (Self    : not null access Canvas_Link_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean;
   overriding function Clip_Line
     (Self   : not null access Canvas_Link_Record;
      P1, P2 : Item_Point) return Item_Point;
   overriding function Link_Anchor_Point
     (Self   : not null access Canvas_Link_Record;
      Anchor : Anchor_Attachment)
      return Item_Point;

private
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Gtkada.Style.Point_Array, Gtkada.Style.Point_Array_Access);

   type Canvas_Model_Record is abstract new Glib.Object.GObject_Record
   with record
      Layout : Pango.Layout.Pango_Layout;
   end record;

   type Canvas_Item_Record is abstract new Abstract_Item_Record with record
      Position : Gtkada.Style.Point :=
        (Gdouble'First, Gdouble'First);
      --  Position within its parent or the canvas view.
   end record;

   type Container_Item_Record is abstract new Canvas_Item_Record with record
      Width, Height : Model_Coordinate;
      --  Computed by Size_Request

      Computed_Position : Gtkada.Style.Point := (Gdouble'First, Gdouble'First);
      --  The position within the parent, as computed in Size_Allocate.
      --  The field Position is used for the position specifically requested by
      --  the user.

      Forced_Width, Forced_Height : Model_Coordinate := -1.0;
      --  Whether the user requested a specific size

      Margin : Margins := No_Margins;
      --  Margins around the child

      Parent : Container_Item;
      --  The parent item

      Min_Width, Min_Height : Gdouble := 1.0;

      Layout   : Child_Layout_Strategy := Vertical_Stack;
      Align    : Alignment_Style := Align_Start;
      Float    : Boolean := False;
      Overflow : Overflow_Style := Overflow_Prevent;

      Style    : Gtkada.Style.Drawing_Style;

      Children : Items_Lists.List;
   end record;

   type Rect_Item_Record is new Container_Item_Record with record
      Radius   : Model_Coordinate;
   end record;

   type Polyline_Item_Record is new Container_Item_Record with record
      Points   : Item_Point_Array_Access;
      Close    : Boolean;
      Relative : Boolean;
   end record;

   type Ellipse_Item_Record is new Container_Item_Record with null record;

   type Text_Item_Record is new Container_Item_Record with record
      Text     : GNAT.Strings.String_Access;
      Directed : Text_Arrow_Direction;
      Requested_Width, Requested_Height : Model_Coordinate;
   end record;

   type Hr_Item_Record is new Container_Item_Record with record
      Text     : GNAT.Strings.String_Access;
      Requested_Width, Requested_Height : Model_Coordinate;

      Space    : Model_Coordinate := 4.0;
      --  Space between text and lines
   end record;

   No_Waypoints : constant Item_Point_Array := (1 .. 0 => (0.0, 0.0));

   type Item_And_Position is record
      Item : Abstract_Item;
      Pos  : Model_Point;
   end record;
   package Item_And_Position_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Item_And_Position);

   type Canvas_View_Record is new Gtk.Widget.Gtk_Widget_Record with record
      Model   : Canvas_Model;
      Topleft : Model_Point := (0.0, 0.0);
      Scale   : Gdouble := 1.0;

      Id_Layout_Changed,
      Id_Item_Contents_Changed : Gtk.Handlers.Handler_Id;
      --  Connections to model signals

      Layout     : Pango.Layout.Pango_Layout;
      Hadj, Vadj : Gtk.Adjustment.Gtk_Adjustment;

      Scale_To_Fit_Requested : Gdouble := 0.0;
      --  Set to true when the user calls Scale_To_Fit before the view has had
      --  a size allocated (and thus we could not perform computation).
      --  This is set to the maximal zoom requested (or 0.0 if not requested)

      Last_Button_Press : Canvas_Event_Details;
      --  Attributes of the last button_press event, used to properly handle
      --  dragging and avoid recomputing the selectd item on button_release.

      Items : Item_And_Position_Lists.List;
      --  The items that are being dragged.

      In_Drag : Boolean := False;
      --  Whether we are in the middle of a drag.

      Topleft_At_Drag_Start : Model_Point;
      --  Toplevel at the stat of the drag
   end record;

   type Canvas_Link_Record is new Abstract_Item_Record with record
      From, To     : Abstract_Item;
      Style        : Gtkada.Style.Drawing_Style;
      Routing      : Route_Style;
      Bounding_Box : Item_Rectangle;
      Label        : Container_Item;
      Label_From   : Container_Item;
      Label_To     : Container_Item;

      Offset : Gdouble := 10.0;
      --  For arc links

      Waypoints   : Item_Point_Array_Access;
      --  The waypoints created by the user (as opposed to Points, which
      --  contains the list of waypoints computed automatically, in addition
      --  to the user's waypoints).
      --  These are absolute coordinates.
      --  For straight and orthogonal links, these are the points the link must
      --  go through.
      --  For curve and arc links, these are the list of points and
      --  control points for the bezier curve:
      --      pt1, ctrl1, ctrl2, pt2, ctrl3, ctrl4, pt3, ...

      Relative_Waypoints : Boolean := False;
      --  Whether the waypoints are given in relative coordinates

      Points   : Item_Point_Array_Access;
      --  The cached computation of waypoints for this link.
      --  These are recomputed every time the layout of the canvas changes, but
      --  are cached so that redrawing the canvas is fast.
      --  These are absolute coordinates.
      --  See the documentation on Waypoints for more information on the format
      --  These points are relative if Relative_Waypoints are relative.

      Anchor_From : Anchor_Attachment := Middle_Attachment;
      Anchor_To   : Anchor_Attachment := Middle_Attachment;
   end record;

   type List_Canvas_Model_Record is new Canvas_Model_Record with record
      Items : Items_Lists.List;
   end record;

end Gtkada.Canvas_View;
