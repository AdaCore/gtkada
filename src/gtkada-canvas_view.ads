------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2022, AdaCore                     --
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
--  Snapping also takes into account all four edges of items, not just their
--  topleft corner.
--
--  User interaction
--  ================
--
--  By default, limited user interaction is supported:
--     * When a view is added to a Gtk_Scrolled_Window, scrolling is
--       automatically supported (it is handled by the scrolled window).
--       Users can use the mouse wheel to scroll vertically, shift and the
--       mouse wheel to scroll horizontally, or use the touchpad to navigate
--       (in general with multiple fingers).
--
--  But of course it supports much more advanced interactions, like clicking
--  on items, moving them with the mouse or keyboard,...
--
--  For this, you need to connect to the "item_event" signal, and either
--  directly handle the signal (a simple click for instance), or set some
--  data in the details parameters, to enable dragging items or the background
--  of the canvas (for scrolling). The package Gtkada.Canvas_View.Views
--  provides a number of precoded behaviors.
--
--  When dragging items, the view will scroll automatically if the mouse is
--  going outside of the visible area. Scrolling will continue while the mouse
--  stays there, even if the user does not move the mouse.
--
--  The following has not been backported yet:
--  ==========================================
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
private with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Numerics.Generic_Elementary_Functions; use Ada.Numerics;
private with Ada.Unchecked_Deallocation;
private with GNAT.Strings;
with Cairo;
with Gdk.Event;        use Gdk.Event;
with Gdk.Pixbuf;       use Gdk.Pixbuf;
with Gdk.Types;        use Gdk.Types;
private with Glib.Main;
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Handlers;
with Gtk.Bin;          use Gtk.Bin;
with Gtk.Widget;
with Gtkada.Style;     use Gtkada.Style;
with Pango.Layout;     use Pango.Layout;

package Gtkada.Canvas_View is

   package Gdouble_Elementary_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Gdouble);

   type Canvas_View_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Canvas_View is access all Canvas_View_Record'Class;
   --  A view is a display of one particular part of the model, or a subset of
   --  it. Multiple views can be associated with a specific model, and will
   --  monitor changes to it view signals.
   --  The view automatically refreshes its display when its model changes.

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

   No_Rectangle  : constant Model_Rectangle := (0.0, 0.0, 0.0, 0.0);
   No_Point      : constant Model_Point := (Gdouble'First, Gdouble'First);
   No_Item_Point : constant Item_Point := (Gdouble'First, Gdouble'First);

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
      Toplevel_Side : Side_Attachment := Auto;
      Distance      : Model_Coordinate := 0.0;
   end record;
   Middle_Attachment : constant Anchor_Attachment := (0.5, 0.5, Auto, 0.0);
   --  Where in the item the link is attached (0.5 means the middle, 0.0
   --  means left or top, and 1.0 means right or bottom).
   --
   --  For the target side of a link, if X or Y are negative, Gtkada will try
   --  to draw a strictly orthogonal or vertical segment next on that end by
   --  adjusting the location of the end point along the border of the item. If
   --  it cannot, then GtkAda will use the absolute value of X and Y to specify
   --  the attachment.
   --
   --  You can therefore force a link to always emerge from the right side of
   --  an item by setting X to 1.0 and Y to any value, for instance.
   --  See the description of Side_Attachment for an example on how to use
   --  Toplevel_Side.
   --  Distance indicates at which distance from the border of the item the
   --  link should stop. By default, it reaches the border.

   type Route_Style is (Orthogonal, Straight, Arc, Curve);
   --  This defines how a link is routed between its two ends.
   --  Curve is similar to orthogonal (links restricted to horizontal and
   --  vertical lines), but using a bezier curve.

   ------------------
   -- Draw context --
   ------------------

   type Draw_Context is record
      Cr     : Cairo.Cairo_Context := Cairo.Null_Context;
      Layout : Pango.Layout.Pango_Layout := null;
      View   : Canvas_View := null;
   end record;
   --  Context to perform the actual drawing

   function Build_Context
     (Self : not null access Canvas_View_Record'Class)
      return Draw_Context;
   --  Returns a draw context for the view. This context is suitable for
   --  computing sizes (in Refresh_Layout), but not for actual drawing.

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

   function Is_Link
     (Self : not null access Abstract_Item_Record) return Boolean is abstract;
   --  Whether this item should be considered as a link between two other
   --  items.
   --  Such links have a few specific behavior: for instance, they cannot be
   --  dragged by the user to a new position (their layout is provided by the
   --  items they are linked to).
   --  They also do not contribute to the smart guides that are used while
   --  items are moved around.

   No_Position : constant Gtkada.Style.Point := (Gdouble'First, Gdouble'First);
   --  Indicates that the item did not get assigned a proper position

   function Position
     (Self : not null access Abstract_Item_Record)
      return Gtkada.Style.Point is abstract;
   --  The coordinates of the item within its parent.
   --  If the item has no parent, the coordinates should be returned in model
   --  coordinates. These coordinates describe the origin (0,0) point of
   --  the item's coordinate system (even if Set_Position was specified to
   --  point to another location in the item).

   procedure Set_Position
     (Self     : not null access Abstract_Item_Record;
      Pos      : Gtkada.Style.Point) is null;
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

   procedure Refresh_Layout
     (Self    : not null access Abstract_Item_Record;
      Context : Draw_Context) is null;
   --  Called when Refresh_Layout is called on the model.
   --  This is an opportunity for the item to update its size for instance, or
   --  do other computation that might impact the result of Bounding_Box.

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
     (Self          : not null access Abstract_Item_Record'Class;
      Context       : Draw_Context;
      As_Outline    : Boolean := False;
      Outline_Style : Drawing_Style := No_Drawing_Style);
   --  Translate the transformation matrix and draw the item.
   --  This procedure should be used instead of calling Draw directly.
   --  If As_Outline is true, then only the outline of the item is displayed,
   --  using the provided style

   procedure Draw_Outline
     (Self    : not null access Abstract_Item_Record;
      Style   : Gtkada.Style.Drawing_Style;
      Context : Draw_Context) is null;
   --  Draw an outline for Self (which is used for the selection for instance).
   --  Do not call this procedure directly, use Translate_And_Draw_Item
   --  instead, unless called directly from an overriding of Draw.

   procedure Draw_As_Selected
     (Self    : not null access Abstract_Item_Record;
      Context : Draw_Context) is abstract;
   --  Draw the item when it is selected.
   --  The default is to draw both the item and its outline.
   --  Do not call this procedure directly, use Translate_And_Draw_Item
   --  instead, unless called directly from an overriding of Draw.

   function Contains
     (Self    : not null access Abstract_Item_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean is abstract;
   --  Should test whether Point is within the painted region for Self (i.e.
   --  whether Self should be selected when the user clicks on the point).
   --  For an item with holes, this function should return False when the
   --  point is inside one of the holes, for instance.

   function Edit_Widget
     (Self  : not null access Abstract_Item_Record;
      View  : not null access Canvas_View_Record'Class)
      return Gtk.Widget.Gtk_Widget is abstract;
   --  Return the widget to use for in-place editing of the item.
   --  null should be returned when the item is not editable in place.
   --  It is the responsibility of the returned widget to monitor events and
   --  validate the editing, update Self, and then call model's layout_changed
   --  signal.

   procedure Destroy
     (Self     : not null access Abstract_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class) is null;
   --  Called when Self is no longer needed.
   --  Do not call directly.

   function Parent
     (Self : not null access Abstract_Item_Record)
      return Abstract_Item is abstract;
   --  Return the item inside which Self is contained.
   --  null is returned for toplevel items, in which case the coordinates of
   --  the bounding box are model coordinats. Otherwise, the coordinates are
   --  relative to the returned item.

   function Get_Toplevel_Item
     (Self : not null access Abstract_Item_Record'Class)
      return Abstract_Item;
   --  Return the toplevel item that contains Self (or self itself)

   function Inner_Most_Item
     (Self     : not null access Abstract_Item_Record;
      At_Point : Model_Point;
      Context  : Draw_Context)
      return Abstract_Item is abstract;
   --  Return the inner-most item at the specific coordinates in Self (or
   --  Self itself).

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

   function Is_Invisible
     (Self : not null access Abstract_Item_Record)
     return Boolean is abstract;
   --  True if Self has no filling or stroke information (and therefore is
   --  invisible even when displayed, although some of its children might be
   --  visible).
   --  This function is independent of Set_Visibility_Threshold, Show or Hide.

   procedure Set_Visibility_Threshold
     (Self      : not null access Abstract_Item_Record;
      Threshold : Gdouble) is null;
   function Get_Visibility_Threshold
     (Self : not null access Abstract_Item_Record) return Gdouble is abstract;
   --  When the items bounding box (on the screen) width or height are less
   --  than Threshold pixels, the item is automatically hidden.
   --  Making the item invisibile does not impact the visibility of links from
   --  or to that item (but you could use Include_Related_Items to find these
   --  related items.
   --  You need to refresh the view afterwards

   procedure Show (Self : not null access Abstract_Item_Record'Class);
   procedure Hide (Self : not null access Abstract_Item_Record'Class);
   --  Hide or show the item unconditionally. This overrides the settings
   --  done by Set_Visibility_Threshold.

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

   overriding function Is_Link
     (Self : not null access Canvas_Item_Record) return Boolean is (False);
   overriding function Parent
     (Self : not null access Canvas_Item_Record)
      return Abstract_Item is (null);
   overriding function Is_Invisible
     (Self : not null access Canvas_Item_Record)
     return Boolean is (False);
   function Inner_Most_Item
     (Self           : not null access Canvas_Item_Record;
      Dummy_At_Point : Model_Point;
      Dummy_Context  : Draw_Context)
      return Abstract_Item is (Self);
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
   overriding function Edit_Widget
     (Self  : not null access Canvas_Item_Record;
      View  : not null access Canvas_View_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   overriding procedure Draw_As_Selected
     (Self    : not null access Canvas_Item_Record;
      Context : Draw_Context);
   overriding procedure Draw_Outline
     (Self    : not null access Canvas_Item_Record;
      Style   : Gtkada.Style.Drawing_Style;
      Context : Draw_Context);
   overriding procedure Set_Visibility_Threshold
     (Self      : not null access Canvas_Item_Record;
      Threshold : Gdouble);
   overriding function Get_Visibility_Threshold
     (Self : not null access Canvas_Item_Record) return Gdouble;

   overriding procedure Set_Position
     (Self     : not null access Canvas_Item_Record;
      Pos      : Gtkada.Style.Point);
   --  Sets the position of the item within its parent (or within the canvas
   --  view if Self has no parent).

   ------------------
   -- Canvas_Model --
   ------------------

   function Model_Get_Type return Glib.GType;
   pragma Convention (C, Model_Get_Type);
   --  Return the internal type

   procedure Initialize
     (Self : not null access Canvas_Model_Record'Class);
   --  Initialize the internal data so that signals can be sent.
   --  This procedure must always be called when you create a new model.

   type Item_Kind_Filter is (Kind_Item, Kind_Link, Kind_Any);
   procedure For_Each_Item
     (Self     : not null access Canvas_Model_Record;
      Callback : not null access procedure
        (Item : not null access Abstract_Item_Record'Class);
      Selected_Only : Boolean := False;
      Filter        : Item_Kind_Filter := Kind_Any;
      In_Area       : Model_Rectangle := No_Rectangle) is abstract;
   --  Calls Callback for each item in the model, including links.
   --  Only the items that intersect In_Area should be returned for
   --  efficiency, although it is valid to return all items.
   --
   --  If Selected_Only is true, then only selected items are returned
   --
   --  Items are returned in z-layer order: lowest items first, highest items
   --  last.
   --
   --  You should not remove items while iterating, since removing items might
   --  end up removing other items (links to or from the original item for
   --  instance). Instead, create a temporary structure via
   --  Include_Related_Items and use Remove to remove them all at once.

   function Hash (Key : Abstract_Item) return Ada.Containers.Hash_Type;
   package Item_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Abstract_Item,
      Hash                => Hash,
      Equivalent_Elements => "=",
      "="                 => "=");

   function Get_Selected_Items
     (Self : not null access Canvas_Model_Record) return Item_Sets.Set;
   --  Return the currently selected items. If no item is selected, an empty
   --  set is returned.

   procedure For_Each_Link
     (Self       : not null access Canvas_Model_Record;
      Callback   : not null access procedure
        (Item : not null access Abstract_Item_Record'Class);
      From_Or_To : Item_Sets.Set);
   --  This iterator should return all the links in the model.
   --  If possible, it should restrict itself to the links with at least one
   --  end on an item in From_Or_To (or on a link to such an item).
   --  This function is important for performance when dragging items in a
   --  large model (tens of thousands of items). The default implementation
   --  simply calls For_Each_Item.
   --  From_Or_To is never empty.

   procedure Include_Related_Items
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class;
      Set  : in out Item_Sets.Set);
   --  Append Item and all items and links related to Item (i.e. the links for
   --  which one of the ends is Item, and then the links to these links, and so
   --  on).

   procedure From
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class;
      Set  : in out Item_Sets.Set);
   --  Append all the items with a link coming from Item

   procedure To
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class;
      Set  : in out Item_Sets.Set);
   --  Append all the items with a link going to Item

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

   procedure Refresh_Layout
     (Self        : not null access Canvas_Model_Record;
      Send_Signal : Boolean := True);
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
   --  This procedure will in general send a Layout_Changed signal if
   --  Send_Signal is true. This should in general always be left to True
   --  unless you are writting your own model.
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

   procedure Remove
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class) is null;
   --  Remove an item from the model, and destroy it.
   --  This also removes all links to and from the element, and links to
   --  these links (and so on).

   procedure Remove
     (Self : not null access Canvas_Model_Record;
      Set  : Item_Sets.Set);
   --  Remove all elements in the set from the model.
   --  It is expected that the set already contains related items (see
   --  Include_Related_Items)
   --  The default implementation is to call Remove for each of the element in
   --  the set, so you will need to override this procedure if your
   --  implementation of Remove calls this one.

   procedure Raise_Item
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class) is abstract;
   procedure Lower_Item
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class) is abstract;
   --  Change the z-order of the item.
   --  This emits the layout_changed signal

   type Selection_Mode is
     (Selection_None, Selection_Single, Selection_Multiple);
   procedure Set_Selection_Mode
     (Self : not null access Canvas_Model_Record;
      Mode : Selection_Mode);
   --  Controls whether items can be selected.
   --  Changing the mode always clears the selection.

   procedure Clear_Selection (Self : not null access Canvas_Model_Record);
   procedure Add_To_Selection
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class);
   procedure Remove_From_Selection
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class);
   function Is_Selected
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class)
      return Boolean;
   --  Handling of selection. Depending on the selection mode, some of these
   --  operations might have no effect, or might unselect the current selection
   --  before selecting a new item.
   --  The selection might contain child items (i.e. not just toplevel items).
   --
   --  Whenever the selection is changed, the signal "selection_changed" is
   --  emitted.

   function Is_Selectable
     (Self       : not null access Canvas_Model_Record;
      Dummy_Item : not null access Abstract_Item_Record'Class)
      return Boolean is (True);
   --  Whether the given item is selectable. By default, all items are
   --  selectable.

   procedure Selection_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Item : access Abstract_Item_Record'Class := null);
   function On_Selection_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : not null access GObject_Record'Class;
         Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id;
   Signal_Selection_Changed : constant Glib.Signal_Name := "selection_changed";
   --  Item is set to null when the selection was cleared, otherwise it is
   --  set to the element that was just added or removed from the selection.

   procedure Layout_Changed (Self : not null access Canvas_Model_Record'Class);
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

   function On_Item_Destroyed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class;
         Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id;
   Signal_Item_Destroyed : constant Glib.Signal_Name := "item_destroyed";
   --  This signal is emitted just before an item is destroyed.

   ----------------
   -- List Model --
   ----------------

   type List_Canvas_Model_Record is new Canvas_Model_Record with private;
   type List_Canvas_Model is access all List_Canvas_Model_Record'Class;
   --  A very simple-minded concrete implementation for a model.
   --  This model is suitable for most cases where only a few thousands items
   --  are displayed. If you have tens of thousands, you should consider
   --  wrapping this model with a Gtkada.Canvas_View.Models.Rtree_Model to
   --  speed things up.

   procedure Gtk_New (Self : out List_Canvas_Model);
   --  Create a new model

   procedure Add
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class);
   --  Add a new item to the model.

   procedure Clear
     (Self : not null access List_Canvas_Model_Record);
   --  Remove all items from the model, and destroy them.

   overriding procedure Remove
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class);
   overriding procedure Remove
     (Self : not null access List_Canvas_Model_Record;
      Set  : Item_Sets.Set);
   overriding procedure For_Each_Item
     (Self     : not null access List_Canvas_Model_Record;
      Callback : not null access procedure
        (Item : not null access Abstract_Item_Record'Class);
      Selected_Only : Boolean := False;
      Filter        : Item_Kind_Filter := Kind_Any;
      In_Area       : Model_Rectangle := No_Rectangle);
   overriding procedure Raise_Item
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class);
   overriding procedure Lower_Item
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class);
   overriding function Toplevel_Item_At
     (Self    : not null access List_Canvas_Model_Record;
      Point   : Model_Point;
      Context : Draw_Context) return Abstract_Item;

   -----------------
   -- Canvas_View --
   -----------------

   View_Margin : constant View_Coordinate := 20.0;
   --  The number of blank pixels on each sides of the view. This avoids having
   --  items displays exactly next to the border of the view.

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

   procedure Set_Grid_Size
     (Self : not null access Canvas_View_Record'Class;
      Size : Model_Coordinate := 30.0);
   --  Set the size of the grid.
   --  This grid is not visible by default. To display it, you should override
   --  Draw_Internal and call one of the functions in Gtkada.Canvas_View.Views.
   --
   --  This grid is also size for snapping of items while they are moved: when
   --  they are dragged to a position close to one of the grid lines, they will
   --  be moved by a small extra amount to align on this grid line.

   Default_Guide_Style : constant Gtkada.Style.Drawing_Style :=
     Gtkada.Style.Gtk_New (Stroke => (0.957, 0.363, 0.913, 1.0));

   procedure Set_Snap
     (Self           : not null access Canvas_View_Record'Class;
      Snap_To_Grid   : Boolean := True;
      Snap_To_Guides : Boolean := False;
      Snap_Margin    : Model_Coordinate := 5.0;
      Guides_Style   : Gtkada.Style.Drawing_Style := Default_Guide_Style);
   --  Configure the snapping feature.
   --  When items are moved interactively, they will tend to snap to various
   --  coordinates, as defined for instance by Set_Grid_Size.
   --  For instance, when any size of the item gets close to one of the grid
   --  lines (i.e. less than Snap_Margin), it will be moved an extra small
   --  amount so that the coordinate of that size of the item is exactly that
   --  of the grid line. This results in nicer alignment of the items.
   --
   --  No snapping to grid occurs if the grid size is set to 0.

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

   procedure Set_Selection_Style
     (Self  : not null access Canvas_View_Record;
      Style : Gtkada.Style.Drawing_Style);
   function Get_Selection_Style
     (Self  : not null access Canvas_View_Record)
      return Gtkada.Style.Drawing_Style;
   --  The style used to highlight selected items

   procedure Set_Scale
     (Self     : not null access Canvas_View_Record;
      Scale    : Gdouble := 1.0;
      Preserve : Model_Point := No_Point);
   --  Changes the scaling factor for Self.
   --  This also scrolls the view so that either Preserve or the current center
   --  of the view remains at the same location in the widget, as if the user
   --  was zooming towards that specific point.
   --  See also Gtkada.Canvas_View.Views.Animate_Scale for a way to do this
   --  change via an animation.

   procedure Set_Topleft
     (Self         : not null access Canvas_View_Record;
      Topleft      : Model_Point);
   --  Set a specific position for the topleft corner of the visible area.
   --  This function is mostly useful to restore previous settings (which you
   --  can get through Get_Visible_Area). Interactively, it is likely better
   --  to call one of Center_On, Scroll_Into_View or Scale_To_Fit.

   procedure Center_On
     (Self         : not null access Canvas_View_Record;
      Center_On    : Model_Point;
      X_Pos, Y_Pos : Gdouble := 0.5;
      Duration     : Standard.Duration := 0.0);
   --  Scroll the canvas so that Center_On appears at the given position
   --  within the view (center when using 0.5, or left when using 0.0, and so
   --  on).
   --  If the duration is not 0, animation is used.

   procedure Scroll_Into_View
     (Self     : not null access Canvas_View_Record;
      Item     : not null access Abstract_Item_Record'Class;
      Duration : Standard.Duration := 0.0);
   procedure Scroll_Into_View
     (Self     : not null access Canvas_View_Record;
      Rect     : Model_Rectangle;
      Duration : Standard.Duration := 0.0);
   --  Do the minimal amount of scrolling to make the item or rectangle
   --  visible. If the duration is not 0, animation is used.

   function Get_Scale
     (Self : not null access Canvas_View_Record) return Gdouble;
   --  Return the current scale

   procedure Scale_To_Fit
     (Self      : not null access Canvas_View_Record;
      Rect      : Model_Rectangle := No_Rectangle;
      Min_Scale : Gdouble := 1.0 / 4.0;
      Max_Scale : Gdouble := 4.0;
      Duration  : Standard.Duration := 0.0);
   --  Chose the scale and scroll position so that the whole model (or the
   --  specified rectangle) is visible.
   --  This procedure leaves a small margin on each sides of the model, since
   --  that looks nicer.
   --  This function can be called even before Self has got a size assigned by
   --  window manager, but the computation of the scale will be delayed until
   --  an actual size is known.
   --  If a duration is specified, the scaling and scrolling will be animated

   procedure Avoid_Overlap
     (Self     : not null access Canvas_View_Record'Class;
      Avoid    : Boolean;
      Duration : Standard.Duration := 0.2);
   --  Sets whether items should avoid overlap when possible.
   --  When the user is moving items interactively and dropping them in a new
   --  position, items that would be overlapped are moved aside to make space
   --  for the new item.
   --  If Duration is not 0, the other items are animated to the new position.
   --
   --  This setting has no effect when you set the position of items
   --  explicitly via a call to Set_Position. In such cases, you can force
   --  the behavior manually by calling Gtkada.Canvas_View.Views.Reserve_Space.

   type Page_Format is record
      Width_In_Inches, Height_In_Inches : Gdouble;
   end record;

   type Predefined_Page_Format_Type is
     (A3_Portrait,
      A3_Landscape,
      A4_Portrait,
      A4_Landscape,
      Letter_Portrait,
      Letter_Landscape);

   function To_Page_Format
     (Value : Predefined_Page_Format_Type) return Page_Format
   is
     (case Value is
         when A3_Portrait => (11.7, 16.5),
         when A3_Landscape => (16.5, 11.7),
         when A4_Portrait => (8.3, 11.7),
         when A4_Landscape => (11.7, 8.3),
         when Letter_Portrait => (8.5, 11.0),
         when Letter_Landscape => (11.0, 8.5));

   type Export_Format is (Export_PDF, Export_SVG, Export_PNG);

   function Export
     (Self              : not null access Canvas_View_Record;
      Filename          : String;
      Page              : Page_Format;
      Format            : Export_Format := Export_PDF;
      Visible_Area_Only : Boolean := True)
     return Boolean;
   --  Create a file with the contents of the view (or the whole model
   --  if Visible_Area_Only is False).
   --  True is returned if the file was created successfully, False otherwise

   No_Drag_Allowed : constant Model_Rectangle := (0.0, 0.0, 0.0, 0.0);
   Drag_Anywhere   : constant Model_Rectangle :=
     (Gdouble'First, Gdouble'First, Gdouble'Last, Gdouble'Last);
   --  Values for the Event_Details.Allowed_Drag_Area field

   type Canvas_Event_Type is
     (Button_Press, Button_Release, Double_Click,
      Start_Drag, In_Drag, End_Drag, Key_Press, Scroll, Custom);
   --  The event types that are emitted for the Item_Event signal:
   --  * Button_Press is called when the user presses any mouse buttton either
   --    on an item or in the background.
   --    This event can also be used to start a drag event (by
   --    setting the Allowed_Drag_Area field of the Canvas_Event_Details).
   --    It can be used also to display contextual menus.
   --
   --  * Double_Click is used when the left mouse button is pressed twice in
   --    rapid succession (note that Button_Press is also emitted for the first
   --    click).
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
   --
   --  * Scroll is used when the user uses the mouse wheel. It is not possible
   --    to start a drag from this event.
   --    In the Canvas_Event_Details, the button is set to either 5 or 6,
   --    depending on the direction of the scrolling.
   --
   --  * Custom is used when generating a custom event from the code.

   type Canvas_Event_Details is record
      Event_Type     : Canvas_Event_Type;
      Button         : Guint;

      State          : Gdk.Types.Gdk_Modifier_Type;
      --  The modifier keys (shift, alt, control). It can be used to activate
      --  different behavior in such cases.

      Key            : Gdk.Types.Gdk_Key_Type;
      --  The key that was pressed (for key events)

      Root_Point     : Gtkada.Style.Point;
      --  Coordinates in root window.
      --  Attributes of the low-level event.
      --   This is an implementation detail for proper handling of dragging.

      M_Point        : Model_Point;
      --  Where in the model the user clicked. This is independent of the zoom
      --  level or current scrolling.

      Item           : Abstract_Item;
      --  The actual item that was clicked.
      --  Set to null when the user clicked in the background.

      Toplevel_Item  : Abstract_Item;
      --  The toplevel item that contains Item (might be Item itself).
      --  Set to null when the user clicked in the background.

      T_Point        : Item_Point;
      --  The corodinates of the click in toplevel_item

      I_Point        : Item_Point;
      --  The coordinates of the click in Item

      Allowed_Drag_Area : Model_Rectangle := No_Drag_Allowed;
      --  Allowed_Drag_Area should be modified by the callback when the event
      --  is a button_press event. It should be set to the area within which
      --  the item (and all currently selected items) can be moved. If you
      --  leave it to No_Drag_Allowed, the item cannot be moved.
      --
      --  This field is ignored for events other than button_press, since it
      --  makes no sense for instance to start a drag on a button release.

      Allow_Snapping    : Boolean := True;
      --  If set to False, this temporary overrides the settings from
      --  Set_Snap, and prevents any snapping on the grid or smart guides.
      --  It should be set at the same time that Allowed_Drag_Area is set.
   end record;
   type Event_Details_Access is not null access all Canvas_Event_Details;
   --  This record describes high-level aspects of user interaction with the
   --  canvas.

   Null_Canvas_Event_Details : constant Canvas_Event_Details :=
     Canvas_Event_Details'
       (Event_Type        => Custom,
        Button            => 0,
        State             => 0,
        Key               => 0,
        Root_Point        => (0.0, 0.0),
        M_Point           => (0.0, 0.0),
        Item              => null,
        Toplevel_Item     => null,
        T_Point           => (0.0, 0.0),
        I_Point           => (0.0, 0.0),
        Allowed_Drag_Area => (0.0, 0.0, 0.0, 0.0),
        Allow_Snapping    => False);

   procedure Initialize_Details
     (Self    : not null access Canvas_View_Record'Class;
      Details : out Canvas_Event_Details);
   --  Initialize Details for a Custom event type.
   --  When you have a real Gtk event, better to use Set_Details below.

   procedure Set_Details
     (Self    : not null access Canvas_View_Record'Class;
      Details : out Canvas_Event_Details;
      Event   : Gdk.Event.Gdk_Event_Button);
   --  Set the details from a specific gtk+ event

   procedure Viewport_Changed
     (Self   : not null access Canvas_View_Record'Class);
   function On_Viewport_Changed
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access procedure
        (Self : not null access GObject_Record'Class);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id;
   Signal_Viewport_Changed : constant Glib.Signal_Name := "viewport_changed";
   --  This signal is emitted whenever the view is zoomed or scrolled.
   --  This can be used for instance to synchronize multiple views, or display
   --  a "mini-map" of the whole view.

   function Item_Event
     (Self    : not null access Canvas_View_Record'Class;
      Details : Event_Details_Access) return Boolean;
   procedure On_Item_Event
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access function
        (Self    : not null access GObject_Record'Class;
         Details : Event_Details_Access)
      return Boolean;
      Slot : access GObject_Record'Class := null);
   Signal_Item_Event : constant Glib.Signal_Name := "item_event";
   --  This signal is emitted whenever the user interacts with an item (button
   --  press or release, key events,...).
   --  It is recommended to connect to this signal rather than the lower-level
   --  Button_Press_Event, Button_Release_Event,... since most information is
   --  provided here in the form of the details parameter.
   --
   --  The callback should return True if the event was processed, or False if
   --  the default handling should be performed.
   --
   --  The package Gtkada.Canvas_View.Views contains a number of examples of
   --  compatible callbacks which enable behaviors such as a moving items,
   --  scrolling the canvas by dragging the background,...

   procedure Inline_Editing_Started
     (Self : not null access Canvas_View_Record'Class;
      Item : not null access Abstract_Item_Record'Class);
   function On_Inline_Editing_Started
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class; Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id;
   Signal_Inline_Editing_Started : constant Glib.Signal_Name :=
      "inline_editing_started";
   --  Called when the user starts inline editing of items.

   procedure Inline_Editing_Finished
     (Self : not null access Canvas_View_Record'Class;
      Item : not null access Abstract_Item_Record'Class);
   function On_Inline_Editing_Finished
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class; Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id;
   Signal_Inline_Editing_Finished : constant Glib.Signal_Name :=
      "inline_editing_finished";
   --  Called when the user finishes (cancels ot validates) inline
   --  editing of items.

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
   --  account horizontally; and similarly when the parent organizes its
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
   --  Alignment does not apply to floating children, nor to children with
   --  a specific position given along a specific axis (in which case the
   --  Anchor_X or Anchor_Y might be used for a slightly similar effect).

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
      Pack_End : Boolean := False;
      Margin   : Margins := No_Margins;
      Float    : Boolean := False;
      Overflow : Overflow_Style := Overflow_Prevent);
   --  Add a new child to the container.
   --  If the child's position is set, it is then interpreted as relative to
   --  Self. If the position is not specified, it will be computed
   --  automatically based on the container's policy (either below the previous
   --  child, or to its right).
   --
   --  When Pack_End is true, the child will be added at the end of the
   --  parent's area (right or bottom depending on orientation). If the
   --  parent's size is larger than that needed by all its children, there
   --  will thus be an empty space between children with Pack_End=>False and
   --  children with Pack_End => True.
   --
   --  When Pack_End is True, the children are put in reverse order starting
   --  from the end of Self: for a vertical layout, for instance, the first
   --  pack_end child will appear at the bottom of Self.
   --
   --  Margin are added to each size of the child. The child's width, as set
   --  via Set_Size, does not include the margins.
   --
   --  A floating child does not participate in the stacking: it will still be
   --  displayed below or to the right of the previous child, but the next
   --  item will then be displayed at the same coordinate as the floating
   --  child.

   procedure Clear
      (Self     : not null access Container_Item_Record;
       In_Model : not null access Canvas_Model_Record'Class);
   --  Remove all children of Self

   type Size_Unit is (Unit_Pixels, Unit_Percent, Unit_Auto, Unit_Fit);
   --  A size can be expressed either in actual screen pixels, or
   --  proportionnaly to the parent's size.
   --  When the unit is Unit_Auto, the size of the item is computed
   --  automatically based on its children or its own intrinsic needs
   --  (for a text, this is the size needed to display the text in the given
   --  font).
   --  When the unit is Unit_Fit: this sets the width of a child so that
   --  this width plus the child's margins take the full width of the parent
   --  container. Setting a width to 100% using Unit_Percent would not take
   --  the margins into account, so that the full size (margins+width) might
   --  actually be wider than the parent. When the parent layout is
   --  horizontal, the above description applies to the height of the child.
   --  In both cases, Unit_Fit is ignored for the other axis (height for
   --  a vertical layout), in which case the child's height will be that
   --  computed from the children.

   type Size (Unit : Size_Unit := Unit_Pixels) is record
      case Unit is
         when Unit_Auto | Unit_Fit =>
            null;
         when Unit_Pixels =>
            Length : Model_Coordinate;
         when Unit_Percent =>
            Value  : Percent;
      end case;
   end record;

   Auto_Size : constant Size := (Unit => Unit_Auto);
   Fit_Size : constant Size := (Unit => Unit_Fit);
   --  See the descriptions for Size_Unit.

   procedure Set_Width_Range
     (Self     : not null access Container_Item_Record;
      Min, Max : Size := Auto_Size);
   procedure Set_Height_Range
     (Self     : not null access Container_Item_Record;
      Min, Max : Size := Auto_Size);
   --  Specify a minimal and maximal size for the item, along each axis.
   --  The default is for items to occupy the full width of their parent
   --  (in vertical layout) or the full height (in horizontal layout),
   --  and the child required by their children for the other axis.
   --  Calling this procedure overrides any specific size set via
   --  Set_Size or one of the constructors for the items, like rectangles
   --  and ellipsis, for that axis.

   procedure Set_Size
      (Self : not null access Container_Item_Record;
       Width, Height : Size := Auto_Size);
   --  Force a specific size for the item if any of the dimensions is positive.
   --  When Auto_Size is given, the size along that axis will be computed
   --  automatically.
   --  Calling this procedure cancels effects from Set_Size_Range.
   --  The size of a container is influenced by its children as follows:
   --     * the preferred size for each child is computed, based on its own
   --       intrinsic needs (given size for rectangles, text size,...)
   --     * if the child has a min and max size given in pixels, these
   --       constraints are applied immediately.
   --     * the container will then use the maximal computed size amongst
   --       its children.
   --     * Once the size of the container is known, the size for its
   --       children is recomputed when the size or the size constraints
   --       were given as percent of the parent size. It means that sizees
   --       given in percent do not influence the parent's size computation.

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
   --  One example of overridding this procedure is when you are building an
   --  item which shoud align some text on two columns (for instance in a UML
   --  diagram we might want the field names and their types to each be on
   --  their own column. In this case, the container's Size_Request would
   --  first call the inherited version (so that each child requests a size),
   --  then iterate over the children in each column and compute the maximum
   --  requested width for that column. Finally, another pass for the children
   --  in each column to call Set_Size_Request and override their requested
   --  width.

   procedure Set_Size_Request
     (Self    : not null access Container_Item_Record;
      Width, Height : Gdouble := -1.0);
   --  This procedure should only be called from an override of Size_Request
   --  (but it can then be called for any item, not just the one passed in
   --  parameter).
   --  It can be used to request a specific size for an item, or override the
   --  size already computed. When Width or Height is negative, they do not
   --  override the existing size request.

   procedure Size_Allocate
     (Self  : not null access Container_Item_Record);
   --  Called once the size of the parent object has been decided (i.e. after
   --  all the calls to Size_Request).
   --  The parent must set its child's position and size, and then call
   --  Size_Allocate to let it know about the final size and position.
   --  This can be used to compute attributes that need the actual size of the
   --  item (gradients, centering or right-aligning objects,...)
   --  Alignments and margins are automatically handled by the parent.

   procedure For_Each_Child
     (Self     : not null access Container_Item_Record'Class;
      Callback : not null access procedure
        (Child : not null access Container_Item_Record'Class);
      Recursive : Boolean := False);
   --  Traverse all children of Self, and calls Callback for each.

   procedure Draw_Children
     (Self    : not null access Container_Item_Record'Class;
      Context : Draw_Context);
   --  Display all the children of Self

   procedure Set_Style
     (Self  : not null access Container_Item_Record;
      Style : Drawing_Style);
   function Get_Style
     (Self : not null access Container_Item_Record) return Drawing_Style;
   --  Return the style used for the drawingo of this item.
   --  When changing the style, you must force a refresh of the canvas.

   overriding procedure Refresh_Layout
     (Self    : not null access Container_Item_Record;
      Context : Draw_Context);
   overriding procedure Set_Position
     (Self     : not null access Container_Item_Record;
      Pos      : Gtkada.Style.Point);
   procedure Set_Position
     (Self     : not null access Container_Item_Record;
      Pos      : Gtkada.Style.Point := (Gdouble'First, Gdouble'First);
      Anchor_X : Percent;
      Anchor_Y : Percent);
   --  Anchor_X and Anchor_Y indicate which part of the item is at the given
   --  coordinates. For instance, (0, 0) indicates that Pos is the location of
   --  the top-left corner of the item, but (0.5, 0.5) indicates that Pos is
   --  the position of the center of the item.

   overriding procedure Destroy
     (Self     : not null access Container_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class);
   overriding function Position
     (Self : not null access Container_Item_Record) return Gtkada.Style.Point;
   overriding function Parent
     (Self : not null access Container_Item_Record)
      return Abstract_Item;
   overriding function Bounding_Box
     (Self : not null access Container_Item_Record)
      return Item_Rectangle;
   overriding function Inner_Most_Item
     (Self     : not null access Container_Item_Record;
      At_Point : Model_Point;
      Context  : Draw_Context) return Abstract_Item;
   overriding function Is_Invisible
     (Self : not null access Container_Item_Record)
      return Boolean;

   ----------------
   -- Rectangles --
   ----------------

   type Rect_Item_Record is new Container_Item_Record with private;
   type Rect_Item is access all Rect_Item_Record'Class;
   --  A predefined type object which displays itself as a rectangle or a
   --  rectangle with rounded corners.

   Fit_Size_As_Double  : constant Model_Coordinate := -1.0;
   Auto_Size_As_Double : constant Model_Coordinate := -2.0;
   --  See the description of Fit_Size and Auto_Size.
   --  These are used for parameters that take a Double instead of a Size
   --  for backward compatibility (consider using Set_Size instead).

   function Gtk_New_Rect
     (Style         : Gtkada.Style.Drawing_Style;
      Width, Height : Model_Coordinate := Fit_Size_As_Double;
      Radius        : Model_Coordinate := 0.0)
      return Rect_Item;
   procedure Initialize_Rect
     (Self          : not null access Rect_Item_Record'Class;
      Style         : Gtkada.Style.Drawing_Style;
      Width, Height : Model_Coordinate := Fit_Size_As_Double;
      Radius        : Model_Coordinate := 0.0);
   --  Create a new rectangle item.
   --  Specifying the size should rather be done with a call to
   --  Set_Size, which provides more flexibility with regards to the units
   --  used to describe the size.

   overriding procedure Draw
     (Self    : not null access Rect_Item_Record;
      Context : Draw_Context);
   overriding procedure Draw_Outline
     (Self    : not null access Rect_Item_Record;
      Style   : Gtkada.Style.Drawing_Style;
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
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
      return Ellipse_Item;
   procedure Initialize_Ellipse
     (Self          : not null access Ellipse_Item_Record'Class;
      Style         : Gtkada.Style.Drawing_Style;
      Width, Height : Model_Coordinate := Fit_Size_As_Double);
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

   ------------
   -- Images --
   ------------

   type Image_Item_Record is new Container_Item_Record with private;
   type Image_Item is access all Image_Item_Record'Class;
   --  An item that shows an image.
   --  The style is used to draw a rectangle around the image

   function Gtk_New_Image
     (Style  : Gtkada.Style.Drawing_Style;
      Image  : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
      return Image_Item;
   procedure Initialize_Image
     (Self   : not null access Image_Item_Record'Class;
      Style  : Gtkada.Style.Drawing_Style;
      Image  : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double);
   --  Create a new image item.
   --  By default, the size is computed from the image, but if self is
   --  actually allocated a different size, the image will be rescaled as
   --  appropriate. You can disable this behavior by setting Allow_Rescale to
   --  False.

   function Gtk_New_Image
     (Style  : Gtkada.Style.Drawing_Style;
      Icon_Name : String;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
      return Image_Item;
   procedure Initialize_Image
     (Self   : not null access Image_Item_Record'Class;
      Style  : Gtkada.Style.Drawing_Style;
      Icon_Name : String;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double);
   --  Same as buffer, but the image is created from one of the files given
   --  by the Gtk.Icon_Theme. This will often result in better (more sharp)
   --  rendering.
   --  You should in general specify the size you want to use, since the
   --  icon_name itself does not provide this information.

   overriding procedure Draw
     (Self    : not null access Image_Item_Record;
      Context : Draw_Context);
   overriding procedure Destroy
     (Self     : not null access Image_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class);
   overriding procedure Size_Request
     (Self    : not null access Image_Item_Record;
      Context : Draw_Context);

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
   procedure Initialize_Polyline
     (Self     : not null access Polyline_Item_Record'Class;
      Style    : Gtkada.Style.Drawing_Style;
      Points   : Item_Point_Array;
      Close    : Boolean := False;
      Relative : Boolean := False);
   --  Create a new polyline item.
   --  If Relative is true, then each point is relative to the previous one
   --  (i.e. its coordinates are the previous points's coordinate plus the
   --  offset given in points). The first point is of course in item
   --  coordinates.

   overriding procedure Draw
     (Self    : not null access Polyline_Item_Record;
      Context : Draw_Context);
   overriding procedure Destroy
     (Self     : not null access Polyline_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class);
   overriding procedure Size_Request
     (Self    : not null access Polyline_Item_Record;
      Context : Draw_Context);
   overriding function Contains
     (Self    : not null access Polyline_Item_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean;
   overriding function Clip_Line
     (Self   : not null access Polyline_Item_Record;
      P1, P2 : Item_Point) return Item_Point;

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
      Directed : Text_Arrow_Direction := No_Text_Arrow;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
      return Text_Item;
   procedure Initialize_Text
     (Self     : not null access Text_Item_Record'Class;
      Style    : Gtkada.Style.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow;
      Width, Height : Model_Coordinate := Fit_Size_As_Double);
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
   function Get_Text
     (Self : not null access Text_Item_Record) return String;
   --  Change the text displayed in the item.
   --  This does not force a refresh of the item, and it is likely that you
   --  will need to call the Model's Refresh_Layout method to properly
   --  recompute sizes of items and link paths.

   overriding procedure Draw
     (Self    : not null access Text_Item_Record;
      Context : Draw_Context);
   overriding procedure Destroy
     (Self     : not null access Text_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class);
   overriding procedure Size_Request
     (Self    : not null access Text_Item_Record;
      Context : Draw_Context);

   -------------------
   -- Editable text --
   -------------------

   type Editable_Text_Item_Record is new Text_Item_Record with private;
   type Editable_Text_Item is access all Editable_Text_Item_Record'Class;
   --  A special text item that can be double-clicked on to be editing in
   --  place (provided the Gtkada.Canvas_View.Views.On_Item_Event_Edit
   --  callback was added to the view).

   function Gtk_New_Editable_Text
     (Style    : Gtkada.Style.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow)
      return Editable_Text_Item;
   procedure Initialize_Editable_Text
     (Self     : not null access Editable_Text_Item_Record'Class;
      Style    : Gtkada.Style.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow);
   --  Create a new text item

   procedure Set_Editable
      (Self   : not null access Editable_Text_Item_Record'Class;
       Editable : Boolean);
   function Is_Editable
      (Self   : not null access Editable_Text_Item_Record'Class)
      return Boolean;
   --  Sets whether Self can be edited interactively by double-clicking
   --  on it. You should also call
   --  Gtkada.Canvas_View.Views.Cancel_Inline_Editing in case some editing
   --  was taking place.

   procedure On_Edited
     (Self     : not null access Editable_Text_Item_Record;
      Old_Text : String) is null;
   --  Called after the text has been edited

   overriding function Edit_Widget
     (Self  : not null access Editable_Text_Item_Record;
      View  : not null access Canvas_View_Record'Class)
      return Gtk.Widget.Gtk_Widget;

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
   procedure Initialize_Hr
     (Self    : not null access Hr_Item_Record'Class;
      Style   : Gtkada.Style.Drawing_Style;
      Text    : String := "");
   --  Create a new horizontal rule

   overriding procedure Draw
     (Self    : not null access Hr_Item_Record;
      Context : Draw_Context);
   overriding procedure Destroy
     (Self     : not null access Hr_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class);
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

   function Get_From
     (Self : not null access Canvas_Link_Record) return Abstract_Item;
   function Get_To
     (Self : not null access Canvas_Link_Record) return Abstract_Item;
   --  Return both ends of the link

   function Get_Label
     (Self : not null access Canvas_Link_Record) return Container_Item;
   function Get_Label_From
     (Self : not null access Canvas_Link_Record) return Container_Item;
   function Get_Label_To
     (Self : not null access Canvas_Link_Record) return Container_Item;
   --  Retrieve the various label items

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

   procedure Set_Style
     (Self  : not null access Canvas_Link_Record;
      Style : Drawing_Style);
   function Get_Style
     (Self : not null access Canvas_Link_Record) return Drawing_Style;
   --  Return the style used for the drawingo of this link.
   --  When changing the style, you must force a refresh of the canvas.

   function Get_Points
     (Self : not null access Canvas_Link_Record)
      return Item_Point_Array_Access;
   --  Return the computed points for the link.
   --  Do not free or store the result

   overriding function Is_Invisible
     (Self : not null access Canvas_Link_Record)
     return Boolean is (False);
   overriding function Inner_Most_Item
     (Self           : not null access Canvas_Link_Record;
      Dummy_At_Point : Model_Point;
      Dummy_Context  : Draw_Context)
      return Abstract_Item is (Self);
   overriding function Parent
     (Self : not null access Canvas_Link_Record)
      return Abstract_Item is (null);
   overriding function Edit_Widget
     (Self       : not null access Canvas_Link_Record;
      Dummy_View : not null access Canvas_View_Record'Class)
      return Gtk.Widget.Gtk_Widget is (null);
   overriding procedure Set_Visibility_Threshold
     (Self      : not null access Canvas_Link_Record;
      Threshold : Gdouble);
   overriding function Get_Visibility_Threshold
     (Self : not null access Canvas_Link_Record) return Gdouble;
   overriding procedure Destroy
     (Self     : not null access Canvas_Link_Record;
      In_Model : not null access Canvas_Model_Record'Class);
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
   overriding function Is_Link
     (Self : not null access Canvas_Link_Record)
      return Boolean is (True);
   procedure Draw_As_Selected
     (Self    : not null access Canvas_Link_Record;
      Context : Draw_Context);

private
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Gtkada.Style.Point_Array, Gtkada.Style.Point_Array_Access);

   type Canvas_Model_Record is abstract new Glib.Object.GObject_Record
   with record
      Layout    : Pango.Layout.Pango_Layout;

      Selection : Item_Sets.Set;
      Mode      : Selection_Mode := Selection_Single;
   end record;

   type Canvas_Item_Record is abstract new Abstract_Item_Record with record
      Position : Gtkada.Style.Point := No_Position;
      --  Position within its parent or the canvas view.

      Visibility_Threshold : Gdouble := 0.0;
      --  See Set_Visibility_Threshold.
   end record;

   type Container_Item_Record is abstract new Canvas_Item_Record with record
      Width, Height : Model_Coordinate;
      --  Computed by Size_Request. Always expressed in pixels.
      --  These do not include the margins.

      Computed_Position : Gtkada.Style.Point := (Gdouble'First, Gdouble'First);
      --  The position within the parent, as computed in Size_Allocate.
      --  The field Position is used for the position specifically requested by
      --  the user.
      --  This is always the position of the top-left corner, no matter what
      --  Anchor_X and Anchor_Y are set to.

      Anchor_X : Percent := 0.0;
      Anchor_Y : Percent := 0.0;
      --  The position within the item that Self.Position points to. This
      --  is only relevant when an explicit position was given by the user.

      Margin : Margins := No_Margins;
      --  Margins around the child

      Parent : Container_Item;
      --  The parent item

      Min_Width, Min_Height : Size := (Unit_Pixels, 1.0);
      Max_Width, Max_Height : Size := Fit_Size;
      --  Size constraints for the child. If Max_* if Fixed_Size, then the
      --  child is constrained to have Min_* has a specific size.

      Pack_End : Boolean := False;
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

   type Image_Item_Record is new Container_Item_Record with record
      Image         : Gdk.Pixbuf.Gdk_Pixbuf;
      Icon_Name     : GNAT.Strings.String_Access;
      Allow_Rescale : Boolean := True;
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
   end record;

   type Editable_Text_Item_Record is new Text_Item_Record with record
      Editable : Boolean := True;
   end record;

   type Hr_Item_Record is new Container_Item_Record with record
      Text     : GNAT.Strings.String_Access;
      Requested_Width, Requested_Height : Model_Coordinate;

      Space    : Model_Coordinate := 4.0;
      --  Space between text and lines
   end record;

   No_Waypoints : constant Item_Point_Array := (1 .. 0 => (0.0, 0.0));

   type Item_Drag_Info is record
      Item : Abstract_Item;
      Pos  : Model_Point;
   end record;

   package Item_Drag_Infos is new Ada.Containers.Hashed_Maps
     (Key_Type        => Abstract_Item,
      Element_Type    => Item_Drag_Info,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Continuous_Scroll_Data is record
      Id      : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      --  The timeout callback used to provide continuous scrolling

      Dx, Dy  : Model_Coordinate := 0.0;
      --  Amount of scrolling at each step

      Timeout : Glib.Guint := 30;
      --  Number of milliseconds between each step of the auto scrolling

      Margin  : View_Coordinate := 10.0;
      --  Number of pixels on each side of the view in which the auto
      --  scrolling should start. We can't start it only when the mouse is
      --  outside of the view, since otherwise there would be no way to get
      --  it started when the view is aligned with the screen edge.

      Speed   : Model_Coordinate := 15.0;
      --  Speed of the scrolling at each step
   end record;

   type Smart_Guide is record
      Pos        : Model_Coordinate;
      Min, Max   : Model_Coordinate;
      Visible    : Boolean := False;
   end record;
   --  Description for a smart guide:
   --  For a horizontal guide, Pos is the y coordinate of the guide, and
   --  Min,Max are its minimum and maximum x coordinates for all items along
   --  that guide.

   package Smart_Guide_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Smart_Guide);

   type Snap_Data is record
      Grid             : Boolean := True;
      Smart_Guides     : Boolean := False;
      Margin           : Model_Coordinate := 5.0;

      Hguides, Vguides : Smart_Guide_Lists.List;
      Style            : Gtkada.Style.Drawing_Style := Default_Guide_Style;
   end record;

   type Inline_Edit_Data is record
      Item : Abstract_Item;
   end record;
   --  Data used when editing a widget

   type Base_Animation_Data is abstract tagged null record;
   type Base_Animation_Data_Access is access Base_Animation_Data'Class;

   type Canvas_View_Record is new Gtk.Bin.Gtk_Bin_Record with record
      Model     : Canvas_Model;
      Topleft   : Model_Point := (0.0, 0.0);
      Scale     : Gdouble := 1.0;
      Grid_Size : Model_Coordinate := 20.0;

      Animation_Data : Base_Animation_Data_Access;
      Id_Animation   : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      --  The animation loop (see Gtkada.Canvas_View.Views.Animate)

      Id_Layout_Changed,
      Id_Item_Contents_Changed,
      Id_Item_Destroyed,
      Id_Selection_Changed : Gtk.Handlers.Handler_Id :=
         (Gtk.Handlers.Null_Handler_Id, null);
      --  Connections to model signals

      Layout     : Pango.Layout.Pango_Layout;
      Hadj, Vadj : Gtk.Adjustment.Gtk_Adjustment;

      Selection_Style : Gtkada.Style.Drawing_Style :=
        Gtkada.Style.Gtk_New
          (Stroke     => (0.8, 0.0, 0.0, 0.3),
           Line_Width => 4.0);

      Scale_To_Fit_Requested : Gdouble := 0.0;
      Scale_To_Fit_Area : Model_Rectangle;
      --  Set to true when the user calls Scale_To_Fit before the view has had
      --  a size allocated (and thus we could not perform computation).
      --  This is set to the maximal zoom requested (or 0.0 if not requested)

      Last_Button_Press : Canvas_Event_Details;
      --  Attributes of the last button_press event, used to properly handle
      --  dragging and avoid recomputing the selectd item on button_release.

      Dragged_Items : Item_Drag_Infos.Map;
      --  The items that are being dragged.

      In_Drag : Boolean := False;
      --  Whether we are in the middle of a drag.

      Topleft_At_Drag_Start : Model_Point;
      --  Toplevel at the stat of the drag

      Avoid_Overlap : Boolean := False;
      Avoid_Overlap_Duration : Standard.Duration := 0.2;

      Continuous_Scroll : Continuous_Scroll_Data;
      Snap              : Snap_Data;
      Inline_Edit       : Inline_Edit_Data;
   end record;

   type Canvas_Link_Record is new Abstract_Item_Record with record
      From, To     : Abstract_Item;
      Style        : Gtkada.Style.Drawing_Style;
      Routing      : Route_Style;
      Bounding_Box : Item_Rectangle;
      Label        : Container_Item;
      Label_From   : Container_Item;
      Label_To     : Container_Item;

      Visibility_Threshold : Gdouble := 0.0;

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
      --  Whether the waypoints are given in relative coordinates.
      --  This does not apply to Points.

      Points   : Item_Point_Array_Access;
      --  The cached computation of waypoints for this link.
      --  These are recomputed every time the layout of the canvas changes, but
      --  are cached so that redrawing the canvas is fast.
      --  These are absolute coordinates, even if waypoints are relative.
      --  See the documentation on Waypoints for more information on the format

      Anchor_From : Anchor_Attachment := Middle_Attachment;
      Anchor_To   : Anchor_Attachment := Middle_Attachment;
   end record;

   type List_Canvas_Model_Record is new Canvas_Model_Record with record
      Items : Items_Lists.List;
      --  items are sorted: lowest items first (minimal z-layer)
   end record;

   procedure Refresh_Link_Layout
     (Model : not null access Canvas_Model_Record'Class;
      Items : Item_Drag_Infos.Map := Item_Drag_Infos.Empty_Map);
   --  Refresh the layout for all links (or only the ones linked to Item, or
   --  indirectly to a link to Item).

   procedure Copy_Selected_To_Dragged_Items
     (Self  : not null access Canvas_View_Record'Class;
      Force : access Abstract_Item_Record'Class);
   --  Setup the 'dragged_items" field from the contents of the selection, and
   --  forces a specific item to be there (in addition)

   procedure Set_Adjustment_Values
     (Self : not null access Canvas_View_Record'Class);
   --  Update the values for both adjustments

end Gtkada.Canvas_View;
