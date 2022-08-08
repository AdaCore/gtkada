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
--  The Gtk.Cell_Area.Gtk_Cell_Area is an abstract class for
--  Gtk.Cell_Layout.Gtk_Cell_Layout widgets (also referred to as "layouting
--  widgets") to interface with an arbitrary number of Gtk_Cell_Renderers and
--  interact with the user for a given Gtk.Tree_Model.Gtk_Tree_Model row.
--
--  The cell area handles events, focus navigation, drawing and size requests
--  and allocations for a given row of data.
--
--  Usually users dont have to interact with the Gtk.Cell_Area.Gtk_Cell_Area
--  directly unless they are implementing a cell-layouting widget themselves.
--
--  # Requesting area sizes
--
--  As outlined in [GtkWidget's geometry management
--  section][geometry-management], GTK+ uses a height-for-width geometry
--  management system to compute the sizes of widgets and user interfaces.
--  Gtk.Cell_Area.Gtk_Cell_Area uses the same semantics to calculate the size
--  of an area for an arbitrary number of Gtk.Tree_Model.Gtk_Tree_Model rows.
--
--  When requesting the size of a cell area one needs to calculate the size
--  for a handful of rows, and this will be done differently by different
--  layouting widgets. For instance a Gtk.Tree_View_Column.Gtk_Tree_View_Column
--  always lines up the areas from top to bottom while a
--  Gtk.Icon_View.Gtk_Icon_View on the other hand might enforce that all areas
--  received the same width and wrap the areas around, requesting height for
--  more cell areas when allocated less width.
--
--  It's also important for areas to maintain some cell alignments with areas
--  rendered for adjacent rows (cells can appear "columnized" inside an area
--  even when the size of cells are different in each row). For this reason the
--  Gtk.Cell_Area.Gtk_Cell_Area uses a
--  Gtk.Cell_Area_Context.Gtk_Cell_Area_Context object to store the alignments
--  and sizes along the way (as well as the overall largest minimum and natural
--  size for all the rows which have been calculated with the said context).
--
--  The Gtk.Cell_Area_Context.Gtk_Cell_Area_Context is an opaque object
--  specific to the Gtk.Cell_Area.Gtk_Cell_Area which created it (see
--  Gtk.Cell_Area.Create_Context). The owning cell-layouting widget can create
--  as many contexts as it wishes to calculate sizes of rows which should
--  receive the same size in at least one orientation (horizontally or
--  vertically), However, it's important that the same
--  Gtk.Cell_Area_Context.Gtk_Cell_Area_Context which was used to request the
--  sizes for a given Gtk.Tree_Model.Gtk_Tree_Model row be used when rendering
--  or processing events for that row.
--
--  In order to request the width of all the rows at the root level of a
--  Gtk.Tree_Model.Gtk_Tree_Model one would do the following:
--
--  |[<!-- language="C" --> GtkTreeIter iter; gint minimum_width; gint
--  natural_width;
--
--  valid = gtk_tree_model_get_iter_first (model, &iter); while (valid) {
--  gtk_cell_area_apply_attributes (area, model, &iter, FALSE, FALSE);
--  gtk_cell_area_get_preferred_width (area, context, widget, NULL, NULL);
--
--  valid = gtk_tree_model_iter_next (model, &iter); }
--  gtk_cell_area_context_get_preferred_width (context, &minimum_width,
--  &natural_width); ]|
--
--  Note that in this example it's not important to observe the returned
--  minimum and natural width of the area for each row unless the
--  cell-layouting object is actually interested in the widths of individual
--  rows. The overall width is however stored in the accompanying
--  Gtk.Cell_Area_Context.Gtk_Cell_Area_Context object and can be consulted at
--  any time.
--
--  This can be useful since Gtk.Cell_Layout.Gtk_Cell_Layout widgets usually
--  have to support requesting and rendering rows in treemodels with an
--  exceedingly large amount of rows. The Gtk.Cell_Layout.Gtk_Cell_Layout
--  widget in that case would calculate the required width of the rows in an
--  idle or timeout source (see g_timeout_add) and when the widget is requested
--  its actual width in Gtk.Widget.GObject_Class.get_preferred_width it can
--  simply consult the width accumulated so far in the
--  Gtk.Cell_Area_Context.Gtk_Cell_Area_Context object.
--
--  A simple example where rows are rendered from top to bottom and take up
--  the full width of the layouting widget would look like:
--
--  |[<!-- language="C" --> static void foo_get_preferred_width (GtkWidget
--  *widget, gint *minimum_size, gint *natural_size) { Foo *foo = FOO (widget);
--  FooPrivate *priv = foo->priv;
--
--  foo_ensure_at_least_one_handfull_of_rows_have_been_requested (foo);
--
--  gtk_cell_area_context_get_preferred_width (priv->context, minimum_size,
--  natural_size); } ]|
--
--  In the above example the Foo widget has to make sure that some row sizes
--  have been calculated (the amount of rows that Foo judged was appropriate to
--  request space for in a single timeout iteration) before simply returning
--  the amount of space required by the area via the
--  Gtk.Cell_Area_Context.Gtk_Cell_Area_Context.
--
--  Requesting the height for width (or width for height) of an area is a
--  similar task except in this case the
--  Gtk.Cell_Area_Context.Gtk_Cell_Area_Context does not store the data
--  (actually, it does not know how much space the layouting widget plans to
--  allocate it for every row. It's up to the layouting widget to render each
--  row of data with the appropriate height and width which was requested by
--  the Gtk.Cell_Area.Gtk_Cell_Area).
--
--  In order to request the height for width of all the rows at the root level
--  of a Gtk.Tree_Model.Gtk_Tree_Model one would do the following:
--
--  |[<!-- language="C" --> GtkTreeIter iter; gint minimum_height; gint
--  natural_height; gint full_minimum_height = 0; gint full_natural_height = 0;
--
--  valid = gtk_tree_model_get_iter_first (model, &iter); while (valid) {
--  gtk_cell_area_apply_attributes (area, model, &iter, FALSE, FALSE);
--  gtk_cell_area_get_preferred_height_for_width (area, context, widget, width,
--  &minimum_height, &natural_height);
--
--  if (width_is_for_allocation) cache_row_height (&iter, minimum_height,
--  natural_height);
--
--  full_minimum_height += minimum_height; full_natural_height +=
--  natural_height;
--
--  valid = gtk_tree_model_iter_next (model, &iter); } ]|
--
--  Note that in the above example we would need to cache the heights returned
--  for each row so that we would know what sizes to render the areas for each
--  row. However we would only want to really cache the heights if the request
--  is intended for the layouting widgets real allocation.
--
--  In some cases the layouting widget is requested the height for an
--  arbitrary for_width, this is a special case for layouting widgets who need
--  to request size for tens of thousands of rows. For this case it's only
--  important that the layouting widget calculate one reasonably sized chunk of
--  rows and return that height synchronously. The reasoning here is that any
--  layouting widget is at least capable of synchronously calculating enough
--  height to fill the screen height (or scrolled window height) in response to
--  a single call to Gtk.Widget.GObject_Class.get_preferred_height_for_width.
--  Returning a perfect height for width that is larger than the screen area is
--  inconsequential since after the layouting receives an allocation from a
--  scrolled window it simply continues to drive the scrollbar values while
--  more and more height is required for the row heights that are calculated in
--  the background.
--
--  # Rendering Areas
--
--  Once area sizes have been aquired at least for the rows in the visible
--  area of the layouting widget they can be rendered at
--  Gtk.Widget.GObject_Class.draw time.
--
--  A crude example of how to render all the rows at the root level runs as
--  follows:
--
--  |[<!-- language="C" --> GtkAllocation allocation; GdkRectangle cell_area =
--  { 0, }; GtkTreeIter iter; gint minimum_width; gint natural_width;
--
--  gtk_widget_get_allocation (widget, &allocation); cell_area.width =
--  allocation.width;
--
--  valid = gtk_tree_model_get_iter_first (model, &iter); while (valid) {
--  cell_area.height = get_cached_height_for_row (&iter);
--
--  gtk_cell_area_apply_attributes (area, model, &iter, FALSE, FALSE);
--  gtk_cell_area_render (area, context, widget, cr, &cell_area, &cell_area,
--  state_flags, FALSE);
--
--  cell_area.y += cell_area.height;
--
--  valid = gtk_tree_model_iter_next (model, &iter); } ]|
--
--  Note that the cached height in this example really depends on how the
--  layouting widget works. The layouting widget might decide to give every row
--  its minimum or natural height or, if the model content is expected to fit
--  inside the layouting widget without scrolling, it would make sense to
--  calculate the allocation for each row at
--  Gtk.Widget.Gtk_Widget::size-allocate time using
--  gtk_distribute_natural_allocation.
--
--  # Handling Events and Driving Keyboard Focus
--
--  Passing events to the area is as simple as handling events on any normal
--  widget and then passing them to the Gtk.Cell_Area.Event API as they come
--  in. Usually Gtk.Cell_Area.Gtk_Cell_Area is only interested in button
--  events, however some customized derived areas can be implemented who are
--  interested in handling other events. Handling an event can trigger the
--  Gtk.Cell_Area.Gtk_Cell_Area::focus-changed signal to fire; as well as
--  Gtk.Cell_Area.Gtk_Cell_Area::add-editable in the case that an editable cell
--  was clicked and needs to start editing. You can call
--  Gtk.Cell_Area.Stop_Editing at any time to cancel any cell editing that is
--  currently in progress.
--
--  The Gtk.Cell_Area.Gtk_Cell_Area drives keyboard focus from cell to cell in
--  a way similar to Gtk.Widget.Gtk_Widget. For layouting widgets that support
--  giving focus to cells it's important to remember to pass
--  Gtk.Cell_Renderer.Cell_Renderer_Focused to the area functions for the row
--  that has focus and to tell the area to paint the focus at render time.
--
--  Layouting widgets that accept focus on cells should implement the
--  Gtk.Widget.GObject_Class.focus virtual method. The layouting widget is
--  always responsible for knowing where Gtk.Tree_Model.Gtk_Tree_Model rows are
--  rendered inside the widget, so at Gtk.Widget.GObject_Class.focus time the
--  layouting widget should use the Gtk.Cell_Area.Gtk_Cell_Area methods to
--  navigate focus inside the area and then observe the GtkDirectionType to
--  pass the focus to adjacent rows and areas.
--
--  A basic example of how the Gtk.Widget.GObject_Class.focus virtual method
--  should be implemented:
--
--  |[<!-- language="C" --> static gboolean foo_focus (GtkWidget *widget,
--  GtkDirectionType direction) { Foo *foo = FOO (widget); FooPrivate *priv =
--  foo->priv; gint focus_row; gboolean have_focus = FALSE;
--
--  focus_row = priv->focus_row;
--
--  if (!gtk_widget_has_focus (widget)) gtk_widget_grab_focus (widget);
--
--  valid = gtk_tree_model_iter_nth_child (priv->model, &iter, NULL,
--  priv->focus_row); while (valid) { gtk_cell_area_apply_attributes
--  (priv->area, priv->model, &iter, FALSE, FALSE);
--
--  if (gtk_cell_area_focus (priv->area, direction)) { priv->focus_row =
--  focus_row; have_focus = TRUE; break; } else { if (direction ==
--  GTK_DIR_RIGHT || direction == GTK_DIR_LEFT) break; else if (direction ==
--  GTK_DIR_UP || direction == GTK_DIR_TAB_BACKWARD) { if (focus_row == 0)
--  break; else { focus_row--; valid = gtk_tree_model_iter_nth_child
--  (priv->model, &iter, NULL, focus_row); } } else { if (focus_row ==
--  last_row) break; else { focus_row++; valid = gtk_tree_model_iter_next
--  (priv->model, &iter); } } } } return have_focus; } ]|
--
--  Note that the layouting widget is responsible for matching the
--  GtkDirectionType values to the way it lays out its cells.
--
--  # Cell Properties
--
--  The Gtk.Cell_Area.Gtk_Cell_Area introduces cell properties for
--  Gtk_Cell_Renderers in very much the same way that
--  Gtk.Container.Gtk_Container introduces [child properties][child-properties]
--  for Gtk_Widgets. This provides some general interfaces for defining the
--  relationship cell areas have with their cells. For instance in a
--  Gtk.Cell_Area_Box.Gtk_Cell_Area_Box a cell might "expand" and receive extra
--  space when the area is allocated more than its full natural request, or a
--  cell might be configured to "align" with adjacent rows which were requested
--  and rendered with the same Gtk.Cell_Area_Context.Gtk_Cell_Area_Context.
--
--  Use gtk_cell_area_class_install_cell_property to install cell properties
--  for a cell area class and gtk_cell_area_class_find_cell_property or
--  gtk_cell_area_class_list_cell_properties to get information about existing
--  cell properties.
--
--  To set the value of a cell property, use Gtk.Cell_Area.Cell_Set_Property,
--  gtk_cell_area_cell_set or gtk_cell_area_cell_set_valist. To obtain the
--  value of a cell property, use Gtk.Cell_Area.Cell_Get_Property,
--  gtk_cell_area_cell_get or gtk_cell_area_cell_get_valist.
--
--  </description>
--  <group>Layout Containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;                 use Cairo;
with Gdk.Event;             use Gdk.Event;
with Gdk.Rectangle;         use Gdk.Rectangle;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Glib.Values;           use Glib.Values;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Cell_Area_Context; use Gtk.Cell_Area_Context;
with Gtk.Cell_Editable;     use Gtk.Cell_Editable;
with Gtk.Cell_Layout;       use Gtk.Cell_Layout;
with Gtk.Cell_Renderer;     use Gtk.Cell_Renderer;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Cell_Area is

   type Gtk_Cell_Area_Record is new GObject_Record with null record;
   type Gtk_Cell_Area is access all Gtk_Cell_Area_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Cell_Callback is access function
     (Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   return Boolean;
   --  The type of the callback functions used for iterating over the cell
   --  renderers of a Gtk.Cell_Area.Gtk_Cell_Area, see Gtk.Cell_Area.Foreach.
   --  "renderer": the cell renderer to operate on

   type Gtk_Cell_Alloc_Callback is access function
     (Renderer        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Background : Gdk.Rectangle.Gdk_Rectangle) return Boolean;
   --  The type of the callback functions used for iterating over the cell
   --  renderers and their allocated areas inside a
   --  Gtk.Cell_Area.Gtk_Cell_Area, see Gtk.Cell_Area.Foreach_Alloc.
   --  "renderer": the cell renderer to operate on
   --  "cell_area": the area allocated to Renderer inside the rectangle
   --  provided to Gtk.Cell_Area.Foreach_Alloc.
   --  "cell_background": the background area for Renderer inside the
   --  background area provided to Gtk.Cell_Area.Foreach_Alloc.

   type Gtk_Cell_Layout_Data_Func is access procedure
     (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  A function which should set the value of Cell_Layout's cell renderer(s)
   --  as appropriate.
   --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
   --  "cell": the cell renderer whose value is to be set
   --  "tree_model": the model
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter indicating the row to set the
   --  value for

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_area_get_type");

   -------------
   -- Methods --
   -------------

   function Activate
      (Self      : not null access Gtk_Cell_Area_Record;
       Context   : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
       Flags     : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State;
       Edit_Only : Boolean) return Boolean;
   --  Activates Area, usually by activating the currently focused cell,
   --  however some subclasses which embed widgets in the area can also
   --  activate a widget if it currently has the focus.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context in context
   --  with the current row data
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering on
   --  "cell_area": the size and location of Area relative to Widget's
   --  allocation
   --  "flags": the Gtk.Cell_Renderer.Gtk_Cell_Renderer_State flags for Area
   --  for this row of data.
   --  "edit_only": if True then only cell renderers that are
   --  Gtk.Cell_Renderer.Cell_Renderer_Mode_Editable will be activated.

   function Activate_Cell
      (Self      : not null access Gtk_Cell_Area_Record;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Event     : Gdk.Event.Gdk_Event;
       Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
       Flags     : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State) return Boolean;
   --  This is used by Gtk.Cell_Area.Gtk_Cell_Area subclasses when handling
   --  events to activate cells, the base Gtk.Cell_Area.Gtk_Cell_Area class
   --  activates cells for keyboard events for free in its own
   --  GtkCellArea->activate implementation.
   --  Since: gtk+ 3.0
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering onto
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer in Area to activate
   --  "event": the Gdk.Event.Gdk_Event for which cell activation should occur
   --  "cell_area": the Gdk.Rectangle.Gdk_Rectangle in Widget relative
   --  coordinates of Renderer for the current row.
   --  "flags": the Gtk.Cell_Renderer.Gtk_Cell_Renderer_State for Renderer

   procedure Add
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Adds Renderer to Area with the default child cell properties.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to add to Area

   procedure Add_Focus_Sibling
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Sibling  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Adds Sibling to Renderer's focusable area, focus will be drawn around
   --  Renderer and all of its siblings if Renderer can focus for a given row.
   --  Events handled by focus siblings can also activate the given focusable
   --  Renderer.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer expected to have
   --  focus
   --  "sibling": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to add to Renderer's
   --  focus area

   procedure Apply_Attributes
      (Self        : not null access Gtk_Cell_Area_Record;
       Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
       Is_Expander : Boolean;
       Is_Expanded : Boolean);
   --  Applies any connected attributes to the renderers in Area by pulling
   --  the values from Tree_Model.
   --  Since: gtk+ 3.0
   --  "tree_model": the Gtk.Tree_Model.Gtk_Tree_Model to pull values from
   --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter in Tree_Model to apply values
   --  for
   --  "is_expander": whether Iter has children
   --  "is_expanded": whether Iter is expanded in the view and children are
   --  visible

   procedure Attribute_Connect
      (Self      : not null access Gtk_Cell_Area_Record;
       Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String;
       Column    : Glib.Gint);
   --  Connects an Attribute to apply values from Column for the
   --  Gtk.Tree_Model.Gtk_Tree_Model in use.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to connect an
   --  attribute for
   --  "attribute": the attribute name
   --  "column": the Gtk.Tree_Model.Gtk_Tree_Model column to fetch attribute
   --  values from

   procedure Attribute_Disconnect
      (Self      : not null access Gtk_Cell_Area_Record;
       Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String);
   --  Disconnects Attribute for the Renderer in Area so that attribute will
   --  no longer be updated with values from the model.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to disconnect an
   --  attribute for
   --  "attribute": the attribute name

   function Attribute_Get_Column
      (Self      : not null access Gtk_Cell_Area_Record;
       Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String) return Glib.Gint;
   --  Returns the model column that an attribute has been mapped to, or -1 if
   --  the attribute is not mapped.
   --  Since: gtk+ 3.14
   --  "renderer": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  "attribute": an attribute on the renderer

   procedure Cell_Get_Property
      (Self          : not null access Gtk_Cell_Area_Record;
       Renderer      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue);
   --  Gets the value of a cell property for Renderer in Area.
   --  Since: gtk+ 3.0
   --  "renderer": a Gtk.Cell_Renderer.Gtk_Cell_Renderer inside Area
   --  "property_name": the name of the property to get
   --  "value": a location to return the value

   procedure Cell_Set_Property
      (Self          : not null access Gtk_Cell_Area_Record;
       Renderer      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue);
   --  Sets a cell property for Renderer in Area.
   --  Since: gtk+ 3.0
   --  "renderer": a Gtk.Cell_Renderer.Gtk_Cell_Renderer inside Area
   --  "property_name": the name of the cell property to set
   --  "value": the value to set the cell property to

   function Copy_Context
      (Self    : not null access Gtk_Cell_Area_Record;
       Context : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class)
       return Gtk.Cell_Area_Context.Gtk_Cell_Area_Context;
   --  This is sometimes needed for cases where rows need to share alignments
   --  in one orientation but may be separately grouped in the opposing
   --  orientation.
   --  For instance, Gtk.Icon_View.Gtk_Icon_View creates all icons (rows) to
   --  have the same width and the cells theirin to have the same horizontal
   --  alignments. However each row of icons may have a separate collective
   --  height. Gtk.Icon_View.Gtk_Icon_View uses this to request the heights of
   --  each row based on a context which was already used to request all the
   --  row widths that are to be displayed.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context to copy

   function Create_Context
      (Self : not null access Gtk_Cell_Area_Record)
       return Gtk.Cell_Area_Context.Gtk_Cell_Area_Context;
   --  Creates a Gtk.Cell_Area_Context.Gtk_Cell_Area_Context to be used with
   --  Area for all purposes. Gtk.Cell_Area_Context.Gtk_Cell_Area_Context
   --  stores geometry information for rows for which it was operated on, it is
   --  important to use the same context for the same row of data at all times
   --  (i.e. one should render and handle events with the same
   --  Gtk.Cell_Area_Context.Gtk_Cell_Area_Context which was used to request
   --  the size of those rows of data).
   --  Since: gtk+ 3.0

   function Event
      (Self      : not null access Gtk_Cell_Area_Record;
       Context   : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Event     : Gdk.Event.Gdk_Event;
       Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
       Flags     : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State)
       return Glib.Gint;
   --  Delegates event handling to a Gtk.Cell_Area.Gtk_Cell_Area.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context for this row
   --  of data.
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering to
   --  "event": the Gdk.Event.Gdk_Event to handle
   --  "cell_area": the Widget relative coordinates for Area
   --  "flags": the Gtk.Cell_Renderer.Gtk_Cell_Renderer_State for Area in this
   --  row.

   function Focus
      (Self      : not null access Gtk_Cell_Area_Record;
       Direction : Gtk.Enums.Gtk_Direction_Type) return Boolean;
   --  This should be called by the Area's owning layout widget when focus is
   --  to be passed to Area, or moved within Area for a given Direction and row
   --  data.
   --  Implementing Gtk.Cell_Area.Gtk_Cell_Area classes should implement this
   --  method to receive and navigate focus in its own way particular to how it
   --  lays out cells.
   --  Since: gtk+ 3.0
   --  "direction": the Gtk.Enums.Gtk_Direction_Type

   procedure Foreach
      (Self     : not null access Gtk_Cell_Area_Record;
       Callback : Gtk_Cell_Callback);
   --  Calls Callback for every Gtk.Cell_Renderer.Gtk_Cell_Renderer in Area.
   --  Since: gtk+ 3.0
   --  "callback": the Gtk_Cell_Callback to call

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Foreach_User_Data is

      type Gtk_Cell_Callback is access function
        (Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Data     : User_Data_Type) return Boolean;
      --  The type of the callback functions used for iterating over the cell
      --  renderers of a Gtk.Cell_Area.Gtk_Cell_Area, see Gtk.Cell_Area.Foreach.
      --  "renderer": the cell renderer to operate on
      --  "data": user-supplied data

      procedure Foreach
         (Self          : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
          Callback      : Gtk_Cell_Callback;
          Callback_Data : User_Data_Type);
      --  Calls Callback for every Gtk.Cell_Renderer.Gtk_Cell_Renderer in
      --  Area.
      --  Since: gtk+ 3.0
      --  "callback": the Gtk_Cell_Callback to call
      --  "callback_data": user provided data pointer

   end Foreach_User_Data;

   procedure Foreach_Alloc
      (Self            : not null access Gtk_Cell_Area_Record;
       Context         : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Callback        : Gtk_Cell_Alloc_Callback);
   --  Calls Callback for every Gtk.Cell_Renderer.Gtk_Cell_Renderer in Area
   --  with the allocated rectangle inside Cell_Area.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context for this row
   --  of data.
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering to
   --  "cell_area": the Widget relative coordinates and size for Area
   --  "background_area": the Widget relative coordinates of the background
   --  area
   --  "callback": the Gtk_Cell_Alloc_Callback to call

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Foreach_Alloc_User_Data is

      type Gtk_Cell_Alloc_Callback is access function
        (Renderer        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
         Cell_Background : Gdk.Rectangle.Gdk_Rectangle;
         Data            : User_Data_Type) return Boolean;
      --  The type of the callback functions used for iterating over the cell
      --  renderers and their allocated areas inside a
      --  Gtk.Cell_Area.Gtk_Cell_Area, see Gtk.Cell_Area.Foreach_Alloc.
      --  "renderer": the cell renderer to operate on
      --  "cell_area": the area allocated to Renderer inside the rectangle
      --  provided to Gtk.Cell_Area.Foreach_Alloc.
      --  "cell_background": the background area for Renderer inside the
      --  background area provided to Gtk.Cell_Area.Foreach_Alloc.
      --  "data": user-supplied data

      procedure Foreach_Alloc
         (Self            : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
          Context         : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
          Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
          Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
          Background_Area : Gdk.Rectangle.Gdk_Rectangle;
          Callback        : Gtk_Cell_Alloc_Callback;
          Callback_Data   : User_Data_Type);
      --  Calls Callback for every Gtk.Cell_Renderer.Gtk_Cell_Renderer in Area
      --  with the allocated rectangle inside Cell_Area.
      --  Since: gtk+ 3.0
      --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context for this
      --  row of data.
      --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering to
      --  "cell_area": the Widget relative coordinates and size for Area
      --  "background_area": the Widget relative coordinates of the background
      --  area
      --  "callback": the Gtk_Cell_Alloc_Callback to call
      --  "callback_data": user provided data pointer

   end Foreach_Alloc_User_Data;

   procedure Get_Cell_Allocation
      (Self       : not null access Gtk_Cell_Area_Record;
       Context    : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Renderer   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Cell_Area  : access Gdk.Rectangle.Gdk_Rectangle;
       Allocation : access Gdk.Rectangle.Gdk_Rectangle);
   --  Derives the allocation of Renderer inside Area if Area were to be
   --  renderered in Cell_Area.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context used to hold
   --  sizes for Area.
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering on
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to get the
   --  allocation for
   --  "cell_area": the whole allocated area for Area in Widget for this row
   --  "allocation": where to store the allocation for Renderer

   function Get_Current_Path_String
      (Self : not null access Gtk_Cell_Area_Record) return UTF8_String;
   --  Gets the current Gtk.Tree_Model.Gtk_Tree_Path string for the currently
   --  applied Gtk.Tree_Model.Gtk_Tree_Iter, this is implicitly updated when
   --  Gtk.Cell_Area.Apply_Attributes is called and can be used to interact
   --  with renderers from Gtk.Cell_Area.Gtk_Cell_Area subclasses.
   --  Since: gtk+ 3.0

   function Get_Edit_Widget
      (Self : not null access Gtk_Cell_Area_Record)
       return Gtk.Cell_Editable.Gtk_Cell_Editable;
   --  Gets the Gtk.Cell_Editable.Gtk_Cell_Editable widget currently used to
   --  edit the currently edited cell.
   --  Since: gtk+ 3.0

   function Get_Edited_Cell
      (Self : not null access Gtk_Cell_Area_Record)
       return Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   --  Gets the Gtk.Cell_Renderer.Gtk_Cell_Renderer in Area that is currently
   --  being edited.
   --  Since: gtk+ 3.0

   function Get_Focus_Cell
      (Self : not null access Gtk_Cell_Area_Record)
       return Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   --  Retrieves the currently focused cell for Area
   --  Since: gtk+ 3.0

   procedure Set_Focus_Cell
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Explicitly sets the currently focused cell to Renderer.
   --  This is generally called by implementations of
   --  Gtk.Cell_Area_Class.Gtk_Cell_Area_Class.focus or
   --  Gtk.Cell_Area_Class.Gtk_Cell_Area_Class.event, however it can also be
   --  used to implement functions such as Gtk.Tree_View.Set_Cursor_On_Cell.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to give focus to

   function Get_Focus_From_Sibling
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   --  Gets the Gtk.Cell_Renderer.Gtk_Cell_Renderer which is expected to be
   --  focusable for which Renderer is, or may be a sibling.
   --  This is handy for Gtk.Cell_Area.Gtk_Cell_Area subclasses when handling
   --  events, after determining the renderer at the event location it can then
   --  chose to activate the focus cell for which the event cell may have been
   --  a sibling.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer

   function Get_Focus_Siblings
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Glib.Object.Object_Simple_List.Glist;
   --  Gets the focus sibling cell renderers for Renderer.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer expected to have
   --  focus

   procedure Get_Preferred_Height
      (Self           : not null access Gtk_Cell_Area_Record;
       Context        : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint);
   --  Retrieves a cell area's initial minimum and natural height.
   --  Area will store some geometrical information in Context along the way;
   --  when requesting sizes over an arbitrary number of rows, it's not
   --  important to check the Minimum_Height and Natural_Height of this call
   --  but rather to consult Gtk.Cell_Area_Context.Get_Preferred_Height after a
   --  series of requests.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context to perform
   --  this request with
   --  "widget": the Gtk.Widget.Gtk_Widget where Area will be rendering
   --  "minimum_height": location to store the minimum height, or null
   --  "natural_height": location to store the natural height, or null

   procedure Get_Preferred_Height_For_Width
      (Self           : not null access Gtk_Cell_Area_Record;
       Context        : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Width          : Glib.Gint;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint);
   --  Retrieves a cell area's minimum and natural height if it would be given
   --  the specified Width.
   --  Area stores some geometrical information in Context along the way while
   --  calling Gtk.Cell_Area.Get_Preferred_Width. It's important to perform a
   --  series of Gtk.Cell_Area.Get_Preferred_Width requests with Context first
   --  and then call Gtk.Cell_Area.Get_Preferred_Height_For_Width on each cell
   --  area individually to get the height for width of each fully requested
   --  row.
   --  If at some point, the width of a single row changes, it should be
   --  requested with Gtk.Cell_Area.Get_Preferred_Width again and then the full
   --  width of the requested rows checked again with
   --  Gtk.Cell_Area_Context.Get_Preferred_Width.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context which has
   --  already been requested for widths.
   --  "widget": the Gtk.Widget.Gtk_Widget where Area will be rendering
   --  "width": the width for which to check the height of this area
   --  "minimum_height": location to store the minimum height, or null
   --  "natural_height": location to store the natural height, or null

   procedure Get_Preferred_Width
      (Self          : not null access Gtk_Cell_Area_Record;
       Context       : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint);
   --  Retrieves a cell area's initial minimum and natural width.
   --  Area will store some geometrical information in Context along the way;
   --  when requesting sizes over an arbitrary number of rows, it's not
   --  important to check the Minimum_Width and Natural_Width of this call but
   --  rather to consult Gtk.Cell_Area_Context.Get_Preferred_Width after a
   --  series of requests.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context to perform
   --  this request with
   --  "widget": the Gtk.Widget.Gtk_Widget where Area will be rendering
   --  "minimum_width": location to store the minimum width, or null
   --  "natural_width": location to store the natural width, or null

   procedure Get_Preferred_Width_For_Height
      (Self          : not null access Gtk_Cell_Area_Record;
       Context       : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Height        : Glib.Gint;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint);
   --  Retrieves a cell area's minimum and natural width if it would be given
   --  the specified Height.
   --  Area stores some geometrical information in Context along the way while
   --  calling Gtk.Cell_Area.Get_Preferred_Height. It's important to perform a
   --  series of Gtk.Cell_Area.Get_Preferred_Height requests with Context first
   --  and then call Gtk.Cell_Area.Get_Preferred_Width_For_Height on each cell
   --  area individually to get the height for width of each fully requested
   --  row.
   --  If at some point, the height of a single row changes, it should be
   --  requested with Gtk.Cell_Area.Get_Preferred_Height again and then the
   --  full height of the requested rows checked again with
   --  Gtk.Cell_Area_Context.Get_Preferred_Height.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context which has
   --  already been requested for widths.
   --  "widget": the Gtk.Widget.Gtk_Widget where Area will be rendering
   --  "height": the height for which to check the width of this area
   --  "minimum_width": location to store the minimum width, or null
   --  "natural_width": location to store the natural width, or null

   function Get_Request_Mode
      (Self : not null access Gtk_Cell_Area_Record)
       return Gtk.Enums.Gtk_Size_Request_Mode;
   --  Gets whether the area prefers a height-for-width layout or a
   --  width-for-height layout.
   --  Since: gtk+ 3.0

   function Has_Renderer
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Boolean;
   --  Checks if Area contains Renderer.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to check

   procedure Inner_Cell_Area
      (Self       : not null access Gtk_Cell_Area_Record;
       Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_Area  : Gdk.Rectangle.Gdk_Rectangle;
       Inner_Area : out Gdk.Rectangle.Gdk_Rectangle);
   --  This is a convenience function for Gtk.Cell_Area.Gtk_Cell_Area
   --  implementations to get the inner area where a given
   --  Gtk.Cell_Renderer.Gtk_Cell_Renderer will be rendered. It removes any
   --  padding previously added by Gtk.Cell_Area.Request_Renderer.
   --  Since: gtk+ 3.0
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering onto
   --  "cell_area": the Widget relative coordinates where one of Area's cells
   --  is to be placed
   --  "inner_area": the return location for the inner cell area

   function Is_Activatable
      (Self : not null access Gtk_Cell_Area_Record) return Boolean;
   --  Returns whether the area can do anything when activated, after applying
   --  new attributes to Area.
   --  Since: gtk+ 3.0

   function Is_Focus_Sibling
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Sibling  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Boolean;
   --  Returns whether Sibling is one of Renderer's focus siblings (see
   --  Gtk.Cell_Area.Add_Focus_Sibling).
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer expected to have
   --  focus
   --  "sibling": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to check against
   --  Renderer's sibling list

   procedure Remove
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Removes Renderer from Area.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to remove from Area

   procedure Remove_Focus_Sibling
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Sibling  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Removes Sibling from Renderer's focus sibling list (see
   --  Gtk.Cell_Area.Add_Focus_Sibling).
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer expected to have
   --  focus
   --  "sibling": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to remove from
   --  Renderer's focus area

   procedure Render
      (Self            : not null access Gtk_Cell_Area_Record;
       Context         : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Cr              : Cairo.Cairo_Context;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Flags           : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State;
       Paint_Focus     : Boolean);
   --  Renders Area's cells according to Area's layout onto Widget at the
   --  given coordinates.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context for this row
   --  of data.
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering to
   --  "cr": the cairo_t to render with
   --  "background_area": the Widget relative coordinates for Area's
   --  background
   --  "cell_area": the Widget relative coordinates for Area
   --  "flags": the Gtk.Cell_Renderer.Gtk_Cell_Renderer_State for Area in this
   --  row.
   --  "paint_focus": whether Area should paint focus on focused cells for
   --  focused rows or not.

   procedure Request_Renderer
      (Self         : not null access Gtk_Cell_Area_Record;
       Renderer     : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Orientation  : Gtk.Enums.Gtk_Orientation;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       For_Size     : Glib.Gint;
       Minimum_Size : out Glib.Gint;
       Natural_Size : out Glib.Gint);
   --  This is a convenience function for Gtk.Cell_Area.Gtk_Cell_Area
   --  implementations to request size for cell renderers. It's important to
   --  use this function to request size and then use
   --  Gtk.Cell_Area.Inner_Cell_Area at render and event time since this
   --  function will add padding around the cell for focus painting.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to request size for
   --  "orientation": the Gtk.Enums.Gtk_Orientation in which to request size
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering onto
   --  "for_size": the allocation contextual size to request for, or -1 if the
   --  base request for the orientation is to be returned.
   --  "minimum_size": location to store the minimum size, or null
   --  "natural_size": location to store the natural size, or null

   procedure Stop_Editing
      (Self     : not null access Gtk_Cell_Area_Record;
       Canceled : Boolean);
   --  Explicitly stops the editing of the currently edited cell.
   --  If Canceled is True, the currently edited cell renderer will emit the
   --  ::editing-canceled signal, otherwise the the ::editing-done signal will
   --  be emitted on the current edit widget.
   --  See Gtk.Cell_Area.Get_Edited_Cell and Gtk.Cell_Area.Get_Edit_Widget.
   --  Since: gtk+ 3.0
   --  "canceled": whether editing was canceled.

   procedure Set_Cell_Data_Func
      (Cell_Layout : not null access Gtk_Cell_Area_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func        : Gtk_Cell_Layout_Data_Func);
   --  Sets the Gtk_Cell_Layout_Data_Func to use for Cell_Layout.
   --  This function is used instead of the standard attributes mapping for
   --  setting the column value, and should set the value of Cell_Layout's cell
   --  renderer(s) as appropriate.
   --  Func may be null to remove a previously set function.
   --  Since: gtk+ 2.4
   --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  "func": the Gtk_Cell_Layout_Data_Func to use, or null

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Cell_Data_Func_User_Data is

      type Gtk_Cell_Layout_Data_Func is access procedure
        (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
         Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
         Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data        : User_Data_Type);
      --  A function which should set the value of Cell_Layout's cell renderer(s)
      --  as appropriate.
      --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
      --  "cell": the cell renderer whose value is to be set
      --  "tree_model": the model
      --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter indicating the row to set the
      --  value for
      --  "data": user data passed to Gtk.Cell_Layout.Set_Cell_Data_Func

      procedure Set_Cell_Data_Func
         (Cell_Layout : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
          Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func        : Gtk_Cell_Layout_Data_Func;
          Func_Data   : User_Data_Type);
      --  Sets the Gtk_Cell_Layout_Data_Func to use for Cell_Layout.
      --  This function is used instead of the standard attributes mapping for
      --  setting the column value, and should set the value of Cell_Layout's
      --  cell renderer(s) as appropriate.
      --  Func may be null to remove a previously set function.
      --  Since: gtk+ 2.4
      --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
      --  "func": the Gtk_Cell_Layout_Data_Func to use, or null
      --  "func_data": user data for Func

   end Set_Cell_Data_Func_User_Data;

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get_Area
     (Context : access Gtk_Cell_Area_Context_Record)
   return Gtk.Cell_Area.Gtk_Cell_Area;
   --  Fetches the Gtk.Cell_Area.Gtk_Cell_Area this Context was created by.
   --  This is generally unneeded by layouting widgets; however it is important
   --  for the context implementation itself to fetch information about the
   --  area it is being used for.
   --  For instance at GtkCellAreaContextClass.allocate time its important to
   --  know details about any cell spacing that the Gtk.Cell_Area.Gtk_Cell_Area
   --  is configured with in order to compute a proper allocation.
   --  Since: gtk+ 3.0

   function Get_Area
     (Cell_Layout : Gtk_Cell_Layout) return Gtk.Cell_Area.Gtk_Cell_Area;
   --  Returns the underlying Gtk.Cell_Area.Gtk_Cell_Area which might be
   --  Cell_Layout if called on a Gtk.Cell_Area.Gtk_Cell_Area or might be null
   --  if no Gtk.Cell_Area.Gtk_Cell_Area is used by Cell_Layout.
   --  Since: gtk+ 3.0

   procedure Get_Cell_At_Position
     (Self       : access Gtk_Cell_Area_Record;
      Context    : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
      Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cell_Area  : Gdk.Rectangle.Gdk_Rectangle;
      X          : Gint;
      Y          : Gint;
      Alloc_Area : out Gdk.Rectangle.Gdk_Rectangle;
      Renderer   : out Gtk.Cell_Renderer.Gtk_Cell_Renderer);
   --  Gets the Gtk.Cell_Renderer.Gtk_Cell_Renderer at X and Y coordinates
   --  inside Area and optionally returns the full cell allocation for it
   --  inside Cell_Area.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context used to hold
   --  sizes for Area.
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering on
   --  "cell_area": the whole allocated area for Area in Widget for this row
   --  "x": the x position
   --  "y": the y position
   --  "alloc_area": where to store the inner allocated area of the returned
   --  cell renderer, or null.
   --  "renderer": the rendered that was found.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Add_Attribute
      (Cell_Layout : not null access Gtk_Cell_Area_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute   : UTF8_String;
       Column      : Glib.Gint);

   procedure Clear (Cell_Layout : not null access Gtk_Cell_Area_Record);

   procedure Clear_Attributes
      (Cell_Layout : not null access Gtk_Cell_Area_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);

   function Get_Cells
      (Cell_Layout : not null access Gtk_Cell_Area_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;

   procedure Pack_End
      (Cell_Layout : not null access Gtk_Cell_Area_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Pack_Start
      (Cell_Layout : not null access Gtk_Cell_Area_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Reorder
      (Cell_Layout : not null access Gtk_Cell_Area_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position    : Glib.Gint);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Edit_Widget_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Cell_Editable.Gtk_Cell_Editable
   --  The widget currently editing the edited cell
   --
   --  This property is read-only and only changes as a result of a call
   --  Gtk.Cell_Area.Activate_Cell.

   Edited_Cell_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  The cell in the area that is currently edited
   --
   --  This property is read-only and only changes as a result of a call
   --  Gtk.Cell_Area.Activate_Cell.

   Focus_Cell_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  The cell in the area that currently has focus

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void is not null access procedure
     (Self      : access Gtk_Cell_Area_Record'Class;
      Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Editable  : Gtk.Cell_Editable.Gtk_Cell_Editable;
      Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
      Path      : UTF8_String);

   type Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void is not null access procedure
     (Self      : access Glib.Object.GObject_Record'Class;
      Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Editable  : Gtk.Cell_Editable.Gtk_Cell_Editable;
      Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
      Path      : UTF8_String);

   Signal_Add_Editable : constant Glib.Signal_Name := "add-editable";
   procedure On_Add_Editable
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Add_Editable
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Indicates that editing has started on Renderer and that Editable should
   --  be added to the owning cell-layouting widget at Cell_Area.
   -- 
   --  Callback parameters:
   --    --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer that started the
   --    --  edited
   --    --  "editable": the Gtk.Cell_Editable.Gtk_Cell_Editable widget to add
   --    --  "cell_area": the Gtk.Widget.Gtk_Widget relative
   --    --  Gdk.Rectangle.Gdk_Rectangle coordinates where Editable should be added
   --    --  "path": the Gtk.Tree_Model.Gtk_Tree_Path string this edit was initiated
   --    --  for

   type Cb_Gtk_Cell_Area_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void is not null access procedure
     (Self        : access Gtk_Cell_Area_Record'Class;
      Model       : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Is_Expander : Boolean;
      Is_Expanded : Boolean);

   type Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void is not null access procedure
     (Self        : access Glib.Object.GObject_Record'Class;
      Model       : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Is_Expander : Boolean;
      Is_Expanded : Boolean);

   Signal_Apply_Attributes : constant Glib.Signal_Name := "apply-attributes";
   procedure On_Apply_Attributes
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_Gtk_Cell_Area_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void;
       After : Boolean := False);
   procedure On_Apply_Attributes
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted whenever applying attributes to Area from Model
   -- 
   --  Callback parameters:
   --    --  "model": the Gtk.Tree_Model.Gtk_Tree_Model to apply the attributes from
   --    --  "iter": the Gtk.Tree_Model.Gtk_Tree_Iter indicating which row to apply
   --    --  the attributes of
   --    --  "is_expander": whether the view shows children for this row
   --    --  "is_expanded": whether the view is currently showing the children of
   --    --  this row

   type Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_UTF8_String_Void is not null access procedure
     (Self     : access Gtk_Cell_Area_Record'Class;
      Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Path     : UTF8_String);

   type Cb_GObject_Gtk_Cell_Renderer_UTF8_String_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Path     : UTF8_String);

   Signal_Focus_Changed : constant Glib.Signal_Name := "focus-changed";
   procedure On_Focus_Changed
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Focus_Changed
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_GObject_Gtk_Cell_Renderer_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Indicates that focus changed on this Area. This signal is emitted
   --  either as a result of focus handling or event handling.
   --
   --  It's possible that the signal is emitted even if the currently focused
   --  renderer did not change, this is because focus may change to the same
   --  renderer in the same cell area for a different row of data.
   -- 
   --  Callback parameters:
   --    --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer that has focus
   --    --  "path": the current Gtk.Tree_Model.Gtk_Tree_Path string set for Area

   type Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void is not null access procedure
     (Self     : access Gtk_Cell_Area_Record'Class;
      Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Editable : Gtk.Cell_Editable.Gtk_Cell_Editable);

   type Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Editable : Gtk.Cell_Editable.Gtk_Cell_Editable);

   Signal_Remove_Editable : constant Glib.Signal_Name := "remove-editable";
   procedure On_Remove_Editable
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void;
       After : Boolean := False);
   procedure On_Remove_Editable
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Indicates that editing finished on Renderer and that Editable should be
   --  removed from the owning cell-layouting widget.
   -- 
   --  Callback parameters:
   --    --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer that finished
   --    --  editeding
   --    --  "editable": the Gtk.Cell_Editable.Gtk_Cell_Editable widget to remove

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "CellLayout"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Cell_Area_Record, Gtk_Cell_Area);
   function "+"
     (Widget : access Gtk_Cell_Area_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Cell_Area
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Cell_Layout is new Glib.Types.Implements
     (Gtk.Cell_Layout.Gtk_Cell_Layout, Gtk_Cell_Area_Record, Gtk_Cell_Area);
   function "+"
     (Widget : access Gtk_Cell_Area_Record'Class)
   return Gtk.Cell_Layout.Gtk_Cell_Layout
   renames Implements_Gtk_Cell_Layout.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Layout.Gtk_Cell_Layout)
   return Gtk_Cell_Area
   renames Implements_Gtk_Cell_Layout.To_Object;

private
   Focus_Cell_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("focus-cell");
   Edited_Cell_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("edited-cell");
   Edit_Widget_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("edit-widget");
end Gtk.Cell_Area;
