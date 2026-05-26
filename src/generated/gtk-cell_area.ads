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

--  An abstract class for laying out `GtkCellRenderer`s
--
--  The `GtkCellArea` is an abstract class for [ifaceGtk.CellLayout] widgets
--  (also referred to as "layouting widgets") to interface with an arbitrary
--  number of [classGtk.CellRenderer]s and interact with the user for a given
--  [ifaceGtk.TreeModel] row.
--
--  The cell area handles events, focus navigation, drawing and size requests
--  and allocations for a given row of data.
--
--  Usually users dont have to interact with the `GtkCellArea` directly unless
--  they are implementing a cell-layouting widget themselves.
--
--  ## Requesting area sizes
--
--  As outlined in [GtkWidget's geometry management
--  section](class.Widget.htmlheight-for-width-geometry-management), GTK uses a
--  height-for-width geometry management system to compute the sizes of widgets
--  and user interfaces. `GtkCellArea` uses the same semantics to calculate the
--  size of an area for an arbitrary number of `GtkTreeModel` rows.
--
--  When requesting the size of a cell area one needs to calculate the size
--  for a handful of rows, and this will be done differently by different
--  layouting widgets. For instance a [classGtk.TreeViewColumn] always lines up
--  the areas from top to bottom while a [classGtk.IconView] on the other hand
--  might enforce that all areas received the same width and wrap the areas
--  around, requesting height for more cell areas when allocated less width.
--
--  It's also important for areas to maintain some cell alignments with areas
--  rendered for adjacent rows (cells can appear "columnized" inside an area
--  even when the size of cells are different in each row). For this reason the
--  `GtkCellArea` uses a [classGtk.CellAreaContext] object to store the
--  alignments and sizes along the way (as well as the overall largest minimum
--  and natural size for all the rows which have been calculated with the said
--  context).
--
--  The [classGtk.CellAreaContext] is an opaque object specific to the
--  `GtkCellArea` which created it (see [methodGtk.CellArea.create_context]).
--
--  The owning cell-layouting widget can create as many contexts as it wishes
--  to calculate sizes of rows which should receive the same size in at least
--  one orientation (horizontally or vertically), However, it's important that
--  the same [classGtk.CellAreaContext] which was used to request the sizes for
--  a given `GtkTreeModel` row be used when rendering or processing events for
--  that row.
--
--  In order to request the width of all the rows at the root level of a
--  `GtkTreeModel` one would do the following:
--
--  ```c GtkTreeIter iter; int minimum_width; int natural_width;
--
--  valid = gtk_tree_model_get_iter_first (model, &iter); while (valid) {
--  gtk_cell_area_apply_attributes (area, model, &iter, FALSE, FALSE);
--  gtk_cell_area_get_preferred_width (area, context, widget, NULL, NULL);
--
--  valid = gtk_tree_model_iter_next (model, &iter); }
--
--  gtk_cell_area_context_get_preferred_width (context, &minimum_width,
--  &natural_width); ```
--
--  Note that in this example it's not important to observe the returned
--  minimum and natural width of the area for each row unless the
--  cell-layouting object is actually interested in the widths of individual
--  rows. The overall width is however stored in the accompanying
--  `GtkCellAreaContext` object and can be consulted at any time.
--
--  This can be useful since `GtkCellLayout` widgets usually have to support
--  requesting and rendering rows in treemodels with an exceedingly large
--  amount of rows. The `GtkCellLayout` widget in that case would calculate the
--  required width of the rows in an idle or timeout source (see
--  [funcGlib.timeout_add]) and when the widget is requested its actual width
--  in [vfuncGtk.Widget.measure] it can simply consult the width accumulated so
--  far in the `GtkCellAreaContext` object.
--
--  A simple example where rows are rendered from top to bottom and take up
--  the full width of the layouting widget would look like:
--
--  ```c static void foo_get_preferred_width (GtkWidget *widget, int
--  *minimum_size, int *natural_size) { Foo *self = FOO (widget); FooPrivate
--  *priv = foo_get_instance_private (self);
--
--  foo_ensure_at_least_one_handfull_of_rows_have_been_requested (self);
--
--  gtk_cell_area_context_get_preferred_width (priv->context, minimum_size,
--  natural_size); } ```
--
--  In the above example the `Foo` widget has to make sure that some row sizes
--  have been calculated (the amount of rows that `Foo` judged was appropriate
--  to request space for in a single timeout iteration) before simply returning
--  the amount of space required by the area via the `GtkCellAreaContext`.
--
--  Requesting the height for width (or width for height) of an area is a
--  similar task except in this case the `GtkCellAreaContext` does not store
--  the data (actually, it does not know how much space the layouting widget
--  plans to allocate it for every row. It's up to the layouting widget to
--  render each row of data with the appropriate height and width which was
--  requested by the `GtkCellArea`).
--
--  In order to request the height for width of all the rows at the root level
--  of a `GtkTreeModel` one would do the following:
--
--  ```c GtkTreeIter iter; int minimum_height; int natural_height; int
--  full_minimum_height = 0; int full_natural_height = 0;
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
--  valid = gtk_tree_model_iter_next (model, &iter); } ```
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
--  a single call to [vfuncGtk.Widget.measure]. Returning a perfect height for
--  width that is larger than the screen area is inconsequential since after
--  the layouting receives an allocation from a scrolled window it simply
--  continues to drive the scrollbar values while more and more height is
--  required for the row heights that are calculated in the background.
--
--  ## Rendering Areas
--
--  Once area sizes have been acquired at least for the rows in the visible
--  area of the layouting widget they can be rendered at
--  [vfuncGtk.Widget.snapshot] time.
--
--  A crude example of how to render all the rows at the root level runs as
--  follows:
--
--  ```c GtkAllocation allocation; GdkRectangle cell_area = { 0, };
--  GtkTreeIter iter; int minimum_width; int natural_width;
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
--  valid = gtk_tree_model_iter_next (model, &iter); } ```
--
--  Note that the cached height in this example really depends on how the
--  layouting widget works. The layouting widget might decide to give every row
--  its minimum or natural height or, if the model content is expected to fit
--  inside the layouting widget without scrolling, it would make sense to
--  calculate the allocation for each row at the time the widget is allocated
--  using [funcGtk.distribute_natural_allocation].
--
--  ## Handling Events and Driving Keyboard Focus
--
--  Passing events to the area is as simple as handling events on any normal
--  widget and then passing them to the [methodGtk.CellArea.event] API as they
--  come in. Usually `GtkCellArea` is only interested in button events, however
--  some customized derived areas can be implemented who are interested in
--  handling other events. Handling an event can trigger the
--  [signalGtk.CellArea::focus-changed] signal to fire; as well as
--  [signalGtk.CellArea::add-editable] in the case that an editable cell was
--  clicked and needs to start editing. You can call
--  [methodGtk.CellArea.stop_editing] at any time to cancel any cell editing
--  that is currently in progress.
--
--  The `GtkCellArea` drives keyboard focus from cell to cell in a way similar
--  to `GtkWidget`. For layouting widgets that support giving focus to cells
--  it's important to remember to pass `GTK_CELL_RENDERER_FOCUSED` to the area
--  functions for the row that has focus and to tell the area to paint the
--  focus at render time.
--
--  Layouting widgets that accept focus on cells should implement the
--  [vfuncGtk.Widget.focus] virtual method. The layouting widget is always
--  responsible for knowing where `GtkTreeModel` rows are rendered inside the
--  widget, so at [vfuncGtk.Widget.focus] time the layouting widget should use
--  the `GtkCellArea` methods to navigate focus inside the area and then
--  observe the [enumGtk.DirectionType] to pass the focus to adjacent rows and
--  areas.
--
--  A basic example of how the [vfuncGtk.Widget.focus] virtual method should
--  be implemented:
--
--  ``` static gboolean foo_focus (GtkWidget *widget, GtkDirectionType
--  direction) { Foo *self = FOO (widget); FooPrivate *priv =
--  foo_get_instance_private (self); int focus_row = priv->focus_row; gboolean
--  have_focus = FALSE;
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
--  (priv->model, &iter); } } } } return have_focus; } ```
--
--  Note that the layouting widget is responsible for matching the
--  `GtkDirectionType` values to the way it lays out its cells.
--
--  ## Cell Properties
--
--  The `GtkCellArea` introduces cell properties for `GtkCellRenderer`s. This
--  provides some general interfaces for defining the relationship cell areas
--  have with their cells. For instance in a [classGtk.CellAreaBox] a cell
--  might "expand" and receive extra space when the area is allocated more than
--  its full natural request, or a cell might be configured to "align" with
--  adjacent rows which were requested and rendered with the same
--  `GtkCellAreaContext`.
--
--  Use [methodGtk.CellAreaClass.install_cell_property] to install cell
--  properties for a cell area class and
--  [methodGtk.CellAreaClass.find_cell_property] or
--  [methodGtk.CellAreaClass.list_cell_properties] to get information about
--  existing cell properties.
--
--  To set the value of a cell property, use
--  [methodGtk.CellArea.cell_set_property], [methodGtk.CellArea.cell_set] or
--  [methodGtk.CellArea.cell_set_valist]. To obtain the value of a cell
--  property, use [methodGtk.CellArea.cell_get_property]
--  [methodGtk.CellArea.cell_get] or [methodGtk.CellArea.cell_get_valist].
--
--  <group>Layout Containers</group>

pragma Warnings (Off, "*is already use-visible*");
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
   --  renderers of a `GtkCellArea`, see Gtk.Cell_Area.Foreach.
   --  @param Renderer the cell renderer to operate on
   --  @return True to stop iterating over cells.

   type Gtk_Cell_Alloc_Callback is access function
     (Renderer        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Background : Gdk.Rectangle.Gdk_Rectangle) return Boolean;
   --  The type of the callback functions used for iterating over the cell
   --  renderers and their allocated areas inside a `GtkCellArea`, see
   --  Gtk.Cell_Area.Foreach_Alloc.
   --  @param Renderer the cell renderer to operate on
   --  @param Cell_Area the area allocated to Renderer inside the rectangle
   --  provided to Gtk.Cell_Area.Foreach_Alloc.
   --  @param Cell_Background the background area for Renderer inside the
   --  background area provided to Gtk.Cell_Area.Foreach_Alloc.
   --  @return True to stop iterating over cells.

   type Gtk_Cell_Layout_Data_Func is access procedure
     (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  A function which should set the value of Cell_Layout's cell renderer(s)
   --  as appropriate.
   --  @param Cell_Layout a `GtkCellLayout`
   --  @param Cell the cell renderer whose value is to be set
   --  @param Tree_Model the model
   --  @param Iter a `GtkTreeIter` indicating the row to set the value for

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
   pragma Obsolescent (Activate);
   --  Activates Area, usually by activating the currently focused cell,
   --  however some subclasses which embed widgets in the area can also
   --  activate a widget if it currently has the focus.
   --  Deprecated since 4.10, 1
   --  @param Context the `GtkCellArea`Context in context with the current row
   --  data
   --  @param Widget the `GtkWidget` that Area is rendering on
   --  @param Cell_Area the size and location of Area relative to Widget's
   --  allocation
   --  @param Flags the `GtkCellRenderer`State flags for Area for this row of
   --  data.
   --  @param Edit_Only if True then only cell renderers that are
   --  Gtk.Cell_Renderer.Cell_Renderer_Mode_Editable will be activated.
   --  @return Whether Area was successfully activated.

   procedure Add
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   pragma Obsolescent (Add);
   --  Adds Renderer to Area with the default child cell properties.
   --  Deprecated since 4.10, 1
   --  @param Renderer the `GtkCellRenderer` to add to Area

   procedure Add_Focus_Sibling
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Sibling  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   pragma Obsolescent (Add_Focus_Sibling);
   --  Adds Sibling to Renderer's focusable area, focus will be drawn around
   --  Renderer and all of its siblings if Renderer can focus for a given row.
   --  Events handled by focus siblings can also activate the given focusable
   --  Renderer.
   --  Deprecated since 4.10, 1
   --  @param Renderer the `GtkCellRenderer` expected to have focus
   --  @param Sibling the `GtkCellRenderer` to add to Renderer's focus area

   procedure Apply_Attributes
      (Self        : not null access Gtk_Cell_Area_Record;
       Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
       Is_Expander : Boolean;
       Is_Expanded : Boolean);
   pragma Obsolescent (Apply_Attributes);
   --  Applies any connected attributes to the renderers in Area by pulling
   --  the values from Tree_Model.
   --  Deprecated since 4.10, 1
   --  @param Tree_Model the `GtkTreeModel` to pull values from
   --  @param Iter the `GtkTreeIter` in Tree_Model to apply values for
   --  @param Is_Expander whether Iter has children
   --  @param Is_Expanded whether Iter is expanded in the view and children
   --  are visible

   procedure Attribute_Connect
      (Self      : not null access Gtk_Cell_Area_Record;
       Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String;
       Column    : Glib.Gint);
   pragma Obsolescent (Attribute_Connect);
   --  Connects an Attribute to apply values from Column for the
   --  `GtkTreeModel` in use.
   --  Deprecated since 4.10, 1
   --  @param Renderer the `GtkCellRenderer` to connect an attribute for
   --  @param Attribute the attribute name
   --  @param Column the `GtkTreeModel` column to fetch attribute values from

   procedure Attribute_Disconnect
      (Self      : not null access Gtk_Cell_Area_Record;
       Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String);
   pragma Obsolescent (Attribute_Disconnect);
   --  Disconnects Attribute for the Renderer in Area so that attribute will
   --  no longer be updated with values from the model.
   --  Deprecated since 4.10, 1
   --  @param Renderer the `GtkCellRenderer` to disconnect an attribute for
   --  @param Attribute the attribute name

   function Attribute_Get_Column
      (Self      : not null access Gtk_Cell_Area_Record;
       Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String) return Glib.Gint;
   pragma Obsolescent (Attribute_Get_Column);
   --  Returns the model column that an attribute has been mapped to, or -1 if
   --  the attribute is not mapped.
   --  Deprecated since 4.10, 1
   --  @param Renderer a `GtkCellRenderer`
   --  @param Attribute an attribute on the renderer
   --  @return the model column, or -1

   procedure Cell_Get_Property
      (Self          : not null access Gtk_Cell_Area_Record;
       Renderer      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue);
   pragma Obsolescent (Cell_Get_Property);
   --  Gets the value of a cell property for Renderer in Area.
   --  Deprecated since 4.10, 1
   --  @param Renderer a `GtkCellRenderer` inside Area
   --  @param Property_Name the name of the property to get
   --  @param Value a location to return the value

   procedure Cell_Set_Property
      (Self          : not null access Gtk_Cell_Area_Record;
       Renderer      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue);
   pragma Obsolescent (Cell_Set_Property);
   --  Sets a cell property for Renderer in Area.
   --  Deprecated since 4.10, 1
   --  @param Renderer a `GtkCellRenderer` inside Area
   --  @param Property_Name the name of the cell property to set
   --  @param Value the value to set the cell property to

   function Copy_Context
      (Self    : not null access Gtk_Cell_Area_Record;
       Context : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class)
       return Gtk.Cell_Area_Context.Gtk_Cell_Area_Context;
   pragma Obsolescent (Copy_Context);
   --  This is sometimes needed for cases where rows need to share alignments
   --  in one orientation but may be separately grouped in the opposing
   --  orientation.
   --  For instance, `GtkIconView` creates all icons (rows) to have the same
   --  width and the cells theirin to have the same horizontal alignments.
   --  However each row of icons may have a separate collective height.
   --  `GtkIconView` uses this to request the heights of each row based on a
   --  context which was already used to request all the row widths that are to
   --  be displayed.
   --  Deprecated since 4.10, 1
   --  @param Context the `GtkCellArea`Context to copy
   --  @return a newly created `GtkCellArea`Context copy of Context.

   function Create_Context
      (Self : not null access Gtk_Cell_Area_Record)
       return Gtk.Cell_Area_Context.Gtk_Cell_Area_Context;
   pragma Obsolescent (Create_Context);
   --  Creates a `GtkCellArea`Context to be used with Area for all purposes.
   --  `GtkCellArea`Context stores geometry information for rows for which it
   --  was operated on, it is important to use the same context for the same
   --  row of data at all times (i.e. one should render and handle events with
   --  the same `GtkCellArea`Context which was used to request the size of
   --  those rows of data).
   --  Deprecated since 4.10, 1
   --  @return a newly created `GtkCellArea`Context which can be used with
   --  Area.

   function Focus
      (Self      : not null access Gtk_Cell_Area_Record;
       Direction : Gtk.Enums.Gtk_Direction_Type) return Boolean;
   pragma Obsolescent (Focus);
   --  This should be called by the Area's owning layout widget when focus is
   --  to be passed to Area, or moved within Area for a given Direction and row
   --  data.
   --  Implementing `GtkCellArea` classes should implement this method to
   --  receive and navigate focus in its own way particular to how it lays out
   --  cells.
   --  Deprecated since 4.10, 1
   --  @param Direction the `GtkDirectionType`
   --  @return True if focus remains inside Area as a result of this call.

   procedure Foreach
      (Self     : not null access Gtk_Cell_Area_Record;
       Callback : Gtk_Cell_Callback);
   pragma Obsolescent (Foreach);
   --  Calls Callback for every `GtkCellRenderer` in Area.
   --  Deprecated since 4.10, 1
   --  @param Callback the `GtkCellCallback` to call

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Foreach_User_Data is

      type Gtk_Cell_Callback is access function
        (Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Data     : User_Data_Type) return Boolean;
      --  The type of the callback functions used for iterating over the cell
      --  renderers of a `GtkCellArea`, see Gtk.Cell_Area.Foreach.
      --  @param Renderer the cell renderer to operate on
      --  @param Data user-supplied data
      --  @return True to stop iterating over cells.

      procedure Foreach
         (Self          : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
          Callback      : Gtk_Cell_Callback;
          Callback_Data : User_Data_Type);
      pragma Obsolescent (Foreach);
      --  Calls Callback for every `GtkCellRenderer` in Area.
      --  Deprecated since 4.10, 1
      --  @param Callback the `GtkCellCallback` to call
      --  @param Callback_Data user provided data pointer

   end Foreach_User_Data;

   procedure Foreach_Alloc
      (Self            : not null access Gtk_Cell_Area_Record;
       Context         : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Callback        : Gtk_Cell_Alloc_Callback);
   --  Calls Callback for every `GtkCellRenderer` in Area with the allocated
   --  rectangle inside Cell_Area.
   --  @param Context the `GtkCellArea`Context for this row of data.
   --  @param Widget the `GtkWidget` that Area is rendering to
   --  @param Cell_Area the Widget relative coordinates and size for Area
   --  @param Background_Area the Widget relative coordinates of the
   --  background area
   --  @param Callback the `GtkCellAllocCallback` to call

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
      --  renderers and their allocated areas inside a `GtkCellArea`, see
      --  Gtk.Cell_Area.Foreach_Alloc.
      --  @param Renderer the cell renderer to operate on
      --  @param Cell_Area the area allocated to Renderer inside the rectangle
      --  provided to Gtk.Cell_Area.Foreach_Alloc.
      --  @param Cell_Background the background area for Renderer inside the
      --  background area provided to Gtk.Cell_Area.Foreach_Alloc.
      --  @param Data user-supplied data
      --  @return True to stop iterating over cells.

      procedure Foreach_Alloc
         (Self            : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
          Context         : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
          Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
          Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
          Background_Area : Gdk.Rectangle.Gdk_Rectangle;
          Callback        : Gtk_Cell_Alloc_Callback;
          Callback_Data   : User_Data_Type);
      --  Calls Callback for every `GtkCellRenderer` in Area with the
      --  allocated rectangle inside Cell_Area.
      --  @param Context the `GtkCellArea`Context for this row of data.
      --  @param Widget the `GtkWidget` that Area is rendering to
      --  @param Cell_Area the Widget relative coordinates and size for Area
      --  @param Background_Area the Widget relative coordinates of the
      --  background area
      --  @param Callback the `GtkCellAllocCallback` to call
      --  @param Callback_Data user provided data pointer

   end Foreach_Alloc_User_Data;

   procedure Get_Cell_Allocation
      (Self       : not null access Gtk_Cell_Area_Record;
       Context    : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Renderer   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Cell_Area  : access Gdk.Rectangle.Gdk_Rectangle;
       Allocation : access Gdk.Rectangle.Gdk_Rectangle);
   pragma Obsolescent (Get_Cell_Allocation);
   --  Derives the allocation of Renderer inside Area if Area were to be
   --  rendered in Cell_Area.
   --  Deprecated since 4.10, 1
   --  @param Context the `GtkCellArea`Context used to hold sizes for Area.
   --  @param Widget the `GtkWidget` that Area is rendering on
   --  @param Renderer the `GtkCellRenderer` to get the allocation for
   --  @param Cell_Area the whole allocated area for Area in Widget for this
   --  row
   --  @param Allocation where to store the allocation for Renderer

   function Get_Current_Path_String
      (Self : not null access Gtk_Cell_Area_Record) return UTF8_String;
   --  Gets the current `GtkTreePath` string for the currently applied
   --  `GtkTreeIter`, this is implicitly updated when
   --  Gtk.Cell_Area.Apply_Attributes is called and can be used to interact
   --  with renderers from `GtkCellArea` subclasses.
   --  @return The current `GtkTreePath` string for the current attributes
   --  applied to Area. This string belongs to the area and should not be
   --  freed.

   function Get_Edit_Widget
      (Self : not null access Gtk_Cell_Area_Record)
       return Gtk.Cell_Editable.Gtk_Cell_Editable;
   pragma Obsolescent (Get_Edit_Widget);
   --  Gets the `GtkCellEditable` widget currently used to edit the currently
   --  edited cell.
   --  Deprecated since 4.10, 1
   --  @return The currently active `GtkCellEditable` widget

   function Get_Edited_Cell
      (Self : not null access Gtk_Cell_Area_Record)
       return Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   pragma Obsolescent (Get_Edited_Cell);
   --  Gets the `GtkCellRenderer` in Area that is currently being edited.
   --  Deprecated since 4.10, 1
   --  @return The currently edited `GtkCellRenderer`

   function Get_Focus_Cell
      (Self : not null access Gtk_Cell_Area_Record)
       return Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   pragma Obsolescent (Get_Focus_Cell);
   --  Retrieves the currently focused cell for Area
   --  Deprecated since 4.10, 1
   --  @return the currently focused cell in Area.

   procedure Set_Focus_Cell
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   pragma Obsolescent (Set_Focus_Cell);
   --  Explicitly sets the currently focused cell to Renderer.
   --  This is generally called by implementations of `GtkCellAreaClass.focus`
   --  or `GtkCellAreaClass.event`, however it can also be used to implement
   --  functions such as gtk_tree_view_set_cursor_on_cell.
   --  Deprecated since 4.10, 1
   --  @param Renderer the `GtkCellRenderer` to give focus to

   function Get_Focus_From_Sibling
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   pragma Obsolescent (Get_Focus_From_Sibling);
   --  Gets the `GtkCellRenderer` which is expected to be focusable for which
   --  Renderer is, or may be a sibling.
   --  This is handy for `GtkCellArea` subclasses when handling events, after
   --  determining the renderer at the event location it can then chose to
   --  activate the focus cell for which the event cell may have been a
   --  sibling.
   --  Deprecated since 4.10, 1
   --  @param Renderer the `GtkCellRenderer`
   --  @return the `GtkCellRenderer` for which Renderer is a sibling

   function Get_Focus_Siblings
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Glib.Object.Object_Simple_List.Glist;
   pragma Obsolescent (Get_Focus_Siblings);
   --  Gets the focus sibling cell renderers for Renderer.
   --  Deprecated since 4.10, 1
   --  @param Renderer the `GtkCellRenderer` expected to have focus

   procedure Get_Preferred_Height
      (Self           : not null access Gtk_Cell_Area_Record;
       Context        : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint);
   pragma Obsolescent (Get_Preferred_Height);
   --  Retrieves a cell area's initial minimum and natural height.
   --  Area will store some geometrical information in Context along the way;
   --  when requesting sizes over an arbitrary number of rows, it's not
   --  important to check the Minimum_Height and Natural_Height of this call
   --  but rather to consult Gtk.Cell_Area_Context.Get_Preferred_Height after a
   --  series of requests.
   --  Deprecated since 4.10, 1
   --  @param Context the `GtkCellArea`Context to perform this request with
   --  @param Widget the `GtkWidget` where Area will be rendering
   --  @param Minimum_Height location to store the minimum height
   --  @param Natural_Height location to store the natural height

   procedure Get_Preferred_Height_For_Width
      (Self           : not null access Gtk_Cell_Area_Record;
       Context        : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Width          : Glib.Gint;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint);
   pragma Obsolescent (Get_Preferred_Height_For_Width);
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
   --  Deprecated since 4.10, 1
   --  @param Context the `GtkCellArea`Context which has already been
   --  requested for widths.
   --  @param Widget the `GtkWidget` where Area will be rendering
   --  @param Width the width for which to check the height of this area
   --  @param Minimum_Height location to store the minimum height
   --  @param Natural_Height location to store the natural height

   procedure Get_Preferred_Width
      (Self          : not null access Gtk_Cell_Area_Record;
       Context       : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint);
   pragma Obsolescent (Get_Preferred_Width);
   --  Retrieves a cell area's initial minimum and natural width.
   --  Area will store some geometrical information in Context along the way;
   --  when requesting sizes over an arbitrary number of rows, it's not
   --  important to check the Minimum_Width and Natural_Width of this call but
   --  rather to consult Gtk.Cell_Area_Context.Get_Preferred_Width after a
   --  series of requests.
   --  Deprecated since 4.10, 1
   --  @param Context the `GtkCellArea`Context to perform this request with
   --  @param Widget the `GtkWidget` where Area will be rendering
   --  @param Minimum_Width location to store the minimum width
   --  @param Natural_Width location to store the natural width

   procedure Get_Preferred_Width_For_Height
      (Self          : not null access Gtk_Cell_Area_Record;
       Context       : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Height        : Glib.Gint;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint);
   pragma Obsolescent (Get_Preferred_Width_For_Height);
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
   --  Deprecated since 4.10, 1
   --  @param Context the `GtkCellArea`Context which has already been
   --  requested for widths.
   --  @param Widget the `GtkWidget` where Area will be rendering
   --  @param Height the height for which to check the width of this area
   --  @param Minimum_Width location to store the minimum width
   --  @param Natural_Width location to store the natural width

   function Get_Request_Mode
      (Self : not null access Gtk_Cell_Area_Record)
       return Gtk.Enums.Gtk_Size_Request_Mode;
   --  Gets whether the area prefers a height-for-width layout or a
   --  width-for-height layout.
   --  @return The `GtkSizeRequestMode` preferred by Area.

   function Has_Renderer
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Boolean;
   pragma Obsolescent (Has_Renderer);
   --  Checks if Area contains Renderer.
   --  Deprecated since 4.10, 1
   --  @param Renderer the `GtkCellRenderer` to check
   --  @return True if Renderer is in the Area.

   procedure Inner_Cell_Area
      (Self       : not null access Gtk_Cell_Area_Record;
       Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_Area  : Gdk.Rectangle.Gdk_Rectangle;
       Inner_Area : out Gdk.Rectangle.Gdk_Rectangle);
   pragma Obsolescent (Inner_Cell_Area);
   --  This is a convenience function for `GtkCellArea` implementations to get
   --  the inner area where a given `GtkCellRenderer` will be rendered. It
   --  removes any padding previously added by Gtk.Cell_Area.Request_Renderer.
   --  Deprecated since 4.10, 1
   --  @param Widget the `GtkWidget` that Area is rendering onto
   --  @param Cell_Area the Widget relative coordinates where one of Area's
   --  cells is to be placed
   --  @param Inner_Area the return location for the inner cell area

   function Is_Activatable
      (Self : not null access Gtk_Cell_Area_Record) return Boolean;
   pragma Obsolescent (Is_Activatable);
   --  Returns whether the area can do anything when activated, after applying
   --  new attributes to Area.
   --  Deprecated since 4.10, 1
   --  @return whether Area can do anything when activated.

   function Is_Focus_Sibling
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Sibling  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Boolean;
   pragma Obsolescent (Is_Focus_Sibling);
   --  Returns whether Sibling is one of Renderer's focus siblings (see
   --  Gtk.Cell_Area.Add_Focus_Sibling).
   --  Deprecated since 4.10, 1
   --  @param Renderer the `GtkCellRenderer` expected to have focus
   --  @param Sibling the `GtkCellRenderer` to check against Renderer's
   --  sibling list
   --  @return True if Sibling is a focus sibling of Renderer

   procedure Remove
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   pragma Obsolescent (Remove);
   --  Removes Renderer from Area.
   --  Deprecated since 4.10, 1
   --  @param Renderer the `GtkCellRenderer` to remove from Area

   procedure Remove_Focus_Sibling
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Sibling  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   pragma Obsolescent (Remove_Focus_Sibling);
   --  Removes Sibling from Renderer's focus sibling list (see
   --  Gtk.Cell_Area.Add_Focus_Sibling).
   --  Deprecated since 4.10, 1
   --  @param Renderer the `GtkCellRenderer` expected to have focus
   --  @param Sibling the `GtkCellRenderer` to remove from Renderer's focus
   --  area

   procedure Request_Renderer
      (Self         : not null access Gtk_Cell_Area_Record;
       Renderer     : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Orientation  : Gtk.Enums.Gtk_Orientation;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       For_Size     : Glib.Gint;
       Minimum_Size : out Glib.Gint;
       Natural_Size : out Glib.Gint);
   pragma Obsolescent (Request_Renderer);
   --  This is a convenience function for `GtkCellArea` implementations to
   --  request size for cell renderers. It's important to use this function to
   --  request size and then use Gtk.Cell_Area.Inner_Cell_Area at render and
   --  event time since this function will add padding around the cell for
   --  focus painting.
   --  Deprecated since 4.10, 1
   --  @param Renderer the `GtkCellRenderer` to request size for
   --  @param Orientation the `GtkOrientation` in which to request size
   --  @param Widget the `GtkWidget` that Area is rendering onto
   --  @param For_Size the allocation contextual size to request for, or -1 if
   --  the base request for the orientation is to be returned.
   --  @param Minimum_Size location to store the minimum size
   --  @param Natural_Size location to store the natural size

   procedure Stop_Editing
      (Self     : not null access Gtk_Cell_Area_Record;
       Canceled : Boolean);
   pragma Obsolescent (Stop_Editing);
   --  Explicitly stops the editing of the currently edited cell.
   --  If Canceled is True, the currently edited cell renderer will emit the
   --  ::editing-canceled signal, otherwise the the ::editing-done signal will
   --  be emitted on the current edit widget.
   --  See Gtk.Cell_Area.Get_Edited_Cell and Gtk.Cell_Area.Get_Edit_Widget.
   --  Deprecated since 4.10, 1
   --  @param Canceled whether editing was canceled.

   procedure Set_Cell_Data_Func
      (Self : not null access Gtk_Cell_Area_Record;
       Cell : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func : Gtk_Cell_Layout_Data_Func);
   pragma Obsolescent (Set_Cell_Data_Func);
   --  Sets the `GtkCellLayout`DataFunc to use for Cell_Layout.
   --  This function is used instead of the standard attributes mapping for
   --  setting the column value, and should set the value of Cell_Layout's cell
   --  renderer(s) as appropriate.
   --  Func may be null to remove a previously set function.
   --  Deprecated since 4.10, 1
   --  @param Cell a `GtkCellRenderer`
   --  @param Func the `GtkCellLayout`DataFunc to use

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
      --  @param Cell_Layout a `GtkCellLayout`
      --  @param Cell the cell renderer whose value is to be set
      --  @param Tree_Model the model
      --  @param Iter a `GtkTreeIter` indicating the row to set the value for
      --  @param Data user data passed to Gtk.Cell_Layout.Set_Cell_Data_Func

      procedure Set_Cell_Data_Func
         (Self      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
          Cell      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func      : Gtk_Cell_Layout_Data_Func;
          Func_Data : User_Data_Type);
      pragma Obsolescent (Set_Cell_Data_Func);
      --  Sets the `GtkCellLayout`DataFunc to use for Cell_Layout.
      --  This function is used instead of the standard attributes mapping for
      --  setting the column value, and should set the value of Cell_Layout's
      --  cell renderer(s) as appropriate.
      --  Func may be null to remove a previously set function.
      --  Deprecated since 4.10, 1
      --  @param Cell a `GtkCellRenderer`
      --  @param Func the `GtkCellLayout`DataFunc to use
      --  @param Func_Data user data for Func

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
      (Self      : not null access Gtk_Cell_Area_Record;
       Cell      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String;
       Column    : Glib.Gint);
   pragma Obsolescent (Add_Attribute);

   procedure Clear (Self : not null access Gtk_Cell_Area_Record);
   pragma Obsolescent (Clear);

   procedure Clear_Attributes
      (Self : not null access Gtk_Cell_Area_Record;
       Cell : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   pragma Obsolescent (Clear_Attributes);

   function Get_Cells
      (Self : not null access Gtk_Cell_Area_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
   pragma Obsolescent (Get_Cells);

   procedure Pack_End
      (Self   : not null access Gtk_Cell_Area_Record;
       Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand : Boolean);
   pragma Obsolescent (Pack_End);

   procedure Pack_Start
      (Self   : not null access Gtk_Cell_Area_Record;
       Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand : Boolean);
   pragma Obsolescent (Pack_Start);

   procedure Reorder
      (Self     : not null access Gtk_Cell_Area_Record;
       Cell     : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position : Glib.Gint);
   pragma Obsolescent (Reorder);

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
   --  gtk_cell_area_activate_cell.

   Edited_Cell_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  The cell in the area that is currently edited
   --
   --  This property is read-only and only changes as a result of a call
   --  gtk_cell_area_activate_cell.

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
   --    --  @param Renderer the `GtkCellRenderer` that started the edited
   --    --  @param Editable the `GtkCellEditable` widget to add
   --    --  @param Cell_Area the `GtkWidget` relative `GdkRectangle` coordinates
   --    --  where Editable should be added
   --    --  @param Path the `GtkTreePath` string this edit was initiated for

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
   --    --  @param Model the `GtkTreeModel` to apply the attributes from
   --    --  @param Iter the `GtkTreeIter` indicating which row to apply the
   --    --  attributes of
   --    --  @param Is_Expander whether the view shows children for this row
   --    --  @param Is_Expanded whether the view is currently showing the children
   --    --  of this row

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
   --    --  @param Renderer the `GtkCellRenderer` that has focus
   --    --  @param Path the current `GtkTreePath` string set for Area

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
   --    --  @param Renderer the `GtkCellRenderer` that finished editeding
   --    --  @param Editable the `GtkCellEditable` widget to remove

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.CellLayout"

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
