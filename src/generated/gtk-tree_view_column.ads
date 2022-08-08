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
--  The GtkTreeViewColumn object represents a visible column in a
--  Gtk.Tree_View.Gtk_Tree_View widget. It allows to set properties of the
--  column header, and functions as a holding pen for the cell renderers which
--  determine how the data in the column is displayed.
--
--  Please refer to the [tree widget conceptual overview][TreeWidget] for an
--  overview of all the objects and data types related to the tree widget and
--  how they work together.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Rectangle;           use Gdk.Rectangle;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Glist;              use Glib.Glist;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Cell_Area;           use Gtk.Cell_Area;
with Gtk.Cell_Layout;         use Gtk.Cell_Layout;
with Gtk.Cell_Renderer;       use Gtk.Cell_Renderer;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Tree_Model;          use Gtk.Tree_Model;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Tree_View_Column is

   type Gtk_Tree_View_Column_Record is new GObject_Record with null record;
   type Gtk_Tree_View_Column is access all Gtk_Tree_View_Column_Record'Class;

   type Gtk_Tree_View_Column_Sizing is (
      Tree_View_Column_Grow_Only,
      Tree_View_Column_Autosize,
      Tree_View_Column_Fixed);
   pragma Convention (C, Gtk_Tree_View_Column_Sizing);
   --  The sizing method the column uses to determine its width. Please note
   --  that Gtk_Tree_View_Column_Autosize are inefficient for large views, and
   --  can make columns appear choppy.

   function Convert (R : Gtk.Tree_View_Column.Gtk_Tree_View_Column) return System.Address;
   function Convert (R : System.Address) return Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   package Column_List is new Generic_List (Gtk.Tree_View_Column.Gtk_Tree_View_Column);

   ---------------
   -- Callbacks --
   ---------------

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

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Tree_View_Column_Sizing_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Tree_View_Column_Sizing);
   type Property_Gtk_Tree_View_Column_Sizing is new Gtk_Tree_View_Column_Sizing_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Tree_Column : out Gtk_Tree_View_Column);
   procedure Initialize
      (Tree_Column : not null access Gtk_Tree_View_Column_Record'Class);
   --  Creates a new Gtk.Tree_View_Column.Gtk_Tree_View_Column.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Tree_View_Column_New return Gtk_Tree_View_Column;
   --  Creates a new Gtk.Tree_View_Column.Gtk_Tree_View_Column.

   procedure Gtk_New_With_Area
      (Tree_Column : out Gtk_Tree_View_Column;
       Area        : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class);
   procedure Initialize_With_Area
      (Tree_Column : not null access Gtk_Tree_View_Column_Record'Class;
       Area        : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class);
   --  Creates a new Gtk.Tree_View_Column.Gtk_Tree_View_Column using Area to
   --  render its cells.
   --  Since: gtk+ 3.0
   --  Initialize_With_Area does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "area": the Gtk.Cell_Area.Gtk_Cell_Area that the newly created column
   --  should use to layout cells.

   function Gtk_Tree_View_Column_New_With_Area
      (Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
       return Gtk_Tree_View_Column;
   --  Creates a new Gtk.Tree_View_Column.Gtk_Tree_View_Column using Area to
   --  render its cells.
   --  Since: gtk+ 3.0
   --  "area": the Gtk.Cell_Area.Gtk_Cell_Area that the newly created column
   --  should use to layout cells.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_view_column_get_type");

   -------------
   -- Methods --
   -------------

   procedure Cell_Get_Position
      (Tree_Column   : not null access Gtk_Tree_View_Column_Record;
       Cell_Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       X_Offset      : out Glib.Gint;
       Width         : out Glib.Gint;
       Success       : out Boolean);
   --  Obtains the horizontal position and size of a cell in a column. If the
   --  cell is not found in the column, Start_Pos and Width are not changed and
   --  False is returned.
   --  "cell_renderer": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  "x_offset": return location for the horizontal position of Cell within
   --  Tree_Column, may be null
   --  "width": return location for the width of Cell, may be null

   procedure Cell_Get_Size
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Cell_Area   : Gdk.Rectangle.Gdk_Rectangle;
       X_Offset    : out Glib.Gint;
       Y_Offset    : out Glib.Gint;
       Width       : out Glib.Gint;
       Height      : out Glib.Gint);
   --  Obtains the width and height needed to render the column. This is used
   --  primarily by the Gtk.Tree_View.Gtk_Tree_View.
   --  "cell_area": The area a cell in the column will be allocated, or null
   --  "x_offset": location to return x offset of a cell relative to
   --  Cell_Area, or null
   --  "y_offset": location to return y offset of a cell relative to
   --  Cell_Area, or null
   --  "width": location to return width needed to render a cell, or null
   --  "height": location to return height needed to render a cell, or null

   function Cell_Is_Visible
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Boolean;
   --  Returns True if any of the cells packed into the Tree_Column are
   --  visible. For this to be meaningful, you must first initialize the cells
   --  with Gtk.Tree_View_Column.Cell_Set_Cell_Data

   procedure Cell_Set_Cell_Data
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
       Is_Expander : Boolean;
       Is_Expanded : Boolean);
   --  Sets the cell renderer based on the Tree_Model and Iter. That is, for
   --  every attribute mapping in Tree_Column, it will get a value from the set
   --  column on the Iter, and use that value to set the attribute on the cell
   --  renderer. This is used primarily by the Gtk.Tree_View.Gtk_Tree_View.
   --  "tree_model": The Gtk.Tree_Model.Gtk_Tree_Model to to get the cell
   --  renderers attributes from.
   --  "iter": The Gtk.Tree_Model.Gtk_Tree_Iter to to get the cell renderer's
   --  attributes from.
   --  "is_expander": True, if the row has children
   --  "is_expanded": True, if the row has visible children

   procedure Clicked
      (Tree_Column : not null access Gtk_Tree_View_Column_Record);
   --  Emits the "clicked" signal on the column. This function will only work
   --  if Tree_Column is clickable.

   procedure Focus_Cell
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Sets the current keyboard focus to be at Cell, if the column contains 2
   --  or more editable and activatable cells.
   --  Since: gtk+ 2.2
   --  "cell": A Gtk.Cell_Renderer.Gtk_Cell_Renderer

   function Get_Alignment
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Gfloat;
   --  Returns the current x alignment of Tree_Column. This value can range
   --  between 0.0 and 1.0.

   procedure Set_Alignment
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Xalign      : Gfloat);
   --  Sets the alignment of the title or custom widget inside the column
   --  header. The alignment determines its location inside the button -- 0.0
   --  for left, 0.5 for center, 1.0 for right.
   --  "xalign": The alignment, which is between [0.0 and 1.0] inclusive.

   function Get_Button
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the button used in the treeview column header
   --  Since: gtk+ 3.0

   function Get_Clickable
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Boolean;
   --  Returns True if the user can click on the header for the column.

   procedure Set_Clickable
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Clickable   : Boolean);
   --  Sets the header to be active if Clickable is True. When the header is
   --  active, then it can take keyboard focus, and can be clicked.
   --  "clickable": True if the header is active.

   function Get_Expand
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Boolean;
   --  Returns True if the column expands to fill available space.
   --  Since: gtk+ 2.4

   procedure Set_Expand
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Expand      : Boolean);
   --  Sets the column to take available extra space. This space is shared
   --  equally amongst all columns that have the expand set to True. If no
   --  column has this option set, then the last column gets all extra space.
   --  By default, every column is created with this False.
   --  Along with "fixed-width", the "expand" property changes when the column
   --  is resized by the user.
   --  Since: gtk+ 2.4
   --  "expand": True if the column should expand to fill available space.

   function Get_Fixed_Width
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Glib.Gint;
   --  Gets the fixed width of the column. This may not be the actual
   --  displayed width of the column; for that, use
   --  Gtk.Tree_View_Column.Get_Width.

   procedure Set_Fixed_Width
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Fixed_Width : Glib.Gint);
   --  If Fixed_Width is not -1, sets the fixed width of Tree_Column;
   --  otherwise unsets it. The effective value of Fixed_Width is clamped
   --  between the minimum and maximum width of the column; however, the value
   --  stored in the "fixed-width" property is not clamped. If the column
   --  sizing is GTK_TREE_VIEW_COLUMN_GROW_ONLY or
   --  GTK_TREE_VIEW_COLUMN_AUTOSIZE, setting a fixed width overrides the
   --  automatically calculated width. Note that Fixed_Width is only a hint to
   --  GTK+; the width actually allocated to the column may be greater or less
   --  than requested.
   --  Along with "expand", the "fixed-width" property changes when the column
   --  is resized by the user.
   --  "fixed_width": The new fixed width, in pixels, or -1.

   function Get_Max_Width
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Glib.Gint;
   --  Returns the maximum width in pixels of the Tree_Column, or -1 if no
   --  maximum width is set.

   procedure Set_Max_Width
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Max_Width   : Glib.Gint);
   --  Sets the maximum width of the Tree_Column. If Max_Width is -1, then the
   --  maximum width is unset. Note, the column can actually be wider than max
   --  width if it's the last column in a view. In this case, the column
   --  expands to fill any extra space.
   --  "max_width": The maximum width of the column in pixels, or -1.

   function Get_Min_Width
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Glib.Gint;
   --  Returns the minimum width in pixels of the Tree_Column, or -1 if no
   --  minimum width is set.

   procedure Set_Min_Width
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Min_Width   : Glib.Gint);
   --  Sets the minimum width of the Tree_Column. If Min_Width is -1, then the
   --  minimum width is unset.
   --  "min_width": The minimum width of the column in pixels, or -1.

   function Get_Reorderable
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Boolean;
   --  Returns True if the Tree_Column can be reordered by the user.

   procedure Set_Reorderable
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Reorderable : Boolean);
   --  If Reorderable is True, then the column can be reordered by the end
   --  user dragging the header.
   --  "reorderable": True, if the column can be reordered.

   function Get_Resizable
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Boolean;
   --  Returns True if the Tree_Column can be resized by the end user.

   procedure Set_Resizable
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Resizable   : Boolean);
   --  If Resizable is True, then the user can explicitly resize the column by
   --  grabbing the outer edge of the column button. If resizable is True and
   --  sizing mode of the column is GTK_TREE_VIEW_COLUMN_AUTOSIZE, then the
   --  sizing mode is changed to GTK_TREE_VIEW_COLUMN_GROW_ONLY.
   --  "resizable": True, if the column can be resized

   function Get_Sizing
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Gtk_Tree_View_Column_Sizing;
   --  Returns the current type of Tree_Column.

   procedure Set_Sizing
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       The_Type    : Gtk_Tree_View_Column_Sizing);
   --  Sets the growth behavior of Tree_Column to Type.
   --  "type": The Gtk.Tree_View_Column.Gtk_Tree_View_Column_Sizing.

   function Get_Sort_Column_Id
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Glib.Gint;
   --  Gets the logical Sort_Column_Id that the model sorts on when this
   --  column is selected for sorting. See
   --  Gtk.Tree_View_Column.Set_Sort_Column_Id.

   procedure Set_Sort_Column_Id
      (Tree_Column    : not null access Gtk_Tree_View_Column_Record;
       Sort_Column_Id : Glib.Gint);
   --  Sets the logical Sort_Column_Id that this column sorts on when this
   --  column is selected for sorting. Doing so makes the column header
   --  clickable.
   --  "sort_column_id": The Sort_Column_Id of the model to sort on.

   function Get_Sort_Indicator
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Boolean;
   --  Gets the value set by Gtk.Tree_View_Column.Set_Sort_Indicator.

   procedure Set_Sort_Indicator
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Setting     : Boolean);
   --  Call this function with a Setting of True to display an arrow in the
   --  header button indicating the column is sorted. Call
   --  Gtk.Tree_View_Column.Set_Sort_Order to change the direction of the
   --  arrow.
   --  "setting": True to display an indicator that the column is sorted

   function Get_Sort_Order
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Gtk.Enums.Gtk_Sort_Type;
   --  Gets the value set by Gtk.Tree_View_Column.Set_Sort_Order.

   procedure Set_Sort_Order
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Order       : Gtk.Enums.Gtk_Sort_Type);
   --  Changes the appearance of the sort indicator.
   --  This does not actually sort the model. Use
   --  Gtk.Tree_View_Column.Set_Sort_Column_Id if you want automatic sorting
   --  support. This function is primarily for custom sorting behavior, and
   --  should be used in conjunction with Gtk.Tree_Sortable.Set_Sort_Column_Id
   --  to do that. For custom models, the mechanism will vary.
   --  The sort indicator changes direction to indicate normal sort or reverse
   --  sort. Note that you must have the sort indicator enabled to see anything
   --  when calling this function; see Gtk.Tree_View_Column.Set_Sort_Indicator.
   --  "order": sort order that the sort indicator should indicate

   function Get_Spacing
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Glib.Gint;
   --  Returns the spacing of Tree_Column.

   procedure Set_Spacing
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Spacing     : Glib.Gint);
   --  Sets the spacing field of Tree_Column, which is the number of pixels to
   --  place between cell renderers packed into it.
   --  "spacing": distance between cell renderers in pixels.

   function Get_Title
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return UTF8_String;
   --  Returns the title of the widget.

   procedure Set_Title
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Title       : UTF8_String);
   --  Sets the title of the Tree_Column. If a custom widget has been set,
   --  then this value is ignored.
   --  "title": The title of the Tree_Column.

   function Get_Tree_View
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the Gtk.Tree_View.Gtk_Tree_View wherein Tree_Column has been
   --  inserted. If Column is currently not inserted in any tree view, null is
   --  returned.
   --  Since: gtk+ 2.12

   function Get_Visible
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Boolean;
   --  Returns True if Tree_Column is visible.

   procedure Set_Visible
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Visible     : Boolean);
   --  Sets the visibility of Tree_Column.
   --  "visible": True if the Tree_Column is visible.

   function Get_Widget
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the Gtk.Widget.Gtk_Widget in the button on the column header.
   --  If a custom widget has not been set then null is returned.

   procedure Set_Widget
      (Tree_Column : not null access Gtk_Tree_View_Column_Record;
       Widget      : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the widget in the header to be Widget. If widget is null, then the
   --  header button is set with a Gtk.Label.Gtk_Label set to the title of
   --  Tree_Column.
   --  "widget": A child Gtk.Widget.Gtk_Widget, or null.

   function Get_Width
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Glib.Gint;
   --  Returns the current size of Tree_Column in pixels.

   function Get_X_Offset
      (Tree_Column : not null access Gtk_Tree_View_Column_Record)
       return Glib.Gint;
   --  Returns the current X offset of Tree_Column in pixels.
   --  Since: gtk+ 3.2

   procedure Queue_Resize
      (Tree_Column : not null access Gtk_Tree_View_Column_Record);
   --  Flags the column, and the cell renderers added to this column, to have
   --  their sizes renegotiated.
   --  Since: gtk+ 2.8

   procedure Set_Cell_Data_Func
      (Cell_Layout : not null access Gtk_Tree_View_Column_Record;
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
         (Cell_Layout : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
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

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Add_Attribute
      (Cell_Layout : not null access Gtk_Tree_View_Column_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute   : UTF8_String;
       Column      : Glib.Gint);

   procedure Clear
      (Cell_Layout : not null access Gtk_Tree_View_Column_Record);

   procedure Clear_Attributes
      (Cell_Layout : not null access Gtk_Tree_View_Column_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);

   function Get_Cells
      (Cell_Layout : not null access Gtk_Tree_View_Column_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;

   procedure Pack_End
      (Cell_Layout : not null access Gtk_Tree_View_Column_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Pack_Start
      (Cell_Layout : not null access Gtk_Tree_View_Column_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Reorder
      (Cell_Layout : not null access Gtk_Tree_View_Column_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position    : Glib.Gint);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Alignment_Property : constant Glib.Properties.Property_Float;

   Cell_Area_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Cell_Area.Gtk_Cell_Area
   --  The Gtk.Cell_Area.Gtk_Cell_Area used to layout cell renderers for this
   --  column.
   --
   --  If no area is specified when creating the tree view column with
   --  Gtk.Tree_View_Column.Gtk_New_With_Area a horizontally oriented
   --  Gtk.Cell_Area_Box.Gtk_Cell_Area_Box will be used.

   Clickable_Property : constant Glib.Properties.Property_Boolean;

   Expand_Property : constant Glib.Properties.Property_Boolean;

   Fixed_Width_Property : constant Glib.Properties.Property_Int;

   Max_Width_Property : constant Glib.Properties.Property_Int;

   Min_Width_Property : constant Glib.Properties.Property_Int;

   Reorderable_Property : constant Glib.Properties.Property_Boolean;

   Resizable_Property : constant Glib.Properties.Property_Boolean;

   Sizing_Property : constant Gtk.Tree_View_Column.Property_Gtk_Tree_View_Column_Sizing;
   --  Type: Gtk_Tree_View_Column_Sizing

   Sort_Column_Id_Property : constant Glib.Properties.Property_Int;
   --  Logical sort column ID this column sorts on when selected for sorting.
   --  Setting the sort column ID makes the column header clickable. Set to -1
   --  to make the column unsortable.

   Sort_Indicator_Property : constant Glib.Properties.Property_Boolean;

   Sort_Order_Property : constant Gtk.Enums.Property_Gtk_Sort_Type;

   Spacing_Property : constant Glib.Properties.Property_Int;

   Title_Property : constant Glib.Properties.Property_String;

   Visible_Property : constant Glib.Properties.Property_Boolean;

   Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   Width_Property : constant Glib.Properties.Property_Int;

   X_Offset_Property : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Tree_View_Column_Void is not null access procedure
     (Self : access Gtk_Tree_View_Column_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Clicked : constant Glib.Signal_Name := "clicked";
   procedure On_Clicked
      (Self  : not null access Gtk_Tree_View_Column_Record;
       Call  : Cb_Gtk_Tree_View_Column_Void;
       After : Boolean := False);
   procedure On_Clicked
      (Self  : not null access Gtk_Tree_View_Column_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "CellLayout"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Tree_View_Column_Record, Gtk_Tree_View_Column);
   function "+"
     (Widget : access Gtk_Tree_View_Column_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Tree_View_Column
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Cell_Layout is new Glib.Types.Implements
     (Gtk.Cell_Layout.Gtk_Cell_Layout, Gtk_Tree_View_Column_Record, Gtk_Tree_View_Column);
   function "+"
     (Widget : access Gtk_Tree_View_Column_Record'Class)
   return Gtk.Cell_Layout.Gtk_Cell_Layout
   renames Implements_Gtk_Cell_Layout.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Layout.Gtk_Cell_Layout)
   return Gtk_Tree_View_Column
   renames Implements_Gtk_Cell_Layout.To_Object;

private
   X_Offset_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("x-offset");
   Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width");
   Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("widget");
   Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible");
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("spacing");
   Sort_Order_Property : constant Gtk.Enums.Property_Gtk_Sort_Type :=
     Gtk.Enums.Build ("sort-order");
   Sort_Indicator_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("sort-indicator");
   Sort_Column_Id_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("sort-column-id");
   Sizing_Property : constant Gtk.Tree_View_Column.Property_Gtk_Tree_View_Column_Sizing :=
     Gtk.Tree_View_Column.Build ("sizing");
   Resizable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("resizable");
   Reorderable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("reorderable");
   Min_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("min-width");
   Max_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-width");
   Fixed_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("fixed-width");
   Expand_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("expand");
   Clickable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("clickable");
   Cell_Area_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("cell-area");
   Alignment_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("alignment");
end Gtk.Tree_View_Column;
