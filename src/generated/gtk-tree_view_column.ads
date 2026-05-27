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

--  A visible column in a [classGtk.TreeView] widget
--
--  The `GtkTreeViewColumn` object represents a visible column in a
--  `GtkTreeView` widget. It allows to set properties of the column header, and
--  functions as a holding pen for the cell renderers which determine how the
--  data in the column is displayed.
--
--  Please refer to the [tree widget conceptual
--  overview](section-tree-widget.html) for an overview of all the objects and
--  data types related to the tree widget and how they work together, and to
--  the [classGtk.TreeView] documentation for specifics about the CSS node
--  structure for treeviews and their headers.

pragma Warnings (Off, "*is already use-visible*");
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
with Interfaces.C;            use Interfaces.C;

package Gtk.Tree_View_Column is

   pragma Obsolescent;
   --  Use [class@Gtk.ColumnView] and [class@Gtk.ColumnViewColumn] instead of [class@Gtk.TreeView] to show a tabular list

   type Gtk_Tree_View_Column_Record is new GObject_Record with null record;
   type Gtk_Tree_View_Column is access all Gtk_Tree_View_Column_Record'Class;

   type Gtk_Tree_View_Column_Sizing is (
      Grow_Only,
      Autosize,
      Fixed);
   pragma Convention (C, Gtk_Tree_View_Column_Sizing);
   --  The sizing method the column uses to determine its width. Please note
   --  that Gtk.Tree_View_Column.Autosize are inefficient for large views, and
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
   --  @param Cell_Layout a `GtkCellLayout`
   --  @param Cell the cell renderer whose value is to be set
   --  @param Tree_Model the model
   --  @param Iter a `GtkTreeIter` indicating the row to set the value for

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Tree_View_Column_Sizing_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Tree_View_Column_Sizing);
   type Property_Gtk_Tree_View_Column_Sizing is new Gtk_Tree_View_Column_Sizing_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Tree_View_Column);
   procedure Initialize
      (Self : not null access Gtk_Tree_View_Column_Record'Class);
   --  Creates a new `GtkTreeViewColumn`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Tree_View_Column_New return Gtk_Tree_View_Column;
   --  Creates a new `GtkTreeViewColumn`.

   procedure Gtk_New_With_Area
      (Self : out Gtk_Tree_View_Column;
       Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class);
   procedure Initialize_With_Area
      (Self : not null access Gtk_Tree_View_Column_Record'Class;
       Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class);
   --  Creates a new `GtkTreeViewColumn` using Area to render its cells.
   --  Initialize_With_Area does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Area the `GtkCellArea` that the newly created column should use
   --  to layout cells.

   function Gtk_Tree_View_Column_New_With_Area
      (Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
       return Gtk_Tree_View_Column;
   --  Creates a new `GtkTreeViewColumn` using Area to render its cells.
   --  @param Area the `GtkCellArea` that the newly created column should use
   --  to layout cells.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_view_column_get_type");

   -------------
   -- Methods --
   -------------

   procedure Cell_Get_Position
      (Self          : not null access Gtk_Tree_View_Column_Record;
       Cell_Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       X_Offset      : out Glib.Gint;
       Width         : out Glib.Gint;
       Success       : out Boolean);
   pragma Obsolescent (Cell_Get_Position);
   --  Obtains the horizontal position and size of a cell in a column.
   --  If the cell is not found in the column, Start_Pos and Width are not
   --  changed and False is returned.
   --  Deprecated since 4.10, 1
   --  @param Cell_Renderer a `GtkCellRenderer`
   --  @param X_Offset return location for the horizontal position of Cell
   --  within Tree_Column
   --  @param Width return location for the width of Cell
   --  @return True if Cell belongs to Tree_Column

   procedure Cell_Get_Size
      (Self     : not null access Gtk_Tree_View_Column_Record;
       X_Offset : out Glib.Gint;
       Y_Offset : out Glib.Gint;
       Width    : out Glib.Gint;
       Height   : out Glib.Gint);
   pragma Obsolescent (Cell_Get_Size);
   --  Obtains the width and height needed to render the column. This is used
   --  primarily by the `GtkTreeView`.
   --  Deprecated since 4.10, 1
   --  @param X_Offset location to return x offset of a cell relative to
   --  Cell_Area
   --  @param Y_Offset location to return y offset of a cell relative to
   --  Cell_Area
   --  @param Width location to return width needed to render a cell
   --  @param Height location to return height needed to render a cell

   function Cell_Is_Visible
      (Self : not null access Gtk_Tree_View_Column_Record) return Boolean;
   pragma Obsolescent (Cell_Is_Visible);
   --  Returns True if any of the cells packed into the Tree_Column are
   --  visible. For this to be meaningful, you must first initialize the cells
   --  with Gtk.Tree_View_Column.Cell_Set_Cell_Data
   --  Deprecated since 4.10, 1
   --  @return True, if any of the cells packed into the Tree_Column are
   --  currently visible

   procedure Cell_Set_Cell_Data
      (Self        : not null access Gtk_Tree_View_Column_Record;
       Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
       Is_Expander : Boolean;
       Is_Expanded : Boolean);
   pragma Obsolescent (Cell_Set_Cell_Data);
   --  Sets the cell renderer based on the Tree_Model and Iter. That is, for
   --  every attribute mapping in Tree_Column, it will get a value from the set
   --  column on the Iter, and use that value to set the attribute on the cell
   --  renderer. This is used primarily by the `GtkTreeView`.
   --  Deprecated since 4.10, 1
   --  @param Tree_Model The `GtkTreeModel` to get the cell renderers
   --  attributes from.
   --  @param Iter The `GtkTreeIter` to get the cell renderer's attributes
   --  from.
   --  @param Is_Expander True, if the row has children
   --  @param Is_Expanded True, if the row has visible children

   procedure Clicked (Self : not null access Gtk_Tree_View_Column_Record);
   pragma Obsolescent (Clicked);
   --  Emits the "clicked" signal on the column. This function will only work
   --  if Tree_Column is clickable.
   --  Deprecated since 4.10, 1

   procedure Focus_Cell
      (Self : not null access Gtk_Tree_View_Column_Record;
       Cell : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   pragma Obsolescent (Focus_Cell);
   --  Sets the current keyboard focus to be at Cell, if the column contains 2
   --  or more editable and activatable cells.
   --  Deprecated since 4.10, 1
   --  @param Cell A `GtkCellRenderer`

   function Get_Alignment
      (Self : not null access Gtk_Tree_View_Column_Record)
       return Interfaces.C.C_float;
   pragma Obsolescent (Get_Alignment);
   --  Returns the current x alignment of Tree_Column. This value can range
   --  between 0.0 and 1.0.
   --  Deprecated since 4.10, 1
   --  @return The current alignent of Tree_Column.

   procedure Set_Alignment
      (Self   : not null access Gtk_Tree_View_Column_Record;
       Xalign : Interfaces.C.C_float);
   pragma Obsolescent (Set_Alignment);
   --  Sets the alignment of the title or custom widget inside the column
   --  header. The alignment determines its location inside the button -- 0.0
   --  for left, 0.5 for center, 1.0 for right.
   --  Deprecated since 4.10, 1
   --  @param Xalign The alignment, which is between [0.0 and 1.0] inclusive.

   function Get_Button
      (Self : not null access Gtk_Tree_View_Column_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Button);
   --  Returns the button used in the treeview column header
   --  Deprecated since 4.10, 1
   --  @return The button for the column header.

   function Get_Clickable
      (Self : not null access Gtk_Tree_View_Column_Record) return Boolean;
   pragma Obsolescent (Get_Clickable);
   --  Returns True if the user can click on the header for the column.
   --  Deprecated since 4.10, 1
   --  @return True if user can click the column header.

   procedure Set_Clickable
      (Self      : not null access Gtk_Tree_View_Column_Record;
       Clickable : Boolean);
   pragma Obsolescent (Set_Clickable);
   --  Sets the header to be active if Clickable is True. When the header is
   --  active, then it can take keyboard focus, and can be clicked.
   --  Deprecated since 4.10, 1
   --  @param Clickable True if the header is active.

   function Get_Expand
      (Self : not null access Gtk_Tree_View_Column_Record) return Boolean;
   pragma Obsolescent (Get_Expand);
   --  Returns True if the column expands to fill available space.
   --  Deprecated since 4.10, 1
   --  @return True if the column expands to fill available space.

   procedure Set_Expand
      (Self   : not null access Gtk_Tree_View_Column_Record;
       Expand : Boolean);
   pragma Obsolescent (Set_Expand);
   --  Sets the column to take available extra space. This space is shared
   --  equally amongst all columns that have the expand set to True. If no
   --  column has this option set, then the last column gets all extra space.
   --  By default, every column is created with this False.
   --  Along with "fixed-width", the "expand" property changes when the column
   --  is resized by the user.
   --  Deprecated since 4.10, 1
   --  @param Expand True if the column should expand to fill available space.

   function Get_Fixed_Width
      (Self : not null access Gtk_Tree_View_Column_Record) return Glib.Gint;
   pragma Obsolescent (Get_Fixed_Width);
   --  Gets the fixed width of the column. This may not be the actual
   --  displayed width of the column; for that, use
   --  Gtk.Tree_View_Column.Get_Width.
   --  Deprecated since 4.10, 1
   --  @return The fixed width of the column.

   procedure Set_Fixed_Width
      (Self        : not null access Gtk_Tree_View_Column_Record;
       Fixed_Width : Glib.Gint);
   pragma Obsolescent (Set_Fixed_Width);
   --  If Fixed_Width is not -1, sets the fixed width of Tree_Column;
   --  otherwise unsets it. The effective value of Fixed_Width is clamped
   --  between the minimum and maximum width of the column; however, the value
   --  stored in the "fixed-width" property is not clamped. If the column
   --  sizing is Gtk.Tree_View_Column.Grow_Only or
   --  Gtk.Tree_View_Column.Autosize, setting a fixed width overrides the
   --  automatically calculated width. Note that Fixed_Width is only a hint to
   --  GTK; the width actually allocated to the column may be greater or less
   --  than requested.
   --  Along with "expand", the "fixed-width" property changes when the column
   --  is resized by the user.
   --  Deprecated since 4.10, 1
   --  @param Fixed_Width The new fixed width, in pixels, or -1.

   function Get_Max_Width
      (Self : not null access Gtk_Tree_View_Column_Record) return Glib.Gint;
   pragma Obsolescent (Get_Max_Width);
   --  Returns the maximum width in pixels of the Tree_Column, or -1 if no
   --  maximum width is set.
   --  Deprecated since 4.10, 1
   --  @return The maximum width of the Tree_Column.

   procedure Set_Max_Width
      (Self      : not null access Gtk_Tree_View_Column_Record;
       Max_Width : Glib.Gint);
   pragma Obsolescent (Set_Max_Width);
   --  Sets the maximum width of the Tree_Column. If Max_Width is -1, then the
   --  maximum width is unset. Note, the column can actually be wider than max
   --  width if it's the last column in a view. In this case, the column
   --  expands to fill any extra space.
   --  Deprecated since 4.10, 1
   --  @param Max_Width The maximum width of the column in pixels, or -1.

   function Get_Min_Width
      (Self : not null access Gtk_Tree_View_Column_Record) return Glib.Gint;
   pragma Obsolescent (Get_Min_Width);
   --  Returns the minimum width in pixels of the Tree_Column, or -1 if no
   --  minimum width is set.
   --  Deprecated since 4.10, 1
   --  @return The minimum width of the Tree_Column.

   procedure Set_Min_Width
      (Self      : not null access Gtk_Tree_View_Column_Record;
       Min_Width : Glib.Gint);
   pragma Obsolescent (Set_Min_Width);
   --  Sets the minimum width of the Tree_Column. If Min_Width is -1, then the
   --  minimum width is unset.
   --  Deprecated since 4.10, 1
   --  @param Min_Width The minimum width of the column in pixels, or -1.

   function Get_Reorderable
      (Self : not null access Gtk_Tree_View_Column_Record) return Boolean;
   pragma Obsolescent (Get_Reorderable);
   --  Returns True if the Tree_Column can be reordered by the user.
   --  Deprecated since 4.10, 1
   --  @return True if the Tree_Column can be reordered by the user.

   procedure Set_Reorderable
      (Self        : not null access Gtk_Tree_View_Column_Record;
       Reorderable : Boolean);
   pragma Obsolescent (Set_Reorderable);
   --  If Reorderable is True, then the column can be reordered by the end
   --  user dragging the header.
   --  Deprecated since 4.10, 1
   --  @param Reorderable True, if the column can be reordered.

   function Get_Resizable
      (Self : not null access Gtk_Tree_View_Column_Record) return Boolean;
   pragma Obsolescent (Get_Resizable);
   --  Returns True if the Tree_Column can be resized by the end user.
   --  Deprecated since 4.10, 1
   --  @return True, if the Tree_Column can be resized.

   procedure Set_Resizable
      (Self      : not null access Gtk_Tree_View_Column_Record;
       Resizable : Boolean);
   pragma Obsolescent (Set_Resizable);
   --  If Resizable is True, then the user can explicitly resize the column by
   --  grabbing the outer edge of the column button.
   --  If resizable is True and sizing mode of the column is
   --  Gtk.Tree_View_Column.Autosize, then the sizing mode is changed to
   --  Gtk.Tree_View_Column.Grow_Only.
   --  Deprecated since 4.10, 1
   --  @param Resizable True, if the column can be resized

   function Get_Sizing
      (Self : not null access Gtk_Tree_View_Column_Record)
       return Gtk_Tree_View_Column_Sizing;
   pragma Obsolescent (Get_Sizing);
   --  Returns the current type of Tree_Column.
   --  Deprecated since 4.10, 1
   --  @return The type of Tree_Column.

   procedure Set_Sizing
      (Self     : not null access Gtk_Tree_View_Column_Record;
       The_Type : Gtk_Tree_View_Column_Sizing);
   pragma Obsolescent (Set_Sizing);
   --  Sets the growth behavior of Tree_Column to Type.
   --  Deprecated since 4.10, 1
   --  @param The_Type The `GtkTreeViewColumn`Sizing.

   function Get_Sort_Column_Id
      (Self : not null access Gtk_Tree_View_Column_Record) return Glib.Gint;
   pragma Obsolescent (Get_Sort_Column_Id);
   --  Gets the logical Sort_Column_Id that the model sorts on when this
   --  column is selected for sorting.
   --  See [methodGtk.TreeViewColumn.set_sort_column_id].
   --  Deprecated since 4.10, 1
   --  @return the current Sort_Column_Id for this column, or -1 if this
   --  column can't be used for sorting

   procedure Set_Sort_Column_Id
      (Self           : not null access Gtk_Tree_View_Column_Record;
       Sort_Column_Id : Glib.Gint);
   pragma Obsolescent (Set_Sort_Column_Id);
   --  Sets the logical Sort_Column_Id that this column sorts on when this
   --  column is selected for sorting. Doing so makes the column header
   --  clickable.
   --  Deprecated since 4.10, 1
   --  @param Sort_Column_Id The Sort_Column_Id of the model to sort on.

   function Get_Sort_Indicator
      (Self : not null access Gtk_Tree_View_Column_Record) return Boolean;
   pragma Obsolescent (Get_Sort_Indicator);
   --  Gets the value set by Gtk.Tree_View_Column.Set_Sort_Indicator.
   --  Deprecated since 4.10, 1
   --  @return whether the sort indicator arrow is displayed

   procedure Set_Sort_Indicator
      (Self    : not null access Gtk_Tree_View_Column_Record;
       Setting : Boolean);
   pragma Obsolescent (Set_Sort_Indicator);
   --  Call this function with a Setting of True to display an arrow in the
   --  header button indicating the column is sorted. Call
   --  Gtk.Tree_View_Column.Set_Sort_Order to change the direction of the
   --  arrow.
   --  Deprecated since 4.10, 1
   --  @param Setting True to display an indicator that the column is sorted

   function Get_Sort_Order
      (Self : not null access Gtk_Tree_View_Column_Record)
       return Gtk.Enums.Gtk_Sort_Type;
   pragma Obsolescent (Get_Sort_Order);
   --  Gets the value set by Gtk.Tree_View_Column.Set_Sort_Order.
   --  Deprecated since 4.10, 1
   --  @return the sort order the sort indicator is indicating

   procedure Set_Sort_Order
      (Self  : not null access Gtk_Tree_View_Column_Record;
       Order : Gtk.Enums.Gtk_Sort_Type);
   pragma Obsolescent (Set_Sort_Order);
   --  Changes the appearance of the sort indicator.
   --  This does not actually sort the model. Use
   --  Gtk.Tree_View_Column.Set_Sort_Column_Id if you want automatic sorting
   --  support. This function is primarily for custom sorting behavior, and
   --  should be used in conjunction with gtk_tree_sortable_set_sort_column_id
   --  to do that. For custom models, the mechanism will vary.
   --  The sort indicator changes direction to indicate normal sort or reverse
   --  sort. Note that you must have the sort indicator enabled to see anything
   --  when calling this function; see Gtk.Tree_View_Column.Set_Sort_Indicator.
   --  Deprecated since 4.10, 1
   --  @param Order sort order that the sort indicator should indicate

   function Get_Spacing
      (Self : not null access Gtk_Tree_View_Column_Record) return Glib.Gint;
   pragma Obsolescent (Get_Spacing);
   --  Returns the spacing of Tree_Column.
   --  Deprecated since 4.10, 1
   --  @return the spacing of Tree_Column.

   procedure Set_Spacing
      (Self    : not null access Gtk_Tree_View_Column_Record;
       Spacing : Glib.Gint);
   pragma Obsolescent (Set_Spacing);
   --  Sets the spacing field of Tree_Column, which is the number of pixels to
   --  place between cell renderers packed into it.
   --  Deprecated since 4.10, 1
   --  @param Spacing distance between cell renderers in pixels.

   function Get_Title
      (Self : not null access Gtk_Tree_View_Column_Record)
       return UTF8_String;
   pragma Obsolescent (Get_Title);
   --  Returns the title of the widget.
   --  Deprecated since 4.10, 1
   --  @return the title of the column. This string should not be modified or
   --  freed.

   procedure Set_Title
      (Self  : not null access Gtk_Tree_View_Column_Record;
       Title : UTF8_String);
   pragma Obsolescent (Set_Title);
   --  Sets the title of the Tree_Column. If a custom widget has been set,
   --  then this value is ignored.
   --  Deprecated since 4.10, 1
   --  @param Title The title of the Tree_Column.

   function Get_Tree_View
      (Self : not null access Gtk_Tree_View_Column_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Tree_View);
   --  Returns the `GtkTreeView` wherein Tree_Column has been inserted. If
   --  Column is currently not inserted in any tree view, null is returned.
   --  Deprecated since 4.10, 1
   --  @return The tree view wherein Column has been inserted

   function Get_Visible
      (Self : not null access Gtk_Tree_View_Column_Record) return Boolean;
   pragma Obsolescent (Get_Visible);
   --  Returns True if Tree_Column is visible.
   --  Deprecated since 4.10, 1
   --  @return whether the column is visible or not. If it is visible, then
   --  the tree will show the column.

   procedure Set_Visible
      (Self    : not null access Gtk_Tree_View_Column_Record;
       Visible : Boolean);
   pragma Obsolescent (Set_Visible);
   --  Sets the visibility of Tree_Column.
   --  Deprecated since 4.10, 1
   --  @param Visible True if the Tree_Column is visible.

   function Get_Widget
      (Self : not null access Gtk_Tree_View_Column_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Widget);
   --  Returns the `GtkWidget` in the button on the column header.
   --  If a custom widget has not been set then null is returned.
   --  Deprecated since 4.10, 1
   --  @return The `GtkWidget` in the column header

   procedure Set_Widget
      (Self   : not null access Gtk_Tree_View_Column_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   pragma Obsolescent (Set_Widget);
   --  Sets the widget in the header to be Widget. If widget is null, then the
   --  header button is set with a `GtkLabel` set to the title of Tree_Column.
   --  Deprecated since 4.10, 1
   --  @param Widget A child `GtkWidget`

   function Get_Width
      (Self : not null access Gtk_Tree_View_Column_Record) return Glib.Gint;
   pragma Obsolescent (Get_Width);
   --  Returns the current size of Tree_Column in pixels.
   --  Deprecated since 4.10, 1
   --  @return The current width of Tree_Column.

   function Get_X_Offset
      (Self : not null access Gtk_Tree_View_Column_Record) return Glib.Gint;
   pragma Obsolescent (Get_X_Offset);
   --  Returns the current X offset of Tree_Column in pixels.
   --  Deprecated since 4.10, 1
   --  @return The current X offset of Tree_Column.

   procedure Queue_Resize
      (Self : not null access Gtk_Tree_View_Column_Record);
   pragma Obsolescent (Queue_Resize);
   --  Flags the column, and the cell renderers added to this column, to have
   --  their sizes renegotiated.
   --  Deprecated since 4.10, 1

   procedure Set_Cell_Data_Func
      (Self : not null access Gtk_Tree_View_Column_Record;
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
         (Self      : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
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

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Add_Attribute
      (Self      : not null access Gtk_Tree_View_Column_Record;
       Cell      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String;
       Column    : Glib.Gint);
   pragma Obsolescent (Add_Attribute);

   procedure Clear (Self : not null access Gtk_Tree_View_Column_Record);
   pragma Obsolescent (Clear);

   procedure Clear_Attributes
      (Self : not null access Gtk_Tree_View_Column_Record;
       Cell : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   pragma Obsolescent (Clear_Attributes);

   function Get_Cells
      (Self : not null access Gtk_Tree_View_Column_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
   pragma Obsolescent (Get_Cells);

   procedure Pack_End
      (Self   : not null access Gtk_Tree_View_Column_Record;
       Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand : Boolean);
   pragma Obsolescent (Pack_End);

   procedure Pack_Start
      (Self   : not null access Gtk_Tree_View_Column_Record;
       Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand : Boolean);
   pragma Obsolescent (Pack_Start);

   procedure Reorder
      (Self     : not null access Gtk_Tree_View_Column_Record;
       Cell     : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position : Glib.Gint);
   pragma Obsolescent (Reorder);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Alignment_Property : constant Glib.Properties.Property_Float;

   Cell_Area_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Cell_Area.Gtk_Cell_Area
   --  The `GtkCellArea` used to layout cell renderers for this column.
   --
   --  If no area is specified when creating the tree view column with
   --  Gtk.Tree_View_Column.Gtk_New_With_Area a horizontally oriented
   --  `GtkCellAreaBox` will be used.

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
   --  Emitted when the column's header has been clicked.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.CellLayout"

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
