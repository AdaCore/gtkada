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

--  A widget for displaying both trees and lists
--
--  <picture> <source srcset="list-and-tree-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkTreeView"
--  src="list-and-tree.png"> </picture>
--  Widget that displays any object that implements the [ifaceGtk.TreeModel]
--  interface.
--
--  Please refer to the [tree widget conceptual
--  overview](section-tree-widget.html) for an overview of all the objects and
--  data types related to the tree widget and how they work together.
--
--  ## Coordinate systems in GtkTreeView API
--
--  Several different coordinate systems are exposed in the `GtkTreeView` API.
--  These are:
--
--  ![](tree-view-coordinates.png)
--
--  - Widget coordinates: Coordinates relative to the widget (usually
--  `widget->window`).
--
--  - Bin window coordinates: Coordinates relative to the window that
--  GtkTreeView renders to.
--
--  - Tree coordinates: Coordinates relative to the entire scrollable area of
--  GtkTreeView. These coordinates start at (0, 0) for row 0 of the tree.
--
--  Several functions are available for converting between the different
--  coordinate systems. The most common translations are between widget and bin
--  window coordinates and between bin window and tree coordinates. For the
--  former you can use [methodGtk.TreeView.convert_widget_to_bin_window_coords]
--  (and vice versa), for the latter
--  [methodGtk.TreeView.convert_bin_window_to_tree_coords] (and vice versa).
--
--  ## `GtkTreeView` as `GtkBuildable`
--
--  The `GtkTreeView` implementation of the `GtkBuildable` interface accepts
--  [classGtk.TreeViewColumn] objects as `<child>` elements and exposes the
--  internal [classGtk.TreeSelection] in UI definitions.
--
--  An example of a UI definition fragment with `GtkTreeView`:
--
--  ```xml <object class="GtkTreeView" id="treeview"> <property
--  name="model">liststore1</property> <child> <object
--  class="GtkTreeViewColumn" id="test-column"> <property
--  name="title">Test</property> <child> <object class="GtkCellRendererText"
--  id="test-renderer"/> <attributes> <attribute name="text">1</attribute>
--  </attributes> </child> </object> </child> <child
--  internal-child="selection"> <object class="GtkTreeSelection"
--  id="selection"> <signal name="changed"
--  handler="on_treeview_selection_changed"/> </object> </child> </object> ```
--
--  ## CSS nodes
--
--  ``` treeview.view ├── header │ ├── button │ │ ╰── [sort-indicator] ┊ ┊ │
--  ╰── button │ ╰── [sort-indicator] │ ├── [rubberband] ╰── [dndtarget] ```
--
--  `GtkTreeView` has a main CSS node with name `treeview` and style class
--  `.view`. It has a subnode with name `header`, which is the parent for all
--  the column header widgets' CSS nodes.
--
--  Each column header consists of a `button`, which among other content, has
--  a child with name `sort-indicator`, which carries the `.ascending` or
--  `.descending` style classes when the column header should show a sort
--  indicator. The CSS is expected to provide a suitable image using the
--  `-gtk-icon-source` property.
--
--  For rubberband selection, a subnode with name `rubberband` is used.
--
--  For the drop target location during DND, a subnode with name `dndtarget`
--  is used.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Paintable;           use Gdk.Paintable;
with Gdk.Rectangle;           use Gdk.Rectangle;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;           use Gtk.Atcontext;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Cell_Renderer;       use Gtk.Cell_Renderer;
with Gtk.Constraint_Target;   use Gtk.Constraint_Target;
with Gtk.Editable;            use Gtk.Editable;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Tooltip;             use Gtk.Tooltip;
with Gtk.Tree_Model;          use Gtk.Tree_Model;
with Gtk.Tree_Selection;      use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;    use Gtk.Tree_View_Column;
with Gtk.Widget;              use Gtk.Widget;
with Interfaces.C;            use Interfaces.C;

package Gtk.Tree_View is

   pragma Obsolescent;
   --  Use [class@Gtk.ListView] for lists, and [class@Gtk.ColumnView] for tabular lists

   type Gtk_Tree_View_Record is new Gtk_Widget_Record with null record;
   type Gtk_Tree_View is access all Gtk_Tree_View_Record'Class;

   type Gtk_Tree_View_Drop_Position is (
      Drop_Before,
      Drop_After,
      Drop_Into_Or_Before,
      Drop_Into_Or_After);
   pragma Convention (C, Gtk_Tree_View_Drop_Position);
   --  An enum for determining where a dropped row goes.

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Tree_Cell_Data_Func is access procedure
     (Tree_Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  A function to set the properties of a cell instead of just using the
   --  straight mapping between the cell and the model.
   --  This function is useful for customizing the cell renderer. For example,
   --  a function might get an* integer from the Tree_Model, and render it to
   --  the "text" attribute of "cell" by converting it to its written
   --  equivalent.
   --  See also: gtk_tree_view_column_set_cell_data_func
   --  @param Tree_Column A `GtkTreeViewColumn`
   --  @param Cell The `GtkCellRenderer` that is being rendered by Tree_Column
   --  @param Tree_Model The `GtkTreeModel` being rendered
   --  @param Iter A `GtkTreeIter` of the current row rendered

   type Gtk_Tree_View_Mapping_Func is access procedure
     (Tree_View : not null access Gtk_Tree_View_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Function used for Gtk.Tree_View.Map_Expanded_Rows.
   --  @param Tree_View A `GtkTreeView`
   --  @param Path The path that's expanded

   type Gtk_Tree_View_Column_Drop_Func is access function
     (Tree_View   : not null access Gtk_Tree_View_Record'Class;
      Column      : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Prev_Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Next_Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class)
   return Boolean;
   --  Function type for determining whether Column can be dropped in a
   --  particular spot (as determined by Prev_Column and Next_Column). In left
   --  to right locales, Prev_Column is on the left of the potential drop spot,
   --  and Next_Column is on the right. In right to left mode, this is
   --  reversed. This function should return True if the spot is a valid drop
   --  spot. Please note that returning True does not actually indicate that
   --  the column drop was made, but is meant only to indicate a possible drop
   --  spot to the user.
   --  @param Tree_View A `GtkTreeView`
   --  @param Column The `GtkTreeViewColumn` being dragged
   --  @param Prev_Column A `GtkTreeViewColumn` on one side of Column
   --  @param Next_Column A `GtkTreeViewColumn` on the other side of Column
   --  @return True, if Column can be dropped in this spot

   type Gtk_Tree_View_Row_Separator_Func is access function
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Function type for determining whether the row pointed to by Iter should
   --  be rendered as a separator. A common way to implement this is to have a
   --  boolean column in the model, whose values the
   --  `GtkTreeViewRowSeparatorFunc` returns.
   --  @param Model the `GtkTreeModel`
   --  @param Iter a `GtkTreeIter` pointing at a row in Model
   --  @return True if the row is a separator

   type Gtk_Tree_View_Search_Equal_Func is access function
     (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Column : Glib.Gint;
      Key    : UTF8_String;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  A function used for checking whether a row in Model matches a search
   --  key string entered by the user. Note the return value is reversed from
   --  what you would normally expect, though it has some similarity to strcmp
   --  returning 0 for equal strings.
   --  @param Model the `GtkTreeModel` being searched
   --  @param Column the search column set by Gtk.Tree_View.Set_Search_Column
   --  @param Key the key string to compare with
   --  @param Iter a `GtkTreeIter` pointing the row of Model that should be
   --  compared with Key.
   --  @return False if the row matches, True otherwise.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Tree_View_Drop_Position_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Tree_View_Drop_Position);
   type Property_Gtk_Tree_View_Drop_Position is new Gtk_Tree_View_Drop_Position_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Tree_View);
   procedure Initialize (Self : not null access Gtk_Tree_View_Record'Class);
   --  Creates a new `GtkTreeView` widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Tree_View_New return Gtk_Tree_View;
   --  Creates a new `GtkTreeView` widget.

   procedure Gtk_New
      (Self  : out Gtk_Tree_View;
       Model : Gtk.Tree_Model.Gtk_Tree_Model);
   procedure Initialize
      (Self  : not null access Gtk_Tree_View_Record'Class;
       Model : Gtk.Tree_Model.Gtk_Tree_Model);
   --  Creates a new `GtkTreeView` widget with the model initialized to Model.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Model the model.

   function Gtk_Tree_View_New_With_Model
      (Model : Gtk.Tree_Model.Gtk_Tree_Model) return Gtk_Tree_View;
   --  Creates a new `GtkTreeView` widget with the model initialized to Model.
   --  @param Model the model.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_view_get_type");

   -------------
   -- Methods --
   -------------

   function Append_Column
      (Self   : not null access Gtk_Tree_View_Record;
       Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class)
       return Glib.Gint;
   pragma Obsolescent (Append_Column);
   --  Appends Column to the list of columns. If Tree_View has "fixed_height"
   --  mode enabled, then Column must have its "sizing" property set to be
   --  GTK_TREE_VIEW_COLUMN_FIXED.
   --  Deprecated since 4.10, 1
   --  @param Column The `GtkTreeViewColumn` to add.
   --  @return The number of columns in Tree_View after appending.

   procedure Collapse_All (Self : not null access Gtk_Tree_View_Record);
   pragma Obsolescent (Collapse_All);
   --  Recursively collapses all visible, expanded nodes in Tree_View.
   --  Deprecated since 4.10, 1

   function Collapse_Row
      (Self : not null access Gtk_Tree_View_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   pragma Obsolescent (Collapse_Row);
   --  Collapses a row (hides its child rows, if they exist).
   --  Deprecated since 4.10, 1
   --  @param Path path to a row in the Tree_View
   --  @return True if the row was collapsed.

   procedure Columns_Autosize (Self : not null access Gtk_Tree_View_Record);
   pragma Obsolescent (Columns_Autosize);
   --  Resizes all columns to their optimal width. Only works after the
   --  treeview has been realized.
   --  Deprecated since 4.10, 1

   procedure Convert_Bin_Window_To_Tree_Coords
      (Self : not null access Gtk_Tree_View_Record;
       Bx   : Glib.Gint;
       By   : Glib.Gint;
       Tx   : out Glib.Gint;
       Ty   : out Glib.Gint);
   pragma Obsolescent (Convert_Bin_Window_To_Tree_Coords);
   --  Converts bin_window coordinates to coordinates for the tree (the full
   --  scrollable area of the tree).
   --  Deprecated since 4.10, 1
   --  @param Bx X coordinate relative to bin_window
   --  @param By Y coordinate relative to bin_window
   --  @param Tx return location for tree X coordinate
   --  @param Ty return location for tree Y coordinate

   procedure Convert_Bin_Window_To_Widget_Coords
      (Self : not null access Gtk_Tree_View_Record;
       Bx   : Glib.Gint;
       By   : Glib.Gint;
       Wx   : out Glib.Gint;
       Wy   : out Glib.Gint);
   pragma Obsolescent (Convert_Bin_Window_To_Widget_Coords);
   --  Converts bin_window coordinates to widget relative coordinates.
   --  Deprecated since 4.10, 1
   --  @param Bx bin_window X coordinate
   --  @param By bin_window Y coordinate
   --  @param Wx return location for widget X coordinate
   --  @param Wy return location for widget Y coordinate

   procedure Convert_Tree_To_Bin_Window_Coords
      (Self : not null access Gtk_Tree_View_Record;
       Tx   : Glib.Gint;
       Ty   : Glib.Gint;
       Bx   : out Glib.Gint;
       By   : out Glib.Gint);
   pragma Obsolescent (Convert_Tree_To_Bin_Window_Coords);
   --  Converts tree coordinates (coordinates in full scrollable area of the
   --  tree) to bin_window coordinates.
   --  Deprecated since 4.10, 1
   --  @param Tx tree X coordinate
   --  @param Ty tree Y coordinate
   --  @param Bx return location for X coordinate relative to bin_window
   --  @param By return location for Y coordinate relative to bin_window

   procedure Convert_Tree_To_Widget_Coords
      (Self : not null access Gtk_Tree_View_Record;
       Tx   : Glib.Gint;
       Ty   : Glib.Gint;
       Wx   : out Glib.Gint;
       Wy   : out Glib.Gint);
   pragma Obsolescent (Convert_Tree_To_Widget_Coords);
   --  Converts tree coordinates (coordinates in full scrollable area of the
   --  tree) to widget coordinates.
   --  Deprecated since 4.10, 1
   --  @param Tx X coordinate relative to the tree
   --  @param Ty Y coordinate relative to the tree
   --  @param Wx return location for widget X coordinate
   --  @param Wy return location for widget Y coordinate

   procedure Convert_Widget_To_Bin_Window_Coords
      (Self : not null access Gtk_Tree_View_Record;
       Wx   : Glib.Gint;
       Wy   : Glib.Gint;
       Bx   : out Glib.Gint;
       By   : out Glib.Gint);
   pragma Obsolescent (Convert_Widget_To_Bin_Window_Coords);
   --  Converts widget coordinates to coordinates for the bin_window.
   --  Deprecated since 4.10, 1
   --  @param Wx X coordinate relative to the widget
   --  @param Wy Y coordinate relative to the widget
   --  @param Bx return location for bin_window X coordinate
   --  @param By return location for bin_window Y coordinate

   procedure Convert_Widget_To_Tree_Coords
      (Self : not null access Gtk_Tree_View_Record;
       Wx   : Glib.Gint;
       Wy   : Glib.Gint;
       Tx   : out Glib.Gint;
       Ty   : out Glib.Gint);
   pragma Obsolescent (Convert_Widget_To_Tree_Coords);
   --  Converts widget coordinates to coordinates for the tree (the full
   --  scrollable area of the tree).
   --  Deprecated since 4.10, 1
   --  @param Wx X coordinate relative to the widget
   --  @param Wy Y coordinate relative to the widget
   --  @param Tx return location for tree X coordinate
   --  @param Ty return location for tree Y coordinate

   function Create_Row_Drag_Icon
      (Self : not null access Gtk_Tree_View_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gdk.Paintable.Gdk_Paintable;
   pragma Obsolescent (Create_Row_Drag_Icon);
   --  Creates a `cairo_surface_t` representation of the row at Path. This
   --  image is used for a drag icon.
   --  Deprecated since 4.10, 1
   --  @param Path a `GtkTreePath` in Tree_View
   --  @return a newly-allocated surface of the drag icon.

   procedure Expand_All (Self : not null access Gtk_Tree_View_Record);
   pragma Obsolescent (Expand_All);
   --  Recursively expands all nodes in the Tree_View.
   --  Deprecated since 4.10, 1

   function Expand_Row
      (Self     : not null access Gtk_Tree_View_Record;
       Path     : Gtk.Tree_Model.Gtk_Tree_Path;
       Open_All : Boolean) return Boolean;
   pragma Obsolescent (Expand_Row);
   --  Opens the row so its children are visible.
   --  Deprecated since 4.10, 1
   --  @param Path path to a row
   --  @param Open_All whether to recursively expand, or just expand immediate
   --  children
   --  @return True if the row existed and had children

   procedure Expand_To_Path
      (Self : not null access Gtk_Tree_View_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path);
   pragma Obsolescent (Expand_To_Path);
   --  Expands the row at Path. This will also expand all parent rows of Path
   --  as necessary.
   --  Deprecated since 4.10, 1
   --  @param Path path to a row.

   function Get_Activate_On_Single_Click
      (Self : not null access Gtk_Tree_View_Record) return Boolean;
   pragma Obsolescent (Get_Activate_On_Single_Click);
   --  Gets the setting set by Gtk.Tree_View.Set_Activate_On_Single_Click.
   --  Deprecated since 4.10, 1
   --  @return True if row-activated will be emitted on a single click

   procedure Set_Activate_On_Single_Click
      (Self   : not null access Gtk_Tree_View_Record;
       Single : Boolean);
   pragma Obsolescent (Set_Activate_On_Single_Click);
   --  Cause the `GtkTreeView`::row-activated signal to be emitted on a single
   --  click instead of a double click.
   --  Deprecated since 4.10, 1
   --  @param Single True to emit row-activated on a single click

   procedure Get_Background_Area
      (Self   : not null access Gtk_Tree_View_Record;
       Path   : Gtk.Tree_Model.Gtk_Tree_Path;
       Column : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Rect   : out Gdk.Rectangle.Gdk_Rectangle);
   pragma Obsolescent (Get_Background_Area);
   --  Fills the bounding rectangle in bin_window coordinates for the cell at
   --  the row specified by Path and the column specified by Column. If Path is
   --  null, or points to a node not found in the tree, the Y and Height fields
   --  of the rectangle will be filled with 0. If Column is null, the X and
   --  Width fields will be filled with 0. The returned rectangle is equivalent
   --  to the Background_Area passed to gtk_cell_renderer_render. These
   --  background areas tile to cover the entire bin window. Contrast with the
   --  Cell_Area, returned by Gtk.Tree_View.Get_Cell_Area, which returns only
   --  the cell itself, excluding surrounding borders and the tree expander
   --  area.
   --  Deprecated since 4.10, 1
   --  @param Path a `GtkTreePath` for the row, or null to get only horizontal
   --  coordinates
   --  @param Column a `GtkTreeViewColumn` for the column, or null to get only
   --  vertical coordinates
   --  @param Rect rectangle to fill with cell background rect

   procedure Get_Cell_Area
      (Self   : not null access Gtk_Tree_View_Record;
       Path   : Gtk.Tree_Model.Gtk_Tree_Path;
       Column : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Rect   : out Gdk.Rectangle.Gdk_Rectangle);
   pragma Obsolescent (Get_Cell_Area);
   --  Fills the bounding rectangle in bin_window coordinates for the cell at
   --  the row specified by Path and the column specified by Column. If Path is
   --  null, or points to a path not currently displayed, the Y and Height
   --  fields of the rectangle will be filled with 0. If Column is null, the X
   --  and Width fields will be filled with 0. The sum of all cell rects does
   --  not cover the entire tree; there are extra pixels in between rows, for
   --  example. The returned rectangle is equivalent to the Cell_Area passed to
   --  gtk_cell_renderer_render. This function is only valid if Tree_View is
   --  realized.
   --  Deprecated since 4.10, 1
   --  @param Path a `GtkTreePath` for the row, or null to get only horizontal
   --  coordinates
   --  @param Column a `GtkTreeViewColumn` for the column, or null to get only
   --  vertical coordinates
   --  @param Rect rectangle to fill with cell rect

   function Get_Column
      (Self : not null access Gtk_Tree_View_Record;
       N    : Glib.Gint) return Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   pragma Obsolescent (Get_Column);
   --  Gets the `GtkTreeViewColumn` at the given position in the tree_view.
   --  Deprecated since 4.10, 1
   --  @param N The position of the column, counting from 0.
   --  @return The `GtkTreeViewColumn`, or null if the position is outside the
   --  range of columns.

   function Get_Columns
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Tree_View_Column.Column_List.Glist;
   pragma Obsolescent (Get_Columns);
   --  Returns a `GList` of all the `GtkTreeViewColumn`s currently in
   --  Tree_View. The returned list must be freed with g_list_free ().
   --  Deprecated since 4.10, 1

   procedure Get_Cursor
      (Self         : not null access Gtk_Tree_View_Record;
       Path         : out Gtk.Tree_Model.Gtk_Tree_Path;
       Focus_Column : out Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   pragma Obsolescent (Get_Cursor);
   --  Fills in Path and Focus_Column with the current path and focus column.
   --  If the cursor isn't currently set, then *Path will be null. If no column
   --  currently has focus, then *Focus_Column will be null.
   --  The returned `GtkTreePath` must be freed with Gtk.Tree_Model.Path_Free
   --  when you are done with it.
   --  Deprecated since 4.10, 1
   --  @param Path A pointer to be filled with the current cursor path
   --  @param Focus_Column A pointer to be filled with the current focus
   --  column

   procedure Set_Cursor
      (Self          : not null access Gtk_Tree_View_Record;
       Path          : Gtk.Tree_Model.Gtk_Tree_Path;
       Focus_Column  : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Start_Editing : Boolean);
   pragma Obsolescent (Set_Cursor);
   --  Sets the current keyboard focus to be at Path, and selects it. This is
   --  useful when you want to focus the user's attention on a particular row.
   --  If Focus_Column is not null, then focus is given to the column specified
   --  by it. Additionally, if Focus_Column is specified, and Start_Editing is
   --  True, then editing should be started in the specified cell. This
   --  function is often followed by Gtk_Widget_Grab_Focus (Tree_View) in order
   --  to give keyboard focus to the widget. Please note that editing can only
   --  happen when the widget is realized.
   --  If Path is invalid for Model, the current cursor (if any) will be unset
   --  and the function will return without failing.
   --  Deprecated since 4.10, 1
   --  @param Path A `GtkTreePath`
   --  @param Focus_Column A `GtkTreeViewColumn`
   --  @param Start_Editing True if the specified cell should start being
   --  edited.

   function Get_Dest_Row_At_Pos
      (Self   : not null access Gtk_Tree_View_Record;
       Drag_X : Glib.Gint;
       Drag_Y : Glib.Gint;
       Path   : access Gtk.Tree_Model.Gtk_Tree_Path;
       Pos    : access Gtk_Tree_View_Drop_Position) return Boolean;
   pragma Obsolescent (Get_Dest_Row_At_Pos);
   --  Determines the destination row for a given position. Drag_X and Drag_Y
   --  are expected to be in widget coordinates. This function is only
   --  meaningful if Tree_View is realized. Therefore this function will always
   --  return False if Tree_View is not realized or does not have a model.
   --  Deprecated since 4.10, 1
   --  @param Drag_X the position to determine the destination row for
   --  @param Drag_Y the position to determine the destination row for
   --  @param Path Return location for the path of the highlighted row
   --  @param Pos Return location for the drop position, or null
   --  @return whether there is a row at the given position, True if this is
   --  indeed the case.

   procedure Get_Drag_Dest_Row
      (Self : not null access Gtk_Tree_View_Record;
       Path : out Gtk.Tree_Model.Gtk_Tree_Path;
       Pos  : out Gtk_Tree_View_Drop_Position);
   pragma Obsolescent (Get_Drag_Dest_Row);
   --  Gets information about the row that is highlighted for feedback.
   --  Deprecated since 4.10, 1
   --  @param Path Return location for the path of the highlighted row
   --  @param Pos Return location for the drop position

   procedure Set_Drag_Dest_Row
      (Self : not null access Gtk_Tree_View_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path;
       Pos  : Gtk_Tree_View_Drop_Position);
   pragma Obsolescent (Set_Drag_Dest_Row);
   --  Sets the row that is highlighted for feedback. If Path is null, an
   --  existing highlight is removed.
   --  Deprecated since 4.10, 1
   --  @param Path The path of the row to highlight
   --  @param Pos Specifies whether to drop before, after or into the row

   function Get_Enable_Search
      (Self : not null access Gtk_Tree_View_Record) return Boolean;
   pragma Obsolescent (Get_Enable_Search);
   --  Returns whether or not the tree allows to start interactive searching
   --  by typing in text.
   --  Deprecated since 4.10, 1
   --  @return whether or not to let the user search interactively

   procedure Set_Enable_Search
      (Self          : not null access Gtk_Tree_View_Record;
       Enable_Search : Boolean);
   pragma Obsolescent (Set_Enable_Search);
   --  If Enable_Search is set, then the user can type in text to search
   --  through the tree interactively (this is sometimes called "typeahead
   --  find").
   --  Note that even if this is False, the user can still initiate a search
   --  using the "start-interactive-search" key binding.
   --  Deprecated since 4.10, 1
   --  @param Enable_Search True, if the user can search interactively

   function Get_Enable_Tree_Lines
      (Self : not null access Gtk_Tree_View_Record) return Boolean;
   pragma Obsolescent (Get_Enable_Tree_Lines);
   --  Returns whether or not tree lines are drawn in Tree_View.
   --  Deprecated since 4.10, 1
   --  @return True if tree lines are drawn in Tree_View, False otherwise.

   procedure Set_Enable_Tree_Lines
      (Self    : not null access Gtk_Tree_View_Record;
       Enabled : Boolean);
   pragma Obsolescent (Set_Enable_Tree_Lines);
   --  Sets whether to draw lines interconnecting the expanders in Tree_View.
   --  This does not have any visible effects for lists.
   --  Deprecated since 4.10, 1
   --  @param Enabled True to enable tree line drawing, False otherwise.

   function Get_Expander_Column
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   pragma Obsolescent (Get_Expander_Column);
   --  Returns the column that is the current expander column, or null if none
   --  has been set. This column has the expander arrow drawn next to it.
   --  Deprecated since 4.10, 1
   --  @return The expander column.

   procedure Set_Expander_Column
      (Self   : not null access Gtk_Tree_View_Record;
       Column : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class);
   pragma Obsolescent (Set_Expander_Column);
   --  Sets the column to draw the expander arrow at. It must be in Tree_View.
   --  If Column is null, then the expander arrow is always at the first
   --  visible column.
   --  If you do not want expander arrow to appear in your tree, set the
   --  expander column to a hidden column.
   --  Deprecated since 4.10, 1
   --  @param Column null, or the column to draw the expander arrow at.

   function Get_Fixed_Height_Mode
      (Self : not null access Gtk_Tree_View_Record) return Boolean;
   pragma Obsolescent (Get_Fixed_Height_Mode);
   --  Returns whether fixed height mode is turned on for Tree_View.
   --  Deprecated since 4.10, 1
   --  @return True if Tree_View is in fixed height mode

   procedure Set_Fixed_Height_Mode
      (Self   : not null access Gtk_Tree_View_Record;
       Enable : Boolean);
   pragma Obsolescent (Set_Fixed_Height_Mode);
   --  Enables or disables the fixed height mode of Tree_View. Fixed height
   --  mode speeds up `GtkTreeView` by assuming that all rows have the same
   --  height. Only enable this option if all rows are the same height and all
   --  columns are of type Gtk.Tree_View_Column.Fixed.
   --  Deprecated since 4.10, 1
   --  @param Enable True to enable fixed height mode

   function Get_Grid_Lines
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Enums.Gtk_Tree_View_Grid_Lines;
   pragma Obsolescent (Get_Grid_Lines);
   --  Returns which grid lines are enabled in Tree_View.
   --  Deprecated since 4.10, 1
   --  @return a `GtkTreeView`GridLines value indicating which grid lines are
   --  enabled.

   procedure Set_Grid_Lines
      (Self       : not null access Gtk_Tree_View_Record;
       Grid_Lines : Gtk.Enums.Gtk_Tree_View_Grid_Lines);
   pragma Obsolescent (Set_Grid_Lines);
   --  Sets which grid lines to draw in Tree_View.
   --  Deprecated since 4.10, 1
   --  @param Grid_Lines a `GtkTreeView`GridLines value indicating which grid
   --  lines to enable.

   function Get_Headers_Clickable
      (Self : not null access Gtk_Tree_View_Record) return Boolean;
   pragma Obsolescent (Get_Headers_Clickable);
   --  Returns whether all header columns are clickable.
   --  Deprecated since 4.10, 1
   --  @return True if all header columns are clickable, otherwise False

   procedure Set_Headers_Clickable
      (Self    : not null access Gtk_Tree_View_Record;
       Setting : Boolean);
   pragma Obsolescent (Set_Headers_Clickable);
   --  Allow the column title buttons to be clicked.
   --  Deprecated since 4.10, 1
   --  @param Setting True if the columns are clickable.

   function Get_Headers_Visible
      (Self : not null access Gtk_Tree_View_Record) return Boolean;
   pragma Obsolescent (Get_Headers_Visible);
   --  Returns True if the headers on the Tree_View are visible.
   --  Deprecated since 4.10, 1
   --  @return Whether the headers are visible or not.

   procedure Set_Headers_Visible
      (Self            : not null access Gtk_Tree_View_Record;
       Headers_Visible : Boolean);
   pragma Obsolescent (Set_Headers_Visible);
   --  Sets the visibility state of the headers.
   --  Deprecated since 4.10, 1
   --  @param Headers_Visible True if the headers are visible

   function Get_Hover_Expand
      (Self : not null access Gtk_Tree_View_Record) return Boolean;
   pragma Obsolescent (Get_Hover_Expand);
   --  Returns whether hover expansion mode is turned on for Tree_View.
   --  Deprecated since 4.10, 1
   --  @return True if Tree_View is in hover expansion mode

   procedure Set_Hover_Expand
      (Self   : not null access Gtk_Tree_View_Record;
       Expand : Boolean);
   pragma Obsolescent (Set_Hover_Expand);
   --  Enables or disables the hover expansion mode of Tree_View. Hover
   --  expansion makes rows expand or collapse if the pointer moves over them.
   --  Deprecated since 4.10, 1
   --  @param Expand True to enable hover selection mode

   function Get_Hover_Selection
      (Self : not null access Gtk_Tree_View_Record) return Boolean;
   pragma Obsolescent (Get_Hover_Selection);
   --  Returns whether hover selection mode is turned on for Tree_View.
   --  Deprecated since 4.10, 1
   --  @return True if Tree_View is in hover selection mode

   procedure Set_Hover_Selection
      (Self  : not null access Gtk_Tree_View_Record;
       Hover : Boolean);
   pragma Obsolescent (Set_Hover_Selection);
   --  Enables or disables the hover selection mode of Tree_View. Hover
   --  selection makes the selected row follow the pointer. Currently, this
   --  works only for the selection modes Gtk.Enums.Selection_Single and
   --  Gtk.Enums.Selection_Browse.
   --  Deprecated since 4.10, 1
   --  @param Hover True to enable hover selection mode

   function Get_Level_Indentation
      (Self : not null access Gtk_Tree_View_Record) return Glib.Gint;
   pragma Obsolescent (Get_Level_Indentation);
   --  Returns the amount, in pixels, of extra indentation for child levels in
   --  Tree_View.
   --  Deprecated since 4.10, 1
   --  @return the amount of extra indentation for child levels in Tree_View.
   --  A return value of 0 means that this feature is disabled.

   procedure Set_Level_Indentation
      (Self        : not null access Gtk_Tree_View_Record;
       Indentation : Glib.Gint);
   pragma Obsolescent (Set_Level_Indentation);
   --  Sets the amount of extra indentation for child levels to use in
   --  Tree_View in addition to the default indentation. The value should be
   --  specified in pixels, a value of 0 disables this feature and in this case
   --  only the default indentation will be used. This does not have any
   --  visible effects for lists.
   --  Deprecated since 4.10, 1
   --  @param Indentation the amount, in pixels, of extra indentation in
   --  Tree_View.

   function Get_Model
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model;
   pragma Obsolescent (Get_Model);
   --  Returns the model the `GtkTreeView` is based on. Returns null if the
   --  model is unset.
   --  Deprecated since 4.10, 1
   --  @return A `GtkTreeModel`

   procedure Set_Model
      (Self  : not null access Gtk_Tree_View_Record;
       Model : Gtk.Tree_Model.Gtk_Tree_Model);
   pragma Obsolescent (Set_Model);
   --  Sets the model for a `GtkTreeView`. If the Tree_View already has a
   --  model set, it will remove it before setting the new model. If Model is
   --  null, then it will unset the old model.
   --  Deprecated since 4.10, 1
   --  @param Model The model.

   function Get_N_Columns
      (Self : not null access Gtk_Tree_View_Record) return Guint;
   pragma Obsolescent (Get_N_Columns);
   --  Queries the number of columns in the given Tree_View.
   --  Deprecated since 4.10, 1
   --  @return The number of columns in the Tree_View

   procedure Get_Path_At_Pos
      (Self      : not null access Gtk_Tree_View_Record;
       X         : Glib.Gint;
       Y         : Glib.Gint;
       Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : out Gtk.Tree_View_Column.Gtk_Tree_View_Column;
       Cell_X    : out Glib.Gint;
       Cell_Y    : out Glib.Gint;
       Row_Found : out Boolean);
   pragma Obsolescent (Get_Path_At_Pos);
   --  Finds the path at the point (X, Y), relative to bin_window coordinates.
   --  That is, X and Y are relative to an events coordinates. Widget-relative
   --  coordinates must be converted using
   --  Gtk.Tree_View.Convert_Widget_To_Bin_Window_Coords. It is primarily for
   --  things like popup menus. If Path is non-null, then it will be filled
   --  with the `GtkTreePath` at that point. This path should be freed with
   --  Gtk.Tree_Model.Path_Free. If Column is non-null, then it will be filled
   --  with the column at that point. Cell_X and Cell_Y return the coordinates
   --  relative to the cell background (i.e. the Background_Area passed to
   --  gtk_cell_renderer_render). This function is only meaningful if Tree_View
   --  is realized. Therefore this function will always return False if
   --  Tree_View is not realized or does not have a model.
   --  For converting widget coordinates (eg. the ones you get from
   --  GtkWidget::query-tooltip), please see
   --  Gtk.Tree_View.Convert_Widget_To_Bin_Window_Coords.
   --  Deprecated since 4.10, 1
   --  @param X The x position to be identified (relative to bin_window).
   --  @param Y The y position to be identified (relative to bin_window).
   --  @param Path A pointer to a `GtkTreePath` pointer to be filled in
   --  @param Column A pointer to a `GtkTreeViewColumn` pointer to be filled
   --  in
   --  @param Cell_X A pointer where the X coordinate relative to the cell can
   --  be placed
   --  @param Cell_Y A pointer where the Y coordinate relative to the cell can
   --  be placed
   --  @return True if a row exists at that coordinate.

   function Get_Reorderable
      (Self : not null access Gtk_Tree_View_Record) return Boolean;
   pragma Obsolescent (Get_Reorderable);
   --  Retrieves whether the user can reorder the tree via drag-and-drop. See
   --  Gtk.Tree_View.Set_Reorderable.
   --  Deprecated since 4.10, 1
   --  @return True if the tree can be reordered.

   procedure Set_Reorderable
      (Self        : not null access Gtk_Tree_View_Record;
       Reorderable : Boolean);
   pragma Obsolescent (Set_Reorderable);
   --  This function is a convenience function to allow you to reorder models
   --  that support the `GtkTreeDragSourceIface` and the
   --  `GtkTreeDragDestIface`. Both `GtkTreeStore` and `GtkListStore` support
   --  these. If Reorderable is True, then the user can reorder the model by
   --  dragging and dropping rows. The developer can listen to these changes by
   --  connecting to the model's `GtkTreeModel::row-inserted` and
   --  `GtkTreeModel::row-deleted` signals. The reordering is implemented by
   --  setting up the tree view as a drag source and destination. Therefore,
   --  drag and drop can not be used in a reorderable view for any other
   --  purpose.
   --  This function does not give you any degree of control over the order --
   --  any reordering is allowed. If more control is needed, you should
   --  probably handle drag and drop manually.
   --  Deprecated since 4.10, 1
   --  @param Reorderable True, if the tree can be reordered.

   procedure Get_Row_Separator_Func
      (Self : not null access Gtk_Tree_View_Record);
   pragma Obsolescent (Get_Row_Separator_Func);
   --  Returns the current row separator function.
   --  Deprecated since 4.10, 1
   --  @return the current row separator function.

   procedure Set_Row_Separator_Func
      (Self : not null access Gtk_Tree_View_Record;
       Func : Gtk_Tree_View_Row_Separator_Func);
   pragma Obsolescent (Set_Row_Separator_Func);
   --  Sets the row separator function, which is used to determine whether a
   --  row should be drawn as a separator. If the row separator function is
   --  null, no separators are drawn. This is the default value.
   --  Deprecated since 4.10, 1
   --  @param Func a `GtkTreeView`RowSeparatorFunc

   function Get_Rubber_Banding
      (Self : not null access Gtk_Tree_View_Record) return Boolean;
   pragma Obsolescent (Get_Rubber_Banding);
   --  Returns whether rubber banding is turned on for Tree_View. If the
   --  selection mode is Gtk.Enums.Selection_Multiple, rubber banding will
   --  allow the user to select multiple rows by dragging the mouse.
   --  Deprecated since 4.10, 1
   --  @return True if rubber banding in Tree_View is enabled.

   procedure Set_Rubber_Banding
      (Self   : not null access Gtk_Tree_View_Record;
       Enable : Boolean);
   pragma Obsolescent (Set_Rubber_Banding);
   --  Enables or disables rubber banding in Tree_View. If the selection mode
   --  is Gtk.Enums.Selection_Multiple, rubber banding will allow the user to
   --  select multiple rows by dragging the mouse.
   --  Deprecated since 4.10, 1
   --  @param Enable True to enable rubber banding

   function Get_Search_Column
      (Self : not null access Gtk_Tree_View_Record) return Glib.Gint;
   pragma Obsolescent (Get_Search_Column);
   --  Gets the column searched on by the interactive search code.
   --  Deprecated since 4.10, 1
   --  @return the column the interactive search code searches in.

   procedure Set_Search_Column
      (Self   : not null access Gtk_Tree_View_Record;
       Column : Glib.Gint);
   pragma Obsolescent (Set_Search_Column);
   --  Sets Column as the column where the interactive search code should
   --  search in for the current model.
   --  If the search column is set, users can use the
   --  "start-interactive-search" key binding to bring up search popup. The
   --  enable-search property controls whether simply typing text will also
   --  start an interactive search.
   --  Note that Column refers to a column of the current model. The search
   --  column is reset to -1 when the model is changed.
   --  Deprecated since 4.10, 1
   --  @param Column the column of the model to search in, or -1 to disable
   --  searching

   function Get_Search_Entry
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Editable.Gtk_Editable;
   pragma Obsolescent (Get_Search_Entry);
   --  Returns the `GtkEntry` which is currently in use as interactive search
   --  entry for Tree_View. In case the built-in entry is being used, null will
   --  be returned.
   --  Deprecated since 4.10, 1
   --  @return the entry currently in use as search entry.

   procedure Set_Search_Entry
      (Self   : not null access Gtk_Tree_View_Record;
       GEntry : Gtk.Editable.Gtk_Editable);
   pragma Obsolescent (Set_Search_Entry);
   --  Sets the entry which the interactive search code will use for this
   --  Tree_View. This is useful when you want to provide a search entry in our
   --  interface at all time at a fixed position. Passing null for Entry will
   --  make the interactive search code use the built-in popup entry again.
   --  Deprecated since 4.10, 1
   --  @param GEntry the entry the interactive search code of Tree_View should
   --  use

   procedure Get_Search_Equal_Func
      (Self : not null access Gtk_Tree_View_Record);
   pragma Obsolescent (Get_Search_Equal_Func);
   --  Returns the compare function currently in use.
   --  Deprecated since 4.10, 1
   --  @return the currently used compare function for the search code.

   procedure Set_Search_Equal_Func
      (Self              : not null access Gtk_Tree_View_Record;
       Search_Equal_Func : Gtk_Tree_View_Search_Equal_Func;
       Search_Destroy    : Glib.G_Destroy_Notify_Address);
   pragma Obsolescent (Set_Search_Equal_Func);
   --  Sets the compare function for the interactive search capabilities; note
   --  that somewhat like strcmp returning 0 for equality
   --  `GtkTreeView`SearchEqualFunc returns False on matches.
   --  Deprecated since 4.10, 1
   --  @param Search_Equal_Func the compare function to use during the search
   --  @param Search_Destroy Destroy notifier for Search_User_Data

   function Get_Selection
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Tree_Selection.Gtk_Tree_Selection;
   pragma Obsolescent (Get_Selection);
   --  Gets the `GtkTreeSelection` associated with Tree_View.
   --  Deprecated since 4.10, 1
   --  @return A `GtkTreeSelection` object.

   function Get_Show_Expanders
      (Self : not null access Gtk_Tree_View_Record) return Boolean;
   pragma Obsolescent (Get_Show_Expanders);
   --  Returns whether or not expanders are drawn in Tree_View.
   --  Deprecated since 4.10, 1
   --  @return True if expanders are drawn in Tree_View, False otherwise.

   procedure Set_Show_Expanders
      (Self    : not null access Gtk_Tree_View_Record;
       Enabled : Boolean);
   pragma Obsolescent (Set_Show_Expanders);
   --  Sets whether to draw and enable expanders and indent child rows in
   --  Tree_View. When disabled there will be no expanders visible in trees and
   --  there will be no way to expand and collapse rows by default. Also note
   --  that hiding the expanders will disable the default indentation. You can
   --  set a custom indentation in this case using
   --  Gtk.Tree_View.Set_Level_Indentation. This does not have any visible
   --  effects for lists.
   --  Deprecated since 4.10, 1
   --  @param Enabled True to enable expander drawing, False otherwise.

   function Get_Tooltip_Column
      (Self : not null access Gtk_Tree_View_Record) return Glib.Gint;
   pragma Obsolescent (Get_Tooltip_Column);
   --  Returns the column of Tree_View's model which is being used for
   --  displaying tooltips on Tree_View's rows.
   --  Deprecated since 4.10, 1
   --  @return the index of the tooltip column that is currently being used,
   --  or -1 if this is disabled.

   procedure Set_Tooltip_Column
      (Self   : not null access Gtk_Tree_View_Record;
       Column : Glib.Gint);
   pragma Obsolescent (Set_Tooltip_Column);
   --  If you only plan to have simple (text-only) tooltips on full rows, you
   --  can use this function to have `GtkTreeView` handle these automatically
   --  for you. Column should be set to the column in Tree_View's model
   --  containing the tooltip texts, or -1 to disable this feature.
   --  When enabled, `GtkWidget:has-tooltip` will be set to True and Tree_View
   --  will connect a `GtkWidget::query-tooltip` signal handler.
   --  Note that the signal handler sets the text with Gtk.Tooltip.Set_Markup,
   --  so &, <, etc have to be escaped in the text.
   --  Deprecated since 4.10, 1
   --  @param Column an integer, which is a valid column number for
   --  Tree_View's model

   procedure Get_Tooltip_Context
      (Self         : not null access Gtk_Tree_View_Record;
       X            : Glib.Gint;
       Y            : Glib.Gint;
       Keyboard_Tip : Boolean;
       Model        : out Gtk.Tree_Model.Gtk_Tree_Model;
       Path         : out Gtk.Tree_Model.Gtk_Tree_Path;
       Iter         : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Success      : out Boolean);
   pragma Obsolescent (Get_Tooltip_Context);
   --  This function is supposed to be used in a ::query-tooltip signal
   --  handler for `GtkTreeView`. The X, Y and Keyboard_Tip values which are
   --  received in the signal handler, should be passed to this function
   --  without modification.
   --  The return value indicates whether there is a tree view row at the
   --  given coordinates (True) or not (False) for mouse tooltips. For keyboard
   --  tooltips the row returned will be the cursor row. When True, then any of
   --  Model, Path and Iter which have been provided will be set to point to
   --  that row and the corresponding model. X and Y will always be converted
   --  to be relative to Tree_View's bin_window if Keyboard_Tooltip is False.
   --  Deprecated since 4.10, 1
   --  @param X the x coordinate (relative to widget coordinates)
   --  @param Y the y coordinate (relative to widget coordinates)
   --  @param Keyboard_Tip whether this is a keyboard tooltip or not
   --  @param Model a pointer to receive a `GtkTreeModel`
   --  @param Path a pointer to receive a `GtkTreePath`
   --  @param Iter a pointer to receive a `GtkTreeIter`
   --  @return whether or not the given tooltip context points to a row

   procedure Get_Visible_Range
      (Self       : not null access Gtk_Tree_View_Record;
       Start_Path : out Gtk.Tree_Model.Gtk_Tree_Path;
       End_Path   : out Gtk.Tree_Model.Gtk_Tree_Path;
       Success    : out Boolean);
   pragma Obsolescent (Get_Visible_Range);
   --  Sets Start_Path and End_Path to be the first and last visible path.
   --  Note that there may be invisible paths in between.
   --  The paths should be freed with Gtk.Tree_Model.Path_Free after use.
   --  Deprecated since 4.10, 1
   --  @param Start_Path Return location for start of region
   --  @param End_Path Return location for end of region
   --  @return True, if valid paths were placed in Start_Path and End_Path.

   procedure Get_Visible_Rect
      (Self         : not null access Gtk_Tree_View_Record;
       Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle);
   pragma Obsolescent (Get_Visible_Rect);
   --  Fills Visible_Rect with the currently-visible region of the buffer, in
   --  tree coordinates. Convert to bin_window coordinates with
   --  Gtk.Tree_View.Convert_Tree_To_Bin_Window_Coords. Tree coordinates start
   --  at 0,0 for row 0 of the tree, and cover the entire scrollable area of
   --  the tree.
   --  Deprecated since 4.10, 1
   --  @param Visible_Rect rectangle to fill

   function Insert_Column
      (Self     : not null access Gtk_Tree_View_Record;
       Column   : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Position : Glib.Gint := -1) return Glib.Gint;
   pragma Obsolescent (Insert_Column);
   --  This inserts the Column into the Tree_View at Position. If Position is
   --  -1, then the column is inserted at the end. If Tree_View has
   --  "fixed_height" mode enabled, then Column must have its "sizing" property
   --  set to be GTK_TREE_VIEW_COLUMN_FIXED.
   --  Deprecated since 4.10, 1
   --  @param Column The `GtkTreeViewColumn` to be inserted.
   --  @param Position The position to insert Column in.
   --  @return The number of columns in Tree_View after insertion.

   function Insert_Column_With_Data_Func
      (Self     : not null access Gtk_Tree_View_Record;
       Position : Glib.Gint;
       Title    : UTF8_String;
       Cell     : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func     : Gtk_Tree_Cell_Data_Func;
       Dnotify  : Glib.G_Destroy_Notify_Address) return Glib.Gint;
   pragma Obsolescent (Insert_Column_With_Data_Func);
   --  Convenience function that inserts a new column into the `GtkTreeView`
   --  with the given cell renderer and a `GtkTreeCellDataFunc` to set cell
   --  renderer attributes (normally using data from the model). See also
   --  gtk_tree_view_column_set_cell_data_func,
   --  gtk_tree_view_column_pack_start. If Tree_View has "fixed_height" mode
   --  enabled, then the new column will have its "sizing" property set to be
   --  GTK_TREE_VIEW_COLUMN_FIXED.
   --  Deprecated since 4.10, 1
   --  @param Position Position to insert, -1 for append
   --  @param Title column title
   --  @param Cell cell renderer for column
   --  @param Func function to set attributes of cell renderer
   --  @param Dnotify destroy notifier for Data
   --  @return number of columns in the tree view post-insert

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Insert_Column_With_Data_Func_User_Data is

      type Gtk_Tree_Cell_Data_Func is access procedure
        (Tree_Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
         Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
         Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data        : User_Data_Type);
      --  A function to set the properties of a cell instead of just using the
      --  straight mapping between the cell and the model.
      --  This function is useful for customizing the cell renderer. For example,
      --  a function might get an* integer from the Tree_Model, and render it to
      --  the "text" attribute of "cell" by converting it to its written
      --  equivalent.
      --  See also: gtk_tree_view_column_set_cell_data_func
      --  @param Tree_Column A `GtkTreeViewColumn`
      --  @param Cell The `GtkCellRenderer` that is being rendered by Tree_Column
      --  @param Tree_Model The `GtkTreeModel` being rendered
      --  @param Iter A `GtkTreeIter` of the current row rendered
      --  @param Data user data

      function Insert_Column_With_Data_Func
         (Self     : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Position : Glib.Gint;
          Title    : UTF8_String;
          Cell     : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func     : Gtk_Tree_Cell_Data_Func;
          Data     : User_Data_Type;
          Dnotify  : Glib.G_Destroy_Notify_Address) return Glib.Gint;
      pragma Obsolescent (Insert_Column_With_Data_Func);
      --  Convenience function that inserts a new column into the
      --  `GtkTreeView` with the given cell renderer and a
      --  `GtkTreeCellDataFunc` to set cell renderer attributes (normally using
      --  data from the model). See also
      --  gtk_tree_view_column_set_cell_data_func,
      --  gtk_tree_view_column_pack_start. If Tree_View has "fixed_height" mode
      --  enabled, then the new column will have its "sizing" property set to
      --  be GTK_TREE_VIEW_COLUMN_FIXED.
      --  Deprecated since 4.10, 1
      --  @param Position Position to insert, -1 for append
      --  @param Title column title
      --  @param Cell cell renderer for column
      --  @param Func function to set attributes of cell renderer
      --  @param Data data for Func
      --  @param Dnotify destroy notifier for Data
      --  @return number of columns in the tree view post-insert

   end Insert_Column_With_Data_Func_User_Data;

   function Is_Blank_At_Pos
      (Self   : not null access Gtk_Tree_View_Record;
       X      : Glib.Gint;
       Y      : Glib.Gint;
       Path   : access Gtk.Tree_Model.Gtk_Tree_Path;
       Column : access Gtk.Tree_View_Column.Gtk_Tree_View_Column;
       Cell_X : access Glib.Gint;
       Cell_Y : access Glib.Gint) return Boolean;
   pragma Obsolescent (Is_Blank_At_Pos);
   --  Determine whether the point (X, Y) in Tree_View is blank, that is no
   --  cell content nor an expander arrow is drawn at the location. If so, the
   --  location can be considered as the background. You might wish to take
   --  special action on clicks on the background, such as clearing a current
   --  selection, having a custom context menu or starting rubber banding.
   --  The X and Y coordinate that are provided must be relative to bin_window
   --  coordinates. Widget-relative coordinates must be converted using
   --  Gtk.Tree_View.Convert_Widget_To_Bin_Window_Coords.
   --  For converting widget coordinates (eg. the ones you get from
   --  GtkWidget::query-tooltip), please see
   --  Gtk.Tree_View.Convert_Widget_To_Bin_Window_Coords.
   --  The Path, Column, Cell_X and Cell_Y arguments will be filled in
   --  likewise as for Gtk.Tree_View.Get_Path_At_Pos. Please see
   --  Gtk.Tree_View.Get_Path_At_Pos for more information.
   --  Deprecated since 4.10, 1
   --  @param X The x position to be identified (relative to bin_window)
   --  @param Y The y position to be identified (relative to bin_window)
   --  @param Path A pointer to a `GtkTreePath` pointer to be filled in
   --  @param Column A pointer to a `GtkTreeViewColumn` pointer to be filled
   --  in
   --  @param Cell_X A pointer where the X coordinate relative to the cell can
   --  be placed
   --  @param Cell_Y A pointer where the Y coordinate relative to the cell can
   --  be placed
   --  @return True if the area at the given coordinates is blank, False
   --  otherwise.

   function Is_Rubber_Banding_Active
      (Self : not null access Gtk_Tree_View_Record) return Boolean;
   pragma Obsolescent (Is_Rubber_Banding_Active);
   --  Returns whether a rubber banding operation is currently being done in
   --  Tree_View.
   --  Deprecated since 4.10, 1
   --  @return True if a rubber banding operation is currently being done in
   --  Tree_View.

   procedure Map_Expanded_Rows
      (Self : not null access Gtk_Tree_View_Record;
       Func : Gtk_Tree_View_Mapping_Func);
   pragma Obsolescent (Map_Expanded_Rows);
   --  Calls Func on all expanded rows.
   --  Deprecated since 4.10, 1
   --  @param Func A function to be called

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Map_Expanded_Rows_User_Data is

      type Gtk_Tree_View_Mapping_Func is access procedure
        (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         User_Data : User_Data_Type);
      --  Function used for Gtk.Tree_View.Map_Expanded_Rows.
      --  @param Tree_View A `GtkTreeView`
      --  @param Path The path that's expanded
      --  @param User_Data user data

      procedure Map_Expanded_Rows
         (Self : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Func : Gtk_Tree_View_Mapping_Func;
          Data : User_Data_Type);
      pragma Obsolescent (Map_Expanded_Rows);
      --  Calls Func on all expanded rows.
      --  Deprecated since 4.10, 1
      --  @param Func A function to be called
      --  @param Data User data to be passed to the function.

   end Map_Expanded_Rows_User_Data;

   procedure Move_Column_After
      (Self        : not null access Gtk_Tree_View_Record;
       Column      : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Base_Column : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class);
   pragma Obsolescent (Move_Column_After);
   --  Moves Column to be after to Base_Column. If Base_Column is null, then
   --  Column is placed in the first position.
   --  Deprecated since 4.10, 1
   --  @param Column The `GtkTreeViewColumn` to be moved.
   --  @param Base_Column The `GtkTreeViewColumn` to be moved relative to

   function Remove_Column
      (Self   : not null access Gtk_Tree_View_Record;
       Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class)
       return Glib.Gint;
   pragma Obsolescent (Remove_Column);
   --  Removes Column from Tree_View.
   --  Deprecated since 4.10, 1
   --  @param Column The `GtkTreeViewColumn` to remove.
   --  @return The number of columns in Tree_View after removing.

   procedure Row_Activated
      (Self   : not null access Gtk_Tree_View_Record;
       Path   : Gtk.Tree_Model.Gtk_Tree_Path;
       Column : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class);
   pragma Obsolescent (Row_Activated);
   --  Activates the cell determined by Path and Column.
   --  Deprecated since 4.10, 1
   --  @param Path The `GtkTreePath` to be activated.
   --  @param Column The `GtkTreeViewColumn` to be activated.

   function Row_Expanded
      (Self : not null access Gtk_Tree_View_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   pragma Obsolescent (Row_Expanded);
   --  Returns True if the node pointed to by Path is expanded in Tree_View.
   --  Deprecated since 4.10, 1
   --  @param Path A `GtkTreePath` to test expansion state.
   --  @return True if path is expanded.

   procedure Scroll_To_Cell
      (Self      : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Use_Align : Boolean;
       Row_Align : Interfaces.C.C_float;
       Col_Align : Interfaces.C.C_float);
   pragma Obsolescent (Scroll_To_Cell);
   --  Moves the alignments of Tree_View to the position specified by Column
   --  and Path. If Column is null, then no horizontal scrolling occurs.
   --  Likewise, if Path is null no vertical scrolling occurs. At a minimum,
   --  one of Column or Path need to be non-null. Row_Align determines where
   --  the row is placed, and Col_Align determines where Column is placed. Both
   --  are expected to be between 0.0 and 1.0. 0.0 means left/top alignment,
   --  1.0 means right/bottom alignment, 0.5 means center.
   --  If Use_Align is False, then the alignment arguments are ignored, and
   --  the tree does the minimum amount of work to scroll the cell onto the
   --  screen. This means that the cell will be scrolled to the edge closest to
   --  its current position. If the cell is currently visible on the screen,
   --  nothing is done.
   --  This function only works if the model is set, and Path is a valid row
   --  on the model. If the model changes before the Tree_View is realized, the
   --  centered path will be modified to reflect this change.
   --  Deprecated since 4.10, 1
   --  @param Path The path of the row to move to
   --  @param Column The `GtkTreeViewColumn` to move horizontally to
   --  @param Use_Align whether to use alignment arguments, or False.
   --  @param Row_Align The vertical alignment of the row specified by Path.
   --  @param Col_Align The horizontal alignment of the column specified by
   --  Column.

   procedure Scroll_To_Point
      (Self   : not null access Gtk_Tree_View_Record;
       Tree_X : Glib.Gint;
       Tree_Y : Glib.Gint);
   pragma Obsolescent (Scroll_To_Point);
   --  Scrolls the tree view such that the top-left corner of the visible area
   --  is Tree_X, Tree_Y, where Tree_X and Tree_Y are specified in tree
   --  coordinates. The Tree_View must be realized before this function is
   --  called. If it isn't, you probably want to be using
   --  Gtk.Tree_View.Scroll_To_Cell.
   --  If either Tree_X or Tree_Y are -1, then that direction isn't scrolled.
   --  Deprecated since 4.10, 1
   --  @param Tree_X X coordinate of new top-left pixel of visible area, or -1
   --  @param Tree_Y Y coordinate of new top-left pixel of visible area, or -1

   procedure Set_Column_Drag_Function
      (Self : not null access Gtk_Tree_View_Record;
       Func : Gtk_Tree_View_Column_Drop_Func);
   pragma Obsolescent (Set_Column_Drag_Function);
   --  Sets a user function for determining where a column may be dropped when
   --  dragged. This function is called on every column pair in turn at the
   --  beginning of a column drag to determine where a drop can take place. The
   --  arguments passed to Func are: the Tree_View, the `GtkTreeViewColumn`
   --  being dragged, the two `GtkTreeViewColumn`s determining the drop spot,
   --  and User_Data. If either of the `GtkTreeViewColumn` arguments for the
   --  drop spot are null, then they indicate an edge. If Func is set to be
   --  null, then Tree_View reverts to the default behavior of allowing all
   --  columns to be dropped everywhere.
   --  Deprecated since 4.10, 1
   --  @param Func A function to determine which columns are reorderable

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Column_Drag_Function_User_Data is

      type Gtk_Tree_View_Column_Drop_Func is access function
        (Tree_View   : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
         Column      : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
         Prev_Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
         Next_Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
         Data        : User_Data_Type) return Boolean;
      --  Function type for determining whether Column can be dropped in a
      --  particular spot (as determined by Prev_Column and Next_Column). In left
      --  to right locales, Prev_Column is on the left of the potential drop spot,
      --  and Next_Column is on the right. In right to left mode, this is
      --  reversed. This function should return True if the spot is a valid drop
      --  spot. Please note that returning True does not actually indicate that
      --  the column drop was made, but is meant only to indicate a possible drop
      --  spot to the user.
      --  @param Tree_View A `GtkTreeView`
      --  @param Column The `GtkTreeViewColumn` being dragged
      --  @param Prev_Column A `GtkTreeViewColumn` on one side of Column
      --  @param Next_Column A `GtkTreeViewColumn` on the other side of Column
      --  @param Data user data
      --  @return True, if Column can be dropped in this spot

      procedure Set_Column_Drag_Function
         (Self      : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Func      : Gtk_Tree_View_Column_Drop_Func;
          User_Data : User_Data_Type);
      pragma Obsolescent (Set_Column_Drag_Function);
      --  Sets a user function for determining where a column may be dropped
      --  when dragged. This function is called on every column pair in turn at
      --  the beginning of a column drag to determine where a drop can take
      --  place. The arguments passed to Func are: the Tree_View, the
      --  `GtkTreeViewColumn` being dragged, the two `GtkTreeViewColumn`s
      --  determining the drop spot, and User_Data. If either of the
      --  `GtkTreeViewColumn` arguments for the drop spot are null, then they
      --  indicate an edge. If Func is set to be null, then Tree_View reverts
      --  to the default behavior of allowing all columns to be dropped
      --  everywhere.
      --  Deprecated since 4.10, 1
      --  @param Func A function to determine which columns are reorderable
      --  @param User_Data User data to be passed to Func

   end Set_Column_Drag_Function_User_Data;

   procedure Set_Cursor_On_Cell
      (Self          : not null access Gtk_Tree_View_Record;
       Path          : Gtk.Tree_Model.Gtk_Tree_Path;
       Focus_Column  : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Focus_Cell    : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Start_Editing : Boolean);
   pragma Obsolescent (Set_Cursor_On_Cell);
   --  Sets the current keyboard focus to be at Path, and selects it. This is
   --  useful when you want to focus the user's attention on a particular row.
   --  If Focus_Column is not null, then focus is given to the column specified
   --  by it. If Focus_Column and Focus_Cell are not null, and Focus_Column
   --  contains 2 or more editable or activatable cells, then focus is given to
   --  the cell specified by Focus_Cell. Additionally, if Focus_Column is
   --  specified, and Start_Editing is True, then editing should be started in
   --  the specified cell. This function is often followed by
   --  Gtk_Widget_Grab_Focus (Tree_View) in order to give keyboard focus to the
   --  widget. Please note that editing can only happen when the widget is
   --  realized.
   --  If Path is invalid for Model, the current cursor (if any) will be unset
   --  and the function will return without failing.
   --  Deprecated since 4.10, 1
   --  @param Path A `GtkTreePath`
   --  @param Focus_Column A `GtkTreeViewColumn`
   --  @param Focus_Cell A `GtkCellRenderer`
   --  @param Start_Editing True if the specified cell should start being
   --  edited.

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Row_Separator_Func_User_Data is

      type Gtk_Tree_View_Row_Separator_Func is access function
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : User_Data_Type) return Boolean;
      --  Function type for determining whether the row pointed to by Iter should
      --  be rendered as a separator. A common way to implement this is to have a
      --  boolean column in the model, whose values the
      --  `GtkTreeViewRowSeparatorFunc` returns.
      --  @param Model the `GtkTreeModel`
      --  @param Iter a `GtkTreeIter` pointing at a row in Model
      --  @param Data user data
      --  @return True if the row is a separator

      procedure Set_Row_Separator_Func
         (Self : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Func : Gtk_Tree_View_Row_Separator_Func;
          Data : User_Data_Type);
      pragma Obsolescent (Set_Row_Separator_Func);
      --  Sets the row separator function, which is used to determine whether
      --  a row should be drawn as a separator. If the row separator function
      --  is null, no separators are drawn. This is the default value.
      --  Deprecated since 4.10, 1
      --  @param Func a `GtkTreeView`RowSeparatorFunc
      --  @param Data user data to pass to Func

   end Set_Row_Separator_Func_User_Data;

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Search_Equal_Func_User_Data is

      type Gtk_Tree_View_Search_Equal_Func is access function
        (Model       : Gtk.Tree_Model.Gtk_Tree_Model;
         Column      : Glib.Gint;
         Key         : UTF8_String;
         Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
         Search_Data : User_Data_Type) return Boolean;
      --  A function used for checking whether a row in Model matches a search
      --  key string entered by the user. Note the return value is reversed from
      --  what you would normally expect, though it has some similarity to strcmp
      --  returning 0 for equal strings.
      --  @param Model the `GtkTreeModel` being searched
      --  @param Column the search column set by Gtk.Tree_View.Set_Search_Column
      --  @param Key the key string to compare with
      --  @param Iter a `GtkTreeIter` pointing the row of Model that should be
      --  compared with Key.
      --  @param Search_Data user data from Gtk.Tree_View.Set_Search_Equal_Func
      --  @return False if the row matches, True otherwise.

      procedure Set_Search_Equal_Func
         (Self              : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Search_Equal_Func : Gtk_Tree_View_Search_Equal_Func;
          Search_User_Data  : User_Data_Type;
          Search_Destroy    : Glib.G_Destroy_Notify_Address);
      pragma Obsolescent (Set_Search_Equal_Func);
      --  Sets the compare function for the interactive search capabilities;
      --  note that somewhat like strcmp returning 0 for equality
      --  `GtkTreeView`SearchEqualFunc returns False on matches.
      --  Deprecated since 4.10, 1
      --  @param Search_Equal_Func the compare function to use during the
      --  search
      --  @param Search_User_Data user data to pass to Search_Equal_Func
      --  @param Search_Destroy Destroy notifier for Search_User_Data

   end Set_Search_Equal_Func_User_Data;

   procedure Set_Tooltip_Cell
      (Self    : not null access Gtk_Tree_View_Record;
       Tooltip : not null access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
       Path    : Gtk.Tree_Model.Gtk_Tree_Path;
       Column  : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Cell    : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   pragma Obsolescent (Set_Tooltip_Cell);
   --  Sets the tip area of Tooltip to the area Path, Column and Cell have in
   --  common. For example if Path is null and Column is set, the tip area will
   --  be set to the full area covered by Column. See also
   --  Gtk.Tooltip.Set_Tip_Area.
   --  Note that if Path is not specified and Cell is set and part of a column
   --  containing the expander, the tooltip might not show and hide at the
   --  correct position. In such cases Path must be set to the current node
   --  under the mouse cursor for this function to operate correctly.
   --  See also Gtk.Tree_View.Set_Tooltip_Column for a simpler alternative.
   --  Deprecated since 4.10, 1
   --  @param Tooltip a `GtkTooltip`
   --  @param Path a `GtkTreePath`
   --  @param Column a `GtkTreeViewColumn`
   --  @param Cell a `GtkCellRenderer`

   procedure Set_Tooltip_Row
      (Self    : not null access Gtk_Tree_View_Record;
       Tooltip : not null access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
       Path    : Gtk.Tree_Model.Gtk_Tree_Path);
   pragma Obsolescent (Set_Tooltip_Row);
   --  Sets the tip area of Tooltip to be the area covered by the row at Path.
   --  See also Gtk.Tree_View.Set_Tooltip_Column for a simpler alternative. See
   --  also Gtk.Tooltip.Set_Tip_Area.
   --  Deprecated since 4.10, 1
   --  @param Tooltip a `GtkTooltip`
   --  @param Path a `GtkTreePath`

   procedure Unset_Rows_Drag_Dest
      (Self : not null access Gtk_Tree_View_Record);
   pragma Obsolescent (Unset_Rows_Drag_Dest);
   --  Undoes the effect of gtk_tree_view_enable_model_drag_dest. Calling this
   --  method sets `GtkTreeView`:reorderable to False.
   --  Deprecated since 4.10, 1

   procedure Unset_Rows_Drag_Source
      (Self : not null access Gtk_Tree_View_Record);
   pragma Obsolescent (Unset_Rows_Drag_Source);
   --  Undoes the effect of gtk_tree_view_enable_model_drag_source. Calling
   --  this method sets `GtkTreeView`:reorderable to False.
   --  Deprecated since 4.10, 1

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Gtk_New
     (Tree_View : out Gtk_Tree_View;
      Model     : access Gtk.Tree_Model.Gtk_Root_Tree_Model_Record'Class);
   --  A convenience function so that one can directly pass a model, without
   --  converting to a Gtk_Tree_Model via the "+" operator.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Tree_View_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Tree_View_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Tree_View_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Tree_View_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Tree_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Tree_View_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Tree_View_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Tree_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Tree_View_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Tree_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Activate_On_Single_Click_Property : constant Glib.Properties.Property_Boolean;
   --  The activate-on-single-click property specifies whether the
   --  "row-activated" signal will be emitted after a single click.

   Enable_Grid_Lines_Property : constant Gtk.Enums.Property_Gtk_Tree_View_Grid_Lines;

   Enable_Search_Property : constant Glib.Properties.Property_Boolean;

   Enable_Tree_Lines_Property : constant Glib.Properties.Property_Boolean;

   Expander_Column_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Tree_View_Column.Gtk_Tree_View_Column

   Fixed_Height_Mode_Property : constant Glib.Properties.Property_Boolean;
   --  Setting the ::fixed-height-mode property to True speeds up
   --  `GtkTreeView` by assuming that all rows have the same height. Only
   --  enable this option if all rows are the same height. Please see
   --  Gtk.Tree_View.Set_Fixed_Height_Mode for more information on this option.

   Headers_Clickable_Property : constant Glib.Properties.Property_Boolean;

   Headers_Visible_Property : constant Glib.Properties.Property_Boolean;

   Hover_Expand_Property : constant Glib.Properties.Property_Boolean;
   --  Enables or disables the hover expansion mode of Tree_View. Hover
   --  expansion makes rows expand or collapse if the pointer moves over them.
   --
   --  This mode is primarily intended for treeviews in popups, e.g. in
   --  `GtkComboBox` or `GtkEntryCompletion`.

   Hover_Selection_Property : constant Glib.Properties.Property_Boolean;
   --  Enables or disables the hover selection mode of Tree_View. Hover
   --  selection makes the selected row follow the pointer. Currently, this
   --  works only for the selection modes Gtk.Enums.Selection_Single and
   --  Gtk.Enums.Selection_Browse.
   --
   --  This mode is primarily intended for treeviews in popups, e.g. in
   --  `GtkComboBox` or `GtkEntryCompletion`.

   Level_Indentation_Property : constant Glib.Properties.Property_Int;
   --  Extra indentation for each level.

   Model_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Tree_Model.Gtk_Tree_Model

   Reorderable_Property : constant Glib.Properties.Property_Boolean;

   Rubber_Banding_Property : constant Glib.Properties.Property_Boolean;

   Search_Column_Property : constant Glib.Properties.Property_Int;

   Show_Expanders_Property : constant Glib.Properties.Property_Boolean;
   --  True if the view has expanders.

   Tooltip_Column_Property : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Tree_View_Void is not null access procedure (Self : access Gtk_Tree_View_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Columns_Changed : constant Glib.Signal_Name := "columns-changed";
   procedure On_Columns_Changed
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Void;
       After : Boolean := False);
   procedure On_Columns_Changed
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The number of columns of the treeview has changed.

   Signal_Cursor_Changed : constant Glib.Signal_Name := "cursor-changed";
   procedure On_Cursor_Changed
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Void;
       After : Boolean := False);
   procedure On_Cursor_Changed
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The position of the cursor (focused cell) has changed.

   type Cb_Gtk_Tree_View_Boolean_Boolean_Boolean_Boolean is not null access function
     (Self   : access Gtk_Tree_View_Record'Class;
      Object : Boolean;
      P0     : Boolean;
      P1     : Boolean) return Boolean;

   type Cb_GObject_Boolean_Boolean_Boolean_Boolean is not null access function
     (Self   : access Glib.Object.GObject_Record'Class;
      Object : Boolean;
      P0     : Boolean;
      P1     : Boolean) return Boolean;

   Signal_Expand_Collapse_Cursor_Row : constant Glib.Signal_Name := "expand-collapse-cursor-row";
   procedure On_Expand_Collapse_Cursor_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Boolean_Boolean_Boolean_Boolean;
       After : Boolean := False);
   procedure On_Expand_Collapse_Cursor_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Boolean_Boolean_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   -- 
   --  Callback parameters:

   type Cb_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean_Boolean_Boolean is not null access function
     (Self      : access Gtk_Tree_View_Record'Class;
      Step      : Gtk.Enums.Gtk_Movement_Step;
      Direction : Glib.Gint;
      Extend    : Boolean;
      Modify    : Boolean) return Boolean;

   type Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Boolean_Boolean is not null access function
     (Self      : access Glib.Object.GObject_Record'Class;
      Step      : Gtk.Enums.Gtk_Movement_Step;
      Direction : Glib.Gint;
      Extend    : Boolean;
      Modify    : Boolean) return Boolean;

   Signal_Move_Cursor : constant Glib.Signal_Name := "move-cursor";
   procedure On_Move_Cursor
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean_Boolean_Boolean;
       After : Boolean := False);
   procedure On_Move_Cursor
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The `GtkTreeView`::move-cursor signal is a [keybinding
   --  signal][classGtk.SignalAction] which gets emitted when the user presses
   --  one of the cursor keys.
   --
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control the cursor
   --  programmatically. In contrast to Gtk.Tree_View.Set_Cursor and
   --  Gtk.Tree_View.Set_Cursor_On_Cell when moving horizontally
   --  `GtkTreeView`::move-cursor does not reset the current selection.
   -- 
   --  Callback parameters:
   --    --  @param Step the granularity of the move, as a `GtkMovementStep`.
   --    --  Gtk.Enums.Movement_Logical_Positions,
   --    --  Gtk.Enums.Movement_Visual_Positions, Gtk.Enums.Movement_Display_Lines,
   --    --  Gtk.Enums.Movement_Pages and Gtk.Enums.Movement_Buffer_Ends are
   --    --  supported. Gtk.Enums.Movement_Logical_Positions and
   --    --  Gtk.Enums.Movement_Visual_Positions are treated identically.
   --    --  @param Direction the direction to move: +1 to move forwards; -1 to move
   --    --  backwards. The resulting movement is undefined for all other values.
   --    --  @param Extend whether to extend the selection
   --    --  @param Modify whether to modify the selection

   type Cb_Gtk_Tree_View_Gtk_Tree_Path_Gtk_Tree_View_Column_Void is not null access procedure
     (Self   : access Gtk_Tree_View_Record'Class;
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Column : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class);

   type Cb_GObject_Gtk_Tree_Path_Gtk_Tree_View_Column_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Column : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class);

   Signal_Row_Activated : constant Glib.Signal_Name := "row-activated";
   procedure On_Row_Activated
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Gtk_Tree_Path_Gtk_Tree_View_Column_Void;
       After : Boolean := False);
   procedure On_Row_Activated
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_View_Column_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The "row-activated" signal is emitted when the method
   --  [methodGtk.TreeView.row_activated] is called.
   --
   --  This signal is emitted when the user double-clicks a treeview row with
   --  the [propertyGtk.TreeView:activate-on-single-click] property set to
   --  False, or when the user single-clicks a row when that property set to
   --  True.
   --
   --  This signal is also emitted when a non-editable row is selected and one
   --  of the keys: <kbd>Space</kbd>, <kbd>Shift</kbd>+<kbd>Space</kbd>,
   --  <kbd>Return</kbd> or <kbd>Enter</kbd> is pressed.
   --
   --  For selection handling refer to the [tree widget conceptual
   --  overview](section-tree-widget.html) as well as `GtkTreeSelection`.
   -- 
   --  Callback parameters:
   --    --  @param Path the `GtkTreePath` for the activated row
   --    --  @param Column the `GtkTreeViewColumn` in which the activation occurred

   type Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void is not null access procedure
     (Self : access Gtk_Tree_View_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Path : Gtk.Tree_Model.Gtk_Tree_Path);

   type Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Path : Gtk.Tree_Model.Gtk_Tree_Path);

   Signal_Row_Collapsed : constant Glib.Signal_Name := "row-collapsed";
   procedure On_Row_Collapsed
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void;
       After : Boolean := False);
   procedure On_Row_Collapsed
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The given row has been collapsed (child nodes are hidden).
   -- 
   --  Callback parameters:
   --    --  @param Iter the tree iter of the collapsed row
   --    --  @param Path a tree path that points to the row

   Signal_Row_Expanded : constant Glib.Signal_Name := "row-expanded";
   procedure On_Row_Expanded
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void;
       After : Boolean := False);
   procedure On_Row_Expanded
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The given row has been expanded (child nodes are shown).
   -- 
   --  Callback parameters:
   --    --  @param Iter the tree iter of the expanded row
   --    --  @param Path a tree path that points to the row

   type Cb_Gtk_Tree_View_Boolean is not null access function
     (Self : access Gtk_Tree_View_Record'Class) return Boolean;

   type Cb_GObject_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class)
   return Boolean;

   Signal_Select_All : constant Glib.Signal_Name := "select-all";
   procedure On_Select_All
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Boolean;
       After : Boolean := False);
   procedure On_Select_All
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   Signal_Select_Cursor_Parent : constant Glib.Signal_Name := "select-cursor-parent";
   procedure On_Select_Cursor_Parent
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Boolean;
       After : Boolean := False);
   procedure On_Select_Cursor_Parent
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   type Cb_Gtk_Tree_View_Boolean_Boolean is not null access function
     (Self   : access Gtk_Tree_View_Record'Class;
      Object : Boolean) return Boolean;

   type Cb_GObject_Boolean_Boolean is not null access function
     (Self   : access Glib.Object.GObject_Record'Class;
      Object : Boolean) return Boolean;

   Signal_Select_Cursor_Row : constant Glib.Signal_Name := "select-cursor-row";
   procedure On_Select_Cursor_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Boolean_Boolean;
       After : Boolean := False);
   procedure On_Select_Cursor_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   Signal_Start_Interactive_Search : constant Glib.Signal_Name := "start-interactive-search";
   procedure On_Start_Interactive_Search
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Boolean;
       After : Boolean := False);
   procedure On_Start_Interactive_Search
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   type Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean is not null access function
     (Self : access Gtk_Tree_View_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;

   type Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;

   Signal_Test_Collapse_Row : constant Glib.Signal_Name := "test-collapse-row";
   procedure On_Test_Collapse_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean;
       After : Boolean := False);
   procedure On_Test_Collapse_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The given row is about to be collapsed (hide its children nodes). Use
   --  this signal if you need to control the collapsibility of individual
   --  rows.
   -- 
   --  Callback parameters:
   --    --  @param Iter the tree iter of the row to collapse
   --    --  @param Path a tree path that points to the row

   Signal_Test_Expand_Row : constant Glib.Signal_Name := "test-expand-row";
   procedure On_Test_Expand_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean;
       After : Boolean := False);
   procedure On_Test_Expand_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The given row is about to be expanded (show its children nodes). Use
   --  this signal if you need to control the expandability of individual rows.
   -- 
   --  Callback parameters:
   --    --  @param Iter the tree iter of the row to expand
   --    --  @param Path a tree path that points to the row

   Signal_Toggle_Cursor_Row : constant Glib.Signal_Name := "toggle-cursor-row";
   procedure On_Toggle_Cursor_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Boolean;
       After : Boolean := False);
   procedure On_Toggle_Cursor_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   Signal_Unselect_All : constant Glib.Signal_Name := "unselect-all";
   procedure On_Unselect_All
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Boolean;
       After : Boolean := False);
   procedure On_Unselect_All
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Tree_View_Record, Gtk_Tree_View);
   function "+"
     (Widget : access Gtk_Tree_View_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Tree_View
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Tree_View_Record, Gtk_Tree_View);
   function "+"
     (Widget : access Gtk_Tree_View_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Tree_View
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Tree_View_Record, Gtk_Tree_View);
   function "+"
     (Widget : access Gtk_Tree_View_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Tree_View
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Tooltip_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("tooltip-column");
   Show_Expanders_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-expanders");
   Search_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("search-column");
   Rubber_Banding_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("rubber-banding");
   Reorderable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("reorderable");
   Model_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("model");
   Level_Indentation_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("level-indentation");
   Hover_Selection_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("hover-selection");
   Hover_Expand_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("hover-expand");
   Headers_Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("headers-visible");
   Headers_Clickable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("headers-clickable");
   Fixed_Height_Mode_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("fixed-height-mode");
   Expander_Column_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("expander-column");
   Enable_Tree_Lines_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-tree-lines");
   Enable_Search_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-search");
   Enable_Grid_Lines_Property : constant Gtk.Enums.Property_Gtk_Tree_View_Grid_Lines :=
     Gtk.Enums.Build ("enable-grid-lines");
   Activate_On_Single_Click_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activate-on-single-click");
end Gtk.Tree_View;
