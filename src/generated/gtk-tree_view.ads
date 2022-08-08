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
--  Widget that displays any object that implements the
--  Gtk.Tree_Model.Gtk_Tree_Model interface.
--
--  Please refer to the [tree widget conceptual overview][TreeWidget] for an
--  overview of all the objects and data types related to the tree widget and
--  how they work together.
--
--  Several different coordinate systems are exposed in the GtkTreeView API.
--  These are:
--
--  ![](tree-view-coordinates.png)
--
--  Coordinate systems in GtkTreeView API:
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
--  former you can use Gtk.Tree_View.Convert_Widget_To_Bin_Window_Coords (and
--  vice versa), for the latter Gtk.Tree_View.Convert_Bin_Window_To_Tree_Coords
--  (and vice versa).
--
--  # GtkTreeView as GtkBuildable
--
--  The GtkTreeView implementation of the GtkBuildable interface accepts
--  Gtk.Tree_View_Column.Gtk_Tree_View_Column objects as <child> elements and
--  exposes the internal Gtk.Tree_Selection.Gtk_Tree_Selection in UI
--  definitions.
--
--  An example of a UI definition fragment with GtkTreeView: |[ <object
--  class="GtkTreeView" id="treeview"> <property
--  name="model">liststore1</property> <child> <object
--  class="GtkTreeViewColumn" id="test-column"> <property
--  name="title">Test</property> <child> <object class="GtkCellRendererText"
--  id="test-renderer"/> <attributes> <attribute name="text">1</attribute>
--  </attributes> </child> </object> </child> <child
--  internal-child="selection"> <object class="GtkTreeSelection"
--  id="selection"> <signal name="changed"
--  handler="on_treeview_selection_changed"/> </object> </child> </object> ]|
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> treeview.view ├── header │ ├── <column header>
--  ┊ ┊ │ ╰── <column header> │ ╰── [rubberband] ]|
--
--  GtkTreeView has a main CSS node with name treeview and style class .view.
--  It has a subnode with name header, which is the parent for all the column
--  header widgets' CSS nodes. For rubberband selection, a subnode with name
--  rubberband is used.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;                   use Cairo;
with Gdk;                     use Gdk;
with Gdk.Drag_Contexts;       use Gdk.Drag_Contexts;
with Gdk.Rectangle;           use Gdk.Rectangle;
with Gdk.Types;               use Gdk.Types;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Adjustment;          use Gtk.Adjustment;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Cell_Renderer;       use Gtk.Cell_Renderer;
with Gtk.Container;           use Gtk.Container;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.GEntry;              use Gtk.GEntry;
with Gtk.Scrollable;          use Gtk.Scrollable;
with Gtk.Style;               use Gtk.Style;
with Gtk.Target_List;         use Gtk.Target_List;
with Gtk.Tooltip;             use Gtk.Tooltip;
with Gtk.Tree_Model;          use Gtk.Tree_Model;
with Gtk.Tree_Selection;      use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;    use Gtk.Tree_View_Column;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Tree_View is

   type Gtk_Tree_View_Record is new Gtk_Container_Record with null record;
   type Gtk_Tree_View is access all Gtk_Tree_View_Record'Class;

   type Gtk_Tree_View_Drop_Position is (
      Tree_View_Drop_Before,
      Tree_View_Drop_After,
      Tree_View_Drop_Into_Or_Before,
      Tree_View_Drop_Into_Or_After);
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
   --  straight mapping between the cell and the model. This is useful for
   --  customizing the cell renderer. For example, a function might get an
   --  integer from the Tree_Model, and render it to the "text" attribute of
   --  "cell" by converting it to its written equivalent. This is set by
   --  calling gtk_tree_view_column_set_cell_data_func
   --  "tree_column": A Gtk.Tree_View_Column.Gtk_Tree_View_Column
   --  "cell": The Gtk.Cell_Renderer.Gtk_Cell_Renderer that is being rendered
   --  by Tree_Column
   --  "tree_model": The Gtk.Tree_Model.Gtk_Tree_Model being rendered
   --  "iter": A Gtk.Tree_Model.Gtk_Tree_Iter of the current row rendered

   type Gtk_Tree_View_Mapping_Func is access procedure
     (Tree_View : not null access Gtk_Tree_View_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Function used for Gtk.Tree_View.Map_Expanded_Rows.
   --  "tree_view": A Gtk.Tree_View.Gtk_Tree_View
   --  "path": The path that's expanded

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
   --  "tree_view": A Gtk.Tree_View.Gtk_Tree_View
   --  "column": The Gtk.Tree_View_Column.Gtk_Tree_View_Column being dragged
   --  "prev_column": A Gtk.Tree_View_Column.Gtk_Tree_View_Column on one side
   --  of Column
   --  "next_column": A Gtk.Tree_View_Column.Gtk_Tree_View_Column on the other
   --  side of Column

   type Gtk_Tree_Destroy_Count_Func is access procedure
     (Tree_View : not null access Gtk_Tree_View_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Children  : Glib.Gint);

   type Gtk_Tree_View_Row_Separator_Func is access function
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Function type for determining whether the row pointed to by Iter should
   --  be rendered as a separator. A common way to implement this is to have a
   --  boolean column in the model, whose values the
   --  Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func returns.
   --  "model": the Gtk.Tree_Model.Gtk_Tree_Model
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter pointing at a row in Model

   type Gtk_Tree_View_Search_Equal_Func is access function
     (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Column : Glib.Gint;
      Key    : UTF8_String;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  A function used for checking whether a row in Model matches a search
   --  key string entered by the user. Note the return value is reversed from
   --  what you would normally expect, though it has some similarity to strcmp
   --  returning 0 for equal strings.
   --  "model": the Gtk.Tree_Model.Gtk_Tree_Model being searched
   --  "column": the search column set by Gtk.Tree_View.Set_Search_Column
   --  "key": the key string to compare with
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter pointing the row of Model that
   --  should be compared with Key.

   type Gtk_Tree_View_Search_Position_Func is access procedure
     (Tree_View     : not null access Gtk_Tree_View_Record'Class;
      Search_Dialog : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Tree_View_Drop_Position_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Tree_View_Drop_Position);
   type Property_Gtk_Tree_View_Drop_Position is new Gtk_Tree_View_Drop_Position_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Tree_View : out Gtk_Tree_View);
   procedure Initialize
      (Tree_View : not null access Gtk_Tree_View_Record'Class);
   --  Creates a new Gtk.Tree_View.Gtk_Tree_View widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Tree_View_New return Gtk_Tree_View;
   --  Creates a new Gtk.Tree_View.Gtk_Tree_View widget.

   procedure Gtk_New
      (Tree_View : out Gtk_Tree_View;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model);
   procedure Initialize
      (Tree_View : not null access Gtk_Tree_View_Record'Class;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model);
   --  Creates a new Gtk.Tree_View.Gtk_Tree_View widget with the model
   --  initialized to Model.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "model": the model.

   function Gtk_Tree_View_New_With_Model
      (Model : Gtk.Tree_Model.Gtk_Tree_Model) return Gtk_Tree_View;
   --  Creates a new Gtk.Tree_View.Gtk_Tree_View widget with the model
   --  initialized to Model.
   --  "model": the model.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_view_get_type");

   -------------
   -- Methods --
   -------------

   function Append_Column
      (Tree_View : not null access Gtk_Tree_View_Record;
       Column    : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class)
       return Glib.Gint;
   --  Appends Column to the list of columns. If Tree_View has "fixed_height"
   --  mode enabled, then Column must have its "sizing" property set to be
   --  GTK_TREE_VIEW_COLUMN_FIXED.
   --  "column": The Gtk.Tree_View_Column.Gtk_Tree_View_Column to add.

   procedure Collapse_All (Tree_View : not null access Gtk_Tree_View_Record);
   --  Recursively collapses all visible, expanded nodes in Tree_View.

   function Collapse_Row
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   --  Collapses a row (hides its child rows, if they exist).
   --  "path": path to a row in the Tree_View

   procedure Columns_Autosize
      (Tree_View : not null access Gtk_Tree_View_Record);
   --  Resizes all columns to their optimal width. Only works after the
   --  treeview has been realized.

   procedure Convert_Bin_Window_To_Tree_Coords
      (Tree_View : not null access Gtk_Tree_View_Record;
       Bx        : Glib.Gint;
       By        : Glib.Gint;
       Tx        : out Glib.Gint;
       Ty        : out Glib.Gint);
   --  Converts bin_window coordinates to coordinates for the tree (the full
   --  scrollable area of the tree).
   --  Since: gtk+ 2.12
   --  "bx": X coordinate relative to bin_window
   --  "by": Y coordinate relative to bin_window
   --  "tx": return location for tree X coordinate
   --  "ty": return location for tree Y coordinate

   procedure Convert_Bin_Window_To_Widget_Coords
      (Tree_View : not null access Gtk_Tree_View_Record;
       Bx        : Glib.Gint;
       By        : Glib.Gint;
       Wx        : out Glib.Gint;
       Wy        : out Glib.Gint);
   --  Converts bin_window coordinates (see Gtk.Tree_View.Get_Bin_Window) to
   --  widget relative coordinates.
   --  Since: gtk+ 2.12
   --  "bx": bin_window X coordinate
   --  "by": bin_window Y coordinate
   --  "wx": return location for widget X coordinate
   --  "wy": return location for widget Y coordinate

   procedure Convert_Tree_To_Bin_Window_Coords
      (Tree_View : not null access Gtk_Tree_View_Record;
       Tx        : Glib.Gint;
       Ty        : Glib.Gint;
       Bx        : out Glib.Gint;
       By        : out Glib.Gint);
   --  Converts tree coordinates (coordinates in full scrollable area of the
   --  tree) to bin_window coordinates.
   --  Since: gtk+ 2.12
   --  "tx": tree X coordinate
   --  "ty": tree Y coordinate
   --  "bx": return location for X coordinate relative to bin_window
   --  "by": return location for Y coordinate relative to bin_window

   procedure Convert_Tree_To_Widget_Coords
      (Tree_View : not null access Gtk_Tree_View_Record;
       Tx        : Glib.Gint;
       Ty        : Glib.Gint;
       Wx        : out Glib.Gint;
       Wy        : out Glib.Gint);
   --  Converts tree coordinates (coordinates in full scrollable area of the
   --  tree) to widget coordinates.
   --  Since: gtk+ 2.12
   --  "tx": X coordinate relative to the tree
   --  "ty": Y coordinate relative to the tree
   --  "wx": return location for widget X coordinate
   --  "wy": return location for widget Y coordinate

   procedure Convert_Widget_To_Bin_Window_Coords
      (Tree_View : not null access Gtk_Tree_View_Record;
       Wx        : Glib.Gint;
       Wy        : Glib.Gint;
       Bx        : out Glib.Gint;
       By        : out Glib.Gint);
   --  Converts widget coordinates to coordinates for the bin_window (see
   --  Gtk.Tree_View.Get_Bin_Window).
   --  Since: gtk+ 2.12
   --  "wx": X coordinate relative to the widget
   --  "wy": Y coordinate relative to the widget
   --  "bx": return location for bin_window X coordinate
   --  "by": return location for bin_window Y coordinate

   procedure Convert_Widget_To_Tree_Coords
      (Tree_View : not null access Gtk_Tree_View_Record;
       Wx        : Glib.Gint;
       Wy        : Glib.Gint;
       Tx        : out Glib.Gint;
       Ty        : out Glib.Gint);
   --  Converts widget coordinates to coordinates for the tree (the full
   --  scrollable area of the tree).
   --  Since: gtk+ 2.12
   --  "wx": X coordinate relative to the widget
   --  "wy": Y coordinate relative to the widget
   --  "tx": return location for tree X coordinate
   --  "ty": return location for tree Y coordinate

   function Create_Row_Drag_Icon
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Cairo.Cairo_Surface;
   --  Creates a cairo_surface_t representation of the row at Path. This image
   --  is used for a drag icon.
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path in Tree_View

   procedure Enable_Model_Drag_Dest
      (Tree_View : not null access Gtk_Tree_View_Record;
       Targets   : Gtk.Target_List.Target_Entry_Array;
       Actions   : Gdk.Drag_Contexts.Gdk_Drag_Action);
   --  Turns Tree_View into a drop destination for automatic DND. Calling this
   --  method sets Gtk.Tree_View.Gtk_Tree_View:reorderable to False.
   --  "targets": the table of targets that the drag will support
   --  "actions": the bitmask of possible actions for a drag from this widget

   procedure Enable_Model_Drag_Source
      (Tree_View         : not null access Gtk_Tree_View_Record;
       Start_Button_Mask : Gdk.Types.Gdk_Modifier_Type;
       Targets           : Gtk.Target_List.Target_Entry_Array;
       Actions           : Gdk.Drag_Contexts.Gdk_Drag_Action);
   --  Turns Tree_View into a drag source for automatic DND. Calling this
   --  method sets Gtk.Tree_View.Gtk_Tree_View:reorderable to False.
   --  "start_button_mask": Mask of allowed buttons to start drag
   --  "targets": the table of targets that the drag will support
   --  "actions": the bitmask of possible actions for a drag from this widget

   procedure Expand_All (Tree_View : not null access Gtk_Tree_View_Record);
   --  Recursively expands all nodes in the Tree_View.

   function Expand_Row
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Open_All  : Boolean) return Boolean;
   --  Opens the row so its children are visible.
   --  "path": path to a row
   --  "open_all": whether to recursively expand, or just expand immediate
   --  children

   procedure Expand_To_Path
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Expands the row at Path. This will also expand all parent rows of Path
   --  as necessary.
   --  Since: gtk+ 2.2
   --  "path": path to a row.

   function Get_Activate_On_Single_Click
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean;
   --  Gets the setting set by Gtk.Tree_View.Set_Activate_On_Single_Click.
   --  Since: gtk+ 3.8

   procedure Set_Activate_On_Single_Click
      (Tree_View : not null access Gtk_Tree_View_Record;
       Single    : Boolean);
   --  Cause the Gtk.Tree_View.Gtk_Tree_View::row-activated signal to be
   --  emitted on a single click instead of a double click.
   --  Since: gtk+ 3.8
   --  "single": True to emit row-activated on a single click

   procedure Get_Background_Area
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Rect      : out Gdk.Rectangle.Gdk_Rectangle);
   --  Fills the bounding rectangle in bin_window coordinates for the cell at
   --  the row specified by Path and the column specified by Column. If Path is
   --  null, or points to a node not found in the tree, the Y and Height fields
   --  of the rectangle will be filled with 0. If Column is null, the X and
   --  Width fields will be filled with 0. The returned rectangle is equivalent
   --  to the Background_Area passed to Gtk.Cell_Renderer.Render. These
   --  background areas tile to cover the entire bin window. Contrast with the
   --  Cell_Area, returned by Gtk.Tree_View.Get_Cell_Area, which returns only
   --  the cell itself, excluding surrounding borders and the tree expander
   --  area.
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path for the row, or null to get only
   --  horizontal coordinates
   --  "column": a Gtk.Tree_View_Column.Gtk_Tree_View_Column for the column,
   --  or null to get only vertical coordiantes
   --  "rect": rectangle to fill with cell background rect

   function Get_Bin_Window
      (Tree_View : not null access Gtk_Tree_View_Record)
       return Gdk.Gdk_Window;
   --  Returns the window that Tree_View renders to. This is used primarily to
   --  compare to `event->window` to confirm that the event on Tree_View is on
   --  the right window.

   procedure Get_Cell_Area
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Rect      : out Gdk.Rectangle.Gdk_Rectangle);
   --  Fills the bounding rectangle in bin_window coordinates for the cell at
   --  the row specified by Path and the column specified by Column. If Path is
   --  null, or points to a path not currently displayed, the Y and Height
   --  fields of the rectangle will be filled with 0. If Column is null, the X
   --  and Width fields will be filled with 0. The sum of all cell rects does
   --  not cover the entire tree; there are extra pixels in between rows, for
   --  example. The returned rectangle is equivalent to the Cell_Area passed to
   --  Gtk.Cell_Renderer.Render. This function is only valid if Tree_View is
   --  realized.
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path for the row, or null to get only
   --  horizontal coordinates
   --  "column": a Gtk.Tree_View_Column.Gtk_Tree_View_Column for the column,
   --  or null to get only vertical coordinates
   --  "rect": rectangle to fill with cell rect

   function Get_Column
      (Tree_View : not null access Gtk_Tree_View_Record;
       N         : Glib.Gint)
       return Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   --  Gets the Gtk.Tree_View_Column.Gtk_Tree_View_Column at the given
   --  position in the tree_view.
   --  "n": The position of the column, counting from 0.

   function Get_Columns
      (Tree_View : not null access Gtk_Tree_View_Record)
       return Gtk.Tree_View_Column.Column_List.Glist;
   --  Returns a GList of all the Gtk.Tree_View_Column.Gtk_Tree_View_Column s
   --  currently in Tree_View. The returned list must be freed with g_list_free
   --  ().

   procedure Get_Cursor
      (Tree_View    : not null access Gtk_Tree_View_Record;
       Path         : out Gtk.Tree_Model.Gtk_Tree_Path;
       Focus_Column : out Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   --  Fills in Path and Focus_Column with the current path and focus column.
   --  If the cursor isn't currently set, then *Path will be null. If no column
   --  currently has focus, then *Focus_Column will be null.
   --  The returned Gtk.Tree_Model.Gtk_Tree_Path must be freed with
   --  Gtk.Tree_Model.Path_Free when you are done with it.
   --  "path": A pointer to be filled with the current cursor path, or null
   --  "focus_column": A pointer to be filled with the current focus column,
   --  or null

   procedure Set_Cursor
      (Tree_View     : not null access Gtk_Tree_View_Record;
       Path          : Gtk.Tree_Model.Gtk_Tree_Path;
       Focus_Column  : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Start_Editing : Boolean);
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
   --  "path": A Gtk.Tree_Model.Gtk_Tree_Path
   --  "focus_column": A Gtk.Tree_View_Column.Gtk_Tree_View_Column, or null
   --  "start_editing": True if the specified cell should start being edited.

   function Get_Dest_Row_At_Pos
      (Tree_View : not null access Gtk_Tree_View_Record;
       Drag_X    : Glib.Gint;
       Drag_Y    : Glib.Gint;
       Path      : access Gtk.Tree_Model.Gtk_Tree_Path;
       Pos       : access Gtk_Tree_View_Drop_Position) return Boolean;
   --  Determines the destination row for a given position. Drag_X and Drag_Y
   --  are expected to be in widget coordinates. This function is only
   --  meaningful if Tree_View is realized. Therefore this function will always
   --  return False if Tree_View is not realized or does not have a model.
   --  "drag_x": the position to determine the destination row for
   --  "drag_y": the position to determine the destination row for
   --  "path": Return location for the path of the highlighted row, or null.
   --  "pos": Return location for the drop position, or null

   procedure Get_Drag_Dest_Row
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
       Pos       : out Gtk_Tree_View_Drop_Position);
   --  Gets information about the row that is highlighted for feedback.
   --  "path": Return location for the path of the highlighted row, or null.
   --  "pos": Return location for the drop position, or null

   procedure Set_Drag_Dest_Row
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Pos       : Gtk_Tree_View_Drop_Position);
   --  Sets the row that is highlighted for feedback. If Path is null, an
   --  existing highlight is removed.
   --  "path": The path of the row to highlight, or null
   --  "pos": Specifies whether to drop before, after or into the row

   function Get_Enable_Search
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean;
   --  Returns whether or not the tree allows to start interactive searching
   --  by typing in text.

   procedure Set_Enable_Search
      (Tree_View     : not null access Gtk_Tree_View_Record;
       Enable_Search : Boolean);
   --  If Enable_Search is set, then the user can type in text to search
   --  through the tree interactively (this is sometimes called "typeahead
   --  find").
   --  Note that even if this is False, the user can still initiate a search
   --  using the "start-interactive-search" key binding.
   --  "enable_search": True, if the user can search interactively

   function Get_Enable_Tree_Lines
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean;
   --  Returns whether or not tree lines are drawn in Tree_View.
   --  Since: gtk+ 2.10

   procedure Set_Enable_Tree_Lines
      (Tree_View : not null access Gtk_Tree_View_Record;
       Enabled   : Boolean);
   --  Sets whether to draw lines interconnecting the expanders in Tree_View.
   --  This does not have any visible effects for lists.
   --  Since: gtk+ 2.10
   --  "enabled": True to enable tree line drawing, False otherwise.

   function Get_Expander_Column
      (Tree_View : not null access Gtk_Tree_View_Record)
       return Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   --  Returns the column that is the current expander column. This column has
   --  the expander arrow drawn next to it.

   procedure Set_Expander_Column
      (Tree_View : not null access Gtk_Tree_View_Record;
       Column    : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class);
   --  Sets the column to draw the expander arrow at. It must be in Tree_View.
   --  If Column is null, then the expander arrow is always at the first
   --  visible column.
   --  If you do not want expander arrow to appear in your tree, set the
   --  expander column to a hidden column.
   --  "column": null, or the column to draw the expander arrow at.

   function Get_Fixed_Height_Mode
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean;
   --  Returns whether fixed height mode is turned on for Tree_View.
   --  Since: gtk+ 2.6

   procedure Set_Fixed_Height_Mode
      (Tree_View : not null access Gtk_Tree_View_Record;
       Enable    : Boolean);
   --  Enables or disables the fixed height mode of Tree_View. Fixed height
   --  mode speeds up Gtk.Tree_View.Gtk_Tree_View by assuming that all rows
   --  have the same height. Only enable this option if all rows are the same
   --  height and all columns are of type
   --  Gtk.Tree_View_Column.Tree_View_Column_Fixed.
   --  Since: gtk+ 2.6
   --  "enable": True to enable fixed height mode

   function Get_Grid_Lines
      (Tree_View : not null access Gtk_Tree_View_Record)
       return Gtk.Enums.Gtk_Tree_View_Grid_Lines;
   --  Returns which grid lines are enabled in Tree_View.
   --  Since: gtk+ 2.10

   procedure Set_Grid_Lines
      (Tree_View  : not null access Gtk_Tree_View_Record;
       Grid_Lines : Gtk.Enums.Gtk_Tree_View_Grid_Lines);
   --  Sets which grid lines to draw in Tree_View.
   --  Since: gtk+ 2.10
   --  "grid_lines": a Gtk.Enums.Gtk_Tree_View_Grid_Lines value indicating
   --  which grid lines to enable.

   function Get_Headers_Clickable
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean;
   --  Returns whether all header columns are clickable.
   --  Since: gtk+ 2.10

   procedure Set_Headers_Clickable
      (Tree_View : not null access Gtk_Tree_View_Record;
       Setting   : Boolean);
   --  Allow the column title buttons to be clicked.
   --  "setting": True if the columns are clickable.

   function Get_Headers_Visible
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean;
   --  Returns True if the headers on the Tree_View are visible.

   procedure Set_Headers_Visible
      (Tree_View       : not null access Gtk_Tree_View_Record;
       Headers_Visible : Boolean);
   --  Sets the visibility state of the headers.
   --  "headers_visible": True if the headers are visible

   function Get_Hover_Expand
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean;
   --  Returns whether hover expansion mode is turned on for Tree_View.
   --  Since: gtk+ 2.6

   procedure Set_Hover_Expand
      (Tree_View : not null access Gtk_Tree_View_Record;
       Expand    : Boolean);
   --  Enables or disables the hover expansion mode of Tree_View. Hover
   --  expansion makes rows expand or collapse if the pointer moves over them.
   --  Since: gtk+ 2.6
   --  "expand": True to enable hover selection mode

   function Get_Hover_Selection
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean;
   --  Returns whether hover selection mode is turned on for Tree_View.
   --  Since: gtk+ 2.6

   procedure Set_Hover_Selection
      (Tree_View : not null access Gtk_Tree_View_Record;
       Hover     : Boolean);
   --  Enables or disables the hover selection mode of Tree_View. Hover
   --  selection makes the selected row follow the pointer. Currently, this
   --  works only for the selection modes Gtk.Enums.Selection_Single and
   --  Gtk.Enums.Selection_Browse.
   --  Since: gtk+ 2.6
   --  "hover": True to enable hover selection mode

   function Get_Level_Indentation
      (Tree_View : not null access Gtk_Tree_View_Record) return Glib.Gint;
   --  Returns the amount, in pixels, of extra indentation for child levels in
   --  Tree_View.
   --  Since: gtk+ 2.12

   procedure Set_Level_Indentation
      (Tree_View   : not null access Gtk_Tree_View_Record;
       Indentation : Glib.Gint);
   --  Sets the amount of extra indentation for child levels to use in
   --  Tree_View in addition to the default indentation. The value should be
   --  specified in pixels, a value of 0 disables this feature and in this case
   --  only the default indentation will be used. This does not have any
   --  visible effects for lists.
   --  Since: gtk+ 2.12
   --  "indentation": the amount, in pixels, of extra indentation in
   --  Tree_View.

   function Get_Model
      (Tree_View : not null access Gtk_Tree_View_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns the model the Gtk.Tree_View.Gtk_Tree_View is based on. Returns
   --  null if the model is unset.

   procedure Set_Model
      (Tree_View : not null access Gtk_Tree_View_Record;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model);
   --  Sets the model for a Gtk.Tree_View.Gtk_Tree_View. If the Tree_View
   --  already has a model set, it will remove it before setting the new model.
   --  If Model is null, then it will unset the old model.
   --  "model": The model.

   function Get_N_Columns
      (Tree_View : not null access Gtk_Tree_View_Record) return Guint;
   --  Queries the number of columns in the given Tree_View.
   --  Since: gtk+ 3.4

   procedure Get_Path_At_Pos
      (Tree_View : not null access Gtk_Tree_View_Record;
       X         : Glib.Gint;
       Y         : Glib.Gint;
       Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : out Gtk.Tree_View_Column.Gtk_Tree_View_Column;
       Cell_X    : out Glib.Gint;
       Cell_Y    : out Glib.Gint;
       Row_Found : out Boolean);
   --  Finds the path at the point (X, Y), relative to bin_window coordinates
   --  (please see Gtk.Tree_View.Get_Bin_Window). That is, X and Y are relative
   --  to an events coordinates. X and Y must come from an event on the
   --  Tree_View only where `event->window == gtk_tree_view_get_bin_window ()`.
   --  It is primarily for things like popup menus. If Path is non-null, then
   --  it will be filled with the Gtk.Tree_Model.Gtk_Tree_Path at that point.
   --  This path should be freed with Gtk.Tree_Model.Path_Free. If Column is
   --  non-null, then it will be filled with the column at that point. Cell_X
   --  and Cell_Y return the coordinates relative to the cell background (i.e.
   --  the Background_Area passed to Gtk.Cell_Renderer.Render). This function
   --  is only meaningful if Tree_View is realized. Therefore this function
   --  will always return False if Tree_View is not realized or does not have a
   --  model.
   --  For converting widget coordinates (eg. the ones you get from
   --  GtkWidget::query-tooltip), please see
   --  Gtk.Tree_View.Convert_Widget_To_Bin_Window_Coords.
   --  "x": The x position to be identified (relative to bin_window).
   --  "y": The y position to be identified (relative to bin_window).
   --  "path": A pointer to a Gtk.Tree_Model.Gtk_Tree_Path pointer to be
   --  filled in, or null
   --  "column": A pointer to a Gtk.Tree_View_Column.Gtk_Tree_View_Column
   --  pointer to be filled in, or null
   --  "cell_x": A pointer where the X coordinate relative to the cell can be
   --  placed, or null
   --  "cell_y": A pointer where the Y coordinate relative to the cell can be
   --  placed, or null

   function Get_Reorderable
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean;
   --  Retrieves whether the user can reorder the tree via drag-and-drop. See
   --  Gtk.Tree_View.Set_Reorderable.

   procedure Set_Reorderable
      (Tree_View   : not null access Gtk_Tree_View_Record;
       Reorderable : Boolean);
   --  This function is a convenience function to allow you to reorder models
   --  that support the Gtk_Tree_Drag_Source_Iface and the
   --  Gtk_Tree_Drag_Dest_Iface. Both Gtk.Tree_Store.Gtk_Tree_Store and
   --  Gtk.List_Store.Gtk_List_Store support these. If Reorderable is True,
   --  then the user can reorder the model by dragging and dropping rows. The
   --  developer can listen to these changes by connecting to the model's
   --  Gtk.Tree_Model.Gtk_Tree_Model::row-inserted and
   --  Gtk.Tree_Model.Gtk_Tree_Model::row-deleted signals. The reordering is
   --  implemented by setting up the tree view as a drag source and
   --  destination. Therefore, drag and drop can not be used in a reorderable
   --  view for any other purpose.
   --  This function does not give you any degree of control over the order --
   --  any reordering is allowed. If more control is needed, you should
   --  probably handle drag and drop manually.
   --  "reorderable": True, if the tree can be reordered.

   procedure Get_Row_Separator_Func
      (Tree_View : not null access Gtk_Tree_View_Record);
   --  Returns the current row separator function.
   --  Since: gtk+ 2.6

   procedure Set_Row_Separator_Func
      (Tree_View : not null access Gtk_Tree_View_Record;
       Func      : Gtk_Tree_View_Row_Separator_Func);
   --  Sets the row separator function, which is used to determine whether a
   --  row should be drawn as a separator. If the row separator function is
   --  null, no separators are drawn. This is the default value.
   --  Since: gtk+ 2.6
   --  "func": a Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func

   function Get_Rubber_Banding
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean;
   --  Returns whether rubber banding is turned on for Tree_View. If the
   --  selection mode is GTK_SELECTION_MULTIPLE, rubber banding will allow the
   --  user to select multiple rows by dragging the mouse.
   --  Since: gtk+ 2.10

   procedure Set_Rubber_Banding
      (Tree_View : not null access Gtk_Tree_View_Record;
       Enable    : Boolean);
   --  Enables or disables rubber banding in Tree_View. If the selection mode
   --  is GTK_SELECTION_MULTIPLE, rubber banding will allow the user to select
   --  multiple rows by dragging the mouse.
   --  Since: gtk+ 2.10
   --  "enable": True to enable rubber banding

   function Get_Rules_Hint
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean;
   pragma Obsolescent (Get_Rules_Hint);
   --  Gets the setting set by Gtk.Tree_View.Set_Rules_Hint.
   --  Deprecated since 3.14, 1

   procedure Set_Rules_Hint
      (Tree_View : not null access Gtk_Tree_View_Record;
       Setting   : Boolean);
   pragma Obsolescent (Set_Rules_Hint);
   --  Sets a hint for the theme to draw even/odd rows in the Tree_View with
   --  different colors, also known as "zebra striping".
   --  This function tells the GTK+ theme that the user interface for your
   --  application requires users to read across tree rows and associate cells
   --  with one another.
   --  Do not use it just because you prefer the appearance of the ruled tree;
   --  that's a question for the theme. Some themes will draw tree rows in
   --  alternating colors even when rules are turned off, and users who prefer
   --  that appearance all the time can choose those themes. You should call
   --  this function only as a semantic hint to the theme engine that your tree
   --  makes alternating colors useful from a functional standpoint (since it
   --  has lots of columns, generally).
   --  Deprecated since 3.14, 1
   --  "setting": True if the tree requires reading across rows

   function Get_Search_Column
      (Tree_View : not null access Gtk_Tree_View_Record) return Glib.Gint;
   --  Gets the column searched on by the interactive search code.

   procedure Set_Search_Column
      (Tree_View : not null access Gtk_Tree_View_Record;
       Column    : Glib.Gint);
   --  Sets Column as the column where the interactive search code should
   --  search in for the current model.
   --  If the search column is set, users can use the
   --  "start-interactive-search" key binding to bring up search popup. The
   --  enable-search property controls whether simply typing text will also
   --  start an interactive search.
   --  Note that Column refers to a column of the current model. The search
   --  column is reset to -1 when the model is changed.
   --  "column": the column of the model to search in, or -1 to disable
   --  searching

   function Get_Search_Entry
      (Tree_View : not null access Gtk_Tree_View_Record)
       return Gtk.GEntry.Gtk_Entry;
   --  Returns the Gtk.GEntry.Gtk_Entry which is currently in use as
   --  interactive search entry for Tree_View. In case the built-in entry is
   --  being used, null will be returned.
   --  Since: gtk+ 2.10

   procedure Set_Search_Entry
      (Tree_View : not null access Gtk_Tree_View_Record;
       GEntry    : access Gtk.GEntry.Gtk_Entry_Record'Class);
   --  Sets the entry which the interactive search code will use for this
   --  Tree_View. This is useful when you want to provide a search entry in our
   --  interface at all time at a fixed position. Passing null for Entry will
   --  make the interactive search code use the built-in popup entry again.
   --  Since: gtk+ 2.10
   --  "entry": the entry the interactive search code of Tree_View should use
   --  or null

   procedure Get_Search_Equal_Func
      (Tree_View : not null access Gtk_Tree_View_Record);
   --  Returns the compare function currently in use.

   procedure Set_Search_Equal_Func
      (Tree_View         : not null access Gtk_Tree_View_Record;
       Search_Equal_Func : Gtk_Tree_View_Search_Equal_Func;
       Search_Destroy    : Glib.G_Destroy_Notify_Address);
   --  Sets the compare function for the interactive search capabilities; note
   --  that somewhat like strcmp returning 0 for equality
   --  Gtk_Tree_View_Search_Equal_Func returns False on matches.
   --  "search_equal_func": the compare function to use during the search
   --  "search_destroy": Destroy notifier for Search_User_Data, or null

   procedure Get_Search_Position_Func
      (Tree_View : not null access Gtk_Tree_View_Record);
   --  Returns the positioning function currently in use.
   --  Since: gtk+ 2.10

   procedure Set_Search_Position_Func
      (Tree_View : not null access Gtk_Tree_View_Record;
       Func      : Gtk_Tree_View_Search_Position_Func);
   --  Sets the function to use when positioning the search dialog.
   --  Since: gtk+ 2.10
   --  "func": the function to use to position the search dialog, or null to
   --  use the default search position function

   function Get_Selection
      (Tree_View : not null access Gtk_Tree_View_Record)
       return Gtk.Tree_Selection.Gtk_Tree_Selection;
   --  Gets the Gtk.Tree_Selection.Gtk_Tree_Selection associated with
   --  Tree_View.

   function Get_Show_Expanders
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean;
   --  Returns whether or not expanders are drawn in Tree_View.
   --  Since: gtk+ 2.12

   procedure Set_Show_Expanders
      (Tree_View : not null access Gtk_Tree_View_Record;
       Enabled   : Boolean);
   --  Sets whether to draw and enable expanders and indent child rows in
   --  Tree_View. When disabled there will be no expanders visible in trees and
   --  there will be no way to expand and collapse rows by default. Also note
   --  that hiding the expanders will disable the default indentation. You can
   --  set a custom indentation in this case using
   --  Gtk.Tree_View.Set_Level_Indentation. This does not have any visible
   --  effects for lists.
   --  Since: gtk+ 2.12
   --  "enabled": True to enable expander drawing, False otherwise.

   function Get_Tooltip_Column
      (Tree_View : not null access Gtk_Tree_View_Record) return Glib.Gint;
   --  Returns the column of Tree_View's model which is being used for
   --  displaying tooltips on Tree_View's rows.
   --  Since: gtk+ 2.12

   procedure Set_Tooltip_Column
      (Tree_View : not null access Gtk_Tree_View_Record;
       Column    : Glib.Gint);
   --  If you only plan to have simple (text-only) tooltips on full rows, you
   --  can use this function to have Gtk.Tree_View.Gtk_Tree_View handle these
   --  automatically for you. Column should be set to the column in Tree_View's
   --  model containing the tooltip texts, or -1 to disable this feature.
   --  When enabled, Gtk.Widget.Gtk_Widget:has-tooltip will be set to True and
   --  Tree_View will connect a Gtk.Widget.Gtk_Widget::query-tooltip signal
   --  handler.
   --  Note that the signal handler sets the text with Gtk.Tooltip.Set_Markup,
   --  so &, <, etc have to be escaped in the text.
   --  Since: gtk+ 2.12
   --  "column": an integer, which is a valid column number for Tree_View's
   --  model

   procedure Get_Tooltip_Context
      (Tree_View    : not null access Gtk_Tree_View_Record;
       X            : in out Glib.Gint;
       Y            : in out Glib.Gint;
       Keyboard_Tip : Boolean;
       Model        : out Gtk.Tree_Model.Gtk_Tree_Model;
       Path         : out Gtk.Tree_Model.Gtk_Tree_Path;
       Iter         : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Success      : out Boolean);
   --  This function is supposed to be used in a
   --  Gtk.Widget.Gtk_Widget::query-tooltip signal handler for
   --  Gtk.Tree_View.Gtk_Tree_View. The X, Y and Keyboard_Tip values which are
   --  received in the signal handler, should be passed to this function
   --  without modification.
   --  The return value indicates whether there is a tree view row at the
   --  given coordinates (True) or not (False) for mouse tooltips. For keyboard
   --  tooltips the row returned will be the cursor row. When True, then any of
   --  Model, Path and Iter which have been provided will be set to point to
   --  that row and the corresponding model. X and Y will always be converted
   --  to be relative to Tree_View's bin_window if Keyboard_Tooltip is False.
   --  Since: gtk+ 2.12
   --  "x": the x coordinate (relative to widget coordinates)
   --  "y": the y coordinate (relative to widget coordinates)
   --  "keyboard_tip": whether this is a keyboard tooltip or not
   --  "model": a pointer to receive a Gtk.Tree_Model.Gtk_Tree_Model or null
   --  "path": a pointer to receive a Gtk.Tree_Model.Gtk_Tree_Path or null
   --  "iter": a pointer to receive a Gtk.Tree_Model.Gtk_Tree_Iter or null

   procedure Get_Visible_Range
      (Tree_View  : not null access Gtk_Tree_View_Record;
       Start_Path : out Gtk.Tree_Model.Gtk_Tree_Path;
       End_Path   : out Gtk.Tree_Model.Gtk_Tree_Path;
       Success    : out Boolean);
   --  Sets Start_Path and End_Path to be the first and last visible path.
   --  Note that there may be invisible paths in between.
   --  The paths should be freed with Gtk.Tree_Model.Path_Free after use.
   --  Since: gtk+ 2.8
   --  "start_path": Return location for start of region, or null.
   --  "end_path": Return location for end of region, or null.

   procedure Get_Visible_Rect
      (Tree_View    : not null access Gtk_Tree_View_Record;
       Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle);
   --  Fills Visible_Rect with the currently-visible region of the buffer, in
   --  tree coordinates. Convert to bin_window coordinates with
   --  Gtk.Tree_View.Convert_Tree_To_Bin_Window_Coords. Tree coordinates start
   --  at 0,0 for row 0 of the tree, and cover the entire scrollable area of
   --  the tree.
   --  "visible_rect": rectangle to fill

   function Insert_Column
      (Tree_View : not null access Gtk_Tree_View_Record;
       Column    : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Position  : Glib.Gint := -1) return Glib.Gint;
   --  This inserts the Column into the Tree_View at Position. If Position is
   --  -1, then the column is inserted at the end. If Tree_View has
   --  "fixed_height" mode enabled, then Column must have its "sizing" property
   --  set to be GTK_TREE_VIEW_COLUMN_FIXED.
   --  "column": The Gtk.Tree_View_Column.Gtk_Tree_View_Column to be inserted.
   --  "position": The position to insert Column in.

   function Insert_Column_With_Data_Func
      (Tree_View : not null access Gtk_Tree_View_Record;
       Position  : Glib.Gint;
       Title     : UTF8_String;
       Cell      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func      : Gtk_Tree_Cell_Data_Func;
       Dnotify   : Glib.G_Destroy_Notify_Address) return Glib.Gint;
   --  Convenience function that inserts a new column into the
   --  Gtk.Tree_View.Gtk_Tree_View with the given cell renderer and a
   --  Gtk_Tree_Cell_Data_Func to set cell renderer attributes (normally using
   --  data from the model). See also gtk_tree_view_column_set_cell_data_func,
   --  gtk_tree_view_column_pack_start. If Tree_View has "fixed_height" mode
   --  enabled, then the new column will have its "sizing" property set to be
   --  GTK_TREE_VIEW_COLUMN_FIXED.
   --  "position": Position to insert, -1 for append
   --  "title": column title
   --  "cell": cell renderer for column
   --  "func": function to set attributes of cell renderer
   --  "dnotify": destroy notifier for Data

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
      --  straight mapping between the cell and the model. This is useful for
      --  customizing the cell renderer. For example, a function might get an
      --  integer from the Tree_Model, and render it to the "text" attribute of
      --  "cell" by converting it to its written equivalent. This is set by
      --  calling gtk_tree_view_column_set_cell_data_func
      --  "tree_column": A Gtk.Tree_View_Column.Gtk_Tree_View_Column
      --  "cell": The Gtk.Cell_Renderer.Gtk_Cell_Renderer that is being rendered
      --  by Tree_Column
      --  "tree_model": The Gtk.Tree_Model.Gtk_Tree_Model being rendered
      --  "iter": A Gtk.Tree_Model.Gtk_Tree_Iter of the current row rendered
      --  "data": user data

      function Insert_Column_With_Data_Func
         (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Position  : Glib.Gint;
          Title     : UTF8_String;
          Cell      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func      : Gtk_Tree_Cell_Data_Func;
          Data      : User_Data_Type;
          Dnotify   : Glib.G_Destroy_Notify_Address) return Glib.Gint;
      --  Convenience function that inserts a new column into the
      --  Gtk.Tree_View.Gtk_Tree_View with the given cell renderer and a
      --  Gtk_Tree_Cell_Data_Func to set cell renderer attributes (normally
      --  using data from the model). See also
      --  gtk_tree_view_column_set_cell_data_func,
      --  gtk_tree_view_column_pack_start. If Tree_View has "fixed_height" mode
      --  enabled, then the new column will have its "sizing" property set to
      --  be GTK_TREE_VIEW_COLUMN_FIXED.
      --  "position": Position to insert, -1 for append
      --  "title": column title
      --  "cell": cell renderer for column
      --  "func": function to set attributes of cell renderer
      --  "data": data for Func
      --  "dnotify": destroy notifier for Data

   end Insert_Column_With_Data_Func_User_Data;

   function Is_Blank_At_Pos
      (Tree_View : not null access Gtk_Tree_View_Record;
       X         : Glib.Gint;
       Y         : Glib.Gint;
       Path      : access Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : access Gtk.Tree_View_Column.Gtk_Tree_View_Column;
       Cell_X    : access Glib.Gint;
       Cell_Y    : access Glib.Gint) return Boolean;
   --  Determine whether the point (X, Y) in Tree_View is blank, that is no
   --  cell content nor an expander arrow is drawn at the location. If so, the
   --  location can be considered as the background. You might wish to take
   --  special action on clicks on the background, such as clearing a current
   --  selection, having a custom context menu or starting rubber banding.
   --  The X and Y coordinate that are provided must be relative to bin_window
   --  coordinates. That is, X and Y must come from an event on Tree_View where
   --  `event->window == gtk_tree_view_get_bin_window ()`.
   --  For converting widget coordinates (eg. the ones you get from
   --  GtkWidget::query-tooltip), please see
   --  Gtk.Tree_View.Convert_Widget_To_Bin_Window_Coords.
   --  The Path, Column, Cell_X and Cell_Y arguments will be filled in
   --  likewise as for Gtk.Tree_View.Get_Path_At_Pos. Please see
   --  Gtk.Tree_View.Get_Path_At_Pos for more information.
   --  Since: gtk+ 3.0
   --  "x": The x position to be identified (relative to bin_window)
   --  "y": The y position to be identified (relative to bin_window)
   --  "path": A pointer to a Gtk.Tree_Model.Gtk_Tree_Path pointer to be
   --  filled in, or null
   --  "column": A pointer to a Gtk.Tree_View_Column.Gtk_Tree_View_Column
   --  pointer to be filled in, or null
   --  "cell_x": A pointer where the X coordinate relative to the cell can be
   --  placed, or null
   --  "cell_y": A pointer where the Y coordinate relative to the cell can be
   --  placed, or null

   function Is_Rubber_Banding_Active
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean;
   --  Returns whether a rubber banding operation is currently being done in
   --  Tree_View.
   --  Since: gtk+ 2.12

   procedure Map_Expanded_Rows
      (Tree_View : not null access Gtk_Tree_View_Record;
       Func      : Gtk_Tree_View_Mapping_Func);
   --  Calls Func on all expanded rows.
   --  "func": A function to be called

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Map_Expanded_Rows_User_Data is

      type Gtk_Tree_View_Mapping_Func is access procedure
        (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         User_Data : User_Data_Type);
      --  Function used for Gtk.Tree_View.Map_Expanded_Rows.
      --  "tree_view": A Gtk.Tree_View.Gtk_Tree_View
      --  "path": The path that's expanded
      --  "user_data": user data

      procedure Map_Expanded_Rows
         (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Func      : Gtk_Tree_View_Mapping_Func;
          Data      : User_Data_Type);
      --  Calls Func on all expanded rows.
      --  "func": A function to be called
      --  "data": User data to be passed to the function.

   end Map_Expanded_Rows_User_Data;

   procedure Move_Column_After
      (Tree_View   : not null access Gtk_Tree_View_Record;
       Column      : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Base_Column : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class);
   --  Moves Column to be after to Base_Column. If Base_Column is null, then
   --  Column is placed in the first position.
   --  "column": The Gtk.Tree_View_Column.Gtk_Tree_View_Column to be moved.
   --  "base_column": The Gtk.Tree_View_Column.Gtk_Tree_View_Column to be
   --  moved relative to, or null.

   function Remove_Column
      (Tree_View : not null access Gtk_Tree_View_Record;
       Column    : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class)
       return Glib.Gint;
   --  Removes Column from Tree_View.
   --  "column": The Gtk.Tree_View_Column.Gtk_Tree_View_Column to remove.

   procedure Row_Activated
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class);
   --  Activates the cell determined by Path and Column.
   --  "path": The Gtk.Tree_Model.Gtk_Tree_Path to be activated.
   --  "column": The Gtk.Tree_View_Column.Gtk_Tree_View_Column to be
   --  activated.

   function Row_Expanded
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   --  Returns True if the node pointed to by Path is expanded in Tree_View.
   --  "path": A Gtk.Tree_Model.Gtk_Tree_Path to test expansion state.

   procedure Scroll_To_Cell
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Use_Align : Boolean;
       Row_Align : Gfloat;
       Col_Align : Gfloat);
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
   --  "path": The path of the row to move to, or null.
   --  "column": The Gtk.Tree_View_Column.Gtk_Tree_View_Column to move
   --  horizontally to, or null.
   --  "use_align": whether to use alignment arguments, or False.
   --  "row_align": The vertical alignment of the row specified by Path.
   --  "col_align": The horizontal alignment of the column specified by
   --  Column.

   procedure Scroll_To_Point
      (Tree_View : not null access Gtk_Tree_View_Record;
       Tree_X    : Glib.Gint;
       Tree_Y    : Glib.Gint);
   --  Scrolls the tree view such that the top-left corner of the visible area
   --  is Tree_X, Tree_Y, where Tree_X and Tree_Y are specified in tree
   --  coordinates. The Tree_View must be realized before this function is
   --  called. If it isn't, you probably want to be using
   --  Gtk.Tree_View.Scroll_To_Cell.
   --  If either Tree_X or Tree_Y are -1, then that direction isn't scrolled.
   --  "tree_x": X coordinate of new top-left pixel of visible area, or -1
   --  "tree_y": Y coordinate of new top-left pixel of visible area, or -1

   procedure Set_Column_Drag_Function
      (Tree_View : not null access Gtk_Tree_View_Record;
       Func      : Gtk_Tree_View_Column_Drop_Func);
   --  Sets a user function for determining where a column may be dropped when
   --  dragged. This function is called on every column pair in turn at the
   --  beginning of a column drag to determine where a drop can take place. The
   --  arguments passed to Func are: the Tree_View, the
   --  Gtk.Tree_View_Column.Gtk_Tree_View_Column being dragged, the two
   --  Gtk.Tree_View_Column.Gtk_Tree_View_Column s determining the drop spot,
   --  and User_Data. If either of the
   --  Gtk.Tree_View_Column.Gtk_Tree_View_Column arguments for the drop spot
   --  are null, then they indicate an edge. If Func is set to be null, then
   --  Tree_View reverts to the default behavior of allowing all columns to be
   --  dropped everywhere.
   --  "func": A function to determine which columns are reorderable, or null.

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
      --  "tree_view": A Gtk.Tree_View.Gtk_Tree_View
      --  "column": The Gtk.Tree_View_Column.Gtk_Tree_View_Column being dragged
      --  "prev_column": A Gtk.Tree_View_Column.Gtk_Tree_View_Column on one side
      --  of Column
      --  "next_column": A Gtk.Tree_View_Column.Gtk_Tree_View_Column on the other
      --  side of Column
      --  "data": user data

      procedure Set_Column_Drag_Function
         (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Func      : Gtk_Tree_View_Column_Drop_Func;
          User_Data : User_Data_Type);
      --  Sets a user function for determining where a column may be dropped
      --  when dragged. This function is called on every column pair in turn at
      --  the beginning of a column drag to determine where a drop can take
      --  place. The arguments passed to Func are: the Tree_View, the
      --  Gtk.Tree_View_Column.Gtk_Tree_View_Column being dragged, the two
      --  Gtk.Tree_View_Column.Gtk_Tree_View_Column s determining the drop
      --  spot, and User_Data. If either of the
      --  Gtk.Tree_View_Column.Gtk_Tree_View_Column arguments for the drop spot
      --  are null, then they indicate an edge. If Func is set to be null, then
      --  Tree_View reverts to the default behavior of allowing all columns to
      --  be dropped everywhere.
      --  "func": A function to determine which columns are reorderable, or
      --  null.
      --  "user_data": User data to be passed to Func, or null

   end Set_Column_Drag_Function_User_Data;

   procedure Set_Cursor_On_Cell
      (Tree_View     : not null access Gtk_Tree_View_Record;
       Path          : Gtk.Tree_Model.Gtk_Tree_Path;
       Focus_Column  : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Focus_Cell    : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Start_Editing : Boolean);
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
   --  Since: gtk+ 2.2
   --  "path": A Gtk.Tree_Model.Gtk_Tree_Path
   --  "focus_column": A Gtk.Tree_View_Column.Gtk_Tree_View_Column, or null
   --  "focus_cell": A Gtk.Cell_Renderer.Gtk_Cell_Renderer, or null
   --  "start_editing": True if the specified cell should start being edited.

   procedure Set_Destroy_Count_Func
      (Tree_View : not null access Gtk_Tree_View_Record;
       Func      : Gtk_Tree_Destroy_Count_Func);
   pragma Obsolescent (Set_Destroy_Count_Func);
   --  This function should almost never be used. It is meant for private use
   --  by ATK for determining the number of visible children that are removed
   --  when the user collapses a row, or a row is deleted.
   --  Deprecated since 3.4, 1
   --  "func": Function to be called when a view row is destroyed, or null

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Destroy_Count_Func_User_Data is

      type Gtk_Tree_Destroy_Count_Func is access procedure
        (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         Children  : Glib.Gint;
         User_Data : User_Data_Type);

      procedure Set_Destroy_Count_Func
         (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Func      : Gtk_Tree_Destroy_Count_Func;
          Data      : User_Data_Type);
      pragma Obsolescent (Set_Destroy_Count_Func);
      --  This function should almost never be used. It is meant for private
      --  use by ATK for determining the number of visible children that are
      --  removed when the user collapses a row, or a row is deleted.
      --  Deprecated since 3.4, 1
      --  "func": Function to be called when a view row is destroyed, or null
      --  "data": User data to be passed to Func, or null

   end Set_Destroy_Count_Func_User_Data;

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
      --  Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func returns.
      --  "model": the Gtk.Tree_Model.Gtk_Tree_Model
      --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter pointing at a row in Model
      --  "data": user data

      procedure Set_Row_Separator_Func
         (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Func      : Gtk_Tree_View_Row_Separator_Func;
          Data      : User_Data_Type);
      --  Sets the row separator function, which is used to determine whether
      --  a row should be drawn as a separator. If the row separator function
      --  is null, no separators are drawn. This is the default value.
      --  Since: gtk+ 2.6
      --  "func": a Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func
      --  "data": user data to pass to Func, or null

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
      --  "model": the Gtk.Tree_Model.Gtk_Tree_Model being searched
      --  "column": the search column set by Gtk.Tree_View.Set_Search_Column
      --  "key": the key string to compare with
      --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter pointing the row of Model that
      --  should be compared with Key.
      --  "search_data": user data from Gtk.Tree_View.Set_Search_Equal_Func

      procedure Set_Search_Equal_Func
         (Tree_View         : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Search_Equal_Func : Gtk_Tree_View_Search_Equal_Func;
          Search_User_Data  : User_Data_Type;
          Search_Destroy    : Glib.G_Destroy_Notify_Address);
      --  Sets the compare function for the interactive search capabilities;
      --  note that somewhat like strcmp returning 0 for equality
      --  Gtk_Tree_View_Search_Equal_Func returns False on matches.
      --  "search_equal_func": the compare function to use during the search
      --  "search_user_data": user data to pass to Search_Equal_Func, or null
      --  "search_destroy": Destroy notifier for Search_User_Data, or null

   end Set_Search_Equal_Func_User_Data;

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Search_Position_Func_User_Data is

      type Gtk_Tree_View_Search_Position_Func is access procedure
        (Tree_View     : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
         Search_Dialog : not null access Gtk.Widget.Gtk_Widget_Record'Class;
         User_Data     : User_Data_Type);

      procedure Set_Search_Position_Func
         (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Func      : Gtk_Tree_View_Search_Position_Func;
          Data      : User_Data_Type);
      --  Sets the function to use when positioning the search dialog.
      --  Since: gtk+ 2.10
      --  "func": the function to use to position the search dialog, or null
      --  to use the default search position function
      --  "data": user data to pass to Func, or null

   end Set_Search_Position_Func_User_Data;

   procedure Set_Tooltip_Cell
      (Tree_View : not null access Gtk_Tree_View_Record;
       Tooltip   : not null access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Cell      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Sets the tip area of Tooltip to the area Path, Column and Cell have in
   --  common. For example if Path is null and Column is set, the tip area will
   --  be set to the full area covered by Column. See also
   --  Gtk.Tooltip.Set_Tip_Area.
   --  Note that if Path is not specified and Cell is set and part of a column
   --  containing the expander, the tooltip might not show and hide at the
   --  correct position. In such cases Path must be set to the current node
   --  under the mouse cursor for this function to operate correctly.
   --  See also Gtk.Tree_View.Set_Tooltip_Column for a simpler alternative.
   --  Since: gtk+ 2.12
   --  "tooltip": a Gtk.Tooltip.Gtk_Tooltip
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path or null
   --  "column": a Gtk.Tree_View_Column.Gtk_Tree_View_Column or null
   --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer or null

   procedure Set_Tooltip_Row
      (Tree_View : not null access Gtk_Tree_View_Record;
       Tooltip   : not null access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Sets the tip area of Tooltip to be the area covered by the row at Path.
   --  See also Gtk.Tree_View.Set_Tooltip_Column for a simpler alternative. See
   --  also Gtk.Tooltip.Set_Tip_Area.
   --  Since: gtk+ 2.12
   --  "tooltip": a Gtk.Tooltip.Gtk_Tooltip
   --  "path": a Gtk.Tree_Model.Gtk_Tree_Path

   procedure Unset_Rows_Drag_Dest
      (Tree_View : not null access Gtk_Tree_View_Record);
   --  Undoes the effect of Gtk.Tree_View.Enable_Model_Drag_Dest. Calling this
   --  method sets Gtk.Tree_View.Gtk_Tree_View:reorderable to False.

   procedure Unset_Rows_Drag_Source
      (Tree_View : not null access Gtk_Tree_View_Record);
   --  Undoes the effect of Gtk.Tree_View.Enable_Model_Drag_Source. Calling
   --  this method sets Gtk.Tree_View.Gtk_Tree_View:reorderable to False.

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

   function Get_Border
      (Self   : not null access Gtk_Tree_View_Record;
       Border : access Gtk.Style.Gtk_Border) return Boolean;

   function Get_Hadjustment
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Hadjustment
      (Self        : not null access Gtk_Tree_View_Record;
       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   function Get_Hscroll_Policy
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy;

   procedure Set_Hscroll_Policy
      (Self   : not null access Gtk_Tree_View_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);

   function Get_Vadjustment
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Vadjustment
      (Self        : not null access Gtk_Tree_View_Record;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   function Get_Vscroll_Policy
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy;

   procedure Set_Vscroll_Policy
      (Self   : not null access Gtk_Tree_View_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);

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
   --  Gtk.Tree_View.Gtk_Tree_View by assuming that all rows have the same
   --  height. Only enable this option if all rows are the same height. Please
   --  see Gtk.Tree_View.Set_Fixed_Height_Mode for more information on this
   --  option.

   Headers_Clickable_Property : constant Glib.Properties.Property_Boolean;

   Headers_Visible_Property : constant Glib.Properties.Property_Boolean;

   Hover_Expand_Property : constant Glib.Properties.Property_Boolean;
   --  Enables or disables the hover expansion mode of Tree_View. Hover
   --  expansion makes rows expand or collapse if the pointer moves over them.
   --
   --  This mode is primarily intended for treeviews in popups, e.g. in
   --  Gtk.Combo_Box.Gtk_Combo_Box or
   --  Gtk.Entry_Completion.Gtk_Entry_Completion.

   Hover_Selection_Property : constant Glib.Properties.Property_Boolean;
   --  Enables or disables the hover selection mode of Tree_View. Hover
   --  selection makes the selected row follow the pointer. Currently, this
   --  works only for the selection modes Gtk.Enums.Selection_Single and
   --  Gtk.Enums.Selection_Browse.
   --
   --  This mode is primarily intended for treeviews in popups, e.g. in
   --  Gtk.Combo_Box.Gtk_Combo_Box or
   --  Gtk.Entry_Completion.Gtk_Entry_Completion.

   Level_Indentation_Property : constant Glib.Properties.Property_Int;
   --  Extra indentation for each level.

   Model_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gtk.Tree_Model.Gtk_Tree_Model

   Reorderable_Property : constant Glib.Properties.Property_Boolean;

   Rubber_Banding_Property : constant Glib.Properties.Property_Boolean;

   Rules_Hint_Property : constant Glib.Properties.Property_Boolean;
   --  Sets a hint to the theme to draw rows in alternating colors.

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

   type Cb_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean is not null access function
     (Self      : access Gtk_Tree_View_Record'Class;
      Step      : Gtk.Enums.Gtk_Movement_Step;
      Direction : Glib.Gint) return Boolean;

   type Cb_GObject_Gtk_Movement_Step_Gint_Boolean is not null access function
     (Self      : access Glib.Object.GObject_Record'Class;
      Step      : Gtk.Enums.Gtk_Movement_Step;
      Direction : Glib.Gint) return Boolean;

   Signal_Move_Cursor : constant Glib.Signal_Name := "move-cursor";
   procedure On_Move_Cursor
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean;
       After : Boolean := False);
   procedure On_Move_Cursor
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The Gtk.Tree_View.Gtk_Tree_View::move-cursor signal is a [keybinding
   --  signal][GtkBindingSignal] which gets emitted when the user presses one
   --  of the cursor keys.
   --
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control the cursor
   --  programmatically. In contrast to Gtk.Tree_View.Set_Cursor and
   --  Gtk.Tree_View.Set_Cursor_On_Cell when moving horizontally
   --  Gtk.Tree_View.Gtk_Tree_View::move-cursor does not reset the current
   --  selection.
   -- 
   --  Callback parameters:
   --    --  "step": the granularity of the move, as a Gtk.Enums.Gtk_Movement_Step.
   --    --  Gtk.Enums.Movement_Logical_Positions,
   --    --  Gtk.Enums.Movement_Visual_Positions, Gtk.Enums.Movement_Display_Lines,
   --    --  Gtk.Enums.Movement_Pages and Gtk.Enums.Movement_Buffer_Ends are
   --    --  supported. Gtk.Enums.Movement_Logical_Positions and
   --    --  Gtk.Enums.Movement_Visual_Positions are treated identically.
   --    --  "direction": the direction to move: +1 to move forwards; -1 to move
   --    --  backwards. The resulting movement is undefined for all other values.
   --    --  Returns True if Step is supported, False otherwise.

   type Cb_Gtk_Tree_View_Gtk_Tree_Path_Gtk_Tree_View_Column_Void is not null access procedure
     (Self   : access Gtk_Tree_View_Record'Class;
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class);

   type Cb_GObject_Gtk_Tree_Path_Gtk_Tree_View_Column_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class);

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
   --  Gtk.Tree_View.Row_Activated is called, when the user double clicks a
   --  treeview row with the "activate-on-single-click" property set to False,
   --  or when the user single clicks a row when the "activate-on-single-click"
   --  property set to True. It is also emitted when a non-editable row is
   --  selected and one of the keys: Space, Shift+Space, Return or Enter is
   --  pressed.
   --
   --  For selection handling refer to the [tree widget conceptual
   --  overview][TreeWidget] as well as Gtk.Tree_Selection.Gtk_Tree_Selection.
   -- 
   --  Callback parameters:
   --    --  "path": the Gtk.Tree_Model.Gtk_Tree_Path for the activated row
   --    --  "column": the Gtk.Tree_View_Column.Gtk_Tree_View_Column in which the
   --    --  activation occurred

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
   --    --  "iter": the tree iter of the collapsed row
   --    --  "path": a tree path that points to the row

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
   --    --  "iter": the tree iter of the expanded row
   --    --  "path": a tree path that points to the row

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
   --    --  "iter": the tree iter of the row to collapse
   --    --  "path": a tree path that points to the row
   --    --  Returns False to allow collapsing, True to reject

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
   --    --  "iter": the tree iter of the row to expand
   --    --  "path": a tree path that points to the row
   --    --  Returns False to allow expansion, True to reject

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
   --  - "Buildable"
   --
   --  - "Scrollable"

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

   package Implements_Gtk_Scrollable is new Glib.Types.Implements
     (Gtk.Scrollable.Gtk_Scrollable, Gtk_Tree_View_Record, Gtk_Tree_View);
   function "+"
     (Widget : access Gtk_Tree_View_Record'Class)
   return Gtk.Scrollable.Gtk_Scrollable
   renames Implements_Gtk_Scrollable.To_Interface;
   function "-"
     (Interf : Gtk.Scrollable.Gtk_Scrollable)
   return Gtk_Tree_View
   renames Implements_Gtk_Scrollable.To_Object;

private
   Tooltip_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("tooltip-column");
   Show_Expanders_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-expanders");
   Search_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("search-column");
   Rules_Hint_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("rules-hint");
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
