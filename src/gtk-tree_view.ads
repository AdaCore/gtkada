-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2003 ACT-Europe                 --
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
--  See extended documentation in Gtk.Tree_View_Column and Gtk.Tree_Store.
--  </description>

--  <c_version>1.3.11</c_version>

with Gdk.Pixmap;
with Gdk.Rectangle;
with Gdk.Window;
with Gtk;
with Gtk.Adjustment;
with Gtk.Container;
with Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_View_Column;

package Gtk.Tree_View is

   type Gtk_Tree_View_Record is
     new Gtk.Container.Gtk_Container_Record with private;
   type Gtk_Tree_View is access all Gtk_Tree_View_Record'Class;

   type Gtk_Tree_View_Drop_Position is
     (Tree_View_Drop_Before,
      Tree_View_Drop_After,
      Tree_View_Drop_Into_Or_Before,
      Tree_View_Drop_Into_Or_After);
   pragma Convention (C, Gtk_Tree_View_Drop_Position);

   procedure Gtk_New (Widget : out Gtk_Tree_View);

   procedure Initialize (Widget : access Gtk_Tree_View_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Gtk_New
     (Widget : out Gtk_Tree_View;
      Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);

   procedure Initialize
     (Widget : access Gtk_Tree_View_Record'Class;
      Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Model
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns the model the the Gtk_Tree_View is based on.  Returns Null if
   --  the model is unset.

   procedure Set_Model
     (Tree_View : access Gtk_Tree_View_Record;
      Model     : Gtk.Tree_Model.Gtk_Tree_Model);
   --  Sets the model for a Gtk_Tree_View.  If the Tree_View already has a
   --  model set, it will remove it before setting the new model.
   --  If Model is Null, then it will unset the old model.

   function Get_Selection
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Tree_Selection.Gtk_Tree_Selection;
   --  Gets the Gtk_Tree_Selection associated with Tree_View.

   function Get_Hadjustment
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Adjustment.Gtk_Adjustment;
   --  Sets the Gtk_Adjustment for the current horizontal aspect.

   procedure Set_Hadjustment
     (Tree_View  : access Gtk_Tree_View_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the Gtk_Adjustment for the current horizontal aspect.

   function Get_Vadjustment
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Adjustment.Gtk_Adjustment;
   --  Gets the Gtk_Adjustment currently being used for the vertical aspect.

   procedure Set_Vadjustment
     (Tree_View  : access Gtk_Tree_View_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the Gtk_Adjustment for the current vertical aspect.

   ----------------------------------
   -- Column and header operations --
   ----------------------------------

   function Get_Headers_Visible
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;
   --  Returns True if the headers on the Tree_View are visible.

   procedure Set_Headers_Visible
     (Tree_View       : access Gtk_Tree_View_Record;
      Headers_Visible : Boolean);
   --  Sets the the visibility state of the headers.

   procedure Columns_Autosize (Tree_View : access Gtk_Tree_View_Record);
   --  Resizes all columns to their optimal width.

   procedure Set_Headers_Clickable
     (Tree_View : access Gtk_Tree_View_Record;
      Setting   : Boolean);
   --  Allow the column title buttons to be clicked.

   procedure Set_Rules_Hint
     (Tree_View : access Gtk_Tree_View_Record;
      Setting   : Boolean);
   --  This function tells GtkAda that the user interface for your
   --  application requires users to read across tree rows and associate
   --  cells with one another. By default, GtkAda will then render the tree
   --  with alternating row colors. Do *not* use it just because you prefer the
   --  appearance of the ruled tree; that's a question for the theme. Some
   --  themes will draw tree rows in alternating colors even when rules are
   --  turned off, and users who prefer that appearance all the time can choose
   --  those themes. You should call this function only as a *semantic*
   --  hint to the theme engine that your tree makes alternating colors
   --  useful from a functional standpoint (since it has lots of columns,
   --  generally).

   function Get_Rules_Hint
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;
   --  Get the setting set by Set_Rules_Hint.

   -----------------------------
   -- Public Column functions --
   -----------------------------

   function Append_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column) return Gint;
   --  Append Column to the list of columns.

   function Remove_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column) return Gint;
   --  Remove Column from Tree_View.
   --  Return value: The number of columns in Tree_View after removing.

   function Insert_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Position  : Gint := -1) return Gint;
   --  Insert the Column into the Tree_View at Position.
   --  If Position is -1, then the column is inserted at the end.
   --  Return the number of columns in Tree_View after insertion.

   function Get_Column
     (Tree_View : access Gtk_Tree_View_Record;
      N         : Gint)
      return Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   --  Gets the Gtk_Tree_ViewColumn at the given position in the Tree_View.

   function Get_Columns
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Tree_View_Column.Column_List.Glist;
   --  Return a list of all the Gtk_Tree_View_Column s currently in Tree_View.
   --  The returned list must be freed with g_list_free ().

   procedure Move_Column_After
     (Tree_View   : access Gtk_Tree_View_Record;
      Column      : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Base_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   --  Move Column to be after to Base_Column.  If Base_Column is Null, then
   --  Column is placed in the first position.

   procedure Set_Expander_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   --  Set the column to draw the expander arrow at. It must be in Tree_View.
   --  If Column is Null, then the expander arrow is fixed at the first column.

   function Get_Expander_Column
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   --  Return the column that is the current expander column.
   --  This column has the expander arrow drawn next to it.

   procedure Scroll_To_Point
     (Tree_View : access Gtk_Tree_View_Record;
      Tree_X    : Gint;
      Tree_Y    : Gint);
   --  Scroll the tree view such that the top-left corner of the visible
   --  area is Tree_X, Tree_Y, where Tree_X and Tree_Y are specified
   --  in tree window coordinates. The Tree_View must be realized before
   --  this function is called. If it isn't, you probably want to be
   --  using Scroll_To_Cell.

   procedure Scroll_To_Cell
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Use_Align : Boolean;
      Row_Align : Gfloat;
      Col_Align : Gfloat);
   --  Move the alignments of Tree_View to the position specified by Column and
   --  Path. If Column is Null, then no horizontal scrolling occurs. Likewise,
   --  if Path is Null no vertical scrolling occurs. Row_Align determines where
   --  the row is placed, and Col_align determines where Column is placed. Both
   --  are expected to be between 0.0 and 1.0. 0.0 means left/top alignment,
   --  1.0 means right/bottom alignment, 0.5 means center.
   --  If Use_Align is False, then the alignment arguments are ignored, and the
   --  tree does the minimum amount of work to scroll the cell onto the screen.

   procedure Row_Activated
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   --  Activate the cell determined by Path and Column.

   procedure Expand_All (Tree_View : access Gtk_Tree_View_Record);
   --  Recursively expand all nodes in the Tree_View.

   procedure Collapse_All (Tree_View : access Gtk_Tree_View_Record);
   --  Recursively collapse all visible, expanded nodes in Tree_View.

   function Expand_Row
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Open_All  : Boolean) return Boolean;
   --  Open the row so its children are visible
   --  Return True if the row existed and had children

   function Collapse_Row
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   --  Collapse a row (hides its child rows, if they exist.)

   function Row_Expanded
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   --  Return True if the node pointed to by Path is expanded in Tree_View.

   procedure Set_Reorderable
     (Tree_View   : access Gtk_Tree_View_Record;
      Reorderable : Boolean);
   --  This function is a convenience function to allow you to reorder models
   --  that support the Gtk_Drag_Source_Iface and the Gtk_Drag_Dest_Iface. Both
   --  Gtk_Tree_Store and Gtk_List_Store support these.
   --  If Reorderable is True, then the user can reorder the model by dragging
   --  and dropping columns.  The developer can listen to these changes by
   --  connecting to the model's signals.
   --  This function does not give you any degree of control over the order
   --  - any reorderering is allowed. If more control is needed, you should
   --  probably handle drag and drop manually.

   function Get_Reorderable
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;
   --  Retrieve whether the user can reorder the tree via drag-and-drop.
   --  See Set_Reorderable.

   procedure Set_Cursor
     (Tree_View     : access Gtk_Tree_View_Record;
      Path          : Gtk.Tree_Model.Gtk_Tree_Path;
      Focus_Column  : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Start_Editing : Boolean);
   --  Sets the current keyboard focus to be at Path, and selects it.  This is
   --  useful when you want to focus the user's attention on a particular row.
   --  If Column is not Null, then focus is given to that column.
   --  Additionally, if Column is specified, and Start_Editing is True, then
   --  editing should be started in the specified cell.
   --  Keyboard focus is given to the widget after this is called.
   --  Please note that editing can only happen when the widget is realized.

   procedure Get_Cursor
     (Tree_View    : access Gtk_Tree_View_Record;
      Path         : out Gtk.Tree_Model.Gtk_Tree_Path;
      Focus_Column : out Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   --  Fills in Path and Focus_Column with the current path and focus column.
   --  If the cursor isn't currently set, then *path will be Null. If no column
   --  currently has focus, then *focus_column will be Null.

   function Get_Bin_Window
     (Tree_View : access Gtk_Tree_View_Record) return Gdk.Window.Gdk_Window;
   --  Return the window that Tree_View renders to.
   --  This is used primarily to compare to Get_Window (Event) to confirm that
   --  the event on Tree_View is on the right window.

   procedure Get_Path_At_Pos
     (Tree_View : access Gtk_Tree_View_Record;
      X         : Gint;
      Y         : Gint;
      Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : out Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Cell_X    : out Gint;
      Cell_Y    : out Gint;
      Row_Found : out Boolean);
   --  Find the path at the point (X, Y) relative to Window.
   --  If Window is null, then the point is found relative to the widget
   --  coordinates. This function is expected to be called after an event.
   --  It is primarily for things like popup menus. Path will be filled
   --  with the Gtk_Tree_Path at that point. It should be freed with
   --  Tree_Path_Free. Column will be filled with the column at that point.
   --  Cell_X and Cell_Y return the coordinates relative to the cell background
   --  (i.e. the background_area passed to gtk_cell_renderer_render()).
   --  This function only works if Tree_View is realized.
   --  Row_Found is set to True if a row exists at that coordinate.

   procedure Get_Cell_Area
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Rect      : out Gdk.Rectangle.Gdk_Rectangle);
   --  Fills the bounding rectangle in tree window coordinates for the cell at
   --  the row specified by Path and the column specified by Column. If Path is
   --  Null, or points to a path not currently displayed, the Y and Height
   --  fields of the rectangle will be filled with 0. If Column is Null,
   --  the X and Width fields will be filled with 0.
   --  The sum of all cell rects does not cover the entire tree;
   --  there are extra pixels in between rows, for example. The
   --  returned rectangle is equivalent to the Cell_Area passed to
   --  gtk_cell_renderer_render().  This function is only valid if Tree_View is
   --  realized.

   procedure Get_Background_Area
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Rect      : Gdk.Rectangle.Gdk_Rectangle);
   --  Fills the bounding rectangle in tree window coordinates for the cell
   --  at the row specified by Path and the column specified by Column.  If
   --  Path is Null, or points to a node not found in the tree, the Y and
   --  Height fields of the rectangle will be filled with 0. If Column is Null,
   --  the X and Width fields will be filled with 0.
   --  The returned rectangle is equivalent to the Background_Area passed to
   --  Gtk.Cell_Renderer.Render.  These background areas tile to cover the
   --  entire tree window (except for the area used for
   --  header buttons). Contrast with the cell_area, returned by
   --  gtk_tree_view_get_cell_area(), which returns only the cell itself,
   --  excluding surrounding borders and the tree expander area.

   procedure Get_Visible_Rect
     (Tree_View    : access Gtk_Tree_View_Record;
      Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle);
   --  Fills Visible_Rect with the currently-visible region of the
   --  buffer, in tree coordinates. Convert to widget coordinates with
   --  gtk_tree_view_tree_to_widget_coords(). Tree coordinates start at
   --  0,0 for row 0 of the tree, and cover the entire scrollable area of
   --  the tree.

   procedure Widget_To_Tree_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Wx        : Gint;
      Wy        : Gint;
      Tx        : out Gint;
      Ty        : out Gint);
   --  Converts widget coordinates to coordinates for the
   --  tree window (the full scrollable area of the tree).

   procedure Tree_To_Widget_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Tx        : Gint;
      Ty        : Gint;
      Wx        : out Gint;
      Wy        : out Gint);
   --  Converts tree coordinates (coordinates in full scrollable area of
   --  the tree) to widget coordinates.

   procedure Unset_Rows_Drag_Source (Tree_View : access Gtk_Tree_View_Record);

   procedure Unset_Rows_Drag_Dest (Tree_View : access Gtk_Tree_View_Record);

   function Create_Row_Drag_Icon
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gdk.Pixmap.Gdk_Pixmap;

   procedure Set_Enable_Search
     (Tree_View     : access Gtk_Tree_View_Record;
      Enable_Search : Boolean);

   function Get_Enable_Search
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;

   function Get_Search_Column
     (Tree_View : access Gtk_Tree_View_Record) return Gint;

   procedure Set_Search_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gint);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "set_scroll_adjustments"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
   --       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --
   --  - "row_activated"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Path : Gtk.Tree_Model.Gtk_Tree_Path;
   --       Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   --
   --  - "test_expand_row"
   --    function Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Iter : access Gtk.Tree_Iter.Gtk_Tree_Iter_Record'Class;
   --       Path : Gtk.Tree_Model.Gtk_Tree_Path)
   --       return Gboolean;
   --
   --  - "test_collapse_row"
   --    function Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Iter : access Gtk.Tree_Iter.Gtk_Tree_Iter_Record'Class;
   --       Path : Gtk.Tree_Model.Gtk_Tree_Path)
   --       return Gboolean;
   --
   --  - "row_expanded"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Iter : access Gtk.Tree_Iter.Gtk_Tree_Iter_Record'Class;
   --       Path : Gtk.Tree_Model.Gtk_Tree_Path);
   --
   --  - "row_collapsed"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Iter : access Gtk.Tree_Iter.Gtk_Tree_Iter_Record'Class;
   --       Path : Gtk.Tree_Model.Gtk_Tree_Path);
   --
   --  - "columns_changed"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class);
   --
   --  - "move_cursor"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Step : Gtk_Movement_Step;
   --       Count : Gint);
   --
   --  - "select_all"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class);
   --
   --  - "select_cursor_row"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Start_Editing : Boolean);
   --
   --  - "toggle_cursor_row"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class);
   --
   --  - "expand_collapse_cursor_row"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Logical : Boolean;
   --       Expand : Boolean;
   --       Open_All : Boolean);
   --
   --  - "select_cursor_parent"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class);
   --
   --  - "start_interactive_search"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class);
   --
   --  </signals>

private
   type Gtk_Tree_View_Record is
     new Gtk.Container.Gtk_Container_Record with null record;

   pragma Import (C, Get_Type, "gtk_tree_view_get_type");
end Gtk.Tree_View;

--  Missing :

--    type Gtk_Tree_View_Mapping_Func is access
--      procedure (Tree_View : Gtk_Tree_View;
--              Path      : Gtk.Tree_Model.Gtk_Tree_Path;

--    type Gtk_Tree_View_Search_Equal_Func is access
--      function (Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
--             Column : Gint;
--             Key    : String;
--             Iter   : access Gtk.Tree_Model.Gtk_Tree_Iter_Record'Class;

--    function Get_Search_Equal_Func (Tree_View : access Gtk_Tree_View_Record)
--                                   return Gtk_Tree_View_Search_Equal_Func;

--    procedure Set_Search_Equal_Func
--      (Tree_View         : access Gtk_Tree_View_Record;
--       Search_Equal_Func : Gtk_Tree_View_Search_Equal_Func;
--       Search_User_Data  : gpointer;
--       Search_Destroy    : Gtk_Destroy_Notify);

--    procedure Set_Destroy_Count_Func
--      (Tree_View : access Gtk_Tree_View_Record;
--       Func      : Gtk_Tree_Destroy_Count_Func;
--       Data      : gpointer;
--       Destroy   : Gtk_Destroy_Notify);

--    procedure Set_Drag_Dest_Row
--      (Tree_View : access Gtk_Tree_View_Record;
--       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
--       Pos       : Gtk_Tree_View_Drop_Position);

--    procedure Get_Drag_Dest_Row
--      (Tree_View : access Gtk_Tree_View_Record;
--       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
--       Pos       : access Gtk_Tree_View_Drop_Position_Record);

--    function Get_Dest_Row_At_Pos
--      (Tree_View : access Gtk_Tree_View_Record;
--       Drag_X    : Gint;
--       Drag_Y    : Gint;
--       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
--       Pos       : access Gtk_Tree_View_Drop_Position_Record)
--       return Boolean;

--    procedure Set_Rows_Drag_Source
--      (Tree_View          : access Gtk_Tree_View_Record;
--       Start_Button_Mask  : Gdk_Modifier_Type;
--       Targets            : access Gtk.Selection.Gtk_Target_Entry;
--       N_Targets          : Gint;
--       Actions            : Gdk_Drag_Action;
--       Row_Draggable_Func : Gtk_Tree_View_Draggable_Func;
--       User_Data          : gpointer);

--    procedure Set_Rows_Drag_Dest
--      (Tree_View               : access Gtk_Tree_View_Record;
--       Targets                 :
--         access Gtk.Selection.Gtk_Target_Entry_Record'Class;
--       N_Targets               : Gint;
--       Actions                 : Gdk_Drag_Action;
--       Location_Droppable_Func : Gtk_Tree_View_Droppable_Func;
--       User_Data               : gpointer);

--    procedure Set_Column_Drag_Function
--      (Tree_View : access Gtk_Tree_View_Record;
--       Func      : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Drop_Func;
--       User_Data : gpointer;
--       Destroy   : Gtk_Destroy_Notify);

--    procedure Map_Expanded_Rows
--      (Tree_View : access Gtk_Tree_View_Record;
--       Func      : Gtk_Tree_View_Mapping_Func;
--       Data      : gpointer);

--    function Insert_Column_With_Data_Func
--      (Tree_View : access Gtk_Tree_View_Record;
--       Position  : Gint;
--       Title     : UTF8_String;
--       Cell      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
--       Func      : Gtk_Tree_Cell_Data_Func;
--       Data      : gpointer;
--       Dnotify   : G_Destroy_Notify)
--       return Gint;
