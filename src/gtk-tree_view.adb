-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2002 ACT-Europe                 --
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

with Gdk.Pixmap;
with Gdk.Rectangle;
with Gdk.Window;
with Gdk; use Gdk;
with Gtk.Adjustment;
with Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk; use Gtk;
with Gtk.Tree_View_Column;
with System;

package body Gtk.Tree_View is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Tree_View) is
   begin
      Widget := new Gtk_Tree_View_Record;
      Gtk.Tree_View.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Tree_View_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_new");

   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Gtk_Tree_View;
      Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class) is
   begin
      Widget := new Gtk_Tree_View_Record;
      Initialize (Widget, Model);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtk_Tree_View_Record'Class;
      Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class)
   is
      function Internal (Model  : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_new_with_model");

   begin
      Set_Object (Widget, Internal (Get_Object (Model)));
   end Initialize;

   --------------------
   -- Get_Bin_Window --
   --------------------

   function Get_Bin_Window
     (Tree_View : access Gtk_Tree_View_Record) return Gdk.Window.Gdk_Window
   is
      function Internal
        (Tree_View : System.Address) return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "gtk_tree_view_get_bin_window");

   begin
      return Internal (Get_Object (Tree_View));
   end Get_Bin_Window;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_model");

      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      return Gtk.Tree_Model.Gtk_Tree_Model
        (Get_User_Data (Internal (Get_Object (Tree_View)),
                        Stub));
   end Get_Model;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
     (Tree_View : access Gtk_Tree_View_Record;
      Model     : Gtk.Tree_Model.Gtk_Tree_Model)
   is
      procedure Internal
        (Tree_View : System.Address;
         Model     : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_set_model");

   begin
      Internal (Get_Object (Tree_View), Get_Object (Model));
   end Set_Model;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Tree_Selection.Gtk_Tree_Selection
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_selection");

      Stub : Gtk.Tree_Selection.Gtk_Tree_Selection_Record;

   begin
      return Gtk.Tree_Selection.Gtk_Tree_Selection
        (Get_User_Data (Internal (Get_Object (Tree_View)),
                        Stub));
   end Get_Selection;

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_hadjustment");

      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;

   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Tree_View)),
                        Stub));
   end Get_Hadjustment;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
     (Tree_View  : access Gtk_Tree_View_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
        (Tree_View  : System.Address;
         Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_set_hadjustment");

   begin
      Internal (Get_Object (Tree_View), Get_Object (Adjustment));
   end Set_Hadjustment;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_vadjustment");

      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;

   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Tree_View)),
                        Stub));
   end Get_Vadjustment;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
     (Tree_View  : access Gtk_Tree_View_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
        (Tree_View  : System.Address;
         Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_set_vadjustment");

   begin
      Internal (Get_Object (Tree_View), Get_Object (Adjustment));
   end Set_Vadjustment;

   -------------------------
   -- Get_Headers_Visible --
   -------------------------

   function Get_Headers_Visible
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_headers_visible");

   begin
      return To_Boolean (Internal (Get_Object (Tree_View)));
   end Get_Headers_Visible;

   -------------------------
   -- Set_Headers_Visible --
   -------------------------

   procedure Set_Headers_Visible
     (Tree_View       : access Gtk_Tree_View_Record;
      Headers_Visible : Boolean)
   is
      procedure Internal
        (Tree_View       : System.Address;
         Headers_Visible : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_headers_visible");

   begin
      Internal (Get_Object (Tree_View), To_Gboolean (Headers_Visible));
   end Set_Headers_Visible;

   ----------------------
   -- Columns_Autosize --
   ----------------------

   procedure Columns_Autosize (Tree_View : access Gtk_Tree_View_Record) is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_columns_autosize");

   begin
      Internal (Get_Object (Tree_View));
   end Columns_Autosize;

   ---------------------------
   -- Set_Headers_Clickable --
   ---------------------------

   procedure Set_Headers_Clickable
     (Tree_View : access Gtk_Tree_View_Record;
      Setting   : Boolean)
   is
      procedure Internal
        (Tree_View : System.Address;
         Setting   : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_headers_clickable");

   begin
      Internal (Get_Object (Tree_View), To_Gboolean (Setting));
   end Set_Headers_Clickable;

   --------------------
   -- Set_Rules_Hint --
   --------------------

   procedure Set_Rules_Hint
     (Tree_View : access Gtk_Tree_View_Record;
      Setting   : Boolean)
   is
      procedure Internal
        (Tree_View : System.Address;
         Setting   : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_rules_hint");

   begin
      Internal (Get_Object (Tree_View), To_Gboolean (Setting));
   end Set_Rules_Hint;

   --------------------
   -- Get_Rules_Hint --
   --------------------

   function Get_Rules_Hint
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_rules_hint");

   begin
      return To_Boolean (Internal (Get_Object (Tree_View)));
   end Get_Rules_Hint;

   -------------------
   -- Append_Column --
   -------------------

   function Append_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column) return Gint
   is
      function Internal
        (Tree_View : System.Address;
         Column    : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_view_append_column");

   begin
      return Internal (Get_Object (Tree_View), Get_Object (Column));
   end Append_Column;

   -------------------
   -- Remove_Column --
   -------------------

   function Remove_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column) return Gint
   is
      function Internal
        (Tree_View : System.Address;
         Column    : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_view_remove_column");

   begin
      return Internal (Get_Object (Tree_View), Get_Object (Column));
   end Remove_Column;

   -------------------
   -- Insert_Column --
   -------------------

   function Insert_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Position  : Gint := -1) return Gint
   is
      function Internal
        (Tree_View : System.Address;
         Column    : System.Address;
         Position  : Gint) return Gint;
      pragma Import (C, Internal, "gtk_tree_view_insert_column");

   begin
      return Internal (Get_Object (Tree_View), Get_Object (Column), Position);
   end Insert_Column;

--    ----------------------------------
--    -- Insert_Column_With_Data_Func --
--    ----------------------------------

--    function Insert_Column_With_Data_Func
--      (Tree_View : access Gtk_Tree_View_Record;
--       Position  : Gint;
--       Title     : String;
--       Cell      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
--       Func      : Gtk_Tree_Cell_Data_Func;
--       Data      : gpointer;
--       Dnotify   : G_Destroy_Notify)
--       return Gint
--    is
--       function Internal
--         (Tree_View : System.Address;
--          Position  : Gint;
--          Title     : String;
--          Cell      : System.Address;
--          Func      : Gint;
--          Data      : Integer;
--          Dnotify   : Gint)
--          return Gint;
--       pragma Import
--         (C, Internal, "gtk_tree_view_insert_column_with_data_func");
--    begin
--       return Internal (Get_Object (Tree_View),
--                        Position,
--                        Title & ASCII.NUL,
--                        Get_Object (Cell),
--                        Gtk_Tree_Cell_Data_Func'Pos (Func),
--                        Data,
--                        G_Destroy_Notify'Pos (Dnotify));
--    end Insert_Column_With_Data_Func;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
     (Tree_View : access Gtk_Tree_View_Record;
      N         : Gint)
      return Gtk.Tree_View_Column.Gtk_Tree_View_Column
   is
      function Internal
        (Tree_View : System.Address;
         N         : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_column");

      Stub :  Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;

   begin
      return Gtk.Tree_View_Column.Gtk_Tree_View_Column
        (Get_User_Data (Internal (Get_Object (Tree_View), N),
                        Stub));
   end Get_Column;

   -----------------
   -- Get_Columns --
   -----------------

   function Get_Columns
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Tree_View_Column.Column_List.Glist
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_columns");

      List : Gtk.Tree_View_Column.Column_List.Glist;

   begin
      Gtk.Tree_View_Column.Column_List.Set_Object
        (List, Internal (Get_Object (Tree_View)));
      return List;
   end Get_Columns;

   -----------------------
   -- Move_Column_After --
   -----------------------

   procedure Move_Column_After
     (Tree_View   : access Gtk_Tree_View_Record;
      Column      : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Base_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column)
   is
      procedure Internal
        (Tree_View   : System.Address;
         Column      : System.Address;
         Base_Column : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_move_column_after");

   begin
      Internal (Get_Object (Tree_View),
                Get_Object (Column),
                Get_Object (Base_Column));
   end Move_Column_After;

   -------------------------
   -- Set_Expander_Column --
   -------------------------

   procedure Set_Expander_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column)
   is
      procedure Internal
        (Tree_View : System.Address;
         Column    : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_set_expander_column");

   begin
      Internal (Get_Object (Tree_View), Get_Object (Column));
   end Set_Expander_Column;

   -------------------------
   -- Get_Expander_Column --
   -------------------------

   function Get_Expander_Column
     (Tree_View : access Gtk_Tree_View_Record)
     return Gtk.Tree_View_Column.Gtk_Tree_View_Column
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_expander_column");

      Stub : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;

   begin
      return Gtk.Tree_View_Column.Gtk_Tree_View_Column
        (Get_User_Data (Internal (Get_Object (Tree_View)),
                        Stub));
   end Get_Expander_Column;

--    ------------------------------
--    -- Set_Column_Drag_Function --
--    ------------------------------

--    procedure Set_Column_Drag_Function
--      (Tree_View : access Gtk_Tree_View_Record;
--       Func      : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Drop_Func;
--       User_Data : gpointer;
--       Destroy   : Gtk_Destroy_Notify)
--    is
--       procedure Internal
--         (Tree_View : System.Address;
--          Func      : Gint;
--          User_Data : Integer;
--          Destroy   : Gint);
--       pragma Import (C, Internal, "gtk_tree_view_set_column_drag_function");
--    begin
--       Internal
--         (Get_Object (Tree_View),
--          Gtk.Tree_View_Column.Gtk_Tree_View_Column_Drop_Func'Pos (Func),
--          User_Data,
--          Gtk_Destroy_Notify'Pos (Destroy));
--    end Set_Column_Drag_Function;

   ---------------------
   -- Scroll_To_Point --
   ---------------------

   procedure Scroll_To_Point
     (Tree_View : access Gtk_Tree_View_Record;
      Tree_X    : Gint;
      Tree_Y    : Gint)
   is
      procedure Internal
        (Tree_View : System.Address;
         Tree_X    : Gint;
         Tree_Y    : Gint);
      pragma Import (C, Internal, "gtk_tree_view_scroll_to_point");

   begin
      Internal (Get_Object (Tree_View), Tree_X, Tree_Y);
   end Scroll_To_Point;

   --------------------
   -- Scroll_To_Cell --
   --------------------

   procedure Scroll_To_Cell
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Use_Align : Boolean;
      Row_Align : Gfloat;
      Col_Align : Gfloat)
   is
      procedure Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         Column    : System.Address;
         Use_Align : Gboolean;
         Row_Align : Gfloat;
         Col_Align : Gfloat);
      pragma Import (C, Internal, "gtk_tree_view_scroll_to_cell");

      Column_Address : System.Address;

      use Gtk.Tree_View_Column;
   begin
      if Column = null then
         Column_Address := System.Null_Address;
      else
         Column_Address := Get_Object (Column);
      end if;

      Internal (Get_Object (Tree_View),
                Path,
                Column_Address,
                To_Gboolean (Use_Align),
                Row_Align,
                Col_Align);
   end Scroll_To_Cell;

   -------------------
   -- Row_Activated --
   -------------------

   procedure Row_Activated
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column)
   is
      procedure Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         Column    : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_row_activated");

   begin
      Internal (Get_Object (Tree_View), Path, Get_Object (Column));
   end Row_Activated;

   ----------------
   -- Expand_All --
   ----------------

   procedure Expand_All (Tree_View : access Gtk_Tree_View_Record) is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_expand_all");

   begin
      Internal (Get_Object (Tree_View));
   end Expand_All;

   ------------------
   -- Collapse_All --
   ------------------

   procedure Collapse_All (Tree_View : access Gtk_Tree_View_Record)
   is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_collapse_all");
   begin
      Internal (Get_Object (Tree_View));
   end Collapse_All;

   ----------------
   -- Expand_Row --
   ----------------

   function Expand_Row
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Open_All  : Boolean) return Boolean
   is
      function Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         Open_All  : Gboolean) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_expand_row");

   begin
      return To_Boolean (Internal
        (Get_Object (Tree_View), Path, To_Gboolean (Open_All)));
   end Expand_Row;

   ------------------
   -- Collapse_Row --
   ------------------

   function Collapse_Row
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path)
      return Boolean
   is
      function Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_collapse_row");

   begin
      return To_Boolean (Internal (Get_Object (Tree_View), Path));
   end Collapse_Row;

--    -----------------------
--    -- Map_Expanded_Rows --
--    -----------------------

--    procedure Map_Expanded_Rows
--      (Tree_View : access Gtk_Tree_View_Record;
--       Func      : Gtk_Tree_View_Mapping_Func;
--       Data      : gpointer)
--    is
--       procedure Internal
--         (Tree_View : System.Address;
--          Func      : Gint;
--          Data      : Integer);
--       pragma Import (C, Internal, "gtk_tree_view_map_expanded_rows");
--    begin
--       Internal (Get_Object (Tree_View),
--                 Gtk_Tree_View_Mapping_Func'Pos (Func),
--                 Data);
--    end Map_Expanded_Rows;

   ------------------
   -- Row_Expanded --
   ------------------

   function Row_Expanded
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      function Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_row_expanded");

   begin
      return To_Boolean (Internal (Get_Object (Tree_View), Path));
   end Row_Expanded;

   ---------------------
   -- Set_Reorderable --
   ---------------------

   procedure Set_Reorderable
     (Tree_View   : access Gtk_Tree_View_Record;
      Reorderable : Boolean)
   is
      procedure Internal
        (Tree_View   : System.Address;
         Reorderable : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_reorderable");

   begin
      Internal (Get_Object (Tree_View), To_Gboolean (Reorderable));
   end Set_Reorderable;

   ---------------------
   -- Get_Reorderable --
   ---------------------

   function Get_Reorderable
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_reorderable");

   begin
      return To_Boolean (Internal (Get_Object (Tree_View)));
   end Get_Reorderable;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor
     (Tree_View     : access Gtk_Tree_View_Record;
      Path          : Gtk.Tree_Model.Gtk_Tree_Path;
      Focus_Column  : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Start_Editing : Boolean)
   is
      procedure Internal
        (Tree_View     : System.Address;
         Path          : Gtk.Tree_Model.Gtk_Tree_Path;
         Focus_Column  : System.Address;
         Start_Editing : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_cursor");

   begin
      Internal (Get_Object (Tree_View),
                Path,
                Get_Object (Focus_Column),
                To_Gboolean (Start_Editing));
   end Set_Cursor;

   ----------------
   -- Get_Cursor --
   ----------------

   procedure Get_Cursor
     (Tree_View    : access Gtk_Tree_View_Record;
      Path         : Gtk.Tree_Model.Gtk_Tree_Path;
      Focus_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column)
   is
      procedure Internal
        (Tree_View    : System.Address;
         Path         : Gtk.Tree_Model.Gtk_Tree_Path;
         Focus_Column : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_get_cursor");

   begin
      Internal (Get_Object (Tree_View), Path, Get_Object (Focus_Column));
   end Get_Cursor;

   ---------------------
   -- Get_Path_At_Pos --
   ---------------------

   procedure Get_Path_At_Pos
     (Tree_View : access Gtk_Tree_View_Record;
      X         : Gint;
      Y         : Gint;
      Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : out Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Cell_X    : out Gint;
      Cell_Y    : out Gint;
      Row_Found : out Boolean)
   is
      function Internal
        (Tree_View : System.Address;
         X         : Gint;
         Y         : Gint;
         Path      : access Gtk.Tree_Model.Gtk_Tree_Path;
         Column    : access System.Address;
         Cell_X    : access Gint;
         Cell_Y    : access Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_path_at_pos");

      Local_X, Local_Y : aliased Gint;

      Local_Column : aliased System.Address;
      Local_Path   : aliased Gtk.Tree_Model.Gtk_Tree_Path;

      Stub         : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;

   begin
      Row_Found := To_Boolean (Internal
        (Get_Object (Tree_View),
         X, Y,
         Local_Path'Access,
         Local_Column'Access,
         Local_X'Access, Local_Y'Access));
      Cell_X := Local_X;
      Cell_Y := Local_Y;

      Column := Gtk.Tree_View_Column.Gtk_Tree_View_Column
        (Get_User_Data (Local_Column, Stub));

      Path := Local_Path;
   end Get_Path_At_Pos;

   -------------------
   -- Get_Cell_Area --
   -------------------

   procedure Get_Cell_Area
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Rect      : Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         Column    : System.Address;
         Rect      : Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_tree_view_get_cell_area");

   begin
      Internal (Get_Object (Tree_View), Path, Get_Object (Column), Rect);
   end Get_Cell_Area;

   -------------------------
   -- Get_Background_Area --
   -------------------------

   procedure Get_Background_Area
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Rect      : Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         Column    : System.Address;
         Rect      : Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_tree_view_get_background_area");

   begin
      Internal (Get_Object (Tree_View), Path, Get_Object (Column), Rect);
   end Get_Background_Area;

   ----------------------
   -- Get_Visible_Rect --
   ----------------------

   procedure Get_Visible_Rect
     (Tree_View    : access Gtk_Tree_View_Record;
      Visible_Rect : Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
        (Tree_View    : System.Address;
         Visible_Rect : Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_tree_view_get_visible_rect");
   begin
      Internal (Get_Object (Tree_View), Visible_Rect);
   end Get_Visible_Rect;

   ---------------------------
   -- Widget_To_Tree_Coords --
   ---------------------------

   procedure Widget_To_Tree_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Wx        : Gint;
      Wy        : Gint;
      Tx        : out Gint;
      Ty        : out Gint)
   is
      procedure Internal
        (Tree_View : System.Address;
         Wx        : Gint;
         Wy        : Gint;
         Tx        : out Gint;
         Ty        : out Gint);
      pragma Import (C, Internal, "gtk_tree_view_widget_to_tree_coords");
   begin
      Internal (Get_Object (Tree_View), Wx, Wy, Tx, Ty);
   end Widget_To_Tree_Coords;

   ---------------------------
   -- Tree_To_Widget_Coords --
   ---------------------------

   procedure Tree_To_Widget_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Tx        : Gint;
      Ty        : Gint;
      Wx        : out Gint;
      Wy        : out Gint)
   is
      procedure Internal
        (Tree_View : System.Address;
         Tx        : Gint;
         Ty        : Gint;
         Wx        : out Gint;
         Wy        : out Gint);
      pragma Import (C, Internal, "gtk_tree_view_tree_to_widget_coords");
   begin
      Internal (Get_Object (Tree_View), Tx, Ty, Wx, Wy);
   end Tree_To_Widget_Coords;

--    --------------------------
--    -- Set_Rows_Drag_Source --
--    --------------------------

--    procedure Set_Rows_Drag_Source
--      (Tree_View          : access Gtk_Tree_View_Record;
--       Start_Button_Mask  : Gdk_Modifier_Type;
--       Targets            : access Gtk.Selection.Gtk_Target_Entry;
--       N_Targets          : Gint;
--       Actions            : Gdk_Drag_Action;
--       Row_Draggable_Func : Gtk_Tree_View_Draggable_Func;
--       User_Data          : gpointer)
--    is
--       procedure Internal
--         (Tree_View          : System.Address;
--          Start_Button_Mask  : Gint;
--          Targets            : System.Address;
--          N_Targets          : Gint;
--          Actions            : Gint;
--          Row_Draggable_Func : Gint;
--          User_Data          : Integer);
--       pragma Import (C, Internal, "gtk_tree_view_set_rows_drag_source");
--    begin
--       Internal (Get_Object (Tree_View),
--                 Gdk_Modifier_Type'Pos (Start_Button_Mask),
--                 Get_Object (Targets),
--                 N_Targets,
--                 Gdk_Drag_Action'Pos (Actions),
--                 Gtk_Tree_View_Draggable_Func'Pos (Row_Draggable_Func),
--                 User_Data);
--    end Set_Rows_Drag_Source;

--    ------------------------
--    -- Set_Rows_Drag_Dest --
--    ------------------------

--    procedure Set_Rows_Drag_Dest
--      (Tree_View               : access Gtk_Tree_View_Record;
--       Targets                 :
--         access Gtk.Selection.Gtk_Target_Entry_Record'Class;
--       N_Targets               : Gint;
--       Actions                 : Gdk_Drag_Action;
--       Location_Droppable_Func : Gtk_Tree_View_Droppable_Func;
--       User_Data               : gpointer)
--    is
--       procedure Internal
--         (Tree_View               : System.Address;
--          Targets                 : System.Address;
--          N_Targets               : Gint;
--          Actions                 : Gint;
--          Location_Droppable_Func : Gtk_Tree_View_Droppable_Func;
--          User_Data               : Integer);
--       pragma Import (C, Internal, "gtk_tree_view_set_rows_drag_dest");
--    begin
--       Internal (Get_Object (Tree_View),
--                 Get_Object (Targets),
--                 N_Targets,
--                 Gdk_Drag_Action'Pos (Actions),
--                 Location_Droppable_Func,
--                 User_Data);
--    end Set_Rows_Drag_Dest;

   ----------------------------
   -- Unset_Rows_Drag_Source --
   ----------------------------

   procedure Unset_Rows_Drag_Source
     (Tree_View : access Gtk_Tree_View_Record)
   is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_unset_rows_drag_source");
   begin
      Internal (Get_Object (Tree_View));
   end Unset_Rows_Drag_Source;

   --------------------------
   -- Unset_Rows_Drag_Dest --
   --------------------------

   procedure Unset_Rows_Drag_Dest (Tree_View : access Gtk_Tree_View_Record) is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_unset_rows_drag_dest");
   begin
      Internal (Get_Object (Tree_View));
   end Unset_Rows_Drag_Dest;

--    -----------------------
--    -- Set_Drag_Dest_Row --
--    -----------------------

--    procedure Set_Drag_Dest_Row
--      (Tree_View : access Gtk_Tree_View_Record;
--       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
--       Pos       : Gtk_Tree_View_Drop_Position)
--    is
--       procedure Internal
--         (Tree_View : System.Address;
--          Path      : Gtk.Tree_Model.Gtk_Tree_Path;
--          Pos       : Gtk_Tree_View_Drop_Position);
--       pragma Import (C, Internal, "gtk_tree_view_set_drag_dest_row");
--    begin
--       Internal (Get_Object (Tree_View), Path, Pos);
--    end Set_Drag_Dest_Row;

--    -----------------------
--    -- Get_Drag_Dest_Row --
--    -----------------------

--    procedure Get_Drag_Dest_Row
--      (Tree_View : access Gtk_Tree_View_Record;
--       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
--       Pos       : Gtk_Tree_View_Drop_Position)
--    is
--       procedure Internal
--         (Tree_View : System.Address;
--          Path      : Gtk.Tree_Model.Gtk_Tree_Path;
--          Pos       : Gtk_Tree_View_Drop_Position);
--       pragma Import (C, Internal, "gtk_tree_view_get_drag_dest_row");
--    begin
--       Internal (Get_Object (Tree_View), Path, Pos);
--    end Get_Drag_Dest_Row;

--    -------------------------
--    -- Get_Dest_Row_At_Pos --
--    -------------------------

--    function Get_Dest_Row_At_Pos
--      (Tree_View : access Gtk_Tree_View_Record;
--       Drag_X    : Gint;
--       Drag_Y    : Gint;
--       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
--       Pos       : Gtk_Tree_View_Drop_Position)
--       return Boolean
--    is
--       function Internal
--         (Tree_View : System.Address;
--          Drag_X    : Gint;
--          Drag_Y    : Gint;
--          Path      : Gtk.Tree_Model.Gtk_Tree_Path;
--          Pos       : Gtk_Tree_View_Drop_Position)
--          return Gboolean;
--       pragma Import (C, Internal, "gtk_tree_view_get_dest_row_at_pos");
--    begin
--       return To_Boolean (Internal
--         (Get_Object (Tree_View), Drag_X, Drag_Y, Path, Pos));
--    end Get_Dest_Row_At_Pos;

   --------------------------
   -- Create_Row_Drag_Icon --
   --------------------------

   function Create_Row_Drag_Icon
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gdk.Pixmap.Gdk_Pixmap
   is
      function Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path)
         return Gdk.Pixmap.Gdk_Pixmap;
      pragma Import (C, Internal, "gtk_tree_view_create_row_drag_icon");

   begin
      return Internal (Get_Object (Tree_View), Path);
   end Create_Row_Drag_Icon;

   -----------------------
   -- Set_Enable_Search --
   -----------------------

   procedure Set_Enable_Search
     (Tree_View     : access Gtk_Tree_View_Record;
      Enable_Search : Boolean)
   is
      procedure Internal
        (Tree_View     : System.Address;
         Enable_Search : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_enable_search");

   begin
      Internal (Get_Object (Tree_View), To_Gboolean (Enable_Search));
   end Set_Enable_Search;

   -----------------------
   -- Get_Enable_Search --
   -----------------------

   function Get_Enable_Search
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_enable_search");
   begin
      return To_Boolean (Internal (Get_Object (Tree_View)));
   end Get_Enable_Search;

   -----------------------
   -- Get_Search_Column --
   -----------------------

   function Get_Search_Column
     (Tree_View : access Gtk_Tree_View_Record) return Gint
   is
      function Internal (Tree_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_view_get_search_column");
   begin
      return Internal (Get_Object (Tree_View));
   end Get_Search_Column;

   -----------------------
   -- Set_Search_Column --
   -----------------------

   procedure Set_Search_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gint)
   is
      procedure Internal
        (Tree_View : System.Address;
         Column    : Gint);
      pragma Import (C, Internal, "gtk_tree_view_set_search_column");

   begin
      Internal (Get_Object (Tree_View), Column);
   end Set_Search_Column;

--    ---------------------------
--    -- Get_Search_Equal_Func --
--    ---------------------------

--    function Get_Search_Equal_Func (Tree_View : access Gtk_Tree_View_Record)
--                                return Gtk_Tree_View_Search_Equal_Func
--    is
--       function Internal (Tree_View : System.Address)
--                          return Gint;
--       pragma Import (C, Internal, "gtk_tree_view_get_search_equal_func");
--    begin
--       return Gtk_Tree_View_Search_Equal_Func'Val
--                (Internal (Get_Object (Tree_View)));
--    end Get_Search_Equal_Func;

--    ---------------------------
--    -- Set_Search_Equal_Func --
--    ---------------------------

--    procedure Set_Search_Equal_Func
--      (Tree_View         : access Gtk_Tree_View_Record;
--       Search_Equal_Func : Gtk_Tree_View_Search_Equal_Func;
--       Search_User_Data  : gpointer;
--       Search_Destroy    : Gtk_Destroy_Notify)
--    is
--       procedure Internal
--         (Tree_View         : System.Address;
--          Search_Equal_Func : Gint;
--          Search_User_Data  : Integer;
--          Search_Destroy    : Gint);
--       pragma Import (C, Internal, "gtk_tree_view_set_search_equal_func");
--    begin
--       Internal (Get_Object (Tree_View),
--                 Gtk_Tree_View_Search_Equal_Func'Pos (Search_Equal_Func),
--                 Search_User_Data,
--                 Gtk_Destroy_Notify'Pos (Search_Destroy));
--    end Set_Search_Equal_Func;

--    ----------------------------
--    -- Set_Destroy_Count_Func --
--    ----------------------------

--    procedure Set_Destroy_Count_Func
--      (Tree_View : access Gtk_Tree_View_Record;
--       Func      : Gtk_Tree_Destroy_Count_Func;
--       Data      : gpointer;
--       Destroy   : Gtk_Destroy_Notify)
--    is
--       procedure Internal
--         (Tree_View : System.Address;
--          Func      : Gint;
--          Data      : Integer;
--          Destroy   : Gint);
--       pragma Import (C, Internal, "gtk_tree_view_set_destroy_count_func");
--    begin
--       Internal (Get_Object (Tree_View),
--                 Gtk_Tree_Destroy_Count_Func'Pos (Func),
--                 Data,
--                 Gtk_Destroy_Notify'Pos (Destroy));
--    end Set_Destroy_Count_Func;

end Gtk.Tree_View;
