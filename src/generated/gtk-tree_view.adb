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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;

package body Gtk.Tree_View is

   procedure Gtk_New
     (Tree_View : out Gtk_Tree_View;
      Model     : access Gtk.Tree_Model.Gtk_Root_Tree_Model_Record'Class)
   is
   begin
      Gtk_New (Tree_View, To_Interface (Model));
   end Gtk_New;

   function C_Gtk_Tree_View_Insert_Column_With_Data_Func
      (Tree_View : System.Address;
       Position  : Glib.Gint;
       Title     : Gtkada.Types.Chars_Ptr;
       Cell      : System.Address;
       Func      : System.Address;
       Data      : System.Address;
       Dnotify   : Glib.G_Destroy_Notify_Address) return Glib.Gint;
   pragma Import (C, C_Gtk_Tree_View_Insert_Column_With_Data_Func, "gtk_tree_view_insert_column_with_data_func");
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
   --  "data": data for Func
   --  "dnotify": destroy notifier for Data

   procedure C_Gtk_Tree_View_Map_Expanded_Rows
      (Tree_View : System.Address;
       Func      : System.Address;
       Data      : System.Address);
   pragma Import (C, C_Gtk_Tree_View_Map_Expanded_Rows, "gtk_tree_view_map_expanded_rows");
   --  Calls Func on all expanded rows.
   --  "func": A function to be called
   --  "data": User data to be passed to the function.

   procedure C_Gtk_Tree_View_Set_Column_Drag_Function
      (Tree_View : System.Address;
       Func      : System.Address;
       User_Data : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Tree_View_Set_Column_Drag_Function, "gtk_tree_view_set_column_drag_function");
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
   --  "user_data": User data to be passed to Func, or null
   --  "destroy": Destroy notifier for User_Data, or null

   procedure C_Gtk_Tree_View_Set_Destroy_Count_Func
      (Tree_View : System.Address;
       Func      : System.Address;
       Data      : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Tree_View_Set_Destroy_Count_Func, "gtk_tree_view_set_destroy_count_func");
   pragma Obsolescent (C_Gtk_Tree_View_Set_Destroy_Count_Func);
   --  This function should almost never be used. It is meant for private use
   --  by ATK for determining the number of visible children that are removed
   --  when the user collapses a row, or a row is deleted.
   --  Deprecated since 3.4, 1
   --  "func": Function to be called when a view row is destroyed, or null
   --  "data": User data to be passed to Func, or null
   --  "destroy": Destroy notifier for Data, or null

   procedure C_Gtk_Tree_View_Set_Row_Separator_Func
      (Tree_View : System.Address;
       Func      : System.Address;
       Data      : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Tree_View_Set_Row_Separator_Func, "gtk_tree_view_set_row_separator_func");
   --  Sets the row separator function, which is used to determine whether a
   --  row should be drawn as a separator. If the row separator function is
   --  null, no separators are drawn. This is the default value.
   --  Since: gtk+ 2.6
   --  "func": a Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func
   --  "data": user data to pass to Func, or null
   --  "destroy": destroy notifier for Data, or null

   procedure C_Gtk_Tree_View_Set_Search_Equal_Func
      (Tree_View         : System.Address;
       Search_Equal_Func : System.Address;
       Search_User_Data  : System.Address;
       Search_Destroy    : Glib.G_Destroy_Notify_Address);
   pragma Import (C, C_Gtk_Tree_View_Set_Search_Equal_Func, "gtk_tree_view_set_search_equal_func");
   --  Sets the compare function for the interactive search capabilities; note
   --  that somewhat like strcmp returning 0 for equality
   --  Gtk_Tree_View_Search_Equal_Func returns False on matches.
   --  "search_equal_func": the compare function to use during the search
   --  "search_user_data": user data to pass to Search_Equal_Func, or null
   --  "search_destroy": Destroy notifier for Search_User_Data, or null

   procedure C_Gtk_Tree_View_Set_Search_Position_Func
      (Tree_View : System.Address;
       Func      : System.Address;
       Data      : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Tree_View_Set_Search_Position_Func, "gtk_tree_view_set_search_position_func");
   --  Sets the function to use when positioning the search dialog.
   --  Since: gtk+ 2.10
   --  "func": the function to use to position the search dialog, or null to
   --  use the default search position function
   --  "data": user data to pass to Func, or null
   --  "destroy": Destroy notifier for Data, or null

   function To_Gtk_Tree_Cell_Data_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Cell_Data_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_Cell_Data_Func, System.Address);

   function To_Gtk_Tree_View_Mapping_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_View_Mapping_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_View_Mapping_Func, System.Address);

   function To_Gtk_Tree_View_Column_Drop_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_View_Column_Drop_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_View_Column_Drop_Func, System.Address);

   function To_Gtk_Tree_Destroy_Count_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Destroy_Count_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_Destroy_Count_Func, System.Address);

   function To_Gtk_Tree_View_Row_Separator_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_View_Row_Separator_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func, System.Address);

   function To_Gtk_Tree_View_Search_Equal_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_View_Search_Equal_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_View_Search_Equal_Func, System.Address);

   function To_Gtk_Tree_View_Search_Position_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_View_Search_Position_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_View_Search_Position_Func, System.Address);

   procedure Internal_Gtk_Tree_Cell_Data_Func
      (Tree_Column : System.Address;
       Cell        : System.Address;
       Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data        : System.Address);
   pragma Convention (C, Internal_Gtk_Tree_Cell_Data_Func);
   --  "tree_column": A Gtk.Tree_View_Column.Gtk_Tree_View_Column
   --  "cell": The Gtk.Cell_Renderer.Gtk_Cell_Renderer that is being rendered
   --  by Tree_Column
   --  "tree_model": The Gtk.Tree_Model.Gtk_Tree_Model being rendered
   --  "iter": A Gtk.Tree_Model.Gtk_Tree_Iter of the current row rendered
   --  "data": user data

   procedure Internal_Gtk_Tree_Destroy_Count_Func
      (Tree_View : System.Address;
       Path      : System.Address;
       Children  : Glib.Gint;
       User_Data : System.Address);
   pragma Convention (C, Internal_Gtk_Tree_Destroy_Count_Func);

   function Internal_Gtk_Tree_View_Column_Drop_Func
      (Tree_View   : System.Address;
       Column      : System.Address;
       Prev_Column : System.Address;
       Next_Column : System.Address;
       Data        : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_Tree_View_Column_Drop_Func);
   --  "tree_view": A Gtk.Tree_View.Gtk_Tree_View
   --  "column": The Gtk.Tree_View_Column.Gtk_Tree_View_Column being dragged
   --  "prev_column": A Gtk.Tree_View_Column.Gtk_Tree_View_Column on one side
   --  of Column
   --  "next_column": A Gtk.Tree_View_Column.Gtk_Tree_View_Column on the other
   --  side of Column
   --  "data": user data

   procedure Internal_Gtk_Tree_View_Mapping_Func
      (Tree_View : System.Address;
       Path      : System.Address;
       User_Data : System.Address);
   pragma Convention (C, Internal_Gtk_Tree_View_Mapping_Func);
   --  "tree_view": A Gtk.Tree_View.Gtk_Tree_View
   --  "path": The path that's expanded
   --  "user_data": user data

   function Internal_Gtk_Tree_View_Row_Separator_Func
      (Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data  : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_Tree_View_Row_Separator_Func);
   --  "model": the Gtk.Tree_Model.Gtk_Tree_Model
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter pointing at a row in Model
   --  "data": user data

   function Internal_Gtk_Tree_View_Search_Equal_Func
      (Model       : Gtk.Tree_Model.Gtk_Tree_Model;
       Column      : Glib.Gint;
       Key         : Gtkada.Types.Chars_Ptr;
       Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Search_Data : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_Tree_View_Search_Equal_Func);
   --  "model": the Gtk.Tree_Model.Gtk_Tree_Model being searched
   --  "column": the search column set by Gtk.Tree_View.Set_Search_Column
   --  "key": the key string to compare with
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter pointing the row of Model that
   --  should be compared with Key.
   --  "search_data": user data from Gtk.Tree_View.Set_Search_Equal_Func

   procedure Internal_Gtk_Tree_View_Search_Position_Func
      (Tree_View     : System.Address;
       Search_Dialog : System.Address;
       User_Data     : System.Address);
   pragma Convention (C, Internal_Gtk_Tree_View_Search_Position_Func);

   --------------------------------------
   -- Internal_Gtk_Tree_Cell_Data_Func --
   --------------------------------------

   procedure Internal_Gtk_Tree_Cell_Data_Func
      (Tree_Column : System.Address;
       Cell        : System.Address;
       Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data        : System.Address)
   is
      Func                      : constant Gtk_Tree_Cell_Data_Func := To_Gtk_Tree_Cell_Data_Func (Data);
      Stub_Gtk_Tree_View_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;
      Stub_Gtk_Cell_Renderer    : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
   begin
      Func (Gtk.Tree_View_Column.Gtk_Tree_View_Column (Get_User_Data (Tree_Column, Stub_Gtk_Tree_View_Column)), Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Cell, Stub_Gtk_Cell_Renderer)), Tree_Model, Iter.all);
   end Internal_Gtk_Tree_Cell_Data_Func;

   ------------------------------------------
   -- Internal_Gtk_Tree_Destroy_Count_Func --
   ------------------------------------------

   procedure Internal_Gtk_Tree_Destroy_Count_Func
      (Tree_View : System.Address;
       Path      : System.Address;
       Children  : Glib.Gint;
       User_Data : System.Address)
   is
      Func               : constant Gtk_Tree_Destroy_Count_Func := To_Gtk_Tree_Destroy_Count_Func (User_Data);
      Stub_Gtk_Tree_View : Gtk_Tree_View_Record;
   begin
      Func (Gtk.Tree_View.Gtk_Tree_View (Get_User_Data (Tree_View, Stub_Gtk_Tree_View)), From_Object (Path), Children);
   end Internal_Gtk_Tree_Destroy_Count_Func;

   ---------------------------------------------
   -- Internal_Gtk_Tree_View_Column_Drop_Func --
   ---------------------------------------------

   function Internal_Gtk_Tree_View_Column_Drop_Func
      (Tree_View   : System.Address;
       Column      : System.Address;
       Prev_Column : System.Address;
       Next_Column : System.Address;
       Data        : System.Address) return Glib.Gboolean
   is
      Func                      : constant Gtk_Tree_View_Column_Drop_Func := To_Gtk_Tree_View_Column_Drop_Func (Data);
      Stub_Gtk_Tree_View        : Gtk_Tree_View_Record;
      Stub_Gtk_Tree_View_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;
   begin
      return Boolean'Pos (Func (Gtk.Tree_View.Gtk_Tree_View (Get_User_Data (Tree_View, Stub_Gtk_Tree_View)), Gtk.Tree_View_Column.Gtk_Tree_View_Column (Get_User_Data (Column, Stub_Gtk_Tree_View_Column)), Gtk.Tree_View_Column.Gtk_Tree_View_Column (Get_User_Data (Prev_Column, Stub_Gtk_Tree_View_Column)), Gtk.Tree_View_Column.Gtk_Tree_View_Column (Get_User_Data (Next_Column, Stub_Gtk_Tree_View_Column))));
   end Internal_Gtk_Tree_View_Column_Drop_Func;

   -----------------------------------------
   -- Internal_Gtk_Tree_View_Mapping_Func --
   -----------------------------------------

   procedure Internal_Gtk_Tree_View_Mapping_Func
      (Tree_View : System.Address;
       Path      : System.Address;
       User_Data : System.Address)
   is
      Func               : constant Gtk_Tree_View_Mapping_Func := To_Gtk_Tree_View_Mapping_Func (User_Data);
      Stub_Gtk_Tree_View : Gtk_Tree_View_Record;
   begin
      Func (Gtk.Tree_View.Gtk_Tree_View (Get_User_Data (Tree_View, Stub_Gtk_Tree_View)), From_Object (Path));
   end Internal_Gtk_Tree_View_Mapping_Func;

   -----------------------------------------------
   -- Internal_Gtk_Tree_View_Row_Separator_Func --
   -----------------------------------------------

   function Internal_Gtk_Tree_View_Row_Separator_Func
      (Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data  : System.Address) return Glib.Gboolean
   is
      Func : constant Gtk_Tree_View_Row_Separator_Func := To_Gtk_Tree_View_Row_Separator_Func (Data);
   begin
      return Boolean'Pos (Func (Model, Iter.all));
   end Internal_Gtk_Tree_View_Row_Separator_Func;

   ----------------------------------------------
   -- Internal_Gtk_Tree_View_Search_Equal_Func --
   ----------------------------------------------

   function Internal_Gtk_Tree_View_Search_Equal_Func
      (Model       : Gtk.Tree_Model.Gtk_Tree_Model;
       Column      : Glib.Gint;
       Key         : Gtkada.Types.Chars_Ptr;
       Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Search_Data : System.Address) return Glib.Gboolean
   is
      Func : constant Gtk_Tree_View_Search_Equal_Func := To_Gtk_Tree_View_Search_Equal_Func (Search_Data);
   begin
      return Boolean'Pos (Func (Model, Column, Gtkada.Bindings.Value_Allowing_Null (Key), Iter.all));
   end Internal_Gtk_Tree_View_Search_Equal_Func;

   -------------------------------------------------
   -- Internal_Gtk_Tree_View_Search_Position_Func --
   -------------------------------------------------

   procedure Internal_Gtk_Tree_View_Search_Position_Func
      (Tree_View     : System.Address;
       Search_Dialog : System.Address;
       User_Data     : System.Address)
   is
      Func               : constant Gtk_Tree_View_Search_Position_Func := To_Gtk_Tree_View_Search_Position_Func (User_Data);
      Stub_Gtk_Tree_View : Gtk_Tree_View_Record;
      Stub_Gtk_Widget    : Gtk.Widget.Gtk_Widget_Record;
   begin
      Func (Gtk.Tree_View.Gtk_Tree_View (Get_User_Data (Tree_View, Stub_Gtk_Tree_View)), Gtk.Widget.Gtk_Widget (Get_User_Data (Search_Dialog, Stub_Gtk_Widget)));
   end Internal_Gtk_Tree_View_Search_Position_Func;

   package Type_Conversion_Gtk_Tree_View is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_View_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Tree_View);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Tree_View : out Gtk_Tree_View) is
   begin
      Tree_View := new Gtk_Tree_View_Record;
      Gtk.Tree_View.Initialize (Tree_View);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Tree_View : out Gtk_Tree_View;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model)
   is
   begin
      Tree_View := new Gtk_Tree_View_Record;
      Gtk.Tree_View.Initialize (Tree_View, Model);
   end Gtk_New;

   -----------------------
   -- Gtk_Tree_View_New --
   -----------------------

   function Gtk_Tree_View_New return Gtk_Tree_View is
      Tree_View : constant Gtk_Tree_View := new Gtk_Tree_View_Record;
   begin
      Gtk.Tree_View.Initialize (Tree_View);
      return Tree_View;
   end Gtk_Tree_View_New;

   ----------------------------------
   -- Gtk_Tree_View_New_With_Model --
   ----------------------------------

   function Gtk_Tree_View_New_With_Model
      (Model : Gtk.Tree_Model.Gtk_Tree_Model) return Gtk_Tree_View
   is
      Tree_View : constant Gtk_Tree_View := new Gtk_Tree_View_Record;
   begin
      Gtk.Tree_View.Initialize (Tree_View, Model);
      return Tree_View;
   end Gtk_Tree_View_New_With_Model;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Tree_View : not null access Gtk_Tree_View_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_new");
   begin
      if not Tree_View.Is_Created then
         Set_Object (Tree_View, Internal);
      end if;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Tree_View : not null access Gtk_Tree_View_Record'Class;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model)
   is
      function Internal
         (Model : Gtk.Tree_Model.Gtk_Tree_Model) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_new_with_model");
   begin
      if not Tree_View.Is_Created then
         Set_Object (Tree_View, Internal (Model));
      end if;
   end Initialize;

   -------------------
   -- Append_Column --
   -------------------

   function Append_Column
      (Tree_View : not null access Gtk_Tree_View_Record;
       Column    : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class)
       return Glib.Gint
   is
      function Internal
         (Tree_View : System.Address;
          Column    : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tree_view_append_column");
   begin
      return Internal (Get_Object (Tree_View), Get_Object (Column));
   end Append_Column;

   ------------------
   -- Collapse_All --
   ------------------

   procedure Collapse_All (Tree_View : not null access Gtk_Tree_View_Record) is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_collapse_all");
   begin
      Internal (Get_Object (Tree_View));
   end Collapse_All;

   ------------------
   -- Collapse_Row --
   ------------------

   function Collapse_Row
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      function Internal
         (Tree_View : System.Address;
          Path      : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_collapse_row");
   begin
      return Internal (Get_Object (Tree_View), Get_Object (Path)) /= 0;
   end Collapse_Row;

   ----------------------
   -- Columns_Autosize --
   ----------------------

   procedure Columns_Autosize
      (Tree_View : not null access Gtk_Tree_View_Record)
   is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_columns_autosize");
   begin
      Internal (Get_Object (Tree_View));
   end Columns_Autosize;

   ---------------------------------------
   -- Convert_Bin_Window_To_Tree_Coords --
   ---------------------------------------

   procedure Convert_Bin_Window_To_Tree_Coords
      (Tree_View : not null access Gtk_Tree_View_Record;
       Bx        : Glib.Gint;
       By        : Glib.Gint;
       Tx        : out Glib.Gint;
       Ty        : out Glib.Gint)
   is
      procedure Internal
         (Tree_View : System.Address;
          Bx        : Glib.Gint;
          By        : Glib.Gint;
          Tx        : out Glib.Gint;
          Ty        : out Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_convert_bin_window_to_tree_coords");
   begin
      Internal (Get_Object (Tree_View), Bx, By, Tx, Ty);
   end Convert_Bin_Window_To_Tree_Coords;

   -----------------------------------------
   -- Convert_Bin_Window_To_Widget_Coords --
   -----------------------------------------

   procedure Convert_Bin_Window_To_Widget_Coords
      (Tree_View : not null access Gtk_Tree_View_Record;
       Bx        : Glib.Gint;
       By        : Glib.Gint;
       Wx        : out Glib.Gint;
       Wy        : out Glib.Gint)
   is
      procedure Internal
         (Tree_View : System.Address;
          Bx        : Glib.Gint;
          By        : Glib.Gint;
          Wx        : out Glib.Gint;
          Wy        : out Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_convert_bin_window_to_widget_coords");
   begin
      Internal (Get_Object (Tree_View), Bx, By, Wx, Wy);
   end Convert_Bin_Window_To_Widget_Coords;

   ---------------------------------------
   -- Convert_Tree_To_Bin_Window_Coords --
   ---------------------------------------

   procedure Convert_Tree_To_Bin_Window_Coords
      (Tree_View : not null access Gtk_Tree_View_Record;
       Tx        : Glib.Gint;
       Ty        : Glib.Gint;
       Bx        : out Glib.Gint;
       By        : out Glib.Gint)
   is
      procedure Internal
         (Tree_View : System.Address;
          Tx        : Glib.Gint;
          Ty        : Glib.Gint;
          Bx        : out Glib.Gint;
          By        : out Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_convert_tree_to_bin_window_coords");
   begin
      Internal (Get_Object (Tree_View), Tx, Ty, Bx, By);
   end Convert_Tree_To_Bin_Window_Coords;

   -----------------------------------
   -- Convert_Tree_To_Widget_Coords --
   -----------------------------------

   procedure Convert_Tree_To_Widget_Coords
      (Tree_View : not null access Gtk_Tree_View_Record;
       Tx        : Glib.Gint;
       Ty        : Glib.Gint;
       Wx        : out Glib.Gint;
       Wy        : out Glib.Gint)
   is
      procedure Internal
         (Tree_View : System.Address;
          Tx        : Glib.Gint;
          Ty        : Glib.Gint;
          Wx        : out Glib.Gint;
          Wy        : out Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_convert_tree_to_widget_coords");
   begin
      Internal (Get_Object (Tree_View), Tx, Ty, Wx, Wy);
   end Convert_Tree_To_Widget_Coords;

   -----------------------------------------
   -- Convert_Widget_To_Bin_Window_Coords --
   -----------------------------------------

   procedure Convert_Widget_To_Bin_Window_Coords
      (Tree_View : not null access Gtk_Tree_View_Record;
       Wx        : Glib.Gint;
       Wy        : Glib.Gint;
       Bx        : out Glib.Gint;
       By        : out Glib.Gint)
   is
      procedure Internal
         (Tree_View : System.Address;
          Wx        : Glib.Gint;
          Wy        : Glib.Gint;
          Bx        : out Glib.Gint;
          By        : out Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_convert_widget_to_bin_window_coords");
   begin
      Internal (Get_Object (Tree_View), Wx, Wy, Bx, By);
   end Convert_Widget_To_Bin_Window_Coords;

   -----------------------------------
   -- Convert_Widget_To_Tree_Coords --
   -----------------------------------

   procedure Convert_Widget_To_Tree_Coords
      (Tree_View : not null access Gtk_Tree_View_Record;
       Wx        : Glib.Gint;
       Wy        : Glib.Gint;
       Tx        : out Glib.Gint;
       Ty        : out Glib.Gint)
   is
      procedure Internal
         (Tree_View : System.Address;
          Wx        : Glib.Gint;
          Wy        : Glib.Gint;
          Tx        : out Glib.Gint;
          Ty        : out Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_convert_widget_to_tree_coords");
   begin
      Internal (Get_Object (Tree_View), Wx, Wy, Tx, Ty);
   end Convert_Widget_To_Tree_Coords;

   --------------------------
   -- Create_Row_Drag_Icon --
   --------------------------

   function Create_Row_Drag_Icon
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Cairo.Cairo_Surface
   is
      function Internal
         (Tree_View : System.Address;
          Path      : System.Address) return Cairo.Cairo_Surface;
      pragma Import (C, Internal, "gtk_tree_view_create_row_drag_icon");
   begin
      return Internal (Get_Object (Tree_View), Get_Object (Path));
   end Create_Row_Drag_Icon;

   ----------------------------
   -- Enable_Model_Drag_Dest --
   ----------------------------

   procedure Enable_Model_Drag_Dest
      (Tree_View : not null access Gtk_Tree_View_Record;
       Targets   : Gtk.Target_List.Target_Entry_Array;
       Actions   : Gdk.Drag_Contexts.Gdk_Drag_Action)
   is
      procedure Internal
         (Tree_View : System.Address;
          Targets   : Gtk.Target_List.Target_Entry_Array;
          N_Targets : Glib.Gint;
          Actions   : Gdk.Drag_Contexts.Gdk_Drag_Action);
      pragma Import (C, Internal, "gtk_tree_view_enable_model_drag_dest");
   begin
      Internal (Get_Object (Tree_View), Targets, Targets'Length, Actions);
   end Enable_Model_Drag_Dest;

   ------------------------------
   -- Enable_Model_Drag_Source --
   ------------------------------

   procedure Enable_Model_Drag_Source
      (Tree_View         : not null access Gtk_Tree_View_Record;
       Start_Button_Mask : Gdk.Types.Gdk_Modifier_Type;
       Targets           : Gtk.Target_List.Target_Entry_Array;
       Actions           : Gdk.Drag_Contexts.Gdk_Drag_Action)
   is
      procedure Internal
         (Tree_View         : System.Address;
          Start_Button_Mask : Gdk.Types.Gdk_Modifier_Type;
          Targets           : Gtk.Target_List.Target_Entry_Array;
          N_Targets         : Glib.Gint;
          Actions           : Gdk.Drag_Contexts.Gdk_Drag_Action);
      pragma Import (C, Internal, "gtk_tree_view_enable_model_drag_source");
   begin
      Internal (Get_Object (Tree_View), Start_Button_Mask, Targets, Targets'Length, Actions);
   end Enable_Model_Drag_Source;

   ----------------
   -- Expand_All --
   ----------------

   procedure Expand_All (Tree_View : not null access Gtk_Tree_View_Record) is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_expand_all");
   begin
      Internal (Get_Object (Tree_View));
   end Expand_All;

   ----------------
   -- Expand_Row --
   ----------------

   function Expand_Row
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Open_All  : Boolean) return Boolean
   is
      function Internal
         (Tree_View : System.Address;
          Path      : System.Address;
          Open_All  : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_expand_row");
   begin
      return Internal (Get_Object (Tree_View), Get_Object (Path), Boolean'Pos (Open_All)) /= 0;
   end Expand_Row;

   --------------------
   -- Expand_To_Path --
   --------------------

   procedure Expand_To_Path
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal (Tree_View : System.Address; Path : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_expand_to_path");
   begin
      Internal (Get_Object (Tree_View), Get_Object (Path));
   end Expand_To_Path;

   ----------------------------------
   -- Get_Activate_On_Single_Click --
   ----------------------------------

   function Get_Activate_On_Single_Click
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_activate_on_single_click");
   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Activate_On_Single_Click;

   -------------------------
   -- Get_Background_Area --
   -------------------------

   procedure Get_Background_Area
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Rect      : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (Tree_View : System.Address;
          Path      : System.Address;
          Column    : System.Address;
          Rect      : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_tree_view_get_background_area");
   begin
      Internal (Get_Object (Tree_View), Get_Object (Path), Get_Object_Or_Null (GObject (Column)), Rect);
   end Get_Background_Area;

   --------------------
   -- Get_Bin_Window --
   --------------------

   function Get_Bin_Window
      (Tree_View : not null access Gtk_Tree_View_Record)
       return Gdk.Gdk_Window
   is
      function Internal (Tree_View : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gtk_tree_view_get_bin_window");
   begin
      return Internal (Get_Object (Tree_View));
   end Get_Bin_Window;

   -------------------
   -- Get_Cell_Area --
   -------------------

   procedure Get_Cell_Area
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Rect      : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (Tree_View : System.Address;
          Path      : System.Address;
          Column    : System.Address;
          Rect      : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_tree_view_get_cell_area");
   begin
      Internal (Get_Object (Tree_View), Get_Object (Path), Get_Object_Or_Null (GObject (Column)), Rect);
   end Get_Cell_Area;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
      (Tree_View : not null access Gtk_Tree_View_Record;
       N         : Glib.Gint)
       return Gtk.Tree_View_Column.Gtk_Tree_View_Column
   is
      function Internal
         (Tree_View : System.Address;
          N         : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_column");
      Stub_Gtk_Tree_View_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;
   begin
      return Gtk.Tree_View_Column.Gtk_Tree_View_Column (Get_User_Data (Internal (Get_Object (Tree_View), N), Stub_Gtk_Tree_View_Column));
   end Get_Column;

   -----------------
   -- Get_Columns --
   -----------------

   function Get_Columns
      (Tree_View : not null access Gtk_Tree_View_Record)
       return Gtk.Tree_View_Column.Column_List.Glist
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_columns");
      Tmp_Return : Gtk.Tree_View_Column.Column_List.Glist;
   begin
      Gtk.Tree_View_Column.Column_List.Set_Object (Tmp_Return, Internal (Get_Object (Tree_View)));
      return Tmp_Return;
   end Get_Columns;

   ----------------
   -- Get_Cursor --
   ----------------

   procedure Get_Cursor
      (Tree_View    : not null access Gtk_Tree_View_Record;
       Path         : out Gtk.Tree_Model.Gtk_Tree_Path;
       Focus_Column : out Gtk.Tree_View_Column.Gtk_Tree_View_Column)
   is
      procedure Internal
         (Tree_View    : System.Address;
          Path         : out System.Address;
          Focus_Column : out System.Address);
      pragma Import (C, Internal, "gtk_tree_view_get_cursor");
      Tmp_Path                  : aliased System.Address;
      Tmp_Focus_Column          : aliased System.Address;
      Stub_Gtk_Tree_View_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;
   begin
      Internal (Get_Object (Tree_View), Tmp_Path, Tmp_Focus_Column);
      Focus_Column := Gtk.Tree_View_Column.Gtk_Tree_View_Column (Get_User_Data (Tmp_Focus_Column, Stub_Gtk_Tree_View_Column));
      Path := From_Object (Tmp_Path);
   end Get_Cursor;

   -------------------------
   -- Get_Dest_Row_At_Pos --
   -------------------------

   function Get_Dest_Row_At_Pos
      (Tree_View : not null access Gtk_Tree_View_Record;
       Drag_X    : Glib.Gint;
       Drag_Y    : Glib.Gint;
       Path      : access Gtk.Tree_Model.Gtk_Tree_Path;
       Pos       : access Gtk_Tree_View_Drop_Position) return Boolean
   is
      function Internal
         (Tree_View : System.Address;
          Drag_X    : Glib.Gint;
          Drag_Y    : Glib.Gint;
          Acc_Path  : access System.Address;
          Acc_Pos   : access Gtk_Tree_View_Drop_Position)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_dest_row_at_pos");
      Acc_Path     : aliased Gtk.Tree_Model.Gtk_Tree_Path;
      Acc_Pos      : aliased Gtk_Tree_View_Drop_Position;
      Tmp_Acc_Path : aliased System.Address;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Tree_View), Drag_X, Drag_Y, Tmp_Acc_Path'Access, Acc_Pos'Access);
      Acc_Path := From_Object (Tmp_Acc_Path);
      if Path /= null then
         Path.all := Acc_Path;
      end if;
      if Pos /= null then
         Pos.all := Acc_Pos;
      end if;
      return Tmp_Return /= 0;
   end Get_Dest_Row_At_Pos;

   -----------------------
   -- Get_Drag_Dest_Row --
   -----------------------

   procedure Get_Drag_Dest_Row
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
       Pos       : out Gtk_Tree_View_Drop_Position)
   is
      procedure Internal
         (Tree_View : System.Address;
          Path      : out System.Address;
          Pos       : out Gtk_Tree_View_Drop_Position);
      pragma Import (C, Internal, "gtk_tree_view_get_drag_dest_row");
      Tmp_Path : aliased System.Address;
   begin
      Internal (Get_Object (Tree_View), Tmp_Path, Pos);
      Path := From_Object (Tmp_Path);
   end Get_Drag_Dest_Row;

   -----------------------
   -- Get_Enable_Search --
   -----------------------

   function Get_Enable_Search
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_enable_search");
   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Enable_Search;

   ---------------------------
   -- Get_Enable_Tree_Lines --
   ---------------------------

   function Get_Enable_Tree_Lines
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_enable_tree_lines");
   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Enable_Tree_Lines;

   -------------------------
   -- Get_Expander_Column --
   -------------------------

   function Get_Expander_Column
      (Tree_View : not null access Gtk_Tree_View_Record)
       return Gtk.Tree_View_Column.Gtk_Tree_View_Column
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_expander_column");
      Stub_Gtk_Tree_View_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;
   begin
      return Gtk.Tree_View_Column.Gtk_Tree_View_Column (Get_User_Data (Internal (Get_Object (Tree_View)), Stub_Gtk_Tree_View_Column));
   end Get_Expander_Column;

   ---------------------------
   -- Get_Fixed_Height_Mode --
   ---------------------------

   function Get_Fixed_Height_Mode
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_fixed_height_mode");
   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Fixed_Height_Mode;

   --------------------
   -- Get_Grid_Lines --
   --------------------

   function Get_Grid_Lines
      (Tree_View : not null access Gtk_Tree_View_Record)
       return Gtk.Enums.Gtk_Tree_View_Grid_Lines
   is
      function Internal
         (Tree_View : System.Address)
          return Gtk.Enums.Gtk_Tree_View_Grid_Lines;
      pragma Import (C, Internal, "gtk_tree_view_get_grid_lines");
   begin
      return Internal (Get_Object (Tree_View));
   end Get_Grid_Lines;

   ---------------------------
   -- Get_Headers_Clickable --
   ---------------------------

   function Get_Headers_Clickable
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_headers_clickable");
   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Headers_Clickable;

   -------------------------
   -- Get_Headers_Visible --
   -------------------------

   function Get_Headers_Visible
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_headers_visible");
   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Headers_Visible;

   ----------------------
   -- Get_Hover_Expand --
   ----------------------

   function Get_Hover_Expand
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_hover_expand");
   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Hover_Expand;

   -------------------------
   -- Get_Hover_Selection --
   -------------------------

   function Get_Hover_Selection
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_hover_selection");
   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Hover_Selection;

   ---------------------------
   -- Get_Level_Indentation --
   ---------------------------

   function Get_Level_Indentation
      (Tree_View : not null access Gtk_Tree_View_Record) return Glib.Gint
   is
      function Internal (Tree_View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tree_view_get_level_indentation");
   begin
      return Internal (Get_Object (Tree_View));
   end Get_Level_Indentation;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Tree_View : not null access Gtk_Tree_View_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model
   is
      function Internal
         (Tree_View : System.Address) return Gtk.Tree_Model.Gtk_Tree_Model;
      pragma Import (C, Internal, "gtk_tree_view_get_model");
   begin
      return Internal (Get_Object (Tree_View));
   end Get_Model;

   -------------------
   -- Get_N_Columns --
   -------------------

   function Get_N_Columns
      (Tree_View : not null access Gtk_Tree_View_Record) return Guint
   is
      function Internal (Tree_View : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_tree_view_get_n_columns");
   begin
      return Internal (Get_Object (Tree_View));
   end Get_N_Columns;

   ---------------------
   -- Get_Path_At_Pos --
   ---------------------

   procedure Get_Path_At_Pos
      (Tree_View : not null access Gtk_Tree_View_Record;
       X         : Glib.Gint;
       Y         : Glib.Gint;
       Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : out Gtk.Tree_View_Column.Gtk_Tree_View_Column;
       Cell_X    : out Glib.Gint;
       Cell_Y    : out Glib.Gint;
       Row_Found : out Boolean)
   is
      function Internal
         (Tree_View  : System.Address;
          X          : Glib.Gint;
          Y          : Glib.Gint;
          Acc_Path   : access System.Address;
          Acc_Column : access System.Address;
          Acc_Cell_X : access Glib.Gint;
          Acc_Cell_Y : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_path_at_pos");
      Acc_Path                  : aliased Gtk.Tree_Model.Gtk_Tree_Path;
      Acc_Column                : aliased Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Acc_Cell_X                : aliased Glib.Gint;
      Acc_Cell_Y                : aliased Glib.Gint;
      Tmp_Acc_Path              : aliased System.Address;
      Tmp_Acc_Column            : aliased System.Address;
      Stub_Gtk_Tree_View_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;
      Tmp_Return                : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Tree_View), X, Y, Tmp_Acc_Path'Access, Tmp_Acc_Column'Access, Acc_Cell_X'Access, Acc_Cell_Y'Access);
      Acc_Column := Gtk.Tree_View_Column.Gtk_Tree_View_Column (Get_User_Data (Tmp_Acc_Column, Stub_Gtk_Tree_View_Column));
      Acc_Path := From_Object (Tmp_Acc_Path);
      Path := Acc_Path;
      Column := Acc_Column;
      Cell_X := Acc_Cell_X;
      Cell_Y := Acc_Cell_Y;
      Row_Found := Tmp_Return /= 0;
   end Get_Path_At_Pos;

   ---------------------
   -- Get_Reorderable --
   ---------------------

   function Get_Reorderable
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_reorderable");
   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Reorderable;

   ----------------------------
   -- Get_Row_Separator_Func --
   ----------------------------

   procedure Get_Row_Separator_Func
      (Tree_View : not null access Gtk_Tree_View_Record)
   is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_get_row_separator_func");
   begin
      Internal (Get_Object (Tree_View));
   end Get_Row_Separator_Func;

   ------------------------
   -- Get_Rubber_Banding --
   ------------------------

   function Get_Rubber_Banding
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_rubber_banding");
   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Rubber_Banding;

   --------------------
   -- Get_Rules_Hint --
   --------------------

   function Get_Rules_Hint
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_rules_hint");
   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Rules_Hint;

   -----------------------
   -- Get_Search_Column --
   -----------------------

   function Get_Search_Column
      (Tree_View : not null access Gtk_Tree_View_Record) return Glib.Gint
   is
      function Internal (Tree_View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tree_view_get_search_column");
   begin
      return Internal (Get_Object (Tree_View));
   end Get_Search_Column;

   ----------------------
   -- Get_Search_Entry --
   ----------------------

   function Get_Search_Entry
      (Tree_View : not null access Gtk_Tree_View_Record)
       return Gtk.GEntry.Gtk_Entry
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_search_entry");
      Stub_Gtk_Entry : Gtk.GEntry.Gtk_Entry_Record;
   begin
      return Gtk.GEntry.Gtk_Entry (Get_User_Data (Internal (Get_Object (Tree_View)), Stub_Gtk_Entry));
   end Get_Search_Entry;

   ---------------------------
   -- Get_Search_Equal_Func --
   ---------------------------

   procedure Get_Search_Equal_Func
      (Tree_View : not null access Gtk_Tree_View_Record)
   is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_get_search_equal_func");
   begin
      Internal (Get_Object (Tree_View));
   end Get_Search_Equal_Func;

   ------------------------------
   -- Get_Search_Position_Func --
   ------------------------------

   procedure Get_Search_Position_Func
      (Tree_View : not null access Gtk_Tree_View_Record)
   is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_get_search_position_func");
   begin
      Internal (Get_Object (Tree_View));
   end Get_Search_Position_Func;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
      (Tree_View : not null access Gtk_Tree_View_Record)
       return Gtk.Tree_Selection.Gtk_Tree_Selection
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_selection");
      Stub_Gtk_Tree_Selection : Gtk.Tree_Selection.Gtk_Tree_Selection_Record;
   begin
      return Gtk.Tree_Selection.Gtk_Tree_Selection (Get_User_Data (Internal (Get_Object (Tree_View)), Stub_Gtk_Tree_Selection));
   end Get_Selection;

   ------------------------
   -- Get_Show_Expanders --
   ------------------------

   function Get_Show_Expanders
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_show_expanders");
   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Show_Expanders;

   ------------------------
   -- Get_Tooltip_Column --
   ------------------------

   function Get_Tooltip_Column
      (Tree_View : not null access Gtk_Tree_View_Record) return Glib.Gint
   is
      function Internal (Tree_View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tree_view_get_tooltip_column");
   begin
      return Internal (Get_Object (Tree_View));
   end Get_Tooltip_Column;

   -------------------------
   -- Get_Tooltip_Context --
   -------------------------

   procedure Get_Tooltip_Context
      (Tree_View    : not null access Gtk_Tree_View_Record;
       X            : in out Glib.Gint;
       Y            : in out Glib.Gint;
       Keyboard_Tip : Boolean;
       Model        : out Gtk.Tree_Model.Gtk_Tree_Model;
       Path         : out Gtk.Tree_Model.Gtk_Tree_Path;
       Iter         : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Success      : out Boolean)
   is
      function Internal
         (Tree_View    : System.Address;
          Acc_X        : access Glib.Gint;
          Acc_Y        : access Glib.Gint;
          Keyboard_Tip : Glib.Gboolean;
          Acc_Model    : access Gtk.Tree_Model.Gtk_Tree_Model;
          Acc_Path     : access System.Address;
          Acc_Iter     : access Gtk.Tree_Model.Gtk_Tree_Iter)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_tooltip_context");
      Acc_X        : aliased Glib.Gint := X;
      Acc_Y        : aliased Glib.Gint := Y;
      Acc_Model    : aliased Gtk.Tree_Model.Gtk_Tree_Model;
      Acc_Path     : aliased Gtk.Tree_Model.Gtk_Tree_Path;
      Acc_Iter     : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
      Tmp_Acc_Path : aliased System.Address;
      Tmp_Acc_Iter : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Tree_View), Acc_X'Access, Acc_Y'Access, Boolean'Pos (Keyboard_Tip), Acc_Model'Access, Tmp_Acc_Path'Access, Tmp_Acc_Iter'Access);
      Acc_Iter := Tmp_Acc_Iter;
      Acc_Path := From_Object (Tmp_Acc_Path);
      X := Acc_X;
      Y := Acc_Y;
      Model := Acc_Model;
      Path := Acc_Path;
      Iter := Acc_Iter;
      Success := Tmp_Return /= 0;
   end Get_Tooltip_Context;

   -----------------------
   -- Get_Visible_Range --
   -----------------------

   procedure Get_Visible_Range
      (Tree_View  : not null access Gtk_Tree_View_Record;
       Start_Path : out Gtk.Tree_Model.Gtk_Tree_Path;
       End_Path   : out Gtk.Tree_Model.Gtk_Tree_Path;
       Success    : out Boolean)
   is
      function Internal
         (Tree_View      : System.Address;
          Acc_Start_Path : access System.Address;
          Acc_End_Path   : access System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_visible_range");
      Acc_Start_Path     : aliased Gtk.Tree_Model.Gtk_Tree_Path;
      Acc_End_Path       : aliased Gtk.Tree_Model.Gtk_Tree_Path;
      Tmp_Acc_Start_Path : aliased System.Address;
      Tmp_Acc_End_Path   : aliased System.Address;
      Tmp_Return         : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Tree_View), Tmp_Acc_Start_Path'Access, Tmp_Acc_End_Path'Access);
      Acc_End_Path := From_Object (Tmp_Acc_End_Path);
      Acc_Start_Path := From_Object (Tmp_Acc_Start_Path);
      Start_Path := Acc_Start_Path;
      End_Path := Acc_End_Path;
      Success := Tmp_Return /= 0;
   end Get_Visible_Range;

   ----------------------
   -- Get_Visible_Rect --
   ----------------------

   procedure Get_Visible_Rect
      (Tree_View    : not null access Gtk_Tree_View_Record;
       Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (Tree_View    : System.Address;
          Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_tree_view_get_visible_rect");
   begin
      Internal (Get_Object (Tree_View), Visible_Rect);
   end Get_Visible_Rect;

   -------------------
   -- Insert_Column --
   -------------------

   function Insert_Column
      (Tree_View : not null access Gtk_Tree_View_Record;
       Column    : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Position  : Glib.Gint := -1) return Glib.Gint
   is
      function Internal
         (Tree_View : System.Address;
          Column    : System.Address;
          Position  : Glib.Gint) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tree_view_insert_column");
   begin
      return Internal (Get_Object (Tree_View), Get_Object (Column), Position);
   end Insert_Column;

   ----------------------------------
   -- Insert_Column_With_Data_Func --
   ----------------------------------

   function Insert_Column_With_Data_Func
      (Tree_View : not null access Gtk_Tree_View_Record;
       Position  : Glib.Gint;
       Title     : UTF8_String;
       Cell      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func      : Gtk_Tree_Cell_Data_Func;
       Dnotify   : Glib.G_Destroy_Notify_Address) return Glib.Gint
   is
      Tmp_Title  : Gtkada.Types.Chars_Ptr := New_String (Title);
      Tmp_Return : Glib.Gint;
   begin
      if Func = null then
         Tmp_Return := C_Gtk_Tree_View_Insert_Column_With_Data_Func (Get_Object (Tree_View), Position, Tmp_Title, Get_Object (Cell), System.Null_Address, System.Null_Address, Dnotify);
         Free (Tmp_Title);
         return Tmp_Return;
      else
         Tmp_Return := C_Gtk_Tree_View_Insert_Column_With_Data_Func (Get_Object (Tree_View), Position, Tmp_Title, Get_Object (Cell), Internal_Gtk_Tree_Cell_Data_Func'Address, To_Address (Func), Dnotify);
         Free (Tmp_Title);
         return Tmp_Return;
      end if;
   end Insert_Column_With_Data_Func;

   package body Insert_Column_With_Data_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tree_Cell_Data_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tree_Cell_Data_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_Cell_Data_Func, System.Address);

      procedure Internal_Cb
         (Tree_Column : System.Address;
          Cell        : System.Address;
          Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
          Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Data        : System.Address);
      pragma Convention (C, Internal_Cb);
      --  A function to set the properties of a cell instead of just using the
      --  straight mapping between the cell and the model. This is useful for
      --  customizing the cell renderer. For example, a function might get an
      --  integer from the Tree_Model, and render it to the "text" attribute of
      --  "cell" by converting it to its written equivalent. This is set by
      --  calling gtk_tree_view_column_set_cell_data_func
      --  "tree_column": A Gtk.Tree_View_Column.Gtk_Tree_View_Column
      --  "cell": The Gtk.Cell_Renderer.Gtk_Cell_Renderer that is being
      --  rendered by Tree_Column
      --  "tree_model": The Gtk.Tree_Model.Gtk_Tree_Model being rendered
      --  "iter": A Gtk.Tree_Model.Gtk_Tree_Iter of the current row rendered
      --  "data": user data

      ----------------------------------
      -- Insert_Column_With_Data_Func --
      ----------------------------------

      function Insert_Column_With_Data_Func
         (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Position  : Glib.Gint;
          Title     : UTF8_String;
          Cell      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func      : Gtk_Tree_Cell_Data_Func;
          Data      : User_Data_Type;
          Dnotify   : Glib.G_Destroy_Notify_Address) return Glib.Gint
      is
         Tmp_Title  : Gtkada.Types.Chars_Ptr := New_String (Title);
         Tmp_Return : Glib.Gint;
         D          : System.Address;
      begin
         if Func = null then
            Tmp_Return := C_Gtk_Tree_View_Insert_Column_With_Data_Func (Get_Object (Tree_View), Position, Tmp_Title, Get_Object (Cell), System.Null_Address, System.Null_Address, Dnotify);
            Free (Tmp_Title);
            return Tmp_Return;
         else
            D := Users.Build (To_Address (Func), Data);
            Tmp_Return := C_Gtk_Tree_View_Insert_Column_With_Data_Func (Get_Object (Tree_View), Position, Tmp_Title, Get_Object (Cell), Internal_Cb'Address, D, Dnotify);
            Free (Tmp_Title);
            return Tmp_Return;
         end if;
      end Insert_Column_With_Data_Func;

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Tree_Column : System.Address;
          Cell        : System.Address;
          Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
          Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Data        : System.Address)
      is
         D                         : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Gtk_Tree_View_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;
         Stub_Gtk_Cell_Renderer    : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
      begin
         To_Gtk_Tree_Cell_Data_Func (D.Func) (Gtk.Tree_View_Column.Gtk_Tree_View_Column (Get_User_Data (Tree_Column, Stub_Gtk_Tree_View_Column)), Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Cell, Stub_Gtk_Cell_Renderer)), Tree_Model, Iter.all, D.Data.all);
      end Internal_Cb;

   end Insert_Column_With_Data_Func_User_Data;

   ---------------------
   -- Is_Blank_At_Pos --
   ---------------------

   function Is_Blank_At_Pos
      (Tree_View : not null access Gtk_Tree_View_Record;
       X         : Glib.Gint;
       Y         : Glib.Gint;
       Path      : access Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : access Gtk.Tree_View_Column.Gtk_Tree_View_Column;
       Cell_X    : access Glib.Gint;
       Cell_Y    : access Glib.Gint) return Boolean
   is
      function Internal
         (Tree_View  : System.Address;
          X          : Glib.Gint;
          Y          : Glib.Gint;
          Acc_Path   : access System.Address;
          Acc_Column : access System.Address;
          Acc_Cell_X : access Glib.Gint;
          Acc_Cell_Y : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_is_blank_at_pos");
      Acc_Path                  : aliased Gtk.Tree_Model.Gtk_Tree_Path;
      Acc_Column                : aliased Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Acc_Cell_X                : aliased Glib.Gint;
      Acc_Cell_Y                : aliased Glib.Gint;
      Tmp_Acc_Path              : aliased System.Address;
      Tmp_Acc_Column            : aliased System.Address;
      Stub_Gtk_Tree_View_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;
      Tmp_Return                : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Tree_View), X, Y, Tmp_Acc_Path'Access, Tmp_Acc_Column'Access, Acc_Cell_X'Access, Acc_Cell_Y'Access);
      Acc_Column := Gtk.Tree_View_Column.Gtk_Tree_View_Column (Get_User_Data (Tmp_Acc_Column, Stub_Gtk_Tree_View_Column));
      Acc_Path := From_Object (Tmp_Acc_Path);
      if Path /= null then
         Path.all := Acc_Path;
      end if;
      if Column /= null then
         Column.all := Acc_Column;
      end if;
      if Cell_X /= null then
         Cell_X.all := Acc_Cell_X;
      end if;
      if Cell_Y /= null then
         Cell_Y.all := Acc_Cell_Y;
      end if;
      return Tmp_Return /= 0;
   end Is_Blank_At_Pos;

   ------------------------------
   -- Is_Rubber_Banding_Active --
   ------------------------------

   function Is_Rubber_Banding_Active
      (Tree_View : not null access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_is_rubber_banding_active");
   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Is_Rubber_Banding_Active;

   -----------------------
   -- Map_Expanded_Rows --
   -----------------------

   procedure Map_Expanded_Rows
      (Tree_View : not null access Gtk_Tree_View_Record;
       Func      : Gtk_Tree_View_Mapping_Func)
   is
   begin
      if Func = null then
         C_Gtk_Tree_View_Map_Expanded_Rows (Get_Object (Tree_View), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Tree_View_Map_Expanded_Rows (Get_Object (Tree_View), Internal_Gtk_Tree_View_Mapping_Func'Address, To_Address (Func));
      end if;
   end Map_Expanded_Rows;

   package body Map_Expanded_Rows_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tree_View_Mapping_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tree_View_Mapping_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_View_Mapping_Func, System.Address);

      procedure Internal_Cb
         (Tree_View : System.Address;
          Path      : System.Address;
          User_Data : System.Address);
      pragma Convention (C, Internal_Cb);
      --  Function used for Gtk.Tree_View.Map_Expanded_Rows.
      --  "tree_view": A Gtk.Tree_View.Gtk_Tree_View
      --  "path": The path that's expanded
      --  "user_data": user data

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Tree_View : System.Address;
          Path      : System.Address;
          User_Data : System.Address)
      is
         D                  : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_Tree_View : Gtk.Tree_View.Gtk_Tree_View_Record;
      begin
         To_Gtk_Tree_View_Mapping_Func (D.Func) (Gtk.Tree_View.Gtk_Tree_View (Get_User_Data (Tree_View, Stub_Gtk_Tree_View)), From_Object (Path), D.Data.all);
      end Internal_Cb;

      -----------------------
      -- Map_Expanded_Rows --
      -----------------------

      procedure Map_Expanded_Rows
         (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Func      : Gtk_Tree_View_Mapping_Func;
          Data      : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Tree_View_Map_Expanded_Rows (Get_Object (Tree_View), System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_Tree_View_Map_Expanded_Rows (Get_Object (Tree_View), Internal_Cb'Address, D);
            Users.Free_Data (D);
         end if;
      end Map_Expanded_Rows;

   end Map_Expanded_Rows_User_Data;

   -----------------------
   -- Move_Column_After --
   -----------------------

   procedure Move_Column_After
      (Tree_View   : not null access Gtk_Tree_View_Record;
       Column      : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Base_Column : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class)
   is
      procedure Internal
         (Tree_View   : System.Address;
          Column      : System.Address;
          Base_Column : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_move_column_after");
   begin
      Internal (Get_Object (Tree_View), Get_Object (Column), Get_Object_Or_Null (GObject (Base_Column)));
   end Move_Column_After;

   -------------------
   -- Remove_Column --
   -------------------

   function Remove_Column
      (Tree_View : not null access Gtk_Tree_View_Record;
       Column    : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class)
       return Glib.Gint
   is
      function Internal
         (Tree_View : System.Address;
          Column    : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tree_view_remove_column");
   begin
      return Internal (Get_Object (Tree_View), Get_Object (Column));
   end Remove_Column;

   -------------------
   -- Row_Activated --
   -------------------

   procedure Row_Activated
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class)
   is
      procedure Internal
         (Tree_View : System.Address;
          Path      : System.Address;
          Column    : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_row_activated");
   begin
      Internal (Get_Object (Tree_View), Get_Object (Path), Get_Object (Column));
   end Row_Activated;

   ------------------
   -- Row_Expanded --
   ------------------

   function Row_Expanded
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      function Internal
         (Tree_View : System.Address;
          Path      : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_row_expanded");
   begin
      return Internal (Get_Object (Tree_View), Get_Object (Path)) /= 0;
   end Row_Expanded;

   --------------------
   -- Scroll_To_Cell --
   --------------------

   procedure Scroll_To_Cell
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Use_Align : Boolean;
       Row_Align : Gfloat;
       Col_Align : Gfloat)
   is
      procedure Internal
         (Tree_View : System.Address;
          Path      : System.Address;
          Column    : System.Address;
          Use_Align : Glib.Gboolean;
          Row_Align : Gfloat;
          Col_Align : Gfloat);
      pragma Import (C, Internal, "gtk_tree_view_scroll_to_cell");
   begin
      Internal (Get_Object (Tree_View), Get_Object (Path), Get_Object_Or_Null (GObject (Column)), Boolean'Pos (Use_Align), Row_Align, Col_Align);
   end Scroll_To_Cell;

   ---------------------
   -- Scroll_To_Point --
   ---------------------

   procedure Scroll_To_Point
      (Tree_View : not null access Gtk_Tree_View_Record;
       Tree_X    : Glib.Gint;
       Tree_Y    : Glib.Gint)
   is
      procedure Internal
         (Tree_View : System.Address;
          Tree_X    : Glib.Gint;
          Tree_Y    : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_scroll_to_point");
   begin
      Internal (Get_Object (Tree_View), Tree_X, Tree_Y);
   end Scroll_To_Point;

   ----------------------------------
   -- Set_Activate_On_Single_Click --
   ----------------------------------

   procedure Set_Activate_On_Single_Click
      (Tree_View : not null access Gtk_Tree_View_Record;
       Single    : Boolean)
   is
      procedure Internal
         (Tree_View : System.Address;
          Single    : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_activate_on_single_click");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Single));
   end Set_Activate_On_Single_Click;

   ------------------------------
   -- Set_Column_Drag_Function --
   ------------------------------

   procedure Set_Column_Drag_Function
      (Tree_View : not null access Gtk_Tree_View_Record;
       Func      : Gtk_Tree_View_Column_Drop_Func)
   is
   begin
      if Func = null then
         C_Gtk_Tree_View_Set_Column_Drag_Function (Get_Object (Tree_View), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Tree_View_Set_Column_Drag_Function (Get_Object (Tree_View), Internal_Gtk_Tree_View_Column_Drop_Func'Address, To_Address (Func), System.Null_Address);
      end if;
   end Set_Column_Drag_Function;

   package body Set_Column_Drag_Function_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tree_View_Column_Drop_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tree_View_Column_Drop_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_View_Column_Drop_Func, System.Address);

      function Internal_Cb
         (Tree_View   : System.Address;
          Column      : System.Address;
          Prev_Column : System.Address;
          Next_Column : System.Address;
          Data        : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
      --  Function type for determining whether Column can be dropped in a
      --  particular spot (as determined by Prev_Column and Next_Column). In
      --  left to right locales, Prev_Column is on the left of the potential
      --  drop spot, and Next_Column is on the right. In right to left mode,
      --  this is reversed. This function should return True if the spot is a
      --  valid drop spot. Please note that returning True does not actually
      --  indicate that the column drop was made, but is meant only to indicate
      --  a possible drop spot to the user.
      --  "tree_view": A Gtk.Tree_View.Gtk_Tree_View
      --  "column": The Gtk.Tree_View_Column.Gtk_Tree_View_Column being
      --  dragged
      --  "prev_column": A Gtk.Tree_View_Column.Gtk_Tree_View_Column on one
      --  side of Column
      --  "next_column": A Gtk.Tree_View_Column.Gtk_Tree_View_Column on the
      --  other side of Column
      --  "data": user data

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Tree_View   : System.Address;
          Column      : System.Address;
          Prev_Column : System.Address;
          Next_Column : System.Address;
          Data        : System.Address) return Glib.Gboolean
      is
         D                         : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Gtk_Tree_View        : Gtk.Tree_View.Gtk_Tree_View_Record;
         Stub_Gtk_Tree_View_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;
      begin
         return Boolean'Pos (To_Gtk_Tree_View_Column_Drop_Func (D.Func) (Gtk.Tree_View.Gtk_Tree_View (Get_User_Data (Tree_View, Stub_Gtk_Tree_View)), Gtk.Tree_View_Column.Gtk_Tree_View_Column (Get_User_Data (Column, Stub_Gtk_Tree_View_Column)), Gtk.Tree_View_Column.Gtk_Tree_View_Column (Get_User_Data (Prev_Column, Stub_Gtk_Tree_View_Column)), Gtk.Tree_View_Column.Gtk_Tree_View_Column (Get_User_Data (Next_Column, Stub_Gtk_Tree_View_Column)), D.Data.all));
      end Internal_Cb;

      ------------------------------
      -- Set_Column_Drag_Function --
      ------------------------------

      procedure Set_Column_Drag_Function
         (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Func      : Gtk_Tree_View_Column_Drop_Func;
          User_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Tree_View_Set_Column_Drag_Function (Get_Object (Tree_View), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), User_Data);
            C_Gtk_Tree_View_Set_Column_Drag_Function (Get_Object (Tree_View), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Column_Drag_Function;

   end Set_Column_Drag_Function_User_Data;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor
      (Tree_View     : not null access Gtk_Tree_View_Record;
       Path          : Gtk.Tree_Model.Gtk_Tree_Path;
       Focus_Column  : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Start_Editing : Boolean)
   is
      procedure Internal
         (Tree_View     : System.Address;
          Path          : System.Address;
          Focus_Column  : System.Address;
          Start_Editing : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_cursor");
   begin
      Internal (Get_Object (Tree_View), Get_Object (Path), Get_Object_Or_Null (GObject (Focus_Column)), Boolean'Pos (Start_Editing));
   end Set_Cursor;

   ------------------------
   -- Set_Cursor_On_Cell --
   ------------------------

   procedure Set_Cursor_On_Cell
      (Tree_View     : not null access Gtk_Tree_View_Record;
       Path          : Gtk.Tree_Model.Gtk_Tree_Path;
       Focus_Column  : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Focus_Cell    : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Start_Editing : Boolean)
   is
      procedure Internal
         (Tree_View     : System.Address;
          Path          : System.Address;
          Focus_Column  : System.Address;
          Focus_Cell    : System.Address;
          Start_Editing : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_cursor_on_cell");
   begin
      Internal (Get_Object (Tree_View), Get_Object (Path), Get_Object_Or_Null (GObject (Focus_Column)), Get_Object_Or_Null (GObject (Focus_Cell)), Boolean'Pos (Start_Editing));
   end Set_Cursor_On_Cell;

   ----------------------------
   -- Set_Destroy_Count_Func --
   ----------------------------

   procedure Set_Destroy_Count_Func
      (Tree_View : not null access Gtk_Tree_View_Record;
       Func      : Gtk_Tree_Destroy_Count_Func)
   is
   begin
      if Func = null then
         C_Gtk_Tree_View_Set_Destroy_Count_Func (Get_Object (Tree_View), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Tree_View_Set_Destroy_Count_Func (Get_Object (Tree_View), Internal_Gtk_Tree_Destroy_Count_Func'Address, To_Address (Func), System.Null_Address);
      end if;
   end Set_Destroy_Count_Func;

   package body Set_Destroy_Count_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tree_Destroy_Count_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tree_Destroy_Count_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_Destroy_Count_Func, System.Address);

      procedure Internal_Cb
         (Tree_View : System.Address;
          Path      : System.Address;
          Children  : Glib.Gint;
          User_Data : System.Address);
      pragma Convention (C, Internal_Cb);

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Tree_View : System.Address;
          Path      : System.Address;
          Children  : Glib.Gint;
          User_Data : System.Address)
      is
         D                  : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_Tree_View : Gtk.Tree_View.Gtk_Tree_View_Record;
      begin
         To_Gtk_Tree_Destroy_Count_Func (D.Func) (Gtk.Tree_View.Gtk_Tree_View (Get_User_Data (Tree_View, Stub_Gtk_Tree_View)), From_Object (Path), Children, D.Data.all);
      end Internal_Cb;

      ----------------------------
      -- Set_Destroy_Count_Func --
      ----------------------------

      procedure Set_Destroy_Count_Func
         (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Func      : Gtk_Tree_Destroy_Count_Func;
          Data      : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Tree_View_Set_Destroy_Count_Func (Get_Object (Tree_View), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_Tree_View_Set_Destroy_Count_Func (Get_Object (Tree_View), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Destroy_Count_Func;

   end Set_Destroy_Count_Func_User_Data;

   -----------------------
   -- Set_Drag_Dest_Row --
   -----------------------

   procedure Set_Drag_Dest_Row
      (Tree_View : not null access Gtk_Tree_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Pos       : Gtk_Tree_View_Drop_Position)
   is
      procedure Internal
         (Tree_View : System.Address;
          Path      : System.Address;
          Pos       : Gtk_Tree_View_Drop_Position);
      pragma Import (C, Internal, "gtk_tree_view_set_drag_dest_row");
   begin
      Internal (Get_Object (Tree_View), Get_Object (Path), Pos);
   end Set_Drag_Dest_Row;

   -----------------------
   -- Set_Enable_Search --
   -----------------------

   procedure Set_Enable_Search
      (Tree_View     : not null access Gtk_Tree_View_Record;
       Enable_Search : Boolean)
   is
      procedure Internal
         (Tree_View     : System.Address;
          Enable_Search : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_enable_search");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Enable_Search));
   end Set_Enable_Search;

   ---------------------------
   -- Set_Enable_Tree_Lines --
   ---------------------------

   procedure Set_Enable_Tree_Lines
      (Tree_View : not null access Gtk_Tree_View_Record;
       Enabled   : Boolean)
   is
      procedure Internal
         (Tree_View : System.Address;
          Enabled   : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_enable_tree_lines");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Enabled));
   end Set_Enable_Tree_Lines;

   -------------------------
   -- Set_Expander_Column --
   -------------------------

   procedure Set_Expander_Column
      (Tree_View : not null access Gtk_Tree_View_Record;
       Column    : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class)
   is
      procedure Internal
         (Tree_View : System.Address;
          Column    : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_set_expander_column");
   begin
      Internal (Get_Object (Tree_View), Get_Object_Or_Null (GObject (Column)));
   end Set_Expander_Column;

   ---------------------------
   -- Set_Fixed_Height_Mode --
   ---------------------------

   procedure Set_Fixed_Height_Mode
      (Tree_View : not null access Gtk_Tree_View_Record;
       Enable    : Boolean)
   is
      procedure Internal
         (Tree_View : System.Address;
          Enable    : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_fixed_height_mode");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Enable));
   end Set_Fixed_Height_Mode;

   --------------------
   -- Set_Grid_Lines --
   --------------------

   procedure Set_Grid_Lines
      (Tree_View  : not null access Gtk_Tree_View_Record;
       Grid_Lines : Gtk.Enums.Gtk_Tree_View_Grid_Lines)
   is
      procedure Internal
         (Tree_View  : System.Address;
          Grid_Lines : Gtk.Enums.Gtk_Tree_View_Grid_Lines);
      pragma Import (C, Internal, "gtk_tree_view_set_grid_lines");
   begin
      Internal (Get_Object (Tree_View), Grid_Lines);
   end Set_Grid_Lines;

   ---------------------------
   -- Set_Headers_Clickable --
   ---------------------------

   procedure Set_Headers_Clickable
      (Tree_View : not null access Gtk_Tree_View_Record;
       Setting   : Boolean)
   is
      procedure Internal
         (Tree_View : System.Address;
          Setting   : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_headers_clickable");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Setting));
   end Set_Headers_Clickable;

   -------------------------
   -- Set_Headers_Visible --
   -------------------------

   procedure Set_Headers_Visible
      (Tree_View       : not null access Gtk_Tree_View_Record;
       Headers_Visible : Boolean)
   is
      procedure Internal
         (Tree_View       : System.Address;
          Headers_Visible : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_headers_visible");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Headers_Visible));
   end Set_Headers_Visible;

   ----------------------
   -- Set_Hover_Expand --
   ----------------------

   procedure Set_Hover_Expand
      (Tree_View : not null access Gtk_Tree_View_Record;
       Expand    : Boolean)
   is
      procedure Internal
         (Tree_View : System.Address;
          Expand    : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_hover_expand");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Expand));
   end Set_Hover_Expand;

   -------------------------
   -- Set_Hover_Selection --
   -------------------------

   procedure Set_Hover_Selection
      (Tree_View : not null access Gtk_Tree_View_Record;
       Hover     : Boolean)
   is
      procedure Internal (Tree_View : System.Address; Hover : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_hover_selection");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Hover));
   end Set_Hover_Selection;

   ---------------------------
   -- Set_Level_Indentation --
   ---------------------------

   procedure Set_Level_Indentation
      (Tree_View   : not null access Gtk_Tree_View_Record;
       Indentation : Glib.Gint)
   is
      procedure Internal
         (Tree_View   : System.Address;
          Indentation : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_set_level_indentation");
   begin
      Internal (Get_Object (Tree_View), Indentation);
   end Set_Level_Indentation;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
      (Tree_View : not null access Gtk_Tree_View_Record;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model)
   is
      procedure Internal
         (Tree_View : System.Address;
          Model     : Gtk.Tree_Model.Gtk_Tree_Model);
      pragma Import (C, Internal, "gtk_tree_view_set_model");
   begin
      Internal (Get_Object (Tree_View), Model);
   end Set_Model;

   ---------------------
   -- Set_Reorderable --
   ---------------------

   procedure Set_Reorderable
      (Tree_View   : not null access Gtk_Tree_View_Record;
       Reorderable : Boolean)
   is
      procedure Internal
         (Tree_View   : System.Address;
          Reorderable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_reorderable");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Reorderable));
   end Set_Reorderable;

   ----------------------------
   -- Set_Row_Separator_Func --
   ----------------------------

   procedure Set_Row_Separator_Func
      (Tree_View : not null access Gtk_Tree_View_Record;
       Func      : Gtk_Tree_View_Row_Separator_Func)
   is
   begin
      if Func = null then
         C_Gtk_Tree_View_Set_Row_Separator_Func (Get_Object (Tree_View), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Tree_View_Set_Row_Separator_Func (Get_Object (Tree_View), Internal_Gtk_Tree_View_Row_Separator_Func'Address, To_Address (Func), System.Null_Address);
      end if;
   end Set_Row_Separator_Func;

   package body Set_Row_Separator_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tree_View_Row_Separator_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tree_View_Row_Separator_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_View_Row_Separator_Func, System.Address);

      function Internal_Cb
         (Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Data  : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
      --  Function type for determining whether the row pointed to by Iter
      --  should be rendered as a separator. A common way to implement this is
      --  to have a boolean column in the model, whose values the
      --  Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func returns.
      --  "model": the Gtk.Tree_Model.Gtk_Tree_Model
      --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter pointing at a row in Model
      --  "data": user data

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Data  : System.Address) return Glib.Gboolean
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         return Boolean'Pos (To_Gtk_Tree_View_Row_Separator_Func (D.Func) (Model, Iter.all, D.Data.all));
      end Internal_Cb;

      ----------------------------
      -- Set_Row_Separator_Func --
      ----------------------------

      procedure Set_Row_Separator_Func
         (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Func      : Gtk_Tree_View_Row_Separator_Func;
          Data      : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Tree_View_Set_Row_Separator_Func (Get_Object (Tree_View), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_Tree_View_Set_Row_Separator_Func (Get_Object (Tree_View), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Row_Separator_Func;

   end Set_Row_Separator_Func_User_Data;

   ------------------------
   -- Set_Rubber_Banding --
   ------------------------

   procedure Set_Rubber_Banding
      (Tree_View : not null access Gtk_Tree_View_Record;
       Enable    : Boolean)
   is
      procedure Internal
         (Tree_View : System.Address;
          Enable    : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_rubber_banding");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Enable));
   end Set_Rubber_Banding;

   --------------------
   -- Set_Rules_Hint --
   --------------------

   procedure Set_Rules_Hint
      (Tree_View : not null access Gtk_Tree_View_Record;
       Setting   : Boolean)
   is
      procedure Internal
         (Tree_View : System.Address;
          Setting   : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_rules_hint");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Setting));
   end Set_Rules_Hint;

   -----------------------
   -- Set_Search_Column --
   -----------------------

   procedure Set_Search_Column
      (Tree_View : not null access Gtk_Tree_View_Record;
       Column    : Glib.Gint)
   is
      procedure Internal (Tree_View : System.Address; Column : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_set_search_column");
   begin
      Internal (Get_Object (Tree_View), Column);
   end Set_Search_Column;

   ----------------------
   -- Set_Search_Entry --
   ----------------------

   procedure Set_Search_Entry
      (Tree_View : not null access Gtk_Tree_View_Record;
       GEntry    : access Gtk.GEntry.Gtk_Entry_Record'Class)
   is
      procedure Internal
         (Tree_View : System.Address;
          GEntry    : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_set_search_entry");
   begin
      Internal (Get_Object (Tree_View), Get_Object_Or_Null (GObject (GEntry)));
   end Set_Search_Entry;

   ---------------------------
   -- Set_Search_Equal_Func --
   ---------------------------

   procedure Set_Search_Equal_Func
      (Tree_View         : not null access Gtk_Tree_View_Record;
       Search_Equal_Func : Gtk_Tree_View_Search_Equal_Func;
       Search_Destroy    : Glib.G_Destroy_Notify_Address)
   is
   begin
      if Search_Equal_Func = null then
         C_Gtk_Tree_View_Set_Search_Equal_Func (Get_Object (Tree_View), System.Null_Address, System.Null_Address, Search_Destroy);
      else
         C_Gtk_Tree_View_Set_Search_Equal_Func (Get_Object (Tree_View), Internal_Gtk_Tree_View_Search_Equal_Func'Address, To_Address (Search_Equal_Func), Search_Destroy);
      end if;
   end Set_Search_Equal_Func;

   package body Set_Search_Equal_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tree_View_Search_Equal_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tree_View_Search_Equal_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_View_Search_Equal_Func, System.Address);

      function Internal_Cb
         (Model       : Gtk.Tree_Model.Gtk_Tree_Model;
          Column      : Glib.Gint;
          Key         : Gtkada.Types.Chars_Ptr;
          Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Search_Data : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
      --  A function used for checking whether a row in Model matches a search
      --  key string entered by the user. Note the return value is reversed
      --  from what you would normally expect, though it has some similarity to
      --  strcmp returning 0 for equal strings.
      --  "model": the Gtk.Tree_Model.Gtk_Tree_Model being searched
      --  "column": the search column set by Gtk.Tree_View.Set_Search_Column
      --  "key": the key string to compare with
      --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter pointing the row of Model
      --  that should be compared with Key.
      --  "search_data": user data from Gtk.Tree_View.Set_Search_Equal_Func

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Model       : Gtk.Tree_Model.Gtk_Tree_Model;
          Column      : Glib.Gint;
          Key         : Gtkada.Types.Chars_Ptr;
          Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Search_Data : System.Address) return Glib.Gboolean
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Search_Data);
      begin
         return Boolean'Pos (To_Gtk_Tree_View_Search_Equal_Func (D.Func) (Model, Column, Gtkada.Bindings.Value_Allowing_Null (Key), Iter.all, D.Data.all));
      end Internal_Cb;

      ---------------------------
      -- Set_Search_Equal_Func --
      ---------------------------

      procedure Set_Search_Equal_Func
         (Tree_View         : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Search_Equal_Func : Gtk_Tree_View_Search_Equal_Func;
          Search_User_Data  : User_Data_Type;
          Search_Destroy    : Glib.G_Destroy_Notify_Address)
      is
         D : System.Address;
      begin
         if Search_Equal_Func = null then
            C_Gtk_Tree_View_Set_Search_Equal_Func (Get_Object (Tree_View), System.Null_Address, System.Null_Address, Search_Destroy);
         else
            D := Users.Build (To_Address (Search_Equal_Func), Search_User_Data);
            C_Gtk_Tree_View_Set_Search_Equal_Func (Get_Object (Tree_View), Internal_Cb'Address, D, Search_Destroy);
         end if;
      end Set_Search_Equal_Func;

   end Set_Search_Equal_Func_User_Data;

   ------------------------------
   -- Set_Search_Position_Func --
   ------------------------------

   procedure Set_Search_Position_Func
      (Tree_View : not null access Gtk_Tree_View_Record;
       Func      : Gtk_Tree_View_Search_Position_Func)
   is
   begin
      if Func = null then
         C_Gtk_Tree_View_Set_Search_Position_Func (Get_Object (Tree_View), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Tree_View_Set_Search_Position_Func (Get_Object (Tree_View), Internal_Gtk_Tree_View_Search_Position_Func'Address, To_Address (Func), System.Null_Address);
      end if;
   end Set_Search_Position_Func;

   package body Set_Search_Position_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tree_View_Search_Position_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tree_View_Search_Position_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_View_Search_Position_Func, System.Address);

      procedure Internal_Cb
         (Tree_View     : System.Address;
          Search_Dialog : System.Address;
          User_Data     : System.Address);
      pragma Convention (C, Internal_Cb);

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Tree_View     : System.Address;
          Search_Dialog : System.Address;
          User_Data     : System.Address)
      is
         D                  : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_Tree_View : Gtk.Tree_View.Gtk_Tree_View_Record;
         Stub_Gtk_Widget    : Gtk.Widget.Gtk_Widget_Record;
      begin
         To_Gtk_Tree_View_Search_Position_Func (D.Func) (Gtk.Tree_View.Gtk_Tree_View (Get_User_Data (Tree_View, Stub_Gtk_Tree_View)), Gtk.Widget.Gtk_Widget (Get_User_Data (Search_Dialog, Stub_Gtk_Widget)), D.Data.all);
      end Internal_Cb;

      ------------------------------
      -- Set_Search_Position_Func --
      ------------------------------

      procedure Set_Search_Position_Func
         (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
          Func      : Gtk_Tree_View_Search_Position_Func;
          Data      : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Tree_View_Set_Search_Position_Func (Get_Object (Tree_View), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_Tree_View_Set_Search_Position_Func (Get_Object (Tree_View), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Search_Position_Func;

   end Set_Search_Position_Func_User_Data;

   ------------------------
   -- Set_Show_Expanders --
   ------------------------

   procedure Set_Show_Expanders
      (Tree_View : not null access Gtk_Tree_View_Record;
       Enabled   : Boolean)
   is
      procedure Internal
         (Tree_View : System.Address;
          Enabled   : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_show_expanders");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Enabled));
   end Set_Show_Expanders;

   ----------------------
   -- Set_Tooltip_Cell --
   ----------------------

   procedure Set_Tooltip_Cell
      (Tree_View : not null access Gtk_Tree_View_Record;
       Tooltip   : not null access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Column    : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
       Cell      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal
         (Tree_View : System.Address;
          Tooltip   : System.Address;
          Path      : System.Address;
          Column    : System.Address;
          Cell      : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_set_tooltip_cell");
   begin
      Internal (Get_Object (Tree_View), Get_Object (Tooltip), Get_Object (Path), Get_Object_Or_Null (GObject (Column)), Get_Object_Or_Null (GObject (Cell)));
   end Set_Tooltip_Cell;

   ------------------------
   -- Set_Tooltip_Column --
   ------------------------

   procedure Set_Tooltip_Column
      (Tree_View : not null access Gtk_Tree_View_Record;
       Column    : Glib.Gint)
   is
      procedure Internal (Tree_View : System.Address; Column : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_set_tooltip_column");
   begin
      Internal (Get_Object (Tree_View), Column);
   end Set_Tooltip_Column;

   ---------------------
   -- Set_Tooltip_Row --
   ---------------------

   procedure Set_Tooltip_Row
      (Tree_View : not null access Gtk_Tree_View_Record;
       Tooltip   : not null access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal
         (Tree_View : System.Address;
          Tooltip   : System.Address;
          Path      : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_set_tooltip_row");
   begin
      Internal (Get_Object (Tree_View), Get_Object (Tooltip), Get_Object (Path));
   end Set_Tooltip_Row;

   --------------------------
   -- Unset_Rows_Drag_Dest --
   --------------------------

   procedure Unset_Rows_Drag_Dest
      (Tree_View : not null access Gtk_Tree_View_Record)
   is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_unset_rows_drag_dest");
   begin
      Internal (Get_Object (Tree_View));
   end Unset_Rows_Drag_Dest;

   ----------------------------
   -- Unset_Rows_Drag_Source --
   ----------------------------

   procedure Unset_Rows_Drag_Source
      (Tree_View : not null access Gtk_Tree_View_Record)
   is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_unset_rows_drag_source");
   begin
      Internal (Get_Object (Tree_View));
   end Unset_Rows_Drag_Source;

   ----------------
   -- Get_Border --
   ----------------

   function Get_Border
      (Self   : not null access Gtk_Tree_View_Record;
       Border : access Gtk.Style.Gtk_Border) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Acc_Border : access Gtk.Style.Gtk_Border) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_scrollable_get_border");
      Acc_Border     : aliased Gtk.Style.Gtk_Border;
      Tmp_Acc_Border : aliased Gtk.Style.Gtk_Border;
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Acc_Border'Access);
      Acc_Border := Tmp_Acc_Border;
      Border.all := Acc_Border;
      return Tmp_Return /= 0;
   end Get_Border;

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrollable_get_hadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Adjustment));
   end Get_Hadjustment;

   ------------------------
   -- Get_Hscroll_Policy --
   ------------------------

   function Get_Hscroll_Policy
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Scrollable_Policy;
      pragma Import (C, Internal, "gtk_scrollable_get_hscroll_policy");
   begin
      return Internal (Get_Object (Self));
   end Get_Hscroll_Policy;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrollable_get_vadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Adjustment));
   end Get_Vadjustment;

   ------------------------
   -- Get_Vscroll_Policy --
   ------------------------

   function Get_Vscroll_Policy
      (Self : not null access Gtk_Tree_View_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Scrollable_Policy;
      pragma Import (C, Internal, "gtk_scrollable_get_vscroll_policy");
   begin
      return Internal (Get_Object (Self));
   end Get_Vscroll_Policy;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
      (Self        : not null access Gtk_Tree_View_Record;
       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Self        : System.Address;
          Hadjustment : System.Address);
      pragma Import (C, Internal, "gtk_scrollable_set_hadjustment");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Hadjustment)));
   end Set_Hadjustment;

   ------------------------
   -- Set_Hscroll_Policy --
   ------------------------

   procedure Set_Hscroll_Policy
      (Self   : not null access Gtk_Tree_View_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy)
   is
      procedure Internal
         (Self   : System.Address;
          Policy : Gtk.Enums.Gtk_Scrollable_Policy);
      pragma Import (C, Internal, "gtk_scrollable_set_hscroll_policy");
   begin
      Internal (Get_Object (Self), Policy);
   end Set_Hscroll_Policy;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
      (Self        : not null access Gtk_Tree_View_Record;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Self        : System.Address;
          Vadjustment : System.Address);
      pragma Import (C, Internal, "gtk_scrollable_set_vadjustment");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Vadjustment)));
   end Set_Vadjustment;

   ------------------------
   -- Set_Vscroll_Policy --
   ------------------------

   procedure Set_Vscroll_Policy
      (Self   : not null access Gtk_Tree_View_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy)
   is
      procedure Internal
         (Self   : System.Address;
          Policy : Gtk.Enums.Gtk_Scrollable_Policy);
      pragma Import (C, Internal, "gtk_scrollable_set_vscroll_policy");
   begin
      Internal (Get_Object (Self), Policy);
   end Set_Vscroll_Policy;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Tree_View_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Tree_View_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Tree_View_Boolean_Boolean_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Tree_View_Boolean_Boolean_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean_Boolean_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean_Boolean_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Movement_Step_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Movement_Step_Gint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Tree_View_Gtk_Tree_Path_Gtk_Tree_View_Column_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Tree_View_Gtk_Tree_Path_Gtk_Tree_View_Column_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Tree_Path_Gtk_Tree_View_Column_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Tree_Path_Gtk_Tree_View_Column_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Tree_View_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Tree_View_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Tree_View_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Tree_View_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean);

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Boolean_Boolean_Boolean_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Gtk_Tree_Path_Gtk_Tree_View_Column_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Boolean_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Movement_Step_Gint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_View_Column_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean);

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean_Boolean);

   procedure Marsh_GObject_Boolean_Boolean_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean_Boolean_Boolean_Boolean);

   procedure Marsh_GObject_Gtk_Movement_Step_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Movement_Step_Gint_Boolean);

   procedure Marsh_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean);

   procedure Marsh_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void);

   procedure Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_View_Column_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_View_Column_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Tree_View_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Tree_View_Boolean);

   procedure Marsh_Gtk_Tree_View_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Tree_View_Boolean_Boolean);

   procedure Marsh_Gtk_Tree_View_Boolean_Boolean_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Tree_View_Boolean_Boolean_Boolean_Boolean);

   procedure Marsh_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean);

   procedure Marsh_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean);

   procedure Marsh_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void);

   procedure Marsh_Gtk_Tree_View_Gtk_Tree_Path_Gtk_Tree_View_Column_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Tree_View_Gtk_Tree_Path_Gtk_Tree_View_Column_Void);

   procedure Marsh_Gtk_Tree_View_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Tree_View_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Tree_View_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Boolean_Boolean_Boolean_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Tree_View_Boolean_Boolean_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Gtk_Tree_Path_Gtk_Tree_View_Column_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Tree_View_Gtk_Tree_Path_Gtk_Tree_View_Column_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Tree_View_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Boolean_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Tree_View_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean_Boolean_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Movement_Step_Gint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Movement_Step_Gint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_View_Column_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_View_Column_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------
   -- Marsh_GObject_Boolean --
   ---------------------------

   procedure Marsh_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean;

   -----------------------------------
   -- Marsh_GObject_Boolean_Boolean --
   -----------------------------------

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean_Boolean;

   ---------------------------------------------------
   -- Marsh_GObject_Boolean_Boolean_Boolean_Boolean --
   ---------------------------------------------------

   procedure Marsh_GObject_Boolean_Boolean_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean_Boolean_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1), Unchecked_To_Boolean (Params, 2), Unchecked_To_Boolean (Params, 3));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean_Boolean_Boolean_Boolean;

   --------------------------------------------------
   -- Marsh_GObject_Gtk_Movement_Step_Gint_Boolean --
   --------------------------------------------------

   procedure Marsh_GObject_Gtk_Movement_Step_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Movement_Step_Gint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Movement_Step (Params, 1), Unchecked_To_Gint (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Movement_Step_Gint_Boolean;

   -------------------------------------------------------
   -- Marsh_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean --
   -------------------------------------------------------

   procedure Marsh_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Tree_Iter (Params, 1), Gtk.Tree_Model.From_Object (Unchecked_To_Address (Params, 2)));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean;

   ----------------------------------------------------
   -- Marsh_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void --
   ----------------------------------------------------

   procedure Marsh_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Tree_Iter (Params, 1), Gtk.Tree_Model.From_Object (Unchecked_To_Address (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void;

   -----------------------------------------------------------
   -- Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_View_Column_Void --
   -----------------------------------------------------------

   procedure Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_View_Column_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Tree_Path_Gtk_Tree_View_Column_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Tree_Model.From_Object (Unchecked_To_Address (Params, 1)), Gtk.Tree_View_Column.Gtk_Tree_View_Column (Unchecked_To_Object (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Tree_Path_Gtk_Tree_View_Column_Void;

   ------------------------
   -- Marsh_GObject_Void --
   ------------------------

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   ---------------------------------
   -- Marsh_Gtk_Tree_View_Boolean --
   ---------------------------------

   procedure Marsh_Gtk_Tree_View_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Tree_View_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Tree_View := Gtk_Tree_View (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Tree_View_Boolean;

   -----------------------------------------
   -- Marsh_Gtk_Tree_View_Boolean_Boolean --
   -----------------------------------------

   procedure Marsh_Gtk_Tree_View_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Tree_View_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Tree_View := Gtk_Tree_View (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Tree_View_Boolean_Boolean;

   ---------------------------------------------------------
   -- Marsh_Gtk_Tree_View_Boolean_Boolean_Boolean_Boolean --
   ---------------------------------------------------------

   procedure Marsh_Gtk_Tree_View_Boolean_Boolean_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Tree_View_Boolean_Boolean_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Tree_View := Gtk_Tree_View (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1), Unchecked_To_Boolean (Params, 2), Unchecked_To_Boolean (Params, 3));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Tree_View_Boolean_Boolean_Boolean_Boolean;

   --------------------------------------------------------
   -- Marsh_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean --
   --------------------------------------------------------

   procedure Marsh_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Tree_View := Gtk_Tree_View (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Movement_Step (Params, 1), Unchecked_To_Gint (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean;

   -------------------------------------------------------------
   -- Marsh_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean --
   -------------------------------------------------------------

   procedure Marsh_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Tree_View := Gtk_Tree_View (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Tree_Iter (Params, 1), Gtk.Tree_Model.From_Object (Unchecked_To_Address (Params, 2)));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean;

   ----------------------------------------------------------
   -- Marsh_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void --
   ----------------------------------------------------------

   procedure Marsh_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Tree_View := Gtk_Tree_View (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Tree_Iter (Params, 1), Gtk.Tree_Model.From_Object (Unchecked_To_Address (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void;

   -----------------------------------------------------------------
   -- Marsh_Gtk_Tree_View_Gtk_Tree_Path_Gtk_Tree_View_Column_Void --
   -----------------------------------------------------------------

   procedure Marsh_Gtk_Tree_View_Gtk_Tree_Path_Gtk_Tree_View_Column_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Tree_View_Gtk_Tree_Path_Gtk_Tree_View_Column_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Tree_View := Gtk_Tree_View (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Tree_Model.From_Object (Unchecked_To_Address (Params, 1)), Gtk.Tree_View_Column.Gtk_Tree_View_Column (Unchecked_To_Object (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Tree_View_Gtk_Tree_Path_Gtk_Tree_View_Column_Void;

   ------------------------------
   -- Marsh_Gtk_Tree_View_Void --
   ------------------------------

   procedure Marsh_Gtk_Tree_View_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Tree_View_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Tree_View := Gtk_Tree_View (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Tree_View_Void;

   ------------------------
   -- On_Columns_Changed --
   ------------------------

   procedure On_Columns_Changed
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "columns-changed" & ASCII.NUL, Call, After);
   end On_Columns_Changed;

   ------------------------
   -- On_Columns_Changed --
   ------------------------

   procedure On_Columns_Changed
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "columns-changed" & ASCII.NUL, Call, After, Slot);
   end On_Columns_Changed;

   -----------------------
   -- On_Cursor_Changed --
   -----------------------

   procedure On_Cursor_Changed
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cursor-changed" & ASCII.NUL, Call, After);
   end On_Cursor_Changed;

   -----------------------
   -- On_Cursor_Changed --
   -----------------------

   procedure On_Cursor_Changed
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cursor-changed" & ASCII.NUL, Call, After, Slot);
   end On_Cursor_Changed;

   -----------------------------------
   -- On_Expand_Collapse_Cursor_Row --
   -----------------------------------

   procedure On_Expand_Collapse_Cursor_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Boolean_Boolean_Boolean_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "expand-collapse-cursor-row" & ASCII.NUL, Call, After);
   end On_Expand_Collapse_Cursor_Row;

   -----------------------------------
   -- On_Expand_Collapse_Cursor_Row --
   -----------------------------------

   procedure On_Expand_Collapse_Cursor_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Boolean_Boolean_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "expand-collapse-cursor-row" & ASCII.NUL, Call, After, Slot);
   end On_Expand_Collapse_Cursor_Row;

   --------------------
   -- On_Move_Cursor --
   --------------------

   procedure On_Move_Cursor
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Gtk_Movement_Step_Gint_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-cursor" & ASCII.NUL, Call, After);
   end On_Move_Cursor;

   --------------------
   -- On_Move_Cursor --
   --------------------

   procedure On_Move_Cursor
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-cursor" & ASCII.NUL, Call, After, Slot);
   end On_Move_Cursor;

   ----------------------
   -- On_Row_Activated --
   ----------------------

   procedure On_Row_Activated
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Gtk_Tree_Path_Gtk_Tree_View_Column_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "row-activated" & ASCII.NUL, Call, After);
   end On_Row_Activated;

   ----------------------
   -- On_Row_Activated --
   ----------------------

   procedure On_Row_Activated
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Gtk_Tree_Path_Gtk_Tree_View_Column_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "row-activated" & ASCII.NUL, Call, After, Slot);
   end On_Row_Activated;

   ----------------------
   -- On_Row_Collapsed --
   ----------------------

   procedure On_Row_Collapsed
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "row-collapsed" & ASCII.NUL, Call, After);
   end On_Row_Collapsed;

   ----------------------
   -- On_Row_Collapsed --
   ----------------------

   procedure On_Row_Collapsed
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "row-collapsed" & ASCII.NUL, Call, After, Slot);
   end On_Row_Collapsed;

   ---------------------
   -- On_Row_Expanded --
   ---------------------

   procedure On_Row_Expanded
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "row-expanded" & ASCII.NUL, Call, After);
   end On_Row_Expanded;

   ---------------------
   -- On_Row_Expanded --
   ---------------------

   procedure On_Row_Expanded
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "row-expanded" & ASCII.NUL, Call, After, Slot);
   end On_Row_Expanded;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "select-all" & ASCII.NUL, Call, After);
   end On_Select_All;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "select-all" & ASCII.NUL, Call, After, Slot);
   end On_Select_All;

   -----------------------------
   -- On_Select_Cursor_Parent --
   -----------------------------

   procedure On_Select_Cursor_Parent
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "select-cursor-parent" & ASCII.NUL, Call, After);
   end On_Select_Cursor_Parent;

   -----------------------------
   -- On_Select_Cursor_Parent --
   -----------------------------

   procedure On_Select_Cursor_Parent
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "select-cursor-parent" & ASCII.NUL, Call, After, Slot);
   end On_Select_Cursor_Parent;

   --------------------------
   -- On_Select_Cursor_Row --
   --------------------------

   procedure On_Select_Cursor_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Boolean_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "select-cursor-row" & ASCII.NUL, Call, After);
   end On_Select_Cursor_Row;

   --------------------------
   -- On_Select_Cursor_Row --
   --------------------------

   procedure On_Select_Cursor_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "select-cursor-row" & ASCII.NUL, Call, After, Slot);
   end On_Select_Cursor_Row;

   ---------------------------------
   -- On_Start_Interactive_Search --
   ---------------------------------

   procedure On_Start_Interactive_Search
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "start-interactive-search" & ASCII.NUL, Call, After);
   end On_Start_Interactive_Search;

   ---------------------------------
   -- On_Start_Interactive_Search --
   ---------------------------------

   procedure On_Start_Interactive_Search
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "start-interactive-search" & ASCII.NUL, Call, After, Slot);
   end On_Start_Interactive_Search;

   --------------------------
   -- On_Test_Collapse_Row --
   --------------------------

   procedure On_Test_Collapse_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "test-collapse-row" & ASCII.NUL, Call, After);
   end On_Test_Collapse_Row;

   --------------------------
   -- On_Test_Collapse_Row --
   --------------------------

   procedure On_Test_Collapse_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "test-collapse-row" & ASCII.NUL, Call, After, Slot);
   end On_Test_Collapse_Row;

   ------------------------
   -- On_Test_Expand_Row --
   ------------------------

   procedure On_Test_Expand_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "test-expand-row" & ASCII.NUL, Call, After);
   end On_Test_Expand_Row;

   ------------------------
   -- On_Test_Expand_Row --
   ------------------------

   procedure On_Test_Expand_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Gtk_Tree_Iter_Gtk_Tree_Path_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "test-expand-row" & ASCII.NUL, Call, After, Slot);
   end On_Test_Expand_Row;

   --------------------------
   -- On_Toggle_Cursor_Row --
   --------------------------

   procedure On_Toggle_Cursor_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "toggle-cursor-row" & ASCII.NUL, Call, After);
   end On_Toggle_Cursor_Row;

   --------------------------
   -- On_Toggle_Cursor_Row --
   --------------------------

   procedure On_Toggle_Cursor_Row
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "toggle-cursor-row" & ASCII.NUL, Call, After, Slot);
   end On_Toggle_Cursor_Row;

   ---------------------
   -- On_Unselect_All --
   ---------------------

   procedure On_Unselect_All
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_Gtk_Tree_View_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "unselect-all" & ASCII.NUL, Call, After);
   end On_Unselect_All;

   ---------------------
   -- On_Unselect_All --
   ---------------------

   procedure On_Unselect_All
      (Self  : not null access Gtk_Tree_View_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "unselect-all" & ASCII.NUL, Call, After, Slot);
   end On_Unselect_All;

end Gtk.Tree_View;
