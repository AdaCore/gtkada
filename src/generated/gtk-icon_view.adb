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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Icon_View is

   procedure Enable_Model_Drag_Dest
     (Icon_View : access Gtk_Icon_View_Record;
      Targets   : Target_Entry_Array;
      Actions   : Drag_Action)
   is
      procedure Internal
        (Icon_View : System.Address;
         Targets   : System.Address;
         N_Targets : Gint;
         Actions   : Drag_Action);
      pragma Import (C, Internal, "gtk_icon_view_enable_model_drag_dest");
   begin
      Internal (Get_Object (Icon_View),
         Targets (Targets'First)'Address, Targets'Length,
         Actions);
   end Enable_Model_Drag_Dest;

   procedure Enable_Model_Drag_Source
     (Icon_View         : access Gtk_Icon_View_Record;
      Start_Button_Mask : Gdk_Modifier_Type;
      Targets           : Target_Entry_Array;
      Actions           : Drag_Action)
   is
      procedure Internal
        (Icon_View         : System.Address;
         Start_Button_Mask : Gdk_Modifier_Type;
         Targets           : System.Address;
         N_Targets         : Gint;
         Actions           : Drag_Action);
      pragma Import (C, Internal, "gtk_icon_view_enable_model_drag_source");
   begin
      Internal (Get_Object (Icon_View), Start_Button_Mask,
         Targets (Targets'First)'Address, Targets'Length,
         Actions);
   end Enable_Model_Drag_Source;

   procedure C_Gtk_Cell_Layout_Set_Cell_Data_Func
      (Cell_Layout : System.Address;
       Cell        : System.Address;
       Func        : System.Address;
       Func_Data   : System.Address;
       Destroy     : System.Address);
   pragma Import (C, C_Gtk_Cell_Layout_Set_Cell_Data_Func, "gtk_cell_layout_set_cell_data_func");
   --  Sets the Gtk_Cell_Layout_Data_Func to use for Cell_Layout.
   --  This function is used instead of the standard attributes mapping for
   --  setting the column value, and should set the value of Cell_Layout's cell
   --  renderer(s) as appropriate.
   --  Func may be null to remove a previously set function.
   --  Since: gtk+ 2.4
   --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  "func": the Gtk_Cell_Layout_Data_Func to use, or null
   --  "func_data": user data for Func
   --  "destroy": destroy notify for Func_Data

   procedure C_Gtk_Icon_View_Selected_Foreach
      (Icon_View : System.Address;
       Func      : System.Address;
       Data      : System.Address);
   pragma Import (C, C_Gtk_Icon_View_Selected_Foreach, "gtk_icon_view_selected_foreach");
   --  Calls a function for each selected icon. Note that the model or
   --  selection cannot be modified from within this function.
   --  Since: gtk+ 2.6
   --  "func": The function to call for each selected icon.
   --  "data": User data to pass to the function.

   function To_Gtk_Icon_View_Foreach_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Icon_View_Foreach_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Icon_View_Foreach_Func, System.Address);

   function To_Gtk_Cell_Layout_Data_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Cell_Layout_Data_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Cell_Layout_Data_Func, System.Address);

   procedure Internal_Gtk_Cell_Layout_Data_Func
      (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
       Cell        : System.Address;
       Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data        : System.Address);
   pragma Convention (C, Internal_Gtk_Cell_Layout_Data_Func);
   --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
   --  "cell": the cell renderer whose value is to be set
   --  "tree_model": the model
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter indicating the row to set the
   --  value for
   --  "data": user data passed to Gtk.Cell_Layout.Set_Cell_Data_Func

   procedure Internal_Gtk_Icon_View_Foreach_Func
      (Icon_View : System.Address;
       Path      : System.Address;
       Data      : System.Address);
   pragma Convention (C, Internal_Gtk_Icon_View_Foreach_Func);
   --  "icon_view": a Gtk.Icon_View.Gtk_Icon_View
   --  "path": The Gtk.Tree_Model.Gtk_Tree_Path of a selected row
   --  "data": user data

   ----------------------------------------
   -- Internal_Gtk_Cell_Layout_Data_Func --
   ----------------------------------------

   procedure Internal_Gtk_Cell_Layout_Data_Func
      (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
       Cell        : System.Address;
       Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data        : System.Address)
   is
      Func                   : constant Gtk_Cell_Layout_Data_Func := To_Gtk_Cell_Layout_Data_Func (Data);
      Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
   begin
      Func (Cell_Layout, Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Cell, Stub_Gtk_Cell_Renderer)), Tree_Model, Iter.all);
   end Internal_Gtk_Cell_Layout_Data_Func;

   -----------------------------------------
   -- Internal_Gtk_Icon_View_Foreach_Func --
   -----------------------------------------

   procedure Internal_Gtk_Icon_View_Foreach_Func
      (Icon_View : System.Address;
       Path      : System.Address;
       Data      : System.Address)
   is
      Func               : constant Gtk_Icon_View_Foreach_Func := To_Gtk_Icon_View_Foreach_Func (Data);
      Stub_Gtk_Icon_View : Gtk_Icon_View_Record;
   begin
      Func (Gtk.Icon_View.Gtk_Icon_View (Get_User_Data (Icon_View, Stub_Gtk_Icon_View)), From_Object (Path));
   end Internal_Gtk_Icon_View_Foreach_Func;

   package Type_Conversion_Gtk_Icon_View is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Icon_View_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Icon_View);

   -----------------------
   -- Gtk_Icon_View_New --
   -----------------------

   function Gtk_Icon_View_New return Gtk_Icon_View is
      Icon_View : constant Gtk_Icon_View := new Gtk_Icon_View_Record;
   begin
      Gtk.Icon_View.Initialize (Icon_View);
      return Icon_View;
   end Gtk_Icon_View_New;

   ---------------------------------
   -- Gtk_Icon_View_New_With_Area --
   ---------------------------------

   function Gtk_Icon_View_New_With_Area
      (Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
       return Gtk_Icon_View
   is
      Icon_View : constant Gtk_Icon_View := new Gtk_Icon_View_Record;
   begin
      Gtk.Icon_View.Initialize_With_Area (Icon_View, Area);
      return Icon_View;
   end Gtk_Icon_View_New_With_Area;

   ----------------------------------
   -- Gtk_Icon_View_New_With_Model --
   ----------------------------------

   function Gtk_Icon_View_New_With_Model
      (Model : Gtk.Tree_Model.Gtk_Tree_Model) return Gtk_Icon_View
   is
      Icon_View : constant Gtk_Icon_View := new Gtk_Icon_View_Record;
   begin
      Gtk.Icon_View.Initialize_With_Model (Icon_View, Model);
      return Icon_View;
   end Gtk_Icon_View_New_With_Model;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Icon_View : out Gtk_Icon_View) is
   begin
      Icon_View := new Gtk_Icon_View_Record;
      Gtk.Icon_View.Initialize (Icon_View);
   end Gtk_New;

   -----------------------
   -- Gtk_New_With_Area --
   -----------------------

   procedure Gtk_New_With_Area
      (Icon_View : out Gtk_Icon_View;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
   is
   begin
      Icon_View := new Gtk_Icon_View_Record;
      Gtk.Icon_View.Initialize_With_Area (Icon_View, Area);
   end Gtk_New_With_Area;

   ------------------------
   -- Gtk_New_With_Model --
   ------------------------

   procedure Gtk_New_With_Model
      (Icon_View : out Gtk_Icon_View;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model)
   is
   begin
      Icon_View := new Gtk_Icon_View_Record;
      Gtk.Icon_View.Initialize_With_Model (Icon_View, Model);
   end Gtk_New_With_Model;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Icon_View : not null access Gtk_Icon_View_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_icon_view_new");
   begin
      if not Icon_View.Is_Created then
         Set_Object (Icon_View, Internal);
      end if;
   end Initialize;

   --------------------------
   -- Initialize_With_Area --
   --------------------------

   procedure Initialize_With_Area
      (Icon_View : not null access Gtk_Icon_View_Record'Class;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
   is
      function Internal (Area : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_view_new_with_area");
   begin
      if not Icon_View.Is_Created then
         Set_Object (Icon_View, Internal (Get_Object (Area)));
      end if;
   end Initialize_With_Area;

   ---------------------------
   -- Initialize_With_Model --
   ---------------------------

   procedure Initialize_With_Model
      (Icon_View : not null access Gtk_Icon_View_Record'Class;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model)
   is
      function Internal
         (Model : Gtk.Tree_Model.Gtk_Tree_Model) return System.Address;
      pragma Import (C, Internal, "gtk_icon_view_new_with_model");
   begin
      if not Icon_View.Is_Created then
         Set_Object (Icon_View, Internal (Model));
      end if;
   end Initialize_With_Model;

   -----------------------------------------
   -- Convert_Widget_To_Bin_Window_Coords --
   -----------------------------------------

   procedure Convert_Widget_To_Bin_Window_Coords
      (Icon_View : not null access Gtk_Icon_View_Record;
       Wx        : Glib.Gint;
       Wy        : Glib.Gint;
       Bx        : out Glib.Gint;
       By        : out Glib.Gint)
   is
      procedure Internal
         (Icon_View : System.Address;
          Wx        : Glib.Gint;
          Wy        : Glib.Gint;
          Bx        : out Glib.Gint;
          By        : out Glib.Gint);
      pragma Import (C, Internal, "gtk_icon_view_convert_widget_to_bin_window_coords");
   begin
      Internal (Get_Object (Icon_View), Wx, Wy, Bx, By);
   end Convert_Widget_To_Bin_Window_Coords;

   ----------------------
   -- Create_Drag_Icon --
   ----------------------

   function Create_Drag_Icon
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Cairo.Cairo_Surface
   is
      function Internal
         (Icon_View : System.Address;
          Path      : System.Address) return Cairo.Cairo_Surface;
      pragma Import (C, Internal, "gtk_icon_view_create_drag_icon");
   begin
      return Internal (Get_Object (Icon_View), Get_Object (Path));
   end Create_Drag_Icon;

   ----------------------------------
   -- Get_Activate_On_Single_Click --
   ----------------------------------

   function Get_Activate_On_Single_Click
      (Icon_View : not null access Gtk_Icon_View_Record) return Boolean
   is
      function Internal (Icon_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_view_get_activate_on_single_click");
   begin
      return Internal (Get_Object (Icon_View)) /= 0;
   end Get_Activate_On_Single_Click;

   -------------------
   -- Get_Cell_Rect --
   -------------------

   function Get_Cell_Rect
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Cell      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Rect      : access Gdk.Rectangle.Gdk_Rectangle) return Boolean
   is
      function Internal
         (Icon_View : System.Address;
          Path      : System.Address;
          Cell      : System.Address;
          Acc_Rect  : access Gdk.Rectangle.Gdk_Rectangle)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_view_get_cell_rect");
      Acc_Rect   : aliased Gdk.Rectangle.Gdk_Rectangle;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Icon_View), Get_Object (Path), Get_Object_Or_Null (GObject (Cell)), Acc_Rect'Access);
      Rect.all := Acc_Rect;
      return Tmp_Return /= 0;
   end Get_Cell_Rect;

   ------------------------
   -- Get_Column_Spacing --
   ------------------------

   function Get_Column_Spacing
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint
   is
      function Internal (Icon_View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_column_spacing");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Column_Spacing;

   -----------------
   -- Get_Columns --
   -----------------

   function Get_Columns
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint
   is
      function Internal (Icon_View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_columns");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Columns;

   ----------------
   -- Get_Cursor --
   ----------------

   procedure Get_Cursor
      (Icon_View     : not null access Gtk_Icon_View_Record;
       Path          : out Gtk.Tree_Model.Gtk_Tree_Path;
       Cell          : out Gtk.Cell_Renderer.Gtk_Cell_Renderer;
       Cursor_Is_Set : out Boolean)
   is
      function Internal
         (Icon_View : System.Address;
          Acc_Path  : access System.Address;
          Acc_Cell  : access System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_view_get_cursor");
      Acc_Path               : aliased Gtk.Tree_Model.Gtk_Tree_Path;
      Acc_Cell               : aliased Gtk.Cell_Renderer.Gtk_Cell_Renderer;
      Tmp_Acc_Path           : aliased System.Address;
      Tmp_Acc_Cell           : aliased System.Address;
      Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
      Tmp_Return             : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Icon_View), Tmp_Acc_Path'Access, Tmp_Acc_Cell'Access);
      Acc_Cell := Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Tmp_Acc_Cell, Stub_Gtk_Cell_Renderer));
      Acc_Path := From_Object (Tmp_Acc_Path);
      Path := Acc_Path;
      Cell := Acc_Cell;
      Cursor_Is_Set := Tmp_Return /= 0;
   end Get_Cursor;

   --------------------------
   -- Get_Dest_Item_At_Pos --
   --------------------------

   procedure Get_Dest_Item_At_Pos
      (Icon_View : not null access Gtk_Icon_View_Record;
       Drag_X    : Glib.Gint;
       Drag_Y    : Glib.Gint;
       Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
       Pos       : out Gtk_Icon_View_Drop_Position;
       Has_Item  : out Boolean)
   is
      function Internal
         (Icon_View : System.Address;
          Drag_X    : Glib.Gint;
          Drag_Y    : Glib.Gint;
          Acc_Path  : access System.Address;
          Acc_Pos   : access Gtk_Icon_View_Drop_Position)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_view_get_dest_item_at_pos");
      Acc_Path     : aliased Gtk.Tree_Model.Gtk_Tree_Path;
      Acc_Pos      : aliased Gtk_Icon_View_Drop_Position;
      Tmp_Acc_Path : aliased System.Address;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Icon_View), Drag_X, Drag_Y, Tmp_Acc_Path'Access, Acc_Pos'Access);
      Acc_Path := From_Object (Tmp_Acc_Path);
      Path := Acc_Path;
      Pos := Acc_Pos;
      Has_Item := Tmp_Return /= 0;
   end Get_Dest_Item_At_Pos;

   ------------------------
   -- Get_Drag_Dest_Item --
   ------------------------

   procedure Get_Drag_Dest_Item
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
       Pos       : out Gtk_Icon_View_Drop_Position)
   is
      procedure Internal
         (Icon_View : System.Address;
          Path      : out System.Address;
          Pos       : out Gtk_Icon_View_Drop_Position);
      pragma Import (C, Internal, "gtk_icon_view_get_drag_dest_item");
      Tmp_Path : aliased System.Address;
   begin
      Internal (Get_Object (Icon_View), Tmp_Path, Pos);
      Path := From_Object (Tmp_Path);
   end Get_Drag_Dest_Item;

   ---------------------
   -- Get_Item_At_Pos --
   ---------------------

   procedure Get_Item_At_Pos
      (Icon_View : not null access Gtk_Icon_View_Record;
       X         : Glib.Gint;
       Y         : Glib.Gint;
       Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
       Cell      : out Gtk.Cell_Renderer.Gtk_Cell_Renderer;
       Has_Item  : out Boolean)
   is
      function Internal
         (Icon_View : System.Address;
          X         : Glib.Gint;
          Y         : Glib.Gint;
          Acc_Path  : access System.Address;
          Acc_Cell  : access System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_view_get_item_at_pos");
      Acc_Path               : aliased Gtk.Tree_Model.Gtk_Tree_Path;
      Acc_Cell               : aliased Gtk.Cell_Renderer.Gtk_Cell_Renderer;
      Tmp_Acc_Path           : aliased System.Address;
      Tmp_Acc_Cell           : aliased System.Address;
      Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
      Tmp_Return             : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Icon_View), X, Y, Tmp_Acc_Path'Access, Tmp_Acc_Cell'Access);
      Acc_Cell := Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Tmp_Acc_Cell, Stub_Gtk_Cell_Renderer));
      Acc_Path := From_Object (Tmp_Acc_Path);
      Path := Acc_Path;
      Cell := Acc_Cell;
      Has_Item := Tmp_Return /= 0;
   end Get_Item_At_Pos;

   ---------------------
   -- Get_Item_Column --
   ---------------------

   function Get_Item_Column
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Glib.Gint
   is
      function Internal
         (Icon_View : System.Address;
          Path      : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_item_column");
   begin
      return Internal (Get_Object (Icon_View), Get_Object (Path));
   end Get_Item_Column;

   --------------------------
   -- Get_Item_Orientation --
   --------------------------

   function Get_Item_Orientation
      (Icon_View : not null access Gtk_Icon_View_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Icon_View : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_icon_view_get_item_orientation");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Item_Orientation;

   ----------------------
   -- Get_Item_Padding --
   ----------------------

   function Get_Item_Padding
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint
   is
      function Internal (Icon_View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_item_padding");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Item_Padding;

   ------------------
   -- Get_Item_Row --
   ------------------

   function Get_Item_Row
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Glib.Gint
   is
      function Internal
         (Icon_View : System.Address;
          Path      : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_item_row");
   begin
      return Internal (Get_Object (Icon_View), Get_Object (Path));
   end Get_Item_Row;

   --------------------
   -- Get_Item_Width --
   --------------------

   function Get_Item_Width
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint
   is
      function Internal (Icon_View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_item_width");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Item_Width;

   ----------------
   -- Get_Margin --
   ----------------

   function Get_Margin
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint
   is
      function Internal (Icon_View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_margin");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Margin;

   -----------------------
   -- Get_Markup_Column --
   -----------------------

   function Get_Markup_Column
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint
   is
      function Internal (Icon_View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_markup_column");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Markup_Column;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Icon_View : not null access Gtk_Icon_View_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model
   is
      function Internal
         (Icon_View : System.Address) return Gtk.Tree_Model.Gtk_Tree_Model;
      pragma Import (C, Internal, "gtk_icon_view_get_model");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Model;

   ---------------------
   -- Get_Path_At_Pos --
   ---------------------

   function Get_Path_At_Pos
      (Icon_View : not null access Gtk_Icon_View_Record;
       X         : Glib.Gint;
       Y         : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal
         (Icon_View : System.Address;
          X         : Glib.Gint;
          Y         : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_icon_view_get_path_at_pos");
   begin
      return From_Object (Internal (Get_Object (Icon_View), X, Y));
   end Get_Path_At_Pos;

   -----------------------
   -- Get_Pixbuf_Column --
   -----------------------

   function Get_Pixbuf_Column
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint
   is
      function Internal (Icon_View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_pixbuf_column");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Pixbuf_Column;

   ---------------------
   -- Get_Reorderable --
   ---------------------

   function Get_Reorderable
      (Icon_View : not null access Gtk_Icon_View_Record) return Boolean
   is
      function Internal (Icon_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_view_get_reorderable");
   begin
      return Internal (Get_Object (Icon_View)) /= 0;
   end Get_Reorderable;

   ---------------------
   -- Get_Row_Spacing --
   ---------------------

   function Get_Row_Spacing
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint
   is
      function Internal (Icon_View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_row_spacing");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Row_Spacing;

   ------------------------
   -- Get_Selected_Items --
   ------------------------

   function Get_Selected_Items
      (Icon_View : not null access Gtk_Icon_View_Record)
       return Gtk.Tree_Model.Gtk_Tree_Path_List.Glist
   is
      function Internal (Icon_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_view_get_selected_items");
      Tmp_Return : Gtk.Tree_Model.Gtk_Tree_Path_List.Glist;
   begin
      Gtk.Tree_Model.Gtk_Tree_Path_List.Set_Object (Tmp_Return, Internal (Get_Object (Icon_View)));
      return Tmp_Return;
   end Get_Selected_Items;

   ------------------------
   -- Get_Selection_Mode --
   ------------------------

   function Get_Selection_Mode
      (Icon_View : not null access Gtk_Icon_View_Record)
       return Gtk.Enums.Gtk_Selection_Mode
   is
      function Internal
         (Icon_View : System.Address) return Gtk.Enums.Gtk_Selection_Mode;
      pragma Import (C, Internal, "gtk_icon_view_get_selection_mode");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Selection_Mode;

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint
   is
      function Internal (Icon_View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_spacing");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Spacing;

   ---------------------
   -- Get_Text_Column --
   ---------------------

   function Get_Text_Column
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint
   is
      function Internal (Icon_View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_text_column");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Text_Column;

   ------------------------
   -- Get_Tooltip_Column --
   ------------------------

   function Get_Tooltip_Column
      (Icon_View : not null access Gtk_Icon_View_Record) return Glib.Gint
   is
      function Internal (Icon_View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_tooltip_column");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Tooltip_Column;

   -------------------------
   -- Get_Tooltip_Context --
   -------------------------

   procedure Get_Tooltip_Context
      (Icon_View    : not null access Gtk_Icon_View_Record;
       X            : in out Glib.Gint;
       Y            : in out Glib.Gint;
       Keyboard_Tip : Boolean;
       Model        : out Gtk.Tree_Model.Gtk_Tree_Model;
       Path         : out Gtk.Tree_Model.Gtk_Tree_Path;
       Iter         : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Success      : out Boolean)
   is
      function Internal
         (Icon_View    : System.Address;
          Acc_X        : access Glib.Gint;
          Acc_Y        : access Glib.Gint;
          Keyboard_Tip : Glib.Gboolean;
          Acc_Model    : access Gtk.Tree_Model.Gtk_Tree_Model;
          Acc_Path     : access System.Address;
          Acc_Iter     : access Gtk.Tree_Model.Gtk_Tree_Iter)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_view_get_tooltip_context");
      Acc_X        : aliased Glib.Gint := X;
      Acc_Y        : aliased Glib.Gint := Y;
      Acc_Model    : aliased Gtk.Tree_Model.Gtk_Tree_Model;
      Acc_Path     : aliased Gtk.Tree_Model.Gtk_Tree_Path;
      Acc_Iter     : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
      Tmp_Acc_Path : aliased System.Address;
      Tmp_Acc_Iter : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Icon_View), Acc_X'Access, Acc_Y'Access, Boolean'Pos (Keyboard_Tip), Acc_Model'Access, Tmp_Acc_Path'Access, Tmp_Acc_Iter'Access);
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
      (Icon_View  : not null access Gtk_Icon_View_Record;
       Start_Path : out Gtk.Tree_Model.Gtk_Tree_Path;
       End_Path   : out Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal
         (Icon_View  : System.Address;
          Start_Path : out System.Address;
          End_Path   : out System.Address);
      pragma Import (C, Internal, "gtk_icon_view_get_visible_range");
      Tmp_Start_Path : aliased System.Address;
      Tmp_End_Path   : aliased System.Address;
   begin
      Internal (Get_Object (Icon_View), Tmp_Start_Path, Tmp_End_Path);
      End_Path := From_Object (Tmp_End_Path);
      Start_Path := From_Object (Tmp_Start_Path);
   end Get_Visible_Range;

   --------------------
   -- Item_Activated --
   --------------------

   procedure Item_Activated
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal (Icon_View : System.Address; Path : System.Address);
      pragma Import (C, Internal, "gtk_icon_view_item_activated");
   begin
      Internal (Get_Object (Icon_View), Get_Object (Path));
   end Item_Activated;

   ----------------------
   -- Path_Is_Selected --
   ----------------------

   function Path_Is_Selected
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      function Internal
         (Icon_View : System.Address;
          Path      : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_view_path_is_selected");
   begin
      return Internal (Get_Object (Icon_View), Get_Object (Path)) /= 0;
   end Path_Is_Selected;

   --------------------
   -- Scroll_To_Path --
   --------------------

   procedure Scroll_To_Path
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Use_Align : Boolean;
       Row_Align : Gfloat;
       Col_Align : Gfloat)
   is
      procedure Internal
         (Icon_View : System.Address;
          Path      : System.Address;
          Use_Align : Glib.Gboolean;
          Row_Align : Gfloat;
          Col_Align : Gfloat);
      pragma Import (C, Internal, "gtk_icon_view_scroll_to_path");
   begin
      Internal (Get_Object (Icon_View), Get_Object (Path), Boolean'Pos (Use_Align), Row_Align, Col_Align);
   end Scroll_To_Path;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Icon_View : not null access Gtk_Icon_View_Record) is
      procedure Internal (Icon_View : System.Address);
      pragma Import (C, Internal, "gtk_icon_view_select_all");
   begin
      Internal (Get_Object (Icon_View));
   end Select_All;

   -----------------
   -- Select_Path --
   -----------------

   procedure Select_Path
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal (Icon_View : System.Address; Path : System.Address);
      pragma Import (C, Internal, "gtk_icon_view_select_path");
   begin
      Internal (Get_Object (Icon_View), Get_Object (Path));
   end Select_Path;

   ----------------------
   -- Selected_Foreach --
   ----------------------

   procedure Selected_Foreach
      (Icon_View : not null access Gtk_Icon_View_Record;
       Func      : Gtk_Icon_View_Foreach_Func)
   is
   begin
      if Func = null then
         C_Gtk_Icon_View_Selected_Foreach (Get_Object (Icon_View), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Icon_View_Selected_Foreach (Get_Object (Icon_View), Internal_Gtk_Icon_View_Foreach_Func'Address, To_Address (Func));
      end if;
   end Selected_Foreach;

   package body Selected_Foreach_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Icon_View_Foreach_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Icon_View_Foreach_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Icon_View_Foreach_Func, System.Address);

      procedure Internal_Cb
         (Icon_View : System.Address;
          Path      : System.Address;
          Data      : System.Address);
      pragma Convention (C, Internal_Cb);
      --  A function used by Gtk.Icon_View.Selected_Foreach to map all
      --  selected rows. It will be called on every selected row in the view.
      --  "icon_view": a Gtk.Icon_View.Gtk_Icon_View
      --  "path": The Gtk.Tree_Model.Gtk_Tree_Path of a selected row
      --  "data": user data

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Icon_View : System.Address;
          Path      : System.Address;
          Data      : System.Address)
      is
         D                  : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Gtk_Icon_View : Gtk.Icon_View.Gtk_Icon_View_Record;
      begin
         To_Gtk_Icon_View_Foreach_Func (D.Func) (Gtk.Icon_View.Gtk_Icon_View (Get_User_Data (Icon_View, Stub_Gtk_Icon_View)), From_Object (Path), D.Data.all);
      end Internal_Cb;

      ----------------------
      -- Selected_Foreach --
      ----------------------

      procedure Selected_Foreach
         (Icon_View : not null access Gtk.Icon_View.Gtk_Icon_View_Record'Class;
          Func      : Gtk_Icon_View_Foreach_Func;
          Data      : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Icon_View_Selected_Foreach (Get_Object (Icon_View), System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_Icon_View_Selected_Foreach (Get_Object (Icon_View), Internal_Cb'Address, D);
            Users.Free_Data (D);
         end if;
      end Selected_Foreach;

   end Selected_Foreach_User_Data;

   ----------------------------------
   -- Set_Activate_On_Single_Click --
   ----------------------------------

   procedure Set_Activate_On_Single_Click
      (Icon_View : not null access Gtk_Icon_View_Record;
       Single    : Boolean)
   is
      procedure Internal
         (Icon_View : System.Address;
          Single    : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_icon_view_set_activate_on_single_click");
   begin
      Internal (Get_Object (Icon_View), Boolean'Pos (Single));
   end Set_Activate_On_Single_Click;

   ------------------------
   -- Set_Cell_Data_Func --
   ------------------------

   procedure Set_Cell_Data_Func
      (Cell_Layout : not null access Gtk_Icon_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func        : Gtk_Cell_Layout_Data_Func)
   is
   begin
      if Func = null then
         C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Cell_Layout), Get_Object (Cell), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Cell_Layout), Get_Object (Cell), Internal_Gtk_Cell_Layout_Data_Func'Address, To_Address (Func), System.Null_Address);
      end if;
   end Set_Cell_Data_Func;

   package body Set_Cell_Data_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Cell_Layout_Data_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Cell_Layout_Data_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Cell_Layout_Data_Func, System.Address);

      procedure Internal_Cb
         (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
          Cell        : System.Address;
          Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
          Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Data        : System.Address);
      pragma Convention (C, Internal_Cb);
      --  A function which should set the value of Cell_Layout's cell
      --  renderer(s) as appropriate.
      --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
      --  "cell": the cell renderer whose value is to be set
      --  "tree_model": the model
      --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter indicating the row to set the
      --  value for
      --  "data": user data passed to Gtk.Cell_Layout.Set_Cell_Data_Func

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
          Cell        : System.Address;
          Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
          Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Data        : System.Address)
      is
         D                      : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
      begin
         To_Gtk_Cell_Layout_Data_Func (D.Func) (Cell_Layout, Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Cell, Stub_Gtk_Cell_Renderer)), Tree_Model, Iter.all, D.Data.all);
      end Internal_Cb;

      ------------------------
      -- Set_Cell_Data_Func --
      ------------------------

      procedure Set_Cell_Data_Func
         (Cell_Layout : not null access Gtk.Icon_View.Gtk_Icon_View_Record'Class;
          Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func        : Gtk_Cell_Layout_Data_Func;
          Func_Data   : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Cell_Layout), Get_Object (Cell), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), Func_Data);
            C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Cell_Layout), Get_Object (Cell), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Cell_Data_Func;

   end Set_Cell_Data_Func_User_Data;

   ------------------------
   -- Set_Column_Spacing --
   ------------------------

   procedure Set_Column_Spacing
      (Icon_View      : not null access Gtk_Icon_View_Record;
       Column_Spacing : Glib.Gint)
   is
      procedure Internal
         (Icon_View      : System.Address;
          Column_Spacing : Glib.Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_column_spacing");
   begin
      Internal (Get_Object (Icon_View), Column_Spacing);
   end Set_Column_Spacing;

   -----------------
   -- Set_Columns --
   -----------------

   procedure Set_Columns
      (Icon_View : not null access Gtk_Icon_View_Record;
       Columns   : Glib.Gint)
   is
      procedure Internal (Icon_View : System.Address; Columns : Glib.Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_columns");
   begin
      Internal (Get_Object (Icon_View), Columns);
   end Set_Columns;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor
      (Icon_View     : not null access Gtk_Icon_View_Record;
       Path          : Gtk.Tree_Model.Gtk_Tree_Path;
       Cell          : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Start_Editing : Boolean)
   is
      procedure Internal
         (Icon_View     : System.Address;
          Path          : System.Address;
          Cell          : System.Address;
          Start_Editing : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_icon_view_set_cursor");
   begin
      Internal (Get_Object (Icon_View), Get_Object (Path), Get_Object_Or_Null (GObject (Cell)), Boolean'Pos (Start_Editing));
   end Set_Cursor;

   ------------------------
   -- Set_Drag_Dest_Item --
   ------------------------

   procedure Set_Drag_Dest_Item
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Pos       : Gtk_Icon_View_Drop_Position)
   is
      procedure Internal
         (Icon_View : System.Address;
          Path      : System.Address;
          Pos       : Gtk_Icon_View_Drop_Position);
      pragma Import (C, Internal, "gtk_icon_view_set_drag_dest_item");
   begin
      Internal (Get_Object (Icon_View), Get_Object (Path), Pos);
   end Set_Drag_Dest_Item;

   --------------------------
   -- Set_Item_Orientation --
   --------------------------

   procedure Set_Item_Orientation
      (Icon_View   : not null access Gtk_Icon_View_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Icon_View   : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_icon_view_set_item_orientation");
   begin
      Internal (Get_Object (Icon_View), Orientation);
   end Set_Item_Orientation;

   ----------------------
   -- Set_Item_Padding --
   ----------------------

   procedure Set_Item_Padding
      (Icon_View    : not null access Gtk_Icon_View_Record;
       Item_Padding : Glib.Gint)
   is
      procedure Internal
         (Icon_View    : System.Address;
          Item_Padding : Glib.Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_item_padding");
   begin
      Internal (Get_Object (Icon_View), Item_Padding);
   end Set_Item_Padding;

   --------------------
   -- Set_Item_Width --
   --------------------

   procedure Set_Item_Width
      (Icon_View  : not null access Gtk_Icon_View_Record;
       Item_Width : Glib.Gint)
   is
      procedure Internal
         (Icon_View  : System.Address;
          Item_Width : Glib.Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_item_width");
   begin
      Internal (Get_Object (Icon_View), Item_Width);
   end Set_Item_Width;

   ----------------
   -- Set_Margin --
   ----------------

   procedure Set_Margin
      (Icon_View : not null access Gtk_Icon_View_Record;
       Margin    : Glib.Gint)
   is
      procedure Internal (Icon_View : System.Address; Margin : Glib.Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_margin");
   begin
      Internal (Get_Object (Icon_View), Margin);
   end Set_Margin;

   -----------------------
   -- Set_Markup_Column --
   -----------------------

   procedure Set_Markup_Column
      (Icon_View : not null access Gtk_Icon_View_Record;
       Column    : Glib.Gint)
   is
      procedure Internal (Icon_View : System.Address; Column : Glib.Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_markup_column");
   begin
      Internal (Get_Object (Icon_View), Column);
   end Set_Markup_Column;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
      (Icon_View : not null access Gtk_Icon_View_Record;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model)
   is
      procedure Internal
         (Icon_View : System.Address;
          Model     : Gtk.Tree_Model.Gtk_Tree_Model);
      pragma Import (C, Internal, "gtk_icon_view_set_model");
   begin
      Internal (Get_Object (Icon_View), Model);
   end Set_Model;

   -----------------------
   -- Set_Pixbuf_Column --
   -----------------------

   procedure Set_Pixbuf_Column
      (Icon_View : not null access Gtk_Icon_View_Record;
       Column    : Glib.Gint)
   is
      procedure Internal (Icon_View : System.Address; Column : Glib.Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_pixbuf_column");
   begin
      Internal (Get_Object (Icon_View), Column);
   end Set_Pixbuf_Column;

   ---------------------
   -- Set_Reorderable --
   ---------------------

   procedure Set_Reorderable
      (Icon_View   : not null access Gtk_Icon_View_Record;
       Reorderable : Boolean)
   is
      procedure Internal
         (Icon_View   : System.Address;
          Reorderable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_icon_view_set_reorderable");
   begin
      Internal (Get_Object (Icon_View), Boolean'Pos (Reorderable));
   end Set_Reorderable;

   ---------------------
   -- Set_Row_Spacing --
   ---------------------

   procedure Set_Row_Spacing
      (Icon_View   : not null access Gtk_Icon_View_Record;
       Row_Spacing : Glib.Gint)
   is
      procedure Internal
         (Icon_View   : System.Address;
          Row_Spacing : Glib.Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_row_spacing");
   begin
      Internal (Get_Object (Icon_View), Row_Spacing);
   end Set_Row_Spacing;

   ------------------------
   -- Set_Selection_Mode --
   ------------------------

   procedure Set_Selection_Mode
      (Icon_View : not null access Gtk_Icon_View_Record;
       Mode      : Gtk.Enums.Gtk_Selection_Mode)
   is
      procedure Internal
         (Icon_View : System.Address;
          Mode      : Gtk.Enums.Gtk_Selection_Mode);
      pragma Import (C, Internal, "gtk_icon_view_set_selection_mode");
   begin
      Internal (Get_Object (Icon_View), Mode);
   end Set_Selection_Mode;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing
      (Icon_View : not null access Gtk_Icon_View_Record;
       Spacing   : Glib.Gint)
   is
      procedure Internal (Icon_View : System.Address; Spacing : Glib.Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_spacing");
   begin
      Internal (Get_Object (Icon_View), Spacing);
   end Set_Spacing;

   ---------------------
   -- Set_Text_Column --
   ---------------------

   procedure Set_Text_Column
      (Icon_View : not null access Gtk_Icon_View_Record;
       Column    : Glib.Gint)
   is
      procedure Internal (Icon_View : System.Address; Column : Glib.Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_text_column");
   begin
      Internal (Get_Object (Icon_View), Column);
   end Set_Text_Column;

   ----------------------
   -- Set_Tooltip_Cell --
   ----------------------

   procedure Set_Tooltip_Cell
      (Icon_View : not null access Gtk_Icon_View_Record;
       Tooltip   : not null access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Cell      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal
         (Icon_View : System.Address;
          Tooltip   : System.Address;
          Path      : System.Address;
          Cell      : System.Address);
      pragma Import (C, Internal, "gtk_icon_view_set_tooltip_cell");
   begin
      Internal (Get_Object (Icon_View), Get_Object (Tooltip), Get_Object (Path), Get_Object_Or_Null (GObject (Cell)));
   end Set_Tooltip_Cell;

   ------------------------
   -- Set_Tooltip_Column --
   ------------------------

   procedure Set_Tooltip_Column
      (Icon_View : not null access Gtk_Icon_View_Record;
       Column    : Glib.Gint)
   is
      procedure Internal (Icon_View : System.Address; Column : Glib.Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_tooltip_column");
   begin
      Internal (Get_Object (Icon_View), Column);
   end Set_Tooltip_Column;

   ----------------------
   -- Set_Tooltip_Item --
   ----------------------

   procedure Set_Tooltip_Item
      (Icon_View : not null access Gtk_Icon_View_Record;
       Tooltip   : not null access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal
         (Icon_View : System.Address;
          Tooltip   : System.Address;
          Path      : System.Address);
      pragma Import (C, Internal, "gtk_icon_view_set_tooltip_item");
   begin
      Internal (Get_Object (Icon_View), Get_Object (Tooltip), Get_Object (Path));
   end Set_Tooltip_Item;

   ------------------
   -- Unselect_All --
   ------------------

   procedure Unselect_All (Icon_View : not null access Gtk_Icon_View_Record) is
      procedure Internal (Icon_View : System.Address);
      pragma Import (C, Internal, "gtk_icon_view_unselect_all");
   begin
      Internal (Get_Object (Icon_View));
   end Unselect_All;

   -------------------
   -- Unselect_Path --
   -------------------

   procedure Unselect_Path
      (Icon_View : not null access Gtk_Icon_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal (Icon_View : System.Address; Path : System.Address);
      pragma Import (C, Internal, "gtk_icon_view_unselect_path");
   begin
      Internal (Get_Object (Icon_View), Get_Object (Path));
   end Unselect_Path;

   ---------------------------
   -- Unset_Model_Drag_Dest --
   ---------------------------

   procedure Unset_Model_Drag_Dest
      (Icon_View : not null access Gtk_Icon_View_Record)
   is
      procedure Internal (Icon_View : System.Address);
      pragma Import (C, Internal, "gtk_icon_view_unset_model_drag_dest");
   begin
      Internal (Get_Object (Icon_View));
   end Unset_Model_Drag_Dest;

   -----------------------------
   -- Unset_Model_Drag_Source --
   -----------------------------

   procedure Unset_Model_Drag_Source
      (Icon_View : not null access Gtk_Icon_View_Record)
   is
      procedure Internal (Icon_View : System.Address);
      pragma Import (C, Internal, "gtk_icon_view_unset_model_drag_source");
   begin
      Internal (Get_Object (Icon_View));
   end Unset_Model_Drag_Source;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
      (Cell_Layout : not null access Gtk_Icon_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute   : UTF8_String;
       Column      : Glib.Gint)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address;
          Attribute   : Gtkada.Types.Chars_Ptr;
          Column      : Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_layout_add_attribute");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell), Tmp_Attribute, Column);
      Free (Tmp_Attribute);
   end Add_Attribute;

   -----------
   -- Clear --
   -----------

   procedure Clear (Cell_Layout : not null access Gtk_Icon_View_Record) is
      procedure Internal (Cell_Layout : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear");
   begin
      Internal (Get_Object (Cell_Layout));
   end Clear;

   ----------------------
   -- Clear_Attributes --
   ----------------------

   procedure Clear_Attributes
      (Cell_Layout : not null access Gtk_Icon_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear_attributes");
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell));
   end Clear_Attributes;

   ----------------
   -- Get_Border --
   ----------------

   function Get_Border
      (Self   : not null access Gtk_Icon_View_Record;
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

   ---------------
   -- Get_Cells --
   ---------------

   function Get_Cells
      (Cell_Layout : not null access Gtk_Icon_View_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist
   is
      function Internal (Cell_Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_layout_get_cells");
      Tmp_Return : Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
   begin
      Gtk.Cell_Renderer.Cell_Renderer_List.Set_Object (Tmp_Return, Internal (Get_Object (Cell_Layout)));
      return Tmp_Return;
   end Get_Cells;

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment
      (Self : not null access Gtk_Icon_View_Record)
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
      (Self : not null access Gtk_Icon_View_Record)
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
      (Self : not null access Gtk_Icon_View_Record)
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
      (Self : not null access Gtk_Icon_View_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Scrollable_Policy;
      pragma Import (C, Internal, "gtk_scrollable_get_vscroll_policy");
   begin
      return Internal (Get_Object (Self));
   end Get_Vscroll_Policy;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
      (Cell_Layout : not null access Gtk_Icon_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address;
          Expand      : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_layout_pack_end");
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell), Boolean'Pos (Expand));
   end Pack_End;

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
      (Cell_Layout : not null access Gtk_Icon_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address;
          Expand      : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_layout_pack_start");
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell), Boolean'Pos (Expand));
   end Pack_Start;

   -------------
   -- Reorder --
   -------------

   procedure Reorder
      (Cell_Layout : not null access Gtk_Icon_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position    : Glib.Gint)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address;
          Position    : Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_layout_reorder");
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell), Position);
   end Reorder;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
      (Self        : not null access Gtk_Icon_View_Record;
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
      (Self   : not null access Gtk_Icon_View_Record;
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
      (Self        : not null access Gtk_Icon_View_Record;
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
      (Self   : not null access Gtk_Icon_View_Record;
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
     (Cb_Gtk_Icon_View_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Icon_View_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Icon_View_Gtk_Tree_Path_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Icon_View_Gtk_Tree_Path_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Tree_Path_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Tree_Path_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Icon_View_Gtk_Movement_Step_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Icon_View_Gtk_Movement_Step_Gint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Movement_Step_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Movement_Step_Gint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Icon_View_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Icon_View_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Icon_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Icon_View_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Icon_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Icon_View_Gtk_Tree_Path_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Icon_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Icon_View_Gtk_Movement_Step_Gint_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Icon_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Icon_View_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Icon_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Icon_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Path_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Icon_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Movement_Step_Gint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Icon_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
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

   procedure Marsh_GObject_Gtk_Movement_Step_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Movement_Step_Gint_Boolean);

   procedure Marsh_GObject_Gtk_Tree_Path_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Tree_Path_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Icon_View_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Icon_View_Boolean);

   procedure Marsh_Gtk_Icon_View_Gtk_Movement_Step_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Icon_View_Gtk_Movement_Step_Gint_Boolean);

   procedure Marsh_Gtk_Icon_View_Gtk_Tree_Path_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Icon_View_Gtk_Tree_Path_Void);

   procedure Marsh_Gtk_Icon_View_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Icon_View_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Icon_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Icon_View_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Icon_View_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Icon_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Icon_View_Gtk_Tree_Path_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Icon_View_Gtk_Tree_Path_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Icon_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Icon_View_Gtk_Movement_Step_Gint_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Icon_View_Gtk_Movement_Step_Gint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Icon_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Icon_View_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Icon_View_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Icon_View_Record'Class;
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
      (Object  : access Gtk_Icon_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Path_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Tree_Path_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Icon_View_Record'Class;
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
      (Object  : access Gtk_Icon_View_Record'Class;
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

   --------------------------------------
   -- Marsh_GObject_Gtk_Tree_Path_Void --
   --------------------------------------

   procedure Marsh_GObject_Gtk_Tree_Path_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Tree_Path_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Tree_Model.From_Object (Unchecked_To_Address (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Tree_Path_Void;

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
   -- Marsh_Gtk_Icon_View_Boolean --
   ---------------------------------

   procedure Marsh_Gtk_Icon_View_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Icon_View_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Icon_View := Gtk_Icon_View (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Icon_View_Boolean;

   --------------------------------------------------------
   -- Marsh_Gtk_Icon_View_Gtk_Movement_Step_Gint_Boolean --
   --------------------------------------------------------

   procedure Marsh_Gtk_Icon_View_Gtk_Movement_Step_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Icon_View_Gtk_Movement_Step_Gint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Icon_View := Gtk_Icon_View (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Movement_Step (Params, 1), Unchecked_To_Gint (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Icon_View_Gtk_Movement_Step_Gint_Boolean;

   --------------------------------------------
   -- Marsh_Gtk_Icon_View_Gtk_Tree_Path_Void --
   --------------------------------------------

   procedure Marsh_Gtk_Icon_View_Gtk_Tree_Path_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Icon_View_Gtk_Tree_Path_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Icon_View := Gtk_Icon_View (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Tree_Model.From_Object (Unchecked_To_Address (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Icon_View_Gtk_Tree_Path_Void;

   ------------------------------
   -- Marsh_Gtk_Icon_View_Void --
   ------------------------------

   procedure Marsh_Gtk_Icon_View_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Icon_View_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Icon_View := Gtk_Icon_View (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Icon_View_Void;

   -----------------------------
   -- On_Activate_Cursor_Item --
   -----------------------------

   procedure On_Activate_Cursor_Item
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate-cursor-item" & ASCII.NUL, Call, After);
   end On_Activate_Cursor_Item;

   -----------------------------
   -- On_Activate_Cursor_Item --
   -----------------------------

   procedure On_Activate_Cursor_Item
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate-cursor-item" & ASCII.NUL, Call, After, Slot);
   end On_Activate_Cursor_Item;

   -----------------------
   -- On_Item_Activated --
   -----------------------

   procedure On_Item_Activated
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Gtk_Tree_Path_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "item-activated" & ASCII.NUL, Call, After);
   end On_Item_Activated;

   -----------------------
   -- On_Item_Activated --
   -----------------------

   procedure On_Item_Activated
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Gtk_Tree_Path_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "item-activated" & ASCII.NUL, Call, After, Slot);
   end On_Item_Activated;

   --------------------
   -- On_Move_Cursor --
   --------------------

   procedure On_Move_Cursor
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Gtk_Movement_Step_Gint_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-cursor" & ASCII.NUL, Call, After);
   end On_Move_Cursor;

   --------------------
   -- On_Move_Cursor --
   --------------------

   procedure On_Move_Cursor
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-cursor" & ASCII.NUL, Call, After, Slot);
   end On_Move_Cursor;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "select-all" & ASCII.NUL, Call, After);
   end On_Select_All;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "select-all" & ASCII.NUL, Call, After, Slot);
   end On_Select_All;

   ---------------------------
   -- On_Select_Cursor_Item --
   ---------------------------

   procedure On_Select_Cursor_Item
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "select-cursor-item" & ASCII.NUL, Call, After);
   end On_Select_Cursor_Item;

   ---------------------------
   -- On_Select_Cursor_Item --
   ---------------------------

   procedure On_Select_Cursor_Item
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "select-cursor-item" & ASCII.NUL, Call, After, Slot);
   end On_Select_Cursor_Item;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "selection-changed" & ASCII.NUL, Call, After);
   end On_Selection_Changed;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "selection-changed" & ASCII.NUL, Call, After, Slot);
   end On_Selection_Changed;

   ---------------------------
   -- On_Toggle_Cursor_Item --
   ---------------------------

   procedure On_Toggle_Cursor_Item
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "toggle-cursor-item" & ASCII.NUL, Call, After);
   end On_Toggle_Cursor_Item;

   ---------------------------
   -- On_Toggle_Cursor_Item --
   ---------------------------

   procedure On_Toggle_Cursor_Item
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "toggle-cursor-item" & ASCII.NUL, Call, After, Slot);
   end On_Toggle_Cursor_Item;

   ---------------------
   -- On_Unselect_All --
   ---------------------

   procedure On_Unselect_All
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_Gtk_Icon_View_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "unselect-all" & ASCII.NUL, Call, After);
   end On_Unselect_All;

   ---------------------
   -- On_Unselect_All --
   ---------------------

   procedure On_Unselect_All
      (Self  : not null access Gtk_Icon_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "unselect-all" & ASCII.NUL, Call, After, Slot);
   end On_Unselect_All;

end Gtk.Icon_View;
