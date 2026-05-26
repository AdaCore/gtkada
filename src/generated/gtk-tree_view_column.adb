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

package body Gtk.Tree_View_Column is

   function Convert (R : Gtk.Tree_View_Column.Gtk_Tree_View_Column) return System.Address is
   begin
      return Get_Object (R);
   end Convert;

   function Convert (R : System.Address) return Gtk.Tree_View_Column.Gtk_Tree_View_Column is
      Stub : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;begin
         return Gtk.Tree_View_Column.Gtk_Tree_View_Column (Glib.Object.Get_User_Data (R, Stub));
      end Convert;

   procedure C_Gtk_Cell_Layout_Set_Cell_Data_Func
      (Self      : System.Address;
       Cell      : System.Address;
       Func      : System.Address;
       Func_Data : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Cell_Layout_Set_Cell_Data_Func, "gtk_cell_layout_set_cell_data_func");
   pragma Obsolescent (C_Gtk_Cell_Layout_Set_Cell_Data_Func);
   --  Sets the `GtkCellLayout`DataFunc to use for Cell_Layout.
   --  This function is used instead of the standard attributes mapping for
   --  setting the column value, and should set the value of Cell_Layout's cell
   --  renderer(s) as appropriate.
   --  Func may be null to remove a previously set function.
   --  Deprecated since 4.10, 1
   --  @param Cell a `GtkCellRenderer`
   --  @param Func the `GtkCellLayout`DataFunc to use
   --  @param Func_Data user data for Func
   --  @param Destroy destroy notify for Func_Data

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
   pragma Obsolescent (Internal_Gtk_Cell_Layout_Data_Func);
   pragma Convention (C, Internal_Gtk_Cell_Layout_Data_Func);
   --  Deprecated since 4.20, 1
   --  @param Cell_Layout a `GtkCellLayout`
   --  @param Cell the cell renderer whose value is to be set
   --  @param Tree_Model the model
   --  @param Iter a `GtkTreeIter` indicating the row to set the value for
   --  @param Data user data passed to Gtk.Cell_Layout.Set_Cell_Data_Func

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

   package Type_Conversion_Gtk_Tree_View_Column is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_View_Column_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Tree_View_Column);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Tree_View_Column) is
   begin
      Self := new Gtk_Tree_View_Column_Record;
      Gtk.Tree_View_Column.Initialize (Self);
   end Gtk_New;

   -----------------------
   -- Gtk_New_With_Area --
   -----------------------

   procedure Gtk_New_With_Area
      (Self : out Gtk_Tree_View_Column;
       Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
   is
   begin
      Self := new Gtk_Tree_View_Column_Record;
      Gtk.Tree_View_Column.Initialize_With_Area (Self, Area);
   end Gtk_New_With_Area;

   ------------------------------
   -- Gtk_Tree_View_Column_New --
   ------------------------------

   function Gtk_Tree_View_Column_New return Gtk_Tree_View_Column is
      Self : constant Gtk_Tree_View_Column := new Gtk_Tree_View_Column_Record;
   begin
      Gtk.Tree_View_Column.Initialize (Self);
      return Self;
   end Gtk_Tree_View_Column_New;

   ----------------------------------------
   -- Gtk_Tree_View_Column_New_With_Area --
   ----------------------------------------

   function Gtk_Tree_View_Column_New_With_Area
      (Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
       return Gtk_Tree_View_Column
   is
      Self : constant Gtk_Tree_View_Column := new Gtk_Tree_View_Column_Record;
   begin
      Gtk.Tree_View_Column.Initialize_With_Area (Self, Area);
      return Self;
   end Gtk_Tree_View_Column_New_With_Area;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Tree_View_Column_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_column_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   --------------------------
   -- Initialize_With_Area --
   --------------------------

   procedure Initialize_With_Area
      (Self : not null access Gtk_Tree_View_Column_Record'Class;
       Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
   is
      function Internal (Area : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_column_new_with_area");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Area)));
      end if;
   end Initialize_With_Area;

   -----------------------
   -- Cell_Get_Position --
   -----------------------

   procedure Cell_Get_Position
      (Self          : not null access Gtk_Tree_View_Column_Record;
       Cell_Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       X_Offset      : out Glib.Gint;
       Width         : out Glib.Gint;
       Success       : out Boolean)
   is
      function Internal
         (Self          : System.Address;
          Cell_Renderer : System.Address;
          Acc_X_Offset  : access Glib.Gint;
          Acc_Width     : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_column_cell_get_position");
      Acc_X_Offset : aliased Glib.Gint;
      Acc_Width    : aliased Glib.Gint;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Get_Object (Cell_Renderer), Acc_X_Offset'Access, Acc_Width'Access);
      X_Offset := Acc_X_Offset;
      Width := Acc_Width;
      Success := Tmp_Return /= 0;
   end Cell_Get_Position;

   -------------------
   -- Cell_Get_Size --
   -------------------

   procedure Cell_Get_Size
      (Self     : not null access Gtk_Tree_View_Column_Record;
       X_Offset : out Glib.Gint;
       Y_Offset : out Glib.Gint;
       Width    : out Glib.Gint;
       Height   : out Glib.Gint)
   is
      procedure Internal
         (Self     : System.Address;
          X_Offset : out Glib.Gint;
          Y_Offset : out Glib.Gint;
          Width    : out Glib.Gint;
          Height   : out Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_column_cell_get_size");
   begin
      Internal (Get_Object (Self), X_Offset, Y_Offset, Width, Height);
   end Cell_Get_Size;

   ---------------------
   -- Cell_Is_Visible --
   ---------------------

   function Cell_Is_Visible
      (Self : not null access Gtk_Tree_View_Column_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_column_cell_is_visible");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Cell_Is_Visible;

   ------------------------
   -- Cell_Set_Cell_Data --
   ------------------------

   procedure Cell_Set_Cell_Data
      (Self        : not null access Gtk_Tree_View_Column_Record;
       Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
       Is_Expander : Boolean;
       Is_Expanded : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
          Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
          Is_Expander : Glib.Gboolean;
          Is_Expanded : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_column_cell_set_cell_data");
   begin
      Internal (Get_Object (Self), Tree_Model, Iter, Boolean'Pos (Is_Expander), Boolean'Pos (Is_Expanded));
   end Cell_Set_Cell_Data;

   -------------
   -- Clicked --
   -------------

   procedure Clicked (Self : not null access Gtk_Tree_View_Column_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_column_clicked");
   begin
      Internal (Get_Object (Self));
   end Clicked;

   ----------------
   -- Focus_Cell --
   ----------------

   procedure Focus_Cell
      (Self : not null access Gtk_Tree_View_Column_Record;
       Cell : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal (Self : System.Address; Cell : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_column_focus_cell");
   begin
      Internal (Get_Object (Self), Get_Object (Cell));
   end Focus_Cell;

   -------------------
   -- Get_Alignment --
   -------------------

   function Get_Alignment
      (Self : not null access Gtk_Tree_View_Column_Record)
       return Interfaces.C.C_float
   is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gtk_tree_view_column_get_alignment");
   begin
      return Internal (Get_Object (Self));
   end Get_Alignment;

   ----------------
   -- Get_Button --
   ----------------

   function Get_Button
      (Self : not null access Gtk_Tree_View_Column_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_column_get_button");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Button;

   -------------------
   -- Get_Clickable --
   -------------------

   function Get_Clickable
      (Self : not null access Gtk_Tree_View_Column_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_column_get_clickable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Clickable;

   ----------------
   -- Get_Expand --
   ----------------

   function Get_Expand
      (Self : not null access Gtk_Tree_View_Column_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_column_get_expand");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Expand;

   ---------------------
   -- Get_Fixed_Width --
   ---------------------

   function Get_Fixed_Width
      (Self : not null access Gtk_Tree_View_Column_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tree_view_column_get_fixed_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Fixed_Width;

   -------------------
   -- Get_Max_Width --
   -------------------

   function Get_Max_Width
      (Self : not null access Gtk_Tree_View_Column_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tree_view_column_get_max_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Max_Width;

   -------------------
   -- Get_Min_Width --
   -------------------

   function Get_Min_Width
      (Self : not null access Gtk_Tree_View_Column_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tree_view_column_get_min_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Min_Width;

   ---------------------
   -- Get_Reorderable --
   ---------------------

   function Get_Reorderable
      (Self : not null access Gtk_Tree_View_Column_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_column_get_reorderable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Reorderable;

   -------------------
   -- Get_Resizable --
   -------------------

   function Get_Resizable
      (Self : not null access Gtk_Tree_View_Column_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_column_get_resizable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Resizable;

   ----------------
   -- Get_Sizing --
   ----------------

   function Get_Sizing
      (Self : not null access Gtk_Tree_View_Column_Record)
       return Gtk_Tree_View_Column_Sizing
   is
      function Internal
         (Self : System.Address) return Gtk_Tree_View_Column_Sizing;
      pragma Import (C, Internal, "gtk_tree_view_column_get_sizing");
   begin
      return Internal (Get_Object (Self));
   end Get_Sizing;

   ------------------------
   -- Get_Sort_Column_Id --
   ------------------------

   function Get_Sort_Column_Id
      (Self : not null access Gtk_Tree_View_Column_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tree_view_column_get_sort_column_id");
   begin
      return Internal (Get_Object (Self));
   end Get_Sort_Column_Id;

   ------------------------
   -- Get_Sort_Indicator --
   ------------------------

   function Get_Sort_Indicator
      (Self : not null access Gtk_Tree_View_Column_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_column_get_sort_indicator");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Sort_Indicator;

   --------------------
   -- Get_Sort_Order --
   --------------------

   function Get_Sort_Order
      (Self : not null access Gtk_Tree_View_Column_Record)
       return Gtk.Enums.Gtk_Sort_Type
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Sort_Type;
      pragma Import (C, Internal, "gtk_tree_view_column_get_sort_order");
   begin
      return Internal (Get_Object (Self));
   end Get_Sort_Order;

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing
      (Self : not null access Gtk_Tree_View_Column_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tree_view_column_get_spacing");
   begin
      return Internal (Get_Object (Self));
   end Get_Spacing;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Self : not null access Gtk_Tree_View_Column_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_tree_view_column_get_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Title;

   -------------------
   -- Get_Tree_View --
   -------------------

   function Get_Tree_View
      (Self : not null access Gtk_Tree_View_Column_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_column_get_tree_view");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Tree_View;

   -----------------
   -- Get_Visible --
   -----------------

   function Get_Visible
      (Self : not null access Gtk_Tree_View_Column_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_column_get_visible");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Visible;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget
      (Self : not null access Gtk_Tree_View_Column_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_column_get_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Widget;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
      (Self : not null access Gtk_Tree_View_Column_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tree_view_column_get_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Width;

   ------------------
   -- Get_X_Offset --
   ------------------

   function Get_X_Offset
      (Self : not null access Gtk_Tree_View_Column_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tree_view_column_get_x_offset");
   begin
      return Internal (Get_Object (Self));
   end Get_X_Offset;

   ------------------
   -- Queue_Resize --
   ------------------

   procedure Queue_Resize
      (Self : not null access Gtk_Tree_View_Column_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_column_queue_resize");
   begin
      Internal (Get_Object (Self));
   end Queue_Resize;

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment
      (Self   : not null access Gtk_Tree_View_Column_Record;
       Xalign : Interfaces.C.C_float)
   is
      procedure Internal
         (Self   : System.Address;
          Xalign : Interfaces.C.C_float);
      pragma Import (C, Internal, "gtk_tree_view_column_set_alignment");
   begin
      Internal (Get_Object (Self), Xalign);
   end Set_Alignment;

   ------------------------
   -- Set_Cell_Data_Func --
   ------------------------

   procedure Set_Cell_Data_Func
      (Self : not null access Gtk_Tree_View_Column_Record;
       Cell : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func : Gtk_Cell_Layout_Data_Func)
   is
   begin
      if Func = null then
         C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Self), Get_Object (Cell), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Self), Get_Object (Cell), Internal_Gtk_Cell_Layout_Data_Func'Address, To_Address (Func), System.Null_Address);
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
      pragma Obsolescent (Internal_Cb);
      pragma Convention (C, Internal_Cb);
      --  A function which should set the value of Cell_Layout's cell
      --  renderer(s) as appropriate.
      --  Deprecated since 4.20, 1
      --  @param Cell_Layout a `GtkCellLayout`
      --  @param Cell the cell renderer whose value is to be set
      --  @param Tree_Model the model
      --  @param Iter a `GtkTreeIter` indicating the row to set the value for
      --  @param Data user data passed to Gtk.Cell_Layout.Set_Cell_Data_Func

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
         (Self      : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
          Cell      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func      : Gtk_Cell_Layout_Data_Func;
          Func_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Self), Get_Object (Cell), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), Func_Data);
            C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Self), Get_Object (Cell), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Cell_Data_Func;

   end Set_Cell_Data_Func_User_Data;

   -------------------
   -- Set_Clickable --
   -------------------

   procedure Set_Clickable
      (Self      : not null access Gtk_Tree_View_Column_Record;
       Clickable : Boolean)
   is
      procedure Internal (Self : System.Address; Clickable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_column_set_clickable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Clickable));
   end Set_Clickable;

   ----------------
   -- Set_Expand --
   ----------------

   procedure Set_Expand
      (Self   : not null access Gtk_Tree_View_Column_Record;
       Expand : Boolean)
   is
      procedure Internal (Self : System.Address; Expand : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_column_set_expand");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Expand));
   end Set_Expand;

   ---------------------
   -- Set_Fixed_Width --
   ---------------------

   procedure Set_Fixed_Width
      (Self        : not null access Gtk_Tree_View_Column_Record;
       Fixed_Width : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Fixed_Width : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_column_set_fixed_width");
   begin
      Internal (Get_Object (Self), Fixed_Width);
   end Set_Fixed_Width;

   -------------------
   -- Set_Max_Width --
   -------------------

   procedure Set_Max_Width
      (Self      : not null access Gtk_Tree_View_Column_Record;
       Max_Width : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Max_Width : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_column_set_max_width");
   begin
      Internal (Get_Object (Self), Max_Width);
   end Set_Max_Width;

   -------------------
   -- Set_Min_Width --
   -------------------

   procedure Set_Min_Width
      (Self      : not null access Gtk_Tree_View_Column_Record;
       Min_Width : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Min_Width : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_column_set_min_width");
   begin
      Internal (Get_Object (Self), Min_Width);
   end Set_Min_Width;

   ---------------------
   -- Set_Reorderable --
   ---------------------

   procedure Set_Reorderable
      (Self        : not null access Gtk_Tree_View_Column_Record;
       Reorderable : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Reorderable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_column_set_reorderable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Reorderable));
   end Set_Reorderable;

   -------------------
   -- Set_Resizable --
   -------------------

   procedure Set_Resizable
      (Self      : not null access Gtk_Tree_View_Column_Record;
       Resizable : Boolean)
   is
      procedure Internal (Self : System.Address; Resizable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_column_set_resizable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Resizable));
   end Set_Resizable;

   ----------------
   -- Set_Sizing --
   ----------------

   procedure Set_Sizing
      (Self     : not null access Gtk_Tree_View_Column_Record;
       The_Type : Gtk_Tree_View_Column_Sizing)
   is
      procedure Internal
         (Self     : System.Address;
          The_Type : Gtk_Tree_View_Column_Sizing);
      pragma Import (C, Internal, "gtk_tree_view_column_set_sizing");
   begin
      Internal (Get_Object (Self), The_Type);
   end Set_Sizing;

   ------------------------
   -- Set_Sort_Column_Id --
   ------------------------

   procedure Set_Sort_Column_Id
      (Self           : not null access Gtk_Tree_View_Column_Record;
       Sort_Column_Id : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Sort_Column_Id : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_column_set_sort_column_id");
   begin
      Internal (Get_Object (Self), Sort_Column_Id);
   end Set_Sort_Column_Id;

   ------------------------
   -- Set_Sort_Indicator --
   ------------------------

   procedure Set_Sort_Indicator
      (Self    : not null access Gtk_Tree_View_Column_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_column_set_sort_indicator");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Sort_Indicator;

   --------------------
   -- Set_Sort_Order --
   --------------------

   procedure Set_Sort_Order
      (Self  : not null access Gtk_Tree_View_Column_Record;
       Order : Gtk.Enums.Gtk_Sort_Type)
   is
      procedure Internal
         (Self  : System.Address;
          Order : Gtk.Enums.Gtk_Sort_Type);
      pragma Import (C, Internal, "gtk_tree_view_column_set_sort_order");
   begin
      Internal (Get_Object (Self), Order);
   end Set_Sort_Order;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing
      (Self    : not null access Gtk_Tree_View_Column_Record;
       Spacing : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Spacing : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_view_column_set_spacing");
   begin
      Internal (Get_Object (Self), Spacing);
   end Set_Spacing;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Self  : not null access Gtk_Tree_View_Column_Record;
       Title : UTF8_String)
   is
      procedure Internal
         (Self  : System.Address;
          Title : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_tree_view_column_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr := New_String (Title);
   begin
      Internal (Get_Object (Self), Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

   -----------------
   -- Set_Visible --
   -----------------

   procedure Set_Visible
      (Self    : not null access Gtk_Tree_View_Column_Record;
       Visible : Boolean)
   is
      procedure Internal (Self : System.Address; Visible : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_column_set_visible");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Visible));
   end Set_Visible;

   ----------------
   -- Set_Widget --
   ----------------

   procedure Set_Widget
      (Self   : not null access Gtk_Tree_View_Column_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Widget : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_column_set_widget");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Widget)));
   end Set_Widget;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
      (Self      : not null access Gtk_Tree_View_Column_Record;
       Cell      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String;
       Column    : Glib.Gint)
   is
      procedure Internal
         (Self      : System.Address;
          Cell      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr;
          Column    : Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_layout_add_attribute");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Self), Get_Object (Cell), Tmp_Attribute, Column);
      Free (Tmp_Attribute);
   end Add_Attribute;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : not null access Gtk_Tree_View_Column_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear");
   begin
      Internal (Get_Object (Self));
   end Clear;

   ----------------------
   -- Clear_Attributes --
   ----------------------

   procedure Clear_Attributes
      (Self : not null access Gtk_Tree_View_Column_Record;
       Cell : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal (Self : System.Address; Cell : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear_attributes");
   begin
      Internal (Get_Object (Self), Get_Object (Cell));
   end Clear_Attributes;

   ---------------
   -- Get_Cells --
   ---------------

   function Get_Cells
      (Self : not null access Gtk_Tree_View_Column_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_layout_get_cells");
      Tmp_Return : Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
   begin
      Gtk.Cell_Renderer.Cell_Renderer_List.Set_Object (Tmp_Return, Internal (Get_Object (Self)));
      return Tmp_Return;
   end Get_Cells;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
      (Self   : not null access Gtk_Tree_View_Column_Record;
       Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand : Boolean)
   is
      procedure Internal
         (Self   : System.Address;
          Cell   : System.Address;
          Expand : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_layout_pack_end");
   begin
      Internal (Get_Object (Self), Get_Object (Cell), Boolean'Pos (Expand));
   end Pack_End;

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
      (Self   : not null access Gtk_Tree_View_Column_Record;
       Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand : Boolean)
   is
      procedure Internal
         (Self   : System.Address;
          Cell   : System.Address;
          Expand : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_layout_pack_start");
   begin
      Internal (Get_Object (Self), Get_Object (Cell), Boolean'Pos (Expand));
   end Pack_Start;

   -------------
   -- Reorder --
   -------------

   procedure Reorder
      (Self     : not null access Gtk_Tree_View_Column_Record;
       Cell     : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position : Glib.Gint)
   is
      procedure Internal
         (Self     : System.Address;
          Cell     : System.Address;
          Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_layout_reorder");
   begin
      Internal (Get_Object (Self), Get_Object (Cell), Position);
   end Reorder;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Tree_View_Column_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Tree_View_Column_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Tree_View_Column_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Column_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Column_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Tree_View_Column_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Tree_View_Column_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Tree_View_Column_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_View_Column_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Tree_View_Column_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Tree_View_Column_Record'Class;
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

   -------------------------------------
   -- Marsh_Gtk_Tree_View_Column_Void --
   -------------------------------------

   procedure Marsh_Gtk_Tree_View_Column_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Tree_View_Column_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Tree_View_Column := Gtk_Tree_View_Column (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Tree_View_Column_Void;

   ----------------
   -- On_Clicked --
   ----------------

   procedure On_Clicked
      (Self  : not null access Gtk_Tree_View_Column_Record;
       Call  : Cb_Gtk_Tree_View_Column_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "clicked" & ASCII.NUL, Call, After);
   end On_Clicked;

   ----------------
   -- On_Clicked --
   ----------------

   procedure On_Clicked
      (Self  : not null access Gtk_Tree_View_Column_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "clicked" & ASCII.NUL, Call, After, Slot);
   end On_Clicked;

end Gtk.Tree_View_Column;
