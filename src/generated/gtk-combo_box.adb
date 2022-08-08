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
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Combo_Box is

   function Get_Active_Iter
     (Combo_Box : not null access Gtk_Combo_Box_Record) return Gtk_Tree_Iter
   is
      function Internal
        (Combo_Box : System.Address;
         Iter      : System.Address)
      return Gboolean;
      pragma Import (C, Internal, "gtk_combo_box_get_active_iter");
      Iter : aliased Gtk_Tree_Iter;
      Tmp  : constant Gboolean := Internal
        (Get_Object (Combo_Box), Iter'Address);
   begin
      if Tmp /= 0 then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Get_Active_Iter;

   function Get_Active_Text
     (Combo_Box : not null access Gtk_Combo_Box_Record)
   return UTF8_String is
   begin
      if not Combo_Box.Get_Has_Entry then
         return "";
      end if;

      return Gtk_Entry (Combo_Box.Get_Child).Get_Text;
   end Get_Active_Text;

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

   procedure C_Gtk_Combo_Box_Set_Row_Separator_Func
      (Combo_Box : System.Address;
       Func      : System.Address;
       Data      : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Combo_Box_Set_Row_Separator_Func, "gtk_combo_box_set_row_separator_func");
   --  Sets the row separator function, which is used to determine whether a
   --  row should be drawn as a separator. If the row separator function is
   --  null, no separators are drawn. This is the default value.
   --  Since: gtk+ 2.6
   --  "func": a Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func
   --  "data": user data to pass to Func, or null
   --  "destroy": destroy notifier for Data, or null

   function To_Gtk_Tree_View_Row_Separator_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_View_Row_Separator_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func, System.Address);

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

   function Internal_Gtk_Tree_View_Row_Separator_Func
      (Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data  : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_Tree_View_Row_Separator_Func);
   --  "model": the Gtk.Tree_Model.Gtk_Tree_Model
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter pointing at a row in Model
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

   package Type_Conversion_Gtk_Combo_Box is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Combo_Box_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Combo_Box);

   -----------------------
   -- Gtk_Combo_Box_New --
   -----------------------

   function Gtk_Combo_Box_New return Gtk_Combo_Box is
      Combo_Box : constant Gtk_Combo_Box := new Gtk_Combo_Box_Record;
   begin
      Gtk.Combo_Box.Initialize (Combo_Box);
      return Combo_Box;
   end Gtk_Combo_Box_New;

   ---------------------------------
   -- Gtk_Combo_Box_New_With_Area --
   ---------------------------------

   function Gtk_Combo_Box_New_With_Area
      (Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
       return Gtk_Combo_Box
   is
      Combo_Box : constant Gtk_Combo_Box := new Gtk_Combo_Box_Record;
   begin
      Gtk.Combo_Box.Initialize_With_Area (Combo_Box, Area);
      return Combo_Box;
   end Gtk_Combo_Box_New_With_Area;

   -------------------------------------------
   -- Gtk_Combo_Box_New_With_Area_And_Entry --
   -------------------------------------------

   function Gtk_Combo_Box_New_With_Area_And_Entry
      (Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
       return Gtk_Combo_Box
   is
      Combo_Box : constant Gtk_Combo_Box := new Gtk_Combo_Box_Record;
   begin
      Gtk.Combo_Box.Initialize_With_Area_And_Entry (Combo_Box, Area);
      return Combo_Box;
   end Gtk_Combo_Box_New_With_Area_And_Entry;

   ----------------------------------
   -- Gtk_Combo_Box_New_With_Entry --
   ----------------------------------

   function Gtk_Combo_Box_New_With_Entry return Gtk_Combo_Box is
      Combo_Box : constant Gtk_Combo_Box := new Gtk_Combo_Box_Record;
   begin
      Gtk.Combo_Box.Initialize_With_Entry (Combo_Box);
      return Combo_Box;
   end Gtk_Combo_Box_New_With_Entry;

   ----------------------------------
   -- Gtk_Combo_Box_New_With_Model --
   ----------------------------------

   function Gtk_Combo_Box_New_With_Model
      (Model : Gtk.Tree_Model.Gtk_Tree_Model) return Gtk_Combo_Box
   is
      Combo_Box : constant Gtk_Combo_Box := new Gtk_Combo_Box_Record;
   begin
      Gtk.Combo_Box.Initialize_With_Model (Combo_Box, Model);
      return Combo_Box;
   end Gtk_Combo_Box_New_With_Model;

   --------------------------------------------
   -- Gtk_Combo_Box_New_With_Model_And_Entry --
   --------------------------------------------

   function Gtk_Combo_Box_New_With_Model_And_Entry
      (Model : Gtk.Tree_Model.Gtk_Tree_Model) return Gtk_Combo_Box
   is
      Combo_Box : constant Gtk_Combo_Box := new Gtk_Combo_Box_Record;
   begin
      Gtk.Combo_Box.Initialize_With_Model_And_Entry (Combo_Box, Model);
      return Combo_Box;
   end Gtk_Combo_Box_New_With_Model_And_Entry;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Combo_Box : out Gtk_Combo_Box) is
   begin
      Combo_Box := new Gtk_Combo_Box_Record;
      Gtk.Combo_Box.Initialize (Combo_Box);
   end Gtk_New;

   -----------------------
   -- Gtk_New_With_Area --
   -----------------------

   procedure Gtk_New_With_Area
      (Combo_Box : out Gtk_Combo_Box;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
   is
   begin
      Combo_Box := new Gtk_Combo_Box_Record;
      Gtk.Combo_Box.Initialize_With_Area (Combo_Box, Area);
   end Gtk_New_With_Area;

   ---------------------------------
   -- Gtk_New_With_Area_And_Entry --
   ---------------------------------

   procedure Gtk_New_With_Area_And_Entry
      (Combo_Box : out Gtk_Combo_Box;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
   is
   begin
      Combo_Box := new Gtk_Combo_Box_Record;
      Gtk.Combo_Box.Initialize_With_Area_And_Entry (Combo_Box, Area);
   end Gtk_New_With_Area_And_Entry;

   ------------------------
   -- Gtk_New_With_Entry --
   ------------------------

   procedure Gtk_New_With_Entry (Combo_Box : out Gtk_Combo_Box) is
   begin
      Combo_Box := new Gtk_Combo_Box_Record;
      Gtk.Combo_Box.Initialize_With_Entry (Combo_Box);
   end Gtk_New_With_Entry;

   ------------------------
   -- Gtk_New_With_Model --
   ------------------------

   procedure Gtk_New_With_Model
      (Combo_Box : out Gtk_Combo_Box;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model)
   is
   begin
      Combo_Box := new Gtk_Combo_Box_Record;
      Gtk.Combo_Box.Initialize_With_Model (Combo_Box, Model);
   end Gtk_New_With_Model;

   ----------------------------------
   -- Gtk_New_With_Model_And_Entry --
   ----------------------------------

   procedure Gtk_New_With_Model_And_Entry
      (Combo_Box : out Gtk_Combo_Box;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model)
   is
   begin
      Combo_Box := new Gtk_Combo_Box_Record;
      Gtk.Combo_Box.Initialize_With_Model_And_Entry (Combo_Box, Model);
   end Gtk_New_With_Model_And_Entry;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Combo_Box : not null access Gtk_Combo_Box_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_new");
   begin
      if not Combo_Box.Is_Created then
         Set_Object (Combo_Box, Internal);
      end if;
   end Initialize;

   --------------------------
   -- Initialize_With_Area --
   --------------------------

   procedure Initialize_With_Area
      (Combo_Box : not null access Gtk_Combo_Box_Record'Class;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
   is
      function Internal (Area : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_new_with_area");
   begin
      if not Combo_Box.Is_Created then
         Set_Object (Combo_Box, Internal (Get_Object (Area)));
      end if;
   end Initialize_With_Area;

   ------------------------------------
   -- Initialize_With_Area_And_Entry --
   ------------------------------------

   procedure Initialize_With_Area_And_Entry
      (Combo_Box : not null access Gtk_Combo_Box_Record'Class;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
   is
      function Internal (Area : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_new_with_area_and_entry");
   begin
      if not Combo_Box.Is_Created then
         Set_Object (Combo_Box, Internal (Get_Object (Area)));
      end if;
   end Initialize_With_Area_And_Entry;

   ---------------------------
   -- Initialize_With_Entry --
   ---------------------------

   procedure Initialize_With_Entry
      (Combo_Box : not null access Gtk_Combo_Box_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_new_with_entry");
   begin
      if not Combo_Box.Is_Created then
         Set_Object (Combo_Box, Internal);
      end if;
   end Initialize_With_Entry;

   ---------------------------
   -- Initialize_With_Model --
   ---------------------------

   procedure Initialize_With_Model
      (Combo_Box : not null access Gtk_Combo_Box_Record'Class;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model)
   is
      function Internal
         (Model : Gtk.Tree_Model.Gtk_Tree_Model) return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_new_with_model");
   begin
      if not Combo_Box.Is_Created then
         Set_Object (Combo_Box, Internal (Model));
      end if;
   end Initialize_With_Model;

   -------------------------------------
   -- Initialize_With_Model_And_Entry --
   -------------------------------------

   procedure Initialize_With_Model_And_Entry
      (Combo_Box : not null access Gtk_Combo_Box_Record'Class;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model)
   is
      function Internal
         (Model : Gtk.Tree_Model.Gtk_Tree_Model) return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_new_with_model_and_entry");
   begin
      if not Combo_Box.Is_Created then
         Set_Object (Combo_Box, Internal (Model));
      end if;
   end Initialize_With_Model_And_Entry;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Glib.Gint
   is
      function Internal (Combo_Box : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_combo_box_get_active");
   begin
      return Internal (Get_Object (Combo_Box));
   end Get_Active;

   -------------------
   -- Get_Active_Id --
   -------------------

   function Get_Active_Id
      (Combo_Box : not null access Gtk_Combo_Box_Record) return UTF8_String
   is
      function Internal
         (Combo_Box : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_combo_box_get_active_id");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Combo_Box)));
   end Get_Active_Id;

   ----------------------
   -- Get_Add_Tearoffs --
   ----------------------

   function Get_Add_Tearoffs
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Boolean
   is
      function Internal (Combo_Box : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_combo_box_get_add_tearoffs");
   begin
      return Internal (Get_Object (Combo_Box)) /= 0;
   end Get_Add_Tearoffs;

   ----------------------------
   -- Get_Button_Sensitivity --
   ----------------------------

   function Get_Button_Sensitivity
      (Combo_Box : not null access Gtk_Combo_Box_Record)
       return Gtk.Enums.Gtk_Sensitivity_Type
   is
      function Internal
         (Combo_Box : System.Address) return Gtk.Enums.Gtk_Sensitivity_Type;
      pragma Import (C, Internal, "gtk_combo_box_get_button_sensitivity");
   begin
      return Internal (Get_Object (Combo_Box));
   end Get_Button_Sensitivity;

   ----------------------------
   -- Get_Column_Span_Column --
   ----------------------------

   function Get_Column_Span_Column
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Glib.Gint
   is
      function Internal (Combo_Box : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_combo_box_get_column_span_column");
   begin
      return Internal (Get_Object (Combo_Box));
   end Get_Column_Span_Column;

   ---------------------------
   -- Get_Entry_Text_Column --
   ---------------------------

   function Get_Entry_Text_Column
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Glib.Gint
   is
      function Internal (Combo_Box : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_combo_box_get_entry_text_column");
   begin
      return Internal (Get_Object (Combo_Box));
   end Get_Entry_Text_Column;

   ------------------------
   -- Get_Focus_On_Click --
   ------------------------

   function Get_Focus_On_Click
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Boolean
   is
      function Internal (Combo_Box : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_combo_box_get_focus_on_click");
   begin
      return Internal (Get_Object (Combo_Box)) /= 0;
   end Get_Focus_On_Click;

   -------------------
   -- Get_Has_Entry --
   -------------------

   function Get_Has_Entry
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Boolean
   is
      function Internal (Combo_Box : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_combo_box_get_has_entry");
   begin
      return Internal (Get_Object (Combo_Box)) /= 0;
   end Get_Has_Entry;

   -------------------
   -- Get_Id_Column --
   -------------------

   function Get_Id_Column
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Glib.Gint
   is
      function Internal (Combo_Box : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_combo_box_get_id_column");
   begin
      return Internal (Get_Object (Combo_Box));
   end Get_Id_Column;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Combo_Box : not null access Gtk_Combo_Box_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model
   is
      function Internal
         (Combo_Box : System.Address) return Gtk.Tree_Model.Gtk_Tree_Model;
      pragma Import (C, Internal, "gtk_combo_box_get_model");
   begin
      return Internal (Get_Object (Combo_Box));
   end Get_Model;

   ---------------------------
   -- Get_Popup_Fixed_Width --
   ---------------------------

   function Get_Popup_Fixed_Width
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Boolean
   is
      function Internal (Combo_Box : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_combo_box_get_popup_fixed_width");
   begin
      return Internal (Get_Object (Combo_Box)) /= 0;
   end Get_Popup_Fixed_Width;

   ----------------------------
   -- Get_Row_Separator_Func --
   ----------------------------

   procedure Get_Row_Separator_Func
      (Combo_Box : not null access Gtk_Combo_Box_Record)
   is
      procedure Internal (Combo_Box : System.Address);
      pragma Import (C, Internal, "gtk_combo_box_get_row_separator_func");
   begin
      Internal (Get_Object (Combo_Box));
   end Get_Row_Separator_Func;

   -------------------------
   -- Get_Row_Span_Column --
   -------------------------

   function Get_Row_Span_Column
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Glib.Gint
   is
      function Internal (Combo_Box : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_combo_box_get_row_span_column");
   begin
      return Internal (Get_Object (Combo_Box));
   end Get_Row_Span_Column;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Combo_Box : not null access Gtk_Combo_Box_Record) return UTF8_String
   is
      function Internal
         (Combo_Box : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_combo_box_get_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Combo_Box)));
   end Get_Title;

   --------------------
   -- Get_Wrap_Width --
   --------------------

   function Get_Wrap_Width
      (Combo_Box : not null access Gtk_Combo_Box_Record) return Glib.Gint
   is
      function Internal (Combo_Box : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_combo_box_get_wrap_width");
   begin
      return Internal (Get_Object (Combo_Box));
   end Get_Wrap_Width;

   -------------
   -- Popdown --
   -------------

   procedure Popdown (Combo_Box : not null access Gtk_Combo_Box_Record) is
      procedure Internal (Combo_Box : System.Address);
      pragma Import (C, Internal, "gtk_combo_box_popdown");
   begin
      Internal (Get_Object (Combo_Box));
   end Popdown;

   -----------
   -- Popup --
   -----------

   procedure Popup (Combo_Box : not null access Gtk_Combo_Box_Record) is
      procedure Internal (Combo_Box : System.Address);
      pragma Import (C, Internal, "gtk_combo_box_popup");
   begin
      Internal (Get_Object (Combo_Box));
   end Popup;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Index     : Glib.Gint)
   is
      procedure Internal (Combo_Box : System.Address; Index : Glib.Gint);
      pragma Import (C, Internal, "gtk_combo_box_set_active");
   begin
      Internal (Get_Object (Combo_Box), Index);
   end Set_Active;

   -------------------
   -- Set_Active_Id --
   -------------------

   function Set_Active_Id
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Active_Id : UTF8_String := "") return Boolean
   is
      function Internal
         (Combo_Box : System.Address;
          Active_Id : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_combo_box_set_active_id");
      Tmp_Active_Id : Gtkada.Types.Chars_Ptr;
      Tmp_Return    : Glib.Gboolean;
   begin
      if Active_Id = "" then
         Tmp_Active_Id := Gtkada.Types.Null_Ptr;
      else
         Tmp_Active_Id := New_String (Active_Id);
      end if;
      Tmp_Return := Internal (Get_Object (Combo_Box), Tmp_Active_Id);
      Free (Tmp_Active_Id);
      return Tmp_Return /= 0;
   end Set_Active_Id;

   ---------------------
   -- Set_Active_Iter --
   ---------------------

   procedure Set_Active_Iter
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Iter      : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal (Combo_Box : System.Address; Iter : System.Address);
      pragma Import (C, Internal, "gtk_combo_box_set_active_iter");
   begin
      Internal (Get_Object (Combo_Box), Iter_Or_Null (Iter'Address));
   end Set_Active_Iter;

   ----------------------
   -- Set_Add_Tearoffs --
   ----------------------

   procedure Set_Add_Tearoffs
      (Combo_Box    : not null access Gtk_Combo_Box_Record;
       Add_Tearoffs : Boolean)
   is
      procedure Internal
         (Combo_Box    : System.Address;
          Add_Tearoffs : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_combo_box_set_add_tearoffs");
   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Add_Tearoffs));
   end Set_Add_Tearoffs;

   ----------------------------
   -- Set_Button_Sensitivity --
   ----------------------------

   procedure Set_Button_Sensitivity
      (Combo_Box   : not null access Gtk_Combo_Box_Record;
       Sensitivity : Gtk.Enums.Gtk_Sensitivity_Type)
   is
      procedure Internal
         (Combo_Box   : System.Address;
          Sensitivity : Gtk.Enums.Gtk_Sensitivity_Type);
      pragma Import (C, Internal, "gtk_combo_box_set_button_sensitivity");
   begin
      Internal (Get_Object (Combo_Box), Sensitivity);
   end Set_Button_Sensitivity;

   ------------------------
   -- Set_Cell_Data_Func --
   ------------------------

   procedure Set_Cell_Data_Func
      (Cell_Layout : not null access Gtk_Combo_Box_Record;
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
         (Cell_Layout : not null access Gtk.Combo_Box.Gtk_Combo_Box_Record'Class;
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

   ----------------------------
   -- Set_Column_Span_Column --
   ----------------------------

   procedure Set_Column_Span_Column
      (Combo_Box   : not null access Gtk_Combo_Box_Record;
       Column_Span : Glib.Gint)
   is
      procedure Internal
         (Combo_Box   : System.Address;
          Column_Span : Glib.Gint);
      pragma Import (C, Internal, "gtk_combo_box_set_column_span_column");
   begin
      Internal (Get_Object (Combo_Box), Column_Span);
   end Set_Column_Span_Column;

   ---------------------------
   -- Set_Entry_Text_Column --
   ---------------------------

   procedure Set_Entry_Text_Column
      (Combo_Box   : not null access Gtk_Combo_Box_Record;
       Text_Column : Glib.Gint)
   is
      procedure Internal
         (Combo_Box   : System.Address;
          Text_Column : Glib.Gint);
      pragma Import (C, Internal, "gtk_combo_box_set_entry_text_column");
   begin
      Internal (Get_Object (Combo_Box), Text_Column);
   end Set_Entry_Text_Column;

   ------------------------
   -- Set_Focus_On_Click --
   ------------------------

   procedure Set_Focus_On_Click
      (Combo_Box      : not null access Gtk_Combo_Box_Record;
       Focus_On_Click : Boolean)
   is
      procedure Internal
         (Combo_Box      : System.Address;
          Focus_On_Click : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_combo_box_set_focus_on_click");
   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Focus_On_Click));
   end Set_Focus_On_Click;

   -------------------
   -- Set_Id_Column --
   -------------------

   procedure Set_Id_Column
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Id_Column : Glib.Gint)
   is
      procedure Internal (Combo_Box : System.Address; Id_Column : Glib.Gint);
      pragma Import (C, Internal, "gtk_combo_box_set_id_column");
   begin
      Internal (Get_Object (Combo_Box), Id_Column);
   end Set_Id_Column;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model)
   is
      procedure Internal
         (Combo_Box : System.Address;
          Model     : Gtk.Tree_Model.Gtk_Tree_Model);
      pragma Import (C, Internal, "gtk_combo_box_set_model");
   begin
      Internal (Get_Object (Combo_Box), Model);
   end Set_Model;

   ---------------------------
   -- Set_Popup_Fixed_Width --
   ---------------------------

   procedure Set_Popup_Fixed_Width
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Fixed     : Boolean)
   is
      procedure Internal (Combo_Box : System.Address; Fixed : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_combo_box_set_popup_fixed_width");
   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Fixed));
   end Set_Popup_Fixed_Width;

   ----------------------------
   -- Set_Row_Separator_Func --
   ----------------------------

   procedure Set_Row_Separator_Func
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Func      : Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func)
   is
   begin
      if Func = null then
         C_Gtk_Combo_Box_Set_Row_Separator_Func (Get_Object (Combo_Box), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Combo_Box_Set_Row_Separator_Func (Get_Object (Combo_Box), Internal_Gtk_Tree_View_Row_Separator_Func'Address, To_Address (Func), System.Null_Address);
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
         (Combo_Box : not null access Gtk.Combo_Box.Gtk_Combo_Box_Record'Class;
          Func      : Gtk_Tree_View_Row_Separator_Func;
          Data      : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Combo_Box_Set_Row_Separator_Func (Get_Object (Combo_Box), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_Combo_Box_Set_Row_Separator_Func (Get_Object (Combo_Box), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Row_Separator_Func;

   end Set_Row_Separator_Func_User_Data;

   -------------------------
   -- Set_Row_Span_Column --
   -------------------------

   procedure Set_Row_Span_Column
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Row_Span  : Glib.Gint)
   is
      procedure Internal (Combo_Box : System.Address; Row_Span : Glib.Gint);
      pragma Import (C, Internal, "gtk_combo_box_set_row_span_column");
   begin
      Internal (Get_Object (Combo_Box), Row_Span);
   end Set_Row_Span_Column;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Title     : UTF8_String)
   is
      procedure Internal
         (Combo_Box : System.Address;
          Title     : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_combo_box_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr := New_String (Title);
   begin
      Internal (Get_Object (Combo_Box), Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

   --------------------
   -- Set_Wrap_Width --
   --------------------

   procedure Set_Wrap_Width
      (Combo_Box : not null access Gtk_Combo_Box_Record;
       Width     : Glib.Gint)
   is
      procedure Internal (Combo_Box : System.Address; Width : Glib.Gint);
      pragma Import (C, Internal, "gtk_combo_box_set_wrap_width");
   begin
      Internal (Get_Object (Combo_Box), Width);
   end Set_Wrap_Width;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
      (Cell_Layout : not null access Gtk_Combo_Box_Record;
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

   procedure Clear (Cell_Layout : not null access Gtk_Combo_Box_Record) is
      procedure Internal (Cell_Layout : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear");
   begin
      Internal (Get_Object (Cell_Layout));
   end Clear;

   ----------------------
   -- Clear_Attributes --
   ----------------------

   procedure Clear_Attributes
      (Cell_Layout : not null access Gtk_Combo_Box_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear_attributes");
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell));
   end Clear_Attributes;

   ------------------
   -- Editing_Done --
   ------------------

   procedure Editing_Done
      (Cell_Editable : not null access Gtk_Combo_Box_Record)
   is
      procedure Internal (Cell_Editable : System.Address);
      pragma Import (C, Internal, "gtk_cell_editable_editing_done");
   begin
      Internal (Get_Object (Cell_Editable));
   end Editing_Done;

   ---------------
   -- Get_Cells --
   ---------------

   function Get_Cells
      (Cell_Layout : not null access Gtk_Combo_Box_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist
   is
      function Internal (Cell_Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_layout_get_cells");
      Tmp_Return : Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
   begin
      Gtk.Cell_Renderer.Cell_Renderer_List.Set_Object (Tmp_Return, Internal (Get_Object (Cell_Layout)));
      return Tmp_Return;
   end Get_Cells;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
      (Cell_Layout : not null access Gtk_Combo_Box_Record;
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
      (Cell_Layout : not null access Gtk_Combo_Box_Record;
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

   -------------------
   -- Remove_Widget --
   -------------------

   procedure Remove_Widget
      (Cell_Editable : not null access Gtk_Combo_Box_Record)
   is
      procedure Internal (Cell_Editable : System.Address);
      pragma Import (C, Internal, "gtk_cell_editable_remove_widget");
   begin
      Internal (Get_Object (Cell_Editable));
   end Remove_Widget;

   -------------
   -- Reorder --
   -------------

   procedure Reorder
      (Cell_Layout : not null access Gtk_Combo_Box_Record;
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

   -------------------
   -- Start_Editing --
   -------------------

   procedure Start_Editing
      (Cell_Editable : not null access Gtk_Combo_Box_Record;
       Event         : Gdk.Event.Gdk_Event)
   is
      procedure Internal
         (Cell_Editable : System.Address;
          Event         : Gdk.Event.Gdk_Event);
      pragma Import (C, Internal, "gtk_cell_editable_start_editing");
   begin
      Internal (Get_Object (Cell_Editable), Event);
   end Start_Editing;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Combo_Box_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Combo_Box_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Combo_Box_UTF8_String_UTF8_String, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Combo_Box_UTF8_String_UTF8_String);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_UTF8_String, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_UTF8_String);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Combo_Box_Gtk_Scroll_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Combo_Box_Gtk_Scroll_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Scroll_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Scroll_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Combo_Box_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Combo_Box_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean);

   procedure Connect
      (Object  : access Gtk_Combo_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Combo_Box_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Combo_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Combo_Box_UTF8_String_UTF8_String;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Combo_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Combo_Box_Gtk_Scroll_Type_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Combo_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Combo_Box_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Combo_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Combo_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_UTF8_String;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Combo_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Combo_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean;
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

   procedure Marsh_GObject_Gtk_Scroll_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Scroll_Type_Void);

   procedure Marsh_GObject_UTF8_String_UTF8_String
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_UTF8_String);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Combo_Box_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Combo_Box_Boolean);

   procedure Marsh_Gtk_Combo_Box_Gtk_Scroll_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Combo_Box_Gtk_Scroll_Type_Void);

   procedure Marsh_Gtk_Combo_Box_UTF8_String_UTF8_String
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Combo_Box_UTF8_String_UTF8_String);

   procedure Marsh_Gtk_Combo_Box_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Combo_Box_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Combo_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Combo_Box_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Combo_Box_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Combo_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Combo_Box_UTF8_String_UTF8_String;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Combo_Box_UTF8_String_UTF8_String'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Combo_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Combo_Box_Gtk_Scroll_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Combo_Box_Gtk_Scroll_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Combo_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Combo_Box_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Combo_Box_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Combo_Box_Record'Class;
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
      (Object  : access Gtk_Combo_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_UTF8_String;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_UTF8_String'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Combo_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Scroll_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Combo_Box_Record'Class;
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

   ----------------------------------------
   -- Marsh_GObject_Gtk_Scroll_Type_Void --
   ----------------------------------------

   procedure Marsh_GObject_Gtk_Scroll_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Scroll_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Scroll_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Scroll_Type_Void;

   -------------------------------------------
   -- Marsh_GObject_UTF8_String_UTF8_String --
   -------------------------------------------

   procedure Marsh_GObject_UTF8_String_UTF8_String
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_UTF8_String := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased UTF8_String := H (Obj, Unchecked_To_UTF8_String (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_UTF8_String;

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
   -- Marsh_Gtk_Combo_Box_Boolean --
   ---------------------------------

   procedure Marsh_Gtk_Combo_Box_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Combo_Box_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Combo_Box := Gtk_Combo_Box (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Combo_Box_Boolean;

   ----------------------------------------------
   -- Marsh_Gtk_Combo_Box_Gtk_Scroll_Type_Void --
   ----------------------------------------------

   procedure Marsh_Gtk_Combo_Box_Gtk_Scroll_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Combo_Box_Gtk_Scroll_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Combo_Box := Gtk_Combo_Box (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Scroll_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Combo_Box_Gtk_Scroll_Type_Void;

   -------------------------------------------------
   -- Marsh_Gtk_Combo_Box_UTF8_String_UTF8_String --
   -------------------------------------------------

   procedure Marsh_Gtk_Combo_Box_UTF8_String_UTF8_String
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Combo_Box_UTF8_String_UTF8_String := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Combo_Box := Gtk_Combo_Box (Unchecked_To_Object (Params, 0));
      V   : aliased UTF8_String := H (Obj, Unchecked_To_UTF8_String (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Combo_Box_UTF8_String_UTF8_String;

   ------------------------------
   -- Marsh_Gtk_Combo_Box_Void --
   ------------------------------

   procedure Marsh_Gtk_Combo_Box_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Combo_Box_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Combo_Box := Gtk_Combo_Box (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Combo_Box_Void;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_Gtk_Combo_Box_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

   --------------------------
   -- On_Format_Entry_Text --
   --------------------------

   procedure On_Format_Entry_Text
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_Gtk_Combo_Box_UTF8_String_UTF8_String;
       After : Boolean := False)
   is
   begin
      Connect (Self, "format-entry-text" & ASCII.NUL, Call, After);
   end On_Format_Entry_Text;

   --------------------------
   -- On_Format_Entry_Text --
   --------------------------

   procedure On_Format_Entry_Text
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_GObject_UTF8_String_UTF8_String;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "format-entry-text" & ASCII.NUL, Call, After, Slot);
   end On_Format_Entry_Text;

   --------------------
   -- On_Move_Active --
   --------------------

   procedure On_Move_Active
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_Gtk_Combo_Box_Gtk_Scroll_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-active" & ASCII.NUL, Call, After);
   end On_Move_Active;

   --------------------
   -- On_Move_Active --
   --------------------

   procedure On_Move_Active
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_GObject_Gtk_Scroll_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-active" & ASCII.NUL, Call, After, Slot);
   end On_Move_Active;

   ----------------
   -- On_Popdown --
   ----------------

   procedure On_Popdown
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_Gtk_Combo_Box_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "popdown" & ASCII.NUL, Call, After);
   end On_Popdown;

   ----------------
   -- On_Popdown --
   ----------------

   procedure On_Popdown
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "popdown" & ASCII.NUL, Call, After, Slot);
   end On_Popdown;

   --------------
   -- On_Popup --
   --------------

   procedure On_Popup
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_Gtk_Combo_Box_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "popup" & ASCII.NUL, Call, After);
   end On_Popup;

   --------------
   -- On_Popup --
   --------------

   procedure On_Popup
      (Self  : not null access Gtk_Combo_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "popup" & ASCII.NUL, Call, After, Slot);
   end On_Popup;

end Gtk.Combo_Box;
