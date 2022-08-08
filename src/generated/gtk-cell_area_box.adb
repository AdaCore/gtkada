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
with Glib.Object;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Cell_Area_Box is

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

   package Type_Conversion_Gtk_Cell_Area_Box is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_Area_Box_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Cell_Area_Box);

   ---------------------------
   -- Gtk_Cell_Area_Box_New --
   ---------------------------

   function Gtk_Cell_Area_Box_New return Gtk_Cell_Area_Box is
      Self : constant Gtk_Cell_Area_Box := new Gtk_Cell_Area_Box_Record;
   begin
      Gtk.Cell_Area_Box.Initialize (Self);
      return Self;
   end Gtk_Cell_Area_Box_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Cell_Area_Box) is
   begin
      Self := new Gtk_Cell_Area_Box_Record;
      Gtk.Cell_Area_Box.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Cell_Area_Box_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_cell_area_box_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing
      (Self : not null access Gtk_Cell_Area_Box_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_cell_area_box_get_spacing");
   begin
      return Internal (Get_Object (Self));
   end Get_Spacing;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
      (Self     : not null access Gtk_Cell_Area_Box_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand   : Boolean;
       Align    : Boolean;
       Fixed    : Boolean)
   is
      procedure Internal
         (Self     : System.Address;
          Renderer : System.Address;
          Expand   : Glib.Gboolean;
          Align    : Glib.Gboolean;
          Fixed    : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_area_box_pack_end");
   begin
      Internal (Get_Object (Self), Get_Object (Renderer), Boolean'Pos (Expand), Boolean'Pos (Align), Boolean'Pos (Fixed));
   end Pack_End;

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
      (Self     : not null access Gtk_Cell_Area_Box_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand   : Boolean;
       Align    : Boolean;
       Fixed    : Boolean)
   is
      procedure Internal
         (Self     : System.Address;
          Renderer : System.Address;
          Expand   : Glib.Gboolean;
          Align    : Glib.Gboolean;
          Fixed    : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_area_box_pack_start");
   begin
      Internal (Get_Object (Self), Get_Object (Renderer), Boolean'Pos (Expand), Boolean'Pos (Align), Boolean'Pos (Fixed));
   end Pack_Start;

   ------------------------
   -- Set_Cell_Data_Func --
   ------------------------

   procedure Set_Cell_Data_Func
      (Cell_Layout : not null access Gtk_Cell_Area_Box_Record;
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
         (Cell_Layout : not null access Gtk.Cell_Area_Box.Gtk_Cell_Area_Box_Record'Class;
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

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing
      (Self    : not null access Gtk_Cell_Area_Box_Record;
       Spacing : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Spacing : Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_area_box_set_spacing");
   begin
      Internal (Get_Object (Self), Spacing);
   end Set_Spacing;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
      (Cell_Layout : not null access Gtk_Cell_Area_Box_Record;
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

   procedure Clear (Cell_Layout : not null access Gtk_Cell_Area_Box_Record) is
      procedure Internal (Cell_Layout : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear");
   begin
      Internal (Get_Object (Cell_Layout));
   end Clear;

   ----------------------
   -- Clear_Attributes --
   ----------------------

   procedure Clear_Attributes
      (Cell_Layout : not null access Gtk_Cell_Area_Box_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear_attributes");
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell));
   end Clear_Attributes;

   ---------------
   -- Get_Cells --
   ---------------

   function Get_Cells
      (Cell_Layout : not null access Gtk_Cell_Area_Box_Record)
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
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Cell_Area_Box_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
      (Cell_Layout : not null access Gtk_Cell_Area_Box_Record;
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
      (Cell_Layout : not null access Gtk_Cell_Area_Box_Record;
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
      (Cell_Layout : not null access Gtk_Cell_Area_Box_Record;
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
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Cell_Area_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

end Gtk.Cell_Area_Box;
