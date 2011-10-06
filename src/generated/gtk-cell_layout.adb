-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2011, AdaCore                   --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;     use Interfaces.C.Strings;

package body Gtk.Cell_Layout is

   function To_Cell_Data_Func is new Ada.Unchecked_Conversion
     (System.Address, Cell_Data_Func);

   procedure C_Gtk_Cell_Layout_Set_Cell_Data_Func
      (Cell_Layout : Gtk_Cell_Layout;
       Cell        : System.Address;
       Func        : System.Address;
       Func_Data   : System.Address;
       Destroy     : System.Address);
   pragma Import (C, C_Gtk_Cell_Layout_Set_Cell_Data_Func, "gtk_cell_layout_set_cell_data_func");
   --  Sets the Gtk.Cell_Layout.Cell_Data_Func to use for Cell_Layout. This
   --  function is used instead of the standard attributes mapping for setting
   --  the column value, and should set the value of Cell_Layout's cell
   --  renderer(s) as appropriate. Func may be null to remove and older one.
   --  Since: gtk+ 2.4
   --  "cell": A Gtk.Cell_Renderer.Gtk_Cell_Renderer.
   --  "func": The Gtk.Cell_Layout.Cell_Data_Func to use.
   --  "func_data": The user data for Func.
   --  "destroy": The destroy notification for Func_Data.

   procedure Internal_Cell_Data_Func
      (Cell_Layout : Gtk_Cell_Layout;
       Cell        : System.Address;
       Tree_Model  : System.Address;
       Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
       Data        : System.Address);
   pragma Convention (C, Internal_Cell_Data_Func);

   -----------------------------
   -- Internal_Cell_Data_Func --
   -----------------------------

   procedure Internal_Cell_Data_Func
      (Cell_Layout : Gtk_Cell_Layout;
       Cell        : System.Address;
       Tree_Model  : System.Address;
       Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
       Data        : System.Address)
   is
      Func                   : constant Cell_Data_Func := To_Cell_Data_Func (Data);
      Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
      Stub_Gtk_Tree_Model    : Gtk.Tree_Model.Gtk_Tree_Model_Record;
   begin
      Func (Cell_Layout, Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Cell, Stub_Gtk_Cell_Renderer)), Gtk.Tree_Model.Gtk_Tree_Model (Get_User_Data (Tree_Model, Stub_Gtk_Tree_Model)), Iter);
   end Internal_Cell_Data_Func;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
      (Cell_Layout : Gtk_Cell_Layout;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute   : UTF8_String;
       Column      : Gint)
   is
      procedure Internal
         (Cell_Layout : Gtk_Cell_Layout;
          Cell        : System.Address;
          Attribute   : Interfaces.C.Strings.chars_ptr;
          Column      : Gint);
      pragma Import (C, Internal, "gtk_cell_layout_add_attribute");
      Tmp_Attribute : Interfaces.C.Strings.chars_ptr := New_String (Attribute);
   begin
      Internal (Cell_Layout, Get_Object (Cell), Tmp_Attribute, Column);
      Free (Tmp_Attribute);
   end Add_Attribute;

   ----------------------
   -- Clear_Attributes --
   ----------------------

   procedure Clear_Attributes
      (Cell_Layout : Gtk_Cell_Layout;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
      
   is
      procedure Internal
         (Cell_Layout : Gtk_Cell_Layout;
          Cell        : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear_attributes");
   begin
      Internal (Cell_Layout, Get_Object (Cell));
   end Clear_Attributes;

   ---------------
   -- Get_Cells --
   ---------------

   function Get_Cells
      (Cell_Layout : Gtk_Cell_Layout)
       return Glib.Object.Object_Simple_List.GList
   is
      function Internal
         (Cell_Layout : Gtk_Cell_Layout) return System.Address;
      pragma Import (C, Internal, "gtk_cell_layout_get_cells");
      Tmp_Return : Glib.Object.Object_Simple_List.GList;
   begin
      Glib.Object.Object_Simple_List.Set_Object (Tmp_Return, Internal (Cell_Layout));
      return Tmp_Return;
   end Get_Cells;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
      (Cell_Layout : Gtk_Cell_Layout;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean)
   is
      procedure Internal
         (Cell_Layout : Gtk_Cell_Layout;
          Cell        : System.Address;
          Expand      : Integer);
      pragma Import (C, Internal, "gtk_cell_layout_pack_end");
   begin
      Internal (Cell_Layout, Get_Object (Cell), Boolean'Pos (Expand));
   end Pack_End;

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
      (Cell_Layout : Gtk_Cell_Layout;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean)
   is
      procedure Internal
         (Cell_Layout : Gtk_Cell_Layout;
          Cell        : System.Address;
          Expand      : Integer);
      pragma Import (C, Internal, "gtk_cell_layout_pack_start");
   begin
      Internal (Cell_Layout, Get_Object (Cell), Boolean'Pos (Expand));
   end Pack_Start;

   -------------
   -- Reorder --
   -------------

   procedure Reorder
      (Cell_Layout : Gtk_Cell_Layout;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position    : Gint)
   is
      procedure Internal
         (Cell_Layout : Gtk_Cell_Layout;
          Cell        : System.Address;
          Position    : Gint);
      pragma Import (C, Internal, "gtk_cell_layout_reorder");
   begin
      Internal (Cell_Layout, Get_Object (Cell), Position);
   end Reorder;

   ------------------------
   -- Set_Cell_Data_Func --
   ------------------------

   procedure Set_Cell_Data_Func
      (Cell_Layout : Gtk_Cell_Layout;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func        : Cell_Data_Func)
   is
   begin
      C_Gtk_Cell_Layout_Set_Cell_Data_Func (Cell_Layout, Get_Object (Cell), Internal_Cell_Data_Func'Address, Func'Address, System.Null_Address);
   end Set_Cell_Data_Func;

   package body Set_Cell_Data_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);
      function To_Cell_Data_Func is new Ada.Unchecked_Conversion
        (System.Address, Cell_Data_Func);

      procedure Internal_Cb
         (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
          Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Tree_Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
          Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
          Data        : System.Address);

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
          Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Tree_Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
          Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
          Data        : System.Address)
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         To_Cell_Data_Func (D.Func) (Cell_Layout, Cell, Tree_Model, Iter, D.Data.all);
      end Internal_Cb;

      ------------------------
      -- Set_Cell_Data_Func --
      ------------------------

      procedure Set_Cell_Data_Func
         (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
          Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func        : Cell_Data_Func;
          Func_Data   : User_Data_Type)
      is
      begin
         C_Gtk_Cell_Layout_Set_Cell_Data_Func (Cell_Layout, Get_Object (Cell), Internal_Cb'Address, Users.Build (Func'Address, Func_Data), Users.Free_Data'Address);
      end Set_Cell_Data_Func;

   end Set_Cell_Data_Func_User_Data;

end Gtk.Cell_Layout;
