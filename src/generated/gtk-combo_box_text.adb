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
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Combo_Box_Text is

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

   package Type_Conversion_Gtk_Combo_Box_Text is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Combo_Box_Text_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Combo_Box_Text);

   ----------------------------
   -- Gtk_Combo_Box_Text_New --
   ----------------------------

   function Gtk_Combo_Box_Text_New return Gtk_Combo_Box_Text is
      Self : constant Gtk_Combo_Box_Text := new Gtk_Combo_Box_Text_Record;
   begin
      Gtk.Combo_Box_Text.Initialize (Self);
      return Self;
   end Gtk_Combo_Box_Text_New;

   ---------------------------------------
   -- Gtk_Combo_Box_Text_New_With_Entry --
   ---------------------------------------

   function Gtk_Combo_Box_Text_New_With_Entry return Gtk_Combo_Box_Text is
      Self : constant Gtk_Combo_Box_Text := new Gtk_Combo_Box_Text_Record;
   begin
      Gtk.Combo_Box_Text.Initialize_With_Entry (Self);
      return Self;
   end Gtk_Combo_Box_Text_New_With_Entry;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Combo_Box_Text) is
   begin
      Self := new Gtk_Combo_Box_Text_Record;
      Gtk.Combo_Box_Text.Initialize (Self);
   end Gtk_New;

   ------------------------
   -- Gtk_New_With_Entry --
   ------------------------

   procedure Gtk_New_With_Entry (Self : out Gtk_Combo_Box_Text) is
   begin
      Self := new Gtk_Combo_Box_Text_Record;
      Gtk.Combo_Box_Text.Initialize_With_Entry (Self);
   end Gtk_New_With_Entry;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Combo_Box_Text_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_text_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_With_Entry --
   ---------------------------

   procedure Initialize_With_Entry
      (Self : not null access Gtk_Combo_Box_Text_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_text_new_with_entry");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize_With_Entry;

   ------------
   -- Append --
   ------------

   procedure Append
      (Self : not null access Gtk_Combo_Box_Text_Record;
       Id   : UTF8_String := "";
       Text : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Id   : Gtkada.Types.Chars_Ptr;
          Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_combo_box_text_append");
      Tmp_Id   : Gtkada.Types.Chars_Ptr;
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      if Id = "" then
         Tmp_Id := Gtkada.Types.Null_Ptr;
      else
         Tmp_Id := New_String (Id);
      end if;
      Internal (Get_Object (Self), Tmp_Id, Tmp_Text);
      Free (Tmp_Text);
      Free (Tmp_Id);
   end Append;

   -----------------
   -- Append_Text --
   -----------------

   procedure Append_Text
      (Self : not null access Gtk_Combo_Box_Text_Record;
       Text : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_combo_box_text_append_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Self), Tmp_Text);
      Free (Tmp_Text);
   end Append_Text;

   ---------------------
   -- Get_Active_Text --
   ---------------------

   function Get_Active_Text
      (Self : not null access Gtk_Combo_Box_Text_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_combo_box_text_get_active_text");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Active_Text;

   ------------
   -- Insert --
   ------------

   procedure Insert
      (Self     : not null access Gtk_Combo_Box_Text_Record;
       Position : Glib.Gint;
       Id       : UTF8_String := "";
       Text     : UTF8_String)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Glib.Gint;
          Id       : Gtkada.Types.Chars_Ptr;
          Text     : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_combo_box_text_insert");
      Tmp_Id   : Gtkada.Types.Chars_Ptr;
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      if Id = "" then
         Tmp_Id := Gtkada.Types.Null_Ptr;
      else
         Tmp_Id := New_String (Id);
      end if;
      Internal (Get_Object (Self), Position, Tmp_Id, Tmp_Text);
      Free (Tmp_Text);
      Free (Tmp_Id);
   end Insert;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
      (Self     : not null access Gtk_Combo_Box_Text_Record;
       Position : Glib.Gint;
       Text     : UTF8_String)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Glib.Gint;
          Text     : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_combo_box_text_insert_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Self), Position, Tmp_Text);
      Free (Tmp_Text);
   end Insert_Text;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
      (Self : not null access Gtk_Combo_Box_Text_Record;
       Id   : UTF8_String := "";
       Text : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Id   : Gtkada.Types.Chars_Ptr;
          Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_combo_box_text_prepend");
      Tmp_Id   : Gtkada.Types.Chars_Ptr;
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      if Id = "" then
         Tmp_Id := Gtkada.Types.Null_Ptr;
      else
         Tmp_Id := New_String (Id);
      end if;
      Internal (Get_Object (Self), Tmp_Id, Tmp_Text);
      Free (Tmp_Text);
      Free (Tmp_Id);
   end Prepend;

   ------------------
   -- Prepend_Text --
   ------------------

   procedure Prepend_Text
      (Self : not null access Gtk_Combo_Box_Text_Record;
       Text : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_combo_box_text_prepend_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Self), Tmp_Text);
      Free (Tmp_Text);
   end Prepend_Text;

   ------------
   -- Remove --
   ------------

   procedure Remove
      (Self     : not null access Gtk_Combo_Box_Text_Record;
       Position : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_combo_box_text_remove");
   begin
      Internal (Get_Object (Self), Position);
   end Remove;

   ----------------
   -- Remove_All --
   ----------------

   procedure Remove_All (Self : not null access Gtk_Combo_Box_Text_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_combo_box_text_remove_all");
   begin
      Internal (Get_Object (Self));
   end Remove_All;

   ------------------------
   -- Set_Cell_Data_Func --
   ------------------------

   procedure Set_Cell_Data_Func
      (Cell_Layout : not null access Gtk_Combo_Box_Text_Record;
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
         (Cell_Layout : not null access Gtk.Combo_Box_Text.Gtk_Combo_Box_Text_Record'Class;
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

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
      (Cell_Layout : not null access Gtk_Combo_Box_Text_Record;
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

   procedure Clear (Cell_Layout : not null access Gtk_Combo_Box_Text_Record) is
      procedure Internal (Cell_Layout : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear");
   begin
      Internal (Get_Object (Cell_Layout));
   end Clear;

   ----------------------
   -- Clear_Attributes --
   ----------------------

   procedure Clear_Attributes
      (Cell_Layout : not null access Gtk_Combo_Box_Text_Record;
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
      (Cell_Editable : not null access Gtk_Combo_Box_Text_Record)
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
      (Cell_Layout : not null access Gtk_Combo_Box_Text_Record)
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
      (Cell_Layout : not null access Gtk_Combo_Box_Text_Record;
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
      (Cell_Layout : not null access Gtk_Combo_Box_Text_Record;
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
      (Cell_Editable : not null access Gtk_Combo_Box_Text_Record)
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
      (Cell_Layout : not null access Gtk_Combo_Box_Text_Record;
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
      (Cell_Editable : not null access Gtk_Combo_Box_Text_Record;
       Event         : Gdk.Event.Gdk_Event)
   is
      procedure Internal
         (Cell_Editable : System.Address;
          Event         : Gdk.Event.Gdk_Event);
      pragma Import (C, Internal, "gtk_cell_editable_start_editing");
   begin
      Internal (Get_Object (Cell_Editable), Event);
   end Start_Editing;

end Gtk.Combo_Box_Text;
