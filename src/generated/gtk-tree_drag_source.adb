------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

package body Gtk.Tree_Drag_Source is

   ----------------------
   -- Drag_Data_Delete --
   ----------------------

   function Drag_Data_Delete
      (Self : Gtk_Tree_Drag_Source;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      function Internal
         (Self : Gtk_Tree_Drag_Source;
          Path : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_tree_drag_source_drag_data_delete");
   begin
      return Boolean'Val (Internal (Self, Get_Object (Path)));
   end Drag_Data_Delete;

   -------------------
   -- Drag_Data_Get --
   -------------------

   function Drag_Data_Get
      (Self           : Gtk_Tree_Drag_Source;
       Path           : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : access Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean
   is
      function Internal
         (Self               : Gtk_Tree_Drag_Source;
          Path               : System.Address;
          Acc_Selection_Data : access System.Address) return Integer;
      pragma Import (C, Internal, "gtk_tree_drag_source_drag_data_get");
      Acc_Selection_Data     : aliased Gtk.Selection_Data.Gtk_Selection_Data;
      Tmp_Acc_Selection_Data : aliased System.Address;
      Tmp_Return             : Integer;
   begin
      Tmp_Return := Internal (Self, Get_Object (Path), Tmp_Acc_Selection_Data'Access);
      Acc_Selection_Data := From_Object (Tmp_Acc_Selection_Data);
      Selection_Data.all := Acc_Selection_Data;
      return Boolean'Val (Tmp_Return);
   end Drag_Data_Get;

   -------------------
   -- Row_Draggable --
   -------------------

   function Row_Draggable
      (Self : Gtk_Tree_Drag_Source;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      function Internal
         (Self : Gtk_Tree_Drag_Source;
          Path : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_tree_drag_source_row_draggable");
   begin
      return Boolean'Val (Internal (Self, Get_Object (Path)));
   end Row_Draggable;

   -----------------------
   -- Get_Row_Drag_Data --
   -----------------------

   procedure Get_Row_Drag_Data
      (Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data;
       Tree_Model     : out Gtk.Tree_Model.Gtk_Tree_Model;
       Path           : out Gtk.Tree_Model.Gtk_Tree_Path;
       succes         : out Boolean)
   is
      function Internal
         (Selection_Data : System.Address;
          Acc_Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model;
          Acc_Path       : access System.Address) return Integer;
      pragma Import (C, Internal, "gtk_tree_get_row_drag_data");
      Acc_Tree_Model : aliased Gtk.Tree_Model.Gtk_Tree_Model;
      Acc_Path       : aliased Gtk.Tree_Model.Gtk_Tree_Path;
      Tmp_Acc_Path   : aliased System.Address;
      Tmp_Return     : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Selection_Data), Acc_Tree_Model'Access, Tmp_Acc_Path'Access);
      Acc_Path := From_Object (Tmp_Acc_Path);
      Tree_Model := Acc_Tree_Model;
      Path := Acc_Path;
      succes := Boolean'Val (Tmp_Return);
   end Get_Row_Drag_Data;

   -----------------------
   -- Set_Row_Drag_Data --
   -----------------------

   function Set_Row_Drag_Data
      (Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data;
       Tree_Model     : Gtk.Tree_Model.Gtk_Tree_Model;
       Path           : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      function Internal
         (Selection_Data : System.Address;
          Tree_Model     : Gtk.Tree_Model.Gtk_Tree_Model;
          Path           : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_tree_set_row_drag_data");
   begin
      return Boolean'Val (Internal (Get_Object (Selection_Data), Tree_Model, Get_Object (Path)));
   end Set_Row_Drag_Data;

end Gtk.Tree_Drag_Source;
