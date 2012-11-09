------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with Gtk.Tree_Model;  use Gtk.Tree_Model;
with Gtk.Selection_Data; use Gtk.Selection_Data;

package body Gtk.Tree_Dnd is

   ----------------------------------
   -- Drag_Dest_Drag_Data_Received --
   ----------------------------------

   function Drag_Dest_Drag_Data_Received
     (Drag_Dest      : Gtk_Tree_Drag_Dest;
      Dest           : Gtk_Tree_Path;
      Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
      return Boolean
   is
      function Internal
        (Drag_Dest      : Gtk_Tree_Drag_Dest;
         Dest           : Gtk_Tree_Path;
         Selection_Data : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_dest_drag_data_received");
   begin
      return Boolean'Val
        (Internal (Drag_Dest, Dest, Get_Object (Selection_Data)));
   end Drag_Dest_Drag_Data_Received;

   ---------------------------------
   -- Drag_Dest_Row_Drop_Possible --
   ---------------------------------

   function Drag_Dest_Row_Drop_Possible
     (Drag_Dest      : Gtk_Tree_Drag_Dest;
      Dest_Path      : Gtk_Tree_Path;
      Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
      return Boolean
   is
      function Internal
        (Drag_Dest      : Gtk_Tree_Drag_Dest;
         Dest_Path      : Gtk_Tree_Path;
         Selection_Data : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_dest_row_drop_possible");
   begin
      return Boolean'Val
        (Internal (Drag_Dest, Dest_Path, Get_Object (Selection_Data)));
   end Drag_Dest_Row_Drop_Possible;

   ----------------------------------
   -- Drag_Source_Drag_Data_Delete --
   ----------------------------------

   function Drag_Source_Drag_Data_Delete
     (Drag_Source : Gtk_Tree_Drag_Source;
      Path        : Gtk_Tree_Path)
      return Boolean
   is
      function Internal
        (Drag_Source : Gtk_Tree_Drag_Source;
         Path        : Gtk_Tree_Path)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_source_drag_data_delete");
   begin
      return Boolean'Val (Internal (Drag_Source, Path));
   end Drag_Source_Drag_Data_Delete;

   -------------------------------
   -- Drag_Source_Drag_Data_Get --
   -------------------------------

   function Drag_Source_Drag_Data_Get
     (Drag_Source    : Gtk_Tree_Drag_Source;
      Path           : Gtk_Tree_Path;
      Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
      return Boolean
   is
      function Internal
        (Drag_Source    : Gtk_Tree_Drag_Source;
         Path           : Gtk_Tree_Path;
         Selection_Data : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_source_drag_data_get");
   begin
      return Boolean'Val
        (Internal (Drag_Source, Path, Get_Object (Selection_Data)));
   end Drag_Source_Drag_Data_Get;

   -------------------------------
   -- Drag_Source_Row_Draggable --
   -------------------------------

   function Drag_Source_Row_Draggable
     (Drag_Source : Gtk_Tree_Drag_Source;
      Path        : Gtk_Tree_Path)
      return Boolean
   is
      function Internal
        (Drag_Source : Gtk_Tree_Drag_Source;
         Path        : Gtk_Tree_Path)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_source_row_draggable");
   begin
      return Boolean'Val (Internal (Drag_Source, Path));
   end Drag_Source_Row_Draggable;

   -----------------------
   -- Get_Row_Drag_Data --
   -----------------------

   procedure Get_Row_Drag_Data
     (Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data;
      Tree_Model     : out Gtk.Tree_Model.Gtk_Tree_Model;
      Path           : out Gtk.Tree_Model.Gtk_Tree_Path;
      Success        : out Boolean)
   is
      function Internal
        (Selection_Data : System.Address;
         Model          : access System.Address;
         Path           : access Gtk_Tree_Path) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_get_row_drag_data");
      M : aliased System.Address;
      P : aliased Gtk_Tree_Path;
      Stub : Gtk_Tree_Model_Record;
   begin
      Success := Boolean'Val
        (Internal (Get_Object (Selection_Data),
         M'Unchecked_Access, P'Unchecked_Access));
      Path := P;
      Tree_Model := Gtk_Tree_Model (Get_User_Data (M, Stub));
   end Get_Row_Drag_Data;

   -----------------------
   -- Set_Row_Drag_Data --
   -----------------------

   function Set_Row_Drag_Data
     (Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data;
      Tree_Model     : access Gtk_Tree_Model_Record'Class;
      Path           : Gtk_Tree_Path)
      return Boolean
   is
      function Internal
        (Selection_Data : System.Address;
         Tree_Model     : System.Address;
         Path           : Gtk_Tree_Path)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_set_row_drag_data");
   begin
      return Boolean'Val
        (Internal
           (Get_Object (Selection_Data), Get_Object (Tree_Model), Path));
   end Set_Row_Drag_Data;

end Gtk.Tree_Dnd;
