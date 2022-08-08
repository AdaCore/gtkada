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

package body Gtk.Tree_Drag_Dest is

   ------------------------
   -- Drag_Data_Received --
   ------------------------

   function Drag_Data_Received
      (Self           : Gtk_Tree_Drag_Dest;
       Dest           : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean
   is
      function Internal
         (Self           : Gtk_Tree_Drag_Dest;
          Dest           : System.Address;
          Selection_Data : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_dest_drag_data_received");
   begin
      return Internal (Self, Get_Object (Dest), Get_Object (Selection_Data)) /= 0;
   end Drag_Data_Received;

   -----------------------
   -- Row_Drop_Possible --
   -----------------------

   function Row_Drop_Possible
      (Self           : Gtk_Tree_Drag_Dest;
       Dest_Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean
   is
      function Internal
         (Self           : Gtk_Tree_Drag_Dest;
          Dest_Path      : System.Address;
          Selection_Data : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_dest_row_drop_possible");
   begin
      return Internal (Self, Get_Object (Dest_Path), Get_Object (Selection_Data)) /= 0;
   end Row_Drop_Possible;

   function "+" (W : Gtk_Tree_Drag_Dest) return Gtk_Tree_Drag_Dest is
   begin
      return W;
   end "+";

end Gtk.Tree_Drag_Dest;
