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

package body Gtk.Tree_Drag_Dest is

   ------------------------
   -- Drag_Data_Received --
   ------------------------

   function Drag_Data_Received
      (Self  : Gtk_Tree_Drag_Dest;
       Dest  : Gtk.Tree_Model.Gtk_Tree_Path;
       Value : in out Glib.Values.GValue) return Boolean
   is
      function Internal
         (Self      : Gtk_Tree_Drag_Dest;
          Dest      : System.Address;
          Acc_Value : access Glib.Values.GValue) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_dest_drag_data_received");
      Acc_Value  : aliased Glib.Values.GValue := Value;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Get_Object (Dest), Acc_Value'Access);
      Value := Acc_Value;
      return Tmp_Return /= 0;
   end Drag_Data_Received;

   -----------------------
   -- Row_Drop_Possible --
   -----------------------

   function Row_Drop_Possible
      (Self      : Gtk_Tree_Drag_Dest;
       Dest_Path : Gtk.Tree_Model.Gtk_Tree_Path;
       Value     : in out Glib.Values.GValue) return Boolean
   is
      function Internal
         (Self      : Gtk_Tree_Drag_Dest;
          Dest_Path : System.Address;
          Acc_Value : access Glib.Values.GValue) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_dest_row_drop_possible");
      Acc_Value  : aliased Glib.Values.GValue := Value;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Get_Object (Dest_Path), Acc_Value'Access);
      Value := Acc_Value;
      return Tmp_Return /= 0;
   end Row_Drop_Possible;

   function "+" (W : Gtk_Tree_Drag_Dest) return Gtk_Tree_Drag_Dest is
   begin
      return W;
   end "+";

end Gtk.Tree_Drag_Dest;
