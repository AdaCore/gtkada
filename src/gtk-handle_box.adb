-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
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

with System;
with Gdk; use Gdk;

package body Gtk.Handle_Box is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Handle_Box : out Gtk_Handle_Box)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_handle_box_new");
   begin
      Set_Object (Handle_Box, Internal);
   end Gtk_New;

   -------------------------
   -- Set_Handle_Position --
   -------------------------

   procedure Set_Handle_Position
     (Handle_Box : in Gtk_Handle_Box;
      Position   : in Enums.Gtk_Position_Type)
   is
      procedure Internal (Handle_Box : System.Address;
                          Position   : Gint);
      pragma Import (C, Internal, "gtk_handle_box_set_handle_position");
   begin
      Internal (Get_Object (Handle_Box),
                Enums.Gtk_Position_Type'Pos (Position));
   end Set_Handle_Position;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
     (Handle_Box : in Gtk_Handle_Box;
      Typ        : in Enums.Gtk_Shadow_Type)
   is
      procedure Internal (Handle_Box : System.Address;
                          Typ        : Gint);
      pragma Import (C, Internal, "gtk_handle_box_set_shadow_type");
   begin
      Internal (Get_Object (Handle_Box),
                Enums.Gtk_Shadow_Type'Pos (Typ));
   end Set_Shadow_Type;

   -------------------
   -- Set_Snap_Edge --
   -------------------

   procedure Set_Snap_Edge
     (Handle_Box : in Gtk_Handle_Box;
      Edge       : in Enums.Gtk_Position_Type)
   is
      procedure Internal (Handle_Box : System.Address;
                          Edgde      : Gint);
      pragma Import (C, Internal, "gtk_handle_box_set_snap_edge");
   begin
      Internal (Get_Object (Handle_Box),
                Enums.Gtk_Position_Type'Pos (Edge));
   end Set_Snap_Edge;

end Gtk.Handle_Box;
