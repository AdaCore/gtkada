-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
with Gtk.Util; use Gtk.Util;

package body Gtk.Handle_Box is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Handle_Box : out Gtk_Handle_Box)
   is
   begin
      Handle_Box := new Gtk_Handle_Box_Record;
      Initialize (Handle_Box);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Handle_Box : access Gtk_Handle_Box_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_handle_box_new");
   begin
      Set_Object (Handle_Box, Internal);
      Initialize_User_Data (Handle_Box);
   end Initialize;

   -------------------------
   -- Set_Handle_Position --
   -------------------------

   procedure Set_Handle_Position
     (Handle_Box : access Gtk_Handle_Box_Record;
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
     (Handle_Box : access Gtk_Handle_Box_Record;
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
     (Handle_Box : access Gtk_Handle_Box_Record;
      Edge       : in Enums.Gtk_Position_Type)
   is
      procedure Internal (Handle_Box : System.Address;
                          Edgde      : Gint);
      pragma Import (C, Internal, "gtk_handle_box_set_snap_edge");
   begin
      Internal (Get_Object (Handle_Box),
                Enums.Gtk_Position_Type'Pos (Edge));
   end Set_Snap_Edge;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
   begin
      Gen_New (N, "Handle_Box", File => File);
      Bin.Generate (N, File);
      Gen_Set (N, "Handle_Box", "shadow_type", File);
      Gen_Set (N, "Handle_Box", "handle_position", File);
      Gen_Set (N, "Handle_Box", "snap_edge", File);
   end Generate;

   procedure Generate
     (Handle_Box : in out Object.Gtk_Object; N : in Node_Ptr)
   is
      S : String_Ptr;
   begin
      if not N.Specific_Data.Created then
         Gtk_New (Gtk_Handle_Box (Handle_Box));
         Set_Object (Get_Field (N, "name"), Handle_Box);
         N.Specific_Data.Created := True;
      end if;

      Bin.Generate (Handle_Box, N);

      S := Get_Field (N, "shadow_type");

      if S /= null then
         Set_Shadow_Type (Gtk_Handle_Box (Handle_Box),
           Enums.Gtk_Shadow_Type'Value (S (S'First + 4 .. S'Last)));
      end if;

      S := Get_Field (N, "handle_position");

      if S /= null then
         Set_Handle_Position (Gtk_Handle_Box (Handle_Box),
           Enums.Gtk_Position_Type'Value (S (S'First + 4 .. S'Last)));
      end if;

      S := Get_Field (N, "snap_edge");

      if S /= null then
         Set_Snap_Edge (Gtk_Handle_Box (Handle_Box),
           Enums.Gtk_Position_Type'Value (S (S'First + 4 .. S'Last)));
      end if;
   end Generate;

end Gtk.Handle_Box;
