-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

package body Gtk.Arrow is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Arrow       : out Gtk_Arrow;
       Arrow_Type  : in Gtk_Arrow_Type;
       Shadow_Type : in Gtk_Shadow_Type) is
   begin
      Arrow := new Gtk_Arrow_Record;
      Initialize (Arrow, Arrow_Type, Shadow_Type);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Arrow       : access Gtk_Arrow_Record'Class;
      Arrow_Type  : in Gtk_Arrow_Type;
      Shadow_Type : in Gtk_Shadow_Type)
   is
      function Internal
        (Arrow_Type  : in Gint;
         Shadow_Type : in Gint)
         return System.Address;
      pragma Import (C, Internal, "gtk_arrow_new");
   begin
      Set_Object (Arrow, Internal (Gtk_Arrow_Type'Pos (Arrow_Type),
                                   Gtk_Shadow_Type'Pos (Shadow_Type)));
      Initialize_User_Data (Arrow);
   end Initialize;

   ---------
   -- Set --
   ---------

   procedure Set
      (Arrow       : access Gtk_Arrow_Record;
       Arrow_Type  : in Gtk_Arrow_Type;
       Shadow_Type : in Gtk_Shadow_Type)
   is
      procedure Internal
         (Arrow       : in System.Address;
          Arrow_Type  : in Gint;
          Shadow_Type : in Gint);
      pragma Import (C, Internal, "gtk_arrow_set");
   begin
      Internal (Get_Object (Arrow),
                Gtk_Arrow_Type'Pos (Arrow_Type),
                Gtk_Shadow_Type'Pos (Shadow_Type));
   end Set;

   --------------
   -- Generate --
   --------------

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type) is
      use Misc;
   begin
      Gen_New (N, "Arrow", Get_Field (N, "arrow_type").all,
        Get_Field (N, "shadow_type").all, File => File);
      Misc.Generate (N, File);
   end Generate;

   procedure Generate
     (Arrow  : in out Gtk.Object.Gtk_Object; N : in Node_Ptr)
   is
      use Misc;

      S, S2 : String_Ptr;
   begin
      if not N.Specific_Data.Created then
         S := Get_Field (N, "arrow_type");
         S2 := Get_Field (N, "shadow_type");
         Gtk_New (Gtk_Arrow (Arrow),
                  Gtk_Arrow_Type'Value (S (S'First + 4 .. S'Last)),
                  Gtk_Shadow_Type'Value (S2 (S2'First + 4 .. S2'Last)));
         Set_Object (Get_Field (N, "name"), Arrow);
         N.Specific_Data.Created := True;
      end if;

      Misc.Generate (Arrow, N);
   end Generate;

end Gtk.Arrow;
