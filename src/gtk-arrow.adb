-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
with Gtk.Enums; use Gtk.Enums;

package body Gtk.Arrow is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Arrow       : out Gtk_Arrow;
       Arrow_Type  : Gtk_Arrow_Type;
       Shadow_Type : Gtk_Shadow_Type) is
   begin
      Arrow := new Gtk_Arrow_Record;
      Initialize (Arrow, Arrow_Type, Shadow_Type);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Arrow       : access Gtk_Arrow_Record'Class;
      Arrow_Type  : Gtk_Arrow_Type;
      Shadow_Type : Gtk_Shadow_Type)
   is
      function Internal
        (Arrow_Type  : Gtk_Arrow_Type;
         Shadow_Type : Gtk_Shadow_Type) return System.Address;
      pragma Import (C, Internal, "gtk_arrow_new");

   begin
      Set_Object (Arrow, Internal (Arrow_Type, Shadow_Type));
      Initialize_User_Data (Arrow);
   end Initialize;

   ---------
   -- Set --
   ---------

   procedure Set
      (Arrow       : access Gtk_Arrow_Record;
       Arrow_Type  : Gtk_Arrow_Type;
       Shadow_Type : Gtk_Shadow_Type)
   is
      procedure Internal
         (Arrow       : System.Address;
          Arrow_Type  : Gtk_Arrow_Type;
          Shadow_Type : Gtk_Shadow_Type);
      pragma Import (C, Internal, "gtk_arrow_set");

   begin
      Internal (Get_Object (Arrow), Arrow_Type, Shadow_Type);
   end Set;

end Gtk.Arrow;
