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

package body Gtk.Drawing_Area is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Drawing_Area : out Gtk_Drawing_Area) is
   begin
      Drawing_Area := new Gtk_Drawing_Area_Record;
      Gtk.Drawing_Area.Initialize (Drawing_Area);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Drawing_Area : access Gtk_Drawing_Area_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_drawing_area_new");
   begin
      Set_Object (Drawing_Area, Internal);
   end Initialize;

   ----------
   -- Size --
   ----------

   procedure Size
     (Darea  : access Gtk_Drawing_Area_Record;
      Width  : Gint;
      Height : Gint)
   is
      procedure Internal
        (Darea  : System.Address;
         Width  : Gint;
         Height : Gint);
      pragma Import (C, Internal, "gtk_drawing_area_size");

   begin
      Internal (Get_Object (Darea), Width, Height);
   end Size;

end Gtk.Drawing_Area;
