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

package body Gtk.Misc is

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment
     (Misc : access Gtk_Misc_Record; Xalign : Gfloat; Yalign : Gfloat)
   is
      procedure Internal (Misc : System.Address; Xalign, Yalign : Gfloat);
      pragma Import (C, Internal, "gtk_misc_set_alignment");

   begin
      Internal (Get_Object (Misc), Xalign, Yalign);
   end Set_Alignment;

   -----------------
   -- Set_Padding --
   -----------------

   procedure Set_Padding
     (Misc : access Gtk_Misc_Record;
      Xpad : Gint;
      Ypad : Gint)
   is
      procedure Internal
        (Misc : System.Address; Xpad, Ypad : Gint);
      pragma Import (C, Internal, "gtk_misc_set_padding");

   begin
      Internal (Get_Object (Misc), Xpad, Ypad);
   end Set_Padding;

end Gtk.Misc;
