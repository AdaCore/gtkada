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
with Gdk; use Gdk;

package body Gtk.Misc is

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment (Misc   : access Gtk_Misc_Record;
                            Xalign : in     Gfloat;
                            Yalign : in     Gfloat) is
      procedure Internal (Misc           : in System.Address;
                          Xalign, Yalign : in Gfloat);
      pragma Import (C, Internal, "gtk_misc_set_alignment");
   begin
      Internal (Get_Object (Misc), Xalign, Yalign);
   end Set_Alignment;

   -----------------
   -- Set_Padding --
   -----------------

   procedure Set_Padding (Misc : access Gtk_Misc_Record;
                          Xpad : in     Gint;
                          Ypad : in     Gint) is
      procedure Internal (Misc       : in System.Address;
                          Xpad, Ypad : in Gint);
      pragma Import (C, Internal, "gtk_misc_set_padding");
   begin
      Internal (Get_Object (Misc), Xpad, Ypad);
   end Set_Padding;

   --------------
   -- Generate --
   --------------

   procedure Generate (N    : in Node_Ptr;
                       File : in File_Type) is
   begin
      Widget.Generate (N, File);
      Gen_Set (N, "Misc", "Alignment", "xalign", "yalign", "", "", File,
        Is_Float => True);
      Gen_Set (N, "Misc", "Padding", "xpad", "ypad", "", "", File);
   end Generate;

   procedure Generate (Misc : in out Gtk_Object;
                       N    : in Node_Ptr) is
      S, S2 : String_Ptr;

   begin
      Widget.Generate (Misc, N);

      S := Get_Field (N, "xalign");
      S2 := Get_Field (N, "yalign");

      if S /= null and then S2 /= null then
         Set_Alignment
           (Gtk_Misc (Misc), Gfloat'Value (S.all), Gfloat'Value (S2.all));
      end if;

      S := Get_Field (N, "xpad");
      S2 := Get_Field (N, "ypad");

      if S /= null and then S2 /= null then
         Set_Padding
           (Gtk_Misc (Misc), Gint'Value (S.all), Gint'Value (S2.all));
      end if;
   end Generate;

end Gtk.Misc;
