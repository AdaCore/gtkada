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

package body Gtk.Button_Box is

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout
     (Button_Box : access Gtk_Button_Box_Record)
      return Enums.Gtk_Button_Box_Style
   is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_button_box_get_layout");

   begin
      return Enums.Gtk_Button_Box_Style'Val
        (Internal (Get_Object (Button_Box)));
   end Get_Layout;

   ----------------
   -- Set_Layout --
   ----------------

   procedure Set_Layout
     (Button_Box   : access Gtk_Button_Box_Record;
      Layout_Style : Enums.Gtk_Button_Box_Style)
   is
      procedure Internal
        (Widget       : System.Address;
         Layout_Style : Gint);
      pragma Import (C, Internal, "gtk_button_box_set_layout");

   begin
      Internal
        (Get_Object (Button_Box),
         Enums.Gtk_Button_Box_Style'Pos (Layout_Style));
   end Set_Layout;

end Gtk.Button_Box;
