-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------

with System;
with Gdk; use Gdk;

package body Gtk.Check_Button is

   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Widget : out Gtk_Check_Button) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_check_button_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Widget : out Gtk_Check_Button;
                         With_Label : in String) is
      function Internal (Label : in String) return System.Address;
      pragma Import (C, Internal, "gtk_check_button_new_with_label");
   begin
      Set_Object (Widget, Internal (With_Label & ASCII.NUL));
   end Gtk_New;

end Gtk.Check_Button;
