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

with Gtk.Style;
with Gtk.Widget;

package Gtk.Rc is

   procedure Init;

   procedure Parse (Filename : in String);

   procedure Parse_String (Rc_String : in String);

   function Get_Style (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
                       return Gtk.Style.Gtk_Style;

   procedure Add_Widget_Name_Style
     (Style   : in Gtk.Style.Gtk_Style'Class;
      Pattern : in String);

   procedure Add_Widget_Class_Style
     (Style   : in Gtk.Style.Gtk_Style'Class;
      Pattern : in String);


   pragma Import (C, Init, "gtk_rc_init");

end Gtk.Rc;
