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

with Gtk.Check_Button;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Radio_Button is

   type Gtk_Radio_Button is new Check_Button.Gtk_Check_Button with private;

   function Group (Button : in Gtk_Radio_Button) return Widget_SList.GSlist;

   procedure Gtk_New (Button : out Gtk_Radio_Button;
                      Group  : in Widget_SList.GSlist);

   procedure Gtk_New (Button : out Gtk_Radio_Button;
                      Group  : in Widget_SList.GSlist;
                      Label  : in String);

   procedure Gtk_New
     (Button : out Gtk_Radio_Button;
      Group  : in Gtk_Radio_Button);

   procedure Gtk_New
     (Button : out Gtk_Radio_Button;
      Group  : in Gtk_Radio_Button;
      Label  : in String);

   procedure Set_Group (Button : in Gtk_Radio_Button;
                        Group  : in Widget_SList.GSlist);

private
   type Gtk_Radio_Button is new Check_Button.Gtk_Check_Button with null record;

end Gtk.Radio_Button;
