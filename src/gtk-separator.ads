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
-- Library General Public License for more details.                  --
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


with Gtk.Widget;

package Gtk.Separator is

   type Gtk_Separator is new Gtk.Widget.Gtk_Widget with private;

   procedure Gtk_New_Hseparator (Widget : out Gtk_Separator);
   procedure Gtk_New_Vseparator (Widget : out Gtk_Separator);

private
   type Gtk_Separator is new Gtk.Widget.Gtk_Widget with null record;

   --  mapping: NOT_IMPLEMENTED gtkseparator.h gtk_separator_get_type
   --  mapping: Gtk_New_Hseparator gtkhseparator.h gtk_hseparator_new
   --  mapping: Gtk_New_Vseparator gtkvseparator.h gtk_vseparator_new
end Gtk.Separator;
