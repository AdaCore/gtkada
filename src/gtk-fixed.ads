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


with Gtk.Enums;
with Gtk.Container;
with Gtk.Widget;

package Gtk.Fixed is

   type Gtk_Fixed is new Gtk.Container.Gtk_Container with private;

   function Get_Children (Widget : in Gtk.Fixed.Gtk_Fixed'Class)
                          return      Enums.Widget_List.Glist;
   procedure Gtk_New (Widget : out Gtk_Fixed);
   procedure Move
      (Fixed  : in Gtk_Fixed'Class;
       Widget : in Gtk.Widget.Gtk_Widget'Class;
       X      : in Gint16;
       Y      : in Gint16);
   procedure Put
      (Fixed  : in Gtk_Fixed'Class;
       Widget : in Gtk.Widget.Gtk_Widget'Class;
       X      : in Gint16;
       Y      : in Gint16);

private
   type Gtk_Fixed is new Gtk.Container.Gtk_Container with null record;

   --  mapping: Get_Children gtkfixed.h GtkFixed->children
   --  mapping: NOT_IMPLEMENTED gtkfixed.h gtk_fixed_get_type
   --  mapping: Gtk_New gtkfixed.h gtk_fixed_new
   --  mapping: Move gtkfixed.h gtk_fixed_move
   --  mapping: Put gtkfixed.h gtk_fixed_put
end Gtk.Fixed;
