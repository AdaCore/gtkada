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

with Gtk.Container;
with Gtk.Widget;

package Gtk.Fixed is

   type Gtk_Fixed_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Fixed is access all Gtk_Fixed_Record'Class;

   function Get_Children (Widget : access Gtk_Fixed_Record)
                          return      Widget.Widget_List.Glist;
   procedure Gtk_New (Widget : out Gtk_Fixed);
   procedure Initialize (Widget : access Gtk_Fixed_Record);
   procedure Move
      (Fixed  : access Gtk_Fixed_Record;
       Widget : in Gtk.Widget.Gtk_Widget;
       X      : in Gint16;
       Y      : in Gint16);
   procedure Put
      (Fixed  : access Gtk_Fixed_Record;
       Widget : in Gtk.Widget.Gtk_Widget;
       X      : in Gint16;
       Y      : in Gint16);

private
   type Gtk_Fixed_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

end Gtk.Fixed;
