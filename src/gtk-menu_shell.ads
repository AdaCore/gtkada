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

package Gtk.Menu_Shell is

   type Gtk_Menu_Shell_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Menu_Shell is access all Gtk_Menu_Shell_Record'Class;

   procedure Append
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Child      : in Gtk.Widget.Gtk_Widget);
   procedure Deactivate (Menu_Shell : access Gtk_Menu_Shell_Record);
   procedure Insert
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Child      : in Gtk.Widget.Gtk_Widget;
      Position   : in Gint);
   procedure Prepend
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Child      : in Gtk.Widget.Gtk_Widget);

private
   type Gtk_Menu_Shell_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

end Gtk.Menu_Shell;
