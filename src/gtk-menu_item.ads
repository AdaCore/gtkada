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

with Gtk.Enums;
with Gtk.Item;
with Gtk.Widget;

package Gtk.Menu_Item is

   type Gtk_Menu_Item is new Item.Gtk_Item with private;

   procedure Gtk_New (Menu_Item : out Gtk_Menu_Item);

   procedure Gtk_New (Menu_Item : out Gtk_Menu_Item;
                      Label     : in  String);

   procedure Set_Submenu (Menu_Item : in out Gtk_Menu_Item;
                          Submenu   : in     Widget.Gtk_Widget'Class);

   procedure Remove_Submenu (Menu_Item : in out Gtk_Menu_Item);

   procedure Set_Placement (Menu_Item : in out Gtk_Menu_Item;
                            Placement : in     Enums.Gtk_Submenu_Placement);

   procedure Configure (Menu_Item              : in out Gtk_Menu_Item;
                        Show_Toggle_Indicator  : in     Boolean;
                        Show_Submenu_Indicator : in     Boolean);

   procedure Gtk_Select (Menu_Item : in out Gtk_Menu_Item);

   procedure Deselect (Menu_Item : in out Gtk_Menu_Item);

   procedure Activate (Menu_Item : in out Gtk_Menu_Item);

   procedure Right_Justify (Menu_Item : in out Gtk_Menu_Item);

private

   type Gtk_Menu_Item is new Item.Gtk_Item with null record;

end Gtk.Menu_Item;
