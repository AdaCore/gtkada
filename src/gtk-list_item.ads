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


with Gtk.Item;

package Gtk.List_Item is

   type Gtk_List_Item is new Gtk.Item.Gtk_Item with private;

   procedure Deselect (List_Item : in Gtk_List_Item'Class);
   procedure Gtk_New (Widget : out Gtk_List_Item;
                      Label  : in String);
   procedure Gtk_New (Widget : out Gtk_List_Item);
   procedure Gtk_Select (List_Item : in Gtk_List_Item'Class);

private
   type Gtk_List_Item is new Gtk.Item.Gtk_Item with null record;

   --  mapping: Deselect gtklistitem.h gtk_list_item_deselect
   --  mapping: NOT_IMPLEMENTED gtklistitem.h gtk_list_item_get_type
   --  mapping: Gtk_New gtklistitem.h gtk_list_item_new_with_label
   --  mapping: Gtk_New gtklistitem.h gtk_list_item_new
   --  mapping: Gtk_Select gtklistitem.h gtk_list_item_select
end Gtk.List_Item;
