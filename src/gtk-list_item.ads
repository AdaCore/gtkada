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

--  <c_version>1.3.4</c_version>

with Gtk.Item;

package Gtk.List_Item is

   type Gtk_List_Item_Record is new Gtk.Item.Gtk_Item_Record with private;
   type Gtk_List_Item is access all Gtk_List_Item_Record'Class;

   procedure Gtk_New
     (List_Item : out Gtk_List_Item;
      Label     : String := "");

   procedure Initialize
     (List_Item : access Gtk_List_Item_Record'Class;
      Label     : String := "");

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_List_Item.

   procedure Gtk_Select (List_Item : access Gtk_List_Item_Record);

   procedure Deselect (List_Item : access Gtk_List_Item_Record);

private
   type Gtk_List_Item_Record is new Gtk.Item.Gtk_Item_Record with null record;

   pragma Import (C, Get_Type, "gtk_list_item_get_type");
end Gtk.List_Item;
