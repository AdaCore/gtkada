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

with Gtk.Menu_Item;

package Gtk.Tearoff_Menu_Item is

   type Gtk_Tearoff_Menu_Item_Record is
     new Menu_Item.Gtk_Menu_Item_Record with private;
   type Gtk_Tearoff_Menu_Item is access all Gtk_Tearoff_Menu_Item_Record'Class;

   procedure Gtk_New (Menu_Item : out Gtk_Tearoff_Menu_Item);

   procedure Initialize
     (Menu_Item : access Gtk_Tearoff_Menu_Item_Record'Class);

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Tearoff_Menu_Item.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

private
   type Gtk_Tearoff_Menu_Item_Record is new Menu_Item.Gtk_Menu_Item_Record
     with null record;

   pragma Import (C, Get_Type, "get_tearoff_men_item_get_type");
end Gtk.Tearoff_Menu_Item;
