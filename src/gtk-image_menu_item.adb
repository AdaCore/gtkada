-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Gtk.Widget;
with System;

package body Gtk.Pixmap_Menu_Item is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Pixmap_Menu_Item) is
   begin
      Widget := new Gtk_Pixmap_Menu_Item_Record;
      Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Pixmap_Menu_Item_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_pixmap_menu_item_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   ----------------
   -- Set_Pixmap --
   ----------------

   procedure Set_Pixmap
     (Menu_Item : access Gtk_Pixmap_Menu_Item_Record;
      Pixmap    : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Menu_Item : System.Address;
         Pixmap    : System.Address);
      pragma Import (C, Internal, "gtk_pixmap_menu_item_set_pixmap");
   begin
      Internal (Get_Object (Menu_Item), Get_Object (Pixmap));
   end Set_Pixmap;

end Gtk.Pixmap_Menu_Item;
