-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

with System;

package body Gtk.Item is

   -----------------
   -- Item_Select --
   -----------------

   procedure Item_Select (Item : access Gtk_Item_Record) is
      procedure Internal (Item : in System.Address);
      pragma Import (C, Internal, "gtk_item_select");
   begin
      Internal (Get_Object (Item));
   end Item_Select;

   -------------------
   -- Item_Deselect --
   -------------------

   procedure Item_Deselect (Item : access Gtk_Item_Record) is
      procedure Internal (Item : in System.Address);
      pragma Import (C, Internal, "gtk_item_deselect");
   begin
      Internal (Get_Object (Item));
   end Item_Deselect;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (Item : access Gtk_Item_Record) is
      procedure Internal (Item : in System.Address);
      pragma Import (C, Internal, "gtk_item_toggle");
   begin
      Internal (Get_Object (Item));
   end Toggle;

end Gtk.Item;
