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
with Gtk.Widget; use Gtk.Widget;

package body Gnome.Stock is

   use Gtk.Button;
   use Gtk.Menu_Item;

   ------------
   -- Button --
   ------------

   function Button
     (Button_Type : String;
      Ordinary    : Boolean := False) return Gtk_Button
   is
      function Internal (Button_Type : String) return System.Address;
      pragma Import (C, Internal, "gnome_stock_button");

      function Internal2 (Button_Type : String) return System.Address;
      pragma Import (C, Internal2, "gnome_stock_or_ordinary_button");

   begin
      if Ordinary then
         return Gtk_Button (Convert (Internal2 (Button_Type & ASCII.NUL)));
      else
         return Gtk_Button (Convert (Internal (Button_Type & ASCII.NUL)));
      end if;
   end Button;

   ---------------
   -- Menu_Item --
   ---------------

   function Menu_Item
     (Icon : String;
      Text : String) return Gtk_Menu_Item
   is
      function Internal
        (Icon : String;
         Text : String) return System.Address;
      pragma Import (C, Internal, "gnome_stock_menu_item");

   begin
      return Gtk_Menu_Item
        (Convert (Internal (Icon & ASCII.NUL, Text & ASCII.NUL)));
   end Menu_Item;

end Gnome.Stock;
