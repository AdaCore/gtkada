-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
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

with Gdk;        use Gdk;
with System;

package body Gtk.Extra.Check_Item is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Item  : out Gtk_Check_Item;
                      Label : in String := "")
   is
   begin
      Item := new Gtk_Check_Item_Record;
      Initialize (Item, Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Item  : access Gtk_Check_Item_Record;
                         Label : in String := "")
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_check_item_new");
      function Internal2 (Label  : in String) return System.Address;
      pragma Import (C, Internal2, "gtk_check_item_new_with_label");
   begin
      if Label = "" then
         Set_Object (Item, Internal);
      else
         Set_Object (Item, Internal2 (Label & ASCII.Nul));
      end if;
      Initialize_User_Data (Item);
   end Initialize;

end Gtk.Extra.Check_Item;
