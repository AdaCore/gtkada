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

with System;
with Gdk; use Gdk;

package body Gtk.Radio_Menu_Item is

   use Widget_SList;

   -----------
   -- Group --
   -----------

   function Group (Radio_Menu_Item : in Gtk_Radio_Menu_Item'Class)
                   return               Widget_SList.GSlist
   is
      function Internal (Radio_Menu_Item : in System.Address)
                         return               System.Address;
      pragma Import (C, Internal, "gtk_radio_menu_item_group");
      Group : Widget_SList.GSlist;
   begin
      Set_Object (Group, Internal (Get_Object (Radio_Menu_Item)));
      return Group;
   end Group;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget : out Gtk_Radio_Menu_Item;
       Group  : in Widget_SList.GSlist;
       Label  : in String)
   is
      function Internal
         (Group  : in System.Address;
          Label  : in String)
          return      System.Address;
      pragma Import (C, Internal, "gtk_radio_menu_item_new_with_label");
   begin
      Set_Object (Widget, Internal (Get_Object (Group),
                                    Label & Ascii.NUL));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Radio_Menu_Item;
                      Group  : in Widget_SList.GSlist)
   is
      function Internal (Group  : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_radio_menu_item_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Group)));
   end Gtk_New;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group
      (Radio_Menu_Item : in Gtk_Radio_Menu_Item'Class;
       Group           : in Widget_SList.GSlist)
   is
      procedure Internal
         (Radio_Menu_Item : in System.Address;
          Group           : in System.Address);
      pragma Import (C, Internal, "gtk_radio_menu_item_set_group");
   begin
      Internal (Get_Object (Radio_Menu_Item), Get_Object (Group));
   end Set_Group;

end Gtk.Radio_Menu_Item;
