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

with System;
with Gdk; use Gdk;
with Gtk.Container; use Gtk.Container;
with Gtk.Util; use Gtk.Util;

package body Gtk.Menu_Item is

   --------------
   -- Activate --
   --------------

   procedure Activate (Menu_Item : access Gtk_Menu_Item_Record) is
      procedure Internal (Menu_Item : in System.Address);
      pragma Import (C, Internal, "gtk_menu_item_activate");
   begin
      Internal (Get_Object (Menu_Item));
   end Activate;

   ---------------
   -- Configure --
   ---------------

   procedure Configure (Menu_Item              : access Gtk_Menu_Item_Record;
                        Show_Toggle_Indicator  : in     Boolean;
                        Show_Submenu_Indicator : in     Boolean) is
      procedure Internal (Menu_Item : System.Address;
                          Show_Toggle_Indicator : in Boolean;
                          Show_Submenu_Indicator : in Boolean);
      pragma Import (C, Internal, "gtk_menu_item_configure");
   begin
      Internal (Get_Object (Menu_Item), Show_Toggle_Indicator,
                Show_Submenu_Indicator);
   end Configure;

   --------------
   -- Deselect --
   --------------

   procedure Deselect (Menu_Item : access Gtk_Menu_Item_Record) is
      procedure Internal (Menu_Item : in System.Address);
      pragma Import (C, Internal, "gtk_menu_item_deselect");
   begin
      Internal (Get_Object (Menu_Item));
   end Deselect;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Menu_Item : out Gtk_Menu_Item;
                      Label     : in  String := "") is
   begin
      Menu_Item := new Gtk_Menu_Item_Record;
      Initialize (Menu_Item, Label);
   end Gtk_New;

   ----------------
   -- Gtk_Select --
   ----------------

   procedure Gtk_Select (Menu_Item : access Gtk_Menu_Item_Record) is
      procedure Internal (Menu_Item : in System.Address);
      pragma Import (C, Internal, "gtk_menu_item_select");
   begin
      Internal (Get_Object (Menu_Item));
   end Gtk_Select;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Menu_Item : access Gtk_Menu_Item_Record;
                         Label     : in  String) is
      function Internal (Label : in String) return System.Address;
      pragma Import (C, Internal, "gtk_menu_item_new_with_label");
   begin
      Set_Object (Menu_Item, Internal (Label & ASCII.NUL));
      Initialize_User_Data (Menu_Item);
   end Initialize;

   --------------------
   -- Remove_Submenu --
   --------------------

   procedure Remove_Submenu (Menu_Item : access Gtk_Menu_Item_Record) is
      procedure Internal (Menu_Item : in System.Address);
      pragma Import (C, Internal, "gtk_menu_item_remove_submenu");
   begin
      Internal (Get_Object (Menu_Item));
   end Remove_Submenu;

   -------------------
   -- Right_Justify --
   --------------------

   procedure Right_Justify (Menu_Item : access Gtk_Menu_Item_Record) is
      procedure Internal (Menu_Item : in System.Address);
      pragma Import (C, Internal, "gtk_menu_item_right_justify");
   begin
      Internal (Get_Object (Menu_Item));
   end Right_Justify;

   -------------------
   -- Set_Placement --
   -------------------

   procedure Set_Placement (Menu_Item : access Gtk_Menu_Item_Record;
                            Placement : in     Enums.Gtk_Submenu_Placement) is
      procedure Internal (Menu_Item : in System.Address;
                          Placement : in Enums.Gtk_Submenu_Placement);
      pragma Import (C, Internal, "gtk_menu_item_set_placement");
   begin
      Internal (Get_Object (Menu_Item), Placement);
   end Set_Placement;

   -----------------
   -- Set_Submenu --
   -----------------

   procedure Set_Submenu (Menu_Item : access Gtk_Menu_Item_Record;
                          Submenu   : access Widget.Gtk_Widget_Record'Class) is
      procedure Internal (Menu_Item : in System.Address;
                          Submenu   : in System.Address);
      pragma Import (C, Internal, "gtk_menu_item_set_submenu");
   begin
      Internal (Get_Object (Menu_Item), Get_Object (Submenu));
   end Set_Submenu;

   --------------
   -- Generate --
   --------------

   procedure Generate (N         : in Node_Ptr;
                       File      : in File_Type) is
   begin
      Gen_New (N, "Menu_Item", Get_Field (N, "label").all,
        File => File, Delim => '"');
      Item.Generate (N, File);

      if not N.Specific_Data.Has_Container then
         Gen_Call_Child (N, null, "Container", "Add", File => File);
         N.Specific_Data.Has_Container := True;
      end if;
   end Generate;

   procedure Generate (Menu_Item : in out Gtk_Object;
                       N         : in Node_Ptr) is
      S : String_Ptr;
   begin
      if not N.Specific_Data.Created then
         S := Get_Field (N, "label");

         if S = null then
            Gtk_New (Gtk_Menu_Item (Menu_Item));
         else
            Gtk_New (Gtk_Menu_Item (Menu_Item), S.all);
         end if;

         Set_Object (Get_Field (N, "name"), Menu_Item);
         N.Specific_Data.Created := True;
      end if;

      Item.Generate (Menu_Item, N);

      if not N.Specific_Data.Has_Container then
         Container.Add
           (Gtk_Container (Get_Object (Get_Field (N.Parent, "name"))),
            Widget.Gtk_Widget (Menu_Item));
         N.Specific_Data.Has_Container := True;
      end if;
   end Generate;

end Gtk.Menu_Item;
