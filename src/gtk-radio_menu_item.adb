-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                  Copyright (C) 2001 ACT-Europe                    --
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
with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);

package body Gtk.Radio_Menu_Item is

   use Widget_SList;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Type_Conversion (Type_Name : String) return GObject;
   --  This function is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   -----------
   -- Group --
   -----------

   function Group (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record)
     return Widget_SList.GSlist
   is
      function Internal (Radio_Menu_Item : in System.Address)
        return System.Address;
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
     (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
      Group           : in Widget_SList.GSlist;
      Label           : in String := "") is
   begin
      Radio_Menu_Item := new Gtk_Radio_Menu_Item_Record;
      Initialize (Radio_Menu_Item, Group, Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record'Class;
      Group  : in Widget_SList.GSlist;
      Label  : in String := "")
   is
      function Internal
        (Group  : in System.Address;
         Label  : in String) return System.Address;
      pragma Import (C, Internal, "gtk_radio_menu_item_new_with_label");

   begin
      Set_Object
        (Radio_Menu_Item, Internal (Get_Object (Group), Label & ASCII.NUL));
      Initialize_User_Data (Radio_Menu_Item);
   end Initialize;

   ---------------------
   -- Selected_Button --
   ---------------------

   function Selected_Button (In_Group : in Widget_SList.GSlist)
     return Natural
   is
      J   : Natural := 0;
      Tmp : Widget_SList.GSlist := In_Group;
   begin
      while Tmp /= Widget_SList.Null_List loop
         exit when Get_Active (Gtk_Radio_Menu_Item (Get_Data (Tmp)));
         Tmp := Next (Tmp);
         J := J + 1;
      end loop;

      return J;
   end Selected_Button;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record;
      Group           : in Widget_SList.GSlist)
   is
      procedure Internal
        (Radio_Menu_Item : in System.Address;
         Group           : in System.Address);
      pragma Import (C, Internal, "gtk_radio_menu_item_set_group");

   begin
      Internal (Get_Object (Radio_Menu_Item), Get_Object (Group));

      --  This is a workaround for a bug in gtk+ <= 1.2.7 (that has been
      --  reported) The same code might be included in gtk+ at some point, and
      --  can be removed from here then.   ???
      Set_Active (Radio_Menu_Item, False);
   end Set_Group;

   ---------------------
   -- Type_Conversion --
   ---------------------

   function Type_Conversion (Type_Name : String) return GObject is
   begin
      if Type_Name = "GtkRadioMenuItem" then
         return new Gtk_Radio_Menu_Item_Record;
      else
         return null;
      end if;
   end Type_Conversion;

begin
   Glib.Type_Conversion_Hooks.Add_Hook (Type_Conversion'Access);
end Gtk.Radio_Menu_Item;
