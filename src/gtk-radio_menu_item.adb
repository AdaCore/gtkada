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
with Gdk; use Gdk;
with Gtk.Util; use Gtk.Util;

package body Gtk.Radio_Menu_Item is

   use Widget_SList;

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
        (Radio_Menu_Item, Internal (Get_Object (Group), Label & ASCII.Nul));
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
   end Set_Group;

   --------------
   -- Generate --
   --------------

   --  ??? This code is very similar to what is done for Radio Buttons
   --  (see gtk-radio_button.adb), so it would be nice to share the code
   --  Also, this code only takes into account the default case of an unnamed
   --  radio group.

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      Label : constant String_Ptr := Get_Field (N, "label");
      Name  : constant String_Ptr := Get_Field (N, "name");
      Top   : constant String_Ptr := Get_Field (Find_Top_Widget (N), "name");

   begin
      if not N.Specific_Data.Created then
         Add_Package ("Radio_Menu_Item");
         Put (File, "   Gtk_New (" &
           To_Ada (Top.all) & "." & To_Ada (Name.all) & ", " &
           To_Ada (Get_Field (N.Parent, "name").all) & "_Group");

         if Label /= null then
            Put (File, ", """ & Label.all & '"');
         end if;

         Put_Line (File, ");");
         Put_Line (File, "   " & To_Ada (Get_Field (N.Parent, "name").all) &
           "_Group := Group (" & To_Ada (Top.all) & "." &
           To_Ada (Name.all) & ");");
         N.Specific_Data.Created := True;
      end if;

      Check_Menu_Item.Generate (N, File);
   end Generate;

   procedure Generate
     (Radio_Menu_Item : in out Object.Gtk_Object; N : in Node_Ptr)
   is
      S : String_Ptr := Get_Field (N, "label");

      function Find_Group (N : Node_Ptr) return Widget_SList.GSlist;
      --  Find the group associated with the previous radio menu item in the
      --  node N or Null_List if there is none.

      function Find_Group (N : Node_Ptr)
        return Widget_SList.GSlist
      is
         P     : Node_Ptr := N.Parent.Child;
         Q     : Node_Ptr;
         S     : String_Ptr;

      begin
         while P /= N loop
            S := Get_Field (P, "class");

            if S /= null and then S.all = "GtkRadioMenuItem" then
               Q := P;
            end if;

            P := P.Next;
         end loop;

         if Q = null then
            return Widget_SList.Null_List;
         else
            return Group (Gtk_Radio_Menu_Item
              (Get_Object (Get_Field (Q, "name"))));
         end if;
      end Find_Group;

   begin
      if not N.Specific_Data.Created then
         if S = null then
            Gtk_New (Gtk_Radio_Menu_Item (Radio_Menu_Item),
              Find_Group (N));
         else
            Gtk_New (Gtk_Radio_Menu_Item (Radio_Menu_Item),
              Find_Group (N), S.all);
         end if;

         Set_Object (Get_Field (N, "name"), Radio_Menu_Item);
         N.Specific_Data.Created := True;
      end if;

      Check_Menu_Item.Generate (Radio_Menu_Item, N);
   end Generate;

end Gtk.Radio_Menu_Item;
