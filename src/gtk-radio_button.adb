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

with System;

package body Gtk.Radio_Button is

   use Widget_SList;

   -----------
   -- Group --
   -----------

   function Group
     (Radio_Button : access Gtk_Radio_Button_Record) return Widget_SList.GSlist
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_button_group");

      Group : Widget_SList.GSlist;

   begin
      Set_Object (Group, Internal (Get_Object (Radio_Button)));
      return Group;
   end Group;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Radio_Button : out Gtk_Radio_Button;
      Group        : Widget_SList.GSlist := Widget_SList.Null_List;
      Label        : String := "") is
   begin
      Radio_Button := new Gtk_Radio_Button_Record;
      Initialize (Radio_Button, Group, Label);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Radio_Button : out Gtk_Radio_Button;
      Group        : Gtk_Radio_Button;
      Label        : String := "") is
   begin
      Radio_Button := new Gtk_Radio_Button_Record;
      Initialize (Radio_Button, Group, Label);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
     (Radio_Button : out Gtk_Radio_Button;
      Group        : Widget_SList.GSlist := Widget_SList.Null_List;
      Label        : String) is
   begin
      Radio_Button := new Gtk_Radio_Button_Record;
      Initialize_With_Mnemonic (Radio_Button, Group, Label);
   end Gtk_New_With_Mnemonic;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
     (Radio_Button : out Gtk_Radio_Button;
      Group        : Gtk_Radio_Button;
      Label        : String) is
   begin
      Radio_Button := new Gtk_Radio_Button_Record;
      Initialize_With_Mnemonic (Radio_Button, Group, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Radio_Button : access Gtk_Radio_Button_Record'Class;
      Group        : Widget_SList.GSlist;
      Label        : String)
   is
      function Internal
        (Group : System.Address; Label : String) return System.Address;
      pragma Import (C, Internal, "gtk_radio_button_new_with_label");

      function Internal2 (Group : System.Address) return System.Address;
      pragma Import (C, Internal2, "gtk_radio_button_new");

   begin
      if Label = "" then
         Set_Object (Radio_Button, Internal2 (Get_Object (Group)));
      else
         Set_Object
           (Radio_Button, Internal (Get_Object (Group), Label & ASCII.NUL));
      end if;

      Initialize_User_Data (Radio_Button);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Radio_Button : access Gtk_Radio_Button_Record'Class;
      Group        : Gtk_Radio_Button;
      Label        : String)
   is
      function Internal
        (Group : System.Address; Label : String) return System.Address;
      pragma Import
        (C, Internal, "gtk_radio_button_new_with_label_from_widget");

      function Internal2 (Group : System.Address) return System.Address;
      pragma Import (C, Internal2, "gtk_radio_button_new_from_widget");

      S : System.Address := System.Null_Address;

   begin
      if Group /= null then
         S := Get_Object (Group);
      end if;

      if Label = "" then
         Set_Object (Radio_Button, Internal2 (S));
      else
         Set_Object (Radio_Button, Internal (S, Label & ASCII.NUL));
      end if;

      Initialize_User_Data (Radio_Button);
   end Initialize;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
     (Radio_Button : access Gtk_Radio_Button_Record'Class;
      Group        : Widget_SList.GSlist;
      Label        : String)
   is
      function Internal
        (Group : System.Address; Label : String) return System.Address;
      pragma Import (C, Internal, "gtk_radio_button_new_with_mnemonic");
   begin
      Set_Object
        (Radio_Button, Internal (Get_Object (Group), Label & ASCII.NUL));
      Initialize_User_Data (Radio_Button);
   end Initialize_With_Mnemonic;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
     (Radio_Button : access Gtk_Radio_Button_Record'Class;
      Group        : Gtk_Radio_Button;
      Label        : String)
   is
      function Internal
        (Group : System.Address; Label : String) return System.Address;
      pragma Import
        (C, Internal, "gtk_radio_button_new_with_mnemonic_from_widget");
   begin
      Set_Object
        (Radio_Button, Internal (Get_Object (Group), Label & ASCII.NUL));
      Initialize_User_Data (Radio_Button);
   end Initialize_With_Mnemonic;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group
     (Radio_Button : access Gtk_Radio_Button_Record;
      Group        : Widget_SList.GSlist)
   is
      procedure Internal (Button : System.Address; Group : System.Address);
      pragma Import (C, Internal, "gtk_radio_button_set_group");

   begin
      Internal (Get_Object (Radio_Button), Get_Object (Group));
   end Set_Group;

end Gtk.Radio_Button;
