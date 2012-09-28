------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Radio_Menu_Item is

   function Selected_Button
     (In_Group : Widget_SList.GSlist) return Natural
   is
      use Widget_SList;
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

   package Type_Conversion_Gtk_Radio_Menu_Item is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Radio_Menu_Item_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Radio_Menu_Item);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
       Group           : Gtk.Widget.Widget_SList.GSlist;
       Label           : UTF8_String := "")
   is
   begin
      Radio_Menu_Item := new Gtk_Radio_Menu_Item_Record;
      Gtk.Radio_Menu_Item.Initialize (Radio_Menu_Item, Group, Label);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
      (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
       Group           : Gtk.Widget.Widget_SList.GSlist;
       Label           : UTF8_String)
   is
   begin
      Radio_Menu_Item := new Gtk_Radio_Menu_Item_Record;
      Gtk.Radio_Menu_Item.Initialize_With_Mnemonic (Radio_Menu_Item, Group, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Radio_Menu_Item : not null access Gtk_Radio_Menu_Item_Record'Class;
       Group           : Gtk.Widget.Widget_SList.GSlist;
       Label           : UTF8_String := "")
   is
      function Internal
         (Group : System.Address;
          Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_radio_menu_item_new_with_label");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr;
      Tmp_Return : System.Address;
   begin
      if Label = "" then
         Tmp_Label := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Tmp_Return := Internal (Gtk.Widget.Widget_SList.Get_Object (Group), Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Radio_Menu_Item, Tmp_Return);
   end Initialize;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
      (Radio_Menu_Item : not null access Gtk_Radio_Menu_Item_Record'Class;
       Group           : Gtk.Widget.Widget_SList.GSlist;
       Label           : UTF8_String)
   is
      function Internal
         (Group : System.Address;
          Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_radio_menu_item_new_with_mnemonic");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Gtk.Widget.Widget_SList.Get_Object (Group), Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Radio_Menu_Item, Tmp_Return);
   end Initialize_With_Mnemonic;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group
      (Radio_Menu_Item : not null access Gtk_Radio_Menu_Item_Record)
       return Gtk.Widget.Widget_SList.GSlist
   is
      function Internal
         (Radio_Menu_Item : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_menu_item_get_group");
      Tmp_Return : Gtk.Widget.Widget_SList.GSlist;
   begin
      Gtk.Widget.Widget_SList.Set_Object (Tmp_Return, Internal (Get_Object (Radio_Menu_Item)));
      return Tmp_Return;
   end Get_Group;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group
      (Radio_Menu_Item : not null access Gtk_Radio_Menu_Item_Record;
       Group           : Gtk.Widget.Widget_SList.GSlist)
   is
      procedure Internal
         (Radio_Menu_Item : System.Address;
          Group           : System.Address);
      pragma Import (C, Internal, "gtk_radio_menu_item_set_group");
   begin
      Internal (Get_Object (Radio_Menu_Item), Gtk.Widget.Widget_SList.Get_Object (Group));
   end Set_Group;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Radio_Menu_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_do_set_related_action");
   begin
      Internal (Get_Object (Self), Get_Object (Action));
   end Do_Set_Related_Action;

   ------------------------
   -- Get_Related_Action --
   ------------------------

   function Get_Related_Action
      (Self : not null access Gtk_Radio_Menu_Item_Record)
       return Gtk.Action.Gtk_Action
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_activatable_get_related_action");
      Stub_Gtk_Action : Gtk.Action.Gtk_Action_Record;
   begin
      return Gtk.Action.Gtk_Action (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Action));
   end Get_Related_Action;

   -------------------------------
   -- Get_Use_Action_Appearance --
   -------------------------------

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Radio_Menu_Item_Record) return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_activatable_get_use_action_appearance");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
   end Get_Use_Action_Appearance;

   ------------------------
   -- Set_Related_Action --
   ------------------------

   procedure Set_Related_Action
      (Self   : not null access Gtk_Radio_Menu_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_set_related_action");
   begin
      Internal (Get_Object (Self), Get_Object (Action));
   end Set_Related_Action;

   -------------------------------
   -- Set_Use_Action_Appearance --
   -------------------------------

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Radio_Menu_Item_Record;
       Use_Appearance : Boolean)
   is
      procedure Internal (Self : System.Address; Use_Appearance : Integer);
      pragma Import (C, Internal, "gtk_activatable_set_use_action_appearance");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Appearance));
   end Set_Use_Action_Appearance;

   ----------------------------
   -- Sync_Action_Properties --
   ----------------------------

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Radio_Menu_Item_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

end Gtk.Radio_Menu_Item;
