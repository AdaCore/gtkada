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

package body Gtk.Check_Menu_Item is

   package Type_Conversion_Gtk_Check_Menu_Item is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Check_Menu_Item_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Check_Menu_Item);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Check_Menu_Item : out Gtk_Check_Menu_Item;
       Label           : UTF8_String := "")
   is
   begin
      Check_Menu_Item := new Gtk_Check_Menu_Item_Record;
      Gtk.Check_Menu_Item.Initialize (Check_Menu_Item, Label);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
      (Check_Menu_Item : out Gtk_Check_Menu_Item;
       Label           : UTF8_String)
   is
   begin
      Check_Menu_Item := new Gtk_Check_Menu_Item_Record;
      Gtk.Check_Menu_Item.Initialize_With_Mnemonic (Check_Menu_Item, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record'Class;
       Label           : UTF8_String := "")
   is
      function Internal
         (Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_check_menu_item_new_with_label");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr;
      Tmp_Return : System.Address;
   begin
      if Label = "" then
         Tmp_Label := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Tmp_Return := Internal (Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Check_Menu_Item, Tmp_Return);
   end Initialize;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record'Class;
       Label           : UTF8_String)
   is
      function Internal
         (Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_check_menu_item_new_with_mnemonic");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Check_Menu_Item, Tmp_Return);
   end Initialize_With_Mnemonic;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record)
       return Boolean
   is
      function Internal (Check_Menu_Item : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_check_menu_item_get_active");
   begin
      return Boolean'Val (Internal (Get_Object (Check_Menu_Item)));
   end Get_Active;

   -----------------------
   -- Get_Draw_As_Radio --
   -----------------------

   function Get_Draw_As_Radio
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record)
       return Boolean
   is
      function Internal (Check_Menu_Item : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_check_menu_item_get_draw_as_radio");
   begin
      return Boolean'Val (Internal (Get_Object (Check_Menu_Item)));
   end Get_Draw_As_Radio;

   ----------------------
   -- Get_Inconsistent --
   ----------------------

   function Get_Inconsistent
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record)
       return Boolean
   is
      function Internal (Check_Menu_Item : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_check_menu_item_get_inconsistent");
   begin
      return Boolean'Val (Internal (Get_Object (Check_Menu_Item)));
   end Get_Inconsistent;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record;
       Is_Active       : Boolean)
   is
      procedure Internal
         (Check_Menu_Item : System.Address;
          Is_Active       : Integer);
      pragma Import (C, Internal, "gtk_check_menu_item_set_active");
   begin
      Internal (Get_Object (Check_Menu_Item), Boolean'Pos (Is_Active));
   end Set_Active;

   -----------------------
   -- Set_Draw_As_Radio --
   -----------------------

   procedure Set_Draw_As_Radio
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record;
       Draw_As_Radio   : Boolean)
   is
      procedure Internal
         (Check_Menu_Item : System.Address;
          Draw_As_Radio   : Integer);
      pragma Import (C, Internal, "gtk_check_menu_item_set_draw_as_radio");
   begin
      Internal (Get_Object (Check_Menu_Item), Boolean'Pos (Draw_As_Radio));
   end Set_Draw_As_Radio;

   ----------------------
   -- Set_Inconsistent --
   ----------------------

   procedure Set_Inconsistent
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record;
       Setting         : Boolean)
   is
      procedure Internal
         (Check_Menu_Item : System.Address;
          Setting         : Integer);
      pragma Import (C, Internal, "gtk_check_menu_item_set_inconsistent");
   begin
      Internal (Get_Object (Check_Menu_Item), Boolean'Pos (Setting));
   end Set_Inconsistent;

   -------------
   -- Toggled --
   -------------

   procedure Toggled
      (Check_Menu_Item : not null access Gtk_Check_Menu_Item_Record)
   is
      procedure Internal (Check_Menu_Item : System.Address);
      pragma Import (C, Internal, "gtk_check_menu_item_toggled");
   begin
      Internal (Get_Object (Check_Menu_Item));
   end Toggled;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Check_Menu_Item_Record;
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
      (Self : not null access Gtk_Check_Menu_Item_Record)
       return Gtk.Action.Gtk_Action
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_activatable_get_related_action");
      Stub_686 : Gtk.Action.Gtk_Action_Record;
   begin
      return Gtk.Action.Gtk_Action (Get_User_Data (Internal (Get_Object (Self)), Stub_686));
   end Get_Related_Action;

   -------------------------------
   -- Get_Use_Action_Appearance --
   -------------------------------

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Check_Menu_Item_Record) return Boolean
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
      (Self   : not null access Gtk_Check_Menu_Item_Record;
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
      (Self           : not null access Gtk_Check_Menu_Item_Record;
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
      (Self   : not null access Gtk_Check_Menu_Item_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

end Gtk.Check_Menu_Item;
