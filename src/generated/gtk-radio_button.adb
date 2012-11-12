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

package body Gtk.Radio_Button is

   package Type_Conversion_Gtk_Radio_Button is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Radio_Button_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Radio_Button);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Radio_Button : out Gtk_Radio_Button;
       Group        : Gtk.Widget.Widget_SList.GSlist := Widget_SList.Null_List;
       Label        : UTF8_String := "")
   is
   begin
      Radio_Button := new Gtk_Radio_Button_Record;
      Gtk.Radio_Button.Initialize (Radio_Button, Group, Label);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Radio_Button : out Gtk_Radio_Button;
       Group        : access Gtk_Radio_Button_Record'Class;
       Label        : UTF8_String := "")
   is
   begin
      Radio_Button := new Gtk_Radio_Button_Record;
      Gtk.Radio_Button.Initialize (Radio_Button, Group, Label);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
      (Radio_Button : out Gtk_Radio_Button;
       Group        : Gtk.Widget.Widget_SList.GSlist := Widget_SList.Null_List;
       Label        : UTF8_String)
   is
   begin
      Radio_Button := new Gtk_Radio_Button_Record;
      Gtk.Radio_Button.Initialize_With_Mnemonic (Radio_Button, Group, Label);
   end Gtk_New_With_Mnemonic;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
      (Radio_Button : out Gtk_Radio_Button;
       Group        : access Gtk_Radio_Button_Record'Class;
       Label        : UTF8_String)
   is
   begin
      Radio_Button := new Gtk_Radio_Button_Record;
      Gtk.Radio_Button.Initialize_With_Mnemonic (Radio_Button, Group, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Radio_Button : not null access Gtk_Radio_Button_Record'Class;
       Group        : Gtk.Widget.Widget_SList.GSlist := Widget_SList.Null_List;
       Label        : UTF8_String := "")
   is
      function Internal
         (Group : System.Address;
          Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_radio_button_new_with_label");
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
      Set_Object (Radio_Button, Tmp_Return);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Radio_Button : not null access Gtk_Radio_Button_Record'Class;
       Group        : access Gtk_Radio_Button_Record'Class;
       Label        : UTF8_String := "")
   is
      function Internal
         (Group : System.Address;
          Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_radio_button_new_with_label_from_widget");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr;
      Tmp_Return : System.Address;
   begin
      if Label = "" then
         Tmp_Label := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Tmp_Return := Internal (Get_Object_Or_Null (GObject (Group)), Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Radio_Button, Tmp_Return);
   end Initialize;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
      (Radio_Button : not null access Gtk_Radio_Button_Record'Class;
       Group        : Gtk.Widget.Widget_SList.GSlist := Widget_SList.Null_List;
       Label        : UTF8_String)
   is
      function Internal
         (Group : System.Address;
          Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_radio_button_new_with_mnemonic");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Gtk.Widget.Widget_SList.Get_Object (Group), Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Radio_Button, Tmp_Return);
   end Initialize_With_Mnemonic;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
      (Radio_Button : not null access Gtk_Radio_Button_Record'Class;
       Group        : access Gtk_Radio_Button_Record'Class;
       Label        : UTF8_String)
   is
      function Internal
         (Group : System.Address;
          Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_radio_button_new_with_mnemonic_from_widget");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object_Or_Null (GObject (Group)), Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Radio_Button, Tmp_Return);
   end Initialize_With_Mnemonic;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group
      (Radio_Button : not null access Gtk_Radio_Button_Record)
       return Gtk.Widget.Widget_SList.GSlist
   is
      function Internal
         (Radio_Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_button_get_group");
      Tmp_Return : Gtk.Widget.Widget_SList.GSlist;
   begin
      Gtk.Widget.Widget_SList.Set_Object (Tmp_Return, Internal (Get_Object (Radio_Button)));
      return Tmp_Return;
   end Get_Group;

   ----------------
   -- Join_Group --
   ----------------

   procedure Join_Group
      (Radio_Button : not null access Gtk_Radio_Button_Record;
       Group_Source : access Gtk_Radio_Button_Record'Class)
   is
      procedure Internal
         (Radio_Button : System.Address;
          Group_Source : System.Address);
      pragma Import (C, Internal, "gtk_radio_button_join_group");
   begin
      Internal (Get_Object (Radio_Button), Get_Object_Or_Null (GObject (Group_Source)));
   end Join_Group;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group
      (Radio_Button : not null access Gtk_Radio_Button_Record;
       Group        : Gtk.Widget.Widget_SList.GSlist)
   is
      procedure Internal
         (Radio_Button : System.Address;
          Group        : System.Address);
      pragma Import (C, Internal, "gtk_radio_button_set_group");
   begin
      Internal (Get_Object (Radio_Button), Gtk.Widget.Widget_SList.Get_Object (Group));
   end Set_Group;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Radio_Button_Record;
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
      (Self : not null access Gtk_Radio_Button_Record)
       return Gtk.Action.Gtk_Action
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_activatable_get_related_action");
      Stub_2134 : Gtk.Action.Gtk_Action_Record;
   begin
      return Gtk.Action.Gtk_Action (Get_User_Data (Internal (Get_Object (Self)), Stub_2134));
   end Get_Related_Action;

   -------------------------------
   -- Get_Use_Action_Appearance --
   -------------------------------

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Radio_Button_Record) return Boolean
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
      (Self   : not null access Gtk_Radio_Button_Record;
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
      (Self           : not null access Gtk_Radio_Button_Record;
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
      (Self   : not null access Gtk_Radio_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

end Gtk.Radio_Button;
