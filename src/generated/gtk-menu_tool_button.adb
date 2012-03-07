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

pragma Ada_05;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Menu_Tool_Button is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Menu_Tool_Button_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Menu        : out Gtk_Menu_Tool_Button;
       Icon_Widget : Gtk.Widget.Gtk_Widget := null;
       Label       : UTF8_String := "")
   is
   begin
      Menu := new Gtk_Menu_Tool_Button_Record;
      Gtk.Menu_Tool_Button.Initialize (Menu, Icon_Widget, Label);
   end Gtk_New;

   ------------------------
   -- Gtk_New_From_Stock --
   ------------------------

   procedure Gtk_New_From_Stock
      (Menu     : out Gtk_Menu_Tool_Button;
       Stock_Id : UTF8_String)
   is
   begin
      Menu := new Gtk_Menu_Tool_Button_Record;
      Gtk.Menu_Tool_Button.Initialize_From_Stock (Menu, Stock_Id);
   end Gtk_New_From_Stock;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Menu        : not null access Gtk_Menu_Tool_Button_Record'Class;
       Icon_Widget : Gtk.Widget.Gtk_Widget := null;
       Label       : UTF8_String := "")
   is
      function Internal
         (Icon_Widget : System.Address;
          Label       : Interfaces.C.Strings.chars_ptr)
          return System.Address;
      pragma Import (C, Internal, "gtk_menu_tool_button_new");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr;
      Tmp_Return : System.Address;
   begin
      if Label = "" then
         Tmp_Label := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Tmp_Return := Internal (Get_Object_Or_Null (GObject (Icon_Widget)), Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Menu, Tmp_Return);
   end Initialize;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
      (Menu     : not null access Gtk_Menu_Tool_Button_Record'Class;
       Stock_Id : UTF8_String)
   is
      function Internal
         (Stock_Id : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_menu_tool_button_new_from_stock");
      Tmp_Stock_Id : Interfaces.C.Strings.chars_ptr := New_String (Stock_Id);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Stock_Id);
      Free (Tmp_Stock_Id);
      Set_Object (Menu, Tmp_Return);
   end Initialize_From_Stock;

   --------------
   -- Get_Menu --
   --------------

   function Get_Menu
      (Button : not null access Gtk_Menu_Tool_Button_Record)
       return Gtk.Menu.Gtk_Menu
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_tool_button_get_menu");
      Stub_Gtk_Menu : Gtk.Menu.Gtk_Menu_Record;
   begin
      return Gtk.Menu.Gtk_Menu (Get_User_Data (Internal (Get_Object (Button)), Stub_Gtk_Menu));
   end Get_Menu;

   ------------------------------
   -- Set_Arrow_Tooltip_Markup --
   ------------------------------

   procedure Set_Arrow_Tooltip_Markup
      (Button : not null access Gtk_Menu_Tool_Button_Record;
       Markup : UTF8_String)
   is
      procedure Internal
         (Button : System.Address;
          Markup : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_menu_tool_button_set_arrow_tooltip_markup");
      Tmp_Markup : Interfaces.C.Strings.chars_ptr := New_String (Markup);
   begin
      Internal (Get_Object (Button), Tmp_Markup);
      Free (Tmp_Markup);
   end Set_Arrow_Tooltip_Markup;

   ----------------------------
   -- Set_Arrow_Tooltip_Text --
   ----------------------------

   procedure Set_Arrow_Tooltip_Text
      (Button : not null access Gtk_Menu_Tool_Button_Record;
       Text   : UTF8_String)
   is
      procedure Internal
         (Button : System.Address;
          Text   : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_menu_tool_button_set_arrow_tooltip_text");
      Tmp_Text : Interfaces.C.Strings.chars_ptr := New_String (Text);
   begin
      Internal (Get_Object (Button), Tmp_Text);
      Free (Tmp_Text);
   end Set_Arrow_Tooltip_Text;

   --------------
   -- Set_Menu --
   --------------

   procedure Set_Menu
      (Button : not null access Gtk_Menu_Tool_Button_Record;
       Menu   : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Button : System.Address; Menu : System.Address);
      pragma Import (C, Internal, "gtk_menu_tool_button_set_menu");
   begin
      Internal (Get_Object (Button), Get_Object (Menu));
   end Set_Menu;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Menu_Tool_Button_Record;
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
      (Self : not null access Gtk_Menu_Tool_Button_Record)
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
      (Self : not null access Gtk_Menu_Tool_Button_Record) return Boolean
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
      (Self   : not null access Gtk_Menu_Tool_Button_Record;
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
      (Self           : not null access Gtk_Menu_Tool_Button_Record;
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
      (Self   : not null access Gtk_Menu_Tool_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

end Gtk.Menu_Tool_Button;
