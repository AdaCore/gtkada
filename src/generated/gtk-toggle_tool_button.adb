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

package body Gtk.Toggle_Tool_Button is

   package Type_Conversion_Gtk_Toggle_Tool_Button is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Toggle_Tool_Button_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Toggle_Tool_Button);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Button : out Gtk_Toggle_Tool_Button) is
   begin
      Button := new Gtk_Toggle_Tool_Button_Record;
      Gtk.Toggle_Tool_Button.Initialize (Button);
   end Gtk_New;

   ------------------------
   -- Gtk_New_From_Stock --
   ------------------------

   procedure Gtk_New_From_Stock
      (Button   : out Gtk_Toggle_Tool_Button;
       Stock_Id : UTF8_String)
   is
   begin
      Button := new Gtk_Toggle_Tool_Button_Record;
      Gtk.Toggle_Tool_Button.Initialize_From_Stock (Button, Stock_Id);
   end Gtk_New_From_Stock;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Button : not null access Gtk_Toggle_Tool_Button_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_toggle_tool_button_new");
   begin
      Set_Object (Button, Internal);
   end Initialize;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
      (Button   : not null access Gtk_Toggle_Tool_Button_Record'Class;
       Stock_Id : UTF8_String)
   is
      function Internal
         (Stock_Id : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_toggle_tool_button_new_from_stock");
      Tmp_Stock_Id : Interfaces.C.Strings.chars_ptr := New_String (Stock_Id);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Stock_Id);
      Free (Tmp_Stock_Id);
      Set_Object (Button, Tmp_Return);
   end Initialize_From_Stock;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
      (Button : not null access Gtk_Toggle_Tool_Button_Record)
       return Boolean
   is
      function Internal (Button : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_toggle_tool_button_get_active");
   begin
      return Boolean'Val (Internal (Get_Object (Button)));
   end Get_Active;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
      (Button    : not null access Gtk_Toggle_Tool_Button_Record;
       Is_Active : Boolean)
   is
      procedure Internal (Button : System.Address; Is_Active : Integer);
      pragma Import (C, Internal, "gtk_toggle_tool_button_set_active");
   begin
      Internal (Get_Object (Button), Boolean'Pos (Is_Active));
   end Set_Active;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Toggle_Tool_Button_Record;
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
      (Self : not null access Gtk_Toggle_Tool_Button_Record)
       return Gtk.Action.Gtk_Action
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_activatable_get_related_action");
      Stub_2701 : Gtk.Action.Gtk_Action_Record;
   begin
      return Gtk.Action.Gtk_Action (Get_User_Data (Internal (Get_Object (Self)), Stub_2701));
   end Get_Related_Action;

   -------------------------------
   -- Get_Use_Action_Appearance --
   -------------------------------

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Toggle_Tool_Button_Record) return Boolean
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
      (Self   : not null access Gtk_Toggle_Tool_Button_Record;
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
      (Self           : not null access Gtk_Toggle_Tool_Button_Record;
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
      (Self   : not null access Gtk_Toggle_Tool_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

end Gtk.Toggle_Tool_Button;
