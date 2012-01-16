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

package body Gtk.Link_Button is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Link_Button_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Link_Button; Uri : UTF8_String) is
   begin
      Self := new Gtk_Link_Button_Record;
      Gtk.Link_Button.Initialize (Self, Uri);
   end Gtk_New;

   ------------------------
   -- Gtk_New_With_Label --
   ------------------------

   procedure Gtk_New_With_Label
      (Self  : out Gtk_Link_Button;
       Uri   : UTF8_String;
       Label : UTF8_String)
   is
   begin
      Self := new Gtk_Link_Button_Record;
      Gtk.Link_Button.Initialize_With_Label (Self, Uri, Label);
   end Gtk_New_With_Label;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : access Gtk_Link_Button_Record'Class;
       Uri  : UTF8_String)
   is
      function Internal
         (Uri : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_link_button_new");
      Tmp_Uri    : Interfaces.C.Strings.chars_ptr := New_String (Uri);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Uri);
      Free (Tmp_Uri);
      Set_Object (Self, Tmp_Return);
   end Initialize;

   ---------------------------
   -- Initialize_With_Label --
   ---------------------------

   procedure Initialize_With_Label
      (Self  : access Gtk_Link_Button_Record'Class;
       Uri   : UTF8_String;
       Label : UTF8_String)
   is
      function Internal
         (Uri   : Interfaces.C.Strings.chars_ptr;
          Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_link_button_new_with_label");
      Tmp_Uri    : Interfaces.C.Strings.chars_ptr := New_String (Uri);
      Tmp_Label  : Interfaces.C.Strings.chars_ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Uri, Tmp_Label);
      Free (Tmp_Uri);
      Free (Tmp_Label);
      Set_Object (Self, Tmp_Return);
   end Initialize_With_Label;

   -------------
   -- Get_Uri --
   -------------

   function Get_Uri
      (Self : access Gtk_Link_Button_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_link_button_get_uri");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Self)));
   end Get_Uri;

   -----------------
   -- Get_Visited --
   -----------------

   function Get_Visited
      (Self : access Gtk_Link_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_link_button_get_visited");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
   end Get_Visited;

   -------------
   -- Set_Uri --
   -------------

   procedure Set_Uri
      (Self : access Gtk_Link_Button_Record;
       Uri  : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Uri  : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_link_button_set_uri");
      Tmp_Uri : Interfaces.C.Strings.chars_ptr := New_String (Uri);
   begin
      Internal (Get_Object (Self), Tmp_Uri);
      Free (Tmp_Uri);
   end Set_Uri;

   -----------------
   -- Set_Visited --
   -----------------

   procedure Set_Visited
      (Self    : access Gtk_Link_Button_Record;
       Visited : Boolean)
   is
      procedure Internal (Self : System.Address; Visited : Integer);
      pragma Import (C, Internal, "gtk_link_button_set_visited");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Visited));
   end Set_Visited;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : access Gtk_Link_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
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
      (Self : access Gtk_Link_Button_Record) return Gtk.Action.Gtk_Action
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
      (Self : access Gtk_Link_Button_Record) return Boolean
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
      (Self   : access Gtk_Link_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
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
      (Self           : access Gtk_Link_Button_Record;
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
      (Self   : access Gtk_Link_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object (Action));
   end Sync_Action_Properties;

end Gtk.Link_Button;
