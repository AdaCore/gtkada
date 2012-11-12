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
with Gtkada.Bindings;            use Gtkada.Bindings;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Link_Button is

   package Type_Conversion_Gtk_Link_Button is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Link_Button_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Link_Button);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Link_Button; URI : UTF8_String) is
   begin
      Widget := new Gtk_Link_Button_Record;
      Gtk.Link_Button.Initialize (Widget, URI);
   end Gtk_New;

   ------------------------
   -- Gtk_New_With_Label --
   ------------------------

   procedure Gtk_New_With_Label
      (Widget : out Gtk_Link_Button;
       URI    : UTF8_String;
       Label  : UTF8_String := "")
   is
   begin
      Widget := new Gtk_Link_Button_Record;
      Gtk.Link_Button.Initialize_With_Label (Widget, URI, Label);
   end Gtk_New_With_Label;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Widget : not null access Gtk_Link_Button_Record'Class;
       URI    : UTF8_String)
   is
      function Internal
         (URI : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_link_button_new");
      Tmp_URI    : Interfaces.C.Strings.chars_ptr := New_String (URI);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_URI);
      Free (Tmp_URI);
      Set_Object (Widget, Tmp_Return);
   end Initialize;

   ---------------------------
   -- Initialize_With_Label --
   ---------------------------

   procedure Initialize_With_Label
      (Widget : not null access Gtk_Link_Button_Record'Class;
       URI    : UTF8_String;
       Label  : UTF8_String := "")
   is
      function Internal
         (URI   : Interfaces.C.Strings.chars_ptr;
          Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_link_button_new_with_label");
      Tmp_URI    : Interfaces.C.Strings.chars_ptr := New_String (URI);
      Tmp_Label  : Interfaces.C.Strings.chars_ptr;
      Tmp_Return : System.Address;
   begin
      if Label = "" then
         Tmp_Label := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Tmp_Return := Internal (Tmp_URI, Tmp_Label);
      Free (Tmp_Label);
      Free (Tmp_URI);
      Set_Object (Widget, Tmp_Return);
   end Initialize_With_Label;

   -------------
   -- Get_Uri --
   -------------

   function Get_Uri
      (Widget : not null access Gtk_Link_Button_Record) return UTF8_String
   is
      function Internal
         (Widget : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_link_button_get_uri");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Widget)));
   end Get_Uri;

   -----------------
   -- Get_Visited --
   -----------------

   function Get_Visited
      (Widget : not null access Gtk_Link_Button_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_link_button_get_visited");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Visited;

   -------------
   -- Set_Uri --
   -------------

   procedure Set_Uri
      (Widget : not null access Gtk_Link_Button_Record;
       URI    : UTF8_String)
   is
      procedure Internal
         (Widget : System.Address;
          URI    : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_link_button_set_uri");
      Tmp_URI : Interfaces.C.Strings.chars_ptr := New_String (URI);
   begin
      Internal (Get_Object (Widget), Tmp_URI);
      Free (Tmp_URI);
   end Set_Uri;

   -----------------
   -- Set_Visited --
   -----------------

   procedure Set_Visited
      (Widget  : not null access Gtk_Link_Button_Record;
       Visited : Boolean)
   is
      procedure Internal (Widget : System.Address; Visited : Integer);
      pragma Import (C, Internal, "gtk_link_button_set_visited");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Visited));
   end Set_Visited;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Link_Button_Record;
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
      (Self : not null access Gtk_Link_Button_Record)
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
      (Self : not null access Gtk_Link_Button_Record) return Boolean
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
      (Self   : not null access Gtk_Link_Button_Record;
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
      (Self           : not null access Gtk_Link_Button_Record;
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
      (Self   : not null access Gtk_Link_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

end Gtk.Link_Button;
