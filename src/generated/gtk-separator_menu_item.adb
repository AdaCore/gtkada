------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Separator_Menu_Item is

   package Type_Conversion_Gtk_Separator_Menu_Item is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Separator_Menu_Item_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Separator_Menu_Item);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Separator_Menu_Item) is
   begin
      Widget := new Gtk_Separator_Menu_Item_Record;
      Gtk.Separator_Menu_Item.Initialize (Widget);
   end Gtk_New;

   ---------------------------------
   -- Gtk_Separator_Menu_Item_New --
   ---------------------------------

   function Gtk_Separator_Menu_Item_New return Gtk_Separator_Menu_Item is
      Widget : constant Gtk_Separator_Menu_Item := new Gtk_Separator_Menu_Item_Record;
   begin
      Gtk.Separator_Menu_Item.Initialize (Widget);
      return Widget;
   end Gtk_Separator_Menu_Item_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Widget : not null access Gtk_Separator_Menu_Item_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_separator_menu_item_new");
   begin
      if not Widget.Is_Created then
         Set_Object (Widget, Internal);
      end if;
   end Initialize;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Separator_Menu_Item_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_do_set_related_action");
   begin
      Internal (Get_Object (Self), Get_Object (Action));
   end Do_Set_Related_Action;

   ---------------------
   -- Get_Action_Name --
   ---------------------

   function Get_Action_Name
      (Self : not null access Gtk_Separator_Menu_Item_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_actionable_get_action_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Action_Name;

   -----------------------------
   -- Get_Action_Target_Value --
   -----------------------------

   function Get_Action_Target_Value
      (Self : not null access Gtk_Separator_Menu_Item_Record)
       return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_actionable_get_action_target_value");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Action_Target_Value;

   ------------------------
   -- Get_Related_Action --
   ------------------------

   function Get_Related_Action
      (Self : not null access Gtk_Separator_Menu_Item_Record)
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
      (Self : not null access Gtk_Separator_Menu_Item_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_activatable_get_use_action_appearance");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Action_Appearance;

   ---------------------
   -- Set_Action_Name --
   ---------------------

   procedure Set_Action_Name
      (Self        : not null access Gtk_Separator_Menu_Item_Record;
       Action_Name : UTF8_String := "")
   is
      procedure Internal
         (Self        : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_actionable_set_action_name");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Action_Name = "" then
         Tmp_Action_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Action_Name := New_String (Action_Name);
      end if;
      Internal (Get_Object (Self), Tmp_Action_Name);
      Free (Tmp_Action_Name);
   end Set_Action_Name;

   -----------------------------
   -- Set_Action_Target_Value --
   -----------------------------

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Separator_Menu_Item_Record;
       Target_Value : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self         : System.Address;
          Target_Value : System.Address);
      pragma Import (C, Internal, "gtk_actionable_set_action_target_value");
   begin
      Internal (Get_Object (Self), Get_Object (Target_Value));
   end Set_Action_Target_Value;

   ------------------------------
   -- Set_Detailed_Action_Name --
   ------------------------------

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Separator_Menu_Item_Record;
       Detailed_Action_Name : UTF8_String)
   is
      procedure Internal
         (Self                 : System.Address;
          Detailed_Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_actionable_set_detailed_action_name");
      Tmp_Detailed_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Detailed_Action_Name);
   begin
      Internal (Get_Object (Self), Tmp_Detailed_Action_Name);
      Free (Tmp_Detailed_Action_Name);
   end Set_Detailed_Action_Name;

   ------------------------
   -- Set_Related_Action --
   ------------------------

   procedure Set_Related_Action
      (Self   : not null access Gtk_Separator_Menu_Item_Record;
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
      (Self           : not null access Gtk_Separator_Menu_Item_Record;
       Use_Appearance : Boolean)
   is
      procedure Internal
         (Self           : System.Address;
          Use_Appearance : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_activatable_set_use_action_appearance");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Appearance));
   end Set_Use_Action_Appearance;

   ----------------------------
   -- Sync_Action_Properties --
   ----------------------------

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Separator_Menu_Item_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

end Gtk.Separator_Menu_Item;
