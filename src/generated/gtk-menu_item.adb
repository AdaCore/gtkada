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

package body Gtk.Menu_Item is

   package Type_Conversion_Gtk_Menu_Item is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Menu_Item_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Menu_Item);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Menu_Item : out Gtk_Menu_Item) is
   begin
      Menu_Item := new Gtk_Menu_Item_Record;
      Gtk.Menu_Item.Initialize (Menu_Item);
   end Gtk_New;

   ------------------------
   -- Gtk_New_With_Label --
   ------------------------

   procedure Gtk_New_With_Label
      (Menu_Item : out Gtk_Menu_Item;
       Label     : UTF8_String)
   is
   begin
      Menu_Item := new Gtk_Menu_Item_Record;
      Gtk.Menu_Item.Initialize_With_Label (Menu_Item, Label);
   end Gtk_New_With_Label;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
      (Menu_Item : out Gtk_Menu_Item;
       Label     : UTF8_String)
   is
   begin
      Menu_Item := new Gtk_Menu_Item_Record;
      Gtk.Menu_Item.Initialize_With_Mnemonic (Menu_Item, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Menu_Item : not null access Gtk_Menu_Item_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_menu_item_new");
   begin
      Set_Object (Menu_Item, Internal);
   end Initialize;

   ---------------------------
   -- Initialize_With_Label --
   ---------------------------

   procedure Initialize_With_Label
      (Menu_Item : not null access Gtk_Menu_Item_Record'Class;
       Label     : UTF8_String)
   is
      function Internal
         (Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_menu_item_new_with_label");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Menu_Item, Tmp_Return);
   end Initialize_With_Label;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
      (Menu_Item : not null access Gtk_Menu_Item_Record'Class;
       Label     : UTF8_String)
   is
      function Internal
         (Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_menu_item_new_with_mnemonic");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Menu_Item, Tmp_Return);
   end Initialize_With_Mnemonic;

   --------------
   -- Activate --
   --------------

   procedure Activate (Menu_Item : not null access Gtk_Menu_Item_Record) is
      procedure Internal (Menu_Item : System.Address);
      pragma Import (C, Internal, "gtk_menu_item_activate");
   begin
      Internal (Get_Object (Menu_Item));
   end Activate;

   --------------
   -- Deselect --
   --------------

   procedure Deselect (Menu_Item : not null access Gtk_Menu_Item_Record) is
      procedure Internal (Menu_Item : System.Address);
      pragma Import (C, Internal, "gtk_menu_item_deselect");
   begin
      Internal (Get_Object (Menu_Item));
   end Deselect;

   --------------------
   -- Get_Accel_Path --
   --------------------

   function Get_Accel_Path
      (Menu_Item : not null access Gtk_Menu_Item_Record) return UTF8_String
   is
      function Internal
         (Menu_Item : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_menu_item_get_accel_path");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Menu_Item)));
   end Get_Accel_Path;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
      (Menu_Item : not null access Gtk_Menu_Item_Record) return UTF8_String
   is
      function Internal
         (Menu_Item : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_menu_item_get_label");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Menu_Item)));
   end Get_Label;

   ---------------------------
   -- Get_Reserve_Indicator --
   ---------------------------

   function Get_Reserve_Indicator
      (Menu_Item : not null access Gtk_Menu_Item_Record) return Boolean
   is
      function Internal (Menu_Item : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_menu_item_get_reserve_indicator");
   begin
      return Boolean'Val (Internal (Get_Object (Menu_Item)));
   end Get_Reserve_Indicator;

   -------------------------
   -- Get_Right_Justified --
   -------------------------

   function Get_Right_Justified
      (Menu_Item : not null access Gtk_Menu_Item_Record) return Boolean
   is
      function Internal (Menu_Item : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_menu_item_get_right_justified");
   begin
      return Boolean'Val (Internal (Get_Object (Menu_Item)));
   end Get_Right_Justified;

   -----------------
   -- Get_Submenu --
   -----------------

   function Get_Submenu
      (Menu_Item : not null access Gtk_Menu_Item_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Menu_Item : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_item_get_submenu");
      Stub_1806 : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Menu_Item)), Stub_1806));
   end Get_Submenu;

   -----------------------
   -- Get_Use_Underline --
   -----------------------

   function Get_Use_Underline
      (Menu_Item : not null access Gtk_Menu_Item_Record) return Boolean
   is
      function Internal (Menu_Item : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_menu_item_get_use_underline");
   begin
      return Boolean'Val (Internal (Get_Object (Menu_Item)));
   end Get_Use_Underline;

   ----------------
   -- Gtk_Select --
   ----------------

   procedure Gtk_Select (Menu_Item : not null access Gtk_Menu_Item_Record) is
      procedure Internal (Menu_Item : System.Address);
      pragma Import (C, Internal, "gtk_menu_item_select");
   begin
      Internal (Get_Object (Menu_Item));
   end Gtk_Select;

   --------------------
   -- Set_Accel_Path --
   --------------------

   procedure Set_Accel_Path
      (Menu_Item  : not null access Gtk_Menu_Item_Record;
       Accel_Path : UTF8_String := "")
   is
      procedure Internal
         (Menu_Item  : System.Address;
          Accel_Path : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_menu_item_set_accel_path");
      Tmp_Accel_Path : Interfaces.C.Strings.chars_ptr;
   begin
      if Accel_Path = "" then
         Tmp_Accel_Path := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Accel_Path := New_String (Accel_Path);
      end if;
      Internal (Get_Object (Menu_Item), Tmp_Accel_Path);
      Free (Tmp_Accel_Path);
   end Set_Accel_Path;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
      (Menu_Item : not null access Gtk_Menu_Item_Record;
       Label     : UTF8_String)
   is
      procedure Internal
         (Menu_Item : System.Address;
          Label     : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_menu_item_set_label");
      Tmp_Label : Interfaces.C.Strings.chars_ptr := New_String (Label);
   begin
      Internal (Get_Object (Menu_Item), Tmp_Label);
      Free (Tmp_Label);
   end Set_Label;

   ---------------------------
   -- Set_Reserve_Indicator --
   ---------------------------

   procedure Set_Reserve_Indicator
      (Menu_Item : not null access Gtk_Menu_Item_Record;
       Reserve   : Boolean)
   is
      procedure Internal (Menu_Item : System.Address; Reserve : Integer);
      pragma Import (C, Internal, "gtk_menu_item_set_reserve_indicator");
   begin
      Internal (Get_Object (Menu_Item), Boolean'Pos (Reserve));
   end Set_Reserve_Indicator;

   -------------------------
   -- Set_Right_Justified --
   -------------------------

   procedure Set_Right_Justified
      (Menu_Item       : not null access Gtk_Menu_Item_Record;
       Right_Justified : Boolean := True)
   is
      procedure Internal
         (Menu_Item       : System.Address;
          Right_Justified : Integer);
      pragma Import (C, Internal, "gtk_menu_item_set_right_justified");
   begin
      Internal (Get_Object (Menu_Item), Boolean'Pos (Right_Justified));
   end Set_Right_Justified;

   -----------------
   -- Set_Submenu --
   -----------------

   procedure Set_Submenu
      (Menu_Item : not null access Gtk_Menu_Item_Record;
       Submenu   : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Menu_Item : System.Address;
          Submenu   : System.Address);
      pragma Import (C, Internal, "gtk_menu_item_set_submenu");
   begin
      Internal (Get_Object (Menu_Item), Get_Object_Or_Null (GObject (Submenu)));
   end Set_Submenu;

   -----------------------
   -- Set_Use_Underline --
   -----------------------

   procedure Set_Use_Underline
      (Menu_Item : not null access Gtk_Menu_Item_Record;
       Setting   : Boolean)
   is
      procedure Internal (Menu_Item : System.Address; Setting : Integer);
      pragma Import (C, Internal, "gtk_menu_item_set_use_underline");
   begin
      Internal (Get_Object (Menu_Item), Boolean'Pos (Setting));
   end Set_Use_Underline;

   --------------------------
   -- Toggle_Size_Allocate --
   --------------------------

   procedure Toggle_Size_Allocate
      (Menu_Item  : not null access Gtk_Menu_Item_Record;
       Allocation : Gint)
   is
      procedure Internal (Menu_Item : System.Address; Allocation : Gint);
      pragma Import (C, Internal, "gtk_menu_item_toggle_size_allocate");
   begin
      Internal (Get_Object (Menu_Item), Allocation);
   end Toggle_Size_Allocate;

   -------------------------
   -- Toggle_Size_Request --
   -------------------------

   procedure Toggle_Size_Request
      (Menu_Item   : not null access Gtk_Menu_Item_Record;
       Requisition : in out Gint)
   is
      procedure Internal
         (Menu_Item   : System.Address;
          Requisition : in out Gint);
      pragma Import (C, Internal, "gtk_menu_item_toggle_size_request");
   begin
      Internal (Get_Object (Menu_Item), Requisition);
   end Toggle_Size_Request;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Menu_Item_Record;
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
      (Self : not null access Gtk_Menu_Item_Record)
       return Gtk.Action.Gtk_Action
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_activatable_get_related_action");
      Stub_1821 : Gtk.Action.Gtk_Action_Record;
   begin
      return Gtk.Action.Gtk_Action (Get_User_Data (Internal (Get_Object (Self)), Stub_1821));
   end Get_Related_Action;

   -------------------------------
   -- Get_Use_Action_Appearance --
   -------------------------------

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Menu_Item_Record) return Boolean
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
      (Self   : not null access Gtk_Menu_Item_Record;
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
      (Self           : not null access Gtk_Menu_Item_Record;
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
      (Self   : not null access Gtk_Menu_Item_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

end Gtk.Menu_Item;
