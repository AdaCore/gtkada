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

package body Gtk.Image_Menu_Item is

   package Type_Conversion_Gtk_Image_Menu_Item is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Image_Menu_Item_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Image_Menu_Item);

   -----------------------------
   -- Gtk_Image_Menu_Item_New --
   -----------------------------

   function Gtk_Image_Menu_Item_New return Gtk_Image_Menu_Item is
      Self : constant Gtk_Image_Menu_Item := new Gtk_Image_Menu_Item_Record;
   begin
      Gtk.Image_Menu_Item.Initialize (Self);
      return Self;
   end Gtk_Image_Menu_Item_New;

   ----------------------------------------
   -- Gtk_Image_Menu_Item_New_From_Stock --
   ----------------------------------------

   function Gtk_Image_Menu_Item_New_From_Stock
      (Stock_Id    : UTF8_String;
       Accel_Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class)
       return Gtk_Image_Menu_Item
   is
      Self : constant Gtk_Image_Menu_Item := new Gtk_Image_Menu_Item_Record;
   begin
      Gtk.Image_Menu_Item.Initialize_From_Stock (Self, Stock_Id, Accel_Group);
      return Self;
   end Gtk_Image_Menu_Item_New_From_Stock;

   ----------------------------------------
   -- Gtk_Image_Menu_Item_New_With_Label --
   ----------------------------------------

   function Gtk_Image_Menu_Item_New_With_Label
      (Label : UTF8_String) return Gtk_Image_Menu_Item
   is
      Self : constant Gtk_Image_Menu_Item := new Gtk_Image_Menu_Item_Record;
   begin
      Gtk.Image_Menu_Item.Initialize (Self, Label);
      return Self;
   end Gtk_Image_Menu_Item_New_With_Label;

   -------------------------------------------
   -- Gtk_Image_Menu_Item_New_With_Mnemonic --
   -------------------------------------------

   function Gtk_Image_Menu_Item_New_With_Mnemonic
      (Label : UTF8_String) return Gtk_Image_Menu_Item
   is
      Self : constant Gtk_Image_Menu_Item := new Gtk_Image_Menu_Item_Record;
   begin
      Gtk.Image_Menu_Item.Initialize_With_Mnemonic (Self, Label);
      return Self;
   end Gtk_Image_Menu_Item_New_With_Mnemonic;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Image_Menu_Item) is
   begin
      Self := new Gtk_Image_Menu_Item_Record;
      Gtk.Image_Menu_Item.Initialize (Self);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Image_Menu_Item; Label : UTF8_String) is
   begin
      Self := new Gtk_Image_Menu_Item_Record;
      Gtk.Image_Menu_Item.Initialize (Self, Label);
   end Gtk_New;

   ------------------------
   -- Gtk_New_From_Stock --
   ------------------------

   procedure Gtk_New_From_Stock
      (Self        : out Gtk_Image_Menu_Item;
       Stock_Id    : UTF8_String;
       Accel_Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class)
   is
   begin
      Self := new Gtk_Image_Menu_Item_Record;
      Gtk.Image_Menu_Item.Initialize_From_Stock (Self, Stock_Id, Accel_Group);
   end Gtk_New_From_Stock;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
      (Self  : out Gtk_Image_Menu_Item;
       Label : UTF8_String)
   is
   begin
      Self := new Gtk_Image_Menu_Item_Record;
      Gtk.Image_Menu_Item.Initialize_With_Mnemonic (Self, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Image_Menu_Item_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_image_menu_item_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self  : not null access Gtk_Image_Menu_Item_Record'Class;
       Label : UTF8_String)
   is
      function Internal
         (Label : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_image_menu_item_new_with_label");
      Tmp_Label  : Gtkada.Types.Chars_Ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Label);
         Free (Tmp_Label);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
      (Self        : not null access Gtk_Image_Menu_Item_Record'Class;
       Stock_Id    : UTF8_String;
       Accel_Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class)
   is
      function Internal
         (Stock_Id    : Gtkada.Types.Chars_Ptr;
          Accel_Group : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_image_menu_item_new_from_stock");
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr := New_String (Stock_Id);
      Tmp_Return   : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Stock_Id, Get_Object_Or_Null (GObject (Accel_Group)));
         Free (Tmp_Stock_Id);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_From_Stock;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
      (Self  : not null access Gtk_Image_Menu_Item_Record'Class;
       Label : UTF8_String)
   is
      function Internal
         (Label : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_image_menu_item_new_with_mnemonic");
      Tmp_Label  : Gtkada.Types.Chars_Ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Label);
         Free (Tmp_Label);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_With_Mnemonic;

   ---------------------------
   -- Get_Always_Show_Image --
   ---------------------------

   function Get_Always_Show_Image
      (Self : not null access Gtk_Image_Menu_Item_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_image_menu_item_get_always_show_image");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Always_Show_Image;

   ---------------
   -- Get_Image --
   ---------------

   function Get_Image
      (Self : not null access Gtk_Image_Menu_Item_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_image_menu_item_get_image");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Image;

   -------------------
   -- Get_Use_Stock --
   -------------------

   function Get_Use_Stock
      (Self : not null access Gtk_Image_Menu_Item_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_image_menu_item_get_use_stock");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Stock;

   ---------------------
   -- Set_Accel_Group --
   ---------------------

   procedure Set_Accel_Group
      (Self        : not null access Gtk_Image_Menu_Item_Record;
       Accel_Group : not null access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class)
   is
      procedure Internal
         (Self        : System.Address;
          Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_image_menu_item_set_accel_group");
   begin
      Internal (Get_Object (Self), Get_Object (Accel_Group));
   end Set_Accel_Group;

   ---------------------------
   -- Set_Always_Show_Image --
   ---------------------------

   procedure Set_Always_Show_Image
      (Self        : not null access Gtk_Image_Menu_Item_Record;
       Always_Show : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Always_Show : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_image_menu_item_set_always_show_image");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Always_Show));
   end Set_Always_Show_Image;

   ---------------
   -- Set_Image --
   ---------------

   procedure Set_Image
      (Self  : not null access Gtk_Image_Menu_Item_Record;
       Image : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Image : System.Address);
      pragma Import (C, Internal, "gtk_image_menu_item_set_image");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Image)));
   end Set_Image;

   -------------------
   -- Set_Use_Stock --
   -------------------

   procedure Set_Use_Stock
      (Self      : not null access Gtk_Image_Menu_Item_Record;
       Use_Stock : Boolean)
   is
      procedure Internal (Self : System.Address; Use_Stock : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_image_menu_item_set_use_stock");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Stock));
   end Set_Use_Stock;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Image_Menu_Item_Record;
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
      (Self : not null access Gtk_Image_Menu_Item_Record) return UTF8_String
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
      (Self : not null access Gtk_Image_Menu_Item_Record)
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
      (Self : not null access Gtk_Image_Menu_Item_Record)
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
      (Self : not null access Gtk_Image_Menu_Item_Record) return Boolean
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
      (Self        : not null access Gtk_Image_Menu_Item_Record;
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
      (Self         : not null access Gtk_Image_Menu_Item_Record;
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
      (Self                 : not null access Gtk_Image_Menu_Item_Record;
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
      (Self   : not null access Gtk_Image_Menu_Item_Record;
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
      (Self           : not null access Gtk_Image_Menu_Item_Record;
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
      (Self   : not null access Gtk_Image_Menu_Item_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

end Gtk.Image_Menu_Item;
