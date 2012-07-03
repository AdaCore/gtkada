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

package body Gtk.Image_Menu_Item is

   package Type_Conversion_Gtk_Image_Menu_Item is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Image_Menu_Item_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Image_Menu_Item);

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
      Set_Object (Self, Internal);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self  : not null access Gtk_Image_Menu_Item_Record'Class;
       Label : UTF8_String)
   is
      function Internal
         (Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_image_menu_item_new_with_label");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Self, Tmp_Return);
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
         (Stock_Id    : Interfaces.C.Strings.chars_ptr;
          Accel_Group : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_image_menu_item_new_from_stock");
      Tmp_Stock_Id : Interfaces.C.Strings.chars_ptr := New_String (Stock_Id);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Stock_Id, Get_Object_Or_Null (GObject (Accel_Group)));
      Free (Tmp_Stock_Id);
      Set_Object (Self, Tmp_Return);
   end Initialize_From_Stock;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
      (Self  : not null access Gtk_Image_Menu_Item_Record'Class;
       Label : UTF8_String)
   is
      function Internal
         (Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_image_menu_item_new_with_mnemonic");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Self, Tmp_Return);
   end Initialize_With_Mnemonic;

   ---------------------------
   -- Get_Always_Show_Image --
   ---------------------------

   function Get_Always_Show_Image
      (Self : not null access Gtk_Image_Menu_Item_Record) return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_image_menu_item_get_always_show_image");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
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
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_image_menu_item_get_use_stock");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
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
      procedure Internal (Self : System.Address; Always_Show : Integer);
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
      procedure Internal (Self : System.Address; Use_Stock : Integer);
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
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_activatable_get_use_action_appearance");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
   end Get_Use_Action_Appearance;

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
      procedure Internal (Self : System.Address; Use_Appearance : Integer);
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