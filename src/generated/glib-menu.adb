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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Glib.Menu is

   package Type_Conversion_Gmenu is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type_Menu'Access, Gmenu_Record);
   pragma Unreferenced (Type_Conversion_Gmenu);

   package Type_Conversion_Gmenu_Item is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type_Menu_Item'Access, Gmenu_Item_Record);
   pragma Unreferenced (Type_Conversion_Gmenu_Item);

   -----------
   -- G_New --
   -----------

   procedure G_New (Self : out Gmenu) is
   begin
      Self := new Gmenu_Record;
      Glib.Menu.Initialize (Self);
   end G_New;

   -----------
   -- G_New --
   -----------

   procedure G_New
      (Self            : out Gmenu_Item;
       Label           : UTF8_String := "";
       Detailed_Action : UTF8_String := "")
   is
   begin
      Self := new Gmenu_Item_Record;
      Glib.Menu.Initialize (Self, Label, Detailed_Action);
   end G_New;

   ----------------------
   -- G_New_From_Model --
   ----------------------

   procedure G_New_From_Model
      (Self       : out Gmenu_Item;
       Model      : not null access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Item_Index : Glib.Gint)
   is
   begin
      Self := new Gmenu_Item_Record;
      Glib.Menu.Initialize_From_Model (Self, Model, Item_Index);
   end G_New_From_Model;

   -------------------
   -- G_New_Section --
   -------------------

   procedure G_New_Section
      (Self    : out Gmenu_Item;
       Label   : UTF8_String := "";
       Section : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
   begin
      Self := new Gmenu_Item_Record;
      Glib.Menu.Initialize_Section (Self, Label, Section);
   end G_New_Section;

   -------------------
   -- G_New_Submenu --
   -------------------

   procedure G_New_Submenu
      (Self    : out Gmenu_Item;
       Label   : UTF8_String := "";
       Submenu : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
   begin
      Self := new Gmenu_Item_Record;
      Glib.Menu.Initialize_Submenu (Self, Label, Submenu);
   end G_New_Submenu;

   --------------------
   -- Gmenu_Item_New --
   --------------------

   function Gmenu_Item_New
      (Label           : UTF8_String := "";
       Detailed_Action : UTF8_String := "") return Gmenu_Item
   is
      Self : constant Gmenu_Item := new Gmenu_Item_Record;
   begin
      Glib.Menu.Initialize (Self, Label, Detailed_Action);
      return Self;
   end Gmenu_Item_New;

   -------------------------------
   -- Gmenu_Item_New_From_Model --
   -------------------------------

   function Gmenu_Item_New_From_Model
      (Model      : not null access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Item_Index : Glib.Gint) return Gmenu_Item
   is
      Self : constant Gmenu_Item := new Gmenu_Item_Record;
   begin
      Glib.Menu.Initialize_From_Model (Self, Model, Item_Index);
      return Self;
   end Gmenu_Item_New_From_Model;

   ----------------------------
   -- Gmenu_Item_New_Section --
   ----------------------------

   function Gmenu_Item_New_Section
      (Label   : UTF8_String := "";
       Section : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
       return Gmenu_Item
   is
      Self : constant Gmenu_Item := new Gmenu_Item_Record;
   begin
      Glib.Menu.Initialize_Section (Self, Label, Section);
      return Self;
   end Gmenu_Item_New_Section;

   ----------------------------
   -- Gmenu_Item_New_Submenu --
   ----------------------------

   function Gmenu_Item_New_Submenu
      (Label   : UTF8_String := "";
       Submenu : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
       return Gmenu_Item
   is
      Self : constant Gmenu_Item := new Gmenu_Item_Record;
   begin
      Glib.Menu.Initialize_Submenu (Self, Label, Submenu);
      return Self;
   end Gmenu_Item_New_Submenu;

   ---------------
   -- Gmenu_New --
   ---------------

   function Gmenu_New return Gmenu is
      Self : constant Gmenu := new Gmenu_Record;
   begin
      Glib.Menu.Initialize (Self);
      return Self;
   end Gmenu_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gmenu_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_menu_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self            : not null access Gmenu_Item_Record'Class;
       Label           : UTF8_String := "";
       Detailed_Action : UTF8_String := "")
   is
      function Internal
         (Label           : Gtkada.Types.Chars_Ptr;
          Detailed_Action : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_menu_item_new");
      Tmp_Label           : Gtkada.Types.Chars_Ptr;
      Tmp_Detailed_Action : Gtkada.Types.Chars_Ptr;
      Tmp_Return          : System.Address;
   begin
      if not Self.Is_Created then
         if Label = "" then
            Tmp_Label := Gtkada.Types.Null_Ptr;
         else
            Tmp_Label := New_String (Label);
         end if;
         if Detailed_Action = "" then
            Tmp_Detailed_Action := Gtkada.Types.Null_Ptr;
         else
            Tmp_Detailed_Action := New_String (Detailed_Action);
         end if;
         Tmp_Return := Internal (Tmp_Label, Tmp_Detailed_Action);
         Free (Tmp_Detailed_Action);
         Free (Tmp_Label);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_From_Model --
   ---------------------------

   procedure Initialize_From_Model
      (Self       : not null access Gmenu_Item_Record'Class;
       Model      : not null access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Item_Index : Glib.Gint)
   is
      function Internal
         (Model      : System.Address;
          Item_Index : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "g_menu_item_new_from_model");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Model), Item_Index));
      end if;
   end Initialize_From_Model;

   ------------------------
   -- Initialize_Section --
   ------------------------

   procedure Initialize_Section
      (Self    : not null access Gmenu_Item_Record'Class;
       Label   : UTF8_String := "";
       Section : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      function Internal
         (Label   : Gtkada.Types.Chars_Ptr;
          Section : System.Address) return System.Address;
      pragma Import (C, Internal, "g_menu_item_new_section");
      Tmp_Label  : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         if Label = "" then
            Tmp_Label := Gtkada.Types.Null_Ptr;
         else
            Tmp_Label := New_String (Label);
         end if;
         Tmp_Return := Internal (Tmp_Label, Get_Object (Section));
         Free (Tmp_Label);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_Section;

   ------------------------
   -- Initialize_Submenu --
   ------------------------

   procedure Initialize_Submenu
      (Self    : not null access Gmenu_Item_Record'Class;
       Label   : UTF8_String := "";
       Submenu : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      function Internal
         (Label   : Gtkada.Types.Chars_Ptr;
          Submenu : System.Address) return System.Address;
      pragma Import (C, Internal, "g_menu_item_new_submenu");
      Tmp_Label  : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         if Label = "" then
            Tmp_Label := Gtkada.Types.Null_Ptr;
         else
            Tmp_Label := New_String (Label);
         end if;
         Tmp_Return := Internal (Tmp_Label, Get_Object (Submenu));
         Free (Tmp_Label);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_Submenu;

   ------------
   -- Append --
   ------------

   procedure Append
      (Self            : not null access Gmenu_Record;
       Label           : UTF8_String := "";
       Detailed_Action : UTF8_String := "")
   is
      procedure Internal
         (Self            : System.Address;
          Label           : Gtkada.Types.Chars_Ptr;
          Detailed_Action : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_menu_append");
      Tmp_Label           : Gtkada.Types.Chars_Ptr;
      Tmp_Detailed_Action : Gtkada.Types.Chars_Ptr;
   begin
      if Label = "" then
         Tmp_Label := Gtkada.Types.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      if Detailed_Action = "" then
         Tmp_Detailed_Action := Gtkada.Types.Null_Ptr;
      else
         Tmp_Detailed_Action := New_String (Detailed_Action);
      end if;
      Internal (Get_Object (Self), Tmp_Label, Tmp_Detailed_Action);
      Free (Tmp_Detailed_Action);
      Free (Tmp_Label);
   end Append;

   -----------------
   -- Append_Item --
   -----------------

   procedure Append_Item
      (Self : not null access Gmenu_Record;
       Item : not null access Gmenu_Item_Record'Class)
   is
      procedure Internal (Self : System.Address; Item : System.Address);
      pragma Import (C, Internal, "g_menu_append_item");
   begin
      Internal (Get_Object (Self), Get_Object (Item));
   end Append_Item;

   --------------------
   -- Append_Section --
   --------------------

   procedure Append_Section
      (Self    : not null access Gmenu_Record;
       Label   : UTF8_String := "";
       Section : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal
         (Self    : System.Address;
          Label   : Gtkada.Types.Chars_Ptr;
          Section : System.Address);
      pragma Import (C, Internal, "g_menu_append_section");
      Tmp_Label : Gtkada.Types.Chars_Ptr;
   begin
      if Label = "" then
         Tmp_Label := Gtkada.Types.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Internal (Get_Object (Self), Tmp_Label, Get_Object (Section));
      Free (Tmp_Label);
   end Append_Section;

   --------------------
   -- Append_Submenu --
   --------------------

   procedure Append_Submenu
      (Self    : not null access Gmenu_Record;
       Label   : UTF8_String := "";
       Submenu : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal
         (Self    : System.Address;
          Label   : Gtkada.Types.Chars_Ptr;
          Submenu : System.Address);
      pragma Import (C, Internal, "g_menu_append_submenu");
      Tmp_Label : Gtkada.Types.Chars_Ptr;
   begin
      if Label = "" then
         Tmp_Label := Gtkada.Types.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Internal (Get_Object (Self), Tmp_Label, Get_Object (Submenu));
      Free (Tmp_Label);
   end Append_Submenu;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Self : not null access Gmenu_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_menu_freeze");
   begin
      Internal (Get_Object (Self));
   end Freeze;

   -------------------------
   -- Get_Attribute_Value --
   -------------------------

   function Get_Attribute_Value
      (Self          : not null access Gmenu_Item_Record;
       Attribute     : UTF8_String;
       Expected_Type : Glib.Variant.Gvariant_Type)
       return Glib.Variant.Gvariant
   is
      function Internal
         (Self          : System.Address;
          Attribute     : Gtkada.Types.Chars_Ptr;
          Expected_Type : Glib.Variant.Gvariant_Type) return System.Address;
      pragma Import (C, Internal, "g_menu_item_get_attribute_value");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Attribute, Expected_Type);
      Free (Tmp_Attribute);
      return From_Object (Tmp_Return);
   end Get_Attribute_Value;

   --------------
   -- Get_Link --
   --------------

   function Get_Link
      (Self : not null access Gmenu_Item_Record;
       Link : UTF8_String) return Glib.Menu_Model.Gmenu_Model
   is
      function Internal
         (Self : System.Address;
          Link : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_menu_item_get_link");
      Tmp_Link         : Gtkada.Types.Chars_Ptr := New_String (Link);
      Stub_Gmenu_Model : Glib.Menu_Model.Gmenu_Model_Record;
      Tmp_Return       : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Link);
      Free (Tmp_Link);
      return Glib.Menu_Model.Gmenu_Model (Get_User_Data (Tmp_Return, Stub_Gmenu_Model));
   end Get_Link;

   ------------
   -- Insert --
   ------------

   procedure Insert
      (Self            : not null access Gmenu_Record;
       Position        : Glib.Gint;
       Label           : UTF8_String := "";
       Detailed_Action : UTF8_String := "")
   is
      procedure Internal
         (Self            : System.Address;
          Position        : Glib.Gint;
          Label           : Gtkada.Types.Chars_Ptr;
          Detailed_Action : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_menu_insert");
      Tmp_Label           : Gtkada.Types.Chars_Ptr;
      Tmp_Detailed_Action : Gtkada.Types.Chars_Ptr;
   begin
      if Label = "" then
         Tmp_Label := Gtkada.Types.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      if Detailed_Action = "" then
         Tmp_Detailed_Action := Gtkada.Types.Null_Ptr;
      else
         Tmp_Detailed_Action := New_String (Detailed_Action);
      end if;
      Internal (Get_Object (Self), Position, Tmp_Label, Tmp_Detailed_Action);
      Free (Tmp_Detailed_Action);
      Free (Tmp_Label);
   end Insert;

   -----------------
   -- Insert_Item --
   -----------------

   procedure Insert_Item
      (Self     : not null access Gmenu_Record;
       Position : Glib.Gint;
       Item     : not null access Gmenu_Item_Record'Class)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Glib.Gint;
          Item     : System.Address);
      pragma Import (C, Internal, "g_menu_insert_item");
   begin
      Internal (Get_Object (Self), Position, Get_Object (Item));
   end Insert_Item;

   --------------------
   -- Insert_Section --
   --------------------

   procedure Insert_Section
      (Self     : not null access Gmenu_Record;
       Position : Glib.Gint;
       Label    : UTF8_String := "";
       Section  : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Glib.Gint;
          Label    : Gtkada.Types.Chars_Ptr;
          Section  : System.Address);
      pragma Import (C, Internal, "g_menu_insert_section");
      Tmp_Label : Gtkada.Types.Chars_Ptr;
   begin
      if Label = "" then
         Tmp_Label := Gtkada.Types.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Internal (Get_Object (Self), Position, Tmp_Label, Get_Object (Section));
      Free (Tmp_Label);
   end Insert_Section;

   --------------------
   -- Insert_Submenu --
   --------------------

   procedure Insert_Submenu
      (Self     : not null access Gmenu_Record;
       Position : Glib.Gint;
       Label    : UTF8_String := "";
       Submenu  : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Glib.Gint;
          Label    : Gtkada.Types.Chars_Ptr;
          Submenu  : System.Address);
      pragma Import (C, Internal, "g_menu_insert_submenu");
      Tmp_Label : Gtkada.Types.Chars_Ptr;
   begin
      if Label = "" then
         Tmp_Label := Gtkada.Types.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Internal (Get_Object (Self), Position, Tmp_Label, Get_Object (Submenu));
      Free (Tmp_Label);
   end Insert_Submenu;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
      (Self            : not null access Gmenu_Record;
       Label           : UTF8_String := "";
       Detailed_Action : UTF8_String := "")
   is
      procedure Internal
         (Self            : System.Address;
          Label           : Gtkada.Types.Chars_Ptr;
          Detailed_Action : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_menu_prepend");
      Tmp_Label           : Gtkada.Types.Chars_Ptr;
      Tmp_Detailed_Action : Gtkada.Types.Chars_Ptr;
   begin
      if Label = "" then
         Tmp_Label := Gtkada.Types.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      if Detailed_Action = "" then
         Tmp_Detailed_Action := Gtkada.Types.Null_Ptr;
      else
         Tmp_Detailed_Action := New_String (Detailed_Action);
      end if;
      Internal (Get_Object (Self), Tmp_Label, Tmp_Detailed_Action);
      Free (Tmp_Detailed_Action);
      Free (Tmp_Label);
   end Prepend;

   ------------------
   -- Prepend_Item --
   ------------------

   procedure Prepend_Item
      (Self : not null access Gmenu_Record;
       Item : not null access Gmenu_Item_Record'Class)
   is
      procedure Internal (Self : System.Address; Item : System.Address);
      pragma Import (C, Internal, "g_menu_prepend_item");
   begin
      Internal (Get_Object (Self), Get_Object (Item));
   end Prepend_Item;

   ---------------------
   -- Prepend_Section --
   ---------------------

   procedure Prepend_Section
      (Self    : not null access Gmenu_Record;
       Label   : UTF8_String := "";
       Section : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal
         (Self    : System.Address;
          Label   : Gtkada.Types.Chars_Ptr;
          Section : System.Address);
      pragma Import (C, Internal, "g_menu_prepend_section");
      Tmp_Label : Gtkada.Types.Chars_Ptr;
   begin
      if Label = "" then
         Tmp_Label := Gtkada.Types.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Internal (Get_Object (Self), Tmp_Label, Get_Object (Section));
      Free (Tmp_Label);
   end Prepend_Section;

   ---------------------
   -- Prepend_Submenu --
   ---------------------

   procedure Prepend_Submenu
      (Self    : not null access Gmenu_Record;
       Label   : UTF8_String := "";
       Submenu : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal
         (Self    : System.Address;
          Label   : Gtkada.Types.Chars_Ptr;
          Submenu : System.Address);
      pragma Import (C, Internal, "g_menu_prepend_submenu");
      Tmp_Label : Gtkada.Types.Chars_Ptr;
   begin
      if Label = "" then
         Tmp_Label := Gtkada.Types.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Internal (Get_Object (Self), Tmp_Label, Get_Object (Submenu));
      Free (Tmp_Label);
   end Prepend_Submenu;

   ------------
   -- Remove --
   ------------

   procedure Remove
      (Self     : not null access Gmenu_Record;
       Position : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Position : Glib.Gint);
      pragma Import (C, Internal, "g_menu_remove");
   begin
      Internal (Get_Object (Self), Position);
   end Remove;

   ----------------
   -- Remove_All --
   ----------------

   procedure Remove_All (Self : not null access Gmenu_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_menu_remove_all");
   begin
      Internal (Get_Object (Self));
   end Remove_All;

   ---------------------------------
   -- Set_Action_And_Target_Value --
   ---------------------------------

   procedure Set_Action_And_Target_Value
      (Self         : not null access Gmenu_Item_Record;
       Action       : UTF8_String := "";
       Target_Value : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self         : System.Address;
          Action       : Gtkada.Types.Chars_Ptr;
          Target_Value : System.Address);
      pragma Import (C, Internal, "g_menu_item_set_action_and_target_value");
      Tmp_Action : Gtkada.Types.Chars_Ptr;
   begin
      if Action = "" then
         Tmp_Action := Gtkada.Types.Null_Ptr;
      else
         Tmp_Action := New_String (Action);
      end if;
      Internal (Get_Object (Self), Tmp_Action, Get_Object (Target_Value));
      Free (Tmp_Action);
   end Set_Action_And_Target_Value;

   -------------------------
   -- Set_Attribute_Value --
   -------------------------

   procedure Set_Attribute_Value
      (Self      : not null access Gmenu_Item_Record;
       Attribute : UTF8_String;
       Value     : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr;
          Value     : System.Address);
      pragma Import (C, Internal, "g_menu_item_set_attribute_value");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Self), Tmp_Attribute, Get_Object (Value));
      Free (Tmp_Attribute);
   end Set_Attribute_Value;

   -------------------------
   -- Set_Detailed_Action --
   -------------------------

   procedure Set_Detailed_Action
      (Self            : not null access Gmenu_Item_Record;
       Detailed_Action : UTF8_String)
   is
      procedure Internal
         (Self            : System.Address;
          Detailed_Action : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_menu_item_set_detailed_action");
      Tmp_Detailed_Action : Gtkada.Types.Chars_Ptr := New_String (Detailed_Action);
   begin
      Internal (Get_Object (Self), Tmp_Detailed_Action);
      Free (Tmp_Detailed_Action);
   end Set_Detailed_Action;

   --------------
   -- Set_Icon --
   --------------

   procedure Set_Icon
      (Self : not null access Gmenu_Item_Record;
       Icon : Glib.G_Icon.G_Icon)
   is
      procedure Internal (Self : System.Address; Icon : Glib.G_Icon.G_Icon);
      pragma Import (C, Internal, "g_menu_item_set_icon");
   begin
      Internal (Get_Object (Self), Icon);
   end Set_Icon;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
      (Self  : not null access Gmenu_Item_Record;
       Label : UTF8_String := "")
   is
      procedure Internal
         (Self  : System.Address;
          Label : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_menu_item_set_label");
      Tmp_Label : Gtkada.Types.Chars_Ptr;
   begin
      if Label = "" then
         Tmp_Label := Gtkada.Types.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Internal (Get_Object (Self), Tmp_Label);
      Free (Tmp_Label);
   end Set_Label;

   --------------
   -- Set_Link --
   --------------

   procedure Set_Link
      (Self  : not null access Gmenu_Item_Record;
       Link  : UTF8_String;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal
         (Self  : System.Address;
          Link  : Gtkada.Types.Chars_Ptr;
          Model : System.Address);
      pragma Import (C, Internal, "g_menu_item_set_link");
      Tmp_Link : Gtkada.Types.Chars_Ptr := New_String (Link);
   begin
      Internal (Get_Object (Self), Tmp_Link, Get_Object_Or_Null (GObject (Model)));
      Free (Tmp_Link);
   end Set_Link;

   -----------------
   -- Set_Section --
   -----------------

   procedure Set_Section
      (Self    : not null access Gmenu_Item_Record;
       Section : access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal (Self : System.Address; Section : System.Address);
      pragma Import (C, Internal, "g_menu_item_set_section");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Section)));
   end Set_Section;

   -----------------
   -- Set_Submenu --
   -----------------

   procedure Set_Submenu
      (Self    : not null access Gmenu_Item_Record;
       Submenu : access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal (Self : System.Address; Submenu : System.Address);
      pragma Import (C, Internal, "g_menu_item_set_submenu");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Submenu)));
   end Set_Submenu;

end Glib.Menu;
