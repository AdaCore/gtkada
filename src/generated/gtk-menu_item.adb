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
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Menu_Item is

   package Type_Conversion_Gtk_Menu_Item is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Menu_Item_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Menu_Item);

   -----------------------
   -- Gtk_Menu_Item_New --
   -----------------------

   function Gtk_Menu_Item_New return Gtk_Menu_Item is
      Menu_Item : constant Gtk_Menu_Item := new Gtk_Menu_Item_Record;
   begin
      Gtk.Menu_Item.Initialize (Menu_Item);
      return Menu_Item;
   end Gtk_Menu_Item_New;

   ----------------------------------
   -- Gtk_Menu_Item_New_With_Label --
   ----------------------------------

   function Gtk_Menu_Item_New_With_Label
      (Label : UTF8_String) return Gtk_Menu_Item
   is
      Menu_Item : constant Gtk_Menu_Item := new Gtk_Menu_Item_Record;
   begin
      Gtk.Menu_Item.Initialize_With_Label (Menu_Item, Label);
      return Menu_Item;
   end Gtk_Menu_Item_New_With_Label;

   -------------------------------------
   -- Gtk_Menu_Item_New_With_Mnemonic --
   -------------------------------------

   function Gtk_Menu_Item_New_With_Mnemonic
      (Label : UTF8_String) return Gtk_Menu_Item
   is
      Menu_Item : constant Gtk_Menu_Item := new Gtk_Menu_Item_Record;
   begin
      Gtk.Menu_Item.Initialize_With_Mnemonic (Menu_Item, Label);
      return Menu_Item;
   end Gtk_Menu_Item_New_With_Mnemonic;

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
      if not Menu_Item.Is_Created then
         Set_Object (Menu_Item, Internal);
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_With_Label --
   ---------------------------

   procedure Initialize_With_Label
      (Menu_Item : not null access Gtk_Menu_Item_Record'Class;
       Label     : UTF8_String)
   is
      function Internal
         (Label : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_menu_item_new_with_label");
      Tmp_Label  : Gtkada.Types.Chars_Ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      if not Menu_Item.Is_Created then
         Tmp_Return := Internal (Tmp_Label);
         Free (Tmp_Label);
         Set_Object (Menu_Item, Tmp_Return);
      end if;
   end Initialize_With_Label;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
      (Menu_Item : not null access Gtk_Menu_Item_Record'Class;
       Label     : UTF8_String)
   is
      function Internal
         (Label : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_menu_item_new_with_mnemonic");
      Tmp_Label  : Gtkada.Types.Chars_Ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      if not Menu_Item.Is_Created then
         Tmp_Return := Internal (Tmp_Label);
         Free (Tmp_Label);
         Set_Object (Menu_Item, Tmp_Return);
      end if;
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
         (Menu_Item : System.Address) return Gtkada.Types.Chars_Ptr;
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
         (Menu_Item : System.Address) return Gtkada.Types.Chars_Ptr;
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
      function Internal (Menu_Item : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_menu_item_get_reserve_indicator");
   begin
      return Internal (Get_Object (Menu_Item)) /= 0;
   end Get_Reserve_Indicator;

   -------------------------
   -- Get_Right_Justified --
   -------------------------

   function Get_Right_Justified
      (Menu_Item : not null access Gtk_Menu_Item_Record) return Boolean
   is
      function Internal (Menu_Item : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_menu_item_get_right_justified");
   begin
      return Internal (Get_Object (Menu_Item)) /= 0;
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
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Menu_Item)), Stub_Gtk_Widget));
   end Get_Submenu;

   -----------------------
   -- Get_Use_Underline --
   -----------------------

   function Get_Use_Underline
      (Menu_Item : not null access Gtk_Menu_Item_Record) return Boolean
   is
      function Internal (Menu_Item : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_menu_item_get_use_underline");
   begin
      return Internal (Get_Object (Menu_Item)) /= 0;
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
          Accel_Path : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_menu_item_set_accel_path");
      Tmp_Accel_Path : Gtkada.Types.Chars_Ptr;
   begin
      if Accel_Path = "" then
         Tmp_Accel_Path := Gtkada.Types.Null_Ptr;
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
          Label     : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_menu_item_set_label");
      Tmp_Label : Gtkada.Types.Chars_Ptr := New_String (Label);
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
      procedure Internal
         (Menu_Item : System.Address;
          Reserve   : Glib.Gboolean);
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
          Right_Justified : Glib.Gboolean);
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
      procedure Internal
         (Menu_Item : System.Address;
          Setting   : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_menu_item_set_use_underline");
   begin
      Internal (Get_Object (Menu_Item), Boolean'Pos (Setting));
   end Set_Use_Underline;

   --------------------------
   -- Toggle_Size_Allocate --
   --------------------------

   procedure Toggle_Size_Allocate
      (Menu_Item  : not null access Gtk_Menu_Item_Record;
       Allocation : Glib.Gint)
   is
      procedure Internal
         (Menu_Item  : System.Address;
          Allocation : Glib.Gint);
      pragma Import (C, Internal, "gtk_menu_item_toggle_size_allocate");
   begin
      Internal (Get_Object (Menu_Item), Allocation);
   end Toggle_Size_Allocate;

   -------------------------
   -- Toggle_Size_Request --
   -------------------------

   procedure Toggle_Size_Request
      (Menu_Item   : not null access Gtk_Menu_Item_Record;
       Requisition : in out Glib.Gint)
   is
      procedure Internal
         (Menu_Item   : System.Address;
          Requisition : in out Glib.Gint);
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

   ---------------------
   -- Get_Action_Name --
   ---------------------

   function Get_Action_Name
      (Self : not null access Gtk_Menu_Item_Record) return UTF8_String
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
      (Self : not null access Gtk_Menu_Item_Record)
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
      (Self : not null access Gtk_Menu_Item_Record)
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
      (Self : not null access Gtk_Menu_Item_Record) return Boolean
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
      (Self        : not null access Gtk_Menu_Item_Record;
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
      (Self         : not null access Gtk_Menu_Item_Record;
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
      (Self                 : not null access Gtk_Menu_Item_Record;
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
      (Self   : not null access Gtk_Menu_Item_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Menu_Item_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Menu_Item_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Menu_Item_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Menu_Item_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Menu_Item_Address_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Menu_Item_Address_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Address_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Address_Void);

   procedure Connect
      (Object  : access Gtk_Menu_Item_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Item_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Menu_Item_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Item_Gint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Menu_Item_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Item_Address_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Item_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Item_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Item_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Address_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Address_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Address_Void);

   procedure Marsh_GObject_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Menu_Item_Address_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Menu_Item_Address_Void);

   procedure Marsh_Gtk_Menu_Item_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Menu_Item_Gint_Void);

   procedure Marsh_Gtk_Menu_Item_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Menu_Item_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Menu_Item_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Item_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Menu_Item_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Menu_Item_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Item_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Menu_Item_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Menu_Item_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Item_Address_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Menu_Item_Address_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Item_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Item_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Item_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Address_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Address_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   --------------------------------
   -- Marsh_GObject_Address_Void --
   --------------------------------

   procedure Marsh_GObject_Address_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Address_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Address (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Address_Void;

   -----------------------------
   -- Marsh_GObject_Gint_Void --
   -----------------------------

   procedure Marsh_GObject_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gint_Void;

   ------------------------
   -- Marsh_GObject_Void --
   ------------------------

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   --------------------------------------
   -- Marsh_Gtk_Menu_Item_Address_Void --
   --------------------------------------

   procedure Marsh_Gtk_Menu_Item_Address_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Menu_Item_Address_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Menu_Item := Gtk_Menu_Item (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Address (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Menu_Item_Address_Void;

   -----------------------------------
   -- Marsh_Gtk_Menu_Item_Gint_Void --
   -----------------------------------

   procedure Marsh_Gtk_Menu_Item_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Menu_Item_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Menu_Item := Gtk_Menu_Item (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gint (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Menu_Item_Gint_Void;

   ------------------------------
   -- Marsh_Gtk_Menu_Item_Void --
   ------------------------------

   procedure Marsh_Gtk_Menu_Item_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Menu_Item_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Menu_Item := Gtk_Menu_Item (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Menu_Item_Void;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Menu_Item_Record;
       Call  : Cb_Gtk_Menu_Item_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate" & ASCII.NUL, Call, After);
   end On_Activate;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Menu_Item_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate" & ASCII.NUL, Call, After, Slot);
   end On_Activate;

   ----------------------
   -- On_Activate_Item --
   ----------------------

   procedure On_Activate_Item
      (Self  : not null access Gtk_Menu_Item_Record;
       Call  : Cb_Gtk_Menu_Item_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate-item" & ASCII.NUL, Call, After);
   end On_Activate_Item;

   ----------------------
   -- On_Activate_Item --
   ----------------------

   procedure On_Activate_Item
      (Self  : not null access Gtk_Menu_Item_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate-item" & ASCII.NUL, Call, After, Slot);
   end On_Activate_Item;

   -----------------
   -- On_Deselect --
   -----------------

   procedure On_Deselect
      (Self  : not null access Gtk_Menu_Item_Record;
       Call  : Cb_Gtk_Menu_Item_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "deselect" & ASCII.NUL, Call, After);
   end On_Deselect;

   -----------------
   -- On_Deselect --
   -----------------

   procedure On_Deselect
      (Self  : not null access Gtk_Menu_Item_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "deselect" & ASCII.NUL, Call, After, Slot);
   end On_Deselect;

   ---------------
   -- On_Select --
   ---------------

   procedure On_Select
      (Self  : not null access Gtk_Menu_Item_Record;
       Call  : Cb_Gtk_Menu_Item_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "select" & ASCII.NUL, Call, After);
   end On_Select;

   ---------------
   -- On_Select --
   ---------------

   procedure On_Select
      (Self  : not null access Gtk_Menu_Item_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "select" & ASCII.NUL, Call, After, Slot);
   end On_Select;

   -----------------------------
   -- On_Toggle_Size_Allocate --
   -----------------------------

   procedure On_Toggle_Size_Allocate
      (Self  : not null access Gtk_Menu_Item_Record;
       Call  : Cb_Gtk_Menu_Item_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "toggle-size-allocate" & ASCII.NUL, Call, After);
   end On_Toggle_Size_Allocate;

   -----------------------------
   -- On_Toggle_Size_Allocate --
   -----------------------------

   procedure On_Toggle_Size_Allocate
      (Self  : not null access Gtk_Menu_Item_Record;
       Call  : Cb_GObject_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "toggle-size-allocate" & ASCII.NUL, Call, After, Slot);
   end On_Toggle_Size_Allocate;

   ----------------------------
   -- On_Toggle_Size_Request --
   ----------------------------

   procedure On_Toggle_Size_Request
      (Self  : not null access Gtk_Menu_Item_Record;
       Call  : Cb_Gtk_Menu_Item_Address_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "toggle-size-request" & ASCII.NUL, Call, After);
   end On_Toggle_Size_Request;

   ----------------------------
   -- On_Toggle_Size_Request --
   ----------------------------

   procedure On_Toggle_Size_Request
      (Self  : not null access Gtk_Menu_Item_Record;
       Call  : Cb_GObject_Address_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "toggle-size-request" & ASCII.NUL, Call, After, Slot);
   end On_Toggle_Size_Request;

end Gtk.Menu_Item;
