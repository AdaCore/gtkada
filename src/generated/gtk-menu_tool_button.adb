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

package body Gtk.Menu_Tool_Button is

   package Type_Conversion_Gtk_Menu_Tool_Button is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Menu_Tool_Button_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Menu_Tool_Button);

   ------------------------------
   -- Gtk_Menu_Tool_Button_New --
   ------------------------------

   function Gtk_Menu_Tool_Button_New
      (Icon_Widget : Gtk.Widget.Gtk_Widget := null;
       Label       : UTF8_String := "") return Gtk_Menu_Tool_Button
   is
      Menu : constant Gtk_Menu_Tool_Button := new Gtk_Menu_Tool_Button_Record;
   begin
      Gtk.Menu_Tool_Button.Initialize (Menu, Icon_Widget, Label);
      return Menu;
   end Gtk_Menu_Tool_Button_New;

   -----------------------------------------
   -- Gtk_Menu_Tool_Button_New_From_Stock --
   -----------------------------------------

   function Gtk_Menu_Tool_Button_New_From_Stock
      (Stock_Id : UTF8_String) return Gtk_Menu_Tool_Button
   is
      Menu : constant Gtk_Menu_Tool_Button := new Gtk_Menu_Tool_Button_Record;
   begin
      Gtk.Menu_Tool_Button.Initialize_From_Stock (Menu, Stock_Id);
      return Menu;
   end Gtk_Menu_Tool_Button_New_From_Stock;

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
          Label       : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_menu_tool_button_new");
      Tmp_Label  : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Menu.Is_Created then
         if Label = "" then
            Tmp_Label := Gtkada.Types.Null_Ptr;
         else
            Tmp_Label := New_String (Label);
         end if;
         Tmp_Return := Internal (Get_Object_Or_Null (GObject (Icon_Widget)), Tmp_Label);
         Free (Tmp_Label);
         Set_Object (Menu, Tmp_Return);
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
      (Menu     : not null access Gtk_Menu_Tool_Button_Record'Class;
       Stock_Id : UTF8_String)
   is
      function Internal
         (Stock_Id : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_menu_tool_button_new_from_stock");
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr := New_String (Stock_Id);
      Tmp_Return   : System.Address;
   begin
      if not Menu.Is_Created then
         Tmp_Return := Internal (Tmp_Stock_Id);
         Free (Tmp_Stock_Id);
         Set_Object (Menu, Tmp_Return);
      end if;
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
          Markup : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_menu_tool_button_set_arrow_tooltip_markup");
      Tmp_Markup : Gtkada.Types.Chars_Ptr := New_String (Markup);
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
          Text   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_menu_tool_button_set_arrow_tooltip_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
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

   ---------------------
   -- Get_Action_Name --
   ---------------------

   function Get_Action_Name
      (Self : not null access Gtk_Menu_Tool_Button_Record)
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
      (Self : not null access Gtk_Menu_Tool_Button_Record)
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
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_activatable_get_use_action_appearance");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Action_Appearance;

   ---------------------
   -- Set_Action_Name --
   ---------------------

   procedure Set_Action_Name
      (Self        : not null access Gtk_Menu_Tool_Button_Record;
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
      (Self         : not null access Gtk_Menu_Tool_Button_Record;
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
      (Self                 : not null access Gtk_Menu_Tool_Button_Record;
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
      (Self   : not null access Gtk_Menu_Tool_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Menu_Tool_Button_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Menu_Tool_Button_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Menu_Tool_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Tool_Button_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Tool_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Menu_Tool_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Menu_Tool_Button_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Menu_Tool_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Menu_Tool_Button_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Menu_Tool_Button_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Menu_Tool_Button_Record'Class;
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

   -------------------------------------
   -- Marsh_Gtk_Menu_Tool_Button_Void --
   -------------------------------------

   procedure Marsh_Gtk_Menu_Tool_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Menu_Tool_Button_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Menu_Tool_Button := Gtk_Menu_Tool_Button (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Menu_Tool_Button_Void;

   ------------------
   -- On_Show_Menu --
   ------------------

   procedure On_Show_Menu
      (Self  : not null access Gtk_Menu_Tool_Button_Record;
       Call  : Cb_Gtk_Menu_Tool_Button_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "show-menu" & ASCII.NUL, Call, After);
   end On_Show_Menu;

   ------------------
   -- On_Show_Menu --
   ------------------

   procedure On_Show_Menu
      (Self  : not null access Gtk_Menu_Tool_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "show-menu" & ASCII.NUL, Call, After, Slot);
   end On_Show_Menu;

end Gtk.Menu_Tool_Button;
