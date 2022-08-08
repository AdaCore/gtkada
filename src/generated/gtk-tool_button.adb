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

package body Gtk.Tool_Button is

   package Type_Conversion_Gtk_Tool_Button is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tool_Button_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Tool_Button);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Button      : out Gtk_Tool_Button;
       Icon_Widget : Gtk.Widget.Gtk_Widget := null;
       Label       : UTF8_String := "")
   is
   begin
      Button := new Gtk_Tool_Button_Record;
      Gtk.Tool_Button.Initialize (Button, Icon_Widget, Label);
   end Gtk_New;

   ------------------------
   -- Gtk_New_From_Stock --
   ------------------------

   procedure Gtk_New_From_Stock
      (Button   : out Gtk_Tool_Button;
       Stock_Id : UTF8_String)
   is
   begin
      Button := new Gtk_Tool_Button_Record;
      Gtk.Tool_Button.Initialize_From_Stock (Button, Stock_Id);
   end Gtk_New_From_Stock;

   -------------------------
   -- Gtk_Tool_Button_New --
   -------------------------

   function Gtk_Tool_Button_New
      (Icon_Widget : Gtk.Widget.Gtk_Widget := null;
       Label       : UTF8_String := "") return Gtk_Tool_Button
   is
      Button : constant Gtk_Tool_Button := new Gtk_Tool_Button_Record;
   begin
      Gtk.Tool_Button.Initialize (Button, Icon_Widget, Label);
      return Button;
   end Gtk_Tool_Button_New;

   ------------------------------------
   -- Gtk_Tool_Button_New_From_Stock --
   ------------------------------------

   function Gtk_Tool_Button_New_From_Stock
      (Stock_Id : UTF8_String) return Gtk_Tool_Button
   is
      Button : constant Gtk_Tool_Button := new Gtk_Tool_Button_Record;
   begin
      Gtk.Tool_Button.Initialize_From_Stock (Button, Stock_Id);
      return Button;
   end Gtk_Tool_Button_New_From_Stock;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Button      : not null access Gtk_Tool_Button_Record'Class;
       Icon_Widget : Gtk.Widget.Gtk_Widget := null;
       Label       : UTF8_String := "")
   is
      function Internal
         (Icon_Widget : System.Address;
          Label       : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_tool_button_new");
      Tmp_Label  : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Button.Is_Created then
         if Label = "" then
            Tmp_Label := Gtkada.Types.Null_Ptr;
         else
            Tmp_Label := New_String (Label);
         end if;
         Tmp_Return := Internal (Get_Object_Or_Null (GObject (Icon_Widget)), Tmp_Label);
         Free (Tmp_Label);
         Set_Object (Button, Tmp_Return);
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
      (Button   : not null access Gtk_Tool_Button_Record'Class;
       Stock_Id : UTF8_String)
   is
      function Internal
         (Stock_Id : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_tool_button_new_from_stock");
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr := New_String (Stock_Id);
      Tmp_Return   : System.Address;
   begin
      if not Button.Is_Created then
         Tmp_Return := Internal (Tmp_Stock_Id);
         Free (Tmp_Stock_Id);
         Set_Object (Button, Tmp_Return);
      end if;
   end Initialize_From_Stock;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name
      (Button : not null access Gtk_Tool_Button_Record) return UTF8_String
   is
      function Internal
         (Button : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_tool_button_get_icon_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Button)));
   end Get_Icon_Name;

   ---------------------
   -- Get_Icon_Widget --
   ---------------------

   function Get_Icon_Widget
      (Button : not null access Gtk_Tool_Button_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tool_button_get_icon_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Button)), Stub_Gtk_Widget));
   end Get_Icon_Widget;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
      (Button : not null access Gtk_Tool_Button_Record) return UTF8_String
   is
      function Internal
         (Button : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_tool_button_get_label");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Button)));
   end Get_Label;

   ----------------------
   -- Get_Label_Widget --
   ----------------------

   function Get_Label_Widget
      (Button : not null access Gtk_Tool_Button_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tool_button_get_label_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Button)), Stub_Gtk_Widget));
   end Get_Label_Widget;

   ------------------
   -- Get_Stock_Id --
   ------------------

   function Get_Stock_Id
      (Button : not null access Gtk_Tool_Button_Record) return UTF8_String
   is
      function Internal
         (Button : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_tool_button_get_stock_id");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Button)));
   end Get_Stock_Id;

   -----------------------
   -- Get_Use_Underline --
   -----------------------

   function Get_Use_Underline
      (Button : not null access Gtk_Tool_Button_Record) return Boolean
   is
      function Internal (Button : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tool_button_get_use_underline");
   begin
      return Internal (Get_Object (Button)) /= 0;
   end Get_Use_Underline;

   -------------------
   -- Set_Icon_Name --
   -------------------

   procedure Set_Icon_Name
      (Button    : not null access Gtk_Tool_Button_Record;
       Icon_Name : UTF8_String := "")
   is
      procedure Internal
         (Button    : System.Address;
          Icon_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_tool_button_set_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Icon_Name = "" then
         Tmp_Icon_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Icon_Name := New_String (Icon_Name);
      end if;
      Internal (Get_Object (Button), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
   end Set_Icon_Name;

   ---------------------
   -- Set_Icon_Widget --
   ---------------------

   procedure Set_Icon_Widget
      (Button      : not null access Gtk_Tool_Button_Record;
       Icon_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Button      : System.Address;
          Icon_Widget : System.Address);
      pragma Import (C, Internal, "gtk_tool_button_set_icon_widget");
   begin
      Internal (Get_Object (Button), Get_Object_Or_Null (GObject (Icon_Widget)));
   end Set_Icon_Widget;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
      (Button : not null access Gtk_Tool_Button_Record;
       Label  : UTF8_String := "")
   is
      procedure Internal
         (Button : System.Address;
          Label  : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_tool_button_set_label");
      Tmp_Label : Gtkada.Types.Chars_Ptr;
   begin
      if Label = "" then
         Tmp_Label := Gtkada.Types.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Internal (Get_Object (Button), Tmp_Label);
      Free (Tmp_Label);
   end Set_Label;

   ----------------------
   -- Set_Label_Widget --
   ----------------------

   procedure Set_Label_Widget
      (Button       : not null access Gtk_Tool_Button_Record;
       Label_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Button       : System.Address;
          Label_Widget : System.Address);
      pragma Import (C, Internal, "gtk_tool_button_set_label_widget");
   begin
      Internal (Get_Object (Button), Get_Object_Or_Null (GObject (Label_Widget)));
   end Set_Label_Widget;

   ------------------
   -- Set_Stock_Id --
   ------------------

   procedure Set_Stock_Id
      (Button   : not null access Gtk_Tool_Button_Record;
       Stock_Id : UTF8_String := "")
   is
      procedure Internal
         (Button   : System.Address;
          Stock_Id : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_tool_button_set_stock_id");
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr;
   begin
      if Stock_Id = "" then
         Tmp_Stock_Id := Gtkada.Types.Null_Ptr;
      else
         Tmp_Stock_Id := New_String (Stock_Id);
      end if;
      Internal (Get_Object (Button), Tmp_Stock_Id);
      Free (Tmp_Stock_Id);
   end Set_Stock_Id;

   -----------------------
   -- Set_Use_Underline --
   -----------------------

   procedure Set_Use_Underline
      (Button        : not null access Gtk_Tool_Button_Record;
       Use_Underline : Boolean)
   is
      procedure Internal
         (Button        : System.Address;
          Use_Underline : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tool_button_set_use_underline");
   begin
      Internal (Get_Object (Button), Boolean'Pos (Use_Underline));
   end Set_Use_Underline;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Tool_Button_Record;
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
      (Self : not null access Gtk_Tool_Button_Record) return UTF8_String
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
      (Self : not null access Gtk_Tool_Button_Record)
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
      (Self : not null access Gtk_Tool_Button_Record)
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
      (Self : not null access Gtk_Tool_Button_Record) return Boolean
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
      (Self        : not null access Gtk_Tool_Button_Record;
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
      (Self         : not null access Gtk_Tool_Button_Record;
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
      (Self                 : not null access Gtk_Tool_Button_Record;
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
      (Self   : not null access Gtk_Tool_Button_Record;
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
      (Self           : not null access Gtk_Tool_Button_Record;
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
      (Self   : not null access Gtk_Tool_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Tool_Button_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Tool_Button_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Tool_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tool_Button_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Tool_Button_Record'Class;
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

   procedure Marsh_Gtk_Tool_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Tool_Button_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Tool_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tool_Button_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Tool_Button_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Tool_Button_Record'Class;
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

   --------------------------------
   -- Marsh_Gtk_Tool_Button_Void --
   --------------------------------

   procedure Marsh_Gtk_Tool_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Tool_Button_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Tool_Button := Gtk_Tool_Button (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Tool_Button_Void;

   ----------------
   -- On_Clicked --
   ----------------

   procedure On_Clicked
      (Self  : not null access Gtk_Tool_Button_Record;
       Call  : Cb_Gtk_Tool_Button_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "clicked" & ASCII.NUL, Call, After);
   end On_Clicked;

   ----------------
   -- On_Clicked --
   ----------------

   procedure On_Clicked
      (Self  : not null access Gtk_Tool_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "clicked" & ASCII.NUL, Call, After, Slot);
   end On_Clicked;

end Gtk.Tool_Button;
