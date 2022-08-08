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

package body Gtk.Scale_Button is

   package Type_Conversion_Gtk_Scale_Button is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Scale_Button_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Scale_Button);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Button : out Gtk_Scale_Button;
       Size   : Gtk.Enums.Gtk_Icon_Size;
       Min    : Gdouble;
       Max    : Gdouble;
       Step   : Gdouble;
       Icons  : GNAT.Strings.String_List)
   is
   begin
      Button := new Gtk_Scale_Button_Record;
      Gtk.Scale_Button.Initialize (Button, Size, Min, Max, Step, Icons);
   end Gtk_New;

   --------------------------
   -- Gtk_Scale_Button_New --
   --------------------------

   function Gtk_Scale_Button_New
      (Size  : Gtk.Enums.Gtk_Icon_Size;
       Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble;
       Icons : GNAT.Strings.String_List) return Gtk_Scale_Button
   is
      Button : constant Gtk_Scale_Button := new Gtk_Scale_Button_Record;
   begin
      Gtk.Scale_Button.Initialize (Button, Size, Min, Max, Step, Icons);
      return Button;
   end Gtk_Scale_Button_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Button : not null access Gtk_Scale_Button_Record'Class;
       Size   : Gtk.Enums.Gtk_Icon_Size;
       Min    : Gdouble;
       Max    : Gdouble;
       Step   : Gdouble;
       Icons  : GNAT.Strings.String_List)
   is
      function Internal
         (Size  : Gtk.Enums.Gtk_Icon_Size;
          Min   : Gdouble;
          Max   : Gdouble;
          Step  : Gdouble;
          Icons : Gtkada.Types.chars_ptr_array) return System.Address;
      pragma Import (C, Internal, "gtk_scale_button_new");
      Tmp_Icons  : Gtkada.Types.chars_ptr_array := From_String_List (Icons);
      Tmp_Return : System.Address;
   begin
      if not Button.Is_Created then
         Tmp_Return := Internal (Size, Min, Max, Step, Tmp_Icons);
         Gtkada.Types.Free (Tmp_Icons);
         Set_Object (Button, Tmp_Return);
      end if;
   end Initialize;

   --------------------
   -- Get_Adjustment --
   --------------------

   function Get_Adjustment
      (Button : not null access Gtk_Scale_Button_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_button_get_adjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Button)), Stub_Gtk_Adjustment));
   end Get_Adjustment;

   ----------------------
   -- Get_Minus_Button --
   ----------------------

   function Get_Minus_Button
      (Button : not null access Gtk_Scale_Button_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_button_get_minus_button");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Button)), Stub_Gtk_Widget));
   end Get_Minus_Button;

   ---------------------
   -- Get_Plus_Button --
   ---------------------

   function Get_Plus_Button
      (Button : not null access Gtk_Scale_Button_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_button_get_plus_button");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Button)), Stub_Gtk_Widget));
   end Get_Plus_Button;

   ---------------
   -- Get_Popup --
   ---------------

   function Get_Popup
      (Button : not null access Gtk_Scale_Button_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_button_get_popup");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Button)), Stub_Gtk_Widget));
   end Get_Popup;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
      (Button : not null access Gtk_Scale_Button_Record) return Gdouble
   is
      function Internal (Button : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_scale_button_get_value");
   begin
      return Internal (Get_Object (Button));
   end Get_Value;

   --------------------
   -- Set_Adjustment --
   --------------------

   procedure Set_Adjustment
      (Button     : not null access Gtk_Scale_Button_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Button     : System.Address;
          Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_scale_button_set_adjustment");
   begin
      Internal (Get_Object (Button), Get_Object (Adjustment));
   end Set_Adjustment;

   ---------------
   -- Set_Icons --
   ---------------

   procedure Set_Icons
      (Button : not null access Gtk_Scale_Button_Record;
       Icons  : GNAT.Strings.String_List)
   is
      procedure Internal
         (Button : System.Address;
          Icons  : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_scale_button_set_icons");
      Tmp_Icons : Gtkada.Types.chars_ptr_array := From_String_List (Icons);
   begin
      Internal (Get_Object (Button), Tmp_Icons);
      Gtkada.Types.Free (Tmp_Icons);
   end Set_Icons;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
      (Button : not null access Gtk_Scale_Button_Record;
       Value  : Gdouble)
   is
      procedure Internal (Button : System.Address; Value : Gdouble);
      pragma Import (C, Internal, "gtk_scale_button_set_value");
   begin
      Internal (Get_Object (Button), Value);
   end Set_Value;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Scale_Button_Record;
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
      (Self : not null access Gtk_Scale_Button_Record) return UTF8_String
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
      (Self : not null access Gtk_Scale_Button_Record)
       return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_actionable_get_action_target_value");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Action_Target_Value;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Scale_Button_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ------------------------
   -- Get_Related_Action --
   ------------------------

   function Get_Related_Action
      (Self : not null access Gtk_Scale_Button_Record)
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
      (Self : not null access Gtk_Scale_Button_Record) return Boolean
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
      (Self        : not null access Gtk_Scale_Button_Record;
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
      (Self         : not null access Gtk_Scale_Button_Record;
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
      (Self                 : not null access Gtk_Scale_Button_Record;
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

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Scale_Button_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

   ------------------------
   -- Set_Related_Action --
   ------------------------

   procedure Set_Related_Action
      (Self   : not null access Gtk_Scale_Button_Record;
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
      (Self           : not null access Gtk_Scale_Button_Record;
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
      (Self   : not null access Gtk_Scale_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Scale_Button_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Scale_Button_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Scale_Button_Gdouble_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Scale_Button_Gdouble_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdouble_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdouble_Void);

   procedure Connect
      (Object  : access Gtk_Scale_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Scale_Button_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Scale_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Scale_Button_Gdouble_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Scale_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Scale_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdouble_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Scale_Button_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Scale_Button_Gdouble_Void);

   procedure Marsh_Gtk_Scale_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Scale_Button_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Scale_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Scale_Button_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Scale_Button_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Scale_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Scale_Button_Gdouble_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Scale_Button_Gdouble_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Scale_Button_Record'Class;
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
      (Object  : access Gtk_Scale_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdouble_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdouble_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   --------------------------------
   -- Marsh_GObject_Gdouble_Void --
   --------------------------------

   procedure Marsh_GObject_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdouble_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gdouble (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdouble_Void;

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

   -----------------------------------------
   -- Marsh_Gtk_Scale_Button_Gdouble_Void --
   -----------------------------------------

   procedure Marsh_Gtk_Scale_Button_Gdouble_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Scale_Button_Gdouble_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Scale_Button := Gtk_Scale_Button (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gdouble (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Scale_Button_Gdouble_Void;

   ---------------------------------
   -- Marsh_Gtk_Scale_Button_Void --
   ---------------------------------

   procedure Marsh_Gtk_Scale_Button_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Scale_Button_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Scale_Button := Gtk_Scale_Button (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Scale_Button_Void;

   ----------------
   -- On_Popdown --
   ----------------

   procedure On_Popdown
      (Self  : not null access Gtk_Scale_Button_Record;
       Call  : Cb_Gtk_Scale_Button_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "popdown" & ASCII.NUL, Call, After);
   end On_Popdown;

   ----------------
   -- On_Popdown --
   ----------------

   procedure On_Popdown
      (Self  : not null access Gtk_Scale_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "popdown" & ASCII.NUL, Call, After, Slot);
   end On_Popdown;

   --------------
   -- On_Popup --
   --------------

   procedure On_Popup
      (Self  : not null access Gtk_Scale_Button_Record;
       Call  : Cb_Gtk_Scale_Button_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "popup" & ASCII.NUL, Call, After);
   end On_Popup;

   --------------
   -- On_Popup --
   --------------

   procedure On_Popup
      (Self  : not null access Gtk_Scale_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "popup" & ASCII.NUL, Call, After, Slot);
   end On_Popup;

   ----------------------
   -- On_Value_Changed --
   ----------------------

   procedure On_Value_Changed
      (Self  : not null access Gtk_Scale_Button_Record;
       Call  : Cb_Gtk_Scale_Button_Gdouble_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "value-changed" & ASCII.NUL, Call, After);
   end On_Value_Changed;

   ----------------------
   -- On_Value_Changed --
   ----------------------

   procedure On_Value_Changed
      (Self  : not null access Gtk_Scale_Button_Record;
       Call  : Cb_GObject_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "value-changed" & ASCII.NUL, Call, After, Slot);
   end On_Value_Changed;

end Gtk.Scale_Button;
