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

package body Gtk.Link_Button is

   package Type_Conversion_Gtk_Link_Button is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Link_Button_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Link_Button);

   -------------------------
   -- Gtk_Link_Button_New --
   -------------------------

   function Gtk_Link_Button_New (URI : UTF8_String) return Gtk_Link_Button is
      Widget : constant Gtk_Link_Button := new Gtk_Link_Button_Record;
   begin
      Gtk.Link_Button.Initialize (Widget, URI);
      return Widget;
   end Gtk_Link_Button_New;

   ------------------------------------
   -- Gtk_Link_Button_New_With_Label --
   ------------------------------------

   function Gtk_Link_Button_New_With_Label
      (URI   : UTF8_String;
       Label : UTF8_String := "") return Gtk_Link_Button
   is
      Widget : constant Gtk_Link_Button := new Gtk_Link_Button_Record;
   begin
      Gtk.Link_Button.Initialize_With_Label (Widget, URI, Label);
      return Widget;
   end Gtk_Link_Button_New_With_Label;

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
      function Internal (URI : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_link_button_new");
      Tmp_URI    : Gtkada.Types.Chars_Ptr := New_String (URI);
      Tmp_Return : System.Address;
   begin
      if not Widget.Is_Created then
         Tmp_Return := Internal (Tmp_URI);
         Free (Tmp_URI);
         Set_Object (Widget, Tmp_Return);
      end if;
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
         (URI   : Gtkada.Types.Chars_Ptr;
          Label : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_link_button_new_with_label");
      Tmp_URI    : Gtkada.Types.Chars_Ptr := New_String (URI);
      Tmp_Label  : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Widget.Is_Created then
         if Label = "" then
            Tmp_Label := Gtkada.Types.Null_Ptr;
         else
            Tmp_Label := New_String (Label);
         end if;
         Tmp_Return := Internal (Tmp_URI, Tmp_Label);
         Free (Tmp_Label);
         Free (Tmp_URI);
         Set_Object (Widget, Tmp_Return);
      end if;
   end Initialize_With_Label;

   -------------
   -- Get_Uri --
   -------------

   function Get_Uri
      (Widget : not null access Gtk_Link_Button_Record) return UTF8_String
   is
      function Internal
         (Widget : System.Address) return Gtkada.Types.Chars_Ptr;
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
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_link_button_get_visited");
   begin
      return Internal (Get_Object (Widget)) /= 0;
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
          URI    : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_link_button_set_uri");
      Tmp_URI : Gtkada.Types.Chars_Ptr := New_String (URI);
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
      procedure Internal (Widget : System.Address; Visited : Glib.Gboolean);
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

   ---------------------
   -- Get_Action_Name --
   ---------------------

   function Get_Action_Name
      (Self : not null access Gtk_Link_Button_Record) return UTF8_String
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
      (Self : not null access Gtk_Link_Button_Record)
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
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_activatable_get_use_action_appearance");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Action_Appearance;

   ---------------------
   -- Set_Action_Name --
   ---------------------

   procedure Set_Action_Name
      (Self        : not null access Gtk_Link_Button_Record;
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
      (Self         : not null access Gtk_Link_Button_Record;
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
      (Self                 : not null access Gtk_Link_Button_Record;
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
      (Self   : not null access Gtk_Link_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Link_Button_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Link_Button_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean);

   procedure Connect
      (Object  : access Gtk_Link_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Link_Button_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Link_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean);

   procedure Marsh_Gtk_Link_Button_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Link_Button_Boolean);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Link_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Link_Button_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Link_Button_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Link_Button_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------
   -- Marsh_GObject_Boolean --
   ---------------------------

   procedure Marsh_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean;

   -----------------------------------
   -- Marsh_Gtk_Link_Button_Boolean --
   -----------------------------------

   procedure Marsh_Gtk_Link_Button_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Link_Button_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Link_Button := Gtk_Link_Button (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Link_Button_Boolean;

   ----------------------
   -- On_Activate_Link --
   ----------------------

   procedure On_Activate_Link
      (Self  : not null access Gtk_Link_Button_Record;
       Call  : Cb_Gtk_Link_Button_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate-link" & ASCII.NUL, Call, After);
   end On_Activate_Link;

   ----------------------
   -- On_Activate_Link --
   ----------------------

   procedure On_Activate_Link
      (Self  : not null access Gtk_Link_Button_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate-link" & ASCII.NUL, Call, After, Slot);
   end On_Activate_Link;

end Gtk.Link_Button;
