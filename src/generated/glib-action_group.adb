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
with Glib.Values;              use Glib.Values;
with Gtk.Arguments;            use Gtk.Arguments;

package body Glib.Action_Group is

   ------------------
   -- Action_Added --
   ------------------

   procedure Action_Added (Self : Gaction_Group; Action_Name : UTF8_String) is
      procedure Internal
         (Self        : Gaction_Group;
          Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_action_group_action_added");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Self, Tmp_Action_Name);
      Free (Tmp_Action_Name);
   end Action_Added;

   ----------------------------
   -- Action_Enabled_Changed --
   ----------------------------

   procedure Action_Enabled_Changed
      (Self        : Gaction_Group;
       Action_Name : UTF8_String;
       Enabled     : Boolean)
   is
      procedure Internal
         (Self        : Gaction_Group;
          Action_Name : Gtkada.Types.Chars_Ptr;
          Enabled     : Glib.Gboolean);
      pragma Import (C, Internal, "g_action_group_action_enabled_changed");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Self, Tmp_Action_Name, Boolean'Pos (Enabled));
      Free (Tmp_Action_Name);
   end Action_Enabled_Changed;

   --------------------
   -- Action_Removed --
   --------------------

   procedure Action_Removed
      (Self        : Gaction_Group;
       Action_Name : UTF8_String)
   is
      procedure Internal
         (Self        : Gaction_Group;
          Action_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_action_group_action_removed");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Self, Tmp_Action_Name);
      Free (Tmp_Action_Name);
   end Action_Removed;

   --------------------------
   -- Action_State_Changed --
   --------------------------

   procedure Action_State_Changed
      (Self        : Gaction_Group;
       Action_Name : UTF8_String;
       State       : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self        : Gaction_Group;
          Action_Name : Gtkada.Types.Chars_Ptr;
          State       : System.Address);
      pragma Import (C, Internal, "g_action_group_action_state_changed");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Self, Tmp_Action_Name, Get_Object (State));
      Free (Tmp_Action_Name);
   end Action_State_Changed;

   ---------------------
   -- Activate_Action --
   ---------------------

   procedure Activate_Action
      (Self        : Gaction_Group;
       Action_Name : UTF8_String;
       Parameter   : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self        : Gaction_Group;
          Action_Name : Gtkada.Types.Chars_Ptr;
          Parameter   : System.Address);
      pragma Import (C, Internal, "g_action_group_activate_action");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Self, Tmp_Action_Name, Get_Object (Parameter));
      Free (Tmp_Action_Name);
   end Activate_Action;

   -------------------------
   -- Change_Action_State --
   -------------------------

   procedure Change_Action_State
      (Self        : Gaction_Group;
       Action_Name : UTF8_String;
       Value       : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self        : Gaction_Group;
          Action_Name : Gtkada.Types.Chars_Ptr;
          Value       : System.Address);
      pragma Import (C, Internal, "g_action_group_change_action_state");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Self, Tmp_Action_Name, Get_Object (Value));
      Free (Tmp_Action_Name);
   end Change_Action_State;

   ------------------------
   -- Get_Action_Enabled --
   ------------------------

   function Get_Action_Enabled
      (Self        : Gaction_Group;
       Action_Name : UTF8_String) return Boolean
   is
      function Internal
         (Self        : Gaction_Group;
          Action_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_action_group_get_action_enabled");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Return      : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return Tmp_Return /= 0;
   end Get_Action_Enabled;

   -------------------------------
   -- Get_Action_Parameter_Type --
   -------------------------------

   function Get_Action_Parameter_Type
      (Self        : Gaction_Group;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type
   is
      function Internal
         (Self        : Gaction_Group;
          Action_Name : Gtkada.Types.Chars_Ptr)
          return Glib.Variant.Gvariant_Type;
      pragma Import (C, Internal, "g_action_group_get_action_parameter_type");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Return      : Glib.Variant.Gvariant_Type;
   begin
      Tmp_Return := Internal (Self, Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return Tmp_Return;
   end Get_Action_Parameter_Type;

   ----------------------
   -- Get_Action_State --
   ----------------------

   function Get_Action_State
      (Self        : Gaction_Group;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant
   is
      function Internal
         (Self        : Gaction_Group;
          Action_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_action_group_get_action_state");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Self, Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return From_Object (Tmp_Return);
   end Get_Action_State;

   ---------------------------
   -- Get_Action_State_Hint --
   ---------------------------

   function Get_Action_State_Hint
      (Self        : Gaction_Group;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant
   is
      function Internal
         (Self        : Gaction_Group;
          Action_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_action_group_get_action_state_hint");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Self, Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return From_Object (Tmp_Return);
   end Get_Action_State_Hint;

   ---------------------------
   -- Get_Action_State_Type --
   ---------------------------

   function Get_Action_State_Type
      (Self        : Gaction_Group;
       Action_Name : UTF8_String) return Glib.Variant.Gvariant_Type
   is
      function Internal
         (Self        : Gaction_Group;
          Action_Name : Gtkada.Types.Chars_Ptr)
          return Glib.Variant.Gvariant_Type;
      pragma Import (C, Internal, "g_action_group_get_action_state_type");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Return      : Glib.Variant.Gvariant_Type;
   begin
      Tmp_Return := Internal (Self, Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return Tmp_Return;
   end Get_Action_State_Type;

   ----------------
   -- Has_Action --
   ----------------

   function Has_Action
      (Self        : Gaction_Group;
       Action_Name : UTF8_String) return Boolean
   is
      function Internal
         (Self        : Gaction_Group;
          Action_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_action_group_has_action");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Return      : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Action_Name);
      Free (Tmp_Action_Name);
      return Tmp_Return /= 0;
   end Has_Action;

   ------------------
   -- List_Actions --
   ------------------

   function List_Actions
      (Self : Gaction_Group) return GNAT.Strings.String_List
   is
      function Internal (Self : Gaction_Group) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_action_group_list_actions");
   begin
      return To_String_List_And_Free (Internal (Self));
   end List_Actions;

   ------------------
   -- Query_Action --
   ------------------

   function Query_Action
      (Self           : Gaction_Group;
       Action_Name    : UTF8_String;
       Enabled        : access Boolean;
       Parameter_Type : access Glib.Variant.Gvariant_Type;
       State_Type     : access Glib.Variant.Gvariant_Type;
       State_Hint     : access Glib.Variant.Gvariant;
       State          : access Glib.Variant.Gvariant) return Boolean
   is
      function Internal
         (Self               : Gaction_Group;
          Action_Name        : Gtkada.Types.Chars_Ptr;
          Acc_Enabled        : access Glib.Gboolean;
          Acc_Parameter_Type : access Glib.Variant.Gvariant_Type;
          Acc_State_Type     : access Glib.Variant.Gvariant_Type;
          Acc_State_Hint     : access System.Address;
          Acc_State          : access System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_action_group_query_action");
      Acc_Enabled        : aliased Boolean;
      Acc_Parameter_Type : aliased Glib.Variant.Gvariant_Type;
      Acc_State_Type     : aliased Glib.Variant.Gvariant_Type;
      Acc_State_Hint     : aliased Glib.Variant.Gvariant;
      Acc_State          : aliased Glib.Variant.Gvariant;
      Tmp_Action_Name    : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
      Tmp_Acc_Enabled    : aliased Glib.Gboolean;
      Tmp_Acc_State_Hint : aliased System.Address;
      Tmp_Acc_State      : aliased System.Address;
      Tmp_Return         : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Action_Name, Tmp_Acc_Enabled'Access, Acc_Parameter_Type'Access, Acc_State_Type'Access, Tmp_Acc_State_Hint'Access, Tmp_Acc_State'Access);
      Acc_State := From_Object (Tmp_Acc_State);
      Acc_State_Hint := From_Object (Tmp_Acc_State_Hint);
      Acc_Enabled := Tmp_Acc_Enabled /= 0;
      Free (Tmp_Action_Name);
      Enabled.all := Acc_Enabled;
      if Parameter_Type /= null then
         Parameter_Type.all := Acc_Parameter_Type;
      end if;
      if State_Type /= null then
         State_Type.all := Acc_State_Type;
      end if;
      if State_Hint /= null then
         State_Hint.all := Acc_State_Hint;
      end if;
      if State /= null then
         State.all := Acc_State;
      end if;
      return Tmp_Return /= 0;
   end Query_Action;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gaction_Group_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gaction_Group_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gaction_Group_UTF8_String_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gaction_Group_UTF8_String_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gaction_Group_UTF8_String_Gvariant_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gaction_Group_UTF8_String_Gvariant_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Gvariant_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Gvariant_Void);

   procedure Connect
      (Object  : Gaction_Group;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gaction_Group_UTF8_String_Void;
       After   : Boolean);

   procedure Connect
      (Object  : Gaction_Group;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gaction_Group_UTF8_String_Boolean_Void;
       After   : Boolean);

   procedure Connect
      (Object  : Gaction_Group;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gaction_Group_UTF8_String_Gvariant_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : Gaction_Group;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : Gaction_Group;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : Gaction_Group;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Gvariant_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_UTF8_String_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Boolean_Void);

   procedure Marsh_GObject_UTF8_String_Gvariant_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Gvariant_Void);

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Void);

   procedure Marsh_Gaction_Group_UTF8_String_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gaction_Group_UTF8_String_Boolean_Void);

   procedure Marsh_Gaction_Group_UTF8_String_Gvariant_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gaction_Group_UTF8_String_Gvariant_Void);

   procedure Marsh_Gaction_Group_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gaction_Group_UTF8_String_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gaction_Group;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gaction_Group_UTF8_String_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gaction_Group_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gaction_Group;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gaction_Group_UTF8_String_Boolean_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gaction_Group_UTF8_String_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gaction_Group;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gaction_Group_UTF8_String_Gvariant_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gaction_Group_UTF8_String_Gvariant_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gaction_Group;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gaction_Group;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gaction_Group;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Gvariant_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Gvariant_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   --------------------------------------------
   -- Marsh_GObject_UTF8_String_Boolean_Void --
   --------------------------------------------

   procedure Marsh_GObject_UTF8_String_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_Boolean (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Boolean_Void;

   ---------------------------------------------
   -- Marsh_GObject_UTF8_String_Gvariant_Void --
   ---------------------------------------------

   procedure Marsh_GObject_UTF8_String_Gvariant_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Gvariant_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Glib.Variant.From_Object (Unchecked_To_Address (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Gvariant_Void;

   ------------------------------------
   -- Marsh_GObject_UTF8_String_Void --
   ------------------------------------

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Void;

   --------------------------------------------------
   -- Marsh_Gaction_Group_UTF8_String_Boolean_Void --
   --------------------------------------------------

   procedure Marsh_Gaction_Group_UTF8_String_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gaction_Group_UTF8_String_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gaction_Group := Gaction_Group (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_Boolean (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gaction_Group_UTF8_String_Boolean_Void;

   ---------------------------------------------------
   -- Marsh_Gaction_Group_UTF8_String_Gvariant_Void --
   ---------------------------------------------------

   procedure Marsh_Gaction_Group_UTF8_String_Gvariant_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gaction_Group_UTF8_String_Gvariant_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gaction_Group := Gaction_Group (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Glib.Variant.From_Object (Unchecked_To_Address (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gaction_Group_UTF8_String_Gvariant_Void;

   ------------------------------------------
   -- Marsh_Gaction_Group_UTF8_String_Void --
   ------------------------------------------

   procedure Marsh_Gaction_Group_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gaction_Group_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gaction_Group := Gaction_Group (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gaction_Group_UTF8_String_Void;

   ---------------------
   -- On_Action_Added --
   ---------------------

   procedure On_Action_Added
      (Self  : Gaction_Group;
       Call  : Cb_Gaction_Group_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "action-added" & ASCII.NUL, Call, After);
   end On_Action_Added;

   ---------------------
   -- On_Action_Added --
   ---------------------

   procedure On_Action_Added
      (Self  : Gaction_Group;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "action-added" & ASCII.NUL, Call, After, Slot);
   end On_Action_Added;

   -------------------------------
   -- On_Action_Enabled_Changed --
   -------------------------------

   procedure On_Action_Enabled_Changed
      (Self  : Gaction_Group;
       Call  : Cb_Gaction_Group_UTF8_String_Boolean_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "action-enabled-changed" & ASCII.NUL, Call, After);
   end On_Action_Enabled_Changed;

   -------------------------------
   -- On_Action_Enabled_Changed --
   -------------------------------

   procedure On_Action_Enabled_Changed
      (Self  : Gaction_Group;
       Call  : Cb_GObject_UTF8_String_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "action-enabled-changed" & ASCII.NUL, Call, After, Slot);
   end On_Action_Enabled_Changed;

   -----------------------
   -- On_Action_Removed --
   -----------------------

   procedure On_Action_Removed
      (Self  : Gaction_Group;
       Call  : Cb_Gaction_Group_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "action-removed" & ASCII.NUL, Call, After);
   end On_Action_Removed;

   -----------------------
   -- On_Action_Removed --
   -----------------------

   procedure On_Action_Removed
      (Self  : Gaction_Group;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "action-removed" & ASCII.NUL, Call, After, Slot);
   end On_Action_Removed;

   -----------------------------
   -- On_Action_State_Changed --
   -----------------------------

   procedure On_Action_State_Changed
      (Self  : Gaction_Group;
       Call  : Cb_Gaction_Group_UTF8_String_Gvariant_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "action-state-changed" & ASCII.NUL, Call, After);
   end On_Action_State_Changed;

   -----------------------------
   -- On_Action_State_Changed --
   -----------------------------

   procedure On_Action_State_Changed
      (Self  : Gaction_Group;
       Call  : Cb_GObject_UTF8_String_Gvariant_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "action-state-changed" & ASCII.NUL, Call, After, Slot);
   end On_Action_State_Changed;

   function "+" (W : Gaction_Group) return Gaction_Group is
   begin
      return W;
   end "+";

end Glib.Action_Group;
