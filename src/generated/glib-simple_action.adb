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
with Gtkada.Types;               use Gtkada.Types;

package body Glib.Simple_Action is

   package Type_Conversion_Gsimple_Action is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gsimple_Action_Record);
   pragma Unreferenced (Type_Conversion_Gsimple_Action);

   -----------
   -- G_New --
   -----------

   procedure G_New
      (Self           : out Gsimple_Action;
       Name           : UTF8_String;
       Parameter_Type : Glib.Variant.Gvariant_Type)
   is
   begin
      Self := new Gsimple_Action_Record;
      Glib.Simple_Action.Initialize (Self, Name, Parameter_Type);
   end G_New;

   --------------------
   -- G_New_Stateful --
   --------------------

   procedure G_New_Stateful
      (Self           : out Gsimple_Action;
       Name           : UTF8_String;
       Parameter_Type : Glib.Variant.Gvariant_Type;
       State          : Glib.Variant.Gvariant)
   is
   begin
      Self := new Gsimple_Action_Record;
      Glib.Simple_Action.Initialize_Stateful (Self, Name, Parameter_Type, State);
   end G_New_Stateful;

   ------------------------
   -- Gsimple_Action_New --
   ------------------------

   function Gsimple_Action_New
      (Name           : UTF8_String;
       Parameter_Type : Glib.Variant.Gvariant_Type) return Gsimple_Action
   is
      Self : constant Gsimple_Action := new Gsimple_Action_Record;
   begin
      Glib.Simple_Action.Initialize (Self, Name, Parameter_Type);
      return Self;
   end Gsimple_Action_New;

   ---------------------------------
   -- Gsimple_Action_New_Stateful --
   ---------------------------------

   function Gsimple_Action_New_Stateful
      (Name           : UTF8_String;
       Parameter_Type : Glib.Variant.Gvariant_Type;
       State          : Glib.Variant.Gvariant) return Gsimple_Action
   is
      Self : constant Gsimple_Action := new Gsimple_Action_Record;
   begin
      Glib.Simple_Action.Initialize_Stateful (Self, Name, Parameter_Type, State);
      return Self;
   end Gsimple_Action_New_Stateful;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self           : not null access Gsimple_Action_Record'Class;
       Name           : UTF8_String;
       Parameter_Type : Glib.Variant.Gvariant_Type)
   is
      function Internal
         (Name           : Gtkada.Types.Chars_Ptr;
          Parameter_Type : Glib.Variant.Gvariant_Type) return System.Address;
      pragma Import (C, Internal, "g_simple_action_new");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Name, Parameter_Type);
         Free (Tmp_Name);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize;

   -------------------------
   -- Initialize_Stateful --
   -------------------------

   procedure Initialize_Stateful
      (Self           : not null access Gsimple_Action_Record'Class;
       Name           : UTF8_String;
       Parameter_Type : Glib.Variant.Gvariant_Type;
       State          : Glib.Variant.Gvariant)
   is
      function Internal
         (Name           : Gtkada.Types.Chars_Ptr;
          Parameter_Type : Glib.Variant.Gvariant_Type;
          State          : System.Address) return System.Address;
      pragma Import (C, Internal, "g_simple_action_new_stateful");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Name, Parameter_Type, Get_Object (State));
         Free (Tmp_Name);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_Stateful;

   -----------------
   -- Set_Enabled --
   -----------------

   procedure Set_Enabled
      (Self    : not null access Gsimple_Action_Record;
       Enabled : Boolean)
   is
      procedure Internal (Self : System.Address; Enabled : Glib.Gboolean);
      pragma Import (C, Internal, "g_simple_action_set_enabled");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Enabled));
   end Set_Enabled;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State
      (Self  : not null access Gsimple_Action_Record;
       Value : Glib.Variant.Gvariant)
   is
      procedure Internal (Self : System.Address; Value : System.Address);
      pragma Import (C, Internal, "g_simple_action_set_state");
   begin
      Internal (Get_Object (Self), Get_Object (Value));
   end Set_State;

   --------------------
   -- Set_State_Hint --
   --------------------

   procedure Set_State_Hint
      (Self       : not null access Gsimple_Action_Record;
       State_Hint : Glib.Variant.Gvariant)
   is
      procedure Internal
         (Self       : System.Address;
          State_Hint : System.Address);
      pragma Import (C, Internal, "g_simple_action_set_state_hint");
   begin
      Internal (Get_Object (Self), Get_Object (State_Hint));
   end Set_State_Hint;

   --------------
   -- Activate --
   --------------

   procedure Activate
      (Self      : not null access Gsimple_Action_Record;
       Parameter : Glib.Variant.Gvariant)
   is
      procedure Internal (Self : System.Address; Parameter : System.Address);
      pragma Import (C, Internal, "g_action_activate");
   begin
      Internal (Get_Object (Self), Get_Object (Parameter));
   end Activate;

   ------------------
   -- Change_State --
   ------------------

   procedure Change_State
      (Self  : not null access Gsimple_Action_Record;
       Value : Glib.Variant.Gvariant)
   is
      procedure Internal (Self : System.Address; Value : System.Address);
      pragma Import (C, Internal, "g_action_change_state");
   begin
      Internal (Get_Object (Self), Get_Object (Value));
   end Change_State;

   -----------------
   -- Get_Enabled --
   -----------------

   function Get_Enabled
      (Self : not null access Gsimple_Action_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_action_get_enabled");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Enabled;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Self : not null access Gsimple_Action_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_action_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Name;

   ------------------------
   -- Get_Parameter_Type --
   ------------------------

   function Get_Parameter_Type
      (Self : not null access Gsimple_Action_Record)
       return Glib.Variant.Gvariant_Type
   is
      function Internal
         (Self : System.Address) return Glib.Variant.Gvariant_Type;
      pragma Import (C, Internal, "g_action_get_parameter_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Parameter_Type;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
      (Self : not null access Gsimple_Action_Record)
       return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_action_get_state");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_State;

   --------------------
   -- Get_State_Hint --
   --------------------

   function Get_State_Hint
      (Self : not null access Gsimple_Action_Record)
       return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_action_get_state_hint");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_State_Hint;

   --------------------
   -- Get_State_Type --
   --------------------

   function Get_State_Type
      (Self : not null access Gsimple_Action_Record)
       return Glib.Variant.Gvariant_Type
   is
      function Internal
         (Self : System.Address) return Glib.Variant.Gvariant_Type;
      pragma Import (C, Internal, "g_action_get_state_type");
   begin
      return Internal (Get_Object (Self));
   end Get_State_Type;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gsimple_Action_Gvariant_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gsimple_Action_Gvariant_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gvariant_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gvariant_Void);

   procedure Connect
      (Object  : access Gsimple_Action_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gsimple_Action_Gvariant_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gsimple_Action_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gvariant_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gvariant_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gvariant_Void);

   procedure Marsh_Gsimple_Action_Gvariant_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gsimple_Action_Gvariant_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gsimple_Action_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gsimple_Action_Gvariant_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gsimple_Action_Gvariant_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gsimple_Action_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gvariant_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gvariant_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------------
   -- Marsh_GObject_Gvariant_Void --
   ---------------------------------

   procedure Marsh_GObject_Gvariant_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gvariant_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Glib.Variant.From_Object (Unchecked_To_Address (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gvariant_Void;

   ----------------------------------------
   -- Marsh_Gsimple_Action_Gvariant_Void --
   ----------------------------------------

   procedure Marsh_Gsimple_Action_Gvariant_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gsimple_Action_Gvariant_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gsimple_Action := Gsimple_Action (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Glib.Variant.From_Object (Unchecked_To_Address (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gsimple_Action_Gvariant_Void;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gsimple_Action_Record;
       Call  : Cb_Gsimple_Action_Gvariant_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate" & ASCII.NUL, Call, After);
   end On_Activate;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gsimple_Action_Record;
       Call  : Cb_GObject_Gvariant_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate" & ASCII.NUL, Call, After, Slot);
   end On_Activate;

   ---------------------
   -- On_Change_State --
   ---------------------

   procedure On_Change_State
      (Self  : not null access Gsimple_Action_Record;
       Call  : Cb_Gsimple_Action_Gvariant_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "change-state" & ASCII.NUL, Call, After);
   end On_Change_State;

   ---------------------
   -- On_Change_State --
   ---------------------

   procedure On_Change_State
      (Self  : not null access Gsimple_Action_Record;
       Call  : Cb_GObject_Gvariant_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "change-state" & ASCII.NUL, Call, After, Slot);
   end On_Change_State;

end Glib.Simple_Action;
