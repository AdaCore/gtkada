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

package body Gtk.Accel_Group is

   function From_Object_Free (B : access Gtk_Accel_Key) return Gtk_Accel_Key is
      Result : constant Gtk_Accel_Key := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function C_Gtk_Accel_Group_Find
      (Accel_Group : System.Address;
       Find_Func   : System.Address;
       Data        : System.Address) return access Gtk_Accel_Key;
   pragma Import (C, C_Gtk_Accel_Group_Find, "gtk_accel_group_find");
   --  Finds the first entry in an accelerator group for which Find_Func
   --  returns True and returns its Gtk.Accel_Group.Gtk_Accel_Key.
   --  "find_func": a function to filter the entries of Accel_Group with
   --  "data": data to pass to Find_Func

   function To_Gtk_Accel_Group_Find_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Accel_Group_Find_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Accel_Group_Find_Func, System.Address);

   function Internal_Gtk_Accel_Group_Find_Func
      (Key     : access Gtk.Accel_Group.Gtk_Accel_Key;
       Closure : System.Address;
       Data    : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_Accel_Group_Find_Func);

   ----------------------------------------
   -- Internal_Gtk_Accel_Group_Find_Func --
   ----------------------------------------

   function Internal_Gtk_Accel_Group_Find_Func
      (Key     : access Gtk.Accel_Group.Gtk_Accel_Key;
       Closure : System.Address;
       Data    : System.Address) return Glib.Gboolean
   is
      Func : constant Gtk_Accel_Group_Find_Func := To_Gtk_Accel_Group_Find_Func (Data);
   begin
      return Boolean'Pos (Func (Key.all, Closure));
   end Internal_Gtk_Accel_Group_Find_Func;

   package Type_Conversion_Gtk_Accel_Group is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Accel_Group_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Accel_Group);

   -------------------------
   -- Gtk_Accel_Group_New --
   -------------------------

   function Gtk_Accel_Group_New return Gtk_Accel_Group is
      Accel_Group : constant Gtk_Accel_Group := new Gtk_Accel_Group_Record;
   begin
      Gtk.Accel_Group.Initialize (Accel_Group);
      return Accel_Group;
   end Gtk_Accel_Group_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Accel_Group : out Gtk_Accel_Group) is
   begin
      Accel_Group := new Gtk_Accel_Group_Record;
      Gtk.Accel_Group.Initialize (Accel_Group);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Accel_Group : not null access Gtk_Accel_Group_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_accel_group_new");
   begin
      if not Accel_Group.Is_Created then
         Set_Object (Accel_Group, Internal);
      end if;
   end Initialize;

   --------------
   -- Activate --
   --------------

   function Activate
      (Accel_Group   : not null access Gtk_Accel_Group_Record;
       Accel_Quark   : Glib.GQuark;
       Acceleratable : not null access Glib.Object.GObject_Record'Class;
       Accel_Key     : Gdk.Types.Gdk_Key_Type;
       Accel_Mods    : Gdk.Types.Gdk_Modifier_Type) return Boolean
   is
      function Internal
         (Accel_Group   : System.Address;
          Accel_Quark   : Glib.GQuark;
          Acceleratable : System.Address;
          Accel_Key     : Gdk.Types.Gdk_Key_Type;
          Accel_Mods    : Gdk.Types.Gdk_Modifier_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accel_group_activate");
   begin
      return Internal (Get_Object (Accel_Group), Accel_Quark, Get_Object (Acceleratable), Accel_Key, Accel_Mods) /= 0;
   end Activate;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Accel_Group : not null access Gtk_Accel_Group_Record;
       Accel_Key   : Gdk.Types.Gdk_Key_Type;
       Accel_Mods  : Gdk.Types.Gdk_Modifier_Type;
       Accel_Flags : Gtk_Accel_Flags;
       Closure     : C_Gtk_Accel_Group_Activate)
   is
      procedure Internal
         (Accel_Group : System.Address;
          Accel_Key   : Gdk.Types.Gdk_Key_Type;
          Accel_Mods  : Gdk.Types.Gdk_Modifier_Type;
          Accel_Flags : Gtk_Accel_Flags;
          Closure     : C_Gtk_Accel_Group_Activate);
      pragma Import (C, Internal, "gtk_accel_group_connect");
   begin
      Internal (Get_Object (Accel_Group), Accel_Key, Accel_Mods, Accel_Flags, Closure);
   end Connect;

   ---------------------
   -- Connect_By_Path --
   ---------------------

   procedure Connect_By_Path
      (Accel_Group : not null access Gtk_Accel_Group_Record;
       Accel_Path  : UTF8_String;
       Closure     : C_Gtk_Accel_Group_Activate)
   is
      procedure Internal
         (Accel_Group : System.Address;
          Accel_Path  : Gtkada.Types.Chars_Ptr;
          Closure     : C_Gtk_Accel_Group_Activate);
      pragma Import (C, Internal, "gtk_accel_group_connect_by_path");
      Tmp_Accel_Path : Gtkada.Types.Chars_Ptr := New_String (Accel_Path);
   begin
      Internal (Get_Object (Accel_Group), Tmp_Accel_Path, Closure);
      Free (Tmp_Accel_Path);
   end Connect_By_Path;

   ----------------
   -- Disconnect --
   ----------------

   function Disconnect
      (Accel_Group : not null access Gtk_Accel_Group_Record;
       Closure     : C_Gtk_Accel_Group_Activate) return Boolean
   is
      function Internal
         (Accel_Group : System.Address;
          Closure     : C_Gtk_Accel_Group_Activate) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accel_group_disconnect");
   begin
      return Internal (Get_Object (Accel_Group), Closure) /= 0;
   end Disconnect;

   --------------------
   -- Disconnect_Key --
   --------------------

   function Disconnect_Key
      (Accel_Group : not null access Gtk_Accel_Group_Record;
       Accel_Key   : Gdk.Types.Gdk_Key_Type;
       Accel_Mods  : Gdk.Types.Gdk_Modifier_Type) return Boolean
   is
      function Internal
         (Accel_Group : System.Address;
          Accel_Key   : Gdk.Types.Gdk_Key_Type;
          Accel_Mods  : Gdk.Types.Gdk_Modifier_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accel_group_disconnect_key");
   begin
      return Internal (Get_Object (Accel_Group), Accel_Key, Accel_Mods) /= 0;
   end Disconnect_Key;

   ----------
   -- Find --
   ----------

   function Find
      (Accel_Group : not null access Gtk_Accel_Group_Record;
       Find_Func   : Gtk_Accel_Group_Find_Func) return Gtk_Accel_Key
   is
   begin
      if Find_Func = null then
         return C_Gtk_Accel_Group_Find (Get_Object (Accel_Group), System.Null_Address, System.Null_Address).all;
      else
         return C_Gtk_Accel_Group_Find (Get_Object (Accel_Group), Internal_Gtk_Accel_Group_Find_Func'Address, To_Address (Find_Func)).all;
      end if;
   end Find;

   -------------------
   -- Get_Is_Locked --
   -------------------

   function Get_Is_Locked
      (Accel_Group : not null access Gtk_Accel_Group_Record) return Boolean
   is
      function Internal (Accel_Group : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accel_group_get_is_locked");
   begin
      return Internal (Get_Object (Accel_Group)) /= 0;
   end Get_Is_Locked;

   -----------------------
   -- Get_Modifier_Mask --
   -----------------------

   function Get_Modifier_Mask
      (Accel_Group : not null access Gtk_Accel_Group_Record)
       return Gdk.Types.Gdk_Modifier_Type
   is
      function Internal
         (Accel_Group : System.Address) return Gdk.Types.Gdk_Modifier_Type;
      pragma Import (C, Internal, "gtk_accel_group_get_modifier_mask");
   begin
      return Internal (Get_Object (Accel_Group));
   end Get_Modifier_Mask;

   ----------
   -- Lock --
   ----------

   procedure Lock (Accel_Group : not null access Gtk_Accel_Group_Record) is
      procedure Internal (Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_accel_group_lock");
   begin
      Internal (Get_Object (Accel_Group));
   end Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Accel_Group : not null access Gtk_Accel_Group_Record) is
      procedure Internal (Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_accel_group_unlock");
   begin
      Internal (Get_Object (Accel_Group));
   end Unlock;

   ---------------------------
   -- Accel_Groups_Activate --
   ---------------------------

   function Accel_Groups_Activate
      (Object     : not null access Glib.Object.GObject_Record'Class;
       Accel_Key  : Gdk.Types.Gdk_Key_Type;
       Accel_Mods : Gdk.Types.Gdk_Modifier_Type) return Boolean
   is
      function Internal
         (Object     : System.Address;
          Accel_Key  : Gdk.Types.Gdk_Key_Type;
          Accel_Mods : Gdk.Types.Gdk_Modifier_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accel_groups_activate");
   begin
      return Internal (Get_Object (Object), Accel_Key, Accel_Mods) /= 0;
   end Accel_Groups_Activate;

   ---------------------------
   -- Accelerator_Get_Label --
   ---------------------------

   function Accelerator_Get_Label
      (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type) return UTF8_String
   is
      function Internal
         (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
          Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type)
          return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_accelerator_get_label");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Accelerator_Key, Accelerator_Mods));
   end Accelerator_Get_Label;

   ----------------------
   -- Accelerator_Name --
   ----------------------

   function Accelerator_Name
      (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type) return UTF8_String
   is
      function Internal
         (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
          Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type)
          return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_accelerator_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Accelerator_Key, Accelerator_Mods));
   end Accelerator_Name;

   -----------------------
   -- Accelerator_Parse --
   -----------------------

   procedure Accelerator_Parse
      (Accelerator      : UTF8_String;
       Accelerator_Key  : out Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : out Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal
         (Accelerator      : Gtkada.Types.Chars_Ptr;
          Accelerator_Key  : out Gdk.Types.Gdk_Key_Type;
          Accelerator_Mods : out Gdk.Types.Gdk_Modifier_Type);
      pragma Import (C, Internal, "gtk_accelerator_parse");
      Tmp_Accelerator : Gtkada.Types.Chars_Ptr := New_String (Accelerator);
   begin
      Internal (Tmp_Accelerator, Accelerator_Key, Accelerator_Mods);
      Free (Tmp_Accelerator);
   end Accelerator_Parse;

   -----------------------
   -- Accelerator_Valid --
   -----------------------

   function Accelerator_Valid
      (Keyval    : Gdk.Types.Gdk_Key_Type;
       Modifiers : Gdk.Types.Gdk_Modifier_Type) return Boolean
   is
      function Internal
         (Keyval    : Gdk.Types.Gdk_Key_Type;
          Modifiers : Gdk.Types.Gdk_Modifier_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accelerator_valid");
   begin
      return Internal (Keyval, Modifiers) /= 0;
   end Accelerator_Valid;

   ------------------------
   -- From_Accel_Closure --
   ------------------------

   function From_Accel_Closure
      (Closure : C_Gtk_Accel_Group_Activate) return Gtk_Accel_Group
   is
      function Internal
         (Closure : C_Gtk_Accel_Group_Activate) return System.Address;
      pragma Import (C, Internal, "gtk_accel_group_from_accel_closure");
      Stub_Gtk_Accel_Group : Gtk_Accel_Group_Record;
   begin
      return Gtk.Accel_Group.Gtk_Accel_Group (Get_User_Data (Internal (Closure), Stub_Gtk_Accel_Group));
   end From_Accel_Closure;

   -----------------
   -- From_Object --
   -----------------

   function From_Object
      (Object : not null access Glib.Object.GObject_Record'Class)
       return Glib.Object.Object_List.GSlist
   is
      function Internal (Object : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_accel_groups_from_object");
      Tmp_Return : Glib.Object.Object_List.GSlist;
   begin
      Glib.Object.Object_List.Set_Object (Tmp_Return, Internal (Get_Object (Object)));
      return Tmp_Return;
   end From_Object;

   --------------------------
   -- Get_Default_Mod_Mask --
   --------------------------

   function Get_Default_Mod_Mask return Gdk.Types.Gdk_Modifier_Type is
      function Internal return Gdk.Types.Gdk_Modifier_Type;
      pragma Import (C, Internal, "gtk_accelerator_get_default_mod_mask");
   begin
      return Internal;
   end Get_Default_Mod_Mask;

   --------------------------
   -- Set_Default_Mod_Mask --
   --------------------------

   procedure Set_Default_Mod_Mask
      (Default_Mod_Mask : Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal (Default_Mod_Mask : Gdk.Types.Gdk_Modifier_Type);
      pragma Import (C, Internal, "gtk_accelerator_set_default_mod_mask");
   begin
      Internal (Default_Mod_Mask);
   end Set_Default_Mod_Mask;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Accel_Group_GObject_Guint_Gdk_Modifier_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Accel_Group_GObject_Guint_Gdk_Modifier_Type_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_GObject_Guint_Gdk_Modifier_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_GObject_Guint_Gdk_Modifier_Type_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Accel_Group_Guint_Gdk_Modifier_Type_Address_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Accel_Group_Guint_Gdk_Modifier_Type_Address_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Guint_Gdk_Modifier_Type_Address_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Guint_Gdk_Modifier_Type_Address_Void);

   procedure Connect
      (Object  : access Gtk_Accel_Group_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Accel_Group_GObject_Guint_Gdk_Modifier_Type_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Accel_Group_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Accel_Group_Guint_Gdk_Modifier_Type_Address_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Accel_Group_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_GObject_Guint_Gdk_Modifier_Type_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Accel_Group_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Gdk_Modifier_Type_Address_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_GObject_Guint_Gdk_Modifier_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_GObject_Guint_Gdk_Modifier_Type_Boolean);

   procedure Marsh_GObject_Guint_Gdk_Modifier_Type_Address_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Guint_Gdk_Modifier_Type_Address_Void);

   procedure Marsh_Gtk_Accel_Group_GObject_Guint_Gdk_Modifier_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Accel_Group_GObject_Guint_Gdk_Modifier_Type_Boolean);

   procedure Marsh_Gtk_Accel_Group_Guint_Gdk_Modifier_Type_Address_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Accel_Group_Guint_Gdk_Modifier_Type_Address_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Accel_Group_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Accel_Group_GObject_Guint_Gdk_Modifier_Type_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Accel_Group_GObject_Guint_Gdk_Modifier_Type_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Accel_Group_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Accel_Group_Guint_Gdk_Modifier_Type_Address_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Accel_Group_Guint_Gdk_Modifier_Type_Address_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Accel_Group_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_GObject_Guint_Gdk_Modifier_Type_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_GObject_Guint_Gdk_Modifier_Type_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Accel_Group_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Gdk_Modifier_Type_Address_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Guint_Gdk_Modifier_Type_Address_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -----------------------------------------------------------
   -- Marsh_GObject_GObject_Guint_Gdk_Modifier_Type_Boolean --
   -----------------------------------------------------------

   procedure Marsh_GObject_GObject_Guint_Gdk_Modifier_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_GObject_Guint_Gdk_Modifier_Type_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Object (Params, 1), Unchecked_To_Guint (Params, 2), Unchecked_To_Gdk_Modifier_Type (Params, 3));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_GObject_Guint_Gdk_Modifier_Type_Boolean;

   --------------------------------------------------------
   -- Marsh_GObject_Guint_Gdk_Modifier_Type_Address_Void --
   --------------------------------------------------------

   procedure Marsh_GObject_Guint_Gdk_Modifier_Type_Address_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Guint_Gdk_Modifier_Type_Address_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_Gdk_Modifier_Type (Params, 2), Unchecked_To_Address (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Guint_Gdk_Modifier_Type_Address_Void;

   -------------------------------------------------------------------
   -- Marsh_Gtk_Accel_Group_GObject_Guint_Gdk_Modifier_Type_Boolean --
   -------------------------------------------------------------------

   procedure Marsh_Gtk_Accel_Group_GObject_Guint_Gdk_Modifier_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Accel_Group_GObject_Guint_Gdk_Modifier_Type_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Accel_Group := Gtk_Accel_Group (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Object (Params, 1), Unchecked_To_Guint (Params, 2), Unchecked_To_Gdk_Modifier_Type (Params, 3));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Accel_Group_GObject_Guint_Gdk_Modifier_Type_Boolean;

   ----------------------------------------------------------------
   -- Marsh_Gtk_Accel_Group_Guint_Gdk_Modifier_Type_Address_Void --
   ----------------------------------------------------------------

   procedure Marsh_Gtk_Accel_Group_Guint_Gdk_Modifier_Type_Address_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Accel_Group_Guint_Gdk_Modifier_Type_Address_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Accel_Group := Gtk_Accel_Group (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_Gdk_Modifier_Type (Params, 2), Unchecked_To_Address (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Accel_Group_Guint_Gdk_Modifier_Type_Address_Void;

   -----------------------
   -- On_Accel_Activate --
   -----------------------

   procedure On_Accel_Activate
      (Self  : not null access Gtk_Accel_Group_Record;
       Call  : Cb_Gtk_Accel_Group_GObject_Guint_Gdk_Modifier_Type_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "accel-activate" & ASCII.NUL, Call, After);
   end On_Accel_Activate;

   -----------------------
   -- On_Accel_Activate --
   -----------------------

   procedure On_Accel_Activate
      (Self  : not null access Gtk_Accel_Group_Record;
       Call  : Cb_GObject_GObject_Guint_Gdk_Modifier_Type_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "accel-activate" & ASCII.NUL, Call, After, Slot);
   end On_Accel_Activate;

   ----------------------
   -- On_Accel_Changed --
   ----------------------

   procedure On_Accel_Changed
      (Self  : not null access Gtk_Accel_Group_Record;
       Call  : Cb_Gtk_Accel_Group_Guint_Gdk_Modifier_Type_Address_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "accel-changed" & ASCII.NUL, Call, After);
   end On_Accel_Changed;

   ----------------------
   -- On_Accel_Changed --
   ----------------------

   procedure On_Accel_Changed
      (Self  : not null access Gtk_Accel_Group_Record;
       Call  : Cb_GObject_Guint_Gdk_Modifier_Type_Address_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "accel-changed" & ASCII.NUL, Call, After, Slot);
   end On_Accel_Changed;

end Gtk.Accel_Group;
