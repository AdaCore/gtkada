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

package body Gtk.Accel_Map is

   procedure C_Gtk_Accel_Map_Foreach
      (Data         : System.Address;
       Foreach_Func : System.Address);
   pragma Import (C, C_Gtk_Accel_Map_Foreach, "gtk_accel_map_foreach");
   --  Loops over the entries in the accelerator map whose accel path doesn't
   --  match any of the filters added with Gtk.Accel_Map.Add_Filter, and
   --  execute Foreach_Func on each. The signature of Foreach_Func is that of
   --  Gtk_Accel_Map_Foreach, the Changed parameter indicates whether this
   --  accelerator was changed during runtime (thus, would need saving during
   --  an accelerator map dump).
   --  "data": data to be passed into Foreach_Func
   --  "foreach_func": function to be executed for each accel map entry which
   --  is not filtered out

   procedure C_Gtk_Accel_Map_Foreach_Unfiltered
      (Data         : System.Address;
       Foreach_Func : System.Address);
   pragma Import (C, C_Gtk_Accel_Map_Foreach_Unfiltered, "gtk_accel_map_foreach_unfiltered");
   --  Loops over all entries in the accelerator map, and execute Foreach_Func
   --  on each. The signature of Foreach_Func is that of Gtk_Accel_Map_Foreach,
   --  the Changed parameter indicates whether this accelerator was changed
   --  during runtime (thus, would need saving during an accelerator map dump).
   --  "data": data to be passed into Foreach_Func
   --  "foreach_func": function to be executed for each accel map entry

   function To_Gtk_Accel_Map_Foreach is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Accel_Map_Foreach);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Accel_Map_Foreach, System.Address);

   procedure Internal_Gtk_Accel_Map_Foreach
      (Data       : System.Address;
       Accel_Path : Gtkada.Types.Chars_Ptr;
       Accel_Key  : Gdk.Types.Gdk_Key_Type;
       Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
       Changed    : Glib.Gboolean);
   pragma Convention (C, Internal_Gtk_Accel_Map_Foreach);
   --  "data": User data passed to Gtk.Accel_Map.Foreach or
   --  Gtk.Accel_Map.Foreach_Unfiltered
   --  "accel_path": Accel path of the current accelerator
   --  "accel_key": Key of the current accelerator
   --  "accel_mods": Modifiers of the current accelerator
   --  "changed": Changed flag of the accelerator (if True, accelerator has
   --  changed during runtime and would need to be saved during an accelerator
   --  dump)

   ------------------------------------
   -- Internal_Gtk_Accel_Map_Foreach --
   ------------------------------------

   procedure Internal_Gtk_Accel_Map_Foreach
      (Data       : System.Address;
       Accel_Path : Gtkada.Types.Chars_Ptr;
       Accel_Key  : Gdk.Types.Gdk_Key_Type;
       Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
       Changed    : Glib.Gboolean)
   is
      Func : constant Gtk_Accel_Map_Foreach := To_Gtk_Accel_Map_Foreach (Data);
   begin
      Func (Gtkada.Bindings.Value_Allowing_Null (Accel_Path), Accel_Key, Accel_Mods, Changed /= 0);
   end Internal_Gtk_Accel_Map_Foreach;

   package Type_Conversion_Gtk_Accel_Map is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Accel_Map_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Accel_Map);

   -------------
   -- Foreach --
   -------------

   procedure Foreach (Foreach_Func : Gtk_Accel_Map_Foreach) is
   begin
      if Foreach_Func = null then
         C_Gtk_Accel_Map_Foreach (System.Null_Address, System.Null_Address);
      else
         C_Gtk_Accel_Map_Foreach (To_Address (Foreach_Func), Internal_Gtk_Accel_Map_Foreach'Address);
      end if;
   end Foreach;

   ------------------------
   -- Foreach_Unfiltered --
   ------------------------

   procedure Foreach_Unfiltered (Foreach_Func : Gtk_Accel_Map_Foreach) is
   begin
      if Foreach_Func = null then
         C_Gtk_Accel_Map_Foreach_Unfiltered (System.Null_Address, System.Null_Address);
      else
         C_Gtk_Accel_Map_Foreach_Unfiltered (To_Address (Foreach_Func), Internal_Gtk_Accel_Map_Foreach'Address);
      end if;
   end Foreach_Unfiltered;

   package body Foreach_Unfiltered_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Accel_Map_Foreach is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Accel_Map_Foreach);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Accel_Map_Foreach, System.Address);

      procedure Internal_Cb
         (Data       : System.Address;
          Accel_Path : Gtkada.Types.Chars_Ptr;
          Accel_Key  : Gdk.Types.Gdk_Key_Type;
          Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
          Changed    : Glib.Gboolean);
      pragma Convention (C, Internal_Cb);
      --  "data": User data passed to Gtk.Accel_Map.Foreach or
      --  Gtk.Accel_Map.Foreach_Unfiltered
      --  "accel_path": Accel path of the current accelerator
      --  "accel_key": Key of the current accelerator
      --  "accel_mods": Modifiers of the current accelerator
      --  "changed": Changed flag of the accelerator (if True, accelerator has
      --  changed during runtime and would need to be saved during an
      --  accelerator dump)

      ------------------------
      -- Foreach_Unfiltered --
      ------------------------

      procedure Foreach_Unfiltered
         (Data         : User_Data_Type;
          Foreach_Func : Gtk_Accel_Map_Foreach)
      is
         D : System.Address;
      begin
         if Foreach_Func = null then
            C_Gtk_Accel_Map_Foreach_Unfiltered (System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Foreach_Func), Data);
            C_Gtk_Accel_Map_Foreach_Unfiltered (D, Internal_Cb'Address);
            Users.Free_Data (D);
         end if;
      end Foreach_Unfiltered;

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Data       : System.Address;
          Accel_Path : Gtkada.Types.Chars_Ptr;
          Accel_Key  : Gdk.Types.Gdk_Key_Type;
          Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
          Changed    : Glib.Gboolean)
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         To_Gtk_Accel_Map_Foreach (D.Func) (D.Data.all, Gtkada.Bindings.Value_Allowing_Null (Accel_Path), Accel_Key, Accel_Mods, Changed /= 0);
      end Internal_Cb;

   end Foreach_Unfiltered_User_Data;

   package body Foreach_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Accel_Map_Foreach is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Accel_Map_Foreach);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Accel_Map_Foreach, System.Address);

      procedure Internal_Cb
         (Data       : System.Address;
          Accel_Path : Gtkada.Types.Chars_Ptr;
          Accel_Key  : Gdk.Types.Gdk_Key_Type;
          Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
          Changed    : Glib.Gboolean);
      pragma Convention (C, Internal_Cb);
      --  "data": User data passed to Gtk.Accel_Map.Foreach or
      --  Gtk.Accel_Map.Foreach_Unfiltered
      --  "accel_path": Accel path of the current accelerator
      --  "accel_key": Key of the current accelerator
      --  "accel_mods": Modifiers of the current accelerator
      --  "changed": Changed flag of the accelerator (if True, accelerator has
      --  changed during runtime and would need to be saved during an
      --  accelerator dump)

      -------------
      -- Foreach --
      -------------

      procedure Foreach
         (Data         : User_Data_Type;
          Foreach_Func : Gtk_Accel_Map_Foreach)
      is
         D : System.Address;
      begin
         if Foreach_Func = null then
            C_Gtk_Accel_Map_Foreach (System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Foreach_Func), Data);
            C_Gtk_Accel_Map_Foreach (D, Internal_Cb'Address);
            Users.Free_Data (D);
         end if;
      end Foreach;

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Data       : System.Address;
          Accel_Path : Gtkada.Types.Chars_Ptr;
          Accel_Key  : Gdk.Types.Gdk_Key_Type;
          Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
          Changed    : Glib.Gboolean)
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         To_Gtk_Accel_Map_Foreach (D.Func) (D.Data.all, Gtkada.Bindings.Value_Allowing_Null (Accel_Path), Accel_Key, Accel_Mods, Changed /= 0);
      end Internal_Cb;

   end Foreach_User_Data;

   ---------------
   -- Add_Entry --
   ---------------

   procedure Add_Entry
      (Accel_Path : UTF8_String;
       Accel_Key  : Gdk.Types.Gdk_Key_Type;
       Accel_Mods : Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal
         (Accel_Path : Gtkada.Types.Chars_Ptr;
          Accel_Key  : Gdk.Types.Gdk_Key_Type;
          Accel_Mods : Gdk.Types.Gdk_Modifier_Type);
      pragma Import (C, Internal, "gtk_accel_map_add_entry");
      Tmp_Accel_Path : Gtkada.Types.Chars_Ptr := New_String (Accel_Path);
   begin
      Internal (Tmp_Accel_Path, Accel_Key, Accel_Mods);
      Free (Tmp_Accel_Path);
   end Add_Entry;

   ----------------
   -- Add_Filter --
   ----------------

   procedure Add_Filter (Filter_Pattern : UTF8_String) is
      procedure Internal (Filter_Pattern : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_accel_map_add_filter");
      Tmp_Filter_Pattern : Gtkada.Types.Chars_Ptr := New_String (Filter_Pattern);
   begin
      Internal (Tmp_Filter_Pattern);
      Free (Tmp_Filter_Pattern);
   end Add_Filter;

   ------------------
   -- Change_Entry --
   ------------------

   function Change_Entry
      (Accel_Path : UTF8_String;
       Accel_Key  : Gdk.Types.Gdk_Key_Type;
       Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
       Replace    : Boolean) return Boolean
   is
      function Internal
         (Accel_Path : Gtkada.Types.Chars_Ptr;
          Accel_Key  : Gdk.Types.Gdk_Key_Type;
          Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
          Replace    : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accel_map_change_entry");
      Tmp_Accel_Path : Gtkada.Types.Chars_Ptr := New_String (Accel_Path);
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Tmp_Accel_Path, Accel_Key, Accel_Mods, Boolean'Pos (Replace));
      Free (Tmp_Accel_Path);
      return Tmp_Return /= 0;
   end Change_Entry;

   ---------
   -- Get --
   ---------

   function Get return Gtk_Accel_Map is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_accel_map_get");
      Stub_Gtk_Accel_Map : Gtk_Accel_Map_Record;
   begin
      return Gtk.Accel_Map.Gtk_Accel_Map (Get_User_Data (Internal, Stub_Gtk_Accel_Map));
   end Get;

   ----------
   -- Load --
   ----------

   procedure Load (File_Name : UTF8_String) is
      procedure Internal (File_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_accel_map_load");
      Tmp_File_Name : Gtkada.Types.Chars_Ptr := New_String (File_Name);
   begin
      Internal (Tmp_File_Name);
      Free (Tmp_File_Name);
   end Load;

   -------------
   -- Load_Fd --
   -------------

   procedure Load_Fd (Fd : Glib.Gint) is
      procedure Internal (Fd : Glib.Gint);
      pragma Import (C, Internal, "gtk_accel_map_load_fd");
   begin
      Internal (Fd);
   end Load_Fd;

   ---------------
   -- Lock_Path --
   ---------------

   procedure Lock_Path (Accel_Path : UTF8_String) is
      procedure Internal (Accel_Path : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_accel_map_lock_path");
      Tmp_Accel_Path : Gtkada.Types.Chars_Ptr := New_String (Accel_Path);
   begin
      Internal (Tmp_Accel_Path);
      Free (Tmp_Accel_Path);
   end Lock_Path;

   ------------------
   -- Lookup_Entry --
   ------------------

   procedure Lookup_Entry
      (Accel_Path : UTF8_String;
       Key        : out Gtk.Accel_Group.Gtk_Accel_Key;
       Found      : out Boolean)
   is
      function Internal
         (Accel_Path : Gtkada.Types.Chars_Ptr;
          Acc_Key    : access Gtk.Accel_Group.Gtk_Accel_Key)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accel_map_lookup_entry");
      Acc_Key        : aliased Gtk.Accel_Group.Gtk_Accel_Key;
      Tmp_Accel_Path : Gtkada.Types.Chars_Ptr := New_String (Accel_Path);
      Tmp_Acc_Key    : aliased Gtk.Accel_Group.Gtk_Accel_Key;
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Tmp_Accel_Path, Tmp_Acc_Key'Access);
      Acc_Key := Tmp_Acc_Key;
      Free (Tmp_Accel_Path);
      Key := Acc_Key;
      Found := Tmp_Return /= 0;
   end Lookup_Entry;

   ----------
   -- Save --
   ----------

   procedure Save (File_Name : UTF8_String) is
      procedure Internal (File_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_accel_map_save");
      Tmp_File_Name : Gtkada.Types.Chars_Ptr := New_String (File_Name);
   begin
      Internal (Tmp_File_Name);
      Free (Tmp_File_Name);
   end Save;

   -------------
   -- Save_Fd --
   -------------

   procedure Save_Fd (Fd : Glib.Gint) is
      procedure Internal (Fd : Glib.Gint);
      pragma Import (C, Internal, "gtk_accel_map_save_fd");
   begin
      Internal (Fd);
   end Save_Fd;

   -----------------
   -- Unlock_Path --
   -----------------

   procedure Unlock_Path (Accel_Path : UTF8_String) is
      procedure Internal (Accel_Path : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_accel_map_unlock_path");
      Tmp_Accel_Path : Gtkada.Types.Chars_Ptr := New_String (Accel_Path);
   begin
      Internal (Tmp_Accel_Path);
      Free (Tmp_Accel_Path);
   end Unlock_Path;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Accel_Map_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Accel_Map_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void);

   procedure Connect
      (Object  : access Gtk_Accel_Map_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Accel_Map_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Accel_Map_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void);

   procedure Marsh_Gtk_Accel_Map_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Accel_Map_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Accel_Map_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Accel_Map_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Accel_Map_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Accel_Map_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -------------------------------------------------------------------
   -- Marsh_GObject_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void --
   -------------------------------------------------------------------

   procedure Marsh_GObject_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_Gdk_Key_Type (Params, 2), Unchecked_To_Gdk_Modifier_Type (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void;

   -------------------------------------------------------------------------
   -- Marsh_Gtk_Accel_Map_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void --
   -------------------------------------------------------------------------

   procedure Marsh_Gtk_Accel_Map_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Accel_Map_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Accel_Map := Gtk_Accel_Map (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1), Unchecked_To_Gdk_Key_Type (Params, 2), Unchecked_To_Gdk_Modifier_Type (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Accel_Map_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Accel_Map_Record;
       Call  : Cb_Gtk_Accel_Map_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Accel_Map_Record;
       Call  : Cb_GObject_UTF8_String_Gdk_Key_Type_Gdk_Modifier_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

end Gtk.Accel_Map;
