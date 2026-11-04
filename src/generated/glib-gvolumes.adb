------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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
with Glib.Error;
with Glib.Values;              use Glib.Values;
with Gtk.Arguments;            use Gtk.Arguments;

use type Glib.Error.GError;

package body Glib.GVolumes is

   function Convert (R : Glib.GVolumes.Gvolume) return System.Address is
   begin
      return System.Address (R);
   end Convert;

   function Convert (R : System.Address) return Glib.GVolumes.Gvolume is
   begin
      return Glib.GVolumes.Gvolume (R);
   end Convert;

   procedure C_G_Volume_Eject_With_Operation
      (Self            : Gvolume;
       Flags           : Glib.GEnums.GMount_Unmount_Flags;
       Mount_Operation : System.Address;
       Cancellable     : System.Address;
       Callback        : System.Address;
       User_Data       : System.Address);
   pragma Import (C, C_G_Volume_Eject_With_Operation, "g_volume_eject_with_operation");
   --  Ejects a volume. This is an asynchronous operation, and is finished by
   --  calling Glib.GVolumes.Eject_With_Operation_Finish with the Volume and
   --  Glib.G_Async_Result data returned in the Callback.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the unmount if required for eject
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback, or null
   --  @param User_Data user data passed to Callback

   procedure C_G_Volume_Mount
      (Self            : Gvolume;
       Flags           : Glib.GEnums.GMount_Mount_Flags;
       Mount_Operation : System.Address;
       Cancellable     : System.Address;
       Callback        : System.Address;
       User_Data       : System.Address);
   pragma Import (C, C_G_Volume_Mount, "g_volume_mount");
   --  Mounts a volume. This is an asynchronous operation, and is finished by
   --  calling Glib.GVolumes.Mount_Finish with the Volume and
   --  Glib.G_Async_Result returned in the Callback.
   --  @param Flags flags affecting the operation
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback, or null
   --  @param User_Data user data that gets passed to Callback

   function To_Gasync_Ready_Callback is new Ada.Unchecked_Conversion
     (System.Address, Gasync_Ready_Callback);

   function To_Address is new Ada.Unchecked_Conversion
     (Gasync_Ready_Callback, System.Address);

   procedure Internal_Gasync_Ready_Callback
      (Source_Object : System.Address;
       Res           : Glib.G_Async_Result;
       Data          : System.Address);
   pragma Convention (C, Internal_Gasync_Ready_Callback);
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.
   --  @param Data user data passed to the callback.

   ------------------------------------
   -- Internal_Gasync_Ready_Callback --
   ------------------------------------

   procedure Internal_Gasync_Ready_Callback
      (Source_Object : System.Address;
       Res           : Glib.G_Async_Result;
       Data          : System.Address)
   is
      Func         : constant Gasync_Ready_Callback := To_Gasync_Ready_Callback (Data);
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      Func (Get_User_Data (Source_Object, Stub_GObject), Res);
   end Internal_Gasync_Ready_Callback;

   ---------------
   -- Can_Eject --
   ---------------

   function Can_Eject (Self : Gvolume) return Boolean is
      function Internal (Self : Gvolume) return Glib.Gboolean;
      pragma Import (C, Internal, "g_volume_can_eject");
   begin
      return Internal (Self) /= 0;
   end Can_Eject;

   ---------------
   -- Can_Mount --
   ---------------

   function Can_Mount (Self : Gvolume) return Boolean is
      function Internal (Self : Gvolume) return Glib.Gboolean;
      pragma Import (C, Internal, "g_volume_can_mount");
   begin
      return Internal (Self) /= 0;
   end Can_Mount;

   --------------------------
   -- Eject_With_Operation --
   --------------------------

   procedure Eject_With_Operation
      (Self            : Gvolume;
       Flags           : Glib.GEnums.GMount_Unmount_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Volume_Eject_With_Operation (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Volume_Eject_With_Operation (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Eject_With_Operation;

   ---------------------------------
   -- Eject_With_Operation_Finish --
   ---------------------------------

   function Eject_With_Operation_Finish
      (Self   : Gvolume;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : Gvolume;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_volume_eject_with_operation_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Eject_With_Operation_Finish;

   ---------------------------
   -- Enumerate_Identifiers --
   ---------------------------

   function Enumerate_Identifiers
      (Self : Gvolume) return GNAT.Strings.String_List
   is
      function Internal (Self : Gvolume) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_volume_enumerate_identifiers");
   begin
      return To_String_List_And_Free (Internal (Self));
   end Enumerate_Identifiers;

   --------------------
   -- Get_Identifier --
   --------------------

   function Get_Identifier
      (Self : Gvolume;
       Kind : UTF8_String) return UTF8_String
   is
      function Internal
         (Self : Gvolume;
          Kind : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_volume_get_identifier");
      Tmp_Kind   : Gtkada.Types.Chars_Ptr := New_String (Kind);
      Tmp_Return : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Return := Internal (Self, Tmp_Kind);
      Free (Tmp_Kind);
      return Gtkada.Bindings.Value_And_Free (Tmp_Return);
   end Get_Identifier;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Gvolume) return UTF8_String is
      function Internal (Self : Gvolume) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_volume_get_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Name;

   ------------------
   -- Get_Sort_Key --
   ------------------

   function Get_Sort_Key (Self : Gvolume) return UTF8_String is
      function Internal (Self : Gvolume) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_volume_get_sort_key");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Sort_Key;

   --------------
   -- Get_Uuid --
   --------------

   function Get_Uuid (Self : Gvolume) return UTF8_String is
      function Internal (Self : Gvolume) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_volume_get_uuid");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Uuid;

   -----------
   -- Mount --
   -----------

   procedure Mount
      (Self            : Gvolume;
       Flags           : Glib.GEnums.GMount_Mount_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Volume_Mount (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Volume_Mount (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Mount;

   ------------------
   -- Mount_Finish --
   ------------------

   function Mount_Finish
      (Self   : Gvolume;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : Gvolume;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_volume_mount_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Mount_Finish;

   ----------------------
   -- Should_Automount --
   ----------------------

   function Should_Automount (Self : Gvolume) return Boolean is
      function Internal (Self : Gvolume) return Glib.Gboolean;
      pragma Import (C, Internal, "g_volume_should_automount");
   begin
      return Internal (Self) /= 0;
   end Should_Automount;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gvolume_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gvolume_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : Gvolume;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gvolume_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : Gvolume;
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

   procedure Marsh_Gvolume_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gvolume_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gvolume;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gvolume_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gvolume_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gvolume;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
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
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   ------------------------
   -- Marsh_Gvolume_Void --
   ------------------------

   procedure Marsh_Gvolume_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gvolume_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gvolume := Gvolume (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gvolume_Void;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : Gvolume;
       Call  : Cb_Gvolume_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : Gvolume;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

   ----------------
   -- On_Removed --
   ----------------

   procedure On_Removed
      (Self  : Gvolume;
       Call  : Cb_Gvolume_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "removed" & ASCII.NUL, Call, After);
   end On_Removed;

   ----------------
   -- On_Removed --
   ----------------

   procedure On_Removed
      (Self  : Gvolume;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "removed" & ASCII.NUL, Call, After, Slot);
   end On_Removed;

   function "+" (W : Gvolume) return Gvolume is
   begin
      return W;
   end "+";

end Glib.GVolumes;
