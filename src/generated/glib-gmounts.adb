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

package body Glib.GMounts is

   procedure C_G_Mount_Eject_With_Operation
      (Self            : Gmount;
       Flags           : Glib.GEnums.GMount_Unmount_Flags;
       Mount_Operation : System.Address;
       Cancellable     : System.Address;
       Callback        : System.Address;
       User_Data       : System.Address);
   pragma Import (C, C_G_Mount_Eject_With_Operation, "g_mount_eject_with_operation");
   --  Ejects a mount. This is an asynchronous operation, and is finished by
   --  calling Glib.GMounts.Eject_With_Operation_Finish with the Mount and
   --  Glib.G_Async_Result data returned in the Callback.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the unmount if required for eject
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.
   --  @param User_Data user data passed to Callback.

   procedure C_G_Mount_Guess_Content_Type
      (Self         : Gmount;
       Force_Rescan : Glib.Gboolean;
       Cancellable  : System.Address;
       Callback     : System.Address;
       User_Data    : System.Address);
   pragma Import (C, C_G_Mount_Guess_Content_Type, "g_mount_guess_content_type");
   --  Tries to guess the type of content stored on Mount. Returns one or more
   --  textual identifiers of well-known content types (typically prefixed with
   --  "x-content/"), e.g. x-content/image-dcf for camera memory cards. See the
   --  [shared-mime-info](http://www.freedesktop.org/wiki/Specifications/shared-mime-info-spec)
   --  specification for more on x-content types.
   --  This is an asynchronous operation (see
   --  Glib.GMounts.Guess_Content_Type_Sync for the synchronous version), and
   --  is finished by calling Glib.GMounts.Guess_Content_Type_Finish with the
   --  Mount and Glib.G_Async_Result data returned in the Callback.
   --  Since: gtk+ 2.18
   --  @param Force_Rescan Whether to force a rescan of the content. Otherwise
   --  a cached result will be used if available
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback
   --  @param User_Data user data passed to Callback

   procedure C_G_Mount_Remount
      (Self            : Gmount;
       Flags           : Glib.GEnums.GMount_Mount_Flags;
       Mount_Operation : System.Address;
       Cancellable     : System.Address;
       Callback        : System.Address;
       User_Data       : System.Address);
   pragma Import (C, C_G_Mount_Remount, "g_mount_remount");
   --  Remounts a mount. This is an asynchronous operation, and is finished by
   --  calling Glib.GMounts.Remount_Finish with the Mount and GAsync_Results
   --  data returned in the Callback.
   --  Remounting is useful when some setting affecting the operation of the
   --  volume has been changed, as these may need a remount to take affect.
   --  While this is semantically equivalent with unmounting and then
   --  remounting not all backends might need to actually be unmounted.
   --  @param Flags flags affecting the operation
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.
   --  @param User_Data user data passed to Callback.

   procedure C_G_Mount_Unmount_With_Operation
      (Self            : Gmount;
       Flags           : Glib.GEnums.GMount_Unmount_Flags;
       Mount_Operation : System.Address;
       Cancellable     : System.Address;
       Callback        : System.Address;
       User_Data       : System.Address);
   pragma Import (C, C_G_Mount_Unmount_With_Operation, "g_mount_unmount_with_operation");
   --  Unmounts a mount. This is an asynchronous operation, and is finished by
   --  calling Glib.GMounts.Unmount_With_Operation_Finish with the Mount and
   --  Glib.G_Async_Result data returned in the Callback.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the operation
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.
   --  @param User_Data user data passed to Callback.

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

   function Can_Eject (Self : Gmount) return Boolean is
      function Internal (Self : Gmount) return Glib.Gboolean;
      pragma Import (C, Internal, "g_mount_can_eject");
   begin
      return Internal (Self) /= 0;
   end Can_Eject;

   -----------------
   -- Can_Unmount --
   -----------------

   function Can_Unmount (Self : Gmount) return Boolean is
      function Internal (Self : Gmount) return Glib.Gboolean;
      pragma Import (C, Internal, "g_mount_can_unmount");
   begin
      return Internal (Self) /= 0;
   end Can_Unmount;

   --------------------------
   -- Eject_With_Operation --
   --------------------------

   procedure Eject_With_Operation
      (Self            : Gmount;
       Flags           : Glib.GEnums.GMount_Unmount_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Mount_Eject_With_Operation (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Mount_Eject_With_Operation (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Eject_With_Operation;

   ---------------------------------
   -- Eject_With_Operation_Finish --
   ---------------------------------

   function Eject_With_Operation_Finish
      (Self   : Gmount;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : Gmount;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_mount_eject_with_operation_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Eject_With_Operation_Finish;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Gmount) return UTF8_String is
      function Internal (Self : Gmount) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_mount_get_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Name;

   ------------------
   -- Get_Sort_Key --
   ------------------

   function Get_Sort_Key (Self : Gmount) return UTF8_String is
      function Internal (Self : Gmount) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_mount_get_sort_key");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Sort_Key;

   --------------
   -- Get_Uuid --
   --------------

   function Get_Uuid (Self : Gmount) return UTF8_String is
      function Internal (Self : Gmount) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_mount_get_uuid");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Uuid;

   ------------------------
   -- Guess_Content_Type --
   ------------------------

   procedure Guess_Content_Type
      (Self         : Gmount;
       Force_Rescan : Boolean;
       Cancellable  : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback     : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Mount_Guess_Content_Type (Self, Boolean'Pos (Force_Rescan), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Mount_Guess_Content_Type (Self, Boolean'Pos (Force_Rescan), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Guess_Content_Type;

   -------------------------------
   -- Guess_Content_Type_Finish --
   -------------------------------

   function Guess_Content_Type_Finish
      (Self   : Gmount;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return GNAT.Strings.String_List
   is
      function Internal
         (Self      : Gmount;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError)
          return chars_ptr_array_access;
      pragma Import (C, Internal, "g_mount_guess_content_type_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : chars_ptr_array_access;
   begin
      Tmp_Return := Internal (Self, Result, Acc_Error'Access);
      Error := Acc_Error;
      if Error = null then
         return To_String_List_And_Free (Tmp_Return);
      else
         g_strfreev (Tmp_Return);
         return (1..0 => null);
      end if;
   end Guess_Content_Type_Finish;

   -----------------------------
   -- Guess_Content_Type_Sync --
   -----------------------------

   function Guess_Content_Type_Sync
      (Self         : Gmount;
       Force_Rescan : Boolean;
       Cancellable  : access Glib.Cancellable.Gcancellable_Record'Class;
       Error        : out Glib.Error.GError) return GNAT.Strings.String_List
   is
      function Internal
         (Self         : Gmount;
          Force_Rescan : Glib.Gboolean;
          Cancellable  : System.Address;
          Acc_Error    : access Glib.Error.GError)
          return chars_ptr_array_access;
      pragma Import (C, Internal, "g_mount_guess_content_type_sync");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : chars_ptr_array_access;
   begin
      Tmp_Return := Internal (Self, Boolean'Pos (Force_Rescan), Get_Object_Or_Null (GObject (Cancellable)), Acc_Error'Access);
      Error := Acc_Error;
      if Error = null then
         return To_String_List_And_Free (Tmp_Return);
      else
         g_strfreev (Tmp_Return);
         return (1..0 => null);
      end if;
   end Guess_Content_Type_Sync;

   -----------------
   -- Is_Shadowed --
   -----------------

   function Is_Shadowed (Self : Gmount) return Boolean is
      function Internal (Self : Gmount) return Glib.Gboolean;
      pragma Import (C, Internal, "g_mount_is_shadowed");
   begin
      return Internal (Self) /= 0;
   end Is_Shadowed;

   -------------
   -- Remount --
   -------------

   procedure Remount
      (Self            : Gmount;
       Flags           : Glib.GEnums.GMount_Mount_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Mount_Remount (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Mount_Remount (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Remount;

   --------------------
   -- Remount_Finish --
   --------------------

   function Remount_Finish
      (Self   : Gmount;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : Gmount;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_mount_remount_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Remount_Finish;

   ----------------------------
   -- Unmount_With_Operation --
   ----------------------------

   procedure Unmount_With_Operation
      (Self            : Gmount;
       Flags           : Glib.GEnums.GMount_Unmount_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Mount_Unmount_With_Operation (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Mount_Unmount_With_Operation (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Unmount_With_Operation;

   -----------------------------------
   -- Unmount_With_Operation_Finish --
   -----------------------------------

   function Unmount_With_Operation_Finish
      (Self   : Gmount;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : Gmount;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_mount_unmount_with_operation_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Unmount_With_Operation_Finish;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gmount_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gmount_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : Gmount;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmount_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : Gmount;
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

   procedure Marsh_Gmount_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gmount_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gmount;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gmount_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gmount_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gmount;
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

   -----------------------
   -- Marsh_Gmount_Void --
   -----------------------

   procedure Marsh_Gmount_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gmount_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gmount := Gmount (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gmount_Void;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : Gmount;
       Call  : Cb_Gmount_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : Gmount;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

   --------------------
   -- On_Pre_Unmount --
   --------------------

   procedure On_Pre_Unmount
      (Self  : Gmount;
       Call  : Cb_Gmount_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "pre-unmount" & ASCII.NUL, Call, After);
   end On_Pre_Unmount;

   --------------------
   -- On_Pre_Unmount --
   --------------------

   procedure On_Pre_Unmount
      (Self  : Gmount;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "pre-unmount" & ASCII.NUL, Call, After, Slot);
   end On_Pre_Unmount;

   ------------------
   -- On_Unmounted --
   ------------------

   procedure On_Unmounted
      (Self  : Gmount;
       Call  : Cb_Gmount_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "unmounted" & ASCII.NUL, Call, After);
   end On_Unmounted;

   ------------------
   -- On_Unmounted --
   ------------------

   procedure On_Unmounted
      (Self  : Gmount;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "unmounted" & ASCII.NUL, Call, After, Slot);
   end On_Unmounted;

   function "+" (W : Gmount) return Gmount is
   begin
      return W;
   end "+";

end Glib.GMounts;
