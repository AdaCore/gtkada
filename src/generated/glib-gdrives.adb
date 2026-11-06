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

package body Glib.GDrives is

   procedure Internal_Poll_For_Media
     (Self        : Gdrive;
      Cancellable : System.Address;
      Callback    : System.Address;
      User_Data   : System.Address);
   pragma Import (C, Internal_Poll_For_Media, "g_drive_poll_for_media");

   function Internal_To_Gasync_Ready_Callback is new Ada.Unchecked_Conversion
     (System.Address, Gasync_Ready_Callback);

   function Internal_To_Address is new Ada.Unchecked_Conversion
     (Gasync_Ready_Callback, System.Address);

   procedure Internal_Callback
     (Source_Object : System.Address;
      Res           : Glib.G_Async_Result;
      Data          : System.Address);
   pragma Convention (C, Internal_Callback);
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.
   --  @param Data user data passed to the callback.

   -----------------------
   -- Internal_Callback --
   -----------------------

   procedure Internal_Callback
     (Source_Object : System.Address;
      Res           : Glib.G_Async_Result;
      Data          : System.Address)
   is
      Func         : constant Gasync_Ready_Callback :=
      Internal_To_Gasync_Ready_Callback (Data);
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      Func (Get_User_Data (Source_Object, Stub_GObject), Res);
   end Internal_Callback;

   --------------------
   -- Poll_For_Media --
   --------------------

   procedure Poll_For_Media
     (Self        : Gdrive;
      Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
      Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         Internal_Poll_For_Media
           (Self, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         Internal_Poll_For_Media
           (Self, Get_Object_Or_Null (GObject (Cancellable)),
            Internal_Callback'Address,
            Internal_To_Address (Callback));
      end if;
   end Poll_For_Media;

   package body Poll_For_Media_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gasync_Ready_Callback is new Ada.Unchecked_Conversion
        (System.Address, Gasync_Ready_Callback);

      function To_Address is new Ada.Unchecked_Conversion
        (Gasync_Ready_Callback, System.Address);

      procedure Internal_Cb
        (Source_Object : System.Address;
         Res           : Glib.G_Async_Result;
         Data          : System.Address);
      pragma Convention (C, Internal_Cb);
      --  Type definition for a function that will be called back when an
      --  asynchronous operation within GIO has been completed.
      --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
      --  invoked in a later iteration of the thread-default main context (see
      --  [methodGlib.MainContext.push_thread_default]) where the Gtask.Gtask
      --  was created. All other users of Gasync_Ready_Callback must likewise
      --  call it asynchronously in a later iteration of the main context.
      --  The asynchronous operation is guaranteed to have held a reference to
      --  Source_Object from the time when the `*_async` function was called,
      --  until after this callback returns.
      --  @param Source_Object the object the asynchronous operation was
      --  started with.
      --  @param Res a Glib.G_Async_Result.
      --  @param Data user data passed to the callback.

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
        (Source_Object : System.Address;
         Res           : Glib.G_Async_Result;
         Data          : System.Address)
      is
         D            : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_GObject : Glib.Object.GObject_Record;
      begin
         To_Gasync_Ready_Callback (D.Func) (Get_User_Data (Source_Object, Stub_GObject), Res, D.Data.all);
         Users.Free_Data (Data);
      exception
         when others =>
            Users.Free_Data (Data);
         end Internal_Cb;

         --------------------
         -- Poll_For_Media --
         --------------------

         procedure Poll_For_Media
           (Self        : Glib.GDrives.Gdrive;
            Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
            Callback    : Gasync_Ready_Callback;
            User_Data   : User_Data_Type)
         is
            D : System.Address;
         begin
            if Callback = null then
               Internal_Poll_For_Media
                 (Self, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
            else
               D := Users.Build (To_Address (Callback), User_Data);
               Internal_Poll_For_Media
                 (Self, Get_Object_Or_Null (GObject (Cancellable)), Internal_Cb'Address, D);
            end if;
         end Poll_For_Media;

      end Poll_For_Media_User_Data;

   procedure C_G_Drive_Eject_With_Operation
      (Self            : Gdrive;
       Flags           : Glib.GEnums.GMount_Unmount_Flags;
       Mount_Operation : System.Address;
       Cancellable     : System.Address;
       Callback        : System.Address;
       User_Data       : System.Address);
   pragma Import (C, C_G_Drive_Eject_With_Operation, "g_drive_eject_with_operation");
   --  Ejects a drive. This is an asynchronous operation, and is finished by
   --  calling Glib.GDrives.Eject_With_Operation_Finish with the Drive and
   --  Glib.G_Async_Result data returned in the Callback.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the unmount if required for eject
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.
   --  @param User_Data user data passed to Callback.

   procedure C_G_Drive_Start
      (Self            : Gdrive;
       Flags           : Glib.GEnums.GDrive_Start_Flags;
       Mount_Operation : System.Address;
       Cancellable     : System.Address;
       Callback        : System.Address;
       User_Data       : System.Address);
   pragma Import (C, C_G_Drive_Start, "g_drive_start");
   --  Asynchronously starts a drive.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GDrives.Start_Finish to obtain the result of the operation.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the start operation.
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.
   --  @param User_Data user data to pass to Callback

   procedure C_G_Drive_Stop
      (Self            : Gdrive;
       Flags           : Glib.GEnums.GMount_Unmount_Flags;
       Mount_Operation : System.Address;
       Cancellable     : System.Address;
       Callback        : System.Address;
       User_Data       : System.Address);
   pragma Import (C, C_G_Drive_Stop, "g_drive_stop");
   --  Asynchronously stops a drive.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GDrives.Stop_Finish to obtain the result of the operation.
   --  Since: gtk+ 2.22
   --  @param Flags flags affecting the unmount if required for stopping.
   --  @param Mount_Operation a Glib.GMountOperations.Gmount_Operation or null
   --  to avoid user interaction.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback, or null.
   --  @param User_Data user data to pass to Callback

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

   function Can_Eject (Self : Gdrive) return Boolean is
      function Internal (Self : Gdrive) return Glib.Gboolean;
      pragma Import (C, Internal, "g_drive_can_eject");
   begin
      return Internal (Self) /= 0;
   end Can_Eject;

   ------------------------
   -- Can_Poll_For_Media --
   ------------------------

   function Can_Poll_For_Media (Self : Gdrive) return Boolean is
      function Internal (Self : Gdrive) return Glib.Gboolean;
      pragma Import (C, Internal, "g_drive_can_poll_for_media");
   begin
      return Internal (Self) /= 0;
   end Can_Poll_For_Media;

   ---------------
   -- Can_Start --
   ---------------

   function Can_Start (Self : Gdrive) return Boolean is
      function Internal (Self : Gdrive) return Glib.Gboolean;
      pragma Import (C, Internal, "g_drive_can_start");
   begin
      return Internal (Self) /= 0;
   end Can_Start;

   ------------------------
   -- Can_Start_Degraded --
   ------------------------

   function Can_Start_Degraded (Self : Gdrive) return Boolean is
      function Internal (Self : Gdrive) return Glib.Gboolean;
      pragma Import (C, Internal, "g_drive_can_start_degraded");
   begin
      return Internal (Self) /= 0;
   end Can_Start_Degraded;

   --------------
   -- Can_Stop --
   --------------

   function Can_Stop (Self : Gdrive) return Boolean is
      function Internal (Self : Gdrive) return Glib.Gboolean;
      pragma Import (C, Internal, "g_drive_can_stop");
   begin
      return Internal (Self) /= 0;
   end Can_Stop;

   --------------------------
   -- Eject_With_Operation --
   --------------------------

   procedure Eject_With_Operation
      (Self            : Gdrive;
       Flags           : Glib.GEnums.GMount_Unmount_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Drive_Eject_With_Operation (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Drive_Eject_With_Operation (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Eject_With_Operation;

   ---------------------------------
   -- Eject_With_Operation_Finish --
   ---------------------------------

   function Eject_With_Operation_Finish
      (Self   : Gdrive;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : Gdrive;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_drive_eject_with_operation_finish");
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
      (Self : Gdrive) return GNAT.Strings.String_List
   is
      function Internal (Self : Gdrive) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_drive_enumerate_identifiers");
   begin
      return To_String_List_And_Free (Internal (Self));
   end Enumerate_Identifiers;

   --------------------
   -- Get_Identifier --
   --------------------

   function Get_Identifier
      (Self : Gdrive;
       Kind : UTF8_String) return UTF8_String
   is
      function Internal
         (Self : Gdrive;
          Kind : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_drive_get_identifier");
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

   function Get_Name (Self : Gdrive) return UTF8_String is
      function Internal (Self : Gdrive) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_drive_get_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Name;

   ------------------
   -- Get_Sort_Key --
   ------------------

   function Get_Sort_Key (Self : Gdrive) return UTF8_String is
      function Internal (Self : Gdrive) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_drive_get_sort_key");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Sort_Key;

   -----------------
   -- Get_Volumes --
   -----------------

   function Get_Volumes (Self : Gdrive) return Glib.GVolumes.GVolume_GList is
      function Internal (Self : Gdrive) return System.Address;
      pragma Import (C, Internal, "g_drive_get_volumes");
      Res : Glib.GVolumes.GVolume_GList;
   begin
      Glib.GVolumes.GVolume_List.Set_Object (Res, Internal(Self));
      return Res;
   end Get_Volumes;

   ---------------
   -- Has_Media --
   ---------------

   function Has_Media (Self : Gdrive) return Boolean is
      function Internal (Self : Gdrive) return Glib.Gboolean;
      pragma Import (C, Internal, "g_drive_has_media");
   begin
      return Internal (Self) /= 0;
   end Has_Media;

   -----------------
   -- Has_Volumes --
   -----------------

   function Has_Volumes (Self : Gdrive) return Boolean is
      function Internal (Self : Gdrive) return Glib.Gboolean;
      pragma Import (C, Internal, "g_drive_has_volumes");
   begin
      return Internal (Self) /= 0;
   end Has_Volumes;

   ------------------------------
   -- Is_Media_Check_Automatic --
   ------------------------------

   function Is_Media_Check_Automatic (Self : Gdrive) return Boolean is
      function Internal (Self : Gdrive) return Glib.Gboolean;
      pragma Import (C, Internal, "g_drive_is_media_check_automatic");
   begin
      return Internal (Self) /= 0;
   end Is_Media_Check_Automatic;

   ------------------------
   -- Is_Media_Removable --
   ------------------------

   function Is_Media_Removable (Self : Gdrive) return Boolean is
      function Internal (Self : Gdrive) return Glib.Gboolean;
      pragma Import (C, Internal, "g_drive_is_media_removable");
   begin
      return Internal (Self) /= 0;
   end Is_Media_Removable;

   ------------------
   -- Is_Removable --
   ------------------

   function Is_Removable (Self : Gdrive) return Boolean is
      function Internal (Self : Gdrive) return Glib.Gboolean;
      pragma Import (C, Internal, "g_drive_is_removable");
   begin
      return Internal (Self) /= 0;
   end Is_Removable;

   ---------------------------
   -- Poll_For_Media_Finish --
   ---------------------------

   function Poll_For_Media_Finish
      (Self   : Gdrive;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : Gdrive;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_drive_poll_for_media_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Poll_For_Media_Finish;

   -----------
   -- Start --
   -----------

   procedure Start
      (Self            : Gdrive;
       Flags           : Glib.GEnums.GDrive_Start_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Drive_Start (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Drive_Start (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Start;

   ------------------
   -- Start_Finish --
   ------------------

   function Start_Finish
      (Self   : Gdrive;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : Gdrive;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_drive_start_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Start_Finish;

   ----------
   -- Stop --
   ----------

   procedure Stop
      (Self            : Gdrive;
       Flags           : Glib.GEnums.GMount_Unmount_Flags;
       Mount_Operation : access Glib.GMountOperations.Gmount_Operation_Record'Class;
       Cancellable     : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback        : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Drive_Stop (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Drive_Stop (Self, Flags, Get_Object_Or_Null (GObject (Mount_Operation)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Stop;

   -----------------
   -- Stop_Finish --
   -----------------

   function Stop_Finish
      (Self   : Gdrive;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : Gdrive;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_drive_stop_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Stop_Finish;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdrive_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdrive_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : Gdrive;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdrive_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : Gdrive;
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

   procedure Marsh_Gdrive_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdrive_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gdrive;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdrive_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdrive_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gdrive;
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
   -- Marsh_Gdrive_Void --
   -----------------------

   procedure Marsh_Gdrive_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdrive_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdrive := Gdrive (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gdrive_Void;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : Gdrive;
       Call  : Cb_Gdrive_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : Gdrive;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

   ---------------------
   -- On_Disconnected --
   ---------------------

   procedure On_Disconnected
      (Self  : Gdrive;
       Call  : Cb_Gdrive_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "disconnected" & ASCII.NUL, Call, After);
   end On_Disconnected;

   ---------------------
   -- On_Disconnected --
   ---------------------

   procedure On_Disconnected
      (Self  : Gdrive;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "disconnected" & ASCII.NUL, Call, After, Slot);
   end On_Disconnected;

   ---------------------
   -- On_Eject_Button --
   ---------------------

   procedure On_Eject_Button
      (Self  : Gdrive;
       Call  : Cb_Gdrive_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "eject-button" & ASCII.NUL, Call, After);
   end On_Eject_Button;

   ---------------------
   -- On_Eject_Button --
   ---------------------

   procedure On_Eject_Button
      (Self  : Gdrive;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "eject-button" & ASCII.NUL, Call, After, Slot);
   end On_Eject_Button;

   --------------------
   -- On_Stop_Button --
   --------------------

   procedure On_Stop_Button
      (Self  : Gdrive;
       Call  : Cb_Gdrive_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "stop-button" & ASCII.NUL, Call, After);
   end On_Stop_Button;

   --------------------
   -- On_Stop_Button --
   --------------------

   procedure On_Stop_Button
      (Self  : Gdrive;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "stop-button" & ASCII.NUL, Call, After, Slot);
   end On_Stop_Button;

   function "+" (W : Gdrive) return Gdrive is
   begin
      return W;
   end "+";

end Glib.GDrives;
