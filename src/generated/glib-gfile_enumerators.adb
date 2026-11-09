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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

use type Glib.Error.GError;

package body Glib.GFile_Enumerators is

   procedure C_G_File_Enumerator_Close_Async
      (Self        : System.Address;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Enumerator_Close_Async, "g_file_enumerator_close_async");
   --  Asynchronously closes the file enumerator.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned in
   --  Glib.GFile_Enumerators.Close_Finish.
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Enumerator_Next_Files_Async
      (Self        : System.Address;
       Num_Files   : Glib.Gint;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Enumerator_Next_Files_Async, "g_file_enumerator_next_files_async");
   --  Request information for a number of files from the enumerator
   --  asynchronously. When all I/O for the operation is finished the Callback
   --  will be called with the requested information.
   --  See the documentation of Glib.GFile_Enumerators.Gfile_Enumerator for
   --  information about the order of returned files.
   --  Once the end of the enumerator is reached, or if an error occurs, the
   --  Callback will be called with an empty list. In this case, the previous
   --  call to Glib.GFile_Enumerators.Next_Files_Async will typically have
   --  returned fewer than Num_Files items.
   --  If a request is cancelled the callback will be called with
   --  G_IO_ERROR_CANCELLED.
   --  This leads to the following pseudo-code usage:
   --
   --     g_autoptr(GFile) dir = get_directory ();
   --     g_autoptr(GFileEnumerator) enumerator = NULL;
   --     g_autolist(GFileInfo) files = NULL;
   --     g_autoptr(GError) local_error = NULL;
   --
   --     enumerator = yield g_file_enumerate_children_async (dir,
   --                                                         G_FILE_ATTRIBUTE_STANDARD_NAME ","
   --                                                         G_FILE_ATTRIBUTE_STANDARD_TYPE,
   --                                                         G_FILE_QUERY_INFO_NONE,
   --                                                         G_PRIORITY_DEFAULT,
   --                                                         cancellable,
   --                                                         …,
   --                                                         &local_error);
   --     if (enumerator == NULL)
   --       g_error ("Error enumerating: %s", local_error->message);
   --
   --     // Loop until no files are returned, either because the end of the enumerator
   --     // has been reached, or an error was returned.
   --     do
   --       {
   --         files = yield g_file_enumerator_next_files_async (enumerator,
   --                                                           5,  // number of files to request
   --                                                           G_PRIORITY_DEFAULT,
   --                                                           cancellable,
   --                                                           …,
   --                                                           &local_error);
   --
   --         // Process the returned files, but don't assume that exactly 5 were returned.
   --         for (GList *l = files; l != NULL; l = l->next)
   --           {
   --             GFileInfo *info = l->data;
   --             handle_file_info (info);
   --           }
   --       }
   --     while (files != NULL);
   --
   --     if (local_error != NULL &&
   --         !g_error_matches (local_error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
   --       g_error ("Error while enumerating: %s", local_error->message);
   --
   --  During an async request no other sync and async calls are allowed, and
   --  will result in G_IO_ERROR_PENDING errors.
   --  Any outstanding I/O request with higher priority (lower numerical
   --  value) will be executed before an outstanding request with lower
   --  priority. Default priority is G_PRIORITY_DEFAULT.
   --  @param Num_Files the number of file info objects to request
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

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

   package Type_Conversion_Gfile_Enumerator is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gfile_Enumerator_Record);
   pragma Unreferenced (Type_Conversion_Gfile_Enumerator);

   -----------
   -- Close --
   -----------

   function Close
      (Self        : not null access Gfile_Enumerator_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Error       : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self        : System.Address;
          Cancellable : System.Address;
          Acc_Error   : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_enumerator_close");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Cancellable)), Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Close;

   -----------------
   -- Close_Async --
   -----------------

   procedure Close_Async
      (Self        : not null access Gfile_Enumerator_Record;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_File_Enumerator_Close_Async (Get_Object (Self), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_File_Enumerator_Close_Async (Get_Object (Self), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Close_Async;

   ------------------
   -- Close_Finish --
   ------------------

   function Close_Finish
      (Self   : not null access Gfile_Enumerator_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_enumerator_close_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Close_Finish;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Self : not null access Gfile_Enumerator_Record;
       Info : not null access Glib.File_Info.Gfile_Info_Record'Class)
       return Glib.GFile.Gfile
   is
      function Internal
         (Self : System.Address;
          Info : System.Address) return Glib.GFile.Gfile;
      pragma Import (C, Internal, "g_file_enumerator_get_child");
   begin
      return Internal (Get_Object (Self), Get_Object (Info));
   end Get_Child;

   -------------------
   -- Get_Container --
   -------------------

   function Get_Container
      (Self : not null access Gfile_Enumerator_Record)
       return Glib.GFile.Gfile
   is
      function Internal (Self : System.Address) return Glib.GFile.Gfile;
      pragma Import (C, Internal, "g_file_enumerator_get_container");
   begin
      return Internal (Get_Object (Self));
   end Get_Container;

   -----------------
   -- Has_Pending --
   -----------------

   function Has_Pending
      (Self : not null access Gfile_Enumerator_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_enumerator_has_pending");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Has_Pending;

   ---------------
   -- Is_Closed --
   ---------------

   function Is_Closed
      (Self : not null access Gfile_Enumerator_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_enumerator_is_closed");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Closed;

   -------------
   -- Iterate --
   -------------

   function Iterate
      (Self        : not null access Gfile_Enumerator_Record;
       Out_Info    : access Glib.File_Info.Gfile_Info := null;
       Out_Child   : access Glib.GFile.Gfile := null;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Error       : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self        : System.Address;
          Out_Info    : access System.Address;
          Out_Child   : access Glib.GFile.Gfile;
          Cancellable : System.Address;
          Acc_Error   : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_enumerator_iterate");
      Acc_Error       : aliased Glib.Error.GError;
      Tmp_Out_Info    : aliased System.Address := System.Null_Address;
      Acc_Out_Info    : constant access System.Address := (if Out_Info /= null then Tmp_Out_Info'Access else null);
      Stub_Gfile_Info : Glib.File_Info.Gfile_Info_Record;
      Tmp_Return      : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Out_Info, Out_Child, Get_Object_Or_Null (GObject (Cancellable)), Acc_Error'Access);
      if Out_Info /= null then
         Out_Info.all := Glib.File_Info.Gfile_Info (Get_User_Data (Tmp_Out_Info, Stub_Gfile_Info));
      end if;
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Iterate;

   ---------------
   -- Next_File --
   ---------------

   function Next_File
      (Self        : not null access Gfile_Enumerator_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Error       : out Glib.Error.GError) return Glib.File_Info.Gfile_Info
   is
      function Internal
         (Self        : System.Address;
          Cancellable : System.Address;
          Acc_Error   : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "g_file_enumerator_next_file");
      Acc_Error       : aliased Glib.Error.GError;
      Return_Obj      : Glib.File_Info.Gfile_Info;
      Stub_Gfile_Info : Glib.File_Info.Gfile_Info_Record;
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Cancellable)), Acc_Error'Access);
      Error := Acc_Error;
      if Error = null then
         Return_Obj := Glib.File_Info.Gfile_Info (Get_User_Data (Tmp_Return, Stub_Gfile_Info));
      end if;
      return Return_Obj;
   end Next_File;

   ----------------------
   -- Next_Files_Async --
   ----------------------

   procedure Next_Files_Async
      (Self        : not null access Gfile_Enumerator_Record;
       Num_Files   : Glib.Gint;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_File_Enumerator_Next_Files_Async (Get_Object (Self), Num_Files, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_File_Enumerator_Next_Files_Async (Get_Object (Self), Num_Files, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Next_Files_Async;

   -----------------------
   -- Next_Files_Finish --
   -----------------------

   function Next_Files_Finish
      (Self   : not null access Gfile_Enumerator_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError)
       return Glib.File_Info.File_Info_List.Glist
   is
      function Internal
         (Self      : System.Address;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "g_file_enumerator_next_files_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.File_Info.File_Info_List.Glist;
   begin
      Glib.File_Info.File_Info_List.Set_Object (Tmp_Return, Internal (Get_Object (Self), Result, Acc_Error'Access));
      Error := Acc_Error;
      return Tmp_Return;
   end Next_Files_Finish;

   -----------------
   -- Set_Pending --
   -----------------

   procedure Set_Pending
      (Self    : not null access Gfile_Enumerator_Record;
       Pending : Boolean)
   is
      procedure Internal (Self : System.Address; Pending : Glib.Gboolean);
      pragma Import (C, Internal, "g_file_enumerator_set_pending");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Pending));
   end Set_Pending;

end Glib.GFile_Enumerators;
