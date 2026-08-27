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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;          use Gtkada.Bindings;
with Gtkada.Types;             use Gtkada.Types;
pragma Warnings(On);

package body Glib.GFile is

   procedure C_G_File_Append_To_Async
      (Self        : Gfile;
       Flags       : GFile_Create_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Append_To_Async, "g_file_append_to_async");
   --  Asynchronously opens File for appending.
   --  For more details, see Glib.GFile.Append_To which is the synchronous
   --  version of this call.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GFile.Append_To_Finish to get the result of the operation.
   --  @param Flags a set of Glib.GFile.GFile_Create_Flags
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Create_Async
      (Self        : Gfile;
       Flags       : GFile_Create_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Create_Async, "g_file_create_async");
   --  Asynchronously creates a new file and returns an output stream for
   --  writing to it. The file must not already exist.
   --  For more details, see Glib.GFile.Create which is the synchronous
   --  version of this call.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GFile.Create_Finish to get the result of the operation.
   --  @param Flags a set of Glib.GFile.GFile_Create_Flags
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Create_Readwrite_Async
      (Self        : Gfile;
       Flags       : GFile_Create_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Create_Readwrite_Async, "g_file_create_readwrite_async");
   --  Asynchronously creates a new file and returns a stream for reading and
   --  writing to it. The file must not already exist.
   --  For more details, see Glib.GFile.Create_Readwrite which is the
   --  synchronous version of this call.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GFile.Create_Readwrite_Finish to get the result of the
   --  operation.
   --  Since: gtk+ 2.22
   --  @param Flags a set of Glib.GFile.GFile_Create_Flags
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Delete_Async
      (Self        : Gfile;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Delete_Async, "g_file_delete_async");
   --  Asynchronously delete a file. If the File is a directory, it will only
   --  be deleted if it is empty. This has the same semantics as g_unlink.
   --  Since: gtk+ 2.34
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Load_Bytes_Async
      (Self        : Gfile;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Load_Bytes_Async, "g_file_load_bytes_async");
   --  Asynchronously loads the contents of File as Glib.Bytes.Gbytes.
   --  If File is a resource:// based URI, the resulting bytes will reference
   --  the embedded resource instead of a copy. Otherwise, this is equivalent
   --  to calling g_file_load_contents_async and g_bytes_new_take.
   --  Callback should call Glib.GFile.Load_Bytes_Finish to get the result of
   --  this asynchronous operation.
   --  See Glib.GFile.Load_Bytes for more information.
   --  Since: gtk+ 2.56
   --  @param Cancellable a Glib.Cancellable.Gcancellable or null
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Make_Directory_Async
      (Self        : Gfile;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Make_Directory_Async, "g_file_make_directory_async");
   --  Asynchronously creates a directory.
   --  Since: gtk+ 2.38
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Open_Readwrite_Async
      (Self        : Gfile;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Open_Readwrite_Async, "g_file_open_readwrite_async");
   --  Asynchronously opens File for reading and writing.
   --  For more details, see Glib.GFile.Open_Readwrite which is the
   --  synchronous version of this call.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GFile.Open_Readwrite_Finish to get the result of the
   --  operation.
   --  Since: gtk+ 2.22
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Query_Filesystem_Info_Async
      (Self        : Gfile;
       Attributes  : Gtkada.Types.Chars_Ptr;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Query_Filesystem_Info_Async, "g_file_query_filesystem_info_async");
   --  Asynchronously gets the requested information about the filesystem that
   --  the specified File is on. The result is a Glib.File_Info.Gfile_Info
   --  object that contains key-value attributes (such as type or size for the
   --  file).
   --  For more details, see Glib.GFile.Query_Filesystem_Info which is the
   --  synchronous version of this call.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GFile.Query_Info_Finish to get the result of the operation.
   --  @param Attributes an attribute query string
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Query_Info_Async
      (Self        : Gfile;
       Attributes  : Gtkada.Types.Chars_Ptr;
       Flags       : GFile_Query_Info_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Query_Info_Async, "g_file_query_info_async");
   --  Asynchronously gets the requested information about specified File. The
   --  result is a Glib.File_Info.Gfile_Info object that contains key-value
   --  attributes (such as type or size for the file).
   --  For more details, see Glib.GFile.Query_Info which is the synchronous
   --  version of this call.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GFile.Query_Info_Finish to get the result of the operation.
   --  @param Attributes an attribute query string
   --  @param Flags a set of Glib.GFile.GFile_Query_Info_Flags
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Read_Async
      (Self        : Gfile;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Read_Async, "g_file_read_async");
   --  Asynchronously opens File for reading.
   --  For more details, see Glib.GFile.Read which is the synchronous version
   --  of this call.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GFile.Read_Finish to get the result of the operation.
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Replace_Async
      (Self        : Gfile;
       Etag        : Gtkada.Types.Chars_Ptr;
       Make_Backup : Glib.Gboolean;
       Flags       : GFile_Create_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Replace_Async, "g_file_replace_async");
   --  Asynchronously overwrites the file, replacing the contents, possibly
   --  creating a backup copy of the file first.
   --  For more details, see Glib.GFile.Replace which is the synchronous
   --  version of this call.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GFile.Replace_Finish to get the result of the operation.
   --  @param Etag an [entity tag][gfile-etag] for the current
   --  Glib.GFile.Gfile, or null to ignore
   --  @param Make_Backup True if a backup should be created
   --  @param Flags a set of Glib.GFile.GFile_Create_Flags
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Replace_Contents_Async
      (Self        : Gfile;
       Contents    : System.Address;
       Length      : Gsize;
       Etag        : Gtkada.Types.Chars_Ptr;
       Make_Backup : Glib.Gboolean;
       Flags       : GFile_Create_Flags;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Replace_Contents_Async, "g_file_replace_contents_async");
   --  Starts an asynchronous replacement of File with the given Contents of
   --  Length bytes. Etag will replace the document's current entity tag.
   --  When this operation has completed, Callback will be called with
   --  User_User data, and the operation can be finalized with
   --  Glib.GFile.Replace_Contents_Finish.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  If Make_Backup is True, this function will attempt to make a backup of
   --  File.
   --  Note that no copy of Contents will be made, so it must stay valid until
   --  Callback is called. See Glib.GFile.Replace_Contents_Bytes_Async for a
   --  Glib.Bytes.Gbytes version that will automatically hold a reference to
   --  the contents (without copying) for the duration of the call.
   --  @param Contents string of contents to replace the file with
   --  @param Etag a new [entity tag][gfile-etag] for the File, or null
   --  @param Make_Backup True if a backup should be created
   --  @param Flags a set of Glib.GFile.GFile_Create_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Replace_Contents_Bytes_Async
      (Self        : Gfile;
       Contents    : System.Address;
       Etag        : Gtkada.Types.Chars_Ptr;
       Make_Backup : Glib.Gboolean;
       Flags       : GFile_Create_Flags;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Replace_Contents_Bytes_Async, "g_file_replace_contents_bytes_async");
   --  Same as Glib.GFile.Replace_Contents_Async but takes a Glib.Bytes.Gbytes
   --  input instead. This function will keep a ref on Contents until the
   --  operation is done. Unlike Glib.GFile.Replace_Contents_Async this allows
   --  forgetting about the content without waiting for the callback.
   --  When this operation has completed, Callback will be called with
   --  User_User data, and the operation can be finalized with
   --  Glib.GFile.Replace_Contents_Finish.
   --  Since: gtk+ 2.40
   --  @param Contents a Glib.Bytes.Gbytes
   --  @param Etag a new [entity tag][gfile-etag] for the File, or null
   --  @param Make_Backup True if a backup should be created
   --  @param Flags a set of Glib.GFile.GFile_Create_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Replace_Readwrite_Async
      (Self        : Gfile;
       Etag        : Gtkada.Types.Chars_Ptr;
       Make_Backup : Glib.Gboolean;
       Flags       : GFile_Create_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Replace_Readwrite_Async, "g_file_replace_readwrite_async");
   --  Asynchronously overwrites the file in read-write mode, replacing the
   --  contents, possibly creating a backup copy of the file first.
   --  For more details, see Glib.GFile.Replace_Readwrite which is the
   --  synchronous version of this call.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GFile.Replace_Readwrite_Finish to get the result of the
   --  operation.
   --  Since: gtk+ 2.22
   --  @param Etag an [entity tag][gfile-etag] for the current
   --  Glib.GFile.Gfile, or null to ignore
   --  @param Make_Backup True if a backup should be created
   --  @param Flags a set of Glib.GFile.GFile_Create_Flags
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Set_Attributes_Async
      (Self        : Gfile;
       Info        : System.Address;
       Flags       : GFile_Query_Info_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Set_Attributes_Async, "g_file_set_attributes_async");
   --  Asynchronously sets the attributes of File with Info.
   --  For more details, see Glib.GFile.Set_Attributes_From_Info, which is the
   --  synchronous version of this call.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GFile.Set_Attributes_Finish to get the result of the
   --  operation.
   --  @param Info a Glib.File_Info.Gfile_Info
   --  @param Flags a Glib.GFile.GFile_Query_Info_Flags
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback
   --  @param User_Data a System.Address

   procedure C_G_File_Set_Display_Name_Async
      (Self         : Gfile;
       Display_Name : Gtkada.Types.Chars_Ptr;
       Io_Priority  : Glib.Gint;
       Cancellable  : System.Address;
       Callback     : System.Address;
       User_Data    : System.Address);
   pragma Import (C, C_G_File_Set_Display_Name_Async, "g_file_set_display_name_async");
   --  Asynchronously sets the display name for a given Glib.GFile.Gfile.
   --  For more details, see Glib.GFile.Set_Display_Name which is the
   --  synchronous version of this call.
   --  When the operation is finished, Callback will be called. You can then
   --  call Glib.GFile.Set_Display_Name_Finish to get the result of the
   --  operation.
   --  @param Display_Name a string
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_File_Trash_Async
      (Self        : Gfile;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Trash_Async, "g_file_trash_async");
   --  Asynchronously sends File to the Trash location, if possible.
   --  Since: gtk+ 2.38
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
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
       User_Data     : System.Address);
   pragma Convention (C, Internal_Gasync_Ready_Callback);
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.
   --  @param User_Data user data passed to the callback.

   ------------------------------------
   -- Internal_Gasync_Ready_Callback --
   ------------------------------------

   procedure Internal_Gasync_Ready_Callback
      (Source_Object : System.Address;
       Res           : Glib.G_Async_Result;
       User_Data     : System.Address)
   is
      Func         : constant Gasync_Ready_Callback := To_Gasync_Ready_Callback (User_Data);
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      Func (Get_User_Data (Source_Object, Stub_GObject), Res);
   end Internal_Gasync_Ready_Callback;

   ---------------
   -- Append_To --
   ---------------

   function Append_To
      (Self        : Gfile;
       Flags       : GFile_Create_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Output_Stream.Gfile_Output_Stream
   is
      function Internal
         (Self        : Gfile;
          Flags       : GFile_Create_Flags;
          Cancellable : System.Address) return System.Address;
      pragma Import (C, Internal, "g_file_append_to");
      Stub_Gfile_Output_Stream : Glib.File_Output_Stream.Gfile_Output_Stream_Record;
   begin
      return Glib.File_Output_Stream.Gfile_Output_Stream (Get_User_Data (Internal (Self, Flags, Get_Object_Or_Null (GObject (Cancellable))), Stub_Gfile_Output_Stream));
   end Append_To;

   ---------------------
   -- Append_To_Async --
   ---------------------

   procedure Append_To_Async
      (Self        : Gfile;
       Flags       : GFile_Create_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_File_Append_To_Async (Self, Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_File_Append_To_Async (Self, Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Append_To_Async;

   ----------------------
   -- Append_To_Finish --
   ----------------------

   function Append_To_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result)
       return Glib.File_Output_Stream.Gfile_Output_Stream
   is
      function Internal
         (Self : Gfile;
          Res  : Glib.G_Async_Result) return System.Address;
      pragma Import (C, Internal, "g_file_append_to_finish");
      Stub_Gfile_Output_Stream : Glib.File_Output_Stream.Gfile_Output_Stream_Record;
   begin
      return Glib.File_Output_Stream.Gfile_Output_Stream (Get_User_Data (Internal (Self, Res), Stub_Gfile_Output_Stream));
   end Append_To_Finish;

   ---------------------
   -- Copy_Attributes --
   ---------------------

   function Copy_Attributes
      (Self        : Gfile;
       Destination : Gfile;
       Flags       : GFile_Copy_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : Gfile;
          Destination : Gfile;
          Flags       : GFile_Copy_Flags;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_copy_attributes");
   begin
      return Internal (Self, Destination, Flags, Get_Object_Or_Null (GObject (Cancellable))) /= 0;
   end Copy_Attributes;

   ------------
   -- Create --
   ------------

   function Create
      (Self        : Gfile;
       Flags       : GFile_Create_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Output_Stream.Gfile_Output_Stream
   is
      function Internal
         (Self        : Gfile;
          Flags       : GFile_Create_Flags;
          Cancellable : System.Address) return System.Address;
      pragma Import (C, Internal, "g_file_create");
      Stub_Gfile_Output_Stream : Glib.File_Output_Stream.Gfile_Output_Stream_Record;
   begin
      return Glib.File_Output_Stream.Gfile_Output_Stream (Get_User_Data (Internal (Self, Flags, Get_Object_Or_Null (GObject (Cancellable))), Stub_Gfile_Output_Stream));
   end Create;

   ------------------
   -- Create_Async --
   ------------------

   procedure Create_Async
      (Self        : Gfile;
       Flags       : GFile_Create_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_File_Create_Async (Self, Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_File_Create_Async (Self, Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Create_Async;

   -------------------
   -- Create_Finish --
   -------------------

   function Create_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result)
       return Glib.File_Output_Stream.Gfile_Output_Stream
   is
      function Internal
         (Self : Gfile;
          Res  : Glib.G_Async_Result) return System.Address;
      pragma Import (C, Internal, "g_file_create_finish");
      Stub_Gfile_Output_Stream : Glib.File_Output_Stream.Gfile_Output_Stream_Record;
   begin
      return Glib.File_Output_Stream.Gfile_Output_Stream (Get_User_Data (Internal (Self, Res), Stub_Gfile_Output_Stream));
   end Create_Finish;

   ----------------------
   -- Create_Readwrite --
   ----------------------

   function Create_Readwrite
      (Self        : Gfile;
       Flags       : GFile_Create_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_IO_Stream.Gfile_Iostream
   is
      function Internal
         (Self        : Gfile;
          Flags       : GFile_Create_Flags;
          Cancellable : System.Address) return System.Address;
      pragma Import (C, Internal, "g_file_create_readwrite");
      Stub_Gfile_Iostream : Glib.File_IO_Stream.Gfile_Iostream_Record;
   begin
      return Glib.File_IO_Stream.Gfile_Iostream (Get_User_Data (Internal (Self, Flags, Get_Object_Or_Null (GObject (Cancellable))), Stub_Gfile_Iostream));
   end Create_Readwrite;

   ----------------------------
   -- Create_Readwrite_Async --
   ----------------------------

   procedure Create_Readwrite_Async
      (Self        : Gfile;
       Flags       : GFile_Create_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_File_Create_Readwrite_Async (Self, Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_File_Create_Readwrite_Async (Self, Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Create_Readwrite_Async;

   -----------------------------
   -- Create_Readwrite_Finish --
   -----------------------------

   function Create_Readwrite_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result) return Glib.File_IO_Stream.Gfile_Iostream
   is
      function Internal
         (Self : Gfile;
          Res  : Glib.G_Async_Result) return System.Address;
      pragma Import (C, Internal, "g_file_create_readwrite_finish");
      Stub_Gfile_Iostream : Glib.File_IO_Stream.Gfile_Iostream_Record;
   begin
      return Glib.File_IO_Stream.Gfile_Iostream (Get_User_Data (Internal (Self, Res), Stub_Gfile_Iostream));
   end Create_Readwrite_Finish;

   ------------
   -- Delete --
   ------------

   function Delete
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : Gfile;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_delete");
   begin
      return Internal (Self, Get_Object_Or_Null (GObject (Cancellable))) /= 0;
   end Delete;

   ------------------
   -- Delete_Async --
   ------------------

   procedure Delete_Async
      (Self        : Gfile;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_File_Delete_Async (Self, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_File_Delete_Async (Self, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Delete_Async;

   -------------------
   -- Delete_Finish --
   -------------------

   function Delete_Finish
      (Self   : Gfile;
       Result : Glib.G_Async_Result) return Boolean
   is
      function Internal
         (Self   : Gfile;
          Result : Glib.G_Async_Result) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_delete_finish");
   begin
      return Internal (Self, Result) /= 0;
   end Delete_Finish;

   -----------
   -- Equal --
   -----------

   function Equal (Self : Gfile; File2 : Gfile) return Boolean is
      function Internal (Self : Gfile; File2 : Gfile) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_equal");
   begin
      return Internal (Self, File2) /= 0;
   end Equal;

   ------------------
   -- Get_Basename --
   ------------------

   function Get_Basename (Self : Gfile) return UTF8_String is
      function Internal (Self : Gfile) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_get_basename");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Basename;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (Self : Gfile; Name : UTF8_String) return Gfile is
      function Internal
         (Self : Gfile;
          Name : Gtkada.Types.Chars_Ptr) return Gfile;
      pragma Import (C, Internal, "g_file_get_child");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : Gfile;
   begin
      Tmp_Return := Internal (Self, Tmp_Name);
      Free (Tmp_Name);
      return Tmp_Return;
   end Get_Child;

   --------------------------------
   -- Get_Child_For_Display_Name --
   --------------------------------

   function Get_Child_For_Display_Name
      (Self         : Gfile;
       Display_Name : UTF8_String) return Gfile
   is
      function Internal
         (Self         : Gfile;
          Display_Name : Gtkada.Types.Chars_Ptr) return Gfile;
      pragma Import (C, Internal, "g_file_get_child_for_display_name");
      Tmp_Display_Name : Gtkada.Types.Chars_Ptr := New_String (Display_Name);
      Tmp_Return       : Gfile;
   begin
      Tmp_Return := Internal (Self, Tmp_Display_Name);
      Free (Tmp_Display_Name);
      return Tmp_Return;
   end Get_Child_For_Display_Name;

   --------------------
   -- Get_Parse_Name --
   --------------------

   function Get_Parse_Name (Self : Gfile) return UTF8_String is
      function Internal (Self : Gfile) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_get_parse_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Parse_Name;

   --------------
   -- Get_Path --
   --------------

   function Get_Path (Self : Gfile) return UTF8_String is
      function Internal (Self : Gfile) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_get_path");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Path;

   -----------------------
   -- Get_Relative_Path --
   -----------------------

   function Get_Relative_Path
      (Self       : Gfile;
       Descendant : Gfile) return UTF8_String
   is
      function Internal
         (Self       : Gfile;
          Descendant : Gfile) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_get_relative_path");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self, Descendant));
   end Get_Relative_Path;

   -------------
   -- Get_Uri --
   -------------

   function Get_Uri (Self : Gfile) return UTF8_String is
      function Internal (Self : Gfile) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_get_uri");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Uri;

   --------------------
   -- Get_Uri_Scheme --
   --------------------

   function Get_Uri_Scheme (Self : Gfile) return UTF8_String is
      function Internal (Self : Gfile) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_get_uri_scheme");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Get_Uri_Scheme;

   ----------------
   -- Has_Parent --
   ----------------

   function Has_Parent (Self : Gfile; Parent : Gfile) return Boolean is
      function Internal (Self : Gfile; Parent : Gfile) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_has_parent");
   begin
      return Internal (Self, Parent) /= 0;
   end Has_Parent;

   ----------------
   -- Has_Prefix --
   ----------------

   function Has_Prefix (Self : Gfile; Prefix : Gfile) return Boolean is
      function Internal (Self : Gfile; Prefix : Gfile) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_has_prefix");
   begin
      return Internal (Self, Prefix) /= 0;
   end Has_Prefix;

   --------------------
   -- Has_Uri_Scheme --
   --------------------

   function Has_Uri_Scheme
      (Self       : Gfile;
       Uri_Scheme : UTF8_String) return Boolean
   is
      function Internal
         (Self       : Gfile;
          Uri_Scheme : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_has_uri_scheme");
      Tmp_Uri_Scheme : Gtkada.Types.Chars_Ptr := New_String (Uri_Scheme);
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Uri_Scheme);
      Free (Tmp_Uri_Scheme);
      return Tmp_Return /= 0;
   end Has_Uri_Scheme;

   ---------------
   -- Is_Native --
   ---------------

   function Is_Native (Self : Gfile) return Boolean is
      function Internal (Self : Gfile) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_is_native");
   begin
      return Internal (Self) /= 0;
   end Is_Native;

   ----------------
   -- Load_Bytes --
   ----------------

   function Load_Bytes
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Etag_Out    : access UTF8_String := null) return Glib.Bytes.Gbytes
   is
      function Internal
         (Self        : Gfile;
          Cancellable : System.Address;
          Etag_Out    : access Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_file_load_bytes");
      Tmp_Etag_Out : aliased Gtkada.Types.Chars_Ptr;
      Acc_Etag_Out : constant access Gtkada.Types.Chars_Ptr := (if Etag_Out /= null then Tmp_Etag_Out'Access else null);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Self, Get_Object_Or_Null (GObject (Cancellable)), Acc_Etag_Out);
      if Etag_Out /= null then
         Etag_Out.all := Gtkada.Bindings.Value_Allowing_Null (Tmp_Etag_Out);
      end if;
      return From_Object (Tmp_Return);
   end Load_Bytes;

   ----------------------
   -- Load_Bytes_Async --
   ----------------------

   procedure Load_Bytes_Async
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_File_Load_Bytes_Async (Self, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_File_Load_Bytes_Async (Self, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Load_Bytes_Async;

   -----------------------
   -- Load_Bytes_Finish --
   -----------------------

   function Load_Bytes_Finish
      (Self     : Gfile;
       Result   : Glib.G_Async_Result;
       Etag_Out : access UTF8_String := null) return Glib.Bytes.Gbytes
   is
      function Internal
         (Self     : Gfile;
          Result   : Glib.G_Async_Result;
          Etag_Out : access Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_file_load_bytes_finish");
      Tmp_Etag_Out : aliased Gtkada.Types.Chars_Ptr;
      Acc_Etag_Out : constant access Gtkada.Types.Chars_Ptr := (if Etag_Out /= null then Tmp_Etag_Out'Access else null);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Self, Result, Acc_Etag_Out);
      if Etag_Out /= null then
         Etag_Out.all := Gtkada.Bindings.Value_Allowing_Null (Tmp_Etag_Out);
      end if;
      return From_Object (Tmp_Return);
   end Load_Bytes_Finish;

   --------------------
   -- Make_Directory --
   --------------------

   function Make_Directory
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : Gfile;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_make_directory");
   begin
      return Internal (Self, Get_Object_Or_Null (GObject (Cancellable))) /= 0;
   end Make_Directory;

   --------------------------
   -- Make_Directory_Async --
   --------------------------

   procedure Make_Directory_Async
      (Self        : Gfile;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_File_Make_Directory_Async (Self, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_File_Make_Directory_Async (Self, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Make_Directory_Async;

   ---------------------------
   -- Make_Directory_Finish --
   ---------------------------

   function Make_Directory_Finish
      (Self   : Gfile;
       Result : Glib.G_Async_Result) return Boolean
   is
      function Internal
         (Self   : Gfile;
          Result : Glib.G_Async_Result) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_make_directory_finish");
   begin
      return Internal (Self, Result) /= 0;
   end Make_Directory_Finish;

   ---------------------------------
   -- Make_Directory_With_Parents --
   ---------------------------------

   function Make_Directory_With_Parents
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : Gfile;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_make_directory_with_parents");
   begin
      return Internal (Self, Get_Object_Or_Null (GObject (Cancellable))) /= 0;
   end Make_Directory_With_Parents;

   ------------------------
   -- Make_Symbolic_Link --
   ------------------------

   function Make_Symbolic_Link
      (Self          : Gfile;
       Symlink_Value : UTF8_String;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self          : Gfile;
          Symlink_Value : Gtkada.Types.Chars_Ptr;
          Cancellable   : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_make_symbolic_link");
      Tmp_Symlink_Value : Gtkada.Types.Chars_Ptr := New_String (Symlink_Value);
      Tmp_Return        : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Symlink_Value, Get_Object_Or_Null (GObject (Cancellable)));
      Free (Tmp_Symlink_Value);
      return Tmp_Return /= 0;
   end Make_Symbolic_Link;

   --------------------
   -- Open_Readwrite --
   --------------------

   function Open_Readwrite
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_IO_Stream.Gfile_Iostream
   is
      function Internal
         (Self        : Gfile;
          Cancellable : System.Address) return System.Address;
      pragma Import (C, Internal, "g_file_open_readwrite");
      Stub_Gfile_Iostream : Glib.File_IO_Stream.Gfile_Iostream_Record;
   begin
      return Glib.File_IO_Stream.Gfile_Iostream (Get_User_Data (Internal (Self, Get_Object_Or_Null (GObject (Cancellable))), Stub_Gfile_Iostream));
   end Open_Readwrite;

   --------------------------
   -- Open_Readwrite_Async --
   --------------------------

   procedure Open_Readwrite_Async
      (Self        : Gfile;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_File_Open_Readwrite_Async (Self, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_File_Open_Readwrite_Async (Self, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Open_Readwrite_Async;

   ---------------------------
   -- Open_Readwrite_Finish --
   ---------------------------

   function Open_Readwrite_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result) return Glib.File_IO_Stream.Gfile_Iostream
   is
      function Internal
         (Self : Gfile;
          Res  : Glib.G_Async_Result) return System.Address;
      pragma Import (C, Internal, "g_file_open_readwrite_finish");
      Stub_Gfile_Iostream : Glib.File_IO_Stream.Gfile_Iostream_Record;
   begin
      return Glib.File_IO_Stream.Gfile_Iostream (Get_User_Data (Internal (Self, Res), Stub_Gfile_Iostream));
   end Open_Readwrite_Finish;

   ---------------
   -- Peek_Path --
   ---------------

   function Peek_Path (Self : Gfile) return UTF8_String is
      function Internal (Self : Gfile) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_peek_path");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Peek_Path;

   ------------------
   -- Query_Exists --
   ------------------

   function Query_Exists
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : Gfile;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_query_exists");
   begin
      return Internal (Self, Get_Object_Or_Null (GObject (Cancellable))) /= 0;
   end Query_Exists;

   ---------------------
   -- Query_File_Type --
   ---------------------

   function Query_File_Type
      (Self        : Gfile;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Info.GFile_Type
   is
      function Internal
         (Self        : Gfile;
          Flags       : GFile_Query_Info_Flags;
          Cancellable : System.Address) return Glib.File_Info.GFile_Type;
      pragma Import (C, Internal, "g_file_query_file_type");
   begin
      return Internal (Self, Flags, Get_Object_Or_Null (GObject (Cancellable)));
   end Query_File_Type;

   ---------------------------
   -- Query_Filesystem_Info --
   ---------------------------

   function Query_Filesystem_Info
      (Self        : Gfile;
       Attributes  : UTF8_String;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Info.Gfile_Info
   is
      function Internal
         (Self        : Gfile;
          Attributes  : Gtkada.Types.Chars_Ptr;
          Cancellable : System.Address) return System.Address;
      pragma Import (C, Internal, "g_file_query_filesystem_info");
      Tmp_Attributes  : Gtkada.Types.Chars_Ptr := New_String (Attributes);
      Stub_Gfile_Info : Glib.File_Info.Gfile_Info_Record;
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Self, Tmp_Attributes, Get_Object_Or_Null (GObject (Cancellable)));
      Free (Tmp_Attributes);
      return Glib.File_Info.Gfile_Info (Get_User_Data (Tmp_Return, Stub_Gfile_Info));
   end Query_Filesystem_Info;

   ---------------------------------
   -- Query_Filesystem_Info_Async --
   ---------------------------------

   procedure Query_Filesystem_Info_Async
      (Self        : Gfile;
       Attributes  : UTF8_String;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
      Tmp_Attributes : Gtkada.Types.Chars_Ptr := New_String (Attributes);
   begin
      if Callback = null then
         C_G_File_Query_Filesystem_Info_Async (Self, Tmp_Attributes, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
         Free (Tmp_Attributes);
      else
         C_G_File_Query_Filesystem_Info_Async (Self, Tmp_Attributes, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
         Free (Tmp_Attributes);
      end if;
   end Query_Filesystem_Info_Async;

   ----------------------------------
   -- Query_Filesystem_Info_Finish --
   ----------------------------------

   function Query_Filesystem_Info_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result) return Glib.File_Info.Gfile_Info
   is
      function Internal
         (Self : Gfile;
          Res  : Glib.G_Async_Result) return System.Address;
      pragma Import (C, Internal, "g_file_query_filesystem_info_finish");
      Stub_Gfile_Info : Glib.File_Info.Gfile_Info_Record;
   begin
      return Glib.File_Info.Gfile_Info (Get_User_Data (Internal (Self, Res), Stub_Gfile_Info));
   end Query_Filesystem_Info_Finish;

   ----------------
   -- Query_Info --
   ----------------

   function Query_Info
      (Self        : Gfile;
       Attributes  : UTF8_String;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Info.Gfile_Info
   is
      function Internal
         (Self        : Gfile;
          Attributes  : Gtkada.Types.Chars_Ptr;
          Flags       : GFile_Query_Info_Flags;
          Cancellable : System.Address) return System.Address;
      pragma Import (C, Internal, "g_file_query_info");
      Tmp_Attributes  : Gtkada.Types.Chars_Ptr := New_String (Attributes);
      Stub_Gfile_Info : Glib.File_Info.Gfile_Info_Record;
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Self, Tmp_Attributes, Flags, Get_Object_Or_Null (GObject (Cancellable)));
      Free (Tmp_Attributes);
      return Glib.File_Info.Gfile_Info (Get_User_Data (Tmp_Return, Stub_Gfile_Info));
   end Query_Info;

   ----------------------
   -- Query_Info_Async --
   ----------------------

   procedure Query_Info_Async
      (Self        : Gfile;
       Attributes  : UTF8_String;
       Flags       : GFile_Query_Info_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
      Tmp_Attributes : Gtkada.Types.Chars_Ptr := New_String (Attributes);
   begin
      if Callback = null then
         C_G_File_Query_Info_Async (Self, Tmp_Attributes, Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
         Free (Tmp_Attributes);
      else
         C_G_File_Query_Info_Async (Self, Tmp_Attributes, Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
         Free (Tmp_Attributes);
      end if;
   end Query_Info_Async;

   -----------------------
   -- Query_Info_Finish --
   -----------------------

   function Query_Info_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result) return Glib.File_Info.Gfile_Info
   is
      function Internal
         (Self : Gfile;
          Res  : Glib.G_Async_Result) return System.Address;
      pragma Import (C, Internal, "g_file_query_info_finish");
      Stub_Gfile_Info : Glib.File_Info.Gfile_Info_Record;
   begin
      return Glib.File_Info.Gfile_Info (Get_User_Data (Internal (Self, Res), Stub_Gfile_Info));
   end Query_Info_Finish;

   ----------
   -- Read --
   ----------

   function Read
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Input_Stream.Gfile_Input_Stream
   is
      function Internal
         (Self        : Gfile;
          Cancellable : System.Address) return System.Address;
      pragma Import (C, Internal, "g_file_read");
      Stub_Gfile_Input_Stream : Glib.File_Input_Stream.Gfile_Input_Stream_Record;
   begin
      return Glib.File_Input_Stream.Gfile_Input_Stream (Get_User_Data (Internal (Self, Get_Object_Or_Null (GObject (Cancellable))), Stub_Gfile_Input_Stream));
   end Read;

   ----------------
   -- Read_Async --
   ----------------

   procedure Read_Async
      (Self        : Gfile;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_File_Read_Async (Self, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_File_Read_Async (Self, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Read_Async;

   -----------------
   -- Read_Finish --
   -----------------

   function Read_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result)
       return Glib.File_Input_Stream.Gfile_Input_Stream
   is
      function Internal
         (Self : Gfile;
          Res  : Glib.G_Async_Result) return System.Address;
      pragma Import (C, Internal, "g_file_read_finish");
      Stub_Gfile_Input_Stream : Glib.File_Input_Stream.Gfile_Input_Stream_Record;
   begin
      return Glib.File_Input_Stream.Gfile_Input_Stream (Get_User_Data (Internal (Self, Res), Stub_Gfile_Input_Stream));
   end Read_Finish;

   -------------
   -- Replace --
   -------------

   function Replace
      (Self        : Gfile;
       Etag        : UTF8_String := "";
       Make_Backup : Boolean;
       Flags       : GFile_Create_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Output_Stream.Gfile_Output_Stream
   is
      function Internal
         (Self        : Gfile;
          Etag        : Gtkada.Types.Chars_Ptr;
          Make_Backup : Glib.Gboolean;
          Flags       : GFile_Create_Flags;
          Cancellable : System.Address) return System.Address;
      pragma Import (C, Internal, "g_file_replace");
      Tmp_Etag                 : Gtkada.Types.Chars_Ptr;
      Stub_Gfile_Output_Stream : Glib.File_Output_Stream.Gfile_Output_Stream_Record;
      Tmp_Return               : System.Address;
   begin
      Tmp_Etag :=
        (if Etag = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Etag));
      Tmp_Return := Internal (Self, Tmp_Etag, Boolean'Pos (Make_Backup), Flags, Get_Object_Or_Null (GObject (Cancellable)));
      Free (Tmp_Etag);
      return Glib.File_Output_Stream.Gfile_Output_Stream (Get_User_Data (Tmp_Return, Stub_Gfile_Output_Stream));
   end Replace;

   -------------------
   -- Replace_Async --
   -------------------

   procedure Replace_Async
      (Self        : Gfile;
       Etag        : UTF8_String := "";
       Make_Backup : Boolean;
       Flags       : GFile_Create_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
      Tmp_Etag : Gtkada.Types.Chars_Ptr;
   begin
      if Callback = null then
         Tmp_Etag :=
           (if Etag = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Etag));
         C_G_File_Replace_Async (Self, Tmp_Etag, Boolean'Pos (Make_Backup), Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
         Free (Tmp_Etag);
      else
         Tmp_Etag :=
           (if Etag = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Etag));
         C_G_File_Replace_Async (Self, Tmp_Etag, Boolean'Pos (Make_Backup), Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
         Free (Tmp_Etag);
      end if;
   end Replace_Async;

   ----------------------
   -- Replace_Contents --
   ----------------------

   function Replace_Contents
      (Self        : Gfile;
       Contents    : Guint8_Array;
       Etag        : UTF8_String := "";
       Make_Backup : Boolean;
       Flags       : GFile_Create_Flags;
       New_Etag    : access UTF8_String := null;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : Gfile;
          Contents    : System.Address;
          Length      : Gsize;
          Etag        : Gtkada.Types.Chars_Ptr;
          Make_Backup : Glib.Gboolean;
          Flags       : GFile_Create_Flags;
          New_Etag    : access Gtkada.Types.Chars_Ptr;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_replace_contents");
      Tmp_Etag     : Gtkada.Types.Chars_Ptr;
      Tmp_New_Etag : aliased Gtkada.Types.Chars_Ptr;
      Acc_New_Etag : constant access Gtkada.Types.Chars_Ptr := (if New_Etag /= null then Tmp_New_Etag'Access else null);
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Etag :=
        (if Etag = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Etag));
      Tmp_Return := Internal (Self, Contents'Address, Contents'Length, Tmp_Etag, Boolean'Pos (Make_Backup), Flags, Acc_New_Etag, Get_Object_Or_Null (GObject (Cancellable)));
      if New_Etag /= null then
         New_Etag.all := Gtkada.Bindings.Value_Allowing_Null (Tmp_New_Etag);
      end if;
      Free (Tmp_Etag);
      return Tmp_Return /= 0;
   end Replace_Contents;

   ----------------------------
   -- Replace_Contents_Async --
   ----------------------------

   procedure Replace_Contents_Async
      (Self        : Gfile;
       Contents    : Guint8_Array;
       Etag        : UTF8_String := "";
       Make_Backup : Boolean;
       Flags       : GFile_Create_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
      Tmp_Etag : Gtkada.Types.Chars_Ptr;
   begin
      if Callback = null then
         Tmp_Etag :=
           (if Etag = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Etag));
         C_G_File_Replace_Contents_Async (Self, Contents'Address, Contents'Length, Tmp_Etag, Boolean'Pos (Make_Backup), Flags, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
         Free (Tmp_Etag);
      else
         Tmp_Etag :=
           (if Etag = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Etag));
         C_G_File_Replace_Contents_Async (Self, Contents'Address, Contents'Length, Tmp_Etag, Boolean'Pos (Make_Backup), Flags, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
         Free (Tmp_Etag);
      end if;
   end Replace_Contents_Async;

   ----------------------------------
   -- Replace_Contents_Bytes_Async --
   ----------------------------------

   procedure Replace_Contents_Bytes_Async
      (Self        : Gfile;
       Contents    : Glib.Bytes.Gbytes;
       Etag        : UTF8_String := "";
       Make_Backup : Boolean;
       Flags       : GFile_Create_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
      Tmp_Etag : Gtkada.Types.Chars_Ptr;
   begin
      if Callback = null then
         Tmp_Etag :=
           (if Etag = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Etag));
         C_G_File_Replace_Contents_Bytes_Async (Self, Get_Object (Contents), Tmp_Etag, Boolean'Pos (Make_Backup), Flags, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
         Free (Tmp_Etag);
      else
         Tmp_Etag :=
           (if Etag = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Etag));
         C_G_File_Replace_Contents_Bytes_Async (Self, Get_Object (Contents), Tmp_Etag, Boolean'Pos (Make_Backup), Flags, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
         Free (Tmp_Etag);
      end if;
   end Replace_Contents_Bytes_Async;

   -----------------------------
   -- Replace_Contents_Finish --
   -----------------------------

   function Replace_Contents_Finish
      (Self     : Gfile;
       Res      : Glib.G_Async_Result;
       New_Etag : access UTF8_String := null) return Boolean
   is
      function Internal
         (Self     : Gfile;
          Res      : Glib.G_Async_Result;
          New_Etag : access Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_replace_contents_finish");
      Tmp_New_Etag : aliased Gtkada.Types.Chars_Ptr;
      Acc_New_Etag : constant access Gtkada.Types.Chars_Ptr := (if New_Etag /= null then Tmp_New_Etag'Access else null);
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Res, Acc_New_Etag);
      if New_Etag /= null then
         New_Etag.all := Gtkada.Bindings.Value_Allowing_Null (Tmp_New_Etag);
      end if;
      return Tmp_Return /= 0;
   end Replace_Contents_Finish;

   --------------------
   -- Replace_Finish --
   --------------------

   function Replace_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result)
       return Glib.File_Output_Stream.Gfile_Output_Stream
   is
      function Internal
         (Self : Gfile;
          Res  : Glib.G_Async_Result) return System.Address;
      pragma Import (C, Internal, "g_file_replace_finish");
      Stub_Gfile_Output_Stream : Glib.File_Output_Stream.Gfile_Output_Stream_Record;
   begin
      return Glib.File_Output_Stream.Gfile_Output_Stream (Get_User_Data (Internal (Self, Res), Stub_Gfile_Output_Stream));
   end Replace_Finish;

   -----------------------
   -- Replace_Readwrite --
   -----------------------

   function Replace_Readwrite
      (Self        : Gfile;
       Etag        : UTF8_String := "";
       Make_Backup : Boolean;
       Flags       : GFile_Create_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_IO_Stream.Gfile_Iostream
   is
      function Internal
         (Self        : Gfile;
          Etag        : Gtkada.Types.Chars_Ptr;
          Make_Backup : Glib.Gboolean;
          Flags       : GFile_Create_Flags;
          Cancellable : System.Address) return System.Address;
      pragma Import (C, Internal, "g_file_replace_readwrite");
      Tmp_Etag            : Gtkada.Types.Chars_Ptr;
      Stub_Gfile_Iostream : Glib.File_IO_Stream.Gfile_Iostream_Record;
      Tmp_Return          : System.Address;
   begin
      Tmp_Etag :=
        (if Etag = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Etag));
      Tmp_Return := Internal (Self, Tmp_Etag, Boolean'Pos (Make_Backup), Flags, Get_Object_Or_Null (GObject (Cancellable)));
      Free (Tmp_Etag);
      return Glib.File_IO_Stream.Gfile_Iostream (Get_User_Data (Tmp_Return, Stub_Gfile_Iostream));
   end Replace_Readwrite;

   -----------------------------
   -- Replace_Readwrite_Async --
   -----------------------------

   procedure Replace_Readwrite_Async
      (Self        : Gfile;
       Etag        : UTF8_String := "";
       Make_Backup : Boolean;
       Flags       : GFile_Create_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
      Tmp_Etag : Gtkada.Types.Chars_Ptr;
   begin
      if Callback = null then
         Tmp_Etag :=
           (if Etag = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Etag));
         C_G_File_Replace_Readwrite_Async (Self, Tmp_Etag, Boolean'Pos (Make_Backup), Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
         Free (Tmp_Etag);
      else
         Tmp_Etag :=
           (if Etag = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Etag));
         C_G_File_Replace_Readwrite_Async (Self, Tmp_Etag, Boolean'Pos (Make_Backup), Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
         Free (Tmp_Etag);
      end if;
   end Replace_Readwrite_Async;

   ------------------------------
   -- Replace_Readwrite_Finish --
   ------------------------------

   function Replace_Readwrite_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result) return Glib.File_IO_Stream.Gfile_Iostream
   is
      function Internal
         (Self : Gfile;
          Res  : Glib.G_Async_Result) return System.Address;
      pragma Import (C, Internal, "g_file_replace_readwrite_finish");
      Stub_Gfile_Iostream : Glib.File_IO_Stream.Gfile_Iostream_Record;
   begin
      return Glib.File_IO_Stream.Gfile_Iostream (Get_User_Data (Internal (Self, Res), Stub_Gfile_Iostream));
   end Replace_Readwrite_Finish;

   ---------------------------
   -- Resolve_Relative_Path --
   ---------------------------

   function Resolve_Relative_Path
      (Self          : Gfile;
       Relative_Path : UTF8_String) return Gfile
   is
      function Internal
         (Self          : Gfile;
          Relative_Path : Gtkada.Types.Chars_Ptr) return Gfile;
      pragma Import (C, Internal, "g_file_resolve_relative_path");
      Tmp_Relative_Path : Gtkada.Types.Chars_Ptr := New_String (Relative_Path);
      Tmp_Return        : Gfile;
   begin
      Tmp_Return := Internal (Self, Tmp_Relative_Path);
      Free (Tmp_Relative_Path);
      return Tmp_Return;
   end Resolve_Relative_Path;

   -------------------------------
   -- Set_Attribute_Byte_String --
   -------------------------------

   function Set_Attribute_Byte_String
      (Self        : Gfile;
       Attribute   : UTF8_String;
       Value       : UTF8_String;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : Gfile;
          Attribute   : Gtkada.Types.Chars_Ptr;
          Value       : Gtkada.Types.Chars_Ptr;
          Flags       : GFile_Query_Info_Flags;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_set_attribute_byte_string");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Value     : Gtkada.Types.Chars_Ptr := New_String (Value);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Attribute, Tmp_Value, Flags, Get_Object_Or_Null (GObject (Cancellable)));
      Free (Tmp_Value);
      Free (Tmp_Attribute);
      return Tmp_Return /= 0;
   end Set_Attribute_Byte_String;

   -------------------------
   -- Set_Attribute_Int32 --
   -------------------------

   function Set_Attribute_Int32
      (Self        : Gfile;
       Attribute   : UTF8_String;
       Value       : Gint32;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : Gfile;
          Attribute   : Gtkada.Types.Chars_Ptr;
          Value       : Gint32;
          Flags       : GFile_Query_Info_Flags;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_set_attribute_int32");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Attribute, Value, Flags, Get_Object_Or_Null (GObject (Cancellable)));
      Free (Tmp_Attribute);
      return Tmp_Return /= 0;
   end Set_Attribute_Int32;

   -------------------------
   -- Set_Attribute_Int64 --
   -------------------------

   function Set_Attribute_Int64
      (Self        : Gfile;
       Attribute   : UTF8_String;
       Value       : Gint64;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : Gfile;
          Attribute   : Gtkada.Types.Chars_Ptr;
          Value       : Gint64;
          Flags       : GFile_Query_Info_Flags;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_set_attribute_int64");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Attribute, Value, Flags, Get_Object_Or_Null (GObject (Cancellable)));
      Free (Tmp_Attribute);
      return Tmp_Return /= 0;
   end Set_Attribute_Int64;

   --------------------------
   -- Set_Attribute_String --
   --------------------------

   function Set_Attribute_String
      (Self        : Gfile;
       Attribute   : UTF8_String;
       Value       : UTF8_String;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : Gfile;
          Attribute   : Gtkada.Types.Chars_Ptr;
          Value       : Gtkada.Types.Chars_Ptr;
          Flags       : GFile_Query_Info_Flags;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_set_attribute_string");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Value     : Gtkada.Types.Chars_Ptr := New_String (Value);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Attribute, Tmp_Value, Flags, Get_Object_Or_Null (GObject (Cancellable)));
      Free (Tmp_Value);
      Free (Tmp_Attribute);
      return Tmp_Return /= 0;
   end Set_Attribute_String;

   --------------------------
   -- Set_Attribute_Uint32 --
   --------------------------

   function Set_Attribute_Uint32
      (Self        : Gfile;
       Attribute   : UTF8_String;
       Value       : Guint32;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : Gfile;
          Attribute   : Gtkada.Types.Chars_Ptr;
          Value       : Guint32;
          Flags       : GFile_Query_Info_Flags;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_set_attribute_uint32");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Attribute, Value, Flags, Get_Object_Or_Null (GObject (Cancellable)));
      Free (Tmp_Attribute);
      return Tmp_Return /= 0;
   end Set_Attribute_Uint32;

   --------------------------
   -- Set_Attribute_Uint64 --
   --------------------------

   function Set_Attribute_Uint64
      (Self        : Gfile;
       Attribute   : UTF8_String;
       Value       : Guint64;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : Gfile;
          Attribute   : Gtkada.Types.Chars_Ptr;
          Value       : Guint64;
          Flags       : GFile_Query_Info_Flags;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_set_attribute_uint64");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Attribute, Value, Flags, Get_Object_Or_Null (GObject (Cancellable)));
      Free (Tmp_Attribute);
      return Tmp_Return /= 0;
   end Set_Attribute_Uint64;

   --------------------------
   -- Set_Attributes_Async --
   --------------------------

   procedure Set_Attributes_Async
      (Self        : Gfile;
       Info        : not null access Glib.File_Info.Gfile_Info_Record'Class;
       Flags       : GFile_Query_Info_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_File_Set_Attributes_Async (Self, Get_Object (Info), Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_File_Set_Attributes_Async (Self, Get_Object (Info), Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Set_Attributes_Async;

   ---------------------------
   -- Set_Attributes_Finish --
   ---------------------------

   function Set_Attributes_Finish
      (Self   : Gfile;
       Result : Glib.G_Async_Result;
       Info   : out Glib.File_Info.Gfile_Info) return Boolean
   is
      function Internal
         (Self     : Gfile;
          Result   : Glib.G_Async_Result;
          Acc_Info : access System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_set_attributes_finish");
      Acc_Info        : aliased Glib.File_Info.Gfile_Info;
      Tmp_Acc_Info    : aliased System.Address;
      Stub_Gfile_Info : Glib.File_Info.Gfile_Info_Record;
      Tmp_Return      : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Result, Tmp_Acc_Info'Access);
      Acc_Info := Glib.File_Info.Gfile_Info (Get_User_Data (Tmp_Acc_Info, Stub_Gfile_Info));
      Info := Acc_Info;
      return Tmp_Return /= 0;
   end Set_Attributes_Finish;

   ------------------------------
   -- Set_Attributes_From_Info --
   ------------------------------

   function Set_Attributes_From_Info
      (Self        : Gfile;
       Info        : not null access Glib.File_Info.Gfile_Info_Record'Class;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : Gfile;
          Info        : System.Address;
          Flags       : GFile_Query_Info_Flags;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_set_attributes_from_info");
   begin
      return Internal (Self, Get_Object (Info), Flags, Get_Object_Or_Null (GObject (Cancellable))) /= 0;
   end Set_Attributes_From_Info;

   ----------------------
   -- Set_Display_Name --
   ----------------------

   function Set_Display_Name
      (Self         : Gfile;
       Display_Name : UTF8_String;
       Cancellable  : access Glib.Cancellable.Gcancellable_Record'Class)
       return Gfile
   is
      function Internal
         (Self         : Gfile;
          Display_Name : Gtkada.Types.Chars_Ptr;
          Cancellable  : System.Address) return Gfile;
      pragma Import (C, Internal, "g_file_set_display_name");
      Tmp_Display_Name : Gtkada.Types.Chars_Ptr := New_String (Display_Name);
      Tmp_Return       : Gfile;
   begin
      Tmp_Return := Internal (Self, Tmp_Display_Name, Get_Object_Or_Null (GObject (Cancellable)));
      Free (Tmp_Display_Name);
      return Tmp_Return;
   end Set_Display_Name;

   ----------------------------
   -- Set_Display_Name_Async --
   ----------------------------

   procedure Set_Display_Name_Async
      (Self         : Gfile;
       Display_Name : UTF8_String;
       Io_Priority  : Glib.Gint;
       Cancellable  : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback     : Gasync_Ready_Callback)
   is
      Tmp_Display_Name : Gtkada.Types.Chars_Ptr := New_String (Display_Name);
   begin
      if Callback = null then
         C_G_File_Set_Display_Name_Async (Self, Tmp_Display_Name, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
         Free (Tmp_Display_Name);
      else
         C_G_File_Set_Display_Name_Async (Self, Tmp_Display_Name, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
         Free (Tmp_Display_Name);
      end if;
   end Set_Display_Name_Async;

   ------------------------------
   -- Supports_Thread_Contexts --
   ------------------------------

   function Supports_Thread_Contexts (Self : Gfile) return Boolean is
      function Internal (Self : Gfile) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_supports_thread_contexts");
   begin
      return Internal (Self) /= 0;
   end Supports_Thread_Contexts;

   -----------
   -- Trash --
   -----------

   function Trash
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : Gfile;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_trash");
   begin
      return Internal (Self, Get_Object_Or_Null (GObject (Cancellable))) /= 0;
   end Trash;

   -----------------
   -- Trash_Async --
   -----------------

   procedure Trash_Async
      (Self        : Gfile;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_File_Trash_Async (Self, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_File_Trash_Async (Self, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Trash_Async;

   ------------------
   -- Trash_Finish --
   ------------------

   function Trash_Finish
      (Self   : Gfile;
       Result : Glib.G_Async_Result) return Boolean
   is
      function Internal
         (Self   : Gfile;
          Result : Glib.G_Async_Result) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_trash_finish");
   begin
      return Internal (Self, Result) /= 0;
   end Trash_Finish;

   -----------------------------
   -- New_For_Commandline_Arg --
   -----------------------------

   function New_For_Commandline_Arg (Arg : UTF8_String) return Gfile is
      function Internal (Arg : Gtkada.Types.Chars_Ptr) return Gfile;
      pragma Import (C, Internal, "g_file_new_for_commandline_arg");
      Tmp_Arg    : Gtkada.Types.Chars_Ptr := New_String (Arg);
      Tmp_Return : Gfile;
   begin
      Tmp_Return := Internal (Tmp_Arg);
      Free (Tmp_Arg);
      return Tmp_Return;
   end New_For_Commandline_Arg;

   -------------------------------------
   -- New_For_Commandline_Arg_And_Cwd --
   -------------------------------------

   function New_For_Commandline_Arg_And_Cwd
      (Arg : UTF8_String;
       Cwd : UTF8_String) return Gfile
   is
      function Internal
         (Arg : Gtkada.Types.Chars_Ptr;
          Cwd : Gtkada.Types.Chars_Ptr) return Gfile;
      pragma Import (C, Internal, "g_file_new_for_commandline_arg_and_cwd");
      Tmp_Arg    : Gtkada.Types.Chars_Ptr := New_String (Arg);
      Tmp_Cwd    : Gtkada.Types.Chars_Ptr := New_String (Cwd);
      Tmp_Return : Gfile;
   begin
      Tmp_Return := Internal (Tmp_Arg, Tmp_Cwd);
      Free (Tmp_Cwd);
      Free (Tmp_Arg);
      return Tmp_Return;
   end New_For_Commandline_Arg_And_Cwd;

   ------------------
   -- New_For_Path --
   ------------------

   function New_For_Path (Path : UTF8_String) return Gfile is
      function Internal (Path : Gtkada.Types.Chars_Ptr) return Gfile;
      pragma Import (C, Internal, "g_file_new_for_path");
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return : Gfile;
   begin
      Tmp_Return := Internal (Tmp_Path);
      Free (Tmp_Path);
      return Tmp_Return;
   end New_For_Path;

   -----------------
   -- New_For_Uri --
   -----------------

   function New_For_Uri (URI : UTF8_String) return Gfile is
      function Internal (URI : Gtkada.Types.Chars_Ptr) return Gfile;
      pragma Import (C, Internal, "g_file_new_for_uri");
      Tmp_URI    : Gtkada.Types.Chars_Ptr := New_String (URI);
      Tmp_Return : Gfile;
   begin
      Tmp_Return := Internal (Tmp_URI);
      Free (Tmp_URI);
      return Tmp_Return;
   end New_For_Uri;

   ----------------
   -- Parse_Name --
   ----------------

   function Parse_Name (Parse_Name : UTF8_String) return Gfile is
      function Internal (Parse_Name : Gtkada.Types.Chars_Ptr) return Gfile;
      pragma Import (C, Internal, "g_file_parse_name");
      Tmp_Parse_Name : Gtkada.Types.Chars_Ptr := New_String (Parse_Name);
      Tmp_Return     : Gfile;
   begin
      Tmp_Return := Internal (Tmp_Parse_Name);
      Free (Tmp_Parse_Name);
      return Tmp_Return;
   end Parse_Name;

   function "+" (W : Gfile) return Gfile is
   begin
      return W;
   end "+";

end Glib.GFile;
