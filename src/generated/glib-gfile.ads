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

--  Glib.GFile.Gfile is a high level abstraction for manipulating files on a
--  virtual file system. GFiles are lightweight, immutable objects that do no
--  I/O upon creation. It is necessary to understand that Glib.GFile.Gfile
--  objects do not represent files, merely an identifier for a file. All file
--  content I/O is implemented as streaming operations (see
--  Glib.Input_Stream.Ginput_Stream and Glib.Output_Stream.Goutput_Stream).
--
--  To construct a Glib.GFile.Gfile, you can use: - Glib.GFile.New_For_Path if
--  you have a path. - Glib.GFile.New_For_Uri if you have a URI. -
--  Glib.GFile.New_For_Commandline_Arg for a command line argument. -
--  g_file_new_tmp to create a temporary file from a template. -
--  Glib.GFile.New_Tmp_Async to asynchronously create a temporary file. -
--  Glib.GFile.New_Tmp_Dir_Async to asynchronously create a temporary
--  directory. - Glib.GFile.Parse_Name from a UTF-8 string gotten from
--  Glib.GFile.Get_Parse_Name. - g_file_new_build_filename or
--  Glib.GFile.New_Build_Filenamev to create a file from path elements.
--
--  One way to think of a Glib.GFile.Gfile is as an abstraction of a pathname.
--  For normal files the system pathname is what is stored internally, but as
--  GFiles are extensible it could also be something else that corresponds to a
--  pathname in a userspace implementation of a filesystem.
--
--  GFiles make up hierarchies of directories and files that correspond to the
--  files on a filesystem. You can move through the file system with
--  Glib.GFile.Gfile using Glib.GFile.Get_Parent to get an identifier for the
--  parent directory, Glib.GFile.Get_Child to get a child within a directory,
--  Glib.GFile.Resolve_Relative_Path to resolve a relative path between two
--  GFiles. There can be multiple hierarchies, so you may not end up at the
--  same root if you repeatedly call Glib.GFile.Get_Parent on two different
--  files.
--
--  All GFiles have a basename (get with Glib.GFile.Get_Basename). These names
--  are byte strings that are used to identify the file on the filesystem
--  (relative to its parent directory) and there is no guarantees that they
--  have any particular charset encoding or even make any sense at all. If you
--  want to use filenames in a user interface you should use the display name
--  that you can get by requesting the G_FILE_ATTRIBUTE_STANDARD_DISPLAY_NAME
--  attribute with Glib.GFile.Query_Info. This is guaranteed to be in UTF-8 and
--  can be used in a user interface. But always store the real basename or the
--  Glib.GFile.Gfile to use to actually access the file, because there is no
--  way to go from a display name to the actual name.
--
--  Using Glib.GFile.Gfile as an identifier has the same weaknesses as using a
--  path in that there may be multiple aliases for the same file. For instance,
--  hard or soft links may cause two different GFiles to refer to the same
--  file. Other possible causes for aliases are: case insensitive filesystems,
--  short and long names on FAT/NTFS, or bind mounts in Linux. If you want to
--  check if two GFiles point to the same file you can query for the
--  G_FILE_ATTRIBUTE_ID_FILE attribute. Note that Glib.GFile.Gfile does some
--  trivial canonicalization of pathnames passed in, so that trivial
--  differences in the path string used at creation (duplicated slashes, slash
--  at end of path, "." or ".." path segments, etc) does not create different
--  GFiles.
--
--  Many Glib.GFile.Gfile operations have both synchronous and asynchronous
--  versions to suit your application. Asynchronous versions of synchronous
--  functions simply have _async appended to their function names. The
--  asynchronous I/O functions call a Gasync_Ready_Callback which is then used
--  to finalize the operation, producing a GAsyncResult which is then passed to
--  the function's matching _finish operation.
--
--  It is highly recommended to use asynchronous calls when running within a
--  shared main loop, such as in the main thread of an application. This avoids
--  I/O operations blocking other sources on the main loop from being
--  dispatched. Synchronous I/O operations should be performed from worker
--  threads. See the [introduction to asynchronous programming
--  section][async-programming] for more.
--
--  Some Glib.GFile.Gfile operations almost always take a noticeable amount of
--  time, and so do not have synchronous analogs. Notable cases include: -
--  g_file_mount_mountable to mount a mountable file. -
--  g_file_unmount_mountable_with_operation to unmount a mountable file. -
--  g_file_eject_mountable_with_operation to eject a mountable file.
--
--  ## Entity Tags # {gfile-etag}
--
--  One notable feature of GFiles are entity tags, or "etags" for short.
--  Entity tags are somewhat like a more abstract version of the traditional
--  mtime, and can be used to quickly determine if the file has been modified
--  from the version on the file system. See the HTTP 1.1
--  [specification](http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html) for
--  HTTP Etag headers, which are a very similar concept.
--
--  A Glib.GFile.Gfile is an interface value, not a tagged object, so it
--  cannot be unreferenced directly. All the constructors below transfer
--  ownership to the caller; release the file with
--
--  Glib.Object.Unref (Glib.Types.To_Object (File));

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Glib.Bytes;              use Glib.Bytes;
with Glib.Cancellable;        use Glib.Cancellable;
with Glib.File_IO_Stream;     use Glib.File_IO_Stream;
with Glib.File_Info;          use Glib.File_Info;
with Glib.File_Input_Stream;  use Glib.File_Input_Stream;
with Glib.File_Output_Stream; use Glib.File_Output_Stream;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Types;              use Glib.Types;

package Glib.GFile is

   type Gfile is new Glib.Types.GType_Interface;
   Null_Gfile : constant Gfile;

   type GFile_Query_Info_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, GFile_Query_Info_Flags);
   --  Flags used when querying a Glib.File_Info.Gfile_Info.

   G_File_Query_Info_None : constant GFile_Query_Info_Flags := 0;
   G_File_Query_Info_Nofollow_Symlinks : constant GFile_Query_Info_Flags := 1;

   type GFile_Create_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, GFile_Create_Flags);
   --  Flags used when an operation may create a file.

   G_File_Create_None : constant GFile_Create_Flags := 0;
   G_File_Create_Private : constant GFile_Create_Flags := 1;
   G_File_Create_Replace_Destination : constant GFile_Create_Flags := 2;

   type GFile_Copy_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, GFile_Copy_Flags);
   --  Flags used when copying or moving files.

   G_File_Copy_None : constant GFile_Copy_Flags := 0;
   G_File_Copy_Overwrite : constant GFile_Copy_Flags := 1;
   G_File_Copy_Backup : constant GFile_Copy_Flags := 2;
   G_File_Copy_Nofollow_Symlinks : constant GFile_Copy_Flags := 4;
   G_File_Copy_All_Metadata : constant GFile_Copy_Flags := 8;
   G_File_Copy_No_Fallback_For_Move : constant GFile_Copy_Flags := 16;
   G_File_Copy_Target_Default_Perms : constant GFile_Copy_Flags := 32;
   G_File_Copy_Target_Default_Modified_Time : constant GFile_Copy_Flags := 64;

   ---------------
   -- Callbacks --
   ---------------

   type Gasync_Ready_Callback is access procedure
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);
   --  Type definition for a function that will be called back when an
   --  asynchronous operation within GIO has been completed.
   --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
   --  invoked in a later iteration of the thread-default main context (see
   --  [methodGlib.MainContext.push_thread_default]) where the Gtask.Gtask was
   --  created. All other users of Gasync_Ready_Callback must likewise call it
   --  asynchronously in a later iteration of the main context.
   --  The asynchronous operation is guaranteed to have held a reference to
   --  Source_Object from the time when the `*_async` function was called,
   --  until after this callback returns.
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package GFile_Query_Info_Flags_Properties is
      new Generic_Internal_Discrete_Property (GFile_Query_Info_Flags);
   type Property_GFile_Query_Info_Flags is new GFile_Query_Info_Flags_Properties.Property;

   package GFile_Create_Flags_Properties is
      new Generic_Internal_Discrete_Property (GFile_Create_Flags);
   type Property_GFile_Create_Flags is new GFile_Create_Flags_Properties.Property;

   package GFile_Copy_Flags_Properties is
      new Generic_Internal_Discrete_Property (GFile_Copy_Flags);
   type Property_GFile_Copy_Flags is new GFile_Copy_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_file_get_type");

   -------------
   -- Methods --
   -------------

   function Append_To
      (Self        : Gfile;
       Flags       : GFile_Create_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Output_Stream.Gfile_Output_Stream;
   --  Gets an output stream for appending data to the file. If the file
   --  doesn't already exist it is created.
   --  By default files created are generally readable by everyone, but if you
   --  pass Glib.GFile.G_File_Create_Private in Flags the file will be made
   --  readable only to the current user, to the level that is supported on the
   --  target filesystem.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  Some file systems don't allow all file names, and may return an
   --  G_IO_ERROR_INVALID_FILENAME error. If the file is a directory the
   --  G_IO_ERROR_IS_DIRECTORY error will be returned. Other errors are
   --  possible too, and depend on what kind of filesystem the file is on.
   --  @param Flags a set of Glib.GFile.GFile_Create_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return a Glib.File_Output_Stream.Gfile_Output_Stream, or null on
   --  error. Free the returned object with g_object_unref.

   procedure Append_To_Async
      (Self        : Gfile;
       Flags       : GFile_Create_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Append_To_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result)
       return Glib.File_Output_Stream.Gfile_Output_Stream;
   --  Finishes an asynchronous file append operation started with
   --  Glib.GFile.Append_To_Async.
   --  @param Res Glib.G_Async_Result
   --  @return a valid Glib.File_Output_Stream.Gfile_Output_Stream or null on
   --  error. Free the returned object with g_object_unref.

   function Build_Attribute_List_For_Copy
      (Self        : Gfile;
       Flags       : GFile_Copy_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return UTF8_String;
   --  Prepares the file attribute query string for copying to File.
   --  This function prepares an attribute query string to be passed to
   --  Glib.GFile.Query_Info to get a list of attributes normally copied with
   --  the file (see Glib.GFile.Copy_Attributes for the detailed description).
   --  This function is used by the implementation of
   --  Glib.GFile.Copy_Attributes and is useful when one needs to query and set
   --  the attributes in two stages (e.g., for recursive move of a directory).
   --  Since: gtk+ 2.68
   --  @param Flags a set of Glib.GFile.GFile_Copy_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return an attribute query string for Glib.GFile.Query_Info, or null if
   --  an error occurs.

   procedure Copy_Async_With_Closures
      (Self                      : Gfile;
       Destination               : Gfile;
       Flags                     : GFile_Copy_Flags;
       Io_Priority               : Glib.Gint;
       Cancellable               : access Glib.Cancellable.Gcancellable_Record'Class;
       Progress_Callback_Closure : System.Address;
       Ready_Callback_Closure    : System.Address);

   function Copy_Attributes
      (Self        : Gfile;
       Destination : Gfile;
       Flags       : GFile_Copy_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Copies the file attributes from Source to Destination.
   --  Normally only a subset of the file attributes are copied, those that
   --  are copies in a normal file copy operation (which for instance does not
   --  include e.g. owner). However if Glib.GFile.G_File_Copy_All_Metadata is
   --  specified in Flags, then all the metadata that is possible to copy is
   --  copied. This is useful when implementing move by copy + delete source.
   --  @param Destination a Glib.GFile.Gfile to copy attributes to
   --  @param Flags a set of Glib.GFile.GFile_Copy_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return True if the attributes were copied successfully, False
   --  otherwise.

   function Create
      (Self        : Gfile;
       Flags       : GFile_Create_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Output_Stream.Gfile_Output_Stream;
   --  Creates a new file and returns an output stream for writing to it. The
   --  file must not already exist.
   --  By default files created are generally readable by everyone, but if you
   --  pass Glib.GFile.G_File_Create_Private in Flags the file will be made
   --  readable only to the current user, to the level that is supported on the
   --  target filesystem.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  If a file or directory with this name already exists the
   --  G_IO_ERROR_EXISTS error will be returned. Some file systems don't allow
   --  all file names, and may return an G_IO_ERROR_INVALID_FILENAME error, and
   --  if the name is to long G_IO_ERROR_FILENAME_TOO_LONG will be returned.
   --  Other errors are possible too, and depend on what kind of filesystem the
   --  file is on.
   --  @param Flags a set of Glib.GFile.GFile_Create_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return a Glib.File_Output_Stream.Gfile_Output_Stream for the newly
   --  created file, or null on error. Free the returned object with
   --  g_object_unref.

   procedure Create_Async
      (Self        : Gfile;
       Flags       : GFile_Create_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Create_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result)
       return Glib.File_Output_Stream.Gfile_Output_Stream;
   --  Finishes an asynchronous file create operation started with
   --  Glib.GFile.Create_Async.
   --  @param Res a Glib.G_Async_Result
   --  @return a Glib.File_Output_Stream.Gfile_Output_Stream or null on error.
   --  Free the returned object with g_object_unref.

   function Create_Readwrite
      (Self        : Gfile;
       Flags       : GFile_Create_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_IO_Stream.Gfile_Iostream;
   --  Creates a new file and returns a stream for reading and writing to it.
   --  The file must not already exist.
   --  By default files created are generally readable by everyone, but if you
   --  pass Glib.GFile.G_File_Create_Private in Flags the file will be made
   --  readable only to the current user, to the level that is supported on the
   --  target filesystem.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  If a file or directory with this name already exists, the
   --  G_IO_ERROR_EXISTS error will be returned. Some file systems don't allow
   --  all file names, and may return an G_IO_ERROR_INVALID_FILENAME error, and
   --  if the name is too long, G_IO_ERROR_FILENAME_TOO_LONG will be returned.
   --  Other errors are possible too, and depend on what kind of filesystem the
   --  file is on.
   --  Note that in many non-local file cases read and write streams are not
   --  supported, so make sure you really need to do read and write streaming,
   --  rather than just opening for reading or writing.
   --  Since: gtk+ 2.22
   --  @param Flags a set of Glib.GFile.GFile_Create_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return a Glib.File_IO_Stream.Gfile_Iostream for the newly created
   --  file, or null on error. Free the returned object with g_object_unref.

   procedure Create_Readwrite_Async
      (Self        : Gfile;
       Flags       : GFile_Create_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Create_Readwrite_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result) return Glib.File_IO_Stream.Gfile_Iostream;
   --  Finishes an asynchronous file create operation started with
   --  Glib.GFile.Create_Readwrite_Async.
   --  Since: gtk+ 2.22
   --  @param Res a Glib.G_Async_Result
   --  @return a Glib.File_IO_Stream.Gfile_Iostream or null on error. Free the
   --  returned object with g_object_unref.

   function Delete
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Deletes a file. If the File is a directory, it will only be deleted if
   --  it is empty. This has the same semantics as g_unlink.
   --  If File doesn't exist, G_IO_ERROR_NOT_FOUND will be returned. This
   --  allows for deletion to be implemented avoiding [time-of-check to
   --  time-of-use
   --  races](https://en.wikipedia.org/wiki/Time-of-check_to_time-of-use):
   --
   --     g_autoptr(GError) local_error = NULL;
   --     if (!g_file_delete (my_file, my_cancellable, &local_error) &&
   --         !g_error_matches (local_error, G_IO_ERROR, G_IO_ERROR_NOT_FOUND))
   --       {
   --         // deletion failed for some reason other than the file not existing:
   --         // so report the error
   --         g_warning ("Failed to delete %s: %s",
   --                    g_file_peek_path (my_file), local_error->message);
   --       }
   --
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return True if the file was deleted. False otherwise.

   procedure Delete_Async
      (Self        : Gfile;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Asynchronously delete a file. If the File is a directory, it will only
   --  be deleted if it is empty. This has the same semantics as g_unlink.
   --  Since: gtk+ 2.34
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied

   function Delete_Finish
      (Self   : Gfile;
       Result : Glib.G_Async_Result) return Boolean;
   --  Finishes deleting a file started with Glib.GFile.Delete_Async.
   --  Since: gtk+ 2.34
   --  @param Result a Glib.G_Async_Result
   --  @return True if the file was deleted. False otherwise.

   function Dup (Self : Gfile) return Gfile;
   pragma Import (C, Dup, "g_file_dup");
   --  Duplicates a Glib.GFile.Gfile handle. This operation does not duplicate
   --  the actual file or directory represented by the Glib.GFile.Gfile; see
   --  g_file_copy if attempting to copy a file.
   --  Glib.GFile.Dup is useful when a second handle is needed to the same
   --  underlying file, for use in a separate thread (Glib.GFile.Gfile is not
   --  thread-safe). For use within the same thread, use g_object_ref to
   --  increment the existing object's reference count.
   --  This call does no blocking I/O.
   --  @return a new Glib.GFile.Gfile that is a duplicate of the given
   --  Glib.GFile.Gfile.

   function Equal (Self : Gfile; File2 : Gfile) return Boolean;
   --  Checks if the two given GFiles refer to the same file.
   --  Note that two GFiles that differ can still refer to the same file on
   --  the filesystem due to various forms of filename aliasing.
   --  This call does no blocking I/O.
   --  @param File2 the second Glib.GFile.Gfile
   --  @return True if File1 and File2 are equal.

   function Get_Basename (Self : Gfile) return UTF8_String;
   --  Gets the base name (the last component of the path) for a given
   --  Glib.GFile.Gfile.
   --  If called for the top level of a system (such as the filesystem root or
   --  a uri like sftp://host/) it will return a single directory separator
   --  (and on Windows, possibly a drive letter).
   --  The base name is a byte string (not UTF-8). It has no defined encoding
   --  or rules other than it may not contain zero bytes. If you want to use
   --  filenames in a user interface you should use the display name that you
   --  can get by requesting the G_FILE_ATTRIBUTE_STANDARD_DISPLAY_NAME
   --  attribute with Glib.GFile.Query_Info.
   --  This call does no blocking I/O.
   --  @return string containing the Glib.GFile.Gfile's base name, or null if
   --  given Glib.GFile.Gfile is invalid. The returned string should be freed
   --  with g_free when no longer needed.

   function Get_Child (Self : Gfile; Name : UTF8_String) return Gfile;
   --  Gets a child of File with basename equal to Name.
   --  Note that the file with that specific name might not exist, but you can
   --  still have a Glib.GFile.Gfile that points to it. You can use this for
   --  instance to create that file.
   --  This call does no blocking I/O.
   --  @param Name string containing the child's basename
   --  @return a Glib.GFile.Gfile to a child specified by Name. Free the
   --  returned object with g_object_unref.

   function Get_Child_For_Display_Name
      (Self         : Gfile;
       Display_Name : UTF8_String) return Gfile;
   --  Gets the child of File for a given Display_Name (i.e. a UTF-8 version
   --  of the name). If this function fails, it returns null and Error will be
   --  set. This is very useful when constructing a Glib.GFile.Gfile for a new
   --  file and the user entered the filename in the user interface, for
   --  instance when you select a directory and type a filename in the file
   --  selector.
   --  This call does no blocking I/O.
   --  @param Display_Name string to a possible child
   --  @return a Glib.GFile.Gfile to the specified child, or null if the
   --  display name couldn't be converted. Free the returned object with
   --  g_object_unref.

   function Get_Parent (Self : Gfile) return Gfile;
   pragma Import (C, Get_Parent, "g_file_get_parent");
   --  Gets the parent directory for the File. If the File represents the root
   --  directory of the file system, then null will be returned.
   --  This call does no blocking I/O.
   --  @return a Glib.GFile.Gfile structure to the parent of the given
   --  Glib.GFile.Gfile or null if there is no parent. Free the returned object
   --  with g_object_unref.

   function Get_Parse_Name (Self : Gfile) return UTF8_String;
   --  Gets the parse name of the File. A parse name is a UTF-8 string that
   --  describes the file such that one can get the Glib.GFile.Gfile back using
   --  Glib.GFile.Parse_Name.
   --  This is generally used to show the Glib.GFile.Gfile as a nice
   --  full-pathname kind of string in a user interface, like in a location
   --  entry.
   --  For local files with names that can safely be converted to UTF-8 the
   --  pathname is used, otherwise the IRI is used (a form of URI that allows
   --  UTF-8 characters unescaped).
   --  This call does no blocking I/O.
   --  @return a string containing the Glib.GFile.Gfile's parse name. The
   --  returned string should be freed with g_free when no longer needed.

   function Get_Path (Self : Gfile) return UTF8_String;
   --  Gets the local pathname for Glib.GFile.Gfile, if one exists. If
   --  non-null, this is guaranteed to be an absolute, canonical path. It might
   --  contain symlinks.
   --  This call does no blocking I/O.
   --  @return string containing the Glib.GFile.Gfile's path, or null if no
   --  such path exists. The returned string should be freed with g_free when
   --  no longer needed.

   function Get_Relative_Path
      (Self       : Gfile;
       Descendant : Gfile) return UTF8_String;
   --  Gets the path for Descendant relative to Parent.
   --  This call does no blocking I/O.
   --  @param Descendant input Glib.GFile.Gfile
   --  @return string with the relative path from Descendant to Parent, or
   --  null if Descendant doesn't have Parent as prefix. The returned string
   --  should be freed with g_free when no longer needed.

   function Get_Uri (Self : Gfile) return UTF8_String;
   --  Gets the URI for the File.
   --  This call does no blocking I/O.
   --  @return a string containing the Glib.GFile.Gfile's URI. If the
   --  Glib.GFile.Gfile was constructed with an invalid URI, an invalid URI is
   --  returned. The returned string should be freed with g_free when no longer
   --  needed.

   function Get_Uri_Scheme (Self : Gfile) return UTF8_String;
   --  Gets the URI scheme for a Glib.GFile.Gfile. RFC 3986 decodes the scheme
   --  as:
   --
   --     URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
   --
   --  Common schemes include "file", "http", "ftp", etc.
   --  The scheme can be different from the one used to construct the
   --  Glib.GFile.Gfile, in that it might be replaced with one that is
   --  logically equivalent to the Glib.GFile.Gfile.
   --  This call does no blocking I/O.
   --  @return a string containing the URI scheme for the given
   --  Glib.GFile.Gfile or null if the Glib.GFile.Gfile was constructed with an
   --  invalid URI. The returned string should be freed with g_free when no
   --  longer needed.

   function Has_Parent (Self : Gfile; Parent : Gfile) return Boolean;
   --  Checks if File has a parent, and optionally, if it is Parent.
   --  If Parent is null then this function returns True if File has any
   --  parent at all. If Parent is non-null then True is only returned if File
   --  is an immediate child of Parent.
   --  Since: gtk+ 2.24
   --  @param Parent the parent to check for, or null
   --  @return True if File is an immediate child of Parent (or any parent in
   --  the case that Parent is null).

   function Has_Prefix (Self : Gfile; Prefix : Gfile) return Boolean;
   --  Checks whether File has the prefix specified by Prefix.
   --  In other words, if the names of initial elements of File's pathname
   --  match Prefix. Only full pathname elements are matched, so a path like
   --  /foo is not considered a prefix of /foobar, only of /foo/bar.
   --  A Glib.GFile.Gfile is not a prefix of itself. If you want to check for
   --  equality, use Glib.GFile.Equal.
   --  This call does no I/O, as it works purely on names. As such it can
   --  sometimes return False even if File is inside a Prefix (from a
   --  filesystem point of view), because the prefix of File is an alias of
   --  Prefix.
   --  @param Prefix input Glib.GFile.Gfile
   --  @return True if the File's parent, grandparent, etc is Prefix, False
   --  otherwise.

   function Has_Uri_Scheme
      (Self       : Gfile;
       Uri_Scheme : UTF8_String) return Boolean;
   --  Checks to see if a Glib.GFile.Gfile has a given URI scheme.
   --  This call does no blocking I/O.
   --  @param Uri_Scheme a string containing a URI scheme
   --  @return True if Glib.GFile.Gfile's backend supports the given URI
   --  scheme, False if URI scheme is null, not supported, or Glib.GFile.Gfile
   --  is invalid.

   function Is_Native (Self : Gfile) return Boolean;
   --  Checks to see if a file is native to the platform.
   --  A native file is one expressed in the platform-native filename format,
   --  e.g. "C:\Windows" or "/usr/bin/". This does not mean the file is local,
   --  as it might be on a locally mounted remote filesystem.
   --  On some systems non-native files may be available using the native
   --  filesystem via a userspace filesystem (FUSE), in these cases this call
   --  will return False, but Glib.GFile.Get_Path will still return a native
   --  path.
   --  This call does no blocking I/O.
   --  @return True if File is native

   function Load_Bytes
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Etag_Out    : access UTF8_String := null) return Glib.Bytes.Gbytes;
   --  Loads the contents of File and returns it as Glib.Bytes.Gbytes.
   --  If File is a resource:// based URI, the resulting bytes will reference
   --  the embedded resource instead of a copy. Otherwise, this is equivalent
   --  to calling g_file_load_contents and g_bytes_new_take.
   --  For resources, Etag_Out will be set to null.
   --  The data contained in the resulting Glib.Bytes.Gbytes is always
   --  zero-terminated, but this is not included in the Glib.Bytes.Gbytes
   --  length. The resulting Glib.Bytes.Gbytes should be freed with
   --  Glib.Bytes.Unref when no longer in use.
   --  Since: gtk+ 2.56
   --  @param Cancellable a Glib.Cancellable.Gcancellable or null
   --  @param Etag_Out a location to place the current entity tag for the
   --  file, or null if the entity tag is not needed
   --  @return a Glib.Bytes.Gbytes or null and Error is set

   procedure Load_Bytes_Async
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Load_Bytes_Finish
      (Self     : Gfile;
       Result   : Glib.G_Async_Result;
       Etag_Out : access UTF8_String := null) return Glib.Bytes.Gbytes;
   --  Completes an asynchronous request to Glib.GFile.Load_Bytes_Async.
   --  For resources, Etag_Out will be set to null.
   --  The data contained in the resulting Glib.Bytes.Gbytes is always
   --  zero-terminated, but this is not included in the Glib.Bytes.Gbytes
   --  length. The resulting Glib.Bytes.Gbytes should be freed with
   --  Glib.Bytes.Unref when no longer in use.
   --  See Glib.GFile.Load_Bytes for more information.
   --  Since: gtk+ 2.56
   --  @param Result a Glib.G_Async_Result provided to the callback
   --  @param Etag_Out a location to place the current entity tag for the
   --  file, or null if the entity tag is not needed
   --  @return a Glib.Bytes.Gbytes or null and Error is set

   function Make_Directory
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Creates a directory. Note that this will only create a child directory
   --  of the immediate parent directory of the path or URI given by the
   --  Glib.GFile.Gfile. To recursively create directories, see
   --  Glib.GFile.Make_Directory_With_Parents. This function will fail if the
   --  parent directory does not exist, setting Error to G_IO_ERROR_NOT_FOUND.
   --  If the file system doesn't support creating directories, this function
   --  will fail, setting Error to G_IO_ERROR_NOT_SUPPORTED.
   --  For a local Glib.GFile.Gfile the newly created directory will have the
   --  default (current) ownership and permissions of the current process.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return True on successful creation, False otherwise.

   procedure Make_Directory_Async
      (Self        : Gfile;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Asynchronously creates a directory.
   --  Since: gtk+ 2.38
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied

   function Make_Directory_Finish
      (Self   : Gfile;
       Result : Glib.G_Async_Result) return Boolean;
   --  Finishes an asynchronous directory creation, started with
   --  Glib.GFile.Make_Directory_Async.
   --  Since: gtk+ 2.38
   --  @param Result a Glib.G_Async_Result
   --  @return True on successful directory creation, False otherwise.

   function Make_Directory_With_Parents
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Creates a directory and any parent directories that may not exist
   --  similar to 'mkdir -p'. If the file system does not support creating
   --  directories, this function will fail, setting Error to
   --  G_IO_ERROR_NOT_SUPPORTED. If the directory itself already exists, this
   --  function will fail setting Error to G_IO_ERROR_EXISTS, unlike the
   --  similar g_mkdir_with_parents.
   --  For a local Glib.GFile.Gfile the newly created directories will have
   --  the default (current) ownership and permissions of the current process.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  Since: gtk+ 2.18
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return True if all directories have been successfully created, False
   --  otherwise.

   function Make_Symbolic_Link
      (Self          : Gfile;
       Symlink_Value : UTF8_String;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Creates a symbolic link named File which contains the string
   --  Symlink_Value.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  @param Symlink_Value a string with the path for the target of the new
   --  symlink
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return True on the creation of a new symlink, False otherwise.

   procedure Make_Symbolic_Link_Async
      (Self          : Gfile;
       Symlink_Value : UTF8_String;
       Io_Priority   : Glib.Gint;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback      : Gasync_Ready_Callback);
   --  Asynchronously creates a symbolic link named File which contains the
   --  string Symlink_Value.
   --  Since: gtk+ 2.74
   --  @param Symlink_Value a string with the path for the target of the new
   --  symlink
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied

   function Make_Symbolic_Link_Finish
      (Self   : Gfile;
       Result : Glib.G_Async_Result) return Boolean;
   --  Finishes an asynchronous symbolic link creation, started with
   --  Glib.GFile.Make_Symbolic_Link_Async.
   --  Since: gtk+ 2.74
   --  @param Result a Glib.G_Async_Result
   --  @return True on successful directory creation, False otherwise.

   procedure Move_Async_With_Closures
      (Self                      : Gfile;
       Destination               : Gfile;
       Flags                     : GFile_Copy_Flags;
       Io_Priority               : Glib.Gint;
       Cancellable               : access Glib.Cancellable.Gcancellable_Record'Class;
       Progress_Callback_Closure : System.Address;
       Ready_Callback_Closure    : System.Address);

   function Move_Finish
      (Self   : Gfile;
       Result : Glib.G_Async_Result) return Boolean;
   --  Finishes an asynchronous file movement, started with
   --  Glib.GFile.Move_Async.
   --  Since: gtk+ 2.72
   --  @param Result a Glib.G_Async_Result
   --  @return True on successful file move, False otherwise.

   function Open_Readwrite
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_IO_Stream.Gfile_Iostream;
   --  Opens an existing file for reading and writing. The result is a
   --  Glib.File_IO_Stream.Gfile_Iostream that can be used to read and write
   --  the contents of the file.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  If the file does not exist, the G_IO_ERROR_NOT_FOUND error will be
   --  returned. If the file is a directory, the G_IO_ERROR_IS_DIRECTORY error
   --  will be returned. Other errors are possible too, and depend on what kind
   --  of filesystem the file is on. Note that in many non-local file cases
   --  read and write streams are not supported, so make sure you really need
   --  to do read and write streaming, rather than just opening for reading or
   --  writing.
   --  Since: gtk+ 2.22
   --  @param Cancellable a Glib.Cancellable.Gcancellable
   --  @return Glib.File_IO_Stream.Gfile_Iostream or null on error. Free the
   --  returned object with g_object_unref.

   procedure Open_Readwrite_Async
      (Self        : Gfile;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Open_Readwrite_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result) return Glib.File_IO_Stream.Gfile_Iostream;
   --  Finishes an asynchronous file read operation started with
   --  Glib.GFile.Open_Readwrite_Async.
   --  Since: gtk+ 2.22
   --  @param Res a Glib.G_Async_Result
   --  @return a Glib.File_IO_Stream.Gfile_Iostream or null on error. Free the
   --  returned object with g_object_unref.

   function Peek_Path (Self : Gfile) return UTF8_String;
   --  Exactly like Glib.GFile.Get_Path, but caches the result via
   --  g_object_set_qdata_full. This is useful for example in C applications
   --  which mix `g_file_*` APIs with native ones. It also avoids an extra
   --  duplicated string when possible, so will be generally more efficient.
   --  This call does no blocking I/O.
   --  Since: gtk+ 2.56
   --  @return string containing the Glib.GFile.Gfile's path, or null if no
   --  such path exists. The returned string is owned by File.

   function Query_Exists
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Utility function to check if a particular file exists. This is
   --  implemented using Glib.GFile.Query_Info and as such does blocking I/O.
   --  Note that in many cases it is [racy to first check for file
   --  existence](https://en.wikipedia.org/wiki/Time_of_check_to_time_of_use)
   --  and then execute something based on the outcome of that, because the
   --  file might have been created or removed in between the operations. The
   --  general approach to handling that is to not check, but just do the
   --  operation and handle the errors as they come.
   --  As an example of race-free checking, take the case of reading a file,
   --  and if it doesn't exist, creating it. There are two racy versions: read
   --  it, and on error create it; and: check if it exists, if not create it.
   --  These can both result in two processes creating the file (with perhaps a
   --  partially written file as the result). The correct approach is to always
   --  try to create the file with Glib.GFile.Create which will either
   --  atomically create the file or fail with a G_IO_ERROR_EXISTS error.
   --  However, in many cases an existence check is useful in a user
   --  interface, for instance to make a menu item sensitive/insensitive, so
   --  that you don't have to fool users that something is possible and then
   --  just show an error dialog. If you do this, you should make sure to also
   --  handle the errors that can happen due to races when you execute the
   --  operation.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return True if the file exists (and can be detected without error),
   --  False otherwise (or if cancelled).

   function Query_File_Type
      (Self        : Gfile;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Info.GFile_Type;
   --  Utility function to inspect the Glib.File_Info.GFile_Type of a file.
   --  This is implemented using Glib.GFile.Query_Info and as such does
   --  blocking I/O.
   --  The primary use case of this method is to check if a file is a regular
   --  file, directory, or symlink.
   --  Since: gtk+ 2.18
   --  @param Flags a set of Glib.GFile.GFile_Query_Info_Flags passed to
   --  Glib.GFile.Query_Info
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return The Glib.File_Info.GFile_Type of the file and
   --  Glib.File_Info.G_File_Type_Unknown if the file does not exist

   function Query_Filesystem_Info
      (Self        : Gfile;
       Attributes  : UTF8_String;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Info.Gfile_Info;
   --  Similar to Glib.GFile.Query_Info, but obtains information about the
   --  filesystem the File is on, rather than the file itself. For instance the
   --  amount of space available and the type of the filesystem.
   --  The Attributes value is a string that specifies the attributes that
   --  should be gathered. It is not an error if it's not possible to read a
   --  particular requested attribute from a file - it just won't be set.
   --  Attributes should be a comma-separated list of attributes or attribute
   --  wildcards. The wildcard "*" means all attributes, and a wildcard like
   --  "filesystem::*" means all attributes in the filesystem namespace. The
   --  standard namespace for filesystem attributes is "filesystem". Common
   --  attributes of interest are G_FILE_ATTRIBUTE_FILESYSTEM_SIZE (the total
   --  size of the filesystem in bytes), G_FILE_ATTRIBUTE_FILESYSTEM_FREE
   --  (number of bytes available), and G_FILE_ATTRIBUTE_FILESYSTEM_TYPE (type
   --  of the filesystem).
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  If the file does not exist, the G_IO_ERROR_NOT_FOUND error will be
   --  returned. Other errors are possible too, and depend on what kind of
   --  filesystem the file is on.
   --  @param Attributes an attribute query string
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return a Glib.File_Info.Gfile_Info or null if there was an error. Free
   --  the returned object with g_object_unref.

   procedure Query_Filesystem_Info_Async
      (Self        : Gfile;
       Attributes  : UTF8_String;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Query_Filesystem_Info_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result) return Glib.File_Info.Gfile_Info;
   --  Finishes an asynchronous filesystem info query. See
   --  Glib.GFile.Query_Filesystem_Info_Async.
   --  @param Res a Glib.G_Async_Result
   --  @return Glib.File_Info.Gfile_Info for given File or null on error. Free
   --  the returned object with g_object_unref.

   function Query_Info
      (Self        : Gfile;
       Attributes  : UTF8_String;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Info.Gfile_Info;
   --  Gets the requested information about specified File. The result is a
   --  Glib.File_Info.Gfile_Info object that contains key-value attributes
   --  (such as the type or size of the file).
   --  The Attributes value is a string that specifies the file attributes
   --  that should be gathered. It is not an error if it's not possible to read
   --  a particular requested attribute from a file - it just won't be set.
   --  Attributes should be a comma-separated list of attributes or attribute
   --  wildcards. The wildcard "*" means all attributes, and a wildcard like
   --  "standard::*" means all attributes in the standard namespace. An example
   --  attribute query be "standard::*,owner::user". The standard attributes
   --  are available as defines, like G_FILE_ATTRIBUTE_STANDARD_NAME.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  For symlinks, normally the information about the target of the symlink
   --  is returned, rather than information about the symlink itself. However
   --  if you pass Glib.GFile.G_File_Query_Info_Nofollow_Symlinks in Flags the
   --  information about the symlink itself will be returned. Also, for
   --  symlinks that point to non-existing files the information about the
   --  symlink itself will be returned.
   --  If the file does not exist, the G_IO_ERROR_NOT_FOUND error will be
   --  returned. Other errors are possible too, and depend on what kind of
   --  filesystem the file is on.
   --  @param Attributes an attribute query string
   --  @param Flags a set of Glib.GFile.GFile_Query_Info_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return a Glib.File_Info.Gfile_Info for the given File, or null on
   --  error. Free the returned object with g_object_unref.

   procedure Query_Info_Async
      (Self        : Gfile;
       Attributes  : UTF8_String;
       Flags       : GFile_Query_Info_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Query_Info_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result) return Glib.File_Info.Gfile_Info;
   --  Finishes an asynchronous file info query. See
   --  Glib.GFile.Query_Info_Async.
   --  @param Res a Glib.G_Async_Result
   --  @return Glib.File_Info.Gfile_Info for given File or null on error. Free
   --  the returned object with g_object_unref.

   function Read
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Input_Stream.Gfile_Input_Stream;
   --  Opens a file for reading. The result is a
   --  Glib.File_Input_Stream.Gfile_Input_Stream that can be used to read the
   --  contents of the file.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  If the file does not exist, the G_IO_ERROR_NOT_FOUND error will be
   --  returned. If the file is a directory, the G_IO_ERROR_IS_DIRECTORY error
   --  will be returned. Other errors are possible too, and depend on what kind
   --  of filesystem the file is on.
   --  @param Cancellable a Glib.Cancellable.Gcancellable
   --  @return Glib.File_Input_Stream.Gfile_Input_Stream or null on error.
   --  Free the returned object with g_object_unref.

   procedure Read_Async
      (Self        : Gfile;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Read_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result)
       return Glib.File_Input_Stream.Gfile_Input_Stream;
   --  Finishes an asynchronous file read operation started with
   --  Glib.GFile.Read_Async.
   --  @param Res a Glib.G_Async_Result
   --  @return a Glib.File_Input_Stream.Gfile_Input_Stream or null on error.
   --  Free the returned object with g_object_unref.

   function Replace
      (Self        : Gfile;
       Etag        : UTF8_String := "";
       Make_Backup : Boolean;
       Flags       : GFile_Create_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Output_Stream.Gfile_Output_Stream;
   --  Returns an output stream for overwriting the file, possibly creating a
   --  backup copy of the file first. If the file doesn't exist, it will be
   --  created.
   --  This will try to replace the file in the safest way possible so that
   --  any errors during the writing will not affect an already existing copy
   --  of the file. For instance, for local files it may write to a temporary
   --  file and then atomically rename over the destination when the stream is
   --  closed.
   --  By default files created are generally readable by everyone, but if you
   --  pass Glib.GFile.G_File_Create_Private in Flags the file will be made
   --  readable only to the current user, to the level that is supported on the
   --  target filesystem.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  If you pass in a non-null Etag value and File already exists, then this
   --  value is compared to the current entity tag of the file, and if they
   --  differ an G_IO_ERROR_WRONG_ETAG error is returned. This generally means
   --  that the file has been changed since you last read it. You can get the
   --  new etag from Glib.File_Output_Stream.Get_Etag after you've finished
   --  writing and closed the Glib.File_Output_Stream.Gfile_Output_Stream. When
   --  you load a new file you can use Glib.File_Input_Stream.Query_Info to get
   --  the etag of the file.
   --  If Make_Backup is True, this function will attempt to make a backup of
   --  the current file before overwriting it. If this fails a
   --  G_IO_ERROR_CANT_CREATE_BACKUP error will be returned. If you want to
   --  replace anyway, try again with Make_Backup set to False.
   --  If the file is a directory the G_IO_ERROR_IS_DIRECTORY error will be
   --  returned, and if the file is some other form of non-regular file then a
   --  G_IO_ERROR_NOT_REGULAR_FILE error will be returned. Some file systems
   --  don't allow all file names, and may return an
   --  G_IO_ERROR_INVALID_FILENAME error, and if the name is to long
   --  G_IO_ERROR_FILENAME_TOO_LONG will be returned. Other errors are possible
   --  too, and depend on what kind of filesystem the file is on.
   --  @param Etag an optional [entity tag][gfile-etag] for the current
   --  Glib.GFile.Gfile, or NULL to ignore
   --  @param Make_Backup True if a backup should be created
   --  @param Flags a set of Glib.GFile.GFile_Create_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return a Glib.File_Output_Stream.Gfile_Output_Stream or null on error.
   --  Free the returned object with g_object_unref.

   procedure Replace_Async
      (Self        : Gfile;
       Etag        : UTF8_String := "";
       Make_Backup : Boolean;
       Flags       : GFile_Create_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Replace_Contents
      (Self        : Gfile;
       Contents    : Guint8_Array;
       Etag        : UTF8_String := "";
       Make_Backup : Boolean;
       Flags       : GFile_Create_Flags;
       New_Etag    : access UTF8_String := null;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Replaces the contents of File with Contents of Length bytes.
   --  If Etag is specified (not null), any existing file must have that etag,
   --  or the error G_IO_ERROR_WRONG_ETAG will be returned.
   --  If Make_Backup is True, this function will attempt to make a backup of
   --  File. Internally, it uses Glib.GFile.Replace, so will try to replace the
   --  file contents in the safest way possible. For example, atomic renames
   --  are used when replacing local files' contents.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  The returned New_Etag can be used to verify that the file hasn't
   --  changed the next time it is saved over.
   --  @param Contents a string containing the new contents for File
   --  @param Etag the old [entity-tag][gfile-etag] for the document, or null
   --  @param Make_Backup True if a backup should be created
   --  @param Flags a set of Glib.GFile.GFile_Create_Flags
   --  @param New_Etag a location to a new [entity tag][gfile-etag] for the
   --  document. This should be freed with g_free when no longer needed, or
   --  null
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return True if successful. If an error has occurred, this function
   --  will return False and set Error appropriately if present.

   procedure Replace_Contents_Async
      (Self        : Gfile;
       Contents    : Guint8_Array;
       Etag        : UTF8_String := "";
       Make_Backup : Boolean;
       Flags       : GFile_Create_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   procedure Replace_Contents_Bytes_Async
      (Self        : Gfile;
       Contents    : Glib.Bytes.Gbytes;
       Etag        : UTF8_String := "";
       Make_Backup : Boolean;
       Flags       : GFile_Create_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Replace_Contents_Finish
      (Self     : Gfile;
       Res      : Glib.G_Async_Result;
       New_Etag : access UTF8_String := null) return Boolean;
   --  Finishes an asynchronous replace of the given File. See
   --  Glib.GFile.Replace_Contents_Async. Sets New_Etag to the new entity tag
   --  for the document, if present.
   --  @param Res a Glib.G_Async_Result
   --  @param New_Etag a location of a new [entity tag][gfile-etag] for the
   --  document. This should be freed with g_free when it is no longer needed,
   --  or null
   --  @return True on success, False on failure.

   function Replace_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result)
       return Glib.File_Output_Stream.Gfile_Output_Stream;
   --  Finishes an asynchronous file replace operation started with
   --  Glib.GFile.Replace_Async.
   --  @param Res a Glib.G_Async_Result
   --  @return a Glib.File_Output_Stream.Gfile_Output_Stream, or null on
   --  error. Free the returned object with g_object_unref.

   function Replace_Readwrite
      (Self        : Gfile;
       Etag        : UTF8_String := "";
       Make_Backup : Boolean;
       Flags       : GFile_Create_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_IO_Stream.Gfile_Iostream;
   --  Returns an output stream for overwriting the file in readwrite mode,
   --  possibly creating a backup copy of the file first. If the file doesn't
   --  exist, it will be created.
   --  For details about the behaviour, see Glib.GFile.Replace which does the
   --  same thing but returns an output stream only.
   --  Note that in many non-local file cases read and write streams are not
   --  supported, so make sure you really need to do read and write streaming,
   --  rather than just opening for reading or writing.
   --  Since: gtk+ 2.22
   --  @param Etag an optional [entity tag][gfile-etag] for the current
   --  Glib.GFile.Gfile, or NULL to ignore
   --  @param Make_Backup True if a backup should be created
   --  @param Flags a set of Glib.GFile.GFile_Create_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return a Glib.File_IO_Stream.Gfile_Iostream or null on error. Free the
   --  returned object with g_object_unref.

   procedure Replace_Readwrite_Async
      (Self        : Gfile;
       Etag        : UTF8_String := "";
       Make_Backup : Boolean;
       Flags       : GFile_Create_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Replace_Readwrite_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result) return Glib.File_IO_Stream.Gfile_Iostream;
   --  Finishes an asynchronous file replace operation started with
   --  Glib.GFile.Replace_Readwrite_Async.
   --  Since: gtk+ 2.22
   --  @param Res a Glib.G_Async_Result
   --  @return a Glib.File_IO_Stream.Gfile_Iostream, or null on error. Free
   --  the returned object with g_object_unref.

   function Resolve_Relative_Path
      (Self          : Gfile;
       Relative_Path : UTF8_String) return Gfile;
   --  Resolves a relative path for File to an absolute path.
   --  This call does no blocking I/O.
   --  If the Relative_Path is an absolute path name, the resolution is done
   --  absolutely (without taking File path as base).
   --  @param Relative_Path a given relative path string
   --  @return a Glib.GFile.Gfile for the resolved path.

   function Set_Attribute_Byte_String
      (Self        : Gfile;
       Attribute   : UTF8_String;
       Value       : UTF8_String;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Sets Attribute of type G_FILE_ATTRIBUTE_TYPE_BYTE_STRING to Value. If
   --  Attribute is of a different type, this operation will fail, returning
   --  False.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  @param Attribute a string containing the attribute's name
   --  @param Value a string containing the attribute's new value
   --  @param Flags a Glib.GFile.GFile_Query_Info_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return True if the Attribute was successfully set to Value in the
   --  File, False otherwise.

   function Set_Attribute_Int32
      (Self        : Gfile;
       Attribute   : UTF8_String;
       Value       : Gint32;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Sets Attribute of type G_FILE_ATTRIBUTE_TYPE_INT32 to Value. If
   --  Attribute is of a different type, this operation will fail.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  @param Attribute a string containing the attribute's name
   --  @param Value a Gint32 containing the attribute's new value
   --  @param Flags a Glib.GFile.GFile_Query_Info_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return True if the Attribute was successfully set to Value in the
   --  File, False otherwise.

   function Set_Attribute_Int64
      (Self        : Gfile;
       Attribute   : UTF8_String;
       Value       : Gint64;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Sets Attribute of type G_FILE_ATTRIBUTE_TYPE_INT64 to Value. If
   --  Attribute is of a different type, this operation will fail.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  @param Attribute a string containing the attribute's name
   --  @param Value a Guint64 containing the attribute's new value
   --  @param Flags a Glib.GFile.GFile_Query_Info_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return True if the Attribute was successfully set, False otherwise.

   function Set_Attribute_String
      (Self        : Gfile;
       Attribute   : UTF8_String;
       Value       : UTF8_String;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Sets Attribute of type G_FILE_ATTRIBUTE_TYPE_STRING to Value. If
   --  Attribute is of a different type, this operation will fail.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  @param Attribute a string containing the attribute's name
   --  @param Value a string containing the attribute's value
   --  @param Flags Glib.GFile.GFile_Query_Info_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return True if the Attribute was successfully set, False otherwise.

   function Set_Attribute_Uint32
      (Self        : Gfile;
       Attribute   : UTF8_String;
       Value       : Guint32;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Sets Attribute of type G_FILE_ATTRIBUTE_TYPE_UINT32 to Value. If
   --  Attribute is of a different type, this operation will fail.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  @param Attribute a string containing the attribute's name
   --  @param Value a Guint32 containing the attribute's new value
   --  @param Flags a Glib.GFile.GFile_Query_Info_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return True if the Attribute was successfully set to Value in the
   --  File, False otherwise.

   function Set_Attribute_Uint64
      (Self        : Gfile;
       Attribute   : UTF8_String;
       Value       : Guint64;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Sets Attribute of type G_FILE_ATTRIBUTE_TYPE_UINT64 to Value. If
   --  Attribute is of a different type, this operation will fail.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  @param Attribute a string containing the attribute's name
   --  @param Value a Guint64 containing the attribute's new value
   --  @param Flags a Glib.GFile.GFile_Query_Info_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return True if the Attribute was successfully set to Value in the
   --  File, False otherwise.

   procedure Set_Attributes_Async
      (Self        : Gfile;
       Info        : not null access Glib.File_Info.Gfile_Info_Record'Class;
       Flags       : GFile_Query_Info_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied

   function Set_Attributes_Finish
      (Self   : Gfile;
       Result : Glib.G_Async_Result;
       Info   : out Glib.File_Info.Gfile_Info) return Boolean;
   --  Finishes setting an attribute started in
   --  Glib.GFile.Set_Attributes_Async.
   --  Parameter Info has transfer-ownership='full'
   --  @param Result a Glib.G_Async_Result
   --  @param Info a Glib.File_Info.Gfile_Info
   --  @return True if the attributes were set correctly, False otherwise.

   function Set_Attributes_From_Info
      (Self        : Gfile;
       Info        : not null access Glib.File_Info.Gfile_Info_Record'Class;
       Flags       : GFile_Query_Info_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Tries to set all attributes in the Glib.File_Info.Gfile_Info on the
   --  target values, not stopping on the first error.
   --  If there is any error during this operation then Error will be set to
   --  the first error. Error on particular fields are flagged by setting the
   --  "status" field in the attribute value to
   --  G_FILE_ATTRIBUTE_STATUS_ERROR_SETTING, which means you can also detect
   --  further errors.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  @param Info a Glib.File_Info.Gfile_Info
   --  @param Flags Glib.GFile.GFile_Query_Info_Flags
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return False if there was any error, True otherwise.

   function Set_Display_Name
      (Self         : Gfile;
       Display_Name : UTF8_String;
       Cancellable  : access Glib.Cancellable.Gcancellable_Record'Class)
       return Gfile;
   --  Renames File to the specified display name.
   --  The display name is converted from UTF-8 to the correct encoding for
   --  the target filesystem if possible and the File is renamed to this.
   --  If you want to implement a rename operation in the user interface the
   --  edit name (G_FILE_ATTRIBUTE_STANDARD_EDIT_NAME) should be used as the
   --  initial value in the rename widget, and then the result after editing
   --  should be passed to Glib.GFile.Set_Display_Name.
   --  On success the resulting converted filename is returned.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  @param Display_Name a string
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return a Glib.GFile.Gfile specifying what File was renamed to, or null
   --  if there was an error. Free the returned object with g_object_unref.

   procedure Set_Display_Name_Async
      (Self         : Gfile;
       Display_Name : UTF8_String;
       Io_Priority  : Glib.Gint;
       Cancellable  : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback     : Gasync_Ready_Callback);
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

   function Set_Display_Name_Finish
      (Self : Gfile;
       Res  : Glib.G_Async_Result) return Gfile;
   pragma Import (C, Set_Display_Name_Finish, "g_file_set_display_name_finish");
   --  Finishes setting a display name started with
   --  Glib.GFile.Set_Display_Name_Async.
   --  @param Res a Glib.G_Async_Result
   --  @return a Glib.GFile.Gfile or null on error. Free the returned object
   --  with g_object_unref.

   function Supports_Thread_Contexts (Self : Gfile) return Boolean;
   --  Checks if File supports [thread-default
   --  contexts][g-main-context-push-thread-default-context]. If this returns
   --  False, you cannot perform asynchronous operations on File in a thread
   --  that has a thread-default context.
   --  Since: gtk+ 2.22
   --  @return Whether or not File supports thread-default contexts.

   function Trash
      (Self        : Gfile;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Sends File to the "Trashcan", if possible. This is similar to deleting
   --  it, but the user can recover it before emptying the trashcan. Not all
   --  file systems support trashing, so this call can return the
   --  G_IO_ERROR_NOT_SUPPORTED error. Since GLib 2.66, the `x-gvfs-notrash`
   --  unix mount option can be used to disable Glib.GFile.Trash support for
   --  certain mounts, the G_IO_ERROR_NOT_SUPPORTED error will be returned in
   --  that case.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return True on successful trash, False otherwise.

   procedure Trash_Async
      (Self        : Gfile;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Asynchronously sends File to the Trash location, if possible.
   --  Since: gtk+ 2.38
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied

   function Trash_Finish
      (Self   : Gfile;
       Result : Glib.G_Async_Result) return Boolean;
   --  Finishes an asynchronous file trashing operation, started with
   --  Glib.GFile.Trash_Async.
   --  Since: gtk+ 2.38
   --  @param Result a Glib.G_Async_Result
   --  @return True on successful trash, False otherwise.

   procedure New_Tmp_Async
      (Tmpl        : UTF8_String := "";
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Asynchronously opens a file in the preferred directory for temporary
   --  files (as returned by g_get_tmp_dir) as g_file_new_tmp.
   --  Tmpl should be a string in the GLib file name encoding containing a
   --  sequence of six 'X' characters, and containing no directory components.
   --  If it is null, a default template is used.
   --  Since: gtk+ 2.74
   --  @param Tmpl Template for the file name, as in g_file_open_tmp, or null
   --  for a default template
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  done

   procedure New_Tmp_Dir_Async
      (Tmpl        : UTF8_String := "";
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Asynchronously creates a directory in the preferred directory for
   --  temporary files (as returned by g_get_tmp_dir) as g_dir_make_tmp.
   --  Tmpl should be a string in the GLib file name encoding containing a
   --  sequence of six 'X' characters, and containing no directory components.
   --  If it is null, a default template is used.
   --  Since: gtk+ 2.74
   --  @param Tmpl Template for the file name, as in g_dir_make_tmp, or null
   --  for a default template
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  done

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Hash (Self : Gfile) return Guint;
   pragma Import (C, Hash, "g_file_hash");
   --  Creates a hash value for a Glib.GFile.Gfile. Two files that are Equal
   --  have the same hash.
   --
   --  This call does no blocking I/O.
   --
   --  @return 0 if Self is not a valid Glib.GFile.Gfile, otherwise an
   --  integer that can be used as a hash value for it.

   ---------------
   -- Functions --
   ---------------

   function New_Build_Filenamev
      (Args : GNAT.Strings.String_List) return Gfile;
   --  Constructs a Glib.GFile.Gfile from a vector of elements using the
   --  correct separator for filenames.
   --  Using this function is equivalent to calling g_build_filenamev,
   --  followed by Glib.GFile.New_For_Path on the result.
   --  Since: gtk+ 2.78
   --  @param Args null-terminated array of strings containing the path
   --  elements.
   --  @return a new Glib.GFile.Gfile

   function New_For_Commandline_Arg (Arg : UTF8_String) return Gfile;
   --  Creates a Glib.GFile.Gfile with the given argument from the command
   --  line. The value of Arg can be either a URI, an absolute path or a
   --  relative path resolved relative to the current working directory. This
   --  operation never fails, but the returned object might not support any I/O
   --  operation if Arg points to a malformed path.
   --  Note that on Windows, this function expects its argument to be in UTF-8
   --  -- not the system code page. This means that you should not use this
   --  function with string from argv as it is passed to main.
   --  g_win32_get_command_line will return a UTF-8 version of the commandline.
   --  Glib.Application.Gapplication also uses UTF-8 but
   --  g_application_command_line_create_file_for_arg may be more useful for
   --  you there. It is also always possible to use this function with
   --  Glib.Option.Goption_Context arguments of type
   --  Glib.Option.G_Option_Arg_Filename.
   --  @param Arg a command line string
   --  @return a new Glib.GFile.Gfile. Free the returned object with
   --  g_object_unref.

   function New_For_Commandline_Arg_And_Cwd
      (Arg : UTF8_String;
       Cwd : UTF8_String) return Gfile;
   --  Creates a Glib.GFile.Gfile with the given argument from the command
   --  line.
   --  This function is similar to Glib.GFile.New_For_Commandline_Arg except
   --  that it allows for passing the current working directory as an argument
   --  instead of using the current working directory of the process.
   --  This is useful if the commandline argument was given in a context other
   --  than the invocation of the current process.
   --  See also g_application_command_line_create_file_for_arg.
   --  Since: gtk+ 2.36
   --  @param Arg a command line string
   --  @param Cwd the current working directory of the commandline
   --  @return a new Glib.GFile.Gfile

   function New_For_Path (Path : UTF8_String) return Gfile;
   --  Constructs a Glib.GFile.Gfile for a given path. This operation never
   --  fails, but the returned object might not support any I/O operation if
   --  Path is malformed.
   --  @param Path a string containing a relative or absolute path. The string
   --  must be encoded in the glib filename encoding.
   --  @return a new Glib.GFile.Gfile for the given Path. Free the returned
   --  object with g_object_unref.

   function New_For_Uri (URI : UTF8_String) return Gfile;
   --  Constructs a Glib.GFile.Gfile for a given URI. This operation never
   --  fails, but the returned object might not support any I/O operation if
   --  Uri is malformed or if the uri type is not supported.
   --  @param URI a UTF-8 string containing a URI
   --  @return a new Glib.GFile.Gfile for the given Uri. Free the returned
   --  object with g_object_unref.

   function New_Tmp_Dir_Finish (Result : Glib.G_Async_Result) return Gfile;
   pragma Import (C, New_Tmp_Dir_Finish, "g_file_new_tmp_dir_finish");
   --  Finishes a temporary directory creation started by
   --  Glib.GFile.New_Tmp_Dir_Async.
   --  Since: gtk+ 2.74
   --  @param Result a Glib.G_Async_Result
   --  @return a new Glib.GFile.Gfile. Free the returned object with
   --  g_object_unref.

   function New_Tmp_Finish
      (Result   : Glib.G_Async_Result;
       Iostream : out Glib.File_IO_Stream.Gfile_Iostream) return Gfile;
   --  Finishes a temporary file creation started by Glib.GFile.New_Tmp_Async.
   --  Since: gtk+ 2.74
   --  Parameter Iostream has transfer-ownership='full'
   --  @param Result a Glib.G_Async_Result
   --  @param Iostream on return, a Glib.File_IO_Stream.Gfile_Iostream for the
   --  created file
   --  @return a new Glib.GFile.Gfile. Free the returned object with
   --  g_object_unref.

   function Parse_Name (Parse_Name : UTF8_String) return Gfile;
   --  Constructs a Glib.GFile.Gfile with the given Parse_Name (i.e. something
   --  given by Glib.GFile.Get_Parse_Name). This operation never fails, but the
   --  returned object might not support any I/O operation if the Parse_Name
   --  cannot be parsed.
   --  @param Parse_Name a file name or path to be parsed
   --  @return a new Glib.GFile.Gfile.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gfile"

   function "+" (W : Gfile) return Gfile;
   pragma Inline ("+");

private

   Null_Gfile : constant Gfile :=
      Gfile (Glib.Types.Null_Interface);
end Glib.GFile;
