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

--  Glib.GFile_Enumerators.Gfile_Enumerator allows you to operate on a set of
--  GFiles, returning a Glib.File_Info.Gfile_Info structure for each file
--  enumerated (e.g. Glib.GFile.Enumerate_Children will return a
--  Glib.GFile_Enumerators.Gfile_Enumerator for each of the children within a
--  directory).
--
--  To get the next file's information from a
--  Glib.GFile_Enumerators.Gfile_Enumerator, use
--  Glib.GFile_Enumerators.Next_File or its asynchronous version,
--  Glib.GFile_Enumerators.Next_Files_Async. Note that the asynchronous version
--  will return a list of GFile_Infos, whereas the synchronous will only return
--  the next file in the enumerator.
--
--  The ordering of returned files is unspecified for non-Unix platforms; for
--  more information, see g_dir_read_name. On Unix, when operating on local
--  files, returned files will be sorted by inode number. Effectively you can
--  assume that the ordering of returned files will be stable between
--  successive calls (and applications) assuming the directory is unchanged.
--
--  If your application needs a specific ordering, such as by name or
--  modification time, you will have to implement that in your application
--  code.
--
--  To close a Glib.GFile_Enumerators.Gfile_Enumerator, use
--  Glib.GFile_Enumerators.Close, or its asynchronous version,
--  Glib.GFile_Enumerators.Close_Async. Once a
--  Glib.GFile_Enumerators.Gfile_Enumerator is closed, no further actions may
--  be performed on it, and it should be freed with g_object_unref.

pragma Warnings (Off, "*is already use-visible*");
with Glib.Cancellable; use Glib.Cancellable;
with Glib.Error;       use Glib.Error;
with Glib.File_Info;   use Glib.File_Info;
with Glib.GFile;       use Glib.GFile;
with Glib.Object;      use Glib.Object;

package Glib.GFile_Enumerators is

   type Gfile_Enumerator_Record is new GObject_Record with null record;
   type Gfile_Enumerator is access all Gfile_Enumerator_Record'Class;

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

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_file_enumerator_get_type");

   -------------
   -- Methods --
   -------------

   function Close
      (Self        : not null access Gfile_Enumerator_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Error       : out Glib.Error.GError) return Boolean;
   --  Releases all resources used by this enumerator, making the enumerator
   --  return G_IO_ERROR_CLOSED on all calls.
   --  This will be automatically called when the last reference is dropped,
   --  but you might want to call this function to make sure resources are
   --  released as early as possible.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Error the return location for a recoverable error
   --  @return TRUE on success or FALSE on error.

   procedure Close_Async
      (Self        : not null access Gfile_Enumerator_Record;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Close_Finish
      (Self   : not null access Gfile_Enumerator_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean;
   --  Finishes closing a file enumerator, started from
   --  Glib.GFile_Enumerators.Close_Async.
   --  If the file enumerator was already closed when
   --  Glib.GFile_Enumerators.Close_Async was called, then this function will
   --  report G_IO_ERROR_CLOSED in Error, and return False. If the file
   --  enumerator had pending operation when the close operation was started,
   --  then this function will report G_IO_ERROR_PENDING, and return False. If
   --  Cancellable was not null, then the operation may have been cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be set, and False
   --  will be returned.
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return True if the close operation has finished successfully.

   function Get_Child
      (Self : not null access Gfile_Enumerator_Record;
       Info : not null access Glib.File_Info.Gfile_Info_Record'Class)
       return Glib.GFile.Gfile;
   --  Return a new Glib.GFile.Gfile which refers to the file named by Info in
   --  the source directory of Enumerator. This function is primarily intended
   --  to be used inside loops with Glib.GFile_Enumerators.Next_File.
   --  To use this, G_FILE_ATTRIBUTE_STANDARD_NAME must have been listed in
   --  the attributes list used when creating the
   --  Glib.GFile_Enumerators.Gfile_Enumerator.
   --  This is a convenience method that's equivalent to:
   --
   --     gchar *name = g_file_info_get_name (info);
   --     GFile *child = g_file_get_child (g_file_enumerator_get_container (enumr),
   --                                      name);
   --
   --  Since: gtk+ 2.36
   --  @param Info a Glib.File_Info.Gfile_Info gotten from
   --  Glib.GFile_Enumerators.Next_File or the async equivalents.
   --  @return a Glib.GFile.Gfile for the Glib.File_Info.Gfile_Info passed it.

   function Get_Container
      (Self : not null access Gfile_Enumerator_Record)
       return Glib.GFile.Gfile;
   --  Get the Glib.GFile.Gfile container which is being enumerated.
   --  Since: gtk+ 2.18
   --  @return the Glib.GFile.Gfile which is being enumerated.

   function Has_Pending
      (Self : not null access Gfile_Enumerator_Record) return Boolean;
   --  Checks if the file enumerator has pending operations.
   --  @return True if the Enumerator has pending operations.

   function Is_Closed
      (Self : not null access Gfile_Enumerator_Record) return Boolean;
   --  Checks if the file enumerator has been closed.
   --  @return True if the Enumerator is closed.

   function Iterate
      (Self        : not null access Gfile_Enumerator_Record;
       Out_Info    : access Glib.File_Info.Gfile_Info := null;
       Out_Child   : access Glib.GFile.Gfile := null;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Error       : out Glib.Error.GError) return Boolean;
   --  This is a version of Glib.GFile_Enumerators.Next_File that's easier to
   --  use correctly from C programs. With Glib.GFile_Enumerators.Next_File,
   --  the gboolean return value signifies "end of iteration or error", which
   --  requires allocation of a temporary Gerror.Gerror.
   --  In contrast, with this function, a False return from
   --  Glib.GFile_Enumerators.Iterate *always* means "error". End of iteration
   --  is signaled by Out_Info or Out_Child being null.
   --  Another crucial difference is that the references for Out_Info and
   --  Out_Child are owned by Direnum (they are cached as hidden properties).
   --  You must not unref them in your own code. This makes memory management
   --  significantly easier for C code in combination with loops.
   --  Finally, this function optionally allows retrieving a Glib.GFile.Gfile
   --  as well.
   --  You must specify at least one of Out_Info or Out_Child.
   --  The code pattern for correctly using Glib.GFile_Enumerators.Iterate
   --  from C is:
   --
   --     direnum = g_file_enumerate_children (file, ...);
   --     while (TRUE)
   --       {
   --         GFileInfo *info;
   --         if (!g_file_enumerator_iterate (direnum, &info, NULL, cancellable, error))
   --           goto out;
   --         if (!info)
   --           break;
   --         ... do stuff with "info"; do not unref it! ...
   --       }
   --
   --     out:
   --       g_object_unref (direnum); // Note: frees the last Info
   --
   --  Since: gtk+ 2.44
   --  @param Out_Info Output location for the next Glib.File_Info.Gfile_Info,
   --  or null
   --  @param Out_Child Output location for the next Glib.GFile.Gfile, or null
   --  @param Cancellable a Glib.Cancellable.Gcancellable
   --  @param Error the return location for a recoverable error

   function Next_File
      (Self        : not null access Gfile_Enumerator_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Error       : out Glib.Error.GError) return Glib.File_Info.Gfile_Info;
   --  Returns information for the next file in the enumerated object. Will
   --  block until the information is available. The Glib.File_Info.Gfile_Info
   --  returned from this function will contain attributes that match the
   --  attribute string that was passed when the
   --  Glib.GFile_Enumerators.Gfile_Enumerator was created.
   --  See the documentation of Glib.GFile_Enumerators.Gfile_Enumerator for
   --  information about the order of returned files.
   --  On error, returns null and sets Error to the error. If the enumerator
   --  is at the end, null will be returned and Error will be unset.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Error the return location for a recoverable error
   --  @return A Glib.File_Info.Gfile_Info or null on error or end of
   --  enumerator. Free the returned object with g_object_unref when no longer
   --  needed. Has transfer-ownership='full'.

   procedure Next_Files_Async
      (Self        : not null access Gfile_Enumerator_Record;
       Num_Files   : Glib.Gint;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   function Next_Files_Finish
      (Self   : not null access Gfile_Enumerator_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError)
       return Glib.File_Info.File_Info_List.Glist;
   --  Finishes the asynchronous operation started with
   --  Glib.GFile_Enumerators.Next_Files_Async.
   --  @param Result a Glib.G_Async_Result.
   --  @param Error the return location for a recoverable error
   --  @return a GList of GFile_Infos. You must free the list with g_list_free
   --  and unref the infos with g_object_unref when you're done with them.

   procedure Set_Pending
      (Self    : not null access Gfile_Enumerator_Record;
       Pending : Boolean);
   --  Sets the file enumerator as having pending operations.
   --  @param Pending a boolean value.

end Glib.GFile_Enumerators;
