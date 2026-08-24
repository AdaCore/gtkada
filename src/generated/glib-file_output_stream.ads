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

--  GFileOutputStream provides output streams that write their content to a
--  file.
--
--  GFileOutputStream implements Gseekable.Gseekable, which allows the output
--  stream to jump to arbitrary positions in the file and to truncate the file,
--  provided the filesystem of the file supports these operations.
--
--  To find the position of a file output stream, use g_seekable_tell. To find
--  out if a file output stream supports seeking, use g_seekable_can_seek.To
--  position a file output stream, use g_seekable_seek. To find out if a file
--  output stream supports truncating, use g_seekable_can_truncate. To truncate
--  a file output stream, use g_seekable_truncate.

pragma Warnings (Off, "*is already use-visible*");
with Glib.Cancellable;   use Glib.Cancellable;
with Glib.File_Info;     use Glib.File_Info;
with Glib.Object;        use Glib.Object;
with Glib.Output_Stream; use Glib.Output_Stream;

package Glib.File_Output_Stream is

   type Gfile_Output_Stream_Record is new Goutput_Stream_Record with null record;
   type Gfile_Output_Stream is access all Gfile_Output_Stream_Record'Class;

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
   pragma Import (C, Get_Type, "g_file_output_stream_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Etag
      (Self : not null access Gfile_Output_Stream_Record) return UTF8_String;
   --  Gets the entity tag for the file when it has been written. This must be
   --  called after the stream has been written and closed, as the etag can
   --  change while writing.
   --  @return the entity tag for the stream.

   function Query_Info
      (Self        : not null access Gfile_Output_Stream_Record;
       Attributes  : UTF8_String;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Info.Gfile_Info;
   --  Queries a file output stream for the given Attributes. This function
   --  blocks while querying the stream. For the asynchronous version of this
   --  function, see Glib.File_Output_Stream.Query_Info_Async. While the stream
   --  is blocked, the stream will set the pending flag internally, and any
   --  other operations on the stream will fail with G_IO_ERROR_PENDING.
   --  Can fail if the stream was already closed (with Error being set to
   --  G_IO_ERROR_CLOSED), the stream has pending operations (with Error being
   --  set to G_IO_ERROR_PENDING), or if querying info is not supported for the
   --  stream's interface (with Error being set to G_IO_ERROR_NOT_SUPPORTED).
   --  In all cases of failure, null will be returned.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be set, and null will
   --  be returned.
   --  @param Attributes a file attribute query string.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @return a Glib.File_Info.Gfile_Info for the Stream, or null on error.

   procedure Query_Info_Async
      (Self        : not null access Gfile_Output_Stream_Record;
       Attributes  : UTF8_String;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Asynchronously queries the Stream for a Glib.File_Info.Gfile_Info. When
   --  completed, Callback will be called with a Glib.G_Async_Result which can
   --  be used to finish the operation with
   --  Glib.File_Output_Stream.Query_Info_Finish.
   --  For the synchronous version of this function, see
   --  Glib.File_Output_Stream.Query_Info.
   --  @param Attributes a file attribute query string.
   --  @param Io_Priority the [I/O priority][gio-GIOScheduler] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback callback to call when the request is satisfied

   function Query_Info_Finish
      (Self   : not null access Gfile_Output_Stream_Record;
       Result : Glib.G_Async_Result) return Glib.File_Info.Gfile_Info;
   --  Finalizes the asynchronous query started by
   --  Glib.File_Output_Stream.Query_Info_Async.
   --  @param Result a Glib.G_Async_Result.
   --  @return A Glib.File_Info.Gfile_Info for the finished query.

end Glib.File_Output_Stream;
