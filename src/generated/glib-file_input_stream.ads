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

--  GFileInputStream provides input streams that take their content from a
--  file.
--
--  GFileInputStream implements Gseekable.Gseekable, which allows the input
--  stream to jump to arbitrary positions in the file, provided the filesystem
--  of the file allows it. To find the position of a file input stream, use
--  g_seekable_tell. To find out if a file input stream supports seeking, use
--  g_seekable_can_seek. To position a file input stream, use g_seekable_seek.

pragma Warnings (Off, "*is already use-visible*");
with Glib.Cancellable;  use Glib.Cancellable;
with Glib.File_Info;    use Glib.File_Info;
with Glib.Input_Stream; use Glib.Input_Stream;
with Glib.Object;       use Glib.Object;

package Glib.File_Input_Stream is

   type Gfile_Input_Stream_Record is new Ginput_Stream_Record with null record;
   type Gfile_Input_Stream is access all Gfile_Input_Stream_Record'Class;

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
   pragma Import (C, Get_Type, "g_file_input_stream_get_type");

   -------------
   -- Methods --
   -------------

   function Query_Info
      (Self        : not null access Gfile_Input_Stream_Record;
       Attributes  : UTF8_String;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Info.Gfile_Info;
   --  Queries a file input stream the given Attributes. This function blocks
   --  while querying the stream. For the asynchronous (non-blocking) version
   --  of this function, see Glib.File_Input_Stream.Query_Info_Async. While the
   --  stream is blocked, the stream will set the pending flag internally, and
   --  any other operations on the stream will fail with G_IO_ERROR_PENDING.
   --  @param Attributes a file attribute query string.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @return a Glib.File_Info.Gfile_Info, or null on error.

   procedure Query_Info_Async
      (Self        : not null access Gfile_Input_Stream_Record;
       Attributes  : UTF8_String;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Queries the stream information asynchronously. When the operation is
   --  finished Callback will be called. You can then call
   --  Glib.File_Input_Stream.Query_Info_Finish to get the result of the
   --  operation.
   --  For the synchronous version of this function, see
   --  Glib.File_Input_Stream.Query_Info.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be set
   --  @param Attributes a file attribute query string.
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied

   function Query_Info_Finish
      (Self   : not null access Gfile_Input_Stream_Record;
       Result : Glib.G_Async_Result) return Glib.File_Info.Gfile_Info;
   --  Finishes an asynchronous info query operation.
   --  @param Result a Glib.G_Async_Result.
   --  @return Glib.File_Info.Gfile_Info.

end Glib.File_Input_Stream;
