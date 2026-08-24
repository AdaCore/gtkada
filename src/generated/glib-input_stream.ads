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

--  Glib.Input_Stream.Ginput_Stream has functions to read from a stream
--  (g_input_stream_read), to close a stream (g_input_stream_close) and to skip
--  some content (g_input_stream_skip).
--
--  To copy the content of an input stream to an output stream without
--  manually handling the reads and writes, use Glib.Output_Stream.Splice.
--
--  See the documentation for Glib.IO_Stream.Giostream for details of thread
--  safety of streaming APIs.
--
--  All of these functions have async variants too.

pragma Warnings (Off, "*is already use-visible*");
with Glib.Bytes;       use Glib.Bytes;
with Glib.Cancellable; use Glib.Cancellable;
with Glib.Object;      use Glib.Object;

package Glib.Input_Stream is

   type Ginput_Stream_Record is new GObject_Record with null record;
   type Ginput_Stream is access all Ginput_Stream_Record'Class;

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
   pragma Import (C, Get_Type, "g_input_stream_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clear_Pending (Self : not null access Ginput_Stream_Record);
   --  Clears the pending flag on Stream.

   function Close
      (Self        : not null access Ginput_Stream_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Closes the stream, releasing resources related to it.
   --  Once the stream is closed, all other operations will return
   --  G_IO_ERROR_CLOSED. Closing a stream multiple times will not return an
   --  error.
   --  Streams will be automatically closed when the last reference is
   --  dropped, but you might want to call this function to make sure resources
   --  are released as early as possible.
   --  Some streams might keep the backing store of the stream (e.g. a file
   --  descriptor) open after the stream is closed. See the documentation for
   --  the individual stream for details.
   --  On failure the first error that happened will be reported, but the
   --  close operation will finish as much as possible. A stream that failed to
   --  close will still return G_IO_ERROR_CLOSED for all operations. Still, it
   --  is important to check and report the error to the user.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  Cancelling a close will still leave the stream closed, but some streams
   --  can use a faster close that doesn't block to e.g. check errors.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @return True on success, False on failure

   procedure Close_Async
      (Self        : not null access Ginput_Stream_Record;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Requests an asynchronous closes of the stream, releasing resources
   --  related to it. When the operation is finished Callback will be called.
   --  You can then call Glib.Input_Stream.Close_Finish to get the result of
   --  the operation.
   --  For behaviour details see Glib.Input_Stream.Close.
   --  The asynchronous methods have a default fallback that uses threads to
   --  implement asynchronicity, so they are optional for inheriting classes.
   --  However, if you override one you must override all.
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional cancellable object
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied

   function Close_Finish
      (Self   : not null access Ginput_Stream_Record;
       Result : Glib.G_Async_Result) return Boolean;
   --  Finishes closing a stream asynchronously, started from
   --  Glib.Input_Stream.Close_Async.
   --  @param Result a Glib.G_Async_Result.
   --  @return True if the stream was closed successfully.

   function Has_Pending
      (Self : not null access Ginput_Stream_Record) return Boolean;
   --  Checks if an input stream has pending actions.
   --  @return True if Stream has pending actions.

   function Is_Closed
      (Self : not null access Ginput_Stream_Record) return Boolean;
   --  Checks if an input stream is closed.
   --  @return True if the stream is closed.

   function Read
      (Self        : not null access Ginput_Stream_Record;
       Buffer      : out Guint8_Array;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Gssize;
   --  Tries to read Count bytes from the stream into the buffer starting at
   --  Buffer. Will block during this read.
   --  If count is zero returns zero and does nothing. A value of Count larger
   --  than G_MAXSSIZE will cause a G_IO_ERROR_INVALID_ARGUMENT error.
   --  On success, the number of bytes read into the buffer is returned. It is
   --  not an error if this is not the same as the requested size, as it can
   --  happen e.g. near the end of a file. Zero is returned on end of file (or
   --  if Count is zero), but never otherwise.
   --  The returned Buffer is not a nul-terminated string, it can contain nul
   --  bytes at any position, and this function doesn't nul-terminate the
   --  Buffer.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned. If an
   --  operation was partially finished when the operation was cancelled the
   --  partial result will be returned, without an error.
   --  On error -1 is returned and Error is set accordingly.
   --  @param Buffer a buffer to read data into (which should be at least
   --  count bytes long).
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @return Number of bytes read, or -1 on error, or 0 on end of file.

   function Read_All
      (Self        : not null access Ginput_Stream_Record;
       Buffer      : out Guint8_Array;
       Bytes_Read  : out Gsize;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Tries to read Count bytes from the stream into the buffer starting at
   --  Buffer. Will block during this read.
   --  This function is similar to Glib.Input_Stream.Read, except it tries to
   --  read as many bytes as requested, only stopping on an error or end of
   --  stream.
   --  On a successful read of Count bytes, or if we reached the end of the
   --  stream, True is returned, and Bytes_Read is set to the number of bytes
   --  read into Buffer.
   --  If there is an error during the operation False is returned and Error
   --  is set to indicate the error status.
   --  As a special exception to the normal conventions for functions that use
   --  Gerror.Gerror, if this function returns False (and sets Error) then
   --  Bytes_Read will be set to the number of bytes that were successfully
   --  read before the error was encountered. This functionality is only
   --  available from C. If you need it from another language then you must
   --  write your own loop around Glib.Input_Stream.Read.
   --  @param Buffer a buffer to read data into (which should be at least
   --  count bytes long).
   --  @param Bytes_Read location to store the number of bytes that was read
   --  from the stream
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @return True on success, False if there was an error

   function Read_All_Finish
      (Self       : not null access Ginput_Stream_Record;
       Result     : Glib.G_Async_Result;
       Bytes_Read : out Gsize) return Boolean;
   --  Finishes an asynchronous stream read operation started with
   --  g_input_stream_read_all_async.
   --  As a special exception to the normal conventions for functions that use
   --  Gerror.Gerror, if this function returns False (and sets Error) then
   --  Bytes_Read will be set to the number of bytes that were successfully
   --  read before the error was encountered. This functionality is only
   --  available from C. If you need it from another language then you must
   --  write your own loop around g_input_stream_read_async.
   --  Since: gtk+ 2.44
   --  @param Result a Glib.G_Async_Result
   --  @param Bytes_Read location to store the number of bytes that was read
   --  from the stream
   --  @return True on success, False if there was an error

   function Read_Bytes
      (Self        : not null access Ginput_Stream_Record;
       Count       : Gsize;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.Bytes.Gbytes;
   --  Like Glib.Input_Stream.Read, this tries to read Count bytes from the
   --  stream in a blocking fashion. However, rather than reading into a
   --  user-supplied buffer, this will create a new Glib.Bytes.Gbytes
   --  containing the data that was read. This may be easier to use from
   --  language bindings.
   --  If count is zero, returns a zero-length Glib.Bytes.Gbytes and does
   --  nothing. A value of Count larger than G_MAXSSIZE will cause a
   --  G_IO_ERROR_INVALID_ARGUMENT error.
   --  On success, a new Glib.Bytes.Gbytes is returned. It is not an error if
   --  the size of this object is not the same as the requested size, as it can
   --  happen e.g. near the end of a file. A zero-length Glib.Bytes.Gbytes is
   --  returned on end of file (or if Count is zero), but never otherwise.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned. If an
   --  operation was partially finished when the operation was cancelled the
   --  partial result will be returned, without an error.
   --  On error null is returned and Error is set accordingly.
   --  Since: gtk+ 2.34
   --  @param Count maximum number of bytes that will be read from the stream.
   --  Common values include 4096 and 8192.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @return a new Glib.Bytes.Gbytes, or null on error

   procedure Read_Bytes_Async
      (Self        : not null access Ginput_Stream_Record;
       Count       : Gsize;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Request an asynchronous read of Count bytes from the stream into a new
   --  Glib.Bytes.Gbytes. When the operation is finished Callback will be
   --  called. You can then call Glib.Input_Stream.Read_Bytes_Finish to get the
   --  result of the operation.
   --  During an async request no other sync and async calls are allowed on
   --  Stream, and will result in G_IO_ERROR_PENDING errors.
   --  A value of Count larger than G_MAXSSIZE will cause a
   --  G_IO_ERROR_INVALID_ARGUMENT error.
   --  On success, the new Glib.Bytes.Gbytes will be passed to the callback.
   --  It is not an error if this is smaller than the requested size, as it can
   --  happen e.g. near the end of a file, but generally we try to read as many
   --  bytes as requested. Zero is returned on end of file (or if Count is
   --  zero), but never otherwise.
   --  Any outstanding I/O request with higher priority (lower numerical
   --  value) will be executed before an outstanding request with lower
   --  priority. Default priority is G_PRIORITY_DEFAULT.
   --  Since: gtk+ 2.34
   --  @param Count the number of bytes that will be read from the stream
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied

   function Read_Bytes_Finish
      (Self   : not null access Ginput_Stream_Record;
       Result : Glib.G_Async_Result) return Glib.Bytes.Gbytes;
   --  Finishes an asynchronous stream read-into-Glib.Bytes.Gbytes operation.
   --  Since: gtk+ 2.34
   --  @param Result a Glib.G_Async_Result.
   --  @return the newly-allocated Glib.Bytes.Gbytes, or null on error

   function Read_Finish
      (Self   : not null access Ginput_Stream_Record;
       Result : Glib.G_Async_Result) return Gssize;
   --  Finishes an asynchronous stream read operation.
   --  @param Result a Glib.G_Async_Result.
   --  @return number of bytes read in, or -1 on error, or 0 on end of file.

   function Set_Pending
      (Self : not null access Ginput_Stream_Record) return Boolean;
   --  Sets Stream to have actions pending. If the pending flag is already set
   --  or Stream is closed, it will return False and set Error.
   --  @return True if pending was previously unset and is now set.

   function Skip
      (Self        : not null access Ginput_Stream_Record;
       Count       : Gsize;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Gssize;
   --  Tries to skip Count bytes from the stream. Will block during the
   --  operation.
   --  This is identical to Glib.Input_Stream.Read, from a behaviour
   --  standpoint, but the bytes that are skipped are not returned to the user.
   --  Some streams have an implementation that is more efficient than reading
   --  the data.
   --  This function is optional for inherited classes, as the default
   --  implementation emulates it using read.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned. If an
   --  operation was partially finished when the operation was cancelled the
   --  partial result will be returned, without an error.
   --  @param Count the number of bytes that will be skipped from the stream
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @return Number of bytes skipped, or -1 on error

   procedure Skip_Async
      (Self        : not null access Ginput_Stream_Record;
       Count       : Gsize;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Request an asynchronous skip of Count bytes from the stream. When the
   --  operation is finished Callback will be called. You can then call
   --  Glib.Input_Stream.Skip_Finish to get the result of the operation.
   --  During an async request no other sync and async calls are allowed, and
   --  will result in G_IO_ERROR_PENDING errors.
   --  A value of Count larger than G_MAXSSIZE will cause a
   --  G_IO_ERROR_INVALID_ARGUMENT error.
   --  On success, the number of bytes skipped will be passed to the callback.
   --  It is not an error if this is not the same as the requested size, as it
   --  can happen e.g. near the end of a file, but generally we try to skip as
   --  many bytes as requested. Zero is returned on end of file (or if Count is
   --  zero), but never otherwise.
   --  Any outstanding i/o request with higher priority (lower numerical
   --  value) will be executed before an outstanding request with lower
   --  priority. Default priority is G_PRIORITY_DEFAULT.
   --  The asynchronous methods have a default fallback that uses threads to
   --  implement asynchronicity, so they are optional for inheriting classes.
   --  However, if you override one, you must override all.
   --  @param Count the number of bytes that will be skipped from the stream
   --  @param Io_Priority the [I/O priority][io-priority] of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied

   function Skip_Finish
      (Self   : not null access Ginput_Stream_Record;
       Result : Glib.G_Async_Result) return Gssize;
   --  Finishes a stream skip operation.
   --  @param Result a Glib.G_Async_Result.
   --  @return the size of the bytes skipped, or `-1` on error.

end Glib.Input_Stream;
