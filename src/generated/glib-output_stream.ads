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

--  Glib.Output_Stream.Goutput_Stream has functions to write to a stream
--  (g_output_stream_write), to close a stream (g_output_stream_close) and to
--  flush pending writes (g_output_stream_flush).
--
--  To copy the content of an input stream to an output stream without
--  manually handling the reads and writes, use Glib.Output_Stream.Splice.
--
--  See the documentation for Giostream.Giostream for details of thread safety
--  of streaming APIs.
--
--  All of these functions have async variants too.

pragma Warnings (Off, "*is already use-visible*");
with Glib.Bytes;              use Glib.Bytes;
with Glib.Cancellable;        use Glib.Cancellable;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Input_Stream;       use Glib.Input_Stream;
with Glib.Object;             use Glib.Object;

package Glib.Output_Stream is

   type Goutput_Stream_Record is new GObject_Record with null record;
   type Goutput_Stream is access all Goutput_Stream_Record'Class;

   type Output_Stream_Splice_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Output_Stream_Splice_Flags);
   --  GOutputStreamSpliceFlags determine how streams should be spliced.

   G_Output_Stream_Splice_None : constant Output_Stream_Splice_Flags := 0;
   G_Output_Stream_Splice_Close_Source : constant Output_Stream_Splice_Flags := 1;
   G_Output_Stream_Splice_Close_Target : constant Output_Stream_Splice_Flags := 2;

   ---------------
   -- Callbacks --
   ---------------

   type Gasync_Ready_Callback is access procedure
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);
   --  Type definition for a function that will be called back when an
   --  asynchronous operation within GIO has been completed.
   --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
   --  invoked in a later iteration of the [thread-default main
   --  context][g-main-context-push-thread-default] where the Gtask.Gtask was
   --  created. All other users of Gasync_Ready_Callback must likewise call it
   --  asynchronously in a later iteration of the main context.
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Output_Stream_Splice_Flags_Properties is
      new Generic_Internal_Discrete_Property (Output_Stream_Splice_Flags);
   type Property_Output_Stream_Splice_Flags is new Output_Stream_Splice_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_output_stream_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clear_Pending (Self : not null access Goutput_Stream_Record);
   --  Clears the pending flag on Stream.

   function Close
      (Self        : not null access Goutput_Stream_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Closes the stream, releasing resources related to it.
   --  Once the stream is closed, all other operations will return
   --  G_IO_ERROR_CLOSED. Closing a stream multiple times will not return an
   --  error.
   --  Closing a stream will automatically flush any outstanding buffers in
   --  the stream.
   --  Streams will be automatically closed when the last reference is
   --  dropped, but you might want to call this function to make sure resources
   --  are released as early as possible.
   --  Some streams might keep the backing store of the stream (e.g. a file
   --  descriptor) open after the stream is closed. See the documentation for
   --  the individual stream for details.
   --  On failure the first error that happened will be reported, but the
   --  close operation will finish as much as possible. A stream that failed to
   --  close will still return G_IO_ERROR_CLOSED for all operations. Still, it
   --  is important to check and report the error to the user, otherwise there
   --  might be a loss of data as all data might not be written.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  Cancelling a close will still leave the stream closed, but there some
   --  streams can use a faster close that doesn't block to e.g. check errors.
   --  On cancellation (as with any error) there is no guarantee that all
   --  written data will reach the target.
   --  @param Cancellable optional cancellable object
   --  @return True on success, False on failure

   procedure Close_Async
      (Self        : not null access Goutput_Stream_Record;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Requests an asynchronous close of the stream, releasing resources
   --  related to it. When the operation is finished Callback will be called.
   --  You can then call Glib.Output_Stream.Close_Finish to get the result of
   --  the operation.
   --  For behaviour details see Glib.Output_Stream.Close.
   --  The asynchronous methods have a default fallback that uses threads to
   --  implement asynchronicity, so they are optional for inheriting classes.
   --  However, if you override one you must override all.
   --  @param Io_Priority the io priority of the request.
   --  @param Cancellable optional cancellable object
   --  @param Callback callback to call when the request is satisfied

   function Close_Finish
      (Self   : not null access Goutput_Stream_Record;
       Result : Glib.G_Async_Result) return Boolean;
   --  Closes an output stream.
   --  @param Result a Glib.G_Async_Result.
   --  @return True if stream was successfully closed, False otherwise.

   function Flush
      (Self        : not null access Goutput_Stream_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Forces a write of all user-space buffered data for the given Stream.
   --  Will block during the operation. Closing the stream will implicitly
   --  cause a flush.
   --  This function is optional for inherited classes.
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  @param Cancellable optional cancellable object
   --  @return True on success, False on error

   procedure Flush_Async
      (Self        : not null access Goutput_Stream_Record;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Forces an asynchronous write of all user-space buffered data for the
   --  given Stream. For behaviour details see Glib.Output_Stream.Flush.
   --  When the operation is finished Callback will be called. You can then
   --  call Glib.Output_Stream.Flush_Finish to get the result of the operation.
   --  @param Io_Priority the io priority of the request.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied

   function Flush_Finish
      (Self   : not null access Goutput_Stream_Record;
       Result : Glib.G_Async_Result) return Boolean;
   --  Finishes flushing an output stream.
   --  @param Result a GAsyncResult.
   --  @return True if flush operation succeeded, False otherwise.

   function Has_Pending
      (Self : not null access Goutput_Stream_Record) return Boolean;
   --  Checks if an output stream has pending actions.
   --  @return True if Stream has pending actions.

   function Is_Closed
      (Self : not null access Goutput_Stream_Record) return Boolean;
   --  Checks if an output stream has already been closed.
   --  @return True if Stream is closed. False otherwise.

   function Is_Closing
      (Self : not null access Goutput_Stream_Record) return Boolean;
   --  Checks if an output stream is being closed. This can be used inside
   --  e.g. a flush implementation to see if the flush (or other i/o operation)
   --  is called from within the closing operation.
   --  Since: gtk+ 2.24
   --  @return True if Stream is being closed. False otherwise.

   function Set_Pending
      (Self : not null access Goutput_Stream_Record) return Boolean;
   --  Sets Stream to have actions pending. If the pending flag is already set
   --  or Stream is closed, it will return False and set Error.
   --  @return True if pending was previously unset and is now set.

   function Splice
      (Self        : not null access Goutput_Stream_Record;
       Source      : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Flags       : Output_Stream_Splice_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Gssize;
   --  Splices an input stream into an output stream.
   --  @param Source a Glib.Input_Stream.Ginput_Stream.
   --  @param Flags a set of Glib.Output_Stream.Output_Stream_Splice_Flags.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @return a Gssize containing the size of the data spliced, or -1 if an
   --  error occurred. Note that if the number of bytes spliced is greater than
   --  G_MAXSSIZE, then that will be returned, and there is no way to determine
   --  the actual number of bytes spliced.

   procedure Splice_Async
      (Self        : not null access Goutput_Stream_Record;
       Source      : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Flags       : Output_Stream_Splice_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Splices a stream asynchronously. When the operation is finished
   --  Callback will be called. You can then call
   --  Glib.Output_Stream.Splice_Finish to get the result of the operation.
   --  For the synchronous, blocking version of this function, see
   --  Glib.Output_Stream.Splice.
   --  @param Source a Glib.Input_Stream.Ginput_Stream.
   --  @param Flags a set of Glib.Output_Stream.Output_Stream_Splice_Flags.
   --  @param Io_Priority the io priority of the request.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback.

   function Splice_Finish
      (Self   : not null access Goutput_Stream_Record;
       Result : Glib.G_Async_Result) return Gssize;
   --  Finishes an asynchronous stream splice operation.
   --  @param Result a Glib.G_Async_Result.
   --  @return a Gssize of the number of bytes spliced. Note that if the
   --  number of bytes spliced is greater than G_MAXSSIZE, then that will be
   --  returned, and there is no way to determine the actual number of bytes
   --  spliced.

   function Write
      (Self        : not null access Goutput_Stream_Record;
       Buffer      : Guint8_Array;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Gssize;
   --  Tries to write Count bytes from Buffer into the stream. Will block
   --  during the operation.
   --  If count is 0, returns 0 and does nothing. A value of Count larger than
   --  G_MAXSSIZE will cause a G_IO_ERROR_INVALID_ARGUMENT error.
   --  On success, the number of bytes written to the stream is returned. It
   --  is not an error if this is not the same as the requested size, as it can
   --  happen e.g. on a partial I/O error, or if there is not enough storage in
   --  the stream. All writes block until at least one byte is written or an
   --  error occurs; 0 is never returned (unless Count is 0).
   --  If Cancellable is not null, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned. If an
   --  operation was partially finished when the operation was cancelled the
   --  partial result will be returned, without an error.
   --  On error -1 is returned and Error is set accordingly.
   --  @param Buffer the buffer containing the data to write.
   --  @param Cancellable optional cancellable object
   --  @return Number of bytes written, or -1 on error

   function Write_All
      (Self          : not null access Goutput_Stream_Record;
       Buffer        : Guint8_Array;
       Bytes_Written : access Gsize := null;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Tries to write Count bytes from Buffer into the stream. Will block
   --  during the operation.
   --  This function is similar to Glib.Output_Stream.Write, except it tries
   --  to write as many bytes as requested, only stopping on an error.
   --  On a successful write of Count bytes, True is returned, and
   --  Bytes_Written is set to Count.
   --  If there is an error during the operation False is returned and Error
   --  is set to indicate the error status.
   --  As a special exception to the normal conventions for functions that use
   --  Gerror.Gerror, if this function returns False (and sets Error) then
   --  Bytes_Written will be set to the number of bytes that were successfully
   --  written before the error was encountered. This functionality is only
   --  available from C. If you need it from another language then you must
   --  write your own loop around Glib.Output_Stream.Write.
   --  @param Buffer the buffer containing the data to write.
   --  @param Bytes_Written location to store the number of bytes that was
   --  written to the stream
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @return True on success, False if there was an error

   procedure Write_All_Async
      (Self        : not null access Goutput_Stream_Record;
       Buffer      : Guint8_Array;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Request an asynchronous write of Count bytes from Buffer into the
   --  stream. When the operation is finished Callback will be called. You can
   --  then call Glib.Output_Stream.Write_All_Finish to get the result of the
   --  operation.
   --  This is the asynchronous version of Glib.Output_Stream.Write_All.
   --  Call Glib.Output_Stream.Write_All_Finish to collect the result.
   --  Any outstanding I/O request with higher priority (lower numerical
   --  value) will be executed before an outstanding request with lower
   --  priority. Default priority is G_PRIORITY_DEFAULT.
   --  Note that no copy of Buffer will be made, so it must stay valid until
   --  Callback is called.
   --  Since: gtk+ 2.44
   --  @param Buffer the buffer containing the data to write
   --  @param Io_Priority the io priority of the request
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback callback to call when the request is satisfied

   function Write_All_Finish
      (Self          : not null access Goutput_Stream_Record;
       Result        : Glib.G_Async_Result;
       Bytes_Written : access Gsize := null) return Boolean;
   --  Finishes an asynchronous stream write operation started with
   --  Glib.Output_Stream.Write_All_Async.
   --  As a special exception to the normal conventions for functions that use
   --  Gerror.Gerror, if this function returns False (and sets Error) then
   --  Bytes_Written will be set to the number of bytes that were successfully
   --  written before the error was encountered. This functionality is only
   --  available from C. If you need it from another language then you must
   --  write your own loop around Glib.Output_Stream.Write_Async.
   --  Since: gtk+ 2.44
   --  @param Result a Glib.G_Async_Result
   --  @param Bytes_Written location to store the number of bytes that was
   --  written to the stream
   --  @return True on success, False if there was an error

   procedure Write_Async
      (Self        : not null access Goutput_Stream_Record;
       Buffer      : Guint8_Array;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Request an asynchronous write of Count bytes from Buffer into the
   --  stream. When the operation is finished Callback will be called. You can
   --  then call Glib.Output_Stream.Write_Finish to get the result of the
   --  operation.
   --  During an async request no other sync and async calls are allowed, and
   --  will result in G_IO_ERROR_PENDING errors.
   --  A value of Count larger than G_MAXSSIZE will cause a
   --  G_IO_ERROR_INVALID_ARGUMENT error.
   --  On success, the number of bytes written will be passed to the Callback.
   --  It is not an error if this is not the same as the requested size, as it
   --  can happen e.g. on a partial I/O error, but generally we try to write as
   --  many bytes as requested.
   --  You are guaranteed that this method will never fail with
   --  G_IO_ERROR_WOULD_BLOCK - if Stream can't accept more data, the method
   --  will just wait until this changes.
   --  Any outstanding I/O request with higher priority (lower numerical
   --  value) will be executed before an outstanding request with lower
   --  priority. Default priority is G_PRIORITY_DEFAULT.
   --  The asynchronous methods have a default fallback that uses threads to
   --  implement asynchronicity, so they are optional for inheriting classes.
   --  However, if you override one you must override all.
   --  For the synchronous, blocking version of this function, see
   --  Glib.Output_Stream.Write.
   --  Note that no copy of Buffer will be made, so it must stay valid until
   --  Callback is called. See Glib.Output_Stream.Write_Bytes_Async for a
   --  Glib.Bytes.Gbytes version that will automatically hold a reference to
   --  the contents (without copying) for the duration of the call.
   --  @param Buffer the buffer containing the data to write.
   --  @param Io_Priority the io priority of the request.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback callback to call when the request is satisfied

   function Write_Bytes
      (Self        : not null access Goutput_Stream_Record;
       Bytes       : Glib.Bytes.Gbytes;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Gssize;
   --  A wrapper function for Glib.Output_Stream.Write which takes a
   --  Glib.Bytes.Gbytes as input. This can be more convenient for use by
   --  language bindings or in other cases where the refcounted nature of
   --  Glib.Bytes.Gbytes is helpful over a bare pointer interface.
   --  However, note that this function may still perform partial writes, just
   --  like Glib.Output_Stream.Write. If that occurs, to continue writing, you
   --  will need to create a new Glib.Bytes.Gbytes containing just the
   --  remaining bytes, using Glib.Bytes.New_From_Bytes. Passing the same
   --  Glib.Bytes.Gbytes instance multiple times potentially can result in
   --  duplicated data in the output stream.
   --  @param Bytes the Glib.Bytes.Gbytes to write
   --  @param Cancellable optional cancellable object
   --  @return Number of bytes written, or -1 on error

   procedure Write_Bytes_Async
      (Self        : not null access Goutput_Stream_Record;
       Bytes       : Glib.Bytes.Gbytes;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  This function is similar to Glib.Output_Stream.Write_Async, but takes a
   --  Glib.Bytes.Gbytes as input. Due to the refcounted nature of
   --  Glib.Bytes.Gbytes, this allows the stream to avoid taking a copy of the
   --  data.
   --  However, note that this function may still perform partial writes, just
   --  like Glib.Output_Stream.Write_Async. If that occurs, to continue
   --  writing, you will need to create a new Glib.Bytes.Gbytes containing just
   --  the remaining bytes, using Glib.Bytes.New_From_Bytes. Passing the same
   --  Glib.Bytes.Gbytes instance multiple times potentially can result in
   --  duplicated data in the output stream.
   --  For the synchronous, blocking version of this function, see
   --  Glib.Output_Stream.Write_Bytes.
   --  @param Bytes The bytes to write
   --  @param Io_Priority the io priority of the request.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback callback to call when the request is satisfied

   function Write_Bytes_Finish
      (Self   : not null access Goutput_Stream_Record;
       Result : Glib.G_Async_Result) return Gssize;
   --  Finishes a stream write-from-Glib.Bytes.Gbytes operation.
   --  @param Result a Glib.G_Async_Result.
   --  @return a Gssize containing the number of bytes written to the stream.

   function Write_Finish
      (Self   : not null access Goutput_Stream_Record;
       Result : Glib.G_Async_Result) return Gssize;
   --  Finishes a stream write operation.
   --  @param Result a Glib.G_Async_Result.
   --  @return a Gssize containing the number of bytes written to the stream.

   function Writev_All_Finish
      (Self          : not null access Goutput_Stream_Record;
       Result        : Glib.G_Async_Result;
       Bytes_Written : access Gsize := null) return Boolean;
   --  Finishes an asynchronous stream write operation started with
   --  g_output_stream_writev_all_async.
   --  As a special exception to the normal conventions for functions that use
   --  Gerror.Gerror, if this function returns False (and sets Error) then
   --  Bytes_Written will be set to the number of bytes that were successfully
   --  written before the error was encountered. This functionality is only
   --  available from C. If you need it from another language then you must
   --  write your own loop around g_output_stream_writev_async.
   --  Since: gtk+ 2.60
   --  @param Result a Glib.G_Async_Result
   --  @param Bytes_Written location to store the number of bytes that were
   --  written to the stream
   --  @return True on success, False if there was an error

   function Writev_Finish
      (Self          : not null access Goutput_Stream_Record;
       Result        : Glib.G_Async_Result;
       Bytes_Written : access Gsize := null) return Boolean;
   --  Finishes a stream writev operation.
   --  Since: gtk+ 2.60
   --  @param Result a Glib.G_Async_Result.
   --  @param Bytes_Written location to store the number of bytes that were
   --  written to the stream
   --  @return True on success, False if there was an error

end Glib.Output_Stream;
