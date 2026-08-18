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

--  GIOStream represents an object that has both read and write streams.
--  Generally the two streams act as separate input and output streams, but
--  they share some common resources and state. For instance, for seekable
--  streams, both streams may use the same position.
--
--  Examples of Glib.IO_Stream.Giostream objects are
--  Gsocket.Connection.Gsocket_Connection, which represents a two-way network
--  connection; and Glib.File_IO_Stream.Gfile_Iostream, which represents a file
--  handle opened in read-write mode.
--
--  To do the actual reading and writing you need to get the substreams with
--  Glib.IO_Stream.Get_Input_Stream and Glib.IO_Stream.Get_Output_Stream.
--
--  The Glib.IO_Stream.Giostream object owns the input and the output streams,
--  not the other way around, so keeping the substreams alive will not keep the
--  Glib.IO_Stream.Giostream object alive. If the Glib.IO_Stream.Giostream
--  object is freed it will be closed, thus closing the substreams, so even if
--  the substreams stay alive they will always return G_IO_ERROR_CLOSED for all
--  operations.
--
--  To close a stream use Glib.IO_Stream.Close which will close the common
--  stream object and also the individual substreams. You can also close the
--  substreams themselves. In most cases this only marks the substream as
--  closed, so further I/O on it fails but common state in the
--  Glib.IO_Stream.Giostream may still be open. However, some streams may
--  support "half-closed" states where one direction of the stream is actually
--  shut down.
--
--  Operations on GIOStreams cannot be started while another operation on the
--  Glib.IO_Stream.Giostream or its substreams is in progress. Specifically, an
--  application can read from the Glib.Input_Stream.Ginput_Stream and write to
--  the Glib.Output_Stream.Goutput_Stream simultaneously (either in separate
--  threads, or as asynchronous operations in the same thread), but an
--  application cannot start any Glib.IO_Stream.Giostream operation while there
--  is a Glib.IO_Stream.Giostream, Glib.Input_Stream.Ginput_Stream or
--  Glib.Output_Stream.Goutput_Stream operation in progress, and an application
--  can't start any Glib.Input_Stream.Ginput_Stream or
--  Glib.Output_Stream.Goutput_Stream operation while there is a
--  Glib.IO_Stream.Giostream operation in progress.
--
--  This is a product of individual stream operations being associated with a
--  given Gmain.Context.Gmain_Context (the thread-default context at the time
--  the operation was started), rather than entire streams being associated
--  with a single Gmain.Context.Gmain_Context.
--
--  GIO may run operations on GIOStreams from other (worker) threads, and this
--  may be exposed to application code in the behaviour of wrapper streams,
--  such as Gbuffered.Input_Stream.Gbuffered_Input_Stream or
--  Gtls.Connection.Gtls_Connection. With such wrapper APIs, application code
--  may only run operations on the base (wrapped) stream when the wrapper
--  stream is idle. Note that the semantics of such operations may not be
--  well-defined due to the state the wrapper stream leaves the base stream in
--  (though they are guaranteed not to crash).

pragma Warnings (Off, "*is already use-visible*");
with Glib.Cancellable;        use Glib.Cancellable;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Input_Stream;       use Glib.Input_Stream;
with Glib.Object;             use Glib.Object;
with Glib.Output_Stream;      use Glib.Output_Stream;
with Glib.Properties;         use Glib.Properties;

package Glib.IO_Stream is

   type Giostream_Record is new GObject_Record with null record;
   type Giostream is access all Giostream_Record'Class;

   type GIOStream_Splice_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, GIOStream_Splice_Flags);
   --  GIOStreamSpliceFlags determine how streams should be spliced.

   G_Io_Stream_Splice_None : constant GIOStream_Splice_Flags := 0;
   G_Io_Stream_Splice_Close_Stream1 : constant GIOStream_Splice_Flags := 1;
   G_Io_Stream_Splice_Close_Stream2 : constant GIOStream_Splice_Flags := 2;
   G_Io_Stream_Splice_Wait_For_Both : constant GIOStream_Splice_Flags := 4;

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

   package GIOStream_Splice_Flags_Properties is
      new Generic_Internal_Discrete_Property (GIOStream_Splice_Flags);
   type Property_GIOStream_Splice_Flags is new GIOStream_Splice_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_io_stream_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clear_Pending (Self : not null access Giostream_Record);
   --  Clears the pending flag on Stream.
   --  Since: gtk+ 2.22

   function Close
      (Self        : not null access Giostream_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean;
   --  Closes the stream, releasing resources related to it. This will also
   --  close the individual input and output streams, if they are not already
   --  closed.
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
   --  If Cancellable is not NULL, then the operation can be cancelled by
   --  triggering the cancellable object from another thread. If the operation
   --  was cancelled, the error G_IO_ERROR_CANCELLED will be returned.
   --  Cancelling a close will still leave the stream closed, but some streams
   --  can use a faster close that doesn't block to e.g. check errors.
   --  The default implementation of this method just calls close on the
   --  individual input/output streams.
   --  Since: gtk+ 2.22
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @return True on success, False on failure

   procedure Close_Async
      (Self        : not null access Giostream_Record;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Requests an asynchronous close of the stream, releasing resources
   --  related to it. When the operation is finished Callback will be called.
   --  You can then call Glib.IO_Stream.Close_Finish to get the result of the
   --  operation.
   --  For behaviour details see Glib.IO_Stream.Close.
   --  The asynchronous methods have a default fallback that uses threads to
   --  implement asynchronicity, so they are optional for inheriting classes.
   --  However, if you override one you must override all.
   --  Since: gtk+ 2.22
   --  @param Io_Priority the io priority of the request
   --  @param Cancellable optional cancellable object
   --  @param Callback callback to call when the request is satisfied

   function Close_Finish
      (Self   : not null access Giostream_Record;
       Result : Glib.G_Async_Result) return Boolean;
   --  Closes a stream.
   --  Since: gtk+ 2.22
   --  @param Result a Glib.G_Async_Result
   --  @return True if stream was successfully closed, False otherwise.

   function Get_Input_Stream
      (Self : not null access Giostream_Record)
       return Glib.Input_Stream.Ginput_Stream;
   --  Gets the input stream for this object. This is used for reading.
   --  Since: gtk+ 2.22
   --  @return a Glib.Input_Stream.Ginput_Stream, owned by the
   --  Glib.IO_Stream.Giostream. Do not free.
   --  Return has transfer-ownership='none'

   function Get_Output_Stream
      (Self : not null access Giostream_Record)
       return Glib.Output_Stream.Goutput_Stream;
   --  Gets the output stream for this object. This is used for writing.
   --  Since: gtk+ 2.22
   --  @return a Glib.Output_Stream.Goutput_Stream, owned by the
   --  Glib.IO_Stream.Giostream. Do not free.
   --  Return has transfer-ownership='none'

   function Has_Pending
      (Self : not null access Giostream_Record) return Boolean;
   --  Checks if a stream has pending actions.
   --  Since: gtk+ 2.22
   --  @return True if Stream has pending actions.

   function Is_Closed
      (Self : not null access Giostream_Record) return Boolean;
   --  Checks if a stream is closed.
   --  Since: gtk+ 2.22
   --  @return True if the stream is closed.

   function Set_Pending
      (Self : not null access Giostream_Record) return Boolean;
   --  Sets Stream to have actions pending. If the pending flag is already set
   --  or Stream is closed, it will return False and set Error.
   --  Since: gtk+ 2.22
   --  @return True if pending was previously unset and is now set.

   procedure Splice_Async
      (Self        : not null access Giostream_Record;
       Stream2     : not null access Giostream_Record'Class;
       Flags       : GIOStream_Splice_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Asynchronously splice the output stream of Stream1 to the input stream
   --  of Stream2, and splice the output stream of Stream2 to the input stream
   --  of Stream1.
   --  When the operation is finished Callback will be called. You can then
   --  call Glib.IO_Stream.Splice_Finish to get the result of the operation.
   --  Since: gtk+ 2.28
   --  @param Stream2 a Glib.IO_Stream.Giostream.
   --  @param Flags a set of Glib.IO_Stream.GIOStream_Splice_Flags.
   --  @param Io_Priority the io priority of the request.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback.

   ---------------
   -- Functions --
   ---------------

   function Splice_Finish (Result : Glib.G_Async_Result) return Boolean;
   --  Finishes an asynchronous io stream splice operation.
   --  Since: gtk+ 2.28
   --  @param Result a Glib.G_Async_Result.
   --  @return True on success, False otherwise.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Closed_Property : constant Glib.Properties.Property_Boolean;

   Input_Stream_Property : constant Glib.Properties.Property_Object;
   --  Type: Glib.Input_Stream.Ginput_Stream

   Output_Stream_Property : constant Glib.Properties.Property_Object;
   --  Type: Glib.Output_Stream.Goutput_Stream

private
   Output_Stream_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("output-stream");
   Input_Stream_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("input-stream");
   Closed_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("closed");
end Glib.IO_Stream;
