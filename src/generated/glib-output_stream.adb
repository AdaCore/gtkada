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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Glib.Output_Stream is

   procedure C_G_Output_Stream_Close_Async
      (Self        : System.Address;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_Output_Stream_Close_Async, "g_output_stream_close_async");
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
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_Output_Stream_Flush_Async
      (Self        : System.Address;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_Output_Stream_Flush_Async, "g_output_stream_flush_async");
   --  Forces an asynchronous write of all user-space buffered data for the
   --  given Stream. For behaviour details see Glib.Output_Stream.Flush.
   --  When the operation is finished Callback will be called. You can then
   --  call Glib.Output_Stream.Flush_Finish to get the result of the operation.
   --  @param Io_Priority the io priority of the request.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_Output_Stream_Splice_Async
      (Self        : System.Address;
       Source      : System.Address;
       Flags       : Output_Stream_Splice_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_Output_Stream_Splice_Async, "g_output_stream_splice_async");
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
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_Output_Stream_Write_All_Async
      (Self        : System.Address;
       Buffer      : System.Address;
       Count       : Gsize;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_Output_Stream_Write_All_Async, "g_output_stream_write_all_async");
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
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_Output_Stream_Write_Async
      (Self        : System.Address;
       Buffer      : System.Address;
       Count       : Gsize;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_Output_Stream_Write_Async, "g_output_stream_write_async");
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
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_Output_Stream_Write_Bytes_Async
      (Self        : System.Address;
       Bytes       : System.Address;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_Output_Stream_Write_Bytes_Async, "g_output_stream_write_bytes_async");
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

   package Type_Conversion_Goutput_Stream is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Goutput_Stream_Record);
   pragma Unreferenced (Type_Conversion_Goutput_Stream);

   -------------------
   -- Clear_Pending --
   -------------------

   procedure Clear_Pending (Self : not null access Goutput_Stream_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_output_stream_clear_pending");
   begin
      Internal (Get_Object (Self));
   end Clear_Pending;

   -----------
   -- Close --
   -----------

   function Close
      (Self        : not null access Goutput_Stream_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : System.Address;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_output_stream_close");
   begin
      return Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Cancellable))) /= 0;
   end Close;

   -----------------
   -- Close_Async --
   -----------------

   procedure Close_Async
      (Self        : not null access Goutput_Stream_Record;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Output_Stream_Close_Async (Get_Object (Self), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Output_Stream_Close_Async (Get_Object (Self), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Close_Async;

   ------------------
   -- Close_Finish --
   ------------------

   function Close_Finish
      (Self   : not null access Goutput_Stream_Record;
       Result : Glib.G_Async_Result) return Boolean
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Glib.Gboolean;
      pragma Import (C, Internal, "g_output_stream_close_finish");
   begin
      return Internal (Get_Object (Self), Result) /= 0;
   end Close_Finish;

   -----------
   -- Flush --
   -----------

   function Flush
      (Self        : not null access Goutput_Stream_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : System.Address;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_output_stream_flush");
   begin
      return Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Cancellable))) /= 0;
   end Flush;

   -----------------
   -- Flush_Async --
   -----------------

   procedure Flush_Async
      (Self        : not null access Goutput_Stream_Record;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Output_Stream_Flush_Async (Get_Object (Self), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Output_Stream_Flush_Async (Get_Object (Self), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Flush_Async;

   ------------------
   -- Flush_Finish --
   ------------------

   function Flush_Finish
      (Self   : not null access Goutput_Stream_Record;
       Result : Glib.G_Async_Result) return Boolean
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Glib.Gboolean;
      pragma Import (C, Internal, "g_output_stream_flush_finish");
   begin
      return Internal (Get_Object (Self), Result) /= 0;
   end Flush_Finish;

   -----------------
   -- Has_Pending --
   -----------------

   function Has_Pending
      (Self : not null access Goutput_Stream_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_output_stream_has_pending");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Has_Pending;

   ---------------
   -- Is_Closed --
   ---------------

   function Is_Closed
      (Self : not null access Goutput_Stream_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_output_stream_is_closed");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Closed;

   ----------------
   -- Is_Closing --
   ----------------

   function Is_Closing
      (Self : not null access Goutput_Stream_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_output_stream_is_closing");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Closing;

   -----------------
   -- Set_Pending --
   -----------------

   function Set_Pending
      (Self : not null access Goutput_Stream_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_output_stream_set_pending");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Set_Pending;

   ------------
   -- Splice --
   ------------

   function Splice
      (Self        : not null access Goutput_Stream_Record;
       Source      : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Flags       : Output_Stream_Splice_Flags;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Gssize
   is
      function Internal
         (Self        : System.Address;
          Source      : System.Address;
          Flags       : Output_Stream_Splice_Flags;
          Cancellable : System.Address) return Gssize;
      pragma Import (C, Internal, "g_output_stream_splice");
   begin
      return Internal (Get_Object (Self), Get_Object (Source), Flags, Get_Object_Or_Null (GObject (Cancellable)));
   end Splice;

   ------------------
   -- Splice_Async --
   ------------------

   procedure Splice_Async
      (Self        : not null access Goutput_Stream_Record;
       Source      : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Flags       : Output_Stream_Splice_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Output_Stream_Splice_Async (Get_Object (Self), Get_Object (Source), Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Output_Stream_Splice_Async (Get_Object (Self), Get_Object (Source), Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Splice_Async;

   -------------------
   -- Splice_Finish --
   -------------------

   function Splice_Finish
      (Self   : not null access Goutput_Stream_Record;
       Result : Glib.G_Async_Result) return Gssize
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Gssize;
      pragma Import (C, Internal, "g_output_stream_splice_finish");
   begin
      return Internal (Get_Object (Self), Result);
   end Splice_Finish;

   -----------
   -- Write --
   -----------

   function Write
      (Self        : not null access Goutput_Stream_Record;
       Buffer      : Guint8_Array;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Gssize
   is
      function Internal
         (Self        : System.Address;
          Buffer      : System.Address;
          Count       : Gsize;
          Cancellable : System.Address) return Gssize;
      pragma Import (C, Internal, "g_output_stream_write");
   begin
      return Internal (Get_Object (Self), Buffer'Address, Buffer'Length, Get_Object_Or_Null (GObject (Cancellable)));
   end Write;

   ---------------
   -- Write_All --
   ---------------

   function Write_All
      (Self          : not null access Goutput_Stream_Record;
       Buffer        : Guint8_Array;
       Bytes_Written : access Gsize := null;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self          : System.Address;
          Buffer        : System.Address;
          Count         : Gsize;
          Bytes_Written : access Gsize;
          Cancellable   : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_output_stream_write_all");
   begin
      return Internal (Get_Object (Self), Buffer'Address, Buffer'Length, Bytes_Written, Get_Object_Or_Null (GObject (Cancellable))) /= 0;
   end Write_All;

   ---------------------
   -- Write_All_Async --
   ---------------------

   procedure Write_All_Async
      (Self        : not null access Goutput_Stream_Record;
       Buffer      : Guint8_Array;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Output_Stream_Write_All_Async (Get_Object (Self), Buffer'Address, Buffer'Length, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Output_Stream_Write_All_Async (Get_Object (Self), Buffer'Address, Buffer'Length, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Write_All_Async;

   ----------------------
   -- Write_All_Finish --
   ----------------------

   function Write_All_Finish
      (Self          : not null access Goutput_Stream_Record;
       Result        : Glib.G_Async_Result;
       Bytes_Written : access Gsize := null) return Boolean
   is
      function Internal
         (Self          : System.Address;
          Result        : Glib.G_Async_Result;
          Bytes_Written : access Gsize) return Glib.Gboolean;
      pragma Import (C, Internal, "g_output_stream_write_all_finish");
   begin
      return Internal (Get_Object (Self), Result, Bytes_Written) /= 0;
   end Write_All_Finish;

   -----------------
   -- Write_Async --
   -----------------

   procedure Write_Async
      (Self        : not null access Goutput_Stream_Record;
       Buffer      : Guint8_Array;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Output_Stream_Write_Async (Get_Object (Self), Buffer'Address, Buffer'Length, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Output_Stream_Write_Async (Get_Object (Self), Buffer'Address, Buffer'Length, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Write_Async;

   -----------------
   -- Write_Bytes --
   -----------------

   function Write_Bytes
      (Self        : not null access Goutput_Stream_Record;
       Bytes       : Glib.Bytes.Gbytes;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Gssize
   is
      function Internal
         (Self        : System.Address;
          Bytes       : System.Address;
          Cancellable : System.Address) return Gssize;
      pragma Import (C, Internal, "g_output_stream_write_bytes");
   begin
      return Internal (Get_Object (Self), Get_Object (Bytes), Get_Object_Or_Null (GObject (Cancellable)));
   end Write_Bytes;

   -----------------------
   -- Write_Bytes_Async --
   -----------------------

   procedure Write_Bytes_Async
      (Self        : not null access Goutput_Stream_Record;
       Bytes       : Glib.Bytes.Gbytes;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Output_Stream_Write_Bytes_Async (Get_Object (Self), Get_Object (Bytes), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Output_Stream_Write_Bytes_Async (Get_Object (Self), Get_Object (Bytes), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Write_Bytes_Async;

   ------------------------
   -- Write_Bytes_Finish --
   ------------------------

   function Write_Bytes_Finish
      (Self   : not null access Goutput_Stream_Record;
       Result : Glib.G_Async_Result) return Gssize
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Gssize;
      pragma Import (C, Internal, "g_output_stream_write_bytes_finish");
   begin
      return Internal (Get_Object (Self), Result);
   end Write_Bytes_Finish;

   ------------------
   -- Write_Finish --
   ------------------

   function Write_Finish
      (Self   : not null access Goutput_Stream_Record;
       Result : Glib.G_Async_Result) return Gssize
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Gssize;
      pragma Import (C, Internal, "g_output_stream_write_finish");
   begin
      return Internal (Get_Object (Self), Result);
   end Write_Finish;

   -----------------------
   -- Writev_All_Finish --
   -----------------------

   function Writev_All_Finish
      (Self          : not null access Goutput_Stream_Record;
       Result        : Glib.G_Async_Result;
       Bytes_Written : access Gsize := null) return Boolean
   is
      function Internal
         (Self          : System.Address;
          Result        : Glib.G_Async_Result;
          Bytes_Written : access Gsize) return Glib.Gboolean;
      pragma Import (C, Internal, "g_output_stream_writev_all_finish");
   begin
      return Internal (Get_Object (Self), Result, Bytes_Written) /= 0;
   end Writev_All_Finish;

   -------------------
   -- Writev_Finish --
   -------------------

   function Writev_Finish
      (Self          : not null access Goutput_Stream_Record;
       Result        : Glib.G_Async_Result;
       Bytes_Written : access Gsize := null) return Boolean
   is
      function Internal
         (Self          : System.Address;
          Result        : Glib.G_Async_Result;
          Bytes_Written : access Gsize) return Glib.Gboolean;
      pragma Import (C, Internal, "g_output_stream_writev_finish");
   begin
      return Internal (Get_Object (Self), Result, Bytes_Written) /= 0;
   end Writev_Finish;

end Glib.Output_Stream;
