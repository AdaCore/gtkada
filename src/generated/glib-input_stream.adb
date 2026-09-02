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

package body Glib.Input_Stream is

   procedure C_G_Input_Stream_Close_Async
      (Self        : System.Address;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_Input_Stream_Close_Async, "g_input_stream_close_async");
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
   --  @param User_Data the data to pass to callback function

   procedure C_G_Input_Stream_Read_Bytes_Async
      (Self        : System.Address;
       Count       : Gsize;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_Input_Stream_Read_Bytes_Async, "g_input_stream_read_bytes_async");
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
   --  @param User_Data the data to pass to callback function

   procedure C_G_Input_Stream_Skip_Async
      (Self        : System.Address;
       Count       : Gsize;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_Input_Stream_Skip_Async, "g_input_stream_skip_async");
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

   package Type_Conversion_Ginput_Stream is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Ginput_Stream_Record);
   pragma Unreferenced (Type_Conversion_Ginput_Stream);

   -------------------
   -- Clear_Pending --
   -------------------

   procedure Clear_Pending (Self : not null access Ginput_Stream_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_input_stream_clear_pending");
   begin
      Internal (Get_Object (Self));
   end Clear_Pending;

   -----------
   -- Close --
   -----------

   function Close
      (Self        : not null access Ginput_Stream_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : System.Address;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_input_stream_close");
   begin
      return Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Cancellable))) /= 0;
   end Close;

   -----------------
   -- Close_Async --
   -----------------

   procedure Close_Async
      (Self        : not null access Ginput_Stream_Record;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Input_Stream_Close_Async (Get_Object (Self), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Input_Stream_Close_Async (Get_Object (Self), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Close_Async;

   ------------------
   -- Close_Finish --
   ------------------

   function Close_Finish
      (Self   : not null access Ginput_Stream_Record;
       Result : Glib.G_Async_Result) return Boolean
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Glib.Gboolean;
      pragma Import (C, Internal, "g_input_stream_close_finish");
   begin
      return Internal (Get_Object (Self), Result) /= 0;
   end Close_Finish;

   -----------------
   -- Has_Pending --
   -----------------

   function Has_Pending
      (Self : not null access Ginput_Stream_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_input_stream_has_pending");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Has_Pending;

   ---------------
   -- Is_Closed --
   ---------------

   function Is_Closed
      (Self : not null access Ginput_Stream_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_input_stream_is_closed");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Closed;

   ----------
   -- Read --
   ----------

   function Read
      (Self        : not null access Ginput_Stream_Record;
       Buffer      : out Guint8_Array;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Gssize
   is
      function Internal
         (Self        : System.Address;
          Buffer      : System.Address;
          Count       : Gsize;
          Cancellable : System.Address) return Gssize;
      pragma Import (C, Internal, "g_input_stream_read");
   begin
      return Internal (Get_Object (Self), Buffer'Address, Buffer'Length, Get_Object_Or_Null (GObject (Cancellable)));
   end Read;

   --------------
   -- Read_All --
   --------------

   function Read_All
      (Self        : not null access Ginput_Stream_Record;
       Buffer      : out Guint8_Array;
       Bytes_Read  : out Gsize;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self           : System.Address;
          Buffer         : System.Address;
          Count          : Gsize;
          Acc_Bytes_Read : access Gsize;
          Cancellable    : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_input_stream_read_all");
      Acc_Bytes_Read : aliased Gsize;
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Buffer'Address, Buffer'Length, Acc_Bytes_Read'Access, Get_Object_Or_Null (GObject (Cancellable)));
      Bytes_Read := Acc_Bytes_Read;
      return Tmp_Return /= 0;
   end Read_All;

   ---------------------
   -- Read_All_Finish --
   ---------------------

   function Read_All_Finish
      (Self       : not null access Ginput_Stream_Record;
       Result     : Glib.G_Async_Result;
       Bytes_Read : out Gsize) return Boolean
   is
      function Internal
         (Self           : System.Address;
          Result         : Glib.G_Async_Result;
          Acc_Bytes_Read : access Gsize) return Glib.Gboolean;
      pragma Import (C, Internal, "g_input_stream_read_all_finish");
      Acc_Bytes_Read : aliased Gsize;
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Result, Acc_Bytes_Read'Access);
      Bytes_Read := Acc_Bytes_Read;
      return Tmp_Return /= 0;
   end Read_All_Finish;

   ----------------
   -- Read_Bytes --
   ----------------

   function Read_Bytes
      (Self        : not null access Ginput_Stream_Record;
       Count       : Gsize;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.Bytes.Gbytes
   is
      function Internal
         (Self        : System.Address;
          Count       : Gsize;
          Cancellable : System.Address) return System.Address;
      pragma Import (C, Internal, "g_input_stream_read_bytes");
   begin
      return From_Object (Internal (Get_Object (Self), Count, Get_Object_Or_Null (GObject (Cancellable))));
   end Read_Bytes;

   ----------------------
   -- Read_Bytes_Async --
   ----------------------

   procedure Read_Bytes_Async
      (Self        : not null access Ginput_Stream_Record;
       Count       : Gsize;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Input_Stream_Read_Bytes_Async (Get_Object (Self), Count, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Input_Stream_Read_Bytes_Async (Get_Object (Self), Count, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Read_Bytes_Async;

   -----------------------
   -- Read_Bytes_Finish --
   -----------------------

   function Read_Bytes_Finish
      (Self   : not null access Ginput_Stream_Record;
       Result : Glib.G_Async_Result) return Glib.Bytes.Gbytes
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return System.Address;
      pragma Import (C, Internal, "g_input_stream_read_bytes_finish");
   begin
      return From_Object (Internal (Get_Object (Self), Result));
   end Read_Bytes_Finish;

   -----------------
   -- Read_Finish --
   -----------------

   function Read_Finish
      (Self   : not null access Ginput_Stream_Record;
       Result : Glib.G_Async_Result) return Gssize
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Gssize;
      pragma Import (C, Internal, "g_input_stream_read_finish");
   begin
      return Internal (Get_Object (Self), Result);
   end Read_Finish;

   -----------------
   -- Set_Pending --
   -----------------

   function Set_Pending
      (Self : not null access Ginput_Stream_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_input_stream_set_pending");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Set_Pending;

   ----------
   -- Skip --
   ----------

   function Skip
      (Self        : not null access Ginput_Stream_Record;
       Count       : Gsize;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Gssize
   is
      function Internal
         (Self        : System.Address;
          Count       : Gsize;
          Cancellable : System.Address) return Gssize;
      pragma Import (C, Internal, "g_input_stream_skip");
   begin
      return Internal (Get_Object (Self), Count, Get_Object_Or_Null (GObject (Cancellable)));
   end Skip;

   ----------------
   -- Skip_Async --
   ----------------

   procedure Skip_Async
      (Self        : not null access Ginput_Stream_Record;
       Count       : Gsize;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Input_Stream_Skip_Async (Get_Object (Self), Count, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Input_Stream_Skip_Async (Get_Object (Self), Count, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Skip_Async;

   -----------------
   -- Skip_Finish --
   -----------------

   function Skip_Finish
      (Self   : not null access Ginput_Stream_Record;
       Result : Glib.G_Async_Result) return Gssize
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Gssize;
      pragma Import (C, Internal, "g_input_stream_skip_finish");
   begin
      return Internal (Get_Object (Self), Result);
   end Skip_Finish;

end Glib.Input_Stream;
