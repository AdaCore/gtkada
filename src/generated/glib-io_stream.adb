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

package body Glib.IO_Stream is

   procedure C_G_Io_Stream_Close_Async
      (Self        : System.Address;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_Io_Stream_Close_Async, "g_io_stream_close_async");
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
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_G_Io_Stream_Splice_Async
      (Self        : System.Address;
       Stream2     : System.Address;
       Flags       : GIOStream_Splice_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_Io_Stream_Splice_Async, "g_io_stream_splice_async");
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

   package Type_Conversion_Giostream is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Giostream_Record);
   pragma Unreferenced (Type_Conversion_Giostream);

   -------------------
   -- Clear_Pending --
   -------------------

   procedure Clear_Pending (Self : not null access Giostream_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_io_stream_clear_pending");
   begin
      Internal (Get_Object (Self));
   end Clear_Pending;

   -----------
   -- Close --
   -----------

   function Close
      (Self        : not null access Giostream_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Boolean
   is
      function Internal
         (Self        : System.Address;
          Cancellable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_io_stream_close");
   begin
      return Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Cancellable))) /= 0;
   end Close;

   -----------------
   -- Close_Async --
   -----------------

   procedure Close_Async
      (Self        : not null access Giostream_Record;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Io_Stream_Close_Async (Get_Object (Self), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Io_Stream_Close_Async (Get_Object (Self), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Close_Async;

   ------------------
   -- Close_Finish --
   ------------------

   function Close_Finish
      (Self   : not null access Giostream_Record;
       Result : Glib.G_Async_Result) return Boolean
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Glib.Gboolean;
      pragma Import (C, Internal, "g_io_stream_close_finish");
   begin
      return Internal (Get_Object (Self), Result) /= 0;
   end Close_Finish;

   ----------------------
   -- Get_Input_Stream --
   ----------------------

   function Get_Input_Stream
      (Self : not null access Giostream_Record)
       return Glib.Input_Stream.Ginput_Stream
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_io_stream_get_input_stream");
      Stub_Ginput_Stream : Glib.Input_Stream.Ginput_Stream_Record;
   begin
      return Glib.Input_Stream.Ginput_Stream (Get_User_Data (Internal (Get_Object (Self)), Stub_Ginput_Stream));
   end Get_Input_Stream;

   -----------------------
   -- Get_Output_Stream --
   -----------------------

   function Get_Output_Stream
      (Self : not null access Giostream_Record)
       return Glib.Output_Stream.Goutput_Stream
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_io_stream_get_output_stream");
      Stub_Goutput_Stream : Glib.Output_Stream.Goutput_Stream_Record;
   begin
      return Glib.Output_Stream.Goutput_Stream (Get_User_Data (Internal (Get_Object (Self)), Stub_Goutput_Stream));
   end Get_Output_Stream;

   -----------------
   -- Has_Pending --
   -----------------

   function Has_Pending
      (Self : not null access Giostream_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_io_stream_has_pending");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Has_Pending;

   ---------------
   -- Is_Closed --
   ---------------

   function Is_Closed
      (Self : not null access Giostream_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_io_stream_is_closed");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Closed;

   -----------------
   -- Set_Pending --
   -----------------

   function Set_Pending
      (Self : not null access Giostream_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_io_stream_set_pending");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Set_Pending;

   ------------------
   -- Splice_Async --
   ------------------

   procedure Splice_Async
      (Self        : not null access Giostream_Record;
       Stream2     : not null access Giostream_Record'Class;
       Flags       : GIOStream_Splice_Flags;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Io_Stream_Splice_Async (Get_Object (Self), Get_Object (Stream2), Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Io_Stream_Splice_Async (Get_Object (Self), Get_Object (Stream2), Flags, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Splice_Async;

   -------------------
   -- Splice_Finish --
   -------------------

   function Splice_Finish (Result : Glib.G_Async_Result) return Boolean is
      function Internal (Result : Glib.G_Async_Result) return Glib.Gboolean;
      pragma Import (C, Internal, "g_io_stream_splice_finish");
   begin
      return Internal (Result) /= 0;
   end Splice_Finish;

end Glib.IO_Stream;
