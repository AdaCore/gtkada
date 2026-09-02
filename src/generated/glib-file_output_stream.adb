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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Glib.File_Output_Stream is

   procedure C_G_File_Output_Stream_Query_Info_Async
      (Self        : System.Address;
       Attributes  : Gtkada.Types.Chars_Ptr;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_File_Output_Stream_Query_Info_Async, "g_file_output_stream_query_info_async");
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

   package Type_Conversion_Gfile_Output_Stream is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gfile_Output_Stream_Record);
   pragma Unreferenced (Type_Conversion_Gfile_Output_Stream);

   --------------
   -- Get_Etag --
   --------------

   function Get_Etag
      (Self : not null access Gfile_Output_Stream_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_output_stream_get_etag");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Etag;

   ----------------
   -- Query_Info --
   ----------------

   function Query_Info
      (Self        : not null access Gfile_Output_Stream_Record;
       Attributes  : UTF8_String;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.File_Info.Gfile_Info
   is
      function Internal
         (Self        : System.Address;
          Attributes  : Gtkada.Types.Chars_Ptr;
          Cancellable : System.Address) return System.Address;
      pragma Import (C, Internal, "g_file_output_stream_query_info");
      Tmp_Attributes  : Gtkada.Types.Chars_Ptr := New_String (Attributes);
      Stub_Gfile_Info : Glib.File_Info.Gfile_Info_Record;
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Attributes, Get_Object_Or_Null (GObject (Cancellable)));
      Free (Tmp_Attributes);
      return Glib.File_Info.Gfile_Info (Get_User_Data (Tmp_Return, Stub_Gfile_Info));
   end Query_Info;

   ----------------------
   -- Query_Info_Async --
   ----------------------

   procedure Query_Info_Async
      (Self        : not null access Gfile_Output_Stream_Record;
       Attributes  : UTF8_String;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
      Tmp_Attributes : Gtkada.Types.Chars_Ptr := New_String (Attributes);
   begin
      if Callback = null then
         C_G_File_Output_Stream_Query_Info_Async (Get_Object (Self), Tmp_Attributes, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
         Free (Tmp_Attributes);
      else
         C_G_File_Output_Stream_Query_Info_Async (Get_Object (Self), Tmp_Attributes, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
         Free (Tmp_Attributes);
      end if;
   end Query_Info_Async;

   -----------------------
   -- Query_Info_Finish --
   -----------------------

   function Query_Info_Finish
      (Self   : not null access Gfile_Output_Stream_Record;
       Result : Glib.G_Async_Result) return Glib.File_Info.Gfile_Info
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return System.Address;
      pragma Import (C, Internal, "g_file_output_stream_query_info_finish");
      Stub_Gfile_Info : Glib.File_Info.Gfile_Info_Record;
   begin
      return Glib.File_Info.Gfile_Info (Get_User_Data (Internal (Get_Object (Self), Result), Stub_Gfile_Info));
   end Query_Info_Finish;

end Glib.File_Output_Stream;
