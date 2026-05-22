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
with Gdk.Display;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
with System;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gdk.Clipboard is

   procedure C_Gdk_Clipboard_Read_Async
      (Self        : System.Address;
       Mime_Types  : Gtkada.Types.chars_ptr_array;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gdk_Clipboard_Read_Async, "gdk_clipboard_read_async");
   --  Asynchronously requests an input stream to read the Clipboard's
   --  contents from.
   --  The clipboard will choose the most suitable mime type from the given
   --  list to fulfill the request, preferring the ones listed first.
   --  @param Mime_Types a null-terminated array of mime types to choose from
   --  @param Io_Priority the I/O priority of the request
   --  @param Cancellable optional `GCancellable` object
   --  @param Callback callback to call when the request is satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_Gdk_Clipboard_Read_Text_Async
      (Self        : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gdk_Clipboard_Read_Text_Async, "gdk_clipboard_read_text_async");
   --  Asynchronously request the Clipboard contents converted to a string.
   --  This is a simple wrapper around [methodGdk.Clipboard.read_value_async].
   --  Use that function or [methodGdk.Clipboard.read_async] directly if you
   --  need more control over the operation.
   --  @param Cancellable optional `GCancellable` object
   --  @param Callback callback to call when the request is satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_Gdk_Clipboard_Read_Texture_Async
      (Self        : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gdk_Clipboard_Read_Texture_Async, "gdk_clipboard_read_texture_async");
   --  Asynchronously request the Clipboard contents converted to a
   --  `GdkPixbuf`.
   --  This is a simple wrapper around [methodGdk.Clipboard.read_value_async].
   --  Use that function or [methodGdk.Clipboard.read_async] directly if you
   --  need more control over the operation.
   --  @param Cancellable optional `GCancellable` object, null to ignore.
   --  @param Callback callback to call when the request is satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_Gdk_Clipboard_Read_Value_Async
      (Self        : System.Address;
       The_Type    : GType;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gdk_Clipboard_Read_Value_Async, "gdk_clipboard_read_value_async");
   --  Asynchronously request the Clipboard contents converted to the given
   --  Type.
   --  For local clipboard contents that are available in the given `GType`,
   --  the value will be copied directly. Otherwise, GDK will try to use
   --  [funcContent_Deserialize_Async] to convert the clipboard's data.
   --  @param The_Type a `GType` to read
   --  @param Io_Priority the I/O priority of the request
   --  @param Cancellable optional `GCancellable` object
   --  @param Callback callback to call when the request is satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_Gdk_Clipboard_Store_Async
      (Self        : System.Address;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gdk_Clipboard_Store_Async, "gdk_clipboard_store_async");
   --  Asynchronously instructs the Clipboard to store its contents remotely.
   --  If the clipboard is not local, this function does nothing but report
   --  success.
   --  The purpose of this call is to preserve clipboard contents beyond the
   --  lifetime of an application, so this function is typically called on
   --  exit. Depending on the platform, the functionality may not be available
   --  unless a "clipboard manager" is running.
   --  This function is called automatically when a
   --  [GtkApplication](../gtk4/class.Application.html) is shut down, so you
   --  likely don't need to call it.
   --  @param Io_Priority the I/O priority of the request
   --  @param Cancellable optional `GCancellable` object
   --  @param Callback callback to call when the request is satisfied
   --  @param User_Data the data to pass to callback function

   function To_Gasync_Ready_Callback is new Ada.Unchecked_Conversion
     (System.Address, Gasync_Ready_Callback);

   function To_Address is new Ada.Unchecked_Conversion
     (Gasync_Ready_Callback, System.Address);

   procedure Internal_Gasync_Ready_Callback
      (Source_Object : System.Address;
       Res           : Glib.G_Async_Result;
       User_Data     : System.Address);
   pragma Convention (C, Internal_Gasync_Ready_Callback);
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.
   --  @param User_Data user data passed to the callback.

   ------------------------------------
   -- Internal_Gasync_Ready_Callback --
   ------------------------------------

   procedure Internal_Gasync_Ready_Callback
      (Source_Object : System.Address;
       Res           : Glib.G_Async_Result;
       User_Data     : System.Address)
   is
      Func         : constant Gasync_Ready_Callback := To_Gasync_Ready_Callback (User_Data);
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      Func (Get_User_Data (Source_Object, Stub_GObject), Res);
   end Internal_Gasync_Ready_Callback;

   package Type_Conversion_Gdk_Clipboard is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Clipboard_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Clipboard);

   -----------------
   -- Get_Content --
   -----------------

   function Get_Content
      (Self : not null access Gdk_Clipboard_Record)
       return Gdk.Content_Provider.Gdk_Content_Provider
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_clipboard_get_content");
      Stub_Gdk_Content_Provider : Gdk.Content_Provider.Gdk_Content_Provider_Record;
   begin
      return Gdk.Content_Provider.Gdk_Content_Provider (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Content_Provider));
   end Get_Content;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gdk_Clipboard_Record) return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_clipboard_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   -----------------
   -- Get_Formats --
   -----------------

   function Get_Formats
      (Self : not null access Gdk_Clipboard_Record)
       return Gdk.Content_Formats.Gdk_Content_Formats
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_clipboard_get_formats");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Formats;

   --------------
   -- Is_Local --
   --------------

   function Is_Local
      (Self : not null access Gdk_Clipboard_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_clipboard_is_local");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Local;

   ----------------
   -- Read_Async --
   ----------------

   procedure Read_Async
      (Self        : not null access Gdk_Clipboard_Record;
       Mime_Types  : GNAT.Strings.String_List;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
      Tmp_Mime_Types : Gtkada.Types.chars_ptr_array := From_String_List (Mime_Types);
   begin
      if Callback = null then
         C_Gdk_Clipboard_Read_Async (Get_Object (Self), Tmp_Mime_Types, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
         Gtkada.Types.Free (Tmp_Mime_Types);
      else
         C_Gdk_Clipboard_Read_Async (Get_Object (Self), Tmp_Mime_Types, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
         Gtkada.Types.Free (Tmp_Mime_Types);
      end if;
   end Read_Async;

   ---------------------
   -- Read_Text_Async --
   ---------------------

   procedure Read_Text_Async
      (Self        : not null access Gdk_Clipboard_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gdk_Clipboard_Read_Text_Async (Get_Object (Self), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gdk_Clipboard_Read_Text_Async (Get_Object (Self), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Read_Text_Async;

   ----------------------
   -- Read_Text_Finish --
   ----------------------

   function Read_Text_Finish
      (Self   : not null access Gdk_Clipboard_Record;
       Result : Glib.G_Async_Result) return UTF8_String
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_clipboard_read_text_finish");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self), Result));
   end Read_Text_Finish;

   ------------------------
   -- Read_Texture_Async --
   ------------------------

   procedure Read_Texture_Async
      (Self        : not null access Gdk_Clipboard_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gdk_Clipboard_Read_Texture_Async (Get_Object (Self), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gdk_Clipboard_Read_Texture_Async (Get_Object (Self), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Read_Texture_Async;

   ----------------------
   -- Read_Value_Async --
   ----------------------

   procedure Read_Value_Async
      (Self        : not null access Gdk_Clipboard_Record;
       The_Type    : GType;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gdk_Clipboard_Read_Value_Async (Get_Object (Self), The_Type, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gdk_Clipboard_Read_Value_Async (Get_Object (Self), The_Type, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Read_Value_Async;

   -----------------------
   -- Read_Value_Finish --
   -----------------------

   function Read_Value_Finish
      (Self   : not null access Gdk_Clipboard_Record;
       Result : Glib.G_Async_Result) return Glib.Values.GValue
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Glib.Values.GValue;
      pragma Import (C, Internal, "gdk_clipboard_read_value_finish");
   begin
      return Internal (Get_Object (Self), Result);
   end Read_Value_Finish;

   -----------------
   -- Set_Content --
   -----------------

   function Set_Content
      (Self     : not null access Gdk_Clipboard_Record;
       Provider : access Gdk.Content_Provider.Gdk_Content_Provider_Record'Class)
       return Boolean
   is
      function Internal
         (Self     : System.Address;
          Provider : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_clipboard_set_content");
   begin
      return Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Provider))) /= 0;
   end Set_Content;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Self : not null access Gdk_Clipboard_Record;
       Text : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gdk_clipboard_set_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Self), Tmp_Text);
      Free (Tmp_Text);
   end Set_Text;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
      (Self  : not null access Gdk_Clipboard_Record;
       Value : in out Glib.Values.GValue)
   is
      procedure Internal
         (Self  : System.Address;
          Value : in out Glib.Values.GValue);
      pragma Import (C, Internal, "gdk_clipboard_set_value");
   begin
      Internal (Get_Object (Self), Value);
   end Set_Value;

   -----------------
   -- Store_Async --
   -----------------

   procedure Store_Async
      (Self        : not null access Gdk_Clipboard_Record;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gdk_Clipboard_Store_Async (Get_Object (Self), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gdk_Clipboard_Store_Async (Get_Object (Self), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Store_Async;

   ------------------
   -- Store_Finish --
   ------------------

   function Store_Finish
      (Self   : not null access Gdk_Clipboard_Record;
       Result : Glib.G_Async_Result) return Boolean
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_clipboard_store_finish");
   begin
      return Internal (Get_Object (Self), Result) /= 0;
   end Store_Finish;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Clipboard_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Clipboard_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gdk_Clipboard_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Clipboard_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gdk_Clipboard_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gdk_Clipboard_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Clipboard_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Clipboard_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Clipboard_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Clipboard_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Clipboard_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------
   -- Marsh_GObject_Void --
   ------------------------

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   ------------------------------
   -- Marsh_Gdk_Clipboard_Void --
   ------------------------------

   procedure Marsh_Gdk_Clipboard_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Clipboard_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Clipboard := Gdk_Clipboard (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Clipboard_Void;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gdk_Clipboard_Record;
       Call  : Cb_Gdk_Clipboard_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gdk_Clipboard_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

end Gdk.Clipboard;
