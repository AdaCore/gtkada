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
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
with System;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gdk.Content_Provider is

   procedure C_Gdk_Content_Provider_Write_Mime_Type_Async
      (Self        : System.Address;
       Mime_Type   : Gtkada.Types.Chars_Ptr;
       Stream      : System.Address;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gdk_Content_Provider_Write_Mime_Type_Async, "gdk_content_provider_write_mime_type_async");
   --  Asynchronously writes the contents of Provider to Stream in the given
   --  Mime_Type.
   --  The given mime type does not need to be listed in the formats returned
   --  by [methodGdk.ContentProvider.ref_formats]. However, if the given
   --  `GType` is not supported, `G_IO_ERROR_NOT_SUPPORTED` will be reported.
   --  The given Stream will not be closed.
   --  @param Mime_Type the mime type to provide the data in
   --  @param Stream the `GOutputStream` to write to
   --  @param Io_Priority I/O priority of the request.
   --  @param Cancellable optional `GCancellable` object, null to ignore.
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

   package Type_Conversion_Gdk_Content_Provider is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Content_Provider_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Content_Provider);

   ----------------------------------------
   -- Gdk_Content_Provider_New_For_Bytes --
   ----------------------------------------

   function Gdk_Content_Provider_New_For_Bytes
      (Mime_Type : UTF8_String;
       Bytes     : Glib.Bytes.Gbytes) return Gdk_Content_Provider
   is
      Self : constant Gdk_Content_Provider := new Gdk_Content_Provider_Record;
   begin
      Gdk.Content_Provider.Initialize_For_Bytes (Self, Mime_Type, Bytes);
      return Self;
   end Gdk_Content_Provider_New_For_Bytes;

   ----------------------------------------
   -- Gdk_Content_Provider_New_For_Value --
   ----------------------------------------

   function Gdk_Content_Provider_New_For_Value
      (Value : in out Glib.Values.GValue) return Gdk_Content_Provider
   is
      Self : constant Gdk_Content_Provider := new Gdk_Content_Provider_Record;
   begin
      Gdk.Content_Provider.Initialize_For_Value (Self, Value);
      return Self;
   end Gdk_Content_Provider_New_For_Value;

   -----------------------
   -- Gdk_New_For_Bytes --
   -----------------------

   procedure Gdk_New_For_Bytes
      (Self      : out Gdk_Content_Provider;
       Mime_Type : UTF8_String;
       Bytes     : Glib.Bytes.Gbytes)
   is
   begin
      Self := new Gdk_Content_Provider_Record;
      Gdk.Content_Provider.Initialize_For_Bytes (Self, Mime_Type, Bytes);
   end Gdk_New_For_Bytes;

   -----------------------
   -- Gdk_New_For_Value --
   -----------------------

   procedure Gdk_New_For_Value
      (Self  : out Gdk_Content_Provider;
       Value : in out Glib.Values.GValue)
   is
   begin
      Self := new Gdk_Content_Provider_Record;
      Gdk.Content_Provider.Initialize_For_Value (Self, Value);
   end Gdk_New_For_Value;

   --------------------------
   -- Initialize_For_Bytes --
   --------------------------

   procedure Initialize_For_Bytes
      (Self      : not null access Gdk_Content_Provider_Record'Class;
       Mime_Type : UTF8_String;
       Bytes     : Glib.Bytes.Gbytes)
   is
      function Internal
         (Mime_Type : Gtkada.Types.Chars_Ptr;
          Bytes     : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_content_provider_new_for_bytes");
      Tmp_Mime_Type : Gtkada.Types.Chars_Ptr := New_String (Mime_Type);
      Tmp_Return    : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Mime_Type, Get_Object (Bytes));
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Mime_Type);
   end Initialize_For_Bytes;

   --------------------------
   -- Initialize_For_Value --
   --------------------------

   procedure Initialize_For_Value
      (Self  : not null access Gdk_Content_Provider_Record'Class;
       Value : in out Glib.Values.GValue)
   is
      function Internal
         (Acc_Value : access Glib.Values.GValue) return System.Address;
      pragma Import (C, Internal, "gdk_content_provider_new_for_value");
      Acc_Value : aliased Glib.Values.GValue := Value;
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Acc_Value'Access));
      end if;
   end Initialize_For_Value;

   ---------------------
   -- Content_Changed --
   ---------------------

   procedure Content_Changed
      (Self : not null access Gdk_Content_Provider_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_content_provider_content_changed");
   begin
      Internal (Get_Object (Self));
   end Content_Changed;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
      (Self  : not null access Gdk_Content_Provider_Record;
       Value : in out Glib.Values.GValue) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Acc_Value : access Glib.Values.GValue) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_content_provider_get_value");
      Acc_Value  : aliased Glib.Values.GValue := Value;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Value'Access);
      Value := Acc_Value;
      return Tmp_Return /= 0;
   end Get_Value;

   -----------------
   -- Ref_Formats --
   -----------------

   function Ref_Formats
      (Self : not null access Gdk_Content_Provider_Record)
       return Gdk.Content_Formats.Gdk_Content_Formats
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_content_provider_ref_formats");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref_Formats;

   --------------------------
   -- Ref_Storable_Formats --
   --------------------------

   function Ref_Storable_Formats
      (Self : not null access Gdk_Content_Provider_Record)
       return Gdk.Content_Formats.Gdk_Content_Formats
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_content_provider_ref_storable_formats");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref_Storable_Formats;

   ---------------------------
   -- Write_Mime_Type_Async --
   ---------------------------

   procedure Write_Mime_Type_Async
      (Self        : not null access Gdk_Content_Provider_Record;
       Mime_Type   : UTF8_String;
       Stream      : not null access Glib.Output_Stream.Goutput_Stream_Record'Class;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
      Tmp_Mime_Type : Gtkada.Types.Chars_Ptr := New_String (Mime_Type);
   begin
      if Callback = null then
         C_Gdk_Content_Provider_Write_Mime_Type_Async (Get_Object (Self), Tmp_Mime_Type, Get_Object (Stream), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
         Free (Tmp_Mime_Type);
      else
         C_Gdk_Content_Provider_Write_Mime_Type_Async (Get_Object (Self), Tmp_Mime_Type, Get_Object (Stream), Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
         Free (Tmp_Mime_Type);
      end if;
   end Write_Mime_Type_Async;

   ----------------------------
   -- Write_Mime_Type_Finish --
   ----------------------------

   function Write_Mime_Type_Finish
      (Self   : not null access Gdk_Content_Provider_Record;
       Result : Glib.G_Async_Result) return Boolean
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_content_provider_write_mime_type_finish");
   begin
      return Internal (Get_Object (Self), Result) /= 0;
   end Write_Mime_Type_Finish;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Content_Provider_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Content_Provider_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gdk_Content_Provider_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Content_Provider_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gdk_Content_Provider_Record'Class;
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

   procedure Marsh_Gdk_Content_Provider_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Content_Provider_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Content_Provider_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Content_Provider_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Content_Provider_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Content_Provider_Record'Class;
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
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   -------------------------------------
   -- Marsh_Gdk_Content_Provider_Void --
   -------------------------------------

   procedure Marsh_Gdk_Content_Provider_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Content_Provider_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Content_Provider := Gdk_Content_Provider (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gdk_Content_Provider_Void;

   ------------------------
   -- On_Content_Changed --
   ------------------------

   procedure On_Content_Changed
      (Self  : not null access Gdk_Content_Provider_Record;
       Call  : Cb_Gdk_Content_Provider_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "content-changed" & ASCII.NUL, Call, After);
   end On_Content_Changed;

   ------------------------
   -- On_Content_Changed --
   ------------------------

   procedure On_Content_Changed
      (Self  : not null access Gdk_Content_Provider_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "content-changed" & ASCII.NUL, Call, After, Slot);
   end On_Content_Changed;

end Gdk.Content_Provider;
