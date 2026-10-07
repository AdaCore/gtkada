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
with Glib.Error;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

use type Glib.Error.GError;

package body Gdk.Pixbuf is

   procedure C_G_Loadable_Icon_Load_Async
      (Self        : System.Address;
       Size        : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_Loadable_Icon_Load_Async, "g_loadable_icon_load_async");
   --  Loads an icon asynchronously. To finish this function, see
   --  Glib.Loadable_Icon.Load_Finish. For the synchronous, blocking version of
   --  this function, see Glib.Loadable_Icon.Load.
   --  @param Size an integer.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   procedure C_Gdk_Pixbuf_Get_File_Info_Async
      (Filename    : Gtkada.Types.Chars_Ptr;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gdk_Pixbuf_Get_File_Info_Async, "gdk_pixbuf_get_file_info_async");
   --  Asynchronously parses an image file far enough to determine its format
   --  and size.
   --  For more details see Gdk.Pixbuf.Get_File_Info, which is the synchronous
   --  version of this function.
   --  When the operation is finished, Callback will be called in the main
   --  thread. You can then call Gdk.Pixbuf.Get_File_Info_Finish to get the
   --  result of the operation.
   --  Since: gtk+ 2.32
   --  @param Filename The name of the file to identify
   --  @param Cancellable optional `GCancellable` object, `NULL` to ignore
   --  @param Callback a `GAsyncReadyCallback` to call when the file info is
   --  available
   --  @param User_Data the data to pass to the callback function

   procedure C_Gdk_Pixbuf_New_From_Stream_Async
      (Stream      : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gdk_Pixbuf_New_From_Stream_Async, "gdk_pixbuf_new_from_stream_async");
   --  Creates a new pixbuf by asynchronously loading an image from an input
   --  stream.
   --  For more details see Gdk.Pixbuf.Gdk_New_From_Stream, which is the
   --  synchronous version of this function.
   --  When the operation is finished, Callback will be called in the main
   --  thread. You can then call Gdk.Pixbuf.Gdk_New_From_Stream_Finish to get
   --  the result of the operation.
   --  Since: gtk+ 2.24
   --  @param Stream a `GInputStream` from which to load the pixbuf
   --  @param Cancellable optional `GCancellable` object, `NULL` to ignore
   --  @param Callback a `GAsyncReadyCallback` to call when the pixbuf is
   --  loaded
   --  @param User_Data the data to pass to the callback function

   procedure C_Gdk_Pixbuf_New_From_Stream_At_Scale_Async
      (Stream                : System.Address;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Glib.Gboolean;
       Cancellable           : System.Address;
       Callback              : System.Address;
       User_Data             : System.Address);
   pragma Import (C, C_Gdk_Pixbuf_New_From_Stream_At_Scale_Async, "gdk_pixbuf_new_from_stream_at_scale_async");
   --  Creates a new pixbuf by asynchronously loading an image from an input
   --  stream.
   --  For more details see Gdk.Pixbuf.Gdk_New_From_Stream_At_Scale, which is
   --  the synchronous version of this function.
   --  When the operation is finished, Callback will be called in the main
   --  thread. You can then call Gdk.Pixbuf.Gdk_New_From_Stream_Finish to get
   --  the result of the operation.
   --  Since: gtk+ 2.24
   --  @param Stream a `GInputStream` from which to load the pixbuf
   --  @param Width the width the image should have or -1 to not constrain the
   --  width
   --  @param Height the height the image should have or -1 to not constrain
   --  the height
   --  @param Preserve_Aspect_Ratio `TRUE` to preserve the image's aspect
   --  ratio
   --  @param Cancellable optional `GCancellable` object, `NULL` to ignore
   --  @param Callback a `GAsyncReadyCallback` to call when the pixbuf is
   --  loaded
   --  @param User_Data the data to pass to the callback function

   procedure C_Gdk_Pixbuf_Save_To_Streamv_Async
      (Self          : System.Address;
       Stream        : System.Address;
       The_Type      : Gtkada.Types.Chars_Ptr;
       Option_Keys   : Gtkada.Types.chars_ptr_array;
       Option_Values : Gtkada.Types.chars_ptr_array;
       Cancellable   : System.Address;
       Callback      : System.Address;
       User_Data     : System.Address);
   pragma Import (C, C_Gdk_Pixbuf_Save_To_Streamv_Async, "gdk_pixbuf_save_to_streamv_async");
   --  Saves `pixbuf` to an output stream asynchronously.
   --  For more details see Gdk.Pixbuf.Save_To_Streamv, which is the
   --  synchronous version of this function.
   --  When the operation is finished, `callback` will be called in the main
   --  thread.
   --  You can then call Gdk.Pixbuf.Save_To_Stream_Finish to get the result of
   --  the operation.
   --  Since: gtk+ 2.36
   --  @param Stream a `GOutputStream` to which to save the pixbuf
   --  @param The_Type name of file format
   --  @param Option_Keys name of options to set
   --  @param Option_Values values for named options
   --  @param Cancellable optional `GCancellable` object, `NULL` to ignore
   --  @param Callback a `GAsyncReadyCallback` to call when the pixbuf is
   --  saved
   --  @param User_Data the data to pass to the callback function

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

   package Type_Conversion_Gdk_Pixbuf is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Pixbuf_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Pixbuf);

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
      (Self            : out Gdk_Pixbuf;
       Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint)
   is
   begin
      Self := new Gdk_Pixbuf_Record;
      Gdk.Pixbuf.Initialize (Self, Colorspace, Has_Alpha, Bits_Per_Sample, Width, Height);
   end Gdk_New;

   ------------------------
   -- Gdk_New_From_Bytes --
   ------------------------

   procedure Gdk_New_From_Bytes
      (Self            : out Gdk_Pixbuf;
       Data            : Glib.Bytes.Gbytes;
       Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint;
       Rowstride       : Glib.Gint)
   is
   begin
      Self := new Gdk_Pixbuf_Record;
      Gdk.Pixbuf.Initialize_From_Bytes (Self, Data, Colorspace, Has_Alpha, Bits_Per_Sample, Width, Height, Rowstride);
   end Gdk_New_From_Bytes;

   -----------------------
   -- Gdk_New_From_Data --
   -----------------------

   procedure Gdk_New_From_Data
      (Self            : out Gdk_Pixbuf;
       Data            : System.Address;
       Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint;
       Rowstride       : Glib.Gint;
       Destroy_Fn      : Gdk_Pixbuf_Destroy_Notify := null;
       Destroy_Fn_Data : System.Address := System.Null_Address)
   is
   begin
      Self := new Gdk_Pixbuf_Record;
      Gdk.Pixbuf.Initialize_From_Data (Self, Data, Colorspace, Has_Alpha, Bits_Per_Sample, Width, Height, Rowstride, Destroy_Fn, Destroy_Fn_Data);
   end Gdk_New_From_Data;

   -----------------------
   -- Gdk_New_From_File --
   -----------------------

   procedure Gdk_New_From_File
      (Self     : out Gdk_Pixbuf;
       Filename : UTF8_String;
       Error    : out Glib.Error.GError)
   is
   begin
      Self := new Gdk_Pixbuf_Record;
      Gdk.Pixbuf.Initialize_From_File (Self, Filename, Error);
   end Gdk_New_From_File;

   --------------------------------
   -- Gdk_New_From_File_At_Scale --
   --------------------------------

   procedure Gdk_New_From_File_At_Scale
      (Self                  : out Gdk_Pixbuf;
       Filename              : UTF8_String;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Error                 : out Glib.Error.GError)
   is
   begin
      Self := new Gdk_Pixbuf_Record;
      Gdk.Pixbuf.Initialize_From_File_At_Scale (Self, Filename, Width, Height, Preserve_Aspect_Ratio, Error);
   end Gdk_New_From_File_At_Scale;

   -------------------------------
   -- Gdk_New_From_File_At_Size --
   -------------------------------

   procedure Gdk_New_From_File_At_Size
      (Self     : out Gdk_Pixbuf;
       Filename : UTF8_String;
       Width    : Glib.Gint;
       Height   : Glib.Gint;
       Error    : out Glib.Error.GError)
   is
   begin
      Self := new Gdk_Pixbuf_Record;
      Gdk.Pixbuf.Initialize_From_File_At_Size (Self, Filename, Width, Height, Error);
   end Gdk_New_From_File_At_Size;

   -------------------------
   -- Gdk_New_From_Inline --
   -------------------------

   procedure Gdk_New_From_Inline
      (Self        : out Gdk_Pixbuf;
       Data_Length : Glib.Gint;
       Data        : Guint8_Array;
       Error       : out Glib.Error.GError)
   is
   begin
      Self := new Gdk_Pixbuf_Record;
      Gdk.Pixbuf.Initialize_From_Inline (Self, Data_Length, Data, Error);
   end Gdk_New_From_Inline;

   ---------------------------
   -- Gdk_New_From_Resource --
   ---------------------------

   procedure Gdk_New_From_Resource
      (Self          : out Gdk_Pixbuf;
       Resource_Path : UTF8_String;
       Error         : out Glib.Error.GError)
   is
   begin
      Self := new Gdk_Pixbuf_Record;
      Gdk.Pixbuf.Initialize_From_Resource (Self, Resource_Path, Error);
   end Gdk_New_From_Resource;

   ------------------------------------
   -- Gdk_New_From_Resource_At_Scale --
   ------------------------------------

   procedure Gdk_New_From_Resource_At_Scale
      (Self                  : out Gdk_Pixbuf;
       Resource_Path         : UTF8_String;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Error                 : out Glib.Error.GError)
   is
   begin
      Self := new Gdk_Pixbuf_Record;
      Gdk.Pixbuf.Initialize_From_Resource_At_Scale (Self, Resource_Path, Width, Height, Preserve_Aspect_Ratio, Error);
   end Gdk_New_From_Resource_At_Scale;

   -------------------------
   -- Gdk_New_From_Stream --
   -------------------------

   procedure Gdk_New_From_Stream
      (Self        : out Gdk_Pixbuf;
       Stream      : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Error       : out Glib.Error.GError)
   is
   begin
      Self := new Gdk_Pixbuf_Record;
      Gdk.Pixbuf.Initialize_From_Stream (Self, Stream, Cancellable, Error);
   end Gdk_New_From_Stream;

   ----------------------------------
   -- Gdk_New_From_Stream_At_Scale --
   ----------------------------------

   procedure Gdk_New_From_Stream_At_Scale
      (Self                  : out Gdk_Pixbuf;
       Stream                : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Cancellable           : access Glib.Cancellable.Gcancellable_Record'Class;
       Error                 : out Glib.Error.GError)
   is
   begin
      Self := new Gdk_Pixbuf_Record;
      Gdk.Pixbuf.Initialize_From_Stream_At_Scale (Self, Stream, Width, Height, Preserve_Aspect_Ratio, Cancellable, Error);
   end Gdk_New_From_Stream_At_Scale;

   --------------------------------
   -- Gdk_New_From_Stream_Finish --
   --------------------------------

   procedure Gdk_New_From_Stream_Finish
      (Self         : out Gdk_Pixbuf;
       Async_Result : Glib.G_Async_Result;
       Error        : out Glib.Error.GError)
   is
   begin
      Self := new Gdk_Pixbuf_Record;
      Gdk.Pixbuf.Initialize_From_Stream_Finish (Self, Async_Result, Error);
   end Gdk_New_From_Stream_Finish;

   ---------------------------
   -- Gdk_New_From_Xpm_Data --
   ---------------------------

   procedure Gdk_New_From_Xpm_Data
      (Self : out Gdk_Pixbuf;
       Data : GNAT.Strings.String_List)
   is
   begin
      Self := new Gdk_Pixbuf_Record;
      Gdk.Pixbuf.Initialize_From_Xpm_Data (Self, Data);
   end Gdk_New_From_Xpm_Data;

   --------------------
   -- Gdk_Pixbuf_New --
   --------------------

   function Gdk_Pixbuf_New
      (Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint) return Gdk_Pixbuf
   is
      Self : constant Gdk_Pixbuf := new Gdk_Pixbuf_Record;
   begin
      Gdk.Pixbuf.Initialize (Self, Colorspace, Has_Alpha, Bits_Per_Sample, Width, Height);
      return Self;
   end Gdk_Pixbuf_New;

   -------------------------------
   -- Gdk_Pixbuf_New_From_Bytes --
   -------------------------------

   function Gdk_Pixbuf_New_From_Bytes
      (Data            : Glib.Bytes.Gbytes;
       Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint;
       Rowstride       : Glib.Gint) return Gdk_Pixbuf
   is
      Self : constant Gdk_Pixbuf := new Gdk_Pixbuf_Record;
   begin
      Gdk.Pixbuf.Initialize_From_Bytes (Self, Data, Colorspace, Has_Alpha, Bits_Per_Sample, Width, Height, Rowstride);
      return Self;
   end Gdk_Pixbuf_New_From_Bytes;

   ------------------------------
   -- Gdk_Pixbuf_New_From_Data --
   ------------------------------

   function Gdk_Pixbuf_New_From_Data
      (Data            : System.Address;
       Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint;
       Rowstride       : Glib.Gint;
       Destroy_Fn      : Gdk_Pixbuf_Destroy_Notify := null;
       Destroy_Fn_Data : System.Address := System.Null_Address)
       return Gdk_Pixbuf
   is
      Self : constant Gdk_Pixbuf := new Gdk_Pixbuf_Record;
   begin
      Gdk.Pixbuf.Initialize_From_Data (Self, Data, Colorspace, Has_Alpha, Bits_Per_Sample, Width, Height, Rowstride, Destroy_Fn, Destroy_Fn_Data);
      return Self;
   end Gdk_Pixbuf_New_From_Data;

   ------------------------------
   -- Gdk_Pixbuf_New_From_File --
   ------------------------------

   function Gdk_Pixbuf_New_From_File
      (Filename : UTF8_String;
       Error    : out Glib.Error.GError) return Gdk_Pixbuf
   is
      Self : constant Gdk_Pixbuf := new Gdk_Pixbuf_Record;
   begin
      Gdk.Pixbuf.Initialize_From_File (Self, Filename, Error);
      return Self;
   end Gdk_Pixbuf_New_From_File;

   ---------------------------------------
   -- Gdk_Pixbuf_New_From_File_At_Scale --
   ---------------------------------------

   function Gdk_Pixbuf_New_From_File_At_Scale
      (Filename              : UTF8_String;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Error                 : out Glib.Error.GError) return Gdk_Pixbuf
   is
      Self : constant Gdk_Pixbuf := new Gdk_Pixbuf_Record;
   begin
      Gdk.Pixbuf.Initialize_From_File_At_Scale (Self, Filename, Width, Height, Preserve_Aspect_Ratio, Error);
      return Self;
   end Gdk_Pixbuf_New_From_File_At_Scale;

   --------------------------------------
   -- Gdk_Pixbuf_New_From_File_At_Size --
   --------------------------------------

   function Gdk_Pixbuf_New_From_File_At_Size
      (Filename : UTF8_String;
       Width    : Glib.Gint;
       Height   : Glib.Gint;
       Error    : out Glib.Error.GError) return Gdk_Pixbuf
   is
      Self : constant Gdk_Pixbuf := new Gdk_Pixbuf_Record;
   begin
      Gdk.Pixbuf.Initialize_From_File_At_Size (Self, Filename, Width, Height, Error);
      return Self;
   end Gdk_Pixbuf_New_From_File_At_Size;

   --------------------------------
   -- Gdk_Pixbuf_New_From_Inline --
   --------------------------------

   function Gdk_Pixbuf_New_From_Inline
      (Data_Length : Glib.Gint;
       Data        : Guint8_Array;
       Error       : out Glib.Error.GError) return Gdk_Pixbuf
   is
      Self : constant Gdk_Pixbuf := new Gdk_Pixbuf_Record;
   begin
      Gdk.Pixbuf.Initialize_From_Inline (Self, Data_Length, Data, Error);
      return Self;
   end Gdk_Pixbuf_New_From_Inline;

   ----------------------------------
   -- Gdk_Pixbuf_New_From_Resource --
   ----------------------------------

   function Gdk_Pixbuf_New_From_Resource
      (Resource_Path : UTF8_String;
       Error         : out Glib.Error.GError) return Gdk_Pixbuf
   is
      Self : constant Gdk_Pixbuf := new Gdk_Pixbuf_Record;
   begin
      Gdk.Pixbuf.Initialize_From_Resource (Self, Resource_Path, Error);
      return Self;
   end Gdk_Pixbuf_New_From_Resource;

   -------------------------------------------
   -- Gdk_Pixbuf_New_From_Resource_At_Scale --
   -------------------------------------------

   function Gdk_Pixbuf_New_From_Resource_At_Scale
      (Resource_Path         : UTF8_String;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Error                 : out Glib.Error.GError) return Gdk_Pixbuf
   is
      Self : constant Gdk_Pixbuf := new Gdk_Pixbuf_Record;
   begin
      Gdk.Pixbuf.Initialize_From_Resource_At_Scale (Self, Resource_Path, Width, Height, Preserve_Aspect_Ratio, Error);
      return Self;
   end Gdk_Pixbuf_New_From_Resource_At_Scale;

   --------------------------------
   -- Gdk_Pixbuf_New_From_Stream --
   --------------------------------

   function Gdk_Pixbuf_New_From_Stream
      (Stream      : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Error       : out Glib.Error.GError) return Gdk_Pixbuf
   is
      Self : constant Gdk_Pixbuf := new Gdk_Pixbuf_Record;
   begin
      Gdk.Pixbuf.Initialize_From_Stream (Self, Stream, Cancellable, Error);
      return Self;
   end Gdk_Pixbuf_New_From_Stream;

   -----------------------------------------
   -- Gdk_Pixbuf_New_From_Stream_At_Scale --
   -----------------------------------------

   function Gdk_Pixbuf_New_From_Stream_At_Scale
      (Stream                : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Cancellable           : access Glib.Cancellable.Gcancellable_Record'Class;
       Error                 : out Glib.Error.GError) return Gdk_Pixbuf
   is
      Self : constant Gdk_Pixbuf := new Gdk_Pixbuf_Record;
   begin
      Gdk.Pixbuf.Initialize_From_Stream_At_Scale (Self, Stream, Width, Height, Preserve_Aspect_Ratio, Cancellable, Error);
      return Self;
   end Gdk_Pixbuf_New_From_Stream_At_Scale;

   ---------------------------------------
   -- Gdk_Pixbuf_New_From_Stream_Finish --
   ---------------------------------------

   function Gdk_Pixbuf_New_From_Stream_Finish
      (Async_Result : Glib.G_Async_Result;
       Error        : out Glib.Error.GError) return Gdk_Pixbuf
   is
      Self : constant Gdk_Pixbuf := new Gdk_Pixbuf_Record;
   begin
      Gdk.Pixbuf.Initialize_From_Stream_Finish (Self, Async_Result, Error);
      return Self;
   end Gdk_Pixbuf_New_From_Stream_Finish;

   ----------------------------------
   -- Gdk_Pixbuf_New_From_Xpm_Data --
   ----------------------------------

   function Gdk_Pixbuf_New_From_Xpm_Data
      (Data : GNAT.Strings.String_List) return Gdk_Pixbuf
   is
      Self : constant Gdk_Pixbuf := new Gdk_Pixbuf_Record;
   begin
      Gdk.Pixbuf.Initialize_From_Xpm_Data (Self, Data);
      return Self;
   end Gdk_Pixbuf_New_From_Xpm_Data;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self            : not null access Gdk_Pixbuf_Record'Class;
       Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint)
   is
      function Internal
         (Colorspace      : Gdk_Colorspace;
          Has_Alpha       : Glib.Gboolean;
          Bits_Per_Sample : Glib.Gint;
          Width           : Glib.Gint;
          Height          : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Colorspace, Boolean'Pos (Has_Alpha), Bits_Per_Sample, Width, Height));
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_From_Bytes --
   ---------------------------

   procedure Initialize_From_Bytes
      (Self            : not null access Gdk_Pixbuf_Record'Class;
       Data            : Glib.Bytes.Gbytes;
       Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint;
       Rowstride       : Glib.Gint)
   is
      function Internal
         (Data            : System.Address;
          Colorspace      : Gdk_Colorspace;
          Has_Alpha       : Glib.Gboolean;
          Bits_Per_Sample : Glib.Gint;
          Width           : Glib.Gint;
          Height          : Glib.Gint;
          Rowstride       : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_bytes");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Data), Colorspace, Boolean'Pos (Has_Alpha), Bits_Per_Sample, Width, Height, Rowstride));
      end if;
   end Initialize_From_Bytes;

   --------------------------
   -- Initialize_From_Data --
   --------------------------

   procedure Initialize_From_Data
      (Self            : not null access Gdk_Pixbuf_Record'Class;
       Data            : System.Address;
       Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint;
       Rowstride       : Glib.Gint;
       Destroy_Fn      : Gdk_Pixbuf_Destroy_Notify := null;
       Destroy_Fn_Data : System.Address := System.Null_Address)
   is
      function Internal
         (Data            : System.Address;
          Colorspace      : Gdk_Colorspace;
          Has_Alpha       : Glib.Gboolean;
          Bits_Per_Sample : Glib.Gint;
          Width           : Glib.Gint;
          Height          : Glib.Gint;
          Rowstride       : Glib.Gint;
          Destroy_Fn      : Gdk_Pixbuf_Destroy_Notify;
          Destroy_Fn_Data : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_data");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Data, Colorspace, Boolean'Pos (Has_Alpha), Bits_Per_Sample, Width, Height, Rowstride, Destroy_Fn, Destroy_Fn_Data));
      end if;
   end Initialize_From_Data;

   --------------------------
   -- Initialize_From_File --
   --------------------------

   procedure Initialize_From_File
      (Self     : not null access Gdk_Pixbuf_Record'Class;
       Filename : UTF8_String;
       Error    : out Glib.Error.GError)
   is
      function Internal
         (Filename  : Gtkada.Types.Chars_Ptr;
          Acc_Error : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_file");
      Acc_Error    : aliased Glib.Error.GError;
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : System.Address;
   begin
      Error := null;
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Filename, Acc_Error'Access);
         Error := Acc_Error;
         if Error = null then
            Set_Object (Self, Tmp_Return);
         end if;
      end if;
      Free (Tmp_Filename);
   end Initialize_From_File;

   -----------------------------------
   -- Initialize_From_File_At_Scale --
   -----------------------------------

   procedure Initialize_From_File_At_Scale
      (Self                  : not null access Gdk_Pixbuf_Record'Class;
       Filename              : UTF8_String;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Error                 : out Glib.Error.GError)
   is
      function Internal
         (Filename              : Gtkada.Types.Chars_Ptr;
          Width                 : Glib.Gint;
          Height                : Glib.Gint;
          Preserve_Aspect_Ratio : Glib.Gboolean;
          Acc_Error             : access Glib.Error.GError)
          return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_file_at_scale");
      Acc_Error    : aliased Glib.Error.GError;
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : System.Address;
   begin
      Error := null;
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Filename, Width, Height, Boolean'Pos (Preserve_Aspect_Ratio), Acc_Error'Access);
         Error := Acc_Error;
         if Error = null then
            Set_Object (Self, Tmp_Return);
         end if;
      end if;
      Free (Tmp_Filename);
   end Initialize_From_File_At_Scale;

   ----------------------------------
   -- Initialize_From_File_At_Size --
   ----------------------------------

   procedure Initialize_From_File_At_Size
      (Self     : not null access Gdk_Pixbuf_Record'Class;
       Filename : UTF8_String;
       Width    : Glib.Gint;
       Height   : Glib.Gint;
       Error    : out Glib.Error.GError)
   is
      function Internal
         (Filename  : Gtkada.Types.Chars_Ptr;
          Width     : Glib.Gint;
          Height    : Glib.Gint;
          Acc_Error : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_file_at_size");
      Acc_Error    : aliased Glib.Error.GError;
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : System.Address;
   begin
      Error := null;
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Filename, Width, Height, Acc_Error'Access);
         Error := Acc_Error;
         if Error = null then
            Set_Object (Self, Tmp_Return);
         end if;
      end if;
      Free (Tmp_Filename);
   end Initialize_From_File_At_Size;

   ----------------------------
   -- Initialize_From_Inline --
   ----------------------------

   procedure Initialize_From_Inline
      (Self        : not null access Gdk_Pixbuf_Record'Class;
       Data_Length : Glib.Gint;
       Data        : Guint8_Array;
       Error       : out Glib.Error.GError)
   is
      function Internal
         (Data_Length : Glib.Gint;
          Data        : System.Address;
          Copy_Pixels : Glib.Gboolean;
          Acc_Error   : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_inline");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : System.Address;
   begin
      Error := null;
      if not Self.Is_Created then
         Tmp_Return := Internal (Data_Length, Data'Address, 1, Acc_Error'Access);
         Error := Acc_Error;
         if Error = null then
            Set_Object (Self, Tmp_Return);
         end if;
      end if;
   end Initialize_From_Inline;

   ------------------------------
   -- Initialize_From_Resource --
   ------------------------------

   procedure Initialize_From_Resource
      (Self          : not null access Gdk_Pixbuf_Record'Class;
       Resource_Path : UTF8_String;
       Error         : out Glib.Error.GError)
   is
      function Internal
         (Resource_Path : Gtkada.Types.Chars_Ptr;
          Acc_Error     : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_resource");
      Acc_Error         : aliased Glib.Error.GError;
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
      Tmp_Return        : System.Address;
   begin
      Error := null;
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Resource_Path, Acc_Error'Access);
         Error := Acc_Error;
         if Error = null then
            Set_Object (Self, Tmp_Return);
         end if;
      end if;
      Free (Tmp_Resource_Path);
   end Initialize_From_Resource;

   ---------------------------------------
   -- Initialize_From_Resource_At_Scale --
   ---------------------------------------

   procedure Initialize_From_Resource_At_Scale
      (Self                  : not null access Gdk_Pixbuf_Record'Class;
       Resource_Path         : UTF8_String;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Error                 : out Glib.Error.GError)
   is
      function Internal
         (Resource_Path         : Gtkada.Types.Chars_Ptr;
          Width                 : Glib.Gint;
          Height                : Glib.Gint;
          Preserve_Aspect_Ratio : Glib.Gboolean;
          Acc_Error             : access Glib.Error.GError)
          return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_resource_at_scale");
      Acc_Error         : aliased Glib.Error.GError;
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
      Tmp_Return        : System.Address;
   begin
      Error := null;
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Resource_Path, Width, Height, Boolean'Pos (Preserve_Aspect_Ratio), Acc_Error'Access);
         Error := Acc_Error;
         if Error = null then
            Set_Object (Self, Tmp_Return);
         end if;
      end if;
      Free (Tmp_Resource_Path);
   end Initialize_From_Resource_At_Scale;

   ----------------------------
   -- Initialize_From_Stream --
   ----------------------------

   procedure Initialize_From_Stream
      (Self        : not null access Gdk_Pixbuf_Record'Class;
       Stream      : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Error       : out Glib.Error.GError)
   is
      function Internal
         (Stream      : System.Address;
          Cancellable : System.Address;
          Acc_Error   : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_stream");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : System.Address;
   begin
      Error := null;
      if not Self.Is_Created then
         Tmp_Return := Internal (Get_Object (Stream), Get_Object_Or_Null (GObject (Cancellable)), Acc_Error'Access);
         Error := Acc_Error;
         if Error = null then
            Set_Object (Self, Tmp_Return);
         end if;
      end if;
   end Initialize_From_Stream;

   -------------------------------------
   -- Initialize_From_Stream_At_Scale --
   -------------------------------------

   procedure Initialize_From_Stream_At_Scale
      (Self                  : not null access Gdk_Pixbuf_Record'Class;
       Stream                : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Cancellable           : access Glib.Cancellable.Gcancellable_Record'Class;
       Error                 : out Glib.Error.GError)
   is
      function Internal
         (Stream                : System.Address;
          Width                 : Glib.Gint;
          Height                : Glib.Gint;
          Preserve_Aspect_Ratio : Glib.Gboolean;
          Cancellable           : System.Address;
          Acc_Error             : access Glib.Error.GError)
          return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_stream_at_scale");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : System.Address;
   begin
      Error := null;
      if not Self.Is_Created then
         Tmp_Return := Internal (Get_Object (Stream), Width, Height, Boolean'Pos (Preserve_Aspect_Ratio), Get_Object_Or_Null (GObject (Cancellable)), Acc_Error'Access);
         Error := Acc_Error;
         if Error = null then
            Set_Object (Self, Tmp_Return);
         end if;
      end if;
   end Initialize_From_Stream_At_Scale;

   -----------------------------------
   -- Initialize_From_Stream_Finish --
   -----------------------------------

   procedure Initialize_From_Stream_Finish
      (Self         : not null access Gdk_Pixbuf_Record'Class;
       Async_Result : Glib.G_Async_Result;
       Error        : out Glib.Error.GError)
   is
      function Internal
         (Async_Result : Glib.G_Async_Result;
          Acc_Error    : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_stream_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : System.Address;
   begin
      Error := null;
      if not Self.Is_Created then
         Tmp_Return := Internal (Async_Result, Acc_Error'Access);
         Error := Acc_Error;
         if Error = null then
            Set_Object (Self, Tmp_Return);
         end if;
      end if;
   end Initialize_From_Stream_Finish;

   ------------------------------
   -- Initialize_From_Xpm_Data --
   ------------------------------

   procedure Initialize_From_Xpm_Data
      (Self : not null access Gdk_Pixbuf_Record'Class;
       Data : GNAT.Strings.String_List)
   is
      function Internal
         (Data : Gtkada.Types.chars_ptr_array) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_xpm_data");
      Tmp_Data   : Gtkada.Types.chars_ptr_array := From_String_List (Data);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Data);
         Set_Object (Self, Tmp_Return);
      end if;
      Gtkada.Types.Free (Tmp_Data);
   end Initialize_From_Xpm_Data;

   ---------------
   -- Add_Alpha --
   ---------------

   function Add_Alpha
      (Self             : not null access Gdk_Pixbuf_Record;
       Substitute_Color : Boolean;
       R                : Guchar;
       G                : Guchar;
       B                : Guchar) return Gdk_Pixbuf
   is
      function Internal
         (Self             : System.Address;
          Substitute_Color : Glib.Gboolean;
          R                : Guchar;
          G                : Guchar;
          B                : Guchar) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_add_alpha");
      Stub_Gdk_Pixbuf : Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Self), Boolean'Pos (Substitute_Color), R, G, B), Stub_Gdk_Pixbuf));
   end Add_Alpha;

   --------------------------------
   -- Apply_Embedded_Orientation --
   --------------------------------

   function Apply_Embedded_Orientation
      (Self : not null access Gdk_Pixbuf_Record) return Gdk_Pixbuf
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_apply_embedded_orientation");
      Stub_Gdk_Pixbuf : Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Pixbuf));
   end Apply_Embedded_Orientation;

   ---------------
   -- Composite --
   ---------------

   procedure Composite
      (Self          : not null access Gdk_Pixbuf_Record;
       Dest          : not null access Gdk_Pixbuf_Record'Class;
       Dest_X        : Glib.Gint;
       Dest_Y        : Glib.Gint;
       Dest_Width    : Glib.Gint;
       Dest_Height   : Glib.Gint;
       Offset_X      : Gdouble;
       Offset_Y      : Gdouble;
       Scale_X       : Gdouble;
       Scale_Y       : Gdouble;
       Interp_Type   : Gdk_Interp_Type;
       Overall_Alpha : Glib.Gint)
   is
      procedure Internal
         (Self          : System.Address;
          Dest          : System.Address;
          Dest_X        : Glib.Gint;
          Dest_Y        : Glib.Gint;
          Dest_Width    : Glib.Gint;
          Dest_Height   : Glib.Gint;
          Offset_X      : Gdouble;
          Offset_Y      : Gdouble;
          Scale_X       : Gdouble;
          Scale_Y       : Gdouble;
          Interp_Type   : Gdk_Interp_Type;
          Overall_Alpha : Glib.Gint);
      pragma Import (C, Internal, "gdk_pixbuf_composite");
   begin
      Internal (Get_Object (Self), Get_Object (Dest), Dest_X, Dest_Y, Dest_Width, Dest_Height, Offset_X, Offset_Y, Scale_X, Scale_Y, Interp_Type, Overall_Alpha);
   end Composite;

   ---------------------
   -- Composite_Color --
   ---------------------

   procedure Composite_Color
      (Self          : not null access Gdk_Pixbuf_Record;
       Dest          : not null access Gdk_Pixbuf_Record'Class;
       Dest_X        : Glib.Gint;
       Dest_Y        : Glib.Gint;
       Dest_Width    : Glib.Gint;
       Dest_Height   : Glib.Gint;
       Offset_X      : Gdouble;
       Offset_Y      : Gdouble;
       Scale_X       : Gdouble;
       Scale_Y       : Gdouble;
       Interp_Type   : Gdk_Interp_Type;
       Overall_Alpha : Glib.Gint;
       Check_X       : Glib.Gint;
       Check_Y       : Glib.Gint;
       Check_Size    : Glib.Gint;
       Color1        : Guint32;
       Color2        : Guint32)
   is
      procedure Internal
         (Self          : System.Address;
          Dest          : System.Address;
          Dest_X        : Glib.Gint;
          Dest_Y        : Glib.Gint;
          Dest_Width    : Glib.Gint;
          Dest_Height   : Glib.Gint;
          Offset_X      : Gdouble;
          Offset_Y      : Gdouble;
          Scale_X       : Gdouble;
          Scale_Y       : Gdouble;
          Interp_Type   : Gdk_Interp_Type;
          Overall_Alpha : Glib.Gint;
          Check_X       : Glib.Gint;
          Check_Y       : Glib.Gint;
          Check_Size    : Glib.Gint;
          Color1        : Guint32;
          Color2        : Guint32);
      pragma Import (C, Internal, "gdk_pixbuf_composite_color");
   begin
      Internal (Get_Object (Self), Get_Object (Dest), Dest_X, Dest_Y, Dest_Width, Dest_Height, Offset_X, Offset_Y, Scale_X, Scale_Y, Interp_Type, Overall_Alpha, Check_X, Check_Y, Check_Size, Color1, Color2);
   end Composite_Color;

   ----------------------------
   -- Composite_Color_Simple --
   ----------------------------

   function Composite_Color_Simple
      (Self          : not null access Gdk_Pixbuf_Record;
       Dest_Width    : Glib.Gint;
       Dest_Height   : Glib.Gint;
       Interp_Type   : Gdk_Interp_Type;
       Overall_Alpha : Glib.Gint;
       Check_Size    : Glib.Gint;
       Color1        : Guint32;
       Color2        : Guint32) return Gdk_Pixbuf
   is
      function Internal
         (Self          : System.Address;
          Dest_Width    : Glib.Gint;
          Dest_Height   : Glib.Gint;
          Interp_Type   : Gdk_Interp_Type;
          Overall_Alpha : Glib.Gint;
          Check_Size    : Glib.Gint;
          Color1        : Guint32;
          Color2        : Guint32) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_composite_color_simple");
      Stub_Gdk_Pixbuf : Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Self), Dest_Width, Dest_Height, Interp_Type, Overall_Alpha, Check_Size, Color1, Color2), Stub_Gdk_Pixbuf));
   end Composite_Color_Simple;

   ----------
   -- Copy --
   ----------

   function Copy
      (Self : not null access Gdk_Pixbuf_Record) return Gdk_Pixbuf
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_copy");
      Stub_Gdk_Pixbuf : Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Pixbuf));
   end Copy;

   ---------------
   -- Copy_Area --
   ---------------

   procedure Copy_Area
      (Self        : not null access Gdk_Pixbuf_Record;
       Src_X       : Glib.Gint;
       Src_Y       : Glib.Gint;
       Width       : Glib.Gint;
       Height      : Glib.Gint;
       Dest_Pixbuf : not null access Gdk_Pixbuf_Record'Class;
       Dest_X      : Glib.Gint;
       Dest_Y      : Glib.Gint)
   is
      procedure Internal
         (Self        : System.Address;
          Src_X       : Glib.Gint;
          Src_Y       : Glib.Gint;
          Width       : Glib.Gint;
          Height      : Glib.Gint;
          Dest_Pixbuf : System.Address;
          Dest_X      : Glib.Gint;
          Dest_Y      : Glib.Gint);
      pragma Import (C, Internal, "gdk_pixbuf_copy_area");
   begin
      Internal (Get_Object (Self), Src_X, Src_Y, Width, Height, Get_Object (Dest_Pixbuf), Dest_X, Dest_Y);
   end Copy_Area;

   ------------------
   -- Copy_Options --
   ------------------

   function Copy_Options
      (Self        : not null access Gdk_Pixbuf_Record;
       Dest_Pixbuf : not null access Gdk_Pixbuf_Record'Class) return Boolean
   is
      function Internal
         (Self        : System.Address;
          Dest_Pixbuf : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_copy_options");
   begin
      return Internal (Get_Object (Self), Get_Object (Dest_Pixbuf)) /= 0;
   end Copy_Options;

   ----------
   -- Fill --
   ----------

   procedure Fill
      (Self  : not null access Gdk_Pixbuf_Record;
       Pixel : Guint32)
   is
      procedure Internal (Self : System.Address; Pixel : Guint32);
      pragma Import (C, Internal, "gdk_pixbuf_fill");
   begin
      Internal (Get_Object (Self), Pixel);
   end Fill;

   ----------
   -- Flip --
   ----------

   function Flip
      (Self       : not null access Gdk_Pixbuf_Record;
       Horizontal : Boolean) return Gdk_Pixbuf
   is
      function Internal
         (Self       : System.Address;
          Horizontal : Glib.Gboolean) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_flip");
      Stub_Gdk_Pixbuf : Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Self), Boolean'Pos (Horizontal)), Stub_Gdk_Pixbuf));
   end Flip;

   -------------------------
   -- Get_Bits_Per_Sample --
   -------------------------

   function Get_Bits_Per_Sample
      (Self : not null access Gdk_Pixbuf_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_pixbuf_get_bits_per_sample");
   begin
      return Internal (Get_Object (Self));
   end Get_Bits_Per_Sample;

   ---------------------
   -- Get_Byte_Length --
   ---------------------

   function Get_Byte_Length
      (Self : not null access Gdk_Pixbuf_Record) return Gsize
   is
      function Internal (Self : System.Address) return Gsize;
      pragma Import (C, Internal, "gdk_pixbuf_get_byte_length");
   begin
      return Internal (Get_Object (Self));
   end Get_Byte_Length;

   --------------------
   -- Get_Colorspace --
   --------------------

   function Get_Colorspace
      (Self : not null access Gdk_Pixbuf_Record) return Gdk_Colorspace
   is
      function Internal (Self : System.Address) return Gdk_Colorspace;
      pragma Import (C, Internal, "gdk_pixbuf_get_colorspace");
   begin
      return Internal (Get_Object (Self));
   end Get_Colorspace;

   -------------------------
   -- Get_File_Info_Async --
   -------------------------

   procedure Get_File_Info_Async
      (Filename    : UTF8_String;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
   begin
      if Callback = null then
         C_Gdk_Pixbuf_Get_File_Info_Async (Tmp_Filename, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
         Free (Tmp_Filename);
      else
         C_Gdk_Pixbuf_Get_File_Info_Async (Tmp_Filename, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
         Free (Tmp_Filename);
      end if;
   end Get_File_Info_Async;

   -------------------
   -- Get_Has_Alpha --
   -------------------

   function Get_Has_Alpha
      (Self : not null access Gdk_Pixbuf_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_get_has_alpha");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Has_Alpha;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
      (Self : not null access Gdk_Pixbuf_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_pixbuf_get_height");
   begin
      return Internal (Get_Object (Self));
   end Get_Height;

   --------------------
   -- Get_N_Channels --
   --------------------

   function Get_N_Channels
      (Self : not null access Gdk_Pixbuf_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_pixbuf_get_n_channels");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Channels;

   ----------------
   -- Get_Option --
   ----------------

   function Get_Option
      (Self : not null access Gdk_Pixbuf_Record;
       Key  : UTF8_String) return UTF8_String
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_pixbuf_get_option");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Gtkada.Bindings.Value_Allowing_Null (Tmp_Return);
   end Get_Option;

   -----------------
   -- Get_Options --
   -----------------

   function Get_Options
      (Self : not null access Gdk_Pixbuf_Record) return System.Address
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_get_options");
   begin
      return Internal (Get_Object (Self));
   end Get_Options;

   ----------------
   -- Get_Pixels --
   ----------------

   function Get_Pixels
      (Self : not null access Gdk_Pixbuf_Record) return System.Address
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_get_pixels");
   begin
      return Internal (Get_Object (Self));
   end Get_Pixels;

   ----------------------------
   -- Get_Pixels_With_Length --
   ----------------------------

   function Get_Pixels_With_Length
      (Self   : not null access Gdk_Pixbuf_Record;
       Length : out Guint) return System.Address
   is
      function Internal
         (Self       : System.Address;
          Acc_Length : access Guint) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_get_pixels_with_length");
      Acc_Length : aliased Guint;
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Length'Access);
      Length := Acc_Length;
      return Tmp_Return;
   end Get_Pixels_With_Length;

   -------------------
   -- Get_Rowstride --
   -------------------

   function Get_Rowstride
      (Self : not null access Gdk_Pixbuf_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_pixbuf_get_rowstride");
   begin
      return Internal (Get_Object (Self));
   end Get_Rowstride;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
      (Self : not null access Gdk_Pixbuf_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_pixbuf_get_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Width;

   ----------------
   -- Load_Async --
   ----------------

   procedure Load_Async
      (Self        : not null access Gdk_Pixbuf_Record;
       Size        : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Loadable_Icon_Load_Async (Get_Object (Self), Size, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Loadable_Icon_Load_Async (Get_Object (Self), Size, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Load_Async;

   ---------------------------
   -- New_From_Stream_Async --
   ---------------------------

   procedure New_From_Stream_Async
      (Stream      : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gdk_Pixbuf_New_From_Stream_Async (Get_Object (Stream), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gdk_Pixbuf_New_From_Stream_Async (Get_Object (Stream), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end New_From_Stream_Async;

   ------------------------------------
   -- New_From_Stream_At_Scale_Async --
   ------------------------------------

   procedure New_From_Stream_At_Scale_Async
      (Stream                : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Cancellable           : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback              : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gdk_Pixbuf_New_From_Stream_At_Scale_Async (Get_Object (Stream), Width, Height, Boolean'Pos (Preserve_Aspect_Ratio), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gdk_Pixbuf_New_From_Stream_At_Scale_Async (Get_Object (Stream), Width, Height, Boolean'Pos (Preserve_Aspect_Ratio), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end New_From_Stream_At_Scale_Async;

   -------------------
   -- New_Subpixbuf --
   -------------------

   function New_Subpixbuf
      (Self   : not null access Gdk_Pixbuf_Record;
       Src_X  : Glib.Gint;
       Src_Y  : Glib.Gint;
       Width  : Glib.Gint;
       Height : Glib.Gint) return Gdk_Pixbuf
   is
      function Internal
         (Self   : System.Address;
          Src_X  : Glib.Gint;
          Src_Y  : Glib.Gint;
          Width  : Glib.Gint;
          Height : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_subpixbuf");
      Stub_Gdk_Pixbuf : Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Self), Src_X, Src_Y, Width, Height), Stub_Gdk_Pixbuf));
   end New_Subpixbuf;

   ----------------------
   -- Read_Pixel_Bytes --
   ----------------------

   function Read_Pixel_Bytes
      (Self : not null access Gdk_Pixbuf_Record) return Glib.Bytes.Gbytes
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_read_pixel_bytes");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Read_Pixel_Bytes;

   -----------------
   -- Read_Pixels --
   -----------------

   function Read_Pixels
      (Self : not null access Gdk_Pixbuf_Record) return System.Address
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_read_pixels");
   begin
      return Internal (Get_Object (Self));
   end Read_Pixels;

   -------------------
   -- Remove_Option --
   -------------------

   function Remove_Option
      (Self : not null access Gdk_Pixbuf_Record;
       Key  : UTF8_String) return Boolean
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_remove_option");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Remove_Option;

   -------------------
   -- Rotate_Simple --
   -------------------

   function Rotate_Simple
      (Self  : not null access Gdk_Pixbuf_Record;
       Angle : Gdk_Pixbuf_Rotation) return Gdk_Pixbuf
   is
      function Internal
         (Self  : System.Address;
          Angle : Gdk_Pixbuf_Rotation) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_rotate_simple");
      Stub_Gdk_Pixbuf : Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Self), Angle), Stub_Gdk_Pixbuf));
   end Rotate_Simple;

   ---------------------------
   -- Saturate_And_Pixelate --
   ---------------------------

   procedure Saturate_And_Pixelate
      (Self       : not null access Gdk_Pixbuf_Record;
       Dest       : not null access Gdk_Pixbuf_Record'Class;
       Saturation : Gfloat;
       Pixelate   : Boolean)
   is
      procedure Internal
         (Self       : System.Address;
          Dest       : System.Address;
          Saturation : Gfloat;
          Pixelate   : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_pixbuf_saturate_and_pixelate");
   begin
      Internal (Get_Object (Self), Get_Object (Dest), Saturation, Boolean'Pos (Pixelate));
   end Saturate_And_Pixelate;

   ---------------------
   -- Save_To_Bufferv --
   ---------------------

   function Save_To_Bufferv
      (Self          : not null access Gdk_Pixbuf_Record;
       Buffer        : out System.Address;
       Buffer_Size   : out Gsize;
       The_Type      : UTF8_String;
       Option_Keys   : GNAT.Strings.String_List;
       Option_Values : GNAT.Strings.String_List;
       Error         : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self            : System.Address;
          Acc_Buffer      : access System.Address;
          Acc_Buffer_Size : access Gsize;
          The_Type        : Gtkada.Types.Chars_Ptr;
          Option_Keys     : Gtkada.Types.chars_ptr_array;
          Option_Values   : Gtkada.Types.chars_ptr_array;
          Acc_Error       : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_save_to_bufferv");
      Acc_Buffer        : aliased System.Address;
      Acc_Buffer_Size   : aliased Gsize;
      Acc_Error         : aliased Glib.Error.GError;
      Tmp_The_Type      : Gtkada.Types.Chars_Ptr := New_String (The_Type);
      Tmp_Option_Keys   : Gtkada.Types.chars_ptr_array := From_String_List (Option_Keys);
      Tmp_Option_Values : Gtkada.Types.chars_ptr_array := From_String_List (Option_Values);
      Tmp_Return        : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Buffer'Access, Acc_Buffer_Size'Access, Tmp_The_Type, Tmp_Option_Keys, Tmp_Option_Values, Acc_Error'Access);
      Buffer := Acc_Buffer;
      Buffer_Size := Acc_Buffer_Size;
      Error := Acc_Error;
      Gtkada.Types.Free (Tmp_Option_Values);
      Gtkada.Types.Free (Tmp_Option_Keys);
      Free (Tmp_The_Type);
      return Tmp_Return /= 0;
   end Save_To_Bufferv;

   ---------------------
   -- Save_To_Streamv --
   ---------------------

   function Save_To_Streamv
      (Self          : not null access Gdk_Pixbuf_Record;
       Stream        : not null access Glib.Output_Stream.Goutput_Stream_Record'Class;
       The_Type      : UTF8_String;
       Option_Keys   : GNAT.Strings.String_List;
       Option_Values : GNAT.Strings.String_List;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class;
       Error         : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self          : System.Address;
          Stream        : System.Address;
          The_Type      : Gtkada.Types.Chars_Ptr;
          Option_Keys   : Gtkada.Types.chars_ptr_array;
          Option_Values : Gtkada.Types.chars_ptr_array;
          Cancellable   : System.Address;
          Acc_Error     : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_save_to_streamv");
      Acc_Error         : aliased Glib.Error.GError;
      Tmp_The_Type      : Gtkada.Types.Chars_Ptr := New_String (The_Type);
      Tmp_Option_Keys   : Gtkada.Types.chars_ptr_array := From_String_List (Option_Keys);
      Tmp_Option_Values : Gtkada.Types.chars_ptr_array := From_String_List (Option_Values);
      Tmp_Return        : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Get_Object (Stream), Tmp_The_Type, Tmp_Option_Keys, Tmp_Option_Values, Get_Object_Or_Null (GObject (Cancellable)), Acc_Error'Access);
      Error := Acc_Error;
      Gtkada.Types.Free (Tmp_Option_Values);
      Gtkada.Types.Free (Tmp_Option_Keys);
      Free (Tmp_The_Type);
      return Tmp_Return /= 0;
   end Save_To_Streamv;

   ---------------------------
   -- Save_To_Streamv_Async --
   ---------------------------

   procedure Save_To_Streamv_Async
      (Self          : not null access Gdk_Pixbuf_Record;
       Stream        : not null access Glib.Output_Stream.Goutput_Stream_Record'Class;
       The_Type      : UTF8_String;
       Option_Keys   : GNAT.Strings.String_List;
       Option_Values : GNAT.Strings.String_List;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback      : Gasync_Ready_Callback)
   is
      Tmp_The_Type      : Gtkada.Types.Chars_Ptr := New_String (The_Type);
      Tmp_Option_Keys   : Gtkada.Types.chars_ptr_array := From_String_List (Option_Keys);
      Tmp_Option_Values : Gtkada.Types.chars_ptr_array := From_String_List (Option_Values);
   begin
      if Callback = null then
         C_Gdk_Pixbuf_Save_To_Streamv_Async (Get_Object (Self), Get_Object (Stream), Tmp_The_Type, Tmp_Option_Keys, Tmp_Option_Values, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
         Gtkada.Types.Free (Tmp_Option_Values);
         Gtkada.Types.Free (Tmp_Option_Keys);
         Free (Tmp_The_Type);
      else
         C_Gdk_Pixbuf_Save_To_Streamv_Async (Get_Object (Self), Get_Object (Stream), Tmp_The_Type, Tmp_Option_Keys, Tmp_Option_Values, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
         Gtkada.Types.Free (Tmp_Option_Values);
         Gtkada.Types.Free (Tmp_Option_Keys);
         Free (Tmp_The_Type);
      end if;
   end Save_To_Streamv_Async;

   -----------
   -- Savev --
   -----------

   function Savev
      (Self          : not null access Gdk_Pixbuf_Record;
       Filename      : UTF8_String;
       The_Type      : UTF8_String;
       Option_Keys   : GNAT.Strings.String_List;
       Option_Values : GNAT.Strings.String_List;
       Error         : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self          : System.Address;
          Filename      : Gtkada.Types.Chars_Ptr;
          The_Type      : Gtkada.Types.Chars_Ptr;
          Option_Keys   : Gtkada.Types.chars_ptr_array;
          Option_Values : Gtkada.Types.chars_ptr_array;
          Acc_Error     : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_savev");
      Acc_Error         : aliased Glib.Error.GError;
      Tmp_Filename      : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_The_Type      : Gtkada.Types.Chars_Ptr := New_String (The_Type);
      Tmp_Option_Keys   : Gtkada.Types.chars_ptr_array := From_String_List (Option_Keys);
      Tmp_Option_Values : Gtkada.Types.chars_ptr_array := From_String_List (Option_Values);
      Tmp_Return        : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Filename, Tmp_The_Type, Tmp_Option_Keys, Tmp_Option_Values, Acc_Error'Access);
      Error := Acc_Error;
      Gtkada.Types.Free (Tmp_Option_Values);
      Gtkada.Types.Free (Tmp_Option_Keys);
      Free (Tmp_The_Type);
      Free (Tmp_Filename);
      return Tmp_Return /= 0;
   end Savev;

   -----------
   -- Scale --
   -----------

   procedure Scale
      (Self        : not null access Gdk_Pixbuf_Record;
       Dest        : not null access Gdk_Pixbuf_Record'Class;
       Dest_X      : Glib.Gint;
       Dest_Y      : Glib.Gint;
       Dest_Width  : Glib.Gint;
       Dest_Height : Glib.Gint;
       Offset_X    : Gdouble;
       Offset_Y    : Gdouble;
       Scale_X     : Gdouble;
       Scale_Y     : Gdouble;
       Interp_Type : Gdk_Interp_Type)
   is
      procedure Internal
         (Self        : System.Address;
          Dest        : System.Address;
          Dest_X      : Glib.Gint;
          Dest_Y      : Glib.Gint;
          Dest_Width  : Glib.Gint;
          Dest_Height : Glib.Gint;
          Offset_X    : Gdouble;
          Offset_Y    : Gdouble;
          Scale_X     : Gdouble;
          Scale_Y     : Gdouble;
          Interp_Type : Gdk_Interp_Type);
      pragma Import (C, Internal, "gdk_pixbuf_scale");
   begin
      Internal (Get_Object (Self), Get_Object (Dest), Dest_X, Dest_Y, Dest_Width, Dest_Height, Offset_X, Offset_Y, Scale_X, Scale_Y, Interp_Type);
   end Scale;

   ------------------
   -- Scale_Simple --
   ------------------

   function Scale_Simple
      (Self        : not null access Gdk_Pixbuf_Record;
       Dest_Width  : Glib.Gint;
       Dest_Height : Glib.Gint;
       Interp_Type : Gdk_Interp_Type) return Gdk_Pixbuf
   is
      function Internal
         (Self        : System.Address;
          Dest_Width  : Glib.Gint;
          Dest_Height : Glib.Gint;
          Interp_Type : Gdk_Interp_Type) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_scale_simple");
      Stub_Gdk_Pixbuf : Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Self), Dest_Width, Dest_Height, Interp_Type), Stub_Gdk_Pixbuf));
   end Scale_Simple;

   ----------------
   -- Set_Option --
   ----------------

   function Set_Option
      (Self  : not null access Gdk_Pixbuf_Record;
       Key   : UTF8_String;
       Value : UTF8_String) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Key   : Gtkada.Types.Chars_Ptr;
          Value : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_set_option");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Value  : Gtkada.Types.Chars_Ptr := New_String (Value);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Tmp_Value);
      Free (Tmp_Value);
      Free (Tmp_Key);
      return Tmp_Return /= 0;
   end Set_Option;

   ----------
   -- Load --
   ----------

   function Load
      (Self        : not null access Gdk_Pixbuf_Record;
       Size        : Glib.Gint;
       The_Type    : access UTF8_String := null;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Error       : out Glib.Error.GError)
       return Glib.Input_Stream.Ginput_Stream
   is
      function Internal
         (Self        : System.Address;
          Size        : Glib.Gint;
          The_Type    : access Gtkada.Types.Chars_Ptr;
          Cancellable : System.Address;
          Acc_Error   : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "g_loadable_icon_load");
      Acc_Error          : aliased Glib.Error.GError;
      Return_Obj         : Glib.Input_Stream.Ginput_Stream;
      Tmp_The_Type       : aliased Gtkada.Types.Chars_Ptr;
      Acc_The_Type       : constant access Gtkada.Types.Chars_Ptr := (if The_Type /= null then Tmp_The_Type'Access else null);
      Stub_Ginput_Stream : Glib.Input_Stream.Ginput_Stream_Record;
      Tmp_Return         : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Size, Acc_The_Type, Get_Object_Or_Null (GObject (Cancellable)), Acc_Error'Access);
      if The_Type /= null then
         The_Type.all := Gtkada.Bindings.Value_Allowing_Null (Tmp_The_Type);
      end if;
      Error := Acc_Error;
      if Error = null then
         Return_Obj := Glib.Input_Stream.Ginput_Stream (Get_User_Data (Tmp_Return, Stub_Ginput_Stream));
      end if;
      return Return_Obj;
   end Load;

   -----------------
   -- Load_Finish --
   -----------------

   function Load_Finish
      (Self     : not null access Gdk_Pixbuf_Record;
       Res      : Glib.G_Async_Result;
       The_Type : access UTF8_String := null;
       Error    : out Glib.Error.GError)
       return Glib.Input_Stream.Ginput_Stream
   is
      function Internal
         (Self      : System.Address;
          Res       : Glib.G_Async_Result;
          The_Type  : access Gtkada.Types.Chars_Ptr;
          Acc_Error : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "g_loadable_icon_load_finish");
      Acc_Error          : aliased Glib.Error.GError;
      Return_Obj         : Glib.Input_Stream.Ginput_Stream;
      Tmp_The_Type       : aliased Gtkada.Types.Chars_Ptr;
      Acc_The_Type       : constant access Gtkada.Types.Chars_Ptr := (if The_Type /= null then Tmp_The_Type'Access else null);
      Stub_Ginput_Stream : Glib.Input_Stream.Ginput_Stream_Record;
      Tmp_Return         : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Res, Acc_The_Type, Acc_Error'Access);
      if The_Type /= null then
         The_Type.all := Gtkada.Bindings.Value_Allowing_Null (Tmp_The_Type);
      end if;
      Error := Acc_Error;
      if Error = null then
         Return_Obj := Glib.Input_Stream.Ginput_Stream (Get_User_Data (Tmp_Return, Stub_Ginput_Stream));
      end if;
      return Return_Obj;
   end Load_Finish;

   -------------------------
   -- Calculate_Rowstride --
   -------------------------

   function Calculate_Rowstride
      (Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint) return Glib.Gint
   is
      function Internal
         (Colorspace      : Gdk_Colorspace;
          Has_Alpha       : Glib.Gboolean;
          Bits_Per_Sample : Glib.Gint;
          Width           : Glib.Gint;
          Height          : Glib.Gint) return Glib.Gint;
      pragma Import (C, Internal, "gdk_pixbuf_calculate_rowstride");
   begin
      return Internal (Colorspace, Boolean'Pos (Has_Alpha), Bits_Per_Sample, Width, Height);
   end Calculate_Rowstride;

   -------------------
   -- Get_File_Info --
   -------------------

   function Get_File_Info
      (Filename : UTF8_String;
       Width    : access Glib.Gint := null;
       Height   : access Glib.Gint := null)
       return Gdk.Pixbuf_Format.Gdk_Pixbuf_Format_Access
   is
      function Internal
         (Filename : Gtkada.Types.Chars_Ptr;
          Width    : access Glib.Gint;
          Height   : access Glib.Gint)
          return Gdk.Pixbuf_Format.Gdk_Pixbuf_Format_Access;
      pragma Import (C, Internal, "gdk_pixbuf_get_file_info");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : Gdk.Pixbuf_Format.Gdk_Pixbuf_Format_Access;
   begin
      Tmp_Return := Internal (Tmp_Filename, Width, Height);
      Free (Tmp_Filename);
      return Tmp_Return;
   end Get_File_Info;

   --------------------------
   -- Get_File_Info_Finish --
   --------------------------

   function Get_File_Info_Finish
      (Async_Result : Glib.G_Async_Result;
       Width        : out Glib.Gint;
       Height       : out Glib.Gint;
       Error        : out Glib.Error.GError)
       return Gdk.Pixbuf_Format.Gdk_Pixbuf_Format_Access
   is
      function Internal
         (Async_Result : Glib.G_Async_Result;
          Acc_Width    : access Glib.Gint;
          Acc_Height   : access Glib.Gint;
          Acc_Error    : access Glib.Error.GError)
          return Gdk.Pixbuf_Format.Gdk_Pixbuf_Format_Access;
      pragma Import (C, Internal, "gdk_pixbuf_get_file_info_finish");
      Acc_Width  : aliased Glib.Gint;
      Acc_Height : aliased Glib.Gint;
      Acc_Error  : aliased Glib.Error.GError;
      Return_Obj : Gdk.Pixbuf_Format.Gdk_Pixbuf_Format_Access;
      Tmp_Return : Gdk.Pixbuf_Format.Gdk_Pixbuf_Format_Access;
   begin
      Tmp_Return := Internal (Async_Result, Acc_Width'Access, Acc_Height'Access, Acc_Error'Access);
      Width := Acc_Width;
      Height := Acc_Height;
      Error := Acc_Error;
      if Error = null then
         Return_Obj := Tmp_Return;
      end if;
      return Return_Obj;
   end Get_File_Info_Finish;

   -----------------
   -- Get_Formats --
   -----------------

   function Get_Formats return Gdk.Pixbuf_Format.Format_List.GSlist is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_get_formats");
      Tmp_Return : Gdk.Pixbuf_Format.Format_List.GSlist;
   begin
      Gdk.Pixbuf_Format.Format_List.Set_Object (Tmp_Return, Internal);
      return Tmp_Return;
   end Get_Formats;

   ------------------
   -- Init_Modules --
   ------------------

   function Init_Modules
      (Path  : UTF8_String;
       Error : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Path      : Gtkada.Types.Chars_Ptr;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_init_modules");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Tmp_Path, Acc_Error'Access);
      Error := Acc_Error;
      Free (Tmp_Path);
      return Tmp_Return /= 0;
   end Init_Modules;

   ---------------------------
   -- Save_To_Stream_Finish --
   ---------------------------

   function Save_To_Stream_Finish
      (Async_Result : Glib.G_Async_Result;
       Error        : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Async_Result : Glib.G_Async_Result;
          Acc_Error    : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_save_to_stream_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Async_Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Save_To_Stream_Finish;

end Gdk.Pixbuf;
