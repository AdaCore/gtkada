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
with System;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gdk.Texture is

   procedure C_G_Loadable_Icon_Load_Async
      (Self        : System.Address;
       Size        : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_Loadable_Icon_Load_Async, "g_loadable_icon_load_async");
   --  Loads an icon asynchronously. To finish this function, see
   --  g_loadable_icon_load_finish. For the synchronous, blocking version of
   --  this function, see g_loadable_icon_load.
   --  @param Size an integer.
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

   package Type_Conversion_Gdk_Texture is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Texture_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Texture);

   ------------------------
   -- Gdk_New_From_Bytes --
   ------------------------

   procedure Gdk_New_From_Bytes
      (Self  : out Gdk_Texture;
       Bytes : Glib.Bytes.Gbytes)
   is
   begin
      Self := new Gdk_Texture_Record;
      Gdk.Texture.Initialize_From_Bytes (Self, Bytes);
   end Gdk_New_From_Bytes;

   ---------------------------
   -- Gdk_New_From_Filename --
   ---------------------------

   procedure Gdk_New_From_Filename
      (Self : out Gdk_Texture;
       Path : UTF8_String)
   is
   begin
      Self := new Gdk_Texture_Record;
      Gdk.Texture.Initialize_From_Filename (Self, Path);
   end Gdk_New_From_Filename;

   ---------------------------
   -- Gdk_New_From_Resource --
   ---------------------------

   procedure Gdk_New_From_Resource
      (Self          : out Gdk_Texture;
       Resource_Path : UTF8_String)
   is
   begin
      Self := new Gdk_Texture_Record;
      Gdk.Texture.Initialize_From_Resource (Self, Resource_Path);
   end Gdk_New_From_Resource;

   --------------------------------
   -- Gdk_Texture_New_From_Bytes --
   --------------------------------

   function Gdk_Texture_New_From_Bytes
      (Bytes : Glib.Bytes.Gbytes) return Gdk_Texture
   is
      Self : constant Gdk_Texture := new Gdk_Texture_Record;
   begin
      Gdk.Texture.Initialize_From_Bytes (Self, Bytes);
      return Self;
   end Gdk_Texture_New_From_Bytes;

   -----------------------------------
   -- Gdk_Texture_New_From_Filename --
   -----------------------------------

   function Gdk_Texture_New_From_Filename
      (Path : UTF8_String) return Gdk_Texture
   is
      Self : constant Gdk_Texture := new Gdk_Texture_Record;
   begin
      Gdk.Texture.Initialize_From_Filename (Self, Path);
      return Self;
   end Gdk_Texture_New_From_Filename;

   -----------------------------------
   -- Gdk_Texture_New_From_Resource --
   -----------------------------------

   function Gdk_Texture_New_From_Resource
      (Resource_Path : UTF8_String) return Gdk_Texture
   is
      Self : constant Gdk_Texture := new Gdk_Texture_Record;
   begin
      Gdk.Texture.Initialize_From_Resource (Self, Resource_Path);
      return Self;
   end Gdk_Texture_New_From_Resource;

   ---------------------------
   -- Initialize_From_Bytes --
   ---------------------------

   procedure Initialize_From_Bytes
      (Self  : not null access Gdk_Texture_Record'Class;
       Bytes : Glib.Bytes.Gbytes)
   is
      function Internal (Bytes : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_texture_new_from_bytes");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Bytes)));
      end if;
   end Initialize_From_Bytes;

   ------------------------------
   -- Initialize_From_Filename --
   ------------------------------

   procedure Initialize_From_Filename
      (Self : not null access Gdk_Texture_Record'Class;
       Path : UTF8_String)
   is
      function Internal
         (Path : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gdk_texture_new_from_filename");
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Path);
         Free (Tmp_Path);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_From_Filename;

   ------------------------------
   -- Initialize_From_Resource --
   ------------------------------

   procedure Initialize_From_Resource
      (Self          : not null access Gdk_Texture_Record'Class;
       Resource_Path : UTF8_String)
   is
      function Internal
         (Resource_Path : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gdk_texture_new_from_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
      Tmp_Return        : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Resource_Path);
         Free (Tmp_Resource_Path);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_From_Resource;

   --------------
   -- Download --
   --------------

   procedure Download
      (Self   : not null access Gdk_Texture_Record;
       Data   : Gint_Array;
       Stride : Gsize)
   is
      procedure Internal
         (Self   : System.Address;
          Data   : System.Address;
          Stride : Gsize);
      pragma Import (C, Internal, "gdk_texture_download");
   begin
      Internal (Get_Object (Self), Data (Data'First)'Address, Stride);
   end Download;

   ---------------------
   -- Get_Color_State --
   ---------------------

   function Get_Color_State
      (Self : not null access Gdk_Texture_Record)
       return Gdk.Color_State.Gdk_Color_State
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_texture_get_color_state");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Color_State;

   ----------------
   -- Get_Format --
   ----------------

   function Get_Format
      (Self : not null access Gdk_Texture_Record) return Gdk_Memory_Format
   is
      function Internal (Self : System.Address) return Gdk_Memory_Format;
      pragma Import (C, Internal, "gdk_texture_get_format");
   begin
      return Internal (Get_Object (Self));
   end Get_Format;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
      (Self : not null access Gdk_Texture_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_texture_get_height");
   begin
      return Internal (Get_Object (Self));
   end Get_Height;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
      (Self : not null access Gdk_Texture_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_texture_get_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Width;

   ----------------
   -- Load_Async --
   ----------------

   procedure Load_Async
      (Self        : not null access Gdk_Texture_Record;
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

   -----------------
   -- Save_To_Png --
   -----------------

   function Save_To_Png
      (Self     : not null access Gdk_Texture_Record;
       Filename : UTF8_String) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Filename : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_texture_save_to_png");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Filename);
      Free (Tmp_Filename);
      return Tmp_Return /= 0;
   end Save_To_Png;

   -----------------------
   -- Save_To_Png_Bytes --
   -----------------------

   function Save_To_Png_Bytes
      (Self : not null access Gdk_Texture_Record) return Glib.Bytes.Gbytes
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_texture_save_to_png_bytes");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Save_To_Png_Bytes;

   ------------------
   -- Save_To_Tiff --
   ------------------

   function Save_To_Tiff
      (Self     : not null access Gdk_Texture_Record;
       Filename : UTF8_String) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Filename : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_texture_save_to_tiff");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Filename);
      Free (Tmp_Filename);
      return Tmp_Return /= 0;
   end Save_To_Tiff;

   ------------------------
   -- Save_To_Tiff_Bytes --
   ------------------------

   function Save_To_Tiff_Bytes
      (Self : not null access Gdk_Texture_Record) return Glib.Bytes.Gbytes
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_texture_save_to_tiff_bytes");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Save_To_Tiff_Bytes;

   ---------------------------
   -- Compute_Concrete_Size --
   ---------------------------

   procedure Compute_Concrete_Size
      (Self             : not null access Gdk_Texture_Record;
       Specified_Width  : Gdouble;
       Specified_Height : Gdouble;
       Default_Width    : Gdouble;
       Default_Height   : Gdouble;
       Concrete_Width   : out Gdouble;
       Concrete_Height  : out Gdouble)
   is
      procedure Internal
         (Self             : System.Address;
          Specified_Width  : Gdouble;
          Specified_Height : Gdouble;
          Default_Width    : Gdouble;
          Default_Height   : Gdouble;
          Concrete_Width   : out Gdouble;
          Concrete_Height  : out Gdouble);
      pragma Import (C, Internal, "gdk_paintable_compute_concrete_size");
   begin
      Internal (Get_Object (Self), Specified_Width, Specified_Height, Default_Width, Default_Height, Concrete_Width, Concrete_Height);
   end Compute_Concrete_Size;

   -----------------------
   -- Get_Current_Image --
   -----------------------

   function Get_Current_Image
      (Self : not null access Gdk_Texture_Record)
       return Gdk.Paintable.Gdk_Paintable
   is
      function Internal
         (Self : System.Address) return Gdk.Paintable.Gdk_Paintable;
      pragma Import (C, Internal, "gdk_paintable_get_current_image");
   begin
      return Internal (Get_Object (Self));
   end Get_Current_Image;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
      (Self : not null access Gdk_Texture_Record)
       return Gdk.Paintable.Gdk_Paintable_Flags
   is
      function Internal
         (Self : System.Address) return Gdk.Paintable.Gdk_Paintable_Flags;
      pragma Import (C, Internal, "gdk_paintable_get_flags");
   begin
      return Internal (Get_Object (Self));
   end Get_Flags;

   --------------------------------
   -- Get_Intrinsic_Aspect_Ratio --
   --------------------------------

   function Get_Intrinsic_Aspect_Ratio
      (Self : not null access Gdk_Texture_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gdk_paintable_get_intrinsic_aspect_ratio");
   begin
      return Internal (Get_Object (Self));
   end Get_Intrinsic_Aspect_Ratio;

   --------------------------
   -- Get_Intrinsic_Height --
   --------------------------

   function Get_Intrinsic_Height
      (Self : not null access Gdk_Texture_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_paintable_get_intrinsic_height");
   begin
      return Internal (Get_Object (Self));
   end Get_Intrinsic_Height;

   -------------------------
   -- Get_Intrinsic_Width --
   -------------------------

   function Get_Intrinsic_Width
      (Self : not null access Gdk_Texture_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_paintable_get_intrinsic_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Intrinsic_Width;

   -------------------------
   -- Invalidate_Contents --
   -------------------------

   procedure Invalidate_Contents (Self : not null access Gdk_Texture_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_paintable_invalidate_contents");
   begin
      Internal (Get_Object (Self));
   end Invalidate_Contents;

   ---------------------
   -- Invalidate_Size --
   ---------------------

   procedure Invalidate_Size (Self : not null access Gdk_Texture_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_paintable_invalidate_size");
   begin
      Internal (Get_Object (Self));
   end Invalidate_Size;

   --------------
   -- Snapshot --
   --------------

   procedure Snapshot
      (Self     : not null access Gdk_Texture_Record;
       Snapshot : not null access Gdk.Snapshot.Gdk_Snapshot_Record'Class;
       Width    : Gdouble;
       Height   : Gdouble)
   is
      procedure Internal
         (Self     : System.Address;
          Snapshot : System.Address;
          Width    : Gdouble;
          Height   : Gdouble);
      pragma Import (C, Internal, "gdk_paintable_snapshot");
   begin
      Internal (Get_Object (Self), Get_Object (Snapshot), Width, Height);
   end Snapshot;

end Gdk.Texture;
