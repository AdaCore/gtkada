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

package body Gdk.Dmabuf_Texture is

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

   package Type_Conversion_Gdk_Dmabuf_Texture is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Dmabuf_Texture_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Dmabuf_Texture);

   ----------------
   -- Load_Async --
   ----------------

   procedure Load_Async
      (Self        : not null access Gdk_Dmabuf_Texture_Record;
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
   -- Compute_Concrete_Size --
   ---------------------------

   procedure Compute_Concrete_Size
      (Self             : not null access Gdk_Dmabuf_Texture_Record;
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
      (Self : not null access Gdk_Dmabuf_Texture_Record)
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
      (Self : not null access Gdk_Dmabuf_Texture_Record)
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
      (Self : not null access Gdk_Dmabuf_Texture_Record) return Gdouble
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
      (Self : not null access Gdk_Dmabuf_Texture_Record) return Glib.Gint
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
      (Self : not null access Gdk_Dmabuf_Texture_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_paintable_get_intrinsic_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Intrinsic_Width;

   -------------------------
   -- Invalidate_Contents --
   -------------------------

   procedure Invalidate_Contents
      (Self : not null access Gdk_Dmabuf_Texture_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_paintable_invalidate_contents");
   begin
      Internal (Get_Object (Self));
   end Invalidate_Contents;

   ---------------------
   -- Invalidate_Size --
   ---------------------

   procedure Invalidate_Size
      (Self : not null access Gdk_Dmabuf_Texture_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_paintable_invalidate_size");
   begin
      Internal (Get_Object (Self));
   end Invalidate_Size;

   --------------
   -- Snapshot --
   --------------

   procedure Snapshot
      (Self     : not null access Gdk_Dmabuf_Texture_Record;
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

end Gdk.Dmabuf_Texture;
