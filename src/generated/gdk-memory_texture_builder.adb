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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with System;

package body Gdk.Memory_Texture_Builder is

   package Type_Conversion_Gdk_Memory_Texture_Builder is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Memory_Texture_Builder_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Memory_Texture_Builder);

   ------------------------------------
   -- Gdk_Memory_Texture_Builder_New --
   ------------------------------------

   function Gdk_Memory_Texture_Builder_New return Gdk_Memory_Texture_Builder is
      Self : constant Gdk_Memory_Texture_Builder := new Gdk_Memory_Texture_Builder_Record;
   begin
      Gdk.Memory_Texture_Builder.Initialize (Self);
      return Self;
   end Gdk_Memory_Texture_Builder_New;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Self : out Gdk_Memory_Texture_Builder) is
   begin
      Self := new Gdk_Memory_Texture_Builder_Record;
      Gdk.Memory_Texture_Builder.Initialize (Self);
   end Gdk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gdk_Memory_Texture_Builder_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_memory_texture_builder_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------
   -- Build --
   -----------

   function Build
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Gdk.Texture.Gdk_Texture
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_memory_texture_builder_build");
      Stub_Gdk_Texture : Gdk.Texture.Gdk_Texture_Record;
   begin
      return Gdk.Texture.Gdk_Texture (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Texture));
   end Build;

   ---------------
   -- Get_Bytes --
   ---------------

   function Get_Bytes
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Glib.Bytes.Gbytes
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_memory_texture_builder_get_bytes");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Bytes;

   ---------------------
   -- Get_Color_State --
   ---------------------

   function Get_Color_State
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Gdk.Color_State.Gdk_Color_State
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_memory_texture_builder_get_color_state");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Color_State;

   ----------------
   -- Get_Format --
   ----------------

   function Get_Format
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Gdk.Texture.Gdk_Memory_Format
   is
      function Internal
         (Self : System.Address) return Gdk.Texture.Gdk_Memory_Format;
      pragma Import (C, Internal, "gdk_memory_texture_builder_get_format");
   begin
      return Internal (Get_Object (Self));
   end Get_Format;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_memory_texture_builder_get_height");
   begin
      return Internal (Get_Object (Self));
   end Get_Height;

   ----------------
   -- Get_Offset --
   ----------------

   function Get_Offset
      (Self  : not null access Gdk_Memory_Texture_Builder_Record;
       Plane : Guint) return Gsize
   is
      function Internal (Self : System.Address; Plane : Guint) return Gsize;
      pragma Import (C, Internal, "gdk_memory_texture_builder_get_offset");
   begin
      return Internal (Get_Object (Self), Plane);
   end Get_Offset;

   ----------------
   -- Get_Stride --
   ----------------

   function Get_Stride
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Gsize
   is
      function Internal (Self : System.Address) return Gsize;
      pragma Import (C, Internal, "gdk_memory_texture_builder_get_stride");
   begin
      return Internal (Get_Object (Self));
   end Get_Stride;

   --------------------------
   -- Get_Stride_For_Plane --
   --------------------------

   function Get_Stride_For_Plane
      (Self  : not null access Gdk_Memory_Texture_Builder_Record;
       Plane : Guint) return Gsize
   is
      function Internal (Self : System.Address; Plane : Guint) return Gsize;
      pragma Import (C, Internal, "gdk_memory_texture_builder_get_stride_for_plane");
   begin
      return Internal (Get_Object (Self), Plane);
   end Get_Stride_For_Plane;

   -----------------------
   -- Get_Update_Region --
   -----------------------

   function Get_Update_Region
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Cairo.Region.Cairo_Region
   is
      function Internal
         (Self : System.Address) return Cairo.Region.Cairo_Region;
      pragma Import (C, Internal, "gdk_memory_texture_builder_get_update_region");
   begin
      return Internal (Get_Object (Self));
   end Get_Update_Region;

   ------------------------
   -- Get_Update_Texture --
   ------------------------

   function Get_Update_Texture
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Gdk.Texture.Gdk_Texture
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_memory_texture_builder_get_update_texture");
      Stub_Gdk_Texture : Gdk.Texture.Gdk_Texture_Record;
   begin
      return Gdk.Texture.Gdk_Texture (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Texture));
   end Get_Update_Texture;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_memory_texture_builder_get_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Width;

   ---------------
   -- Set_Bytes --
   ---------------

   procedure Set_Bytes
      (Self  : not null access Gdk_Memory_Texture_Builder_Record;
       Bytes : Glib.Bytes.Gbytes)
   is
      procedure Internal (Self : System.Address; Bytes : System.Address);
      pragma Import (C, Internal, "gdk_memory_texture_builder_set_bytes");
   begin
      Internal (Get_Object (Self), Get_Object (Bytes));
   end Set_Bytes;

   ---------------------
   -- Set_Color_State --
   ---------------------

   procedure Set_Color_State
      (Self        : not null access Gdk_Memory_Texture_Builder_Record;
       Color_State : Gdk.Color_State.Gdk_Color_State)
   is
      procedure Internal
         (Self        : System.Address;
          Color_State : System.Address);
      pragma Import (C, Internal, "gdk_memory_texture_builder_set_color_state");
   begin
      Internal (Get_Object (Self), Get_Object (Color_State));
   end Set_Color_State;

   ----------------
   -- Set_Format --
   ----------------

   procedure Set_Format
      (Self   : not null access Gdk_Memory_Texture_Builder_Record;
       Format : Gdk.Texture.Gdk_Memory_Format)
   is
      procedure Internal
         (Self   : System.Address;
          Format : Gdk.Texture.Gdk_Memory_Format);
      pragma Import (C, Internal, "gdk_memory_texture_builder_set_format");
   begin
      Internal (Get_Object (Self), Format);
   end Set_Format;

   ----------------
   -- Set_Height --
   ----------------

   procedure Set_Height
      (Self   : not null access Gdk_Memory_Texture_Builder_Record;
       Height : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Height : Glib.Gint);
      pragma Import (C, Internal, "gdk_memory_texture_builder_set_height");
   begin
      Internal (Get_Object (Self), Height);
   end Set_Height;

   ----------------
   -- Set_Offset --
   ----------------

   procedure Set_Offset
      (Self   : not null access Gdk_Memory_Texture_Builder_Record;
       Plane  : Guint;
       Offset : Gsize)
   is
      procedure Internal
         (Self   : System.Address;
          Plane  : Guint;
          Offset : Gsize);
      pragma Import (C, Internal, "gdk_memory_texture_builder_set_offset");
   begin
      Internal (Get_Object (Self), Plane, Offset);
   end Set_Offset;

   ----------------
   -- Set_Stride --
   ----------------

   procedure Set_Stride
      (Self   : not null access Gdk_Memory_Texture_Builder_Record;
       Stride : Gsize)
   is
      procedure Internal (Self : System.Address; Stride : Gsize);
      pragma Import (C, Internal, "gdk_memory_texture_builder_set_stride");
   begin
      Internal (Get_Object (Self), Stride);
   end Set_Stride;

   --------------------------
   -- Set_Stride_For_Plane --
   --------------------------

   procedure Set_Stride_For_Plane
      (Self   : not null access Gdk_Memory_Texture_Builder_Record;
       Plane  : Guint;
       Stride : Gsize)
   is
      procedure Internal
         (Self   : System.Address;
          Plane  : Guint;
          Stride : Gsize);
      pragma Import (C, Internal, "gdk_memory_texture_builder_set_stride_for_plane");
   begin
      Internal (Get_Object (Self), Plane, Stride);
   end Set_Stride_For_Plane;

   -----------------------
   -- Set_Update_Region --
   -----------------------

   procedure Set_Update_Region
      (Self   : not null access Gdk_Memory_Texture_Builder_Record;
       Region : Cairo.Region.Cairo_Region)
   is
      procedure Internal
         (Self   : System.Address;
          Region : Cairo.Region.Cairo_Region);
      pragma Import (C, Internal, "gdk_memory_texture_builder_set_update_region");
   begin
      Internal (Get_Object (Self), Region);
   end Set_Update_Region;

   ------------------------
   -- Set_Update_Texture --
   ------------------------

   procedure Set_Update_Texture
      (Self    : not null access Gdk_Memory_Texture_Builder_Record;
       Texture : access Gdk.Texture.Gdk_Texture_Record'Class)
   is
      procedure Internal (Self : System.Address; Texture : System.Address);
      pragma Import (C, Internal, "gdk_memory_texture_builder_set_update_texture");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Texture)));
   end Set_Update_Texture;

   ---------------
   -- Set_Width --
   ---------------

   procedure Set_Width
      (Self  : not null access Gdk_Memory_Texture_Builder_Record;
       Width : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Width : Glib.Gint);
      pragma Import (C, Internal, "gdk_memory_texture_builder_set_width");
   begin
      Internal (Get_Object (Self), Width);
   end Set_Width;

end Gdk.Memory_Texture_Builder;
