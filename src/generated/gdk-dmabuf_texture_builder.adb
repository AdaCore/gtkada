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

package body Gdk.Dmabuf_Texture_Builder is

   package Type_Conversion_Gdk_Dmabuf_Texture_Builder is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Dmabuf_Texture_Builder_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Dmabuf_Texture_Builder);

   ------------------------------------
   -- Gdk_Dmabuf_Texture_Builder_New --
   ------------------------------------

   function Gdk_Dmabuf_Texture_Builder_New return Gdk_Dmabuf_Texture_Builder is
      Self : constant Gdk_Dmabuf_Texture_Builder := new Gdk_Dmabuf_Texture_Builder_Record;
   begin
      Gdk.Dmabuf_Texture_Builder.Initialize (Self);
      return Self;
   end Gdk_Dmabuf_Texture_Builder_New;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Self : out Gdk_Dmabuf_Texture_Builder) is
   begin
      Self := new Gdk_Dmabuf_Texture_Builder_Record;
      Gdk.Dmabuf_Texture_Builder.Initialize (Self);
   end Gdk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------
   -- Build --
   -----------

   function Build
      (Self    : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Destroy : Glib.G_Destroy_Notify_Address;
       Data    : System.Address) return Gdk.Texture.Gdk_Texture
   is
      function Internal
         (Self    : System.Address;
          Destroy : Glib.G_Destroy_Notify_Address;
          Data    : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_build");
      Stub_Gdk_Texture : Gdk.Texture.Gdk_Texture_Record;
   begin
      return Gdk.Texture.Gdk_Texture (Get_User_Data (Internal (Get_Object (Self), Destroy, Data), Stub_Gdk_Texture));
   end Build;

   ---------------------
   -- Get_Color_State --
   ---------------------

   function Get_Color_State
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Gdk.Color_State.Gdk_Color_State
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_get_color_state");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Color_State;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   ------------
   -- Get_Fd --
   ------------

   function Get_Fd
      (Self  : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Plane : Guint) return Glib.Gint
   is
      function Internal
         (Self  : System.Address;
          Plane : Guint) return Glib.Gint;
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_get_fd");
   begin
      return Internal (Get_Object (Self), Plane);
   end Get_Fd;

   ----------------
   -- Get_Fourcc --
   ----------------

   function Get_Fourcc
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Guint32
   is
      function Internal (Self : System.Address) return Guint32;
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_get_fourcc");
   begin
      return Internal (Get_Object (Self));
   end Get_Fourcc;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_get_height");
   begin
      return Internal (Get_Object (Self));
   end Get_Height;

   ------------------
   -- Get_Modifier --
   ------------------

   function Get_Modifier
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Guint64
   is
      function Internal (Self : System.Address) return Guint64;
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_get_modifier");
   begin
      return Internal (Get_Object (Self));
   end Get_Modifier;

   ------------------
   -- Get_N_Planes --
   ------------------

   function Get_N_Planes
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_get_n_planes");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Planes;

   ----------------
   -- Get_Offset --
   ----------------

   function Get_Offset
      (Self  : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Plane : Guint) return Guint
   is
      function Internal (Self : System.Address; Plane : Guint) return Guint;
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_get_offset");
   begin
      return Internal (Get_Object (Self), Plane);
   end Get_Offset;

   -----------------------
   -- Get_Premultiplied --
   -----------------------

   function Get_Premultiplied
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_get_premultiplied");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Premultiplied;

   ----------------
   -- Get_Stride --
   ----------------

   function Get_Stride
      (Self  : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Plane : Guint) return Guint
   is
      function Internal (Self : System.Address; Plane : Guint) return Guint;
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_get_stride");
   begin
      return Internal (Get_Object (Self), Plane);
   end Get_Stride;

   -----------------------
   -- Get_Update_Region --
   -----------------------

   function Get_Update_Region
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Cairo.Region.Cairo_Region
   is
      function Internal
         (Self : System.Address) return Cairo.Region.Cairo_Region;
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_get_update_region");
   begin
      return Internal (Get_Object (Self));
   end Get_Update_Region;

   ------------------------
   -- Get_Update_Texture --
   ------------------------

   function Get_Update_Texture
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Gdk.Texture.Gdk_Texture
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_get_update_texture");
      Stub_Gdk_Texture : Gdk.Texture.Gdk_Texture_Record;
   begin
      return Gdk.Texture.Gdk_Texture (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Texture));
   end Get_Update_Texture;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_get_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Width;

   ---------------------
   -- Set_Color_State --
   ---------------------

   procedure Set_Color_State
      (Self        : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Color_State : Gdk.Color_State.Gdk_Color_State)
   is
      procedure Internal
         (Self        : System.Address;
          Color_State : System.Address);
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_set_color_state");
   begin
      Internal (Get_Object (Self), Get_Object (Color_State));
   end Set_Color_State;

   -----------------
   -- Set_Display --
   -----------------

   procedure Set_Display
      (Self    : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class)
   is
      procedure Internal (Self : System.Address; Display : System.Address);
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_set_display");
   begin
      Internal (Get_Object (Self), Get_Object (Display));
   end Set_Display;

   ------------
   -- Set_Fd --
   ------------

   procedure Set_Fd
      (Self  : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Plane : Guint;
       Fd    : Glib.Gint)
   is
      procedure Internal
         (Self  : System.Address;
          Plane : Guint;
          Fd    : Glib.Gint);
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_set_fd");
   begin
      Internal (Get_Object (Self), Plane, Fd);
   end Set_Fd;

   ----------------
   -- Set_Fourcc --
   ----------------

   procedure Set_Fourcc
      (Self   : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Fourcc : Guint32)
   is
      procedure Internal (Self : System.Address; Fourcc : Guint32);
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_set_fourcc");
   begin
      Internal (Get_Object (Self), Fourcc);
   end Set_Fourcc;

   ----------------
   -- Set_Height --
   ----------------

   procedure Set_Height
      (Self   : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Height : Guint)
   is
      procedure Internal (Self : System.Address; Height : Guint);
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_set_height");
   begin
      Internal (Get_Object (Self), Height);
   end Set_Height;

   ------------------
   -- Set_Modifier --
   ------------------

   procedure Set_Modifier
      (Self     : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Modifier : Guint64)
   is
      procedure Internal (Self : System.Address; Modifier : Guint64);
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_set_modifier");
   begin
      Internal (Get_Object (Self), Modifier);
   end Set_Modifier;

   ------------------
   -- Set_N_Planes --
   ------------------

   procedure Set_N_Planes
      (Self     : not null access Gdk_Dmabuf_Texture_Builder_Record;
       N_Planes : Guint)
   is
      procedure Internal (Self : System.Address; N_Planes : Guint);
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_set_n_planes");
   begin
      Internal (Get_Object (Self), N_Planes);
   end Set_N_Planes;

   ----------------
   -- Set_Offset --
   ----------------

   procedure Set_Offset
      (Self   : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Plane  : Guint;
       Offset : Guint)
   is
      procedure Internal
         (Self   : System.Address;
          Plane  : Guint;
          Offset : Guint);
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_set_offset");
   begin
      Internal (Get_Object (Self), Plane, Offset);
   end Set_Offset;

   -----------------------
   -- Set_Premultiplied --
   -----------------------

   procedure Set_Premultiplied
      (Self          : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Premultiplied : Boolean)
   is
      procedure Internal
         (Self          : System.Address;
          Premultiplied : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_set_premultiplied");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Premultiplied));
   end Set_Premultiplied;

   ----------------
   -- Set_Stride --
   ----------------

   procedure Set_Stride
      (Self   : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Plane  : Guint;
       Stride : Guint)
   is
      procedure Internal
         (Self   : System.Address;
          Plane  : Guint;
          Stride : Guint);
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_set_stride");
   begin
      Internal (Get_Object (Self), Plane, Stride);
   end Set_Stride;

   -----------------------
   -- Set_Update_Region --
   -----------------------

   procedure Set_Update_Region
      (Self   : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Region : Cairo.Region.Cairo_Region)
   is
      procedure Internal
         (Self   : System.Address;
          Region : Cairo.Region.Cairo_Region);
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_set_update_region");
   begin
      Internal (Get_Object (Self), Region);
   end Set_Update_Region;

   ------------------------
   -- Set_Update_Texture --
   ------------------------

   procedure Set_Update_Texture
      (Self    : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Texture : access Gdk.Texture.Gdk_Texture_Record'Class)
   is
      procedure Internal (Self : System.Address; Texture : System.Address);
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_set_update_texture");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Texture)));
   end Set_Update_Texture;

   ---------------
   -- Set_Width --
   ---------------

   procedure Set_Width
      (Self  : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Width : Guint)
   is
      procedure Internal (Self : System.Address; Width : Guint);
      pragma Import (C, Internal, "gdk_dmabuf_texture_builder_set_width");
   begin
      Internal (Get_Object (Self), Width);
   end Set_Width;

end Gdk.Dmabuf_Texture_Builder;
