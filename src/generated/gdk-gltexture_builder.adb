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

package body Gdk.GLTexture_Builder is

   package Type_Conversion_Gdk_Gltexture_Builder is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Gltexture_Builder_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Gltexture_Builder);

   -------------------------------
   -- Gdk_Gltexture_Builder_New --
   -------------------------------

   function Gdk_Gltexture_Builder_New return Gdk_Gltexture_Builder is
      Self : constant Gdk_Gltexture_Builder := new Gdk_Gltexture_Builder_Record;
   begin
      Gdk.GLTexture_Builder.Initialize (Self);
      return Self;
   end Gdk_Gltexture_Builder_New;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Self : out Gdk_Gltexture_Builder) is
   begin
      Self := new Gdk_Gltexture_Builder_Record;
      Gdk.GLTexture_Builder.Initialize (Self);
   end Gdk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gdk_Gltexture_Builder_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_gl_texture_builder_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------
   -- Build --
   -----------

   function Build
      (Self    : not null access Gdk_Gltexture_Builder_Record;
       Destroy : Glib.G_Destroy_Notify_Address;
       Data    : System.Address) return Gdk.Texture.Gdk_Texture
   is
      function Internal
         (Self    : System.Address;
          Destroy : Glib.G_Destroy_Notify_Address;
          Data    : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_gl_texture_builder_build");
      Stub_Gdk_Texture : Gdk.Texture.Gdk_Texture_Record;
   begin
      return Gdk.Texture.Gdk_Texture (Get_User_Data (Internal (Get_Object (Self), Destroy, Data), Stub_Gdk_Texture));
   end Build;

   ---------------------
   -- Get_Color_State --
   ---------------------

   function Get_Color_State
      (Self : not null access Gdk_Gltexture_Builder_Record)
       return Gdk.Color_State.Gdk_Color_State
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_gl_texture_builder_get_color_state");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Color_State;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context
      (Self : not null access Gdk_Gltexture_Builder_Record)
       return Gdk.GLContext.Gdk_GLContext
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_gl_texture_builder_get_context");
      Stub_Gdk_GLContext : Gdk.GLContext.Gdk_GLContext_Record;
   begin
      return Gdk.GLContext.Gdk_GLContext (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_GLContext));
   end Get_Context;

   ----------------
   -- Get_Format --
   ----------------

   function Get_Format
      (Self : not null access Gdk_Gltexture_Builder_Record)
       return Gdk.Texture.Gdk_Memory_Format
   is
      function Internal
         (Self : System.Address) return Gdk.Texture.Gdk_Memory_Format;
      pragma Import (C, Internal, "gdk_gl_texture_builder_get_format");
   begin
      return Internal (Get_Object (Self));
   end Get_Format;

   --------------------
   -- Get_Has_Mipmap --
   --------------------

   function Get_Has_Mipmap
      (Self : not null access Gdk_Gltexture_Builder_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_gl_texture_builder_get_has_mipmap");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Has_Mipmap;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
      (Self : not null access Gdk_Gltexture_Builder_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_gl_texture_builder_get_height");
   begin
      return Internal (Get_Object (Self));
   end Get_Height;

   ------------
   -- Get_Id --
   ------------

   function Get_Id
      (Self : not null access Gdk_Gltexture_Builder_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_gl_texture_builder_get_id");
   begin
      return Internal (Get_Object (Self));
   end Get_Id;

   --------------
   -- Get_Sync --
   --------------

   function Get_Sync
      (Self : not null access Gdk_Gltexture_Builder_Record)
       return System.Address
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_gl_texture_builder_get_sync");
   begin
      return Internal (Get_Object (Self));
   end Get_Sync;

   -----------------------
   -- Get_Update_Region --
   -----------------------

   function Get_Update_Region
      (Self : not null access Gdk_Gltexture_Builder_Record)
       return Cairo.Region.Cairo_Region
   is
      function Internal
         (Self : System.Address) return Cairo.Region.Cairo_Region;
      pragma Import (C, Internal, "gdk_gl_texture_builder_get_update_region");
   begin
      return Internal (Get_Object (Self));
   end Get_Update_Region;

   ------------------------
   -- Get_Update_Texture --
   ------------------------

   function Get_Update_Texture
      (Self : not null access Gdk_Gltexture_Builder_Record)
       return Gdk.Texture.Gdk_Texture
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_gl_texture_builder_get_update_texture");
      Stub_Gdk_Texture : Gdk.Texture.Gdk_Texture_Record;
   begin
      return Gdk.Texture.Gdk_Texture (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Texture));
   end Get_Update_Texture;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
      (Self : not null access Gdk_Gltexture_Builder_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_gl_texture_builder_get_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Width;

   ---------------------
   -- Set_Color_State --
   ---------------------

   procedure Set_Color_State
      (Self        : not null access Gdk_Gltexture_Builder_Record;
       Color_State : Gdk.Color_State.Gdk_Color_State)
   is
      procedure Internal
         (Self        : System.Address;
          Color_State : System.Address);
      pragma Import (C, Internal, "gdk_gl_texture_builder_set_color_state");
   begin
      Internal (Get_Object (Self), Get_Object (Color_State));
   end Set_Color_State;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
      (Self    : not null access Gdk_Gltexture_Builder_Record;
       Context : access Gdk.GLContext.Gdk_GLContext_Record'Class)
   is
      procedure Internal (Self : System.Address; Context : System.Address);
      pragma Import (C, Internal, "gdk_gl_texture_builder_set_context");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Context)));
   end Set_Context;

   ----------------
   -- Set_Format --
   ----------------

   procedure Set_Format
      (Self   : not null access Gdk_Gltexture_Builder_Record;
       Format : Gdk.Texture.Gdk_Memory_Format)
   is
      procedure Internal
         (Self   : System.Address;
          Format : Gdk.Texture.Gdk_Memory_Format);
      pragma Import (C, Internal, "gdk_gl_texture_builder_set_format");
   begin
      Internal (Get_Object (Self), Format);
   end Set_Format;

   --------------------
   -- Set_Has_Mipmap --
   --------------------

   procedure Set_Has_Mipmap
      (Self       : not null access Gdk_Gltexture_Builder_Record;
       Has_Mipmap : Boolean)
   is
      procedure Internal (Self : System.Address; Has_Mipmap : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_gl_texture_builder_set_has_mipmap");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Has_Mipmap));
   end Set_Has_Mipmap;

   ----------------
   -- Set_Height --
   ----------------

   procedure Set_Height
      (Self   : not null access Gdk_Gltexture_Builder_Record;
       Height : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Height : Glib.Gint);
      pragma Import (C, Internal, "gdk_gl_texture_builder_set_height");
   begin
      Internal (Get_Object (Self), Height);
   end Set_Height;

   ------------
   -- Set_Id --
   ------------

   procedure Set_Id
      (Self : not null access Gdk_Gltexture_Builder_Record;
       Id   : Guint)
   is
      procedure Internal (Self : System.Address; Id : Guint);
      pragma Import (C, Internal, "gdk_gl_texture_builder_set_id");
   begin
      Internal (Get_Object (Self), Id);
   end Set_Id;

   --------------
   -- Set_Sync --
   --------------

   procedure Set_Sync
      (Self : not null access Gdk_Gltexture_Builder_Record;
       Sync : System.Address)
   is
      procedure Internal (Self : System.Address; Sync : System.Address);
      pragma Import (C, Internal, "gdk_gl_texture_builder_set_sync");
   begin
      Internal (Get_Object (Self), Sync);
   end Set_Sync;

   -----------------------
   -- Set_Update_Region --
   -----------------------

   procedure Set_Update_Region
      (Self   : not null access Gdk_Gltexture_Builder_Record;
       Region : Cairo.Region.Cairo_Region)
   is
      procedure Internal
         (Self   : System.Address;
          Region : Cairo.Region.Cairo_Region);
      pragma Import (C, Internal, "gdk_gl_texture_builder_set_update_region");
   begin
      Internal (Get_Object (Self), Region);
   end Set_Update_Region;

   ------------------------
   -- Set_Update_Texture --
   ------------------------

   procedure Set_Update_Texture
      (Self    : not null access Gdk_Gltexture_Builder_Record;
       Texture : access Gdk.Texture.Gdk_Texture_Record'Class)
   is
      procedure Internal (Self : System.Address; Texture : System.Address);
      pragma Import (C, Internal, "gdk_gl_texture_builder_set_update_texture");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Texture)));
   end Set_Update_Texture;

   ---------------
   -- Set_Width --
   ---------------

   procedure Set_Width
      (Self  : not null access Gdk_Gltexture_Builder_Record;
       Width : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Width : Glib.Gint);
      pragma Import (C, Internal, "gdk_gl_texture_builder_set_width");
   begin
      Internal (Get_Object (Self), Width);
   end Set_Width;

end Gdk.GLTexture_Builder;
