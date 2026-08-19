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

--  Constructs [classGdk.Texture] objects from GL textures.
--
--  The operation is quite simple: Create a texture builder, set all the
--  necessary properties - keep in mind that the properties
--  [propertyGdk.GLTextureBuilder:context], [propertyGdk.GLTextureBuilder:id],
--  [propertyGdk.GLTextureBuilder:width], and
--  [propertyGdk.GLTextureBuilder:height] are mandatory - and then call
--  [methodGdk.GLTextureBuilder.build] to create the new texture.
--
--  `GdkGLTextureBuilder` can be used for quick one-shot construction of
--  textures as well as kept around and reused to construct multiple textures.

pragma Warnings (Off, "*is already use-visible*");
with Cairo.Region;    use Cairo.Region;
with Gdk.Color_State; use Gdk.Color_State;
with Gdk.GLContext;   use Gdk.GLContext;
with Gdk.Texture;     use Gdk.Texture;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with System;

package Gdk.GLTexture_Builder is

   type Gdk_Gltexture_Builder_Record is new GObject_Record with null record;
   type Gdk_Gltexture_Builder is access all Gdk_Gltexture_Builder_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New (Self : out Gdk_Gltexture_Builder);
   --  Creates a new texture builder.
   --  Since: gtk+ 4.12

   procedure Initialize
      (Self : not null access Gdk_Gltexture_Builder_Record'Class);
   --  Creates a new texture builder.
   --  Since: gtk+ 4.12
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gdk_Gltexture_Builder_New return Gdk_Gltexture_Builder;
   --  Creates a new texture builder.
   --  Since: gtk+ 4.12

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_gl_texture_builder_get_type");

   -------------
   -- Methods --
   -------------

   function Build
      (Self    : not null access Gdk_Gltexture_Builder_Record;
       Destroy : Glib.G_Destroy_Notify_Address;
       Data    : System.Address) return Gdk.Texture.Gdk_Texture;
   --  Builds a new `GdkTexture` with the values set up in the builder.
   --  The `destroy` function gets called when the returned texture gets
   --  released; either when the texture is finalized or by an explicit call to
   --  [methodGdk.GLTexture.release]. It should release all GL resources
   --  associated with the texture, such as the
   --  [propertyGdk.GLTextureBuilder:id] and the
   --  [propertyGdk.GLTextureBuilder:sync].
   --  Note that it is a programming error to call this function if any
   --  mandatory property has not been set.
   --  It is possible to call this function multiple times to create multiple
   --  textures, possibly with changing properties in between.
   --  Since: gtk+ 4.12
   --  @param Destroy destroy function to be called when the texture is
   --  released
   --  @param Data user data to pass to the destroy function
   --  @return a newly built `GdkTexture`

   function Get_Color_State
      (Self : not null access Gdk_Gltexture_Builder_Record)
       return Gdk.Color_State.Gdk_Color_State;
   --  Gets the color state previously set via
   --  Gdk.GLTexture_Builder.Set_Color_State.
   --  Since: gtk+ 4.16
   --  @return the color state
   --  Return has transfer-ownership='none'

   procedure Set_Color_State
      (Self        : not null access Gdk_Gltexture_Builder_Record;
       Color_State : Gdk.Color_State.Gdk_Color_State);
   --  Sets the color state for the texture.
   --  By default, the sRGB colorstate is used. If you don't know what
   --  colorstates are, this is probably the right thing.
   --  Since: gtk+ 4.16
   --  @param Color_State a `GdkColorState`

   function Get_Context
      (Self : not null access Gdk_Gltexture_Builder_Record)
       return Gdk.GLContext.Gdk_GLContext;
   --  Gets the context previously set via Gdk.GLTexture_Builder.Set_Context
   --  or null if none was set.
   --  Since: gtk+ 4.12
   --  @return The context
   --  Return has transfer-ownership='none'

   procedure Set_Context
      (Self    : not null access Gdk_Gltexture_Builder_Record;
       Context : access Gdk.GLContext.Gdk_GLContext_Record'Class);
   --  Sets the context to be used for the texture. This is the context that
   --  owns the texture.
   --  The context must be set before calling
   --  [methodGdk.GLTextureBuilder.build].
   --  Since: gtk+ 4.12
   --  @param Context The context the texture belongs to or null to unset

   function Get_Format
      (Self : not null access Gdk_Gltexture_Builder_Record)
       return Gdk.Texture.Gdk_Memory_Format;
   --  Gets the format previously set via Gdk.GLTexture_Builder.Set_Format.
   --  Since: gtk+ 4.12
   --  @return The format

   procedure Set_Format
      (Self   : not null access Gdk_Gltexture_Builder_Record;
       Format : Gdk.Texture.Gdk_Memory_Format);
   --  Sets the format of the texture. The default is
   --  `GDK_MEMORY_R8G8B8A8_PREMULTIPLIED`.
   --  The format is the preferred format the texture data should be
   --  downloaded to. The format must be supported by the GL version of
   --  [propertyGdk.GLTextureBuilder:context].
   --  GDK's texture download code assumes that the format corresponds to the
   --  storage parameters of the GL texture in an obvious way. For example, a
   --  format of `GDK_MEMORY_R16G16B16A16_PREMULTIPLIED` is expected to be
   --  stored as `GL_RGBA16` texture, and `GDK_MEMORY_G8A8` is expected to be
   --  stored as `GL_RG8` texture.
   --  Setting the right format is particularly useful when using high bit
   --  depth textures to preserve the bit depth, to set the correct value for
   --  unpremultiplied textures and to make sure opaque textures are treated as
   --  such.
   --  Non-RGBA textures need to have swizzling parameters set up properly to
   --  be usable in GSK's shaders.
   --  Since: gtk+ 4.12
   --  @param Format The texture's format

   function Get_Has_Mipmap
      (Self : not null access Gdk_Gltexture_Builder_Record) return Boolean;
   --  Gets whether the texture has a mipmap.
   --  Since: gtk+ 4.12
   --  @return Whether the texture has a mipmap

   procedure Set_Has_Mipmap
      (Self       : not null access Gdk_Gltexture_Builder_Record;
       Has_Mipmap : Boolean);
   --  Sets whether the texture has a mipmap. This allows the renderer and
   --  other users of the generated texture to use a higher quality
   --  downscaling.
   --  Typically, the `glGenerateMipmap` function is used to generate a mimap.
   --  Since: gtk+ 4.12
   --  @param Has_Mipmap Whether the texture has a mipmap

   function Get_Height
      (Self : not null access Gdk_Gltexture_Builder_Record) return Glib.Gint;
   --  Gets the height previously set via Gdk.GLTexture_Builder.Set_Height or
   --  0 if the height wasn't set.
   --  Since: gtk+ 4.12
   --  @return The height

   procedure Set_Height
      (Self   : not null access Gdk_Gltexture_Builder_Record;
       Height : Glib.Gint);
   --  Sets the height of the texture.
   --  The height must be set before calling
   --  [methodGdk.GLTextureBuilder.build].
   --  Since: gtk+ 4.12
   --  @param Height The texture's height or 0 to unset

   function Get_Id
      (Self : not null access Gdk_Gltexture_Builder_Record) return Guint;
   --  Gets the texture id previously set via Gdk.GLTexture_Builder.Set_Id or
   --  0 if the id wasn't set.
   --  Since: gtk+ 4.12
   --  @return The id

   procedure Set_Id
      (Self : not null access Gdk_Gltexture_Builder_Record;
       Id   : Guint);
   --  Sets the texture id of the texture. The texture id must remain
   --  unmodified until the texture was finalized. See
   --  [methodGdk.GLTextureBuilder.build] for a longer discussion.
   --  The id must be set before calling [methodGdk.GLTextureBuilder.build].
   --  Since: gtk+ 4.12
   --  @param Id The texture id to be used for creating the texture

   function Get_Sync
      (Self : not null access Gdk_Gltexture_Builder_Record)
       return System.Address;
   --  Gets the `GLsync` previously set via Gdk.GLTexture_Builder.Set_Sync.
   --  Since: gtk+ 4.12
   --  @return the `GLSync`

   procedure Set_Sync
      (Self : not null access Gdk_Gltexture_Builder_Record;
       Sync : System.Address);
   --  Sets the GLSync object to use for the texture.
   --  GTK will wait on this object before using the created `GdkTexture`.
   --  The `destroy` function that is passed to
   --  [methodGdk.GLTextureBuilder.build] is responsible for freeing the sync
   --  object when it is no longer needed. The texture builder does not destroy
   --  it and it is the callers responsibility to make sure it doesn't leak.
   --  Since: gtk+ 4.12
   --  @param Sync the GLSync object

   function Get_Update_Region
      (Self : not null access Gdk_Gltexture_Builder_Record)
       return Cairo.Region.Cairo_Region;
   --  Gets the region previously set via
   --  Gdk.GLTexture_Builder.Set_Update_Region or null if none was set.
   --  Since: gtk+ 4.12
   --  @return The region

   procedure Set_Update_Region
      (Self   : not null access Gdk_Gltexture_Builder_Record;
       Region : Cairo.Region.Cairo_Region);
   --  Sets the region to be updated by this texture. Together with
   --  [propertyGdk.GLTextureBuilder:update-texture] this describes an update
   --  of a previous texture.
   --  When rendering animations of large textures, it is possible that
   --  consecutive textures are only updating contents in parts of the texture.
   --  It is then possible to describe this update via these two properties, so
   --  that GTK can avoid rerendering parts that did not change.
   --  An example would be a screen recording where only the mouse pointer
   --  moves.
   --  Since: gtk+ 4.12
   --  @param Region the region to update

   function Get_Update_Texture
      (Self : not null access Gdk_Gltexture_Builder_Record)
       return Gdk.Texture.Gdk_Texture;
   --  Gets the texture previously set via
   --  Gdk.GLTexture_Builder.Set_Update_Texture or null if none was set.
   --  Since: gtk+ 4.12
   --  @return The texture
   --  Return has transfer-ownership='none'

   procedure Set_Update_Texture
      (Self    : not null access Gdk_Gltexture_Builder_Record;
       Texture : access Gdk.Texture.Gdk_Texture_Record'Class);
   --  Sets the texture to be updated by this texture. See
   --  [methodGdk.GLTextureBuilder.set_update_region] for an explanation.
   --  Since: gtk+ 4.12
   --  @param Texture the texture to update

   function Get_Width
      (Self : not null access Gdk_Gltexture_Builder_Record) return Glib.Gint;
   --  Gets the width previously set via Gdk.GLTexture_Builder.Set_Width or 0
   --  if the width wasn't set.
   --  Since: gtk+ 4.12
   --  @return The width

   procedure Set_Width
      (Self  : not null access Gdk_Gltexture_Builder_Record;
       Width : Glib.Gint);
   --  Sets the width of the texture.
   --  The width must be set before calling
   --  [methodGdk.GLTextureBuilder.build].
   --  Since: gtk+ 4.12
   --  @param Width The texture's width or 0 to unset

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Sync_Property : constant Glib.Properties.Property_String :=
   Glib.Properties.Build ("sync");--  Unknown type: gpointer

   Color_State_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Color_State
   --  The color state of the texture.

   Context_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GLContext
   --  The context owning the texture.

   Format_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Memory_Format
   --  The format when downloading the texture.

   Has_Mipmap_Property : constant Glib.Properties.Property_Boolean;
   --  If the texture has a mipmap.

   Height_Property : constant Glib.Properties.Property_Int;
   --  The height of the texture.

   Id_Property : constant Glib.Properties.Property_Uint;
   --  The texture ID to use.

   Update_Region_Property : constant Glib.Properties.Property_Boxed;
   --  Type: cairo.Region
   --  The update region for [propertyGdk.GLTextureBuilder:update-texture].

   Update_Texture_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Texture
   --  The texture [propertyGdk.GLTextureBuilder:update-region] is an update
   --  for.

   Width_Property : constant Glib.Properties.Property_Int;
   --  The width of the texture.

private
   Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width");
   Update_Texture_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("update-texture");
   Update_Region_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("update-region");
   Id_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("id");
   Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("height");
   Has_Mipmap_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-mipmap");
   Format_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("format");
   Context_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("context");
   Color_State_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("color-state");
end Gdk.GLTexture_Builder;
