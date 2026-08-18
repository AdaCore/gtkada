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

--  Constructs [classGdk.Texture] objects from system memory provided via
--  [structGlib.Bytes].
--
--  The operation is quite simple: Create a texture builder, set all the
--  necessary properties - keep in mind that the properties
--  [propertyGdk.MemoryTextureBuilder:bytes],
--  [propertyGdk.MemoryTextureBuilder:stride],
--  [propertyGdk.MemoryTextureBuilder:width], and
--  [propertyGdk.MemoryTextureBuilder:height] are mandatory - and then call
--  [methodGdk.MemoryTextureBuilder.build] to create the new texture.
--
--  `GdkMemoryTextureBuilder` can be used for quick one-shot construction of
--  textures as well as kept around and reused to construct multiple textures.

pragma Warnings (Off, "*is already use-visible*");
with Cairo.Region;    use Cairo.Region;
with Gdk.Color_State; use Gdk.Color_State;
with Gdk.Texture;     use Gdk.Texture;
with Glib;            use Glib;
with Glib.Bytes;      use Glib.Bytes;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gdk.Memory_Texture_Builder is

   type Gdk_Memory_Texture_Builder_Record is new GObject_Record with null record;
   type Gdk_Memory_Texture_Builder is access all Gdk_Memory_Texture_Builder_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New (Self : out Gdk_Memory_Texture_Builder);
   --  Creates a new texture builder.
   --  Since: gtk+ 4.16

   procedure Initialize
      (Self : not null access Gdk_Memory_Texture_Builder_Record'Class);
   --  Creates a new texture builder.
   --  Since: gtk+ 4.16
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gdk_Memory_Texture_Builder_New return Gdk_Memory_Texture_Builder;
   --  Creates a new texture builder.
   --  Since: gtk+ 4.16

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_memory_texture_builder_get_type");

   -------------
   -- Methods --
   -------------

   function Build
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Gdk.Texture.Gdk_Texture;
   --  Builds a new `GdkTexture` with the values set up in the builder.
   --  Note that it is a programming error to call this function if any
   --  mandatory property has not been set.
   --  It is possible to call this function multiple times to create multiple
   --  textures, possibly with changing properties in between.
   --  Since: gtk+ 4.16
   --  @return a newly built `GdkTexture`

   function Get_Bytes
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Glib.Bytes.Gbytes;
   --  Gets the bytes previously set via Gdk.Memory_Texture_Builder.Set_Bytes
   --  or null if none was set.
   --  Since: gtk+ 4.16
   --  @return The bytes
   --  Return has transfer-ownership='none'

   procedure Set_Bytes
      (Self  : not null access Gdk_Memory_Texture_Builder_Record;
       Bytes : Glib.Bytes.Gbytes);
   --  Sets the data to be shown but the texture.
   --  The bytes must be set before calling
   --  [methodGdk.MemoryTextureBuilder.build].
   --  Since: gtk+ 4.16
   --  @param Bytes The bytes the texture shows or null to unset

   function Get_Color_State
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Gdk.Color_State.Gdk_Color_State;
   --  Gets the colorstate previously set via
   --  Gdk.Memory_Texture_Builder.Set_Color_State.
   --  Since: gtk+ 4.16
   --  @return The colorstate
   --  Return has transfer-ownership='none'

   procedure Set_Color_State
      (Self        : not null access Gdk_Memory_Texture_Builder_Record;
       Color_State : Gdk.Color_State.Gdk_Color_State);
   --  Sets the colorstate describing the data.
   --  By default, the sRGB colorstate is used. If you don't know what
   --  colorstates are, this is probably the right thing.
   --  Since: gtk+ 4.16
   --  @param Color_State The colorstate describing the data

   function Get_Format
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Gdk.Texture.Gdk_Memory_Format;
   --  Gets the format previously set via
   --  Gdk.Memory_Texture_Builder.Set_Format.
   --  Since: gtk+ 4.16
   --  @return The format

   procedure Set_Format
      (Self   : not null access Gdk_Memory_Texture_Builder_Record;
       Format : Gdk.Texture.Gdk_Memory_Format);
   --  Sets the format of the bytes.
   --  The default is `GDK_MEMORY_R8G8B8A8_PREMULTIPLIED`.
   --  Since: gtk+ 4.16
   --  @param Format The texture's format

   function Get_Height
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Glib.Gint;
   --  Gets the height previously set via
   --  Gdk.Memory_Texture_Builder.Set_Height or 0 if the height wasn't set.
   --  Since: gtk+ 4.16
   --  @return The height

   procedure Set_Height
      (Self   : not null access Gdk_Memory_Texture_Builder_Record;
       Height : Glib.Gint);
   --  Sets the height of the texture.
   --  The height must be set before calling
   --  [methodGdk.MemoryTextureBuilder.build] and conform to size requirements
   --  of the provided format.
   --  Since: gtk+ 4.16
   --  @param Height The texture's height or 0 to unset

   function Get_Offset
      (Self  : not null access Gdk_Memory_Texture_Builder_Record;
       Plane : Guint) return Gsize;
   --  Gets the offset previously set via
   --  Gdk.Memory_Texture_Builder.Set_Offset.
   --  Since: gtk+ 4.20
   --  @param Plane a plane
   --  @return The offset associated to a Plane

   procedure Set_Offset
      (Self   : not null access Gdk_Memory_Texture_Builder_Record;
       Plane  : Guint;
       Offset : Gsize);
   --  Sets the offset of the texture for Plane.
   --  Since: gtk+ 4.20
   --  @param Plane a plane
   --  @param Offset the texture's offset for Plane

   function Get_Stride
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Gsize;
   --  Gets the stride previously set via
   --  Gdk.Memory_Texture_Builder.Set_Stride.
   --  Since: gtk+ 4.16
   --  @return the stride

   procedure Set_Stride
      (Self   : not null access Gdk_Memory_Texture_Builder_Record;
       Stride : Gsize);
   --  Sets the rowstride of the bytes used.
   --  The rowstride must be set before calling
   --  [methodGdk.MemoryTextureBuilder.build].
   --  Since: gtk+ 4.16
   --  @param Stride the stride or 0 to unset

   function Get_Stride_For_Plane
      (Self  : not null access Gdk_Memory_Texture_Builder_Record;
       Plane : Guint) return Gsize;
   --  Gets the stride previously set via
   --  Gdk.Memory_Texture_Builder.Set_Stride_For_Plane.
   --  Since: gtk+ 4.20
   --  @param Plane a plane
   --  @return The stride associated to a Plane

   procedure Set_Stride_For_Plane
      (Self   : not null access Gdk_Memory_Texture_Builder_Record;
       Plane  : Guint;
       Stride : Gsize);
   --  Sets the stride of the texture for Plane.
   --  Since: gtk+ 4.20
   --  @param Plane a plane
   --  @param Stride the texture's stride for Plane

   function Get_Update_Region
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Cairo.Region.Cairo_Region;
   --  Gets the region previously set via
   --  Gdk.Memory_Texture_Builder.Set_Update_Region or null if none was set.
   --  Since: gtk+ 4.16
   --  @return The update region

   procedure Set_Update_Region
      (Self   : not null access Gdk_Memory_Texture_Builder_Record;
       Region : Cairo.Region.Cairo_Region);
   --  Sets the region to be updated by this texture.
   --  Together with [propertyGdk.MemoryTextureBuilder:update-texture], this
   --  describes an update of a previous texture.
   --  When rendering animations of large textures, it is possible that
   --  consecutive textures are only updating contents in parts of the texture.
   --  It is then possible to describe this update via these two properties, so
   --  that GTK can avoid rerendering parts that did not change.
   --  An example would be a screen recording where only the mouse pointer
   --  moves.
   --  Since: gtk+ 4.16
   --  @param Region the region to update

   function Get_Update_Texture
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Gdk.Texture.Gdk_Texture;
   --  Gets the texture previously set via
   --  Gdk.Memory_Texture_Builder.Set_Update_Texture or null if none was set.
   --  Since: gtk+ 4.16
   --  @return The update texture
   --  Return has transfer-ownership='none'

   procedure Set_Update_Texture
      (Self    : not null access Gdk_Memory_Texture_Builder_Record;
       Texture : access Gdk.Texture.Gdk_Texture_Record'Class);
   --  Sets the texture to be updated by this texture.
   --  See [methodGdk.MemoryTextureBuilder.set_update_region] for an
   --  explanation.
   --  Since: gtk+ 4.16
   --  @param Texture the texture to update

   function Get_Width
      (Self : not null access Gdk_Memory_Texture_Builder_Record)
       return Glib.Gint;
   --  Gets the width previously set via Gdk.Memory_Texture_Builder.Set_Width
   --  or 0 if the width wasn't set.
   --  Since: gtk+ 4.16
   --  @return The width

   procedure Set_Width
      (Self  : not null access Gdk_Memory_Texture_Builder_Record;
       Width : Glib.Gint);
   --  Sets the width of the texture.
   --  The width must be set before calling
   --  [methodGdk.MemoryTextureBuilder.build] and conform to size requirements
   --  of the provided format.
   --  Since: gtk+ 4.16
   --  @param Width The texture's width or 0 to unset

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Bytes_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GLib.Bytes
   --  The bytes holding the data.

   Color_State_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Color_State
   --  The colorstate describing the data.

   Format_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Memory_Format
   --  The format of the data.

   Height_Property : constant Glib.Properties.Property_Int;
   --  The height of the texture.

   Stride_Property : constant Glib.Properties.Property_Uint;
   --  Type: Guint64
   --  The rowstride of the texture.
   --
   --  The rowstride is the number of bytes between the first pixel in a row
   --  of image data, and the first pixel in the next row.

   Update_Region_Property : constant Glib.Properties.Property_Boxed;
   --  Type: cairo.Region
   --  The update region for
   --  [propertyGdk.MemoryTextureBuilder:update-texture].

   Update_Texture_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Texture
   --  The texture [propertyGdk.MemoryTextureBuilder:update-region] is an
   --  update for.

   Width_Property : constant Glib.Properties.Property_Int;
   --  The width of the texture.

private
   Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width");
   Update_Texture_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("update-texture");
   Update_Region_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("update-region");
   Stride_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("stride");
   Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("height");
   Format_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("format");
   Color_State_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("color-state");
   Bytes_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("bytes");
end Gdk.Memory_Texture_Builder;
