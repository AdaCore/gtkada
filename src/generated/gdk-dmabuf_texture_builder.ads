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

--  Constructs [classGdk.Texture] objects from DMA buffers.
--
--  DMA buffers are commonly called **_dma-bufs_**.
--
--  DMA buffers are a feature of the Linux kernel to enable efficient buffer
--  and memory sharing between hardware such as codecs, GPUs, displays, cameras
--  and the kernel drivers controlling them. For example, a decoder may want
--  its output to be directly shared with the display server for rendering
--  without a copy.
--
--  Any device driver which participates in DMA buffer sharing, can do so as
--  either the exporter or importer of buffers (or both).
--
--  The memory that is shared via DMA buffers is usually stored in non-system
--  memory (maybe in device's local memory or something else not directly
--  accessible by the CPU), and accessing this memory from the CPU may have
--  higher-than-usual overhead.
--
--  In particular for graphics data, it is not uncommon that data consists of
--  multiple separate blocks of memory, for example one block for each of the
--  red, green and blue channels. These blocks are called **_planes_**. DMA
--  buffers can have up to four planes. Even if the memory is a single block,
--  the data can be organized in multiple planes, by specifying offsets from
--  the beginning of the data.
--
--  DMA buffers are exposed to user-space as file descriptors allowing to pass
--  them between processes. If a DMA buffer has multiple planes, more than one
--  file descriptor may be present, up to the number of planes. If the number
--  of file descriptors is less than the number of planes, the remaining ones
--  should be set to -1.
--
--  The format of the data (for graphics data, essentially its colorspace) is
--  described by a 32-bit integer. These format identifiers are defined in the
--  header file `drm_fourcc.h` and commonly referred to as **_fourcc_** values,
--  since they are identified by 4 ASCII characters. Additionally, each DMA
--  buffer has a **_modifier_**, which is a 64-bit integer that describes
--  driver-specific details of the memory layout, such as tiling or
--  compression.
--
--  For historical reasons, some producers of dma-bufs don't provide an
--  explicit modifier, but instead return `DMA_FORMAT_MOD_INVALID` to indicate
--  that their modifier is **_implicit_**. GTK tries to accommodate this
--  situation by accepting `DMA_FORMAT_MOD_INVALID` as modifier.
--
--  The operation of `GdkDmabufTextureBuilder` is quite simple: Create a
--  texture builder, set all the necessary properties, and then call
--  [methodGdk.DmabufTextureBuilder.build] to create the new texture.
--
--  The required properties for a dma-buf texture are
--
--   * The width and height in pixels
--
--   * The `fourcc` code and `modifier` which identify the format and memory
--  layout of the dma-buf
--
--   * The file descriptor, offset and stride for each of the planes
--
--  `GdkDmabufTextureBuilder` can be used for quick one-shot construction of
--  textures as well as kept around and reused to construct multiple textures.
--
--  For further information, see
--
--  * The Linux kernel
--  [documentation](https://docs.kernel.org/driver-api/dma-buf.html)
--
--  * The header file
--  [drm_fourcc.h](https://gitlab.freedesktop.org/mesa/drm/-/blob/main/include/drm/drm_fourcc.h)

pragma Warnings (Off, "*is already use-visible*");
with Cairo.Region;    use Cairo.Region;
with Gdk.Color_State; use Gdk.Color_State;
with Gdk.Display;
with Gdk.Texture;     use Gdk.Texture;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with System;

package Gdk.Dmabuf_Texture_Builder is

   type Gdk_Dmabuf_Texture_Builder_Record is new GObject_Record with null record;
   type Gdk_Dmabuf_Texture_Builder is access all Gdk_Dmabuf_Texture_Builder_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New (Self : out Gdk_Dmabuf_Texture_Builder);
   --  Creates a new texture builder.
   --  Since: gtk+ 4.14

   procedure Initialize
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record'Class);
   --  Creates a new texture builder.
   --  Since: gtk+ 4.14
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gdk_Dmabuf_Texture_Builder_New return Gdk_Dmabuf_Texture_Builder;
   --  Creates a new texture builder.
   --  Since: gtk+ 4.14

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_dmabuf_texture_builder_get_type");

   -------------
   -- Methods --
   -------------

   function Build
      (Self    : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Destroy : Glib.G_Destroy_Notify_Address;
       Data    : System.Address) return Gdk.Texture.Gdk_Texture;
   --  Builds a new `GdkTexture` with the values set up in the builder.
   --  It is a programming error to call this function if any mandatory
   --  property has not been set.
   --  Not all formats defined in the `drm_fourcc.h` header are supported. You
   --  can use [methodGdk.Display.get_dmabuf_formats] to get a list of
   --  supported formats. If the format is not supported by GTK, null will be
   --  returned and Error will be set.
   --  The `destroy` function gets called when the returned texture gets
   --  released.
   --  It is the responsibility of the caller to keep the file descriptors for
   --  the planes open until the created texture is no longer used, and close
   --  them afterwards (possibly using the Destroy notify).
   --  It is possible to call this function multiple times to create multiple
   --  textures, possibly with changing properties in between.
   --  Since: gtk+ 4.14
   --  @param Destroy destroy function to be called when the texture is
   --  released
   --  @param Data user data to pass to the destroy function
   --  @return a newly built `GdkTexture` or `NULL` if the format is not
   --  supported

   function Get_Color_State
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Gdk.Color_State.Gdk_Color_State;
   --  Gets the color state previously set via
   --  Gdk.Dmabuf_Texture_Builder.Set_Color_State.
   --  Since: gtk+ 4.16
   --  @return the color state
   --  Return has transfer-ownership='none'

   procedure Set_Color_State
      (Self        : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Color_State : Gdk.Color_State.Gdk_Color_State);
   --  Sets the color state for the texture.
   --  By default, the colorstate is `NULL`. In that case, GTK will choose the
   --  correct colorstate based on the format. If you don't know what
   --  colorstates are, this is probably the right thing.
   --  Since: gtk+ 4.16
   --  @param Color_State a `GdkColorState` or `NULL` to unset the colorstate.

   function Get_Display
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Gdk.Gdk_Display;
   --  Returns the display that this texture builder is associated with.
   --  Since: gtk+ 4.14
   --  @return the display
   --  Return has transfer-ownership='none'

   procedure Set_Display
      (Self    : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class);
   --  Sets the display that this texture builder is associated with.
   --  The display is used to determine the supported dma-buf formats.
   --  Since: gtk+ 4.14
   --  @param Display the display

   function Get_Fd
      (Self  : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Plane : Guint) return Glib.Gint;
   --  Gets the file descriptor for a plane or -1 if none.
   --  Since: gtk+ 4.14
   --  @param Plane the plane to get the fd for
   --  @return the file descriptor

   procedure Set_Fd
      (Self  : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Plane : Guint;
       Fd    : Glib.Gint);
   --  Sets the file descriptor for a plane or to -1 to unset it.
   --  Since: gtk+ 4.14
   --  @param Plane the plane to set the fd for
   --  @param Fd the file descriptor

   function Get_Fourcc
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Guint32;
   --  Gets the format previously set via
   --  Gdk.Dmabuf_Texture_Builder.Set_Fourcc or 0 if the format wasn't set.
   --  The format is specified as a fourcc code.
   --  Since: gtk+ 4.14
   --  @return The format

   procedure Set_Fourcc
      (Self   : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Fourcc : Guint32);
   --  Sets the format of the texture.
   --  The format is specified as a fourcc code.
   --  The format must be set before calling
   --  [methodGdk.DmabufTextureBuilder.build].
   --  Since: gtk+ 4.14
   --  @param Fourcc the texture's format or 0 to unset

   function Get_Height
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Guint;
   --  Gets the height previously set via
   --  Gdk.Dmabuf_Texture_Builder.Set_Height or 0 if the height wasn't set.
   --  Since: gtk+ 4.14
   --  @return The height

   procedure Set_Height
      (Self   : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Height : Guint);
   --  Sets the height of the texture.
   --  The height must be set before calling
   --  [methodGdk.DmabufTextureBuilder.build].
   --  Since: gtk+ 4.14
   --  @param Height the texture's height or 0 to unset

   function Get_Modifier
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Guint64;
   --  Gets the modifier value.
   --  Since: gtk+ 4.14
   --  @return the modifier

   procedure Set_Modifier
      (Self     : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Modifier : Guint64);
   --  Sets the modifier.
   --  Since: gtk+ 4.14
   --  @param Modifier the modifier value

   function Get_N_Planes
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Guint;
   --  Gets the number of planes.
   --  Since: gtk+ 4.14
   --  @return The number of planes

   procedure Set_N_Planes
      (Self     : not null access Gdk_Dmabuf_Texture_Builder_Record;
       N_Planes : Guint);
   --  Sets the number of planes of the texture.
   --  Since: gtk+ 4.14
   --  @param N_Planes the number of planes

   function Get_Offset
      (Self  : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Plane : Guint) return Guint;
   --  Gets the offset value for a plane.
   --  Since: gtk+ 4.14
   --  @param Plane the plane to get the offset for
   --  @return the offset

   procedure Set_Offset
      (Self   : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Plane  : Guint;
       Offset : Guint);
   --  Sets the offset for a plane.
   --  Since: gtk+ 4.14
   --  @param Plane the plane to set the offset for
   --  @param Offset the offset value

   function Get_Premultiplied
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Boolean;
   --  Whether the data is premultiplied.
   --  Since: gtk+ 4.14
   --  @return whether the data is premultiplied

   procedure Set_Premultiplied
      (Self          : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Premultiplied : Boolean);
   --  Sets whether the data is premultiplied.
   --  Unless otherwise specified, all formats including alpha channels are
   --  assumed to be premultiplied.
   --  Since: gtk+ 4.14
   --  @param Premultiplied whether the data is premultiplied

   function Get_Stride
      (Self  : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Plane : Guint) return Guint;
   --  Gets the stride value for a plane.
   --  Since: gtk+ 4.14
   --  @param Plane the plane to get the stride for
   --  @return the stride

   procedure Set_Stride
      (Self   : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Plane  : Guint;
       Stride : Guint);
   --  Sets the stride for a plane.
   --  The stride must be set for all planes before calling
   --  [methodGdk.DmabufTextureBuilder.build].
   --  Since: gtk+ 4.14
   --  @param Plane the plane to set the stride for
   --  @param Stride the stride value

   function Get_Update_Region
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Cairo.Region.Cairo_Region;
   --  Gets the region previously set via
   --  Gdk.Dmabuf_Texture_Builder.Set_Update_Region or null if none was set.
   --  Since: gtk+ 4.14
   --  @return The region

   procedure Set_Update_Region
      (Self   : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Region : Cairo.Region.Cairo_Region);
   --  Sets the region to be updated by this texture. Together with
   --  [propertyGdk.DmabufTextureBuilder:update-texture] this describes an
   --  update of a previous texture.
   --  When rendering animations of large textures, it is possible that
   --  consecutive textures are only updating contents in parts of the texture.
   --  It is then possible to describe this update via these two properties, so
   --  that GTK can avoid rerendering parts that did not change.
   --  An example would be a screen recording where only the mouse pointer
   --  moves.
   --  Since: gtk+ 4.14
   --  @param Region the region to update

   function Get_Update_Texture
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Gdk.Texture.Gdk_Texture;
   --  Gets the texture previously set via
   --  Gdk.Dmabuf_Texture_Builder.Set_Update_Texture or null if none was set.
   --  Since: gtk+ 4.14
   --  @return The texture
   --  Return has transfer-ownership='none'

   procedure Set_Update_Texture
      (Self    : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Texture : access Gdk.Texture.Gdk_Texture_Record'Class);
   --  Sets the texture to be updated by this texture. See
   --  [methodGdk.DmabufTextureBuilder.set_update_region] for an explanation.
   --  Since: gtk+ 4.14
   --  @param Texture the texture to update

   function Get_Width
      (Self : not null access Gdk_Dmabuf_Texture_Builder_Record)
       return Guint;
   --  Gets the width previously set via Gdk.Dmabuf_Texture_Builder.Set_Width
   --  or 0 if the width wasn't set.
   --  Since: gtk+ 4.14
   --  @return The width

   procedure Set_Width
      (Self  : not null access Gdk_Dmabuf_Texture_Builder_Record;
       Width : Guint);
   --  Sets the width of the texture.
   --  The width must be set before calling
   --  [methodGdk.DmabufTextureBuilder.build].
   --  Since: gtk+ 4.14
   --  @param Width The texture's width or 0 to unset

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Color_State_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Color_State
   --  The color state of the texture.

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display
   --  The display that this texture will be used on.

   Fourcc_Property : constant Glib.Properties.Property_Uint;
   --  The format of the texture, as a fourcc value.

   Height_Property : constant Glib.Properties.Property_Uint;
   --  The height of the texture.

   Modifier_Property : constant Glib.Properties.Property_Uint;
   --  Type: Guint64
   --  The modifier.

   N_Planes_Property : constant Glib.Properties.Property_Uint;
   --  The number of planes of the texture.
   --
   --  Note that you can set properties for other planes, but they will be
   --  ignored when constructing the texture.

   Premultiplied_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the alpha channel is premultiplied into the others.
   --
   --  Only relevant if the format has alpha.

   Update_Region_Property : constant Glib.Properties.Property_Boxed;
   --  Type: cairo.Region
   --  The update region for
   --  [propertyGdk.DmabufTextureBuilder:update-texture].

   Update_Texture_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Texture
   --  The texture [propertyGdk.DmabufTextureBuilder:update-region] is an
   --  update for.

   Width_Property : constant Glib.Properties.Property_Uint;
   --  The width of the texture.

private
   Width_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("width");
   Update_Texture_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("update-texture");
   Update_Region_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("update-region");
   Premultiplied_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("premultiplied");
   N_Planes_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-planes");
   Modifier_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("modifier");
   Height_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("height");
   Fourcc_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("fourcc");
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
   Color_State_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("color-state");
end Gdk.Dmabuf_Texture_Builder;
