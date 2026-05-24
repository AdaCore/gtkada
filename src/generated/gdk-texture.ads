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

--  Refers to pixel data in various forms.
--
--  It is primarily meant for pixel data that will not change over multiple
--  frames, and will be used for a long time.
--
--  There are various ways to create `GdkTexture` objects from a
--  [classGdkpixbuf.Pixbuf], or from bytes stored in memory, a file, or a
--  [structGio.Resource].
--
--  The ownership of the pixel data is transferred to the `GdkTexture`
--  instance; you can only make a copy of it, via [methodGdk.Texture.download].
--
--  `GdkTexture` is an immutable object: That means you cannot change anything
--  about it other than increasing the reference count via
--  [methodGobject.Object.ref], and consequently, it is a threadsafe object.
--
--  GDK provides a number of threadsafe texture loading functions:
--  [ctorGdk.Texture.new_from_resource], [ctorGdk.Texture.new_from_bytes],
--  [ctorGdk.Texture.new_from_file], [ctorGdk.Texture.new_from_filename],
--  [ctorGdk.Texture.new_for_pixbuf]. Note that these are meant for loading
--  icons and resources that are shipped with the toolkit or application. It is
--  recommended that you use a dedicated image loading framework such as
--  [glycin](https://lib.rs/crates/glycin), if you need to load untrusted image
--  data.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Color_State;         use Gdk.Color_State;
with Gdk.Paintable;           use Gdk.Paintable;
with Gdk.Snapshot;            use Gdk.Snapshot;
with Glib;                    use Glib;
with Glib.Bytes;              use Glib.Bytes;
with Glib.Cancellable;        use Glib.Cancellable;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Loadable_Icon;      use Glib.Loadable_Icon;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;

package Gdk.Texture is

   type Gdk_Texture_Record is new GObject_Record with null record;
   type Gdk_Texture is access all Gdk_Texture_Record'Class;

   type Gdk_Memory_Format is (
      Gdk_Memory_B8G8R8A8_Premultiplied,
      Gdk_Memory_A8R8G8B8_Premultiplied,
      Gdk_Memory_R8G8B8A8_Premultiplied,
      Gdk_Memory_B8G8R8A8,
      Gdk_Memory_A8R8G8B8,
      Gdk_Memory_R8G8B8A8,
      Gdk_Memory_A8B8G8R8,
      Gdk_Memory_R8G8B8,
      Gdk_Memory_B8G8R8,
      Gdk_Memory_R16G16B16,
      Gdk_Memory_R16G16B16A16_Premultiplied,
      Gdk_Memory_R16G16B16A16,
      Gdk_Memory_R16G16B16_Float,
      Gdk_Memory_R16G16B16A16_Float_Premultiplied,
      Gdk_Memory_R16G16B16A16_Float,
      Gdk_Memory_R32G32B32_Float,
      Gdk_Memory_R32G32B32A32_Float_Premultiplied,
      Gdk_Memory_R32G32B32A32_Float,
      Gdk_Memory_G8A8_Premultiplied,
      Gdk_Memory_G8A8,
      Gdk_Memory_G8,
      Gdk_Memory_G16A16_Premultiplied,
      Gdk_Memory_G16A16,
      Gdk_Memory_G16,
      Gdk_Memory_A8,
      Gdk_Memory_A16,
      Gdk_Memory_A16_Float,
      Gdk_Memory_A32_Float,
      Gdk_Memory_A8B8G8R8_Premultiplied,
      Gdk_Memory_B8G8R8X8,
      Gdk_Memory_X8R8G8B8,
      Gdk_Memory_R8G8B8X8,
      Gdk_Memory_X8B8G8R8,
      Gdk_Memory_G8_B8R8_420,
      Gdk_Memory_G8_R8B8_420,
      Gdk_Memory_G8_B8R8_422,
      Gdk_Memory_G8_R8B8_422,
      Gdk_Memory_G8_B8R8_444,
      Gdk_Memory_G8_R8B8_444,
      Gdk_Memory_G10X6_B10X6R10X6_420,
      Gdk_Memory_G12X4_B12X4R12X4_420,
      Gdk_Memory_G16_B16R16_420,
      Gdk_Memory_G8_B8_R8_410,
      Gdk_Memory_G8_R8_B8_410,
      Gdk_Memory_G8_B8_R8_411,
      Gdk_Memory_G8_R8_B8_411,
      Gdk_Memory_G8_B8_R8_420,
      Gdk_Memory_G8_R8_B8_420,
      Gdk_Memory_G8_B8_R8_422,
      Gdk_Memory_G8_R8_B8_422,
      Gdk_Memory_G8_B8_R8_444,
      Gdk_Memory_G8_R8_B8_444,
      Gdk_Memory_G8B8G8R8_422,
      Gdk_Memory_G8R8G8B8_422,
      Gdk_Memory_R8G8B8G8_422,
      Gdk_Memory_B8G8R8G8_422,
      Gdk_Memory_X6G10_X6B10_X6R10_420,
      Gdk_Memory_X6G10_X6B10_X6R10_422,
      Gdk_Memory_X6G10_X6B10_X6R10_444,
      Gdk_Memory_X4G12_X4B12_X4R12_420,
      Gdk_Memory_X4G12_X4B12_X4R12_422,
      Gdk_Memory_X4G12_X4B12_X4R12_444,
      Gdk_Memory_G16_B16_R16_420,
      Gdk_Memory_G16_B16_R16_422,
      Gdk_Memory_G16_B16_R16_444,
      Gdk_Memory_N_Formats);
   pragma Convention (C, Gdk_Memory_Format);
   --  Describes formats that image data can have in memory.
   --
   --  It describes formats by listing the contents of the memory passed to
   --  it. So `GDK_MEMORY_A8R8G8B8` will be 1 byte (8 bits) of alpha, followed
   --  by a byte each of red, green and blue. It is not endian-dependent, so
   --  `CAIRO_FORMAT_ARGB32` is represented by different `GdkMemoryFormats` on
   --  architectures with different endiannesses.
   --
   --  Its naming is modelled after
   --  [VkFormat](https://www.khronos.org/registry/vulkan/specs/1.0/html/vkspec.htmlVk_Format)
   --  for details).

   ---------------
   -- Callbacks --
   ---------------

   type Gasync_Ready_Callback is access procedure
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);
   --  Type definition for a function that will be called back when an
   --  asynchronous operation within GIO has been completed.
   --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
   --  invoked in a later iteration of the [thread-default main
   --  context][g-main-context-push-thread-default] where the Gtask.Gtask was
   --  created. All other users of Gasync_Ready_Callback must likewise call it
   --  asynchronously in a later iteration of the main context.
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Memory_Format_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Memory_Format);
   type Property_Gdk_Memory_Format is new Gdk_Memory_Format_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New_From_Bytes
      (Self  : out Gdk_Texture;
       Bytes : Glib.Bytes.Gbytes);
   --  Creates a new texture by loading an image from memory,
   --  The file format is detected automatically. The supported formats are
   --  PNG, JPEG and TIFF, though more formats might be available.
   --  If `NULL` is returned, then Error will be set.
   --  This function is threadsafe, so that you can e.g. use GTask and
   --  [methodGio.Task.run_in_thread] to avoid blocking the main thread while
   --  loading a big image.
   --  ::: warning Note that this function should not be used with untrusted
   --  data. Use a proper image loading framework such as libglycin, which can
   --  load many image formats into a `GdkTexture`.
   --  Since: gtk+ 4.6
   --  @param Bytes a `GBytes` containing the data to load

   procedure Initialize_From_Bytes
      (Self  : not null access Gdk_Texture_Record'Class;
       Bytes : Glib.Bytes.Gbytes);
   --  Creates a new texture by loading an image from memory,
   --  The file format is detected automatically. The supported formats are
   --  PNG, JPEG and TIFF, though more formats might be available.
   --  If `NULL` is returned, then Error will be set.
   --  This function is threadsafe, so that you can e.g. use GTask and
   --  [methodGio.Task.run_in_thread] to avoid blocking the main thread while
   --  loading a big image.
   --  ::: warning Note that this function should not be used with untrusted
   --  data. Use a proper image loading framework such as libglycin, which can
   --  load many image formats into a `GdkTexture`.
   --  Since: gtk+ 4.6
   --  Initialize_From_Bytes does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Bytes a `GBytes` containing the data to load

   function Gdk_Texture_New_From_Bytes
      (Bytes : Glib.Bytes.Gbytes) return Gdk_Texture;
   --  Creates a new texture by loading an image from memory,
   --  The file format is detected automatically. The supported formats are
   --  PNG, JPEG and TIFF, though more formats might be available.
   --  If `NULL` is returned, then Error will be set.
   --  This function is threadsafe, so that you can e.g. use GTask and
   --  [methodGio.Task.run_in_thread] to avoid blocking the main thread while
   --  loading a big image.
   --  ::: warning Note that this function should not be used with untrusted
   --  data. Use a proper image loading framework such as libglycin, which can
   --  load many image formats into a `GdkTexture`.
   --  Since: gtk+ 4.6
   --  @param Bytes a `GBytes` containing the data to load

   procedure Gdk_New_From_Filename
      (Self : out Gdk_Texture;
       Path : UTF8_String);
   --  Creates a new texture by loading an image from a file.
   --  The file format is detected automatically. The supported formats are
   --  PNG, JPEG and TIFF, though more formats might be available.
   --  If `NULL` is returned, then Error will be set.
   --  This function is threadsafe, so that you can e.g. use GTask and
   --  [methodGio.Task.run_in_thread] to avoid blocking the main thread while
   --  loading a big image.
   --  ::: warning Note that this function should not be used with untrusted
   --  data. Use a proper image loading framework such as libglycin, which can
   --  load many image formats into a `GdkTexture`.
   --  Since: gtk+ 4.6
   --  @param Path the filename to load

   procedure Initialize_From_Filename
      (Self : not null access Gdk_Texture_Record'Class;
       Path : UTF8_String);
   --  Creates a new texture by loading an image from a file.
   --  The file format is detected automatically. The supported formats are
   --  PNG, JPEG and TIFF, though more formats might be available.
   --  If `NULL` is returned, then Error will be set.
   --  This function is threadsafe, so that you can e.g. use GTask and
   --  [methodGio.Task.run_in_thread] to avoid blocking the main thread while
   --  loading a big image.
   --  ::: warning Note that this function should not be used with untrusted
   --  data. Use a proper image loading framework such as libglycin, which can
   --  load many image formats into a `GdkTexture`.
   --  Since: gtk+ 4.6
   --  Initialize_From_Filename does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Path the filename to load

   function Gdk_Texture_New_From_Filename
      (Path : UTF8_String) return Gdk_Texture;
   --  Creates a new texture by loading an image from a file.
   --  The file format is detected automatically. The supported formats are
   --  PNG, JPEG and TIFF, though more formats might be available.
   --  If `NULL` is returned, then Error will be set.
   --  This function is threadsafe, so that you can e.g. use GTask and
   --  [methodGio.Task.run_in_thread] to avoid blocking the main thread while
   --  loading a big image.
   --  ::: warning Note that this function should not be used with untrusted
   --  data. Use a proper image loading framework such as libglycin, which can
   --  load many image formats into a `GdkTexture`.
   --  Since: gtk+ 4.6
   --  @param Path the filename to load

   procedure Gdk_New_From_Resource
      (Self          : out Gdk_Texture;
       Resource_Path : UTF8_String);
   --  Creates a new texture by loading an image from a resource.
   --  The file format is detected automatically. The supported formats are
   --  PNG, JPEG and TIFF, though more formats might be available.
   --  It is a fatal error if Resource_Path does not specify a valid image
   --  resource and the program will abort if that happens. If you are unsure
   --  about the validity of a resource, use [ctorGdk.Texture.new_from_file] to
   --  load it.
   --  This function is threadsafe, so that you can e.g. use GTask and
   --  [methodGio.Task.run_in_thread] to avoid blocking the main thread while
   --  loading a big image.
   --  @param Resource_Path the path of the resource file

   procedure Initialize_From_Resource
      (Self          : not null access Gdk_Texture_Record'Class;
       Resource_Path : UTF8_String);
   --  Creates a new texture by loading an image from a resource.
   --  The file format is detected automatically. The supported formats are
   --  PNG, JPEG and TIFF, though more formats might be available.
   --  It is a fatal error if Resource_Path does not specify a valid image
   --  resource and the program will abort if that happens. If you are unsure
   --  about the validity of a resource, use [ctorGdk.Texture.new_from_file] to
   --  load it.
   --  This function is threadsafe, so that you can e.g. use GTask and
   --  [methodGio.Task.run_in_thread] to avoid blocking the main thread while
   --  loading a big image.
   --  Initialize_From_Resource does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Resource_Path the path of the resource file

   function Gdk_Texture_New_From_Resource
      (Resource_Path : UTF8_String) return Gdk_Texture;
   --  Creates a new texture by loading an image from a resource.
   --  The file format is detected automatically. The supported formats are
   --  PNG, JPEG and TIFF, though more formats might be available.
   --  It is a fatal error if Resource_Path does not specify a valid image
   --  resource and the program will abort if that happens. If you are unsure
   --  about the validity of a resource, use [ctorGdk.Texture.new_from_file] to
   --  load it.
   --  This function is threadsafe, so that you can e.g. use GTask and
   --  [methodGio.Task.run_in_thread] to avoid blocking the main thread while
   --  loading a big image.
   --  @param Resource_Path the path of the resource file

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_texture_get_type");

   -------------
   -- Methods --
   -------------

   procedure Download
      (Self   : not null access Gdk_Texture_Record;
       Data   : Gint_Array;
       Stride : Gsize);
   --  Downloads the Texture into local memory.
   --  This may be an expensive operation, as the actual texture data may
   --  reside on a GPU or on a remote display server.
   --  The data format of the downloaded data is equivalent to
   --  CAIRO_FORMAT_ARGB32, so every downloaded pixel requires 4 bytes of
   --  memory.
   --  Downloading a texture into a Cairo image surface: ```c surface =
   --  cairo_image_surface_create (CAIRO_FORMAT_ARGB32, gdk_texture_get_width
   --  (texture), gdk_texture_get_height (texture)); gdk_texture_download
   --  (texture, cairo_image_surface_get_data (surface),
   --  cairo_image_surface_get_stride (surface)); cairo_surface_mark_dirty
   --  (surface); ```
   --  For more flexible download capabilities, see
   --  [structGdk.TextureDownloader].
   --  @param Data pointer to enough memory to be filled with the downloaded
   --  data of Texture
   --  @param Stride rowstride in bytes

   function Get_Color_State
      (Self : not null access Gdk_Texture_Record)
       return Gdk.Color_State.Gdk_Color_State;
   --  Returns the color state associated with the texture.
   --  Since: gtk+ 4.16
   --  @return the color state of the `GdkTexture`

   function Get_Format
      (Self : not null access Gdk_Texture_Record) return Gdk_Memory_Format;
   --  Gets the memory format most closely associated with the data of the
   --  texture.
   --  Note that it may not be an exact match for texture data stored on the
   --  GPU or with compression.
   --  The format can give an indication about the bit depth and opacity of
   --  the texture and is useful to determine the best format for downloading
   --  the texture.
   --  Since: gtk+ 4.10
   --  @return the preferred format for the texture's data

   function Get_Height
      (Self : not null access Gdk_Texture_Record) return Glib.Gint;
   --  Returns the height of the Texture, in pixels.
   --  @return the height of the `GdkTexture`

   function Get_Width
      (Self : not null access Gdk_Texture_Record) return Glib.Gint;
   --  Returns the width of Texture, in pixels.
   --  @return the width of the `GdkTexture`

   function Save_To_Png
      (Self     : not null access Gdk_Texture_Record;
       Filename : UTF8_String) return Boolean;
   --  Store the given Texture to the Filename as a PNG file.
   --  This is a utility function intended for debugging and testing. If you
   --  want more control over formats, proper error handling or want to store
   --  to a [ifaceGio.File] or other location, you might want to use
   --  [methodGdk.Texture.save_to_png_bytes] or look into the libglycin
   --  library.
   --  @param Filename the filename to store to
   --  @return True if saving succeeded, False on failure.

   function Save_To_Png_Bytes
      (Self : not null access Gdk_Texture_Record) return Glib.Bytes.Gbytes;
   --  Store the given Texture in memory as a PNG file.
   --  Use [ctorGdk.Texture.new_from_bytes] to read it back.
   --  If you want to serialize a texture, this is a convenient and portable
   --  way to do that.
   --  If you need more control over the generated image, such as attaching
   --  metadata, you should look into an image handling library such as the
   --  libglycin library.
   --  If you are dealing with high dynamic range float data, you might also
   --  want to consider [methodGdk.Texture.save_to_tiff_bytes] instead.
   --  Since: gtk+ 4.6
   --  @return a newly allocated `GBytes` containing PNG data

   function Save_To_Tiff
      (Self     : not null access Gdk_Texture_Record;
       Filename : UTF8_String) return Boolean;
   --  Store the given Texture to the Filename as a TIFF file.
   --  GTK will attempt to store data without loss.
   --  Since: gtk+ 4.6
   --  @param Filename the filename to store to
   --  @return True if saving succeeded, False on failure.

   function Save_To_Tiff_Bytes
      (Self : not null access Gdk_Texture_Record) return Glib.Bytes.Gbytes;
   --  Store the given Texture in memory as a TIFF file.
   --  Use [ctorGdk.Texture.new_from_bytes] to read it back.
   --  This function is intended to store a representation of the texture's
   --  data that is as accurate as possible. This is particularly relevant when
   --  working with high dynamic range images and floating-point texture data.
   --  If that is not your concern and you are interested in a smaller size
   --  and a more portable format, you might want to use
   --  [methodGdk.Texture.save_to_png_bytes].
   --  Since: gtk+ 4.6
   --  @return a newly allocated `GBytes` containing TIFF data

   procedure Load_Async
      (Self        : not null access Gdk_Texture_Record;
       Size        : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Loads an icon asynchronously. To finish this function, see
   --  g_loadable_icon_load_finish. For the synchronous, blocking version of
   --  this function, see g_loadable_icon_load.
   --  @param Size an integer.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Compute_Concrete_Size
      (Self             : not null access Gdk_Texture_Record;
       Specified_Width  : Gdouble;
       Specified_Height : Gdouble;
       Default_Width    : Gdouble;
       Default_Height   : Gdouble;
       Concrete_Width   : out Gdouble;
       Concrete_Height  : out Gdouble);

   function Get_Current_Image
      (Self : not null access Gdk_Texture_Record)
       return Gdk.Paintable.Gdk_Paintable;

   function Get_Flags
      (Self : not null access Gdk_Texture_Record)
       return Gdk.Paintable.Gdk_Paintable_Flags;

   function Get_Intrinsic_Aspect_Ratio
      (Self : not null access Gdk_Texture_Record) return Gdouble;

   function Get_Intrinsic_Height
      (Self : not null access Gdk_Texture_Record) return Glib.Gint;

   function Get_Intrinsic_Width
      (Self : not null access Gdk_Texture_Record) return Glib.Gint;

   procedure Invalidate_Contents (Self : not null access Gdk_Texture_Record);

   procedure Invalidate_Size (Self : not null access Gdk_Texture_Record);

   procedure Snapshot
      (Self     : not null access Gdk_Texture_Record;
       Snapshot : not null access Gdk.Snapshot.Gdk_Snapshot_Record'Class;
       Width    : Gdouble;
       Height   : Gdouble);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Color_State_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Color_State
   --  The color state of the texture.

   Height_Property : constant Glib.Properties.Property_Int;
   --  The height of the texture, in pixels.

   Width_Property : constant Glib.Properties.Property_Int;
   --  The width of the texture, in pixels.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gdk.Paintable"
   --
   --  - "Gio.LoadableIcon"

   package Implements_Gdk_Paintable is new Glib.Types.Implements
     (Gdk.Paintable.Gdk_Paintable, Gdk_Texture_Record, Gdk_Texture);
   function "+"
     (Widget : access Gdk_Texture_Record'Class)
   return Gdk.Paintable.Gdk_Paintable
   renames Implements_Gdk_Paintable.To_Interface;
   function "-"
     (Interf : Gdk.Paintable.Gdk_Paintable)
   return Gdk_Texture
   renames Implements_Gdk_Paintable.To_Object;

   package Implements_Gloadable_Icon is new Glib.Types.Implements
     (Glib.Loadable_Icon.Gloadable_Icon, Gdk_Texture_Record, Gdk_Texture);
   function "+"
     (Widget : access Gdk_Texture_Record'Class)
   return Glib.Loadable_Icon.Gloadable_Icon
   renames Implements_Gloadable_Icon.To_Interface;
   function "-"
     (Interf : Glib.Loadable_Icon.Gloadable_Icon)
   return Gdk_Texture
   renames Implements_Gloadable_Icon.To_Object;

private
   Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width");
   Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("height");
   Color_State_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("color-state");
end Gdk.Texture;
