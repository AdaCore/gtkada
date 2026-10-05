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

--  A pixel buffer.
--
--  `GdkPixbuf` contains information about an image's pixel data, its color
--  space, bits per sample, width and height, and the rowstride (the number of
--  bytes between the start of one row and the start of the next).
--
--  ## Creating new `GdkPixbuf`
--
--  The most basic way to create a pixbuf is to wrap an existing pixel buffer
--  with a [classGdkpixbuf.Pixbuf] instance. You can use the
--  [`ctorGdkpixbuf.Pixbuf.new_from_data`] function to do this.
--
--  Every time you create a new `GdkPixbuf` instance for some data, you will
--  need to specify the destroy notification function that will be called when
--  the data buffer needs to be freed; this will happen when a `GdkPixbuf` is
--  finalized by the reference counting functions. If you have a chunk of
--  static data compiled into your application, you can pass in `NULL` as the
--  destroy notification function so that the data will not be freed.
--
--  The [`ctorGdkpixbuf.Pixbuf.new`] constructor function can be used as a
--  convenience to create a pixbuf with an empty buffer; this is equivalent to
--  allocating a data buffer using `malloc` and then wrapping it with
--  `gdk_pixbuf_new_from_data`. The `gdk_pixbuf_new` function will compute an
--  optimal rowstride so that rendering can be performed with an efficient
--  algorithm.
--
--  You can also copy an existing pixbuf with the [methodPixbuf.copy]
--  function. This is not the same as just acquiring a reference to the old
--  pixbuf instance: the copy function will actually duplicate the pixel data
--  in memory and create a new [classPixbuf] instance for it.
--
--  ## Reference counting
--
--  `GdkPixbuf` structures are reference counted. This means that an
--  application can share a single pixbuf among many parts of the code. When a
--  piece of the program needs to use a pixbuf, it should acquire a reference
--  to it by calling `g_object_ref`; when it no longer needs the pixbuf, it
--  should release the reference it acquired by calling `g_object_unref`. The
--  resources associated with a `GdkPixbuf` will be freed when its reference
--  count drops to zero. Newly-created `GdkPixbuf` instances start with a
--  reference count of one.
--
--  ## Image Data
--
--  Image data in a pixbuf is stored in memory in an uncompressed, packed
--  format. Rows in the image are stored top to bottom, and in each row pixels
--  are stored from left to right.
--
--  There may be padding at the end of a row.
--
--  The "rowstride" value of a pixbuf, as returned by
--  [`methodGdkpixbuf.Pixbuf.get_rowstride`], indicates the number of bytes
--  between rows.
--
--  **NOTE**: If you are copying raw pixbuf data with `memcpy` note that the
--  last row in the pixbuf may not be as wide as the full rowstride, but rather
--  just as wide as the pixel data needs to be; that is: it is unsafe to do
--  `memcpy (dest, pixels, rowstride * height)` to copy a whole pixbuf. Use
--  [methodGdkpixbuf.Pixbuf.copy] instead, or compute the width in bytes of the
--  last row as:
--
--  ```c last_row = width * ((n_channels * bits_per_sample + 7) / 8); ```
--
--  The same rule applies when iterating over each row of a `GdkPixbuf` pixels
--  array.
--
--  The following code illustrates a simple `put_pixel` function for RGB
--  pixbufs with 8 bits per channel with an alpha channel.
--
--  ```c static void put_pixel (GdkPixbuf *pixbuf, int x, int y, guchar red,
--  guchar green, guchar blue, guchar alpha) { int n_channels =
--  gdk_pixbuf_get_n_channels (pixbuf);
--
--  // Ensure that the pixbuf is valid g_assert (gdk_pixbuf_get_colorspace
--  (pixbuf) == GDK_COLORSPACE_RGB); g_assert (gdk_pixbuf_get_bits_per_sample
--  (pixbuf) == 8); g_assert (gdk_pixbuf_get_has_alpha (pixbuf)); g_assert
--  (n_channels == 4);
--
--  int width = gdk_pixbuf_get_width (pixbuf); int height =
--  gdk_pixbuf_get_height (pixbuf);
--
--  // Ensure that the coordinates are in a valid range g_assert (x >= 0 && x
--  < width); g_assert (y >= 0 && y < height);
--
--  int rowstride = gdk_pixbuf_get_rowstride (pixbuf);
--
--  // The pixel buffer in the GdkPixbuf instance guchar *pixels =
--  gdk_pixbuf_get_pixels (pixbuf);
--
--  // The pixel we wish to modify guchar *p = pixels + y * rowstride + x *
--  n_channels; p[0] = red; p[1] = green; p[2] = blue; p[3] = alpha; } ```
--
--  ## Loading images
--
--  The `GdkPixBuf` class provides a simple mechanism for loading an image
--  from a file in synchronous and asynchronous fashion.
--
--  For GUI applications, it is recommended to use the asynchronous stream API
--  to avoid blocking the control flow of the application.
--
--  Additionally, `GdkPixbuf` provides the [classGdkpixbuf.PixbufLoader`] API
--  for progressive image loading.
--
--  ## Saving images
--
--  The `GdkPixbuf` class provides methods for saving image data in a number
--  of file formats. The formatted data can be written to a file or to a memory
--  buffer. `GdkPixbuf` can also call a user-defined callback on the data,
--  which allows to e.g. write the image to a socket or store it in a database.
--
--  <group>GDK</group>
--  <gtkada_demo>create_pixbuf.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Gdk.Pixbuf_Format;       use Gdk.Pixbuf_Format;
with Glib;                    use Glib;
with Glib.Bytes;              use Glib.Bytes;
with Glib.Cancellable;        use Glib.Cancellable;
with Glib.Error;              use Glib.Error;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Input_Stream;       use Glib.Input_Stream;
with Glib.Loadable_Icon;      use Glib.Loadable_Icon;
with Glib.Object;             use Glib.Object;
with Glib.Output_Stream;      use Glib.Output_Stream;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with System;

package Gdk.Pixbuf is

   type Gdk_Pixbuf_Record is new GObject_Record with null record;
   type Gdk_Pixbuf is access all Gdk_Pixbuf_Record'Class;

   type Gdk_Colorspace is (
      Gdk_Colorspace_Rgb);
   pragma Convention (C, Gdk_Colorspace);
   --  This enumeration defines the color spaces that are supported by the
   --  gdk-pixbuf library.
   --
   --  Currently only RGB is supported.

   type Gdk_Interp_Type is (
      Gdk_Interp_Nearest,
      Gdk_Interp_Tiles,
      Gdk_Interp_Bilinear,
      Gdk_Interp_Hyper);
   pragma Convention (C, Gdk_Interp_Type);
   --  Interpolation modes for scaling functions.
   --
   --  The `GDK_INTERP_NEAREST` mode is the fastest scaling method, but has
   --  horrible quality when scaling down; `GDK_INTERP_BILINEAR` is the best
   --  choice if you aren't sure what to choose, it has a good speed/quality
   --  balance.
   --
   --  **Note**: Cubic filtering is missing from the list; hyperbolic
   --  interpolation is just as fast and results in higher quality.

   type Gdk_Pixbuf_Rotation is (
      Gdk_Pixbuf_Rotate_None,
      Gdk_Pixbuf_Rotate_Counterclockwise,
      Gdk_Pixbuf_Rotate_Upsidedown,
      Gdk_Pixbuf_Rotate_Clockwise);
   pragma Convention (C, Gdk_Pixbuf_Rotation);
   --  The possible rotations which can be passed to Gdk.Pixbuf.Rotate_Simple.
   --
   --  To make them easier to use, their numerical values are the actual
   --  degrees.

   for Gdk_Pixbuf_Rotation use (
      Gdk_Pixbuf_Rotate_None => 0,
      Gdk_Pixbuf_Rotate_Counterclockwise => 90,
      Gdk_Pixbuf_Rotate_Upsidedown => 180,
      Gdk_Pixbuf_Rotate_Clockwise => 270);

   type Gdk_Pixbuf_Error is (
      Gdk_Pixbuf_Error_Corrupt_Image,
      Gdk_Pixbuf_Error_Insufficient_Memory,
      Gdk_Pixbuf_Error_Bad_Option,
      Gdk_Pixbuf_Error_Unknown_Type,
      Gdk_Pixbuf_Error_Unsupported_Operation,
      Gdk_Pixbuf_Error_Failed,
      Gdk_Pixbuf_Error_Incomplete_Animation);
   pragma Convention (C, Gdk_Pixbuf_Error);
   --  An error code in the `GDK_PIXBUF_ERROR` domain.
   --
   --  Many gdk-pixbuf operations can cause errors in this domain, or in the
   --  `G_FILE_ERROR` domain.

   Gdk_Pixbuf_Error_Name   : constant UTF8_String := "gdk-pixbuf-error-quark";
   Gdk_Pixbuf_Error_Domain : constant GQuark := Quark_From_String (Gdk_Pixbuf_Error_Name);
   --  Used to identify error domain in a GError

   function Gdk_Pixbuf_Error_Matches
     (Error : GError; Code : Gdk_Pixbuf_Error) return Boolean
   is (Error_Matches
        (Error,
         Gdk_Pixbuf_Error_Domain,
         Glib.Gint (Gdk_Pixbuf_Error'Pos (Code))));
   --  Convenience helper to match error codes

   type Gdk_Pixbuf_Destroy_Notify is access procedure
     (Pixels : System.Address; Data : System.Address);
   pragma Convention (C, Gdk_Pixbuf_Destroy_Notify);
   --  Data must remain valid until the callback runs at the last unref.
   --  Without a callback, the caller must keep the pixel buffer alive until
   --  the pixbuf releases it, then free it.

   ---------------
   -- Callbacks --
   ---------------

   type Gasync_Ready_Callback is access procedure
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);
   --  Type definition for a function that will be called back when an
   --  asynchronous operation within GIO has been completed.
   --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
   --  invoked in a later iteration of the thread-default main context (see
   --  [methodGlib.MainContext.push_thread_default]) where the Gtask.Gtask was
   --  created. All other users of Gasync_Ready_Callback must likewise call it
   --  asynchronously in a later iteration of the main context.
   --  The asynchronous operation is guaranteed to have held a reference to
   --  Source_Object from the time when the `*_async` function was called,
   --  until after this callback returns.
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Colorspace_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Colorspace);
   type Property_Gdk_Colorspace is new Gdk_Colorspace_Properties.Property;

   package Gdk_Interp_Type_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Interp_Type);
   type Property_Gdk_Interp_Type is new Gdk_Interp_Type_Properties.Property;

   package Gdk_Pixbuf_Rotation_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Pixbuf_Rotation);
   type Property_Gdk_Pixbuf_Rotation is new Gdk_Pixbuf_Rotation_Properties.Property;

   package Gdk_Pixbuf_Error_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Pixbuf_Error);
   type Property_Gdk_Pixbuf_Error is new Gdk_Pixbuf_Error_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New
      (Self            : out Gdk_Pixbuf;
       Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint);
   --  Creates a new `GdkPixbuf` structure and allocates a buffer for it.
   --  If the allocation of the buffer failed, this function will return
   --  `NULL`.
   --  The buffer has an optimal rowstride. Note that the buffer is not
   --  cleared; you will have to fill it completely yourself.
   --  @param Colorspace Color space for image
   --  @param Has_Alpha Whether the image should have transparency information
   --  @param Bits_Per_Sample Number of bits per color sample
   --  @param Width Width of image in pixels, must be > 0
   --  @param Height Height of image in pixels, must be > 0

   procedure Initialize
      (Self            : not null access Gdk_Pixbuf_Record'Class;
       Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint);
   --  Creates a new `GdkPixbuf` structure and allocates a buffer for it.
   --  If the allocation of the buffer failed, this function will return
   --  `NULL`.
   --  The buffer has an optimal rowstride. Note that the buffer is not
   --  cleared; you will have to fill it completely yourself.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Colorspace Color space for image
   --  @param Has_Alpha Whether the image should have transparency information
   --  @param Bits_Per_Sample Number of bits per color sample
   --  @param Width Width of image in pixels, must be > 0
   --  @param Height Height of image in pixels, must be > 0

   function Gdk_Pixbuf_New
      (Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint) return Gdk_Pixbuf;
   --  Creates a new `GdkPixbuf` structure and allocates a buffer for it.
   --  If the allocation of the buffer failed, this function will return
   --  `NULL`.
   --  The buffer has an optimal rowstride. Note that the buffer is not
   --  cleared; you will have to fill it completely yourself.
   --  @param Colorspace Color space for image
   --  @param Has_Alpha Whether the image should have transparency information
   --  @param Bits_Per_Sample Number of bits per color sample
   --  @param Width Width of image in pixels, must be > 0
   --  @param Height Height of image in pixels, must be > 0

   procedure Gdk_New_From_Bytes
      (Self            : out Gdk_Pixbuf;
       Data            : Glib.Bytes.Gbytes;
       Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint;
       Rowstride       : Glib.Gint);
   --  Creates a new Gdk.Pixbuf.Gdk_Pixbuf out of in-memory readonly image
   --  data.
   --  Currently only RGB images with 8 bits per sample are supported.
   --  This is the `GBytes` variant of Gdk.Pixbuf.Gdk_New_From_Data, useful
   --  for language bindings.
   --  Since: gtk+ 2.32
   --  @param Data Image data in 8-bit/sample packed format inside a
   --  Glib.Bytes.Gbytes
   --  @param Colorspace Colorspace for the image data
   --  @param Has_Alpha Whether the data has an opacity channel
   --  @param Bits_Per_Sample Number of bits per sample
   --  @param Width Width of the image in pixels, must be > 0
   --  @param Height Height of the image in pixels, must be > 0
   --  @param Rowstride Distance in bytes between row starts

   procedure Initialize_From_Bytes
      (Self            : not null access Gdk_Pixbuf_Record'Class;
       Data            : Glib.Bytes.Gbytes;
       Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint;
       Rowstride       : Glib.Gint);
   --  Creates a new Gdk.Pixbuf.Gdk_Pixbuf out of in-memory readonly image
   --  data.
   --  Currently only RGB images with 8 bits per sample are supported.
   --  This is the `GBytes` variant of Gdk.Pixbuf.Gdk_New_From_Data, useful
   --  for language bindings.
   --  Since: gtk+ 2.32
   --  Initialize_From_Bytes does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Data Image data in 8-bit/sample packed format inside a
   --  Glib.Bytes.Gbytes
   --  @param Colorspace Colorspace for the image data
   --  @param Has_Alpha Whether the data has an opacity channel
   --  @param Bits_Per_Sample Number of bits per sample
   --  @param Width Width of the image in pixels, must be > 0
   --  @param Height Height of the image in pixels, must be > 0
   --  @param Rowstride Distance in bytes between row starts

   function Gdk_Pixbuf_New_From_Bytes
      (Data            : Glib.Bytes.Gbytes;
       Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint;
       Rowstride       : Glib.Gint) return Gdk_Pixbuf;
   --  Creates a new Gdk.Pixbuf.Gdk_Pixbuf out of in-memory readonly image
   --  data.
   --  Currently only RGB images with 8 bits per sample are supported.
   --  This is the `GBytes` variant of Gdk.Pixbuf.Gdk_New_From_Data, useful
   --  for language bindings.
   --  Since: gtk+ 2.32
   --  @param Data Image data in 8-bit/sample packed format inside a
   --  Glib.Bytes.Gbytes
   --  @param Colorspace Colorspace for the image data
   --  @param Has_Alpha Whether the data has an opacity channel
   --  @param Bits_Per_Sample Number of bits per sample
   --  @param Width Width of the image in pixels, must be > 0
   --  @param Height Height of the image in pixels, must be > 0
   --  @param Rowstride Distance in bytes between row starts

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
       Destroy_Fn_Data : System.Address := System.Null_Address);
   --  Creates a new Gdk.Pixbuf.Gdk_Pixbuf out of in-memory image data.
   --  Currently only RGB images with 8 bits per sample are supported.
   --  Since you are providing a pre-allocated pixel buffer, you must also
   --  specify a way to free that data. This is done with a function of type
   --  `GdkPixbufDestroyNotify`. When a pixbuf created with is finalized, your
   --  destroy notification function will be called, and it is its
   --  responsibility to free the pixel array.
   --  See also: [ctorGdkpixbuf.Pixbuf.new_from_bytes]
   --  Data is retained, not copied. Keep it alive until the last pixbuf
   --  reference is released. Destroy_Fn uses the C calling convention and
   --  receives Data and Destroy_Fn_Data.
   --  @param Data Image data in 8-bit/sample packed format
   --  @param Colorspace Colorspace for the image data
   --  @param Has_Alpha Whether the data has an opacity channel
   --  @param Bits_Per_Sample Number of bits per sample
   --  @param Width Width of the image in pixels, must be > 0
   --  @param Height Height of the image in pixels, must be > 0
   --  @param Rowstride Distance in bytes between row starts
   --  @param Destroy_Fn Function used to free the data when the pixbuf's
   --  reference count drops to zero, or `NULL` if the data should not be freed
   --  @param Destroy_Fn_Data Closure data to pass to the destroy notification
   --  function

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
       Destroy_Fn_Data : System.Address := System.Null_Address);
   --  Creates a new Gdk.Pixbuf.Gdk_Pixbuf out of in-memory image data.
   --  Currently only RGB images with 8 bits per sample are supported.
   --  Since you are providing a pre-allocated pixel buffer, you must also
   --  specify a way to free that data. This is done with a function of type
   --  `GdkPixbufDestroyNotify`. When a pixbuf created with is finalized, your
   --  destroy notification function will be called, and it is its
   --  responsibility to free the pixel array.
   --  See also: [ctorGdkpixbuf.Pixbuf.new_from_bytes]
   --  Data is retained, not copied. Keep it alive until the last pixbuf
   --  reference is released. Destroy_Fn uses the C calling convention and
   --  receives Data and Destroy_Fn_Data.
   --  Initialize_From_Data does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Data Image data in 8-bit/sample packed format
   --  @param Colorspace Colorspace for the image data
   --  @param Has_Alpha Whether the data has an opacity channel
   --  @param Bits_Per_Sample Number of bits per sample
   --  @param Width Width of the image in pixels, must be > 0
   --  @param Height Height of the image in pixels, must be > 0
   --  @param Rowstride Distance in bytes between row starts
   --  @param Destroy_Fn Function used to free the data when the pixbuf's
   --  reference count drops to zero, or `NULL` if the data should not be freed
   --  @param Destroy_Fn_Data Closure data to pass to the destroy notification
   --  function

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
       return Gdk_Pixbuf;
   --  Creates a new Gdk.Pixbuf.Gdk_Pixbuf out of in-memory image data.
   --  Currently only RGB images with 8 bits per sample are supported.
   --  Since you are providing a pre-allocated pixel buffer, you must also
   --  specify a way to free that data. This is done with a function of type
   --  `GdkPixbufDestroyNotify`. When a pixbuf created with is finalized, your
   --  destroy notification function will be called, and it is its
   --  responsibility to free the pixel array.
   --  See also: [ctorGdkpixbuf.Pixbuf.new_from_bytes]
   --  Data is retained, not copied. Keep it alive until the last pixbuf
   --  reference is released. Destroy_Fn uses the C calling convention and
   --  receives Data and Destroy_Fn_Data.
   --  @param Data Image data in 8-bit/sample packed format
   --  @param Colorspace Colorspace for the image data
   --  @param Has_Alpha Whether the data has an opacity channel
   --  @param Bits_Per_Sample Number of bits per sample
   --  @param Width Width of the image in pixels, must be > 0
   --  @param Height Height of the image in pixels, must be > 0
   --  @param Rowstride Distance in bytes between row starts
   --  @param Destroy_Fn Function used to free the data when the pixbuf's
   --  reference count drops to zero, or `NULL` if the data should not be freed
   --  @param Destroy_Fn_Data Closure data to pass to the destroy notification
   --  function

   procedure Gdk_New_From_File
      (Self     : out Gdk_Pixbuf;
       Filename : UTF8_String;
       Error    : out Glib.Error.GError);
   --  Creates a new pixbuf by loading an image from a file.
   --  The file format is detected automatically.
   --  If `NULL` is returned, then Error will be set. Possible errors are:
   --  - the file could not be opened - there is no loader for the file's
   --  format - there is not enough memory to allocate the image buffer - the
   --  image buffer contains invalid data
   --  The error domains are `GDK_PIXBUF_ERROR` and `G_FILE_ERROR`.
   --  @param Filename Name of file to load, in the GLib file name encoding
   --  @param Error the return location for a recoverable error

   procedure Initialize_From_File
      (Self     : not null access Gdk_Pixbuf_Record'Class;
       Filename : UTF8_String;
       Error    : out Glib.Error.GError);
   --  Creates a new pixbuf by loading an image from a file.
   --  The file format is detected automatically.
   --  If `NULL` is returned, then Error will be set. Possible errors are:
   --  - the file could not be opened - there is no loader for the file's
   --  format - there is not enough memory to allocate the image buffer - the
   --  image buffer contains invalid data
   --  The error domains are `GDK_PIXBUF_ERROR` and `G_FILE_ERROR`.
   --  Initialize_From_File does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Filename Name of file to load, in the GLib file name encoding
   --  @param Error the return location for a recoverable error

   function Gdk_Pixbuf_New_From_File
      (Filename : UTF8_String;
       Error    : out Glib.Error.GError) return Gdk_Pixbuf;
   --  Creates a new pixbuf by loading an image from a file.
   --  The file format is detected automatically.
   --  If `NULL` is returned, then Error will be set. Possible errors are:
   --  - the file could not be opened - there is no loader for the file's
   --  format - there is not enough memory to allocate the image buffer - the
   --  image buffer contains invalid data
   --  The error domains are `GDK_PIXBUF_ERROR` and `G_FILE_ERROR`.
   --  @param Filename Name of file to load, in the GLib file name encoding
   --  @param Error the return location for a recoverable error

   procedure Gdk_New_From_File_At_Scale
      (Self                  : out Gdk_Pixbuf;
       Filename              : UTF8_String;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Error                 : out Glib.Error.GError);
   --  Creates a new pixbuf by loading an image from a file.
   --  The file format is detected automatically.
   --  If `NULL` is returned, then Error will be set. Possible errors are:
   --  - the file could not be opened - there is no loader for the file's
   --  format - there is not enough memory to allocate the image buffer - the
   --  image buffer contains invalid data
   --  The error domains are `GDK_PIXBUF_ERROR` and `G_FILE_ERROR`.
   --  The image will be scaled to fit in the requested size, optionally
   --  preserving the image's aspect ratio.
   --  When preserving the aspect ratio, a `width` of -1 will cause the image
   --  to be scaled to the exact given height, and a `height` of -1 will cause
   --  the image to be scaled to the exact given width. When not preserving
   --  aspect ratio, a `width` or `height` of -1 means to not scale the image
   --  at all in that dimension. Negative values for `width` and `height` are
   --  allowed since 2.8.
   --  Since: gtk+ 2.6
   --  @param Filename Name of file to load, in the GLib file name encoding
   --  @param Width The width the image should have or -1 to not constrain the
   --  width
   --  @param Height The height the image should have or -1 to not constrain
   --  the height
   --  @param Preserve_Aspect_Ratio `TRUE` to preserve the image's aspect
   --  ratio
   --  @param Error the return location for a recoverable error

   procedure Initialize_From_File_At_Scale
      (Self                  : not null access Gdk_Pixbuf_Record'Class;
       Filename              : UTF8_String;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Error                 : out Glib.Error.GError);
   --  Creates a new pixbuf by loading an image from a file.
   --  The file format is detected automatically.
   --  If `NULL` is returned, then Error will be set. Possible errors are:
   --  - the file could not be opened - there is no loader for the file's
   --  format - there is not enough memory to allocate the image buffer - the
   --  image buffer contains invalid data
   --  The error domains are `GDK_PIXBUF_ERROR` and `G_FILE_ERROR`.
   --  The image will be scaled to fit in the requested size, optionally
   --  preserving the image's aspect ratio.
   --  When preserving the aspect ratio, a `width` of -1 will cause the image
   --  to be scaled to the exact given height, and a `height` of -1 will cause
   --  the image to be scaled to the exact given width. When not preserving
   --  aspect ratio, a `width` or `height` of -1 means to not scale the image
   --  at all in that dimension. Negative values for `width` and `height` are
   --  allowed since 2.8.
   --  Since: gtk+ 2.6
   --  Initialize_From_File_At_Scale does nothing if the object was already
   --  created with another call to Initialize* or G_New.
   --  @param Filename Name of file to load, in the GLib file name encoding
   --  @param Width The width the image should have or -1 to not constrain the
   --  width
   --  @param Height The height the image should have or -1 to not constrain
   --  the height
   --  @param Preserve_Aspect_Ratio `TRUE` to preserve the image's aspect
   --  ratio
   --  @param Error the return location for a recoverable error

   function Gdk_Pixbuf_New_From_File_At_Scale
      (Filename              : UTF8_String;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Error                 : out Glib.Error.GError) return Gdk_Pixbuf;
   --  Creates a new pixbuf by loading an image from a file.
   --  The file format is detected automatically.
   --  If `NULL` is returned, then Error will be set. Possible errors are:
   --  - the file could not be opened - there is no loader for the file's
   --  format - there is not enough memory to allocate the image buffer - the
   --  image buffer contains invalid data
   --  The error domains are `GDK_PIXBUF_ERROR` and `G_FILE_ERROR`.
   --  The image will be scaled to fit in the requested size, optionally
   --  preserving the image's aspect ratio.
   --  When preserving the aspect ratio, a `width` of -1 will cause the image
   --  to be scaled to the exact given height, and a `height` of -1 will cause
   --  the image to be scaled to the exact given width. When not preserving
   --  aspect ratio, a `width` or `height` of -1 means to not scale the image
   --  at all in that dimension. Negative values for `width` and `height` are
   --  allowed since 2.8.
   --  Since: gtk+ 2.6
   --  @param Filename Name of file to load, in the GLib file name encoding
   --  @param Width The width the image should have or -1 to not constrain the
   --  width
   --  @param Height The height the image should have or -1 to not constrain
   --  the height
   --  @param Preserve_Aspect_Ratio `TRUE` to preserve the image's aspect
   --  ratio
   --  @param Error the return location for a recoverable error

   procedure Gdk_New_From_File_At_Size
      (Self     : out Gdk_Pixbuf;
       Filename : UTF8_String;
       Width    : Glib.Gint;
       Height   : Glib.Gint;
       Error    : out Glib.Error.GError);
   --  Creates a new pixbuf by loading an image from a file.
   --  The file format is detected automatically.
   --  If `NULL` is returned, then Error will be set. Possible errors are:
   --  - the file could not be opened - there is no loader for the file's
   --  format - there is not enough memory to allocate the image buffer - the
   --  image buffer contains invalid data
   --  The error domains are `GDK_PIXBUF_ERROR` and `G_FILE_ERROR`.
   --  The image will be scaled to fit in the requested size, preserving the
   --  image's aspect ratio. Note that the returned pixbuf may be smaller than
   --  `width` x `height`, if the aspect ratio requires it. To load and image
   --  at the requested size, regardless of aspect ratio, use
   --  [ctorGdkpixbuf.Pixbuf.new_from_file_at_scale].
   --  Since: gtk+ 2.4
   --  @param Filename Name of file to load, in the GLib file name encoding
   --  @param Width The width the image should have or -1 to not constrain the
   --  width
   --  @param Height The height the image should have or -1 to not constrain
   --  the height
   --  @param Error the return location for a recoverable error

   procedure Initialize_From_File_At_Size
      (Self     : not null access Gdk_Pixbuf_Record'Class;
       Filename : UTF8_String;
       Width    : Glib.Gint;
       Height   : Glib.Gint;
       Error    : out Glib.Error.GError);
   --  Creates a new pixbuf by loading an image from a file.
   --  The file format is detected automatically.
   --  If `NULL` is returned, then Error will be set. Possible errors are:
   --  - the file could not be opened - there is no loader for the file's
   --  format - there is not enough memory to allocate the image buffer - the
   --  image buffer contains invalid data
   --  The error domains are `GDK_PIXBUF_ERROR` and `G_FILE_ERROR`.
   --  The image will be scaled to fit in the requested size, preserving the
   --  image's aspect ratio. Note that the returned pixbuf may be smaller than
   --  `width` x `height`, if the aspect ratio requires it. To load and image
   --  at the requested size, regardless of aspect ratio, use
   --  [ctorGdkpixbuf.Pixbuf.new_from_file_at_scale].
   --  Since: gtk+ 2.4
   --  Initialize_From_File_At_Size does nothing if the object was already
   --  created with another call to Initialize* or G_New.
   --  @param Filename Name of file to load, in the GLib file name encoding
   --  @param Width The width the image should have or -1 to not constrain the
   --  width
   --  @param Height The height the image should have or -1 to not constrain
   --  the height
   --  @param Error the return location for a recoverable error

   function Gdk_Pixbuf_New_From_File_At_Size
      (Filename : UTF8_String;
       Width    : Glib.Gint;
       Height   : Glib.Gint;
       Error    : out Glib.Error.GError) return Gdk_Pixbuf;
   --  Creates a new pixbuf by loading an image from a file.
   --  The file format is detected automatically.
   --  If `NULL` is returned, then Error will be set. Possible errors are:
   --  - the file could not be opened - there is no loader for the file's
   --  format - there is not enough memory to allocate the image buffer - the
   --  image buffer contains invalid data
   --  The error domains are `GDK_PIXBUF_ERROR` and `G_FILE_ERROR`.
   --  The image will be scaled to fit in the requested size, preserving the
   --  image's aspect ratio. Note that the returned pixbuf may be smaller than
   --  `width` x `height`, if the aspect ratio requires it. To load and image
   --  at the requested size, regardless of aspect ratio, use
   --  [ctorGdkpixbuf.Pixbuf.new_from_file_at_scale].
   --  Since: gtk+ 2.4
   --  @param Filename Name of file to load, in the GLib file name encoding
   --  @param Width The width the image should have or -1 to not constrain the
   --  width
   --  @param Height The height the image should have or -1 to not constrain
   --  the height
   --  @param Error the return location for a recoverable error

   procedure Gdk_New_From_Inline
      (Self        : out Gdk_Pixbuf;
       Data_Length : Glib.Gint;
       Data        : Guint8_Array;
       Error       : out Glib.Error.GError);
   --  Creates a `GdkPixbuf` from a flat representation that is suitable for
   --  storing as inline data in a program.
   --  This is useful if you want to ship a program with images, but don't
   --  want to depend on any external files.
   --  GdkPixbuf ships with a program called `gdk-pixbuf-csource`, which
   --  allows for conversion of `GdkPixbuf`s into such a inline representation.
   --  In almost all cases, you should pass the `--raw` option to
   --  `gdk-pixbuf-csource`. A sample invocation would be:
   --  ``` gdk-pixbuf-csource --raw --name=myimage_inline myimage.png ```
   --  For the typical case where the inline pixbuf is read-only static data,
   --  you don't need to copy the pixel data unless you intend to write to it,
   --  so you can pass `FALSE` for `copy_pixels`. If you pass `--rle` to
   --  `gdk-pixbuf-csource`, a copy will be made even if `copy_pixels` is
   --  `FALSE`, so using this option is generally a bad idea.
   --  If you create a pixbuf from const inline data compiled into your
   --  program, it's probably safe to ignore errors and disable length checks,
   --  since things will always succeed:
   --  ```c pixbuf = gdk_pixbuf_new_from_inline (-1, myimage_inline, FALSE,
   --  NULL); ```
   --  For non-const inline data, you could get out of memory. For untrusted
   --  inline data located at runtime, you could have corrupt inline data in
   --  addition.
   --  @param Data_Length Length in bytes of the `data` argument or -1 to
   --  disable length checks
   --  @param Data Byte data containing a serialized `GdkPixdata` structure
   --  @param Error the return location for a recoverable error

   procedure Initialize_From_Inline
      (Self        : not null access Gdk_Pixbuf_Record'Class;
       Data_Length : Glib.Gint;
       Data        : Guint8_Array;
       Error       : out Glib.Error.GError);
   --  Creates a `GdkPixbuf` from a flat representation that is suitable for
   --  storing as inline data in a program.
   --  This is useful if you want to ship a program with images, but don't
   --  want to depend on any external files.
   --  GdkPixbuf ships with a program called `gdk-pixbuf-csource`, which
   --  allows for conversion of `GdkPixbuf`s into such a inline representation.
   --  In almost all cases, you should pass the `--raw` option to
   --  `gdk-pixbuf-csource`. A sample invocation would be:
   --  ``` gdk-pixbuf-csource --raw --name=myimage_inline myimage.png ```
   --  For the typical case where the inline pixbuf is read-only static data,
   --  you don't need to copy the pixel data unless you intend to write to it,
   --  so you can pass `FALSE` for `copy_pixels`. If you pass `--rle` to
   --  `gdk-pixbuf-csource`, a copy will be made even if `copy_pixels` is
   --  `FALSE`, so using this option is generally a bad idea.
   --  If you create a pixbuf from const inline data compiled into your
   --  program, it's probably safe to ignore errors and disable length checks,
   --  since things will always succeed:
   --  ```c pixbuf = gdk_pixbuf_new_from_inline (-1, myimage_inline, FALSE,
   --  NULL); ```
   --  For non-const inline data, you could get out of memory. For untrusted
   --  inline data located at runtime, you could have corrupt inline data in
   --  addition.
   --  Initialize_From_Inline does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Data_Length Length in bytes of the `data` argument or -1 to
   --  disable length checks
   --  @param Data Byte data containing a serialized `GdkPixdata` structure
   --  @param Error the return location for a recoverable error

   function Gdk_Pixbuf_New_From_Inline
      (Data_Length : Glib.Gint;
       Data        : Guint8_Array;
       Error       : out Glib.Error.GError) return Gdk_Pixbuf;
   --  Creates a `GdkPixbuf` from a flat representation that is suitable for
   --  storing as inline data in a program.
   --  This is useful if you want to ship a program with images, but don't
   --  want to depend on any external files.
   --  GdkPixbuf ships with a program called `gdk-pixbuf-csource`, which
   --  allows for conversion of `GdkPixbuf`s into such a inline representation.
   --  In almost all cases, you should pass the `--raw` option to
   --  `gdk-pixbuf-csource`. A sample invocation would be:
   --  ``` gdk-pixbuf-csource --raw --name=myimage_inline myimage.png ```
   --  For the typical case where the inline pixbuf is read-only static data,
   --  you don't need to copy the pixel data unless you intend to write to it,
   --  so you can pass `FALSE` for `copy_pixels`. If you pass `--rle` to
   --  `gdk-pixbuf-csource`, a copy will be made even if `copy_pixels` is
   --  `FALSE`, so using this option is generally a bad idea.
   --  If you create a pixbuf from const inline data compiled into your
   --  program, it's probably safe to ignore errors and disable length checks,
   --  since things will always succeed:
   --  ```c pixbuf = gdk_pixbuf_new_from_inline (-1, myimage_inline, FALSE,
   --  NULL); ```
   --  For non-const inline data, you could get out of memory. For untrusted
   --  inline data located at runtime, you could have corrupt inline data in
   --  addition.
   --  @param Data_Length Length in bytes of the `data` argument or -1 to
   --  disable length checks
   --  @param Data Byte data containing a serialized `GdkPixdata` structure
   --  @param Error the return location for a recoverable error

   procedure Gdk_New_From_Resource
      (Self          : out Gdk_Pixbuf;
       Resource_Path : UTF8_String;
       Error         : out Glib.Error.GError);
   --  Creates a new pixbuf by loading an image from an resource.
   --  The file format is detected automatically. If `NULL` is returned, then
   --  Error will be set.
   --  Since: gtk+ 2.26
   --  @param Resource_Path the path of the resource file
   --  @param Error the return location for a recoverable error

   procedure Initialize_From_Resource
      (Self          : not null access Gdk_Pixbuf_Record'Class;
       Resource_Path : UTF8_String;
       Error         : out Glib.Error.GError);
   --  Creates a new pixbuf by loading an image from an resource.
   --  The file format is detected automatically. If `NULL` is returned, then
   --  Error will be set.
   --  Since: gtk+ 2.26
   --  Initialize_From_Resource does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Resource_Path the path of the resource file
   --  @param Error the return location for a recoverable error

   function Gdk_Pixbuf_New_From_Resource
      (Resource_Path : UTF8_String;
       Error         : out Glib.Error.GError) return Gdk_Pixbuf;
   --  Creates a new pixbuf by loading an image from an resource.
   --  The file format is detected automatically. If `NULL` is returned, then
   --  Error will be set.
   --  Since: gtk+ 2.26
   --  @param Resource_Path the path of the resource file
   --  @param Error the return location for a recoverable error

   procedure Gdk_New_From_Resource_At_Scale
      (Self                  : out Gdk_Pixbuf;
       Resource_Path         : UTF8_String;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Error                 : out Glib.Error.GError);
   --  Creates a new pixbuf by loading an image from an resource.
   --  The file format is detected automatically. If `NULL` is returned, then
   --  Error will be set.
   --  The image will be scaled to fit in the requested size, optionally
   --  preserving the image's aspect ratio. When preserving the aspect ratio, a
   --  Width of -1 will cause the image to be scaled to the exact given height,
   --  and a Height of -1 will cause the image to be scaled to the exact given
   --  width. When not preserving aspect ratio, a Width or Height of -1 means
   --  to not scale the image at all in that dimension.
   --  The stream is not closed.
   --  Since: gtk+ 2.26
   --  @param Resource_Path the path of the resource file
   --  @param Width The width the image should have or -1 to not constrain the
   --  width
   --  @param Height The height the image should have or -1 to not constrain
   --  the height
   --  @param Preserve_Aspect_Ratio `TRUE` to preserve the image's aspect
   --  ratio
   --  @param Error the return location for a recoverable error

   procedure Initialize_From_Resource_At_Scale
      (Self                  : not null access Gdk_Pixbuf_Record'Class;
       Resource_Path         : UTF8_String;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Error                 : out Glib.Error.GError);
   --  Creates a new pixbuf by loading an image from an resource.
   --  The file format is detected automatically. If `NULL` is returned, then
   --  Error will be set.
   --  The image will be scaled to fit in the requested size, optionally
   --  preserving the image's aspect ratio. When preserving the aspect ratio, a
   --  Width of -1 will cause the image to be scaled to the exact given height,
   --  and a Height of -1 will cause the image to be scaled to the exact given
   --  width. When not preserving aspect ratio, a Width or Height of -1 means
   --  to not scale the image at all in that dimension.
   --  The stream is not closed.
   --  Since: gtk+ 2.26
   --  Initialize_From_Resource_At_Scale does nothing if the object was
   --  already created with another call to Initialize* or G_New.
   --  @param Resource_Path the path of the resource file
   --  @param Width The width the image should have or -1 to not constrain the
   --  width
   --  @param Height The height the image should have or -1 to not constrain
   --  the height
   --  @param Preserve_Aspect_Ratio `TRUE` to preserve the image's aspect
   --  ratio
   --  @param Error the return location for a recoverable error

   function Gdk_Pixbuf_New_From_Resource_At_Scale
      (Resource_Path         : UTF8_String;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Error                 : out Glib.Error.GError) return Gdk_Pixbuf;
   --  Creates a new pixbuf by loading an image from an resource.
   --  The file format is detected automatically. If `NULL` is returned, then
   --  Error will be set.
   --  The image will be scaled to fit in the requested size, optionally
   --  preserving the image's aspect ratio. When preserving the aspect ratio, a
   --  Width of -1 will cause the image to be scaled to the exact given height,
   --  and a Height of -1 will cause the image to be scaled to the exact given
   --  width. When not preserving aspect ratio, a Width or Height of -1 means
   --  to not scale the image at all in that dimension.
   --  The stream is not closed.
   --  Since: gtk+ 2.26
   --  @param Resource_Path the path of the resource file
   --  @param Width The width the image should have or -1 to not constrain the
   --  width
   --  @param Height The height the image should have or -1 to not constrain
   --  the height
   --  @param Preserve_Aspect_Ratio `TRUE` to preserve the image's aspect
   --  ratio
   --  @param Error the return location for a recoverable error

   procedure Gdk_New_From_Stream
      (Self        : out Gdk_Pixbuf;
       Stream      : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Error       : out Glib.Error.GError);
   --  Creates a new pixbuf by loading an image from an input stream.
   --  The file format is detected automatically.
   --  If `NULL` is returned, then `error` will be set.
   --  The `cancellable` can be used to abort the operation from another
   --  thread. If the operation was cancelled, the error `G_IO_ERROR_CANCELLED`
   --  will be returned. Other possible errors are in the `GDK_PIXBUF_ERROR`
   --  and `G_IO_ERROR` domains.
   --  The stream is not closed.
   --  Since: gtk+ 2.14
   --  @param Stream a `GInputStream` to load the pixbuf from
   --  @param Cancellable optional `GCancellable` object, `NULL` to ignore
   --  @param Error the return location for a recoverable error

   procedure Initialize_From_Stream
      (Self        : not null access Gdk_Pixbuf_Record'Class;
       Stream      : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Error       : out Glib.Error.GError);
   --  Creates a new pixbuf by loading an image from an input stream.
   --  The file format is detected automatically.
   --  If `NULL` is returned, then `error` will be set.
   --  The `cancellable` can be used to abort the operation from another
   --  thread. If the operation was cancelled, the error `G_IO_ERROR_CANCELLED`
   --  will be returned. Other possible errors are in the `GDK_PIXBUF_ERROR`
   --  and `G_IO_ERROR` domains.
   --  The stream is not closed.
   --  Since: gtk+ 2.14
   --  Initialize_From_Stream does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Stream a `GInputStream` to load the pixbuf from
   --  @param Cancellable optional `GCancellable` object, `NULL` to ignore
   --  @param Error the return location for a recoverable error

   function Gdk_Pixbuf_New_From_Stream
      (Stream      : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Error       : out Glib.Error.GError) return Gdk_Pixbuf;
   --  Creates a new pixbuf by loading an image from an input stream.
   --  The file format is detected automatically.
   --  If `NULL` is returned, then `error` will be set.
   --  The `cancellable` can be used to abort the operation from another
   --  thread. If the operation was cancelled, the error `G_IO_ERROR_CANCELLED`
   --  will be returned. Other possible errors are in the `GDK_PIXBUF_ERROR`
   --  and `G_IO_ERROR` domains.
   --  The stream is not closed.
   --  Since: gtk+ 2.14
   --  @param Stream a `GInputStream` to load the pixbuf from
   --  @param Cancellable optional `GCancellable` object, `NULL` to ignore
   --  @param Error the return location for a recoverable error

   procedure Gdk_New_From_Stream_At_Scale
      (Self                  : out Gdk_Pixbuf;
       Stream                : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Cancellable           : access Glib.Cancellable.Gcancellable_Record'Class;
       Error                 : out Glib.Error.GError);
   --  Creates a new pixbuf by loading an image from an input stream.
   --  The file format is detected automatically. If `NULL` is returned, then
   --  Error will be set. The Cancellable can be used to abort the operation
   --  from another thread. If the operation was cancelled, the error
   --  `G_IO_ERROR_CANCELLED` will be returned. Other possible errors are in
   --  the `GDK_PIXBUF_ERROR` and `G_IO_ERROR` domains.
   --  The image will be scaled to fit in the requested size, optionally
   --  preserving the image's aspect ratio.
   --  When preserving the aspect ratio, a `width` of -1 will cause the image
   --  to be scaled to the exact given height, and a `height` of -1 will cause
   --  the image to be scaled to the exact given width. If both `width` and
   --  `height` are given, this function will behave as if the smaller of the
   --  two values is passed as -1.
   --  When not preserving aspect ratio, a `width` or `height` of -1 means to
   --  not scale the image at all in that dimension.
   --  The stream is not closed.
   --  Since: gtk+ 2.14
   --  @param Stream a `GInputStream` to load the pixbuf from
   --  @param Width The width the image should have or -1 to not constrain the
   --  width
   --  @param Height The height the image should have or -1 to not constrain
   --  the height
   --  @param Preserve_Aspect_Ratio `TRUE` to preserve the image's aspect
   --  ratio
   --  @param Cancellable optional `GCancellable` object, `NULL` to ignore
   --  @param Error the return location for a recoverable error

   procedure Initialize_From_Stream_At_Scale
      (Self                  : not null access Gdk_Pixbuf_Record'Class;
       Stream                : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Cancellable           : access Glib.Cancellable.Gcancellable_Record'Class;
       Error                 : out Glib.Error.GError);
   --  Creates a new pixbuf by loading an image from an input stream.
   --  The file format is detected automatically. If `NULL` is returned, then
   --  Error will be set. The Cancellable can be used to abort the operation
   --  from another thread. If the operation was cancelled, the error
   --  `G_IO_ERROR_CANCELLED` will be returned. Other possible errors are in
   --  the `GDK_PIXBUF_ERROR` and `G_IO_ERROR` domains.
   --  The image will be scaled to fit in the requested size, optionally
   --  preserving the image's aspect ratio.
   --  When preserving the aspect ratio, a `width` of -1 will cause the image
   --  to be scaled to the exact given height, and a `height` of -1 will cause
   --  the image to be scaled to the exact given width. If both `width` and
   --  `height` are given, this function will behave as if the smaller of the
   --  two values is passed as -1.
   --  When not preserving aspect ratio, a `width` or `height` of -1 means to
   --  not scale the image at all in that dimension.
   --  The stream is not closed.
   --  Since: gtk+ 2.14
   --  Initialize_From_Stream_At_Scale does nothing if the object was already
   --  created with another call to Initialize* or G_New.
   --  @param Stream a `GInputStream` to load the pixbuf from
   --  @param Width The width the image should have or -1 to not constrain the
   --  width
   --  @param Height The height the image should have or -1 to not constrain
   --  the height
   --  @param Preserve_Aspect_Ratio `TRUE` to preserve the image's aspect
   --  ratio
   --  @param Cancellable optional `GCancellable` object, `NULL` to ignore
   --  @param Error the return location for a recoverable error

   function Gdk_Pixbuf_New_From_Stream_At_Scale
      (Stream                : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Cancellable           : access Glib.Cancellable.Gcancellable_Record'Class;
       Error                 : out Glib.Error.GError) return Gdk_Pixbuf;
   --  Creates a new pixbuf by loading an image from an input stream.
   --  The file format is detected automatically. If `NULL` is returned, then
   --  Error will be set. The Cancellable can be used to abort the operation
   --  from another thread. If the operation was cancelled, the error
   --  `G_IO_ERROR_CANCELLED` will be returned. Other possible errors are in
   --  the `GDK_PIXBUF_ERROR` and `G_IO_ERROR` domains.
   --  The image will be scaled to fit in the requested size, optionally
   --  preserving the image's aspect ratio.
   --  When preserving the aspect ratio, a `width` of -1 will cause the image
   --  to be scaled to the exact given height, and a `height` of -1 will cause
   --  the image to be scaled to the exact given width. If both `width` and
   --  `height` are given, this function will behave as if the smaller of the
   --  two values is passed as -1.
   --  When not preserving aspect ratio, a `width` or `height` of -1 means to
   --  not scale the image at all in that dimension.
   --  The stream is not closed.
   --  Since: gtk+ 2.14
   --  @param Stream a `GInputStream` to load the pixbuf from
   --  @param Width The width the image should have or -1 to not constrain the
   --  width
   --  @param Height The height the image should have or -1 to not constrain
   --  the height
   --  @param Preserve_Aspect_Ratio `TRUE` to preserve the image's aspect
   --  ratio
   --  @param Cancellable optional `GCancellable` object, `NULL` to ignore
   --  @param Error the return location for a recoverable error

   procedure Gdk_New_From_Stream_Finish
      (Self         : out Gdk_Pixbuf;
       Async_Result : Glib.G_Async_Result;
       Error        : out Glib.Error.GError);
   --  Finishes an asynchronous pixbuf creation operation started with
   --  Gdk.Pixbuf.New_From_Stream_Async.
   --  Since: gtk+ 2.24
   --  @param Async_Result a `GAsyncResult`
   --  @param Error the return location for a recoverable error

   procedure Initialize_From_Stream_Finish
      (Self         : not null access Gdk_Pixbuf_Record'Class;
       Async_Result : Glib.G_Async_Result;
       Error        : out Glib.Error.GError);
   --  Finishes an asynchronous pixbuf creation operation started with
   --  Gdk.Pixbuf.New_From_Stream_Async.
   --  Since: gtk+ 2.24
   --  Initialize_From_Stream_Finish does nothing if the object was already
   --  created with another call to Initialize* or G_New.
   --  @param Async_Result a `GAsyncResult`
   --  @param Error the return location for a recoverable error

   function Gdk_Pixbuf_New_From_Stream_Finish
      (Async_Result : Glib.G_Async_Result;
       Error        : out Glib.Error.GError) return Gdk_Pixbuf;
   --  Finishes an asynchronous pixbuf creation operation started with
   --  Gdk.Pixbuf.New_From_Stream_Async.
   --  Since: gtk+ 2.24
   --  @param Async_Result a `GAsyncResult`
   --  @param Error the return location for a recoverable error

   procedure Gdk_New_From_Xpm_Data
      (Self : out Gdk_Pixbuf;
       Data : GNAT.Strings.String_List);
   --  Creates a new pixbuf by parsing XPM data in memory.
   --  This data is commonly the result of including an XPM file into a
   --  program's C source.
   --  @param Data Pointer to inline XPM data.

   procedure Initialize_From_Xpm_Data
      (Self : not null access Gdk_Pixbuf_Record'Class;
       Data : GNAT.Strings.String_List);
   --  Creates a new pixbuf by parsing XPM data in memory.
   --  This data is commonly the result of including an XPM file into a
   --  program's C source.
   --  Initialize_From_Xpm_Data does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Data Pointer to inline XPM data.

   function Gdk_Pixbuf_New_From_Xpm_Data
      (Data : GNAT.Strings.String_List) return Gdk_Pixbuf;
   --  Creates a new pixbuf by parsing XPM data in memory.
   --  This data is commonly the result of including an XPM file into a
   --  program's C source.
   --  @param Data Pointer to inline XPM data.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_pixbuf_get_type");

   -------------
   -- Methods --
   -------------

   function Add_Alpha
      (Self             : not null access Gdk_Pixbuf_Record;
       Substitute_Color : Boolean;
       R                : Guchar;
       G                : Guchar;
       B                : Guchar) return Gdk_Pixbuf;
   --  Takes an existing pixbuf and adds an alpha channel to it.
   --  If the existing pixbuf already had an alpha channel, the channel values
   --  are copied from the original; otherwise, the alpha channel is
   --  initialized to 255 (full opacity).
   --  If `substitute_color` is `TRUE`, then the color specified by the (`r`,
   --  `g`, `b`) arguments will be assigned zero opacity. That is, if you pass
   --  `(255, 255, 255)` for the substitute color, all white pixels will become
   --  fully transparent.
   --  If `substitute_color` is `FALSE`, then the (`r`, `g`, `b`) arguments
   --  will be ignored.
   --  @param Substitute_Color Whether to set a color to zero opacity.
   --  @param R Red value to substitute.
   --  @param G Green value to substitute.
   --  @param B Blue value to substitute.
   --  @return A newly-created pixbuf. Has transfer-ownership='full'.

   function Apply_Embedded_Orientation
      (Self : not null access Gdk_Pixbuf_Record) return Gdk_Pixbuf;
   --  Takes an existing pixbuf and checks for the presence of an associated
   --  "orientation" option.
   --  The orientation option may be provided by the JPEG loader (which reads
   --  the exif orientation tag) or the TIFF loader (which reads the TIFF
   --  orientation tag, and compensates it for the partial transforms performed
   --  by libtiff).
   --  If an orientation option/tag is present, the appropriate transform will
   --  be performed so that the pixbuf is oriented correctly.
   --  Since: gtk+ 2.12
   --  @return A newly-created pixbuf. Has transfer-ownership='full'.

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
       Overall_Alpha : Glib.Gint);
   --  Creates a transformation of the source image Src by scaling by Scale_X
   --  and Scale_Y then translating by Offset_X and Offset_Y.
   --  This gives an image in the coordinates of the destination pixbuf. The
   --  rectangle (Dest_X, Dest_Y, Dest_Width, Dest_Height) is then alpha
   --  blended onto the corresponding rectangle of the original destination
   --  image.
   --  When the destination rectangle contains parts not in the source image,
   --  the data at the edges of the source image is replicated to infinity.
   --  ![](composite.png)
   --  @param Dest the Gdk.Pixbuf.Gdk_Pixbuf into which to render the results
   --  @param Dest_X the left coordinate for region to render
   --  @param Dest_Y the top coordinate for region to render
   --  @param Dest_Width the width of the region to render
   --  @param Dest_Height the height of the region to render
   --  @param Offset_X the offset in the X direction (currently rounded to an
   --  integer)
   --  @param Offset_Y the offset in the Y direction (currently rounded to an
   --  integer)
   --  @param Scale_X the scale factor in the X direction
   --  @param Scale_Y the scale factor in the Y direction
   --  @param Interp_Type the interpolation type for the transformation.
   --  @param Overall_Alpha overall alpha for source image (0..255)

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
       Color2        : Guint32);
   --  Creates a transformation of the source image Src by scaling by Scale_X
   --  and Scale_Y then translating by Offset_X and Offset_Y, then alpha blends
   --  the rectangle (Dest_X ,Dest_Y, Dest_Width, Dest_Height) of the resulting
   --  image with a checkboard of the colors Color1 and Color2 and renders it
   --  onto the destination image.
   --  If the source image has no alpha channel, and Overall_Alpha is 255, a
   --  fast path is used which omits the alpha blending and just performs the
   --  scaling.
   --  See Gdk.Pixbuf.Composite_Color_Simple for a simpler variant of this
   --  function suitable for many tasks.
   --  @param Dest the Gdk.Pixbuf.Gdk_Pixbuf into which to render the results
   --  @param Dest_X the left coordinate for region to render
   --  @param Dest_Y the top coordinate for region to render
   --  @param Dest_Width the width of the region to render
   --  @param Dest_Height the height of the region to render
   --  @param Offset_X the offset in the X direction (currently rounded to an
   --  integer)
   --  @param Offset_Y the offset in the Y direction (currently rounded to an
   --  integer)
   --  @param Scale_X the scale factor in the X direction
   --  @param Scale_Y the scale factor in the Y direction
   --  @param Interp_Type the interpolation type for the transformation.
   --  @param Overall_Alpha overall alpha for source image (0..255)
   --  @param Check_X the X offset for the checkboard (origin of checkboard is
   --  at -Check_X, -Check_Y)
   --  @param Check_Y the Y offset for the checkboard
   --  @param Check_Size the size of checks in the checkboard (must be a power
   --  of two)
   --  @param Color1 the color of check at upper left
   --  @param Color2 the color of the other check

   function Composite_Color_Simple
      (Self          : not null access Gdk_Pixbuf_Record;
       Dest_Width    : Glib.Gint;
       Dest_Height   : Glib.Gint;
       Interp_Type   : Gdk_Interp_Type;
       Overall_Alpha : Glib.Gint;
       Check_Size    : Glib.Gint;
       Color1        : Guint32;
       Color2        : Guint32) return Gdk_Pixbuf;
   --  Creates a new pixbuf by scaling `src` to `dest_width` x `dest_height`
   --  and alpha blending the result with a checkboard of colors `color1` and
   --  `color2`.
   --  @param Dest_Width the width of destination image
   --  @param Dest_Height the height of destination image
   --  @param Interp_Type the interpolation type for the transformation.
   --  @param Overall_Alpha overall alpha for source image (0..255)
   --  @param Check_Size the size of checks in the checkboard (must be a power
   --  of two)
   --  @param Color1 the color of check at upper left
   --  @param Color2 the color of the other check
   --  @return the new pixbuf. Has transfer-ownership='full'.

   function Copy
      (Self : not null access Gdk_Pixbuf_Record) return Gdk_Pixbuf;
   --  Creates a new `GdkPixbuf` with a copy of the information in the
   --  specified `pixbuf`.
   --  Note that this does not copy the options set on the original
   --  `GdkPixbuf`, use Gdk.Pixbuf.Copy_Options for this.
   --  @return A newly-created pixbuf. Has transfer-ownership='full'.

   procedure Copy_Area
      (Self        : not null access Gdk_Pixbuf_Record;
       Src_X       : Glib.Gint;
       Src_Y       : Glib.Gint;
       Width       : Glib.Gint;
       Height      : Glib.Gint;
       Dest_Pixbuf : not null access Gdk_Pixbuf_Record'Class;
       Dest_X      : Glib.Gint;
       Dest_Y      : Glib.Gint);
   --  Copies a rectangular area from `src_pixbuf` to `dest_pixbuf`.
   --  Conversion of pixbuf formats is done automatically.
   --  If the source rectangle overlaps the destination rectangle on the same
   --  pixbuf, it will be overwritten during the copy operation. Therefore, you
   --  can not use this function to scroll a pixbuf.
   --  @param Src_X Source X coordinate within Src_Pixbuf.
   --  @param Src_Y Source Y coordinate within Src_Pixbuf.
   --  @param Width Width of the area to copy.
   --  @param Height Height of the area to copy.
   --  @param Dest_Pixbuf Destination pixbuf.
   --  @param Dest_X X coordinate within Dest_Pixbuf.
   --  @param Dest_Y Y coordinate within Dest_Pixbuf.

   function Copy_Options
      (Self        : not null access Gdk_Pixbuf_Record;
       Dest_Pixbuf : not null access Gdk_Pixbuf_Record'Class) return Boolean;
   --  Copies the key/value pair options attached to a `GdkPixbuf` to another
   --  `GdkPixbuf`.
   --  This is useful to keep original metadata after having manipulated a
   --  file. However be careful to remove metadata which you've already
   --  applied, such as the "orientation" option after rotating the image.
   --  Since: gtk+ 2.36
   --  @param Dest_Pixbuf the destination pixbuf
   --  @return `TRUE` on success.

   procedure Fill
      (Self  : not null access Gdk_Pixbuf_Record;
       Pixel : Guint32);
   --  Clears a pixbuf to the given RGBA value, converting the RGBA value into
   --  the pixbuf's pixel format.
   --  The alpha component will be ignored if the pixbuf doesn't have an alpha
   --  channel.
   --  @param Pixel RGBA pixel to used to clear (`0xffffffff` is opaque white,
   --  `0x00000000` transparent black)

   function Flip
      (Self       : not null access Gdk_Pixbuf_Record;
       Horizontal : Boolean) return Gdk_Pixbuf;
   --  Flips a pixbuf horizontally or vertically and returns the result in a
   --  new pixbuf.
   --  Since: gtk+ 2.6
   --  @param Horizontal `TRUE` to flip horizontally, `FALSE` to flip
   --  vertically
   --  @return the new pixbuf. Has transfer-ownership='full'.

   function Get_Bits_Per_Sample
      (Self : not null access Gdk_Pixbuf_Record) return Glib.Gint;
   --  Queries the number of bits per color sample in a pixbuf.
   --  @return Number of bits per color sample.

   function Get_Byte_Length
      (Self : not null access Gdk_Pixbuf_Record) return Gsize;
   --  Returns the length of the pixel data, in bytes.
   --  Since: gtk+ 2.26
   --  @return The length of the pixel data.

   function Get_Colorspace
      (Self : not null access Gdk_Pixbuf_Record) return Gdk_Colorspace;
   --  Queries the color space of a pixbuf.
   --  @return Color space.

   function Get_Has_Alpha
      (Self : not null access Gdk_Pixbuf_Record) return Boolean;
   --  Queries whether a pixbuf has an alpha channel (opacity information).
   --  @return `TRUE` if it has an alpha channel, `FALSE` otherwise.

   function Get_Height
      (Self : not null access Gdk_Pixbuf_Record) return Glib.Gint;
   --  Queries the height of a pixbuf.
   --  @return Height in pixels.

   function Get_N_Channels
      (Self : not null access Gdk_Pixbuf_Record) return Glib.Gint;
   --  Queries the number of channels of a pixbuf.
   --  @return Number of channels.

   function Get_Option
      (Self : not null access Gdk_Pixbuf_Record;
       Key  : UTF8_String) return UTF8_String;
   --  Looks up Key in the list of options that may have been attached to the
   --  Pixbuf when it was loaded, or that may have been attached by another
   --  function using Gdk.Pixbuf.Set_Option.
   --  For instance, the ANI loader provides "Title" and "Artist" options. The
   --  ICO, XBM, and XPM loaders provide "x_hot" and "y_hot" hot-spot options
   --  for cursor definitions. The PNG loader provides the tEXt ancillary chunk
   --  key/value pairs as options. Since 2.12, the TIFF and JPEG loaders return
   --  an "orientation" option string that corresponds to the embedded
   --  TIFF/Exif orientation tag (if present). Since 2.32, the TIFF loader sets
   --  the "multipage" option string to "yes" when a multi-page TIFF is loaded.
   --  Since 2.32 the JPEG and PNG loaders set "x-dpi" and "y-dpi" if the file
   --  contains image density information in dots per inch. Since 2.36.6, the
   --  JPEG loader sets the "comment" option with the comment EXIF tag.
   --  @param Key a nul-terminated string.
   --  @return the value associated with `key`

   function Set_Option
      (Self  : not null access Gdk_Pixbuf_Record;
       Key   : UTF8_String;
       Value : UTF8_String) return Boolean;
   --  Attaches a key/value pair as an option to a `GdkPixbuf`.
   --  If `key` already exists in the list of options attached to the
   --  `pixbuf`, the new value is ignored and `FALSE` is returned.
   --  Since: gtk+ 2.2
   --  @param Key a nul-terminated string.
   --  @param Value a nul-terminated string.
   --  @return `TRUE` on success

   function Get_Options
      (Self : not null access Gdk_Pixbuf_Record) return System.Address;
   --  Returns a `GHashTable` with a list of all the options that may have
   --  been attached to the `pixbuf` when it was loaded, or that may have been
   --  attached by another function using [methodGdkpixbuf.Pixbuf.set_option].
   --  Since: gtk+ 2.32
   --  @return a GHash_Table of key/values pairs

   function Get_Pixels
      (Self : not null access Gdk_Pixbuf_Record) return System.Address;
   --  Queries a pointer to the pixel data of a pixbuf.
   --  This function will cause an implicit copy of the pixbuf data if the
   --  pixbuf was created from read-only data.
   --  Please see the section on [image data](class.Pixbuf.htmlimage-data) for
   --  information about how the pixel data is stored in memory.
   --  Returns a borrowed pixel buffer. Its size is Get_Byte_Length; use
   --  Get_Rowstride to locate each row. Keep the pixbuf alive while accessing
   --  the buffer.
   --  @return A pointer to the pixbuf's pixel data.

   function Get_Pixels_With_Length
      (Self   : not null access Gdk_Pixbuf_Record;
       Length : out Guint) return System.Address;
   --  Queries a pointer to the pixel data of a pixbuf.
   --  This function will cause an implicit copy of the pixbuf data if the
   --  pixbuf was created from read-only data.
   --  Please see the section on [image data](class.Pixbuf.htmlimage-data) for
   --  information about how the pixel data is stored in memory.
   --  Since: gtk+ 2.26
   --  @param Length The length of the binary data.
   --  @return A pointer to the pixbuf's pixel data.

   function Get_Rowstride
      (Self : not null access Gdk_Pixbuf_Record) return Glib.Gint;
   --  Queries the rowstride of a pixbuf, which is the number of bytes between
   --  the start of a row and the start of the next row.
   --  @return Distance between row starts.

   function Get_Width
      (Self : not null access Gdk_Pixbuf_Record) return Glib.Gint;
   --  Queries the width of a pixbuf.
   --  @return Width in pixels.

   function New_Subpixbuf
      (Self   : not null access Gdk_Pixbuf_Record;
       Src_X  : Glib.Gint;
       Src_Y  : Glib.Gint;
       Width  : Glib.Gint;
       Height : Glib.Gint) return Gdk_Pixbuf;
   --  Creates a new pixbuf which represents a sub-region of `src_pixbuf`.
   --  The new pixbuf shares its pixels with the original pixbuf, so writing
   --  to one affects both. The new pixbuf holds a reference to `src_pixbuf`,
   --  so `src_pixbuf` will not be finalized until the new pixbuf is finalized.
   --  Note that if `src_pixbuf` is read-only, this function will force it to
   --  be mutable.
   --  @param Src_X X coord in Src_Pixbuf
   --  @param Src_Y Y coord in Src_Pixbuf
   --  @param Width width of region in Src_Pixbuf
   --  @param Height height of region in Src_Pixbuf
   --  @return a new pixbuf. Has transfer-ownership='full'.

   function Read_Pixel_Bytes
      (Self : not null access Gdk_Pixbuf_Record) return Glib.Bytes.Gbytes;
   --  Provides a Glib.Bytes.Gbytes buffer containing the raw pixel data; the
   --  data must not be modified.
   --  This function allows skipping the implicit copy that must be made if
   --  Gdk.Pixbuf.Get_Pixels is called on a read-only pixbuf.
   --  Since: gtk+ 2.32
   --  @return A new reference to a read-only copy of the pixel data. Note
   --  that for mutable pixbufs, this function will incur a one-time copy of
   --  the pixel data for conversion into the returned Glib.Bytes.Gbytes. Has
   --  transfer-ownership='full'.

   function Read_Pixels
      (Self : not null access Gdk_Pixbuf_Record) return System.Address;
   --  Provides a read-only pointer to the raw pixel data.
   --  This function allows skipping the implicit copy that must be made if
   --  Gdk.Pixbuf.Get_Pixels is called on a read-only pixbuf.
   --  Since: gtk+ 2.32
   --  @return a read-only pointer to the raw pixel data

   function Remove_Option
      (Self : not null access Gdk_Pixbuf_Record;
       Key  : UTF8_String) return Boolean;
   --  Removes the key/value pair option attached to a `GdkPixbuf`.
   --  Since: gtk+ 2.36
   --  @param Key a nul-terminated string representing the key to remove.
   --  @return `TRUE` if an option was removed, `FALSE` if not.

   function Rotate_Simple
      (Self  : not null access Gdk_Pixbuf_Record;
       Angle : Gdk_Pixbuf_Rotation) return Gdk_Pixbuf;
   --  Rotates a pixbuf by a multiple of 90 degrees, and returns the result in
   --  a new pixbuf.
   --  If `angle` is 0, this function will return a copy of `src`.
   --  Since: gtk+ 2.6
   --  @param Angle the angle to rotate by
   --  @return the new pixbuf. Has transfer-ownership='full'.

   procedure Saturate_And_Pixelate
      (Self       : not null access Gdk_Pixbuf_Record;
       Dest       : not null access Gdk_Pixbuf_Record'Class;
       Saturation : Gfloat;
       Pixelate   : Boolean);
   --  Modifies saturation and optionally pixelates `src`, placing the result
   --  in `dest`.
   --  The `src` and `dest` pixbufs must have the same image format, size, and
   --  rowstride.
   --  The `src` and `dest` arguments may be the same pixbuf with no ill
   --  effects.
   --  If `saturation` is 1.0 then saturation is not changed. If it's less
   --  than 1.0, saturation is reduced (the image turns toward grayscale); if
   --  greater than 1.0, saturation is increased (the image gets more vivid
   --  colors).
   --  If `pixelate` is `TRUE`, then pixels are faded in a checkerboard
   --  pattern to create a pixelated image.
   --  @param Dest place to write modified version of Src
   --  @param Saturation saturation factor
   --  @param Pixelate whether to pixelate

   function Save_To_Bufferv
      (Self          : not null access Gdk_Pixbuf_Record;
       Buffer        : out System.Address;
       Buffer_Size   : out Gsize;
       The_Type      : UTF8_String;
       Option_Keys   : GNAT.Strings.String_List;
       Option_Values : GNAT.Strings.String_List;
       Error         : out Glib.Error.GError) return Boolean;
   --  Vector version of `gdk_pixbuf_save_to_buffer`.
   --  Saves pixbuf to a new buffer in format Type, which is currently "jpeg",
   --  "tiff", "png", "ico" or "bmp".
   --  See [methodGdkpixbuf.Pixbuf.save_to_buffer] for more details.
   --  On success, Buffer is owned by the caller; release it with Glib.G_Free
   --  after consuming Buffer_Size bytes.
   --  Since: gtk+ 2.4
   --  @param Buffer location to receive a pointer to the new buffer.
   --  @param Buffer_Size location to receive the size of the new buffer.
   --  @param The_Type name of file format.
   --  @param Option_Keys name of options to set
   --  @param Option_Values values for named options
   --  @param Error the return location for a recoverable error
   --  @return whether an error was set

   function Save_To_Streamv
      (Self          : not null access Gdk_Pixbuf_Record;
       Stream        : not null access Glib.Output_Stream.Goutput_Stream_Record'Class;
       The_Type      : UTF8_String;
       Option_Keys   : GNAT.Strings.String_List;
       Option_Values : GNAT.Strings.String_List;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class;
       Error         : out Glib.Error.GError) return Boolean;
   --  Saves `pixbuf` to an output stream.
   --  Supported file formats are currently "jpeg", "tiff", "png", "ico" or
   --  "bmp".
   --  See [methodGdkpixbuf.Pixbuf.save_to_stream] for more details.
   --  Since: gtk+ 2.36
   --  @param Stream a `GOutputStream` to save the pixbuf to
   --  @param The_Type name of file format
   --  @param Option_Keys name of options to set
   --  @param Option_Values values for named options
   --  @param Cancellable optional `GCancellable` object, `NULL` to ignore
   --  @param Error the return location for a recoverable error
   --  @return `TRUE` if the pixbuf was saved successfully, `FALSE` if an
   --  error was set.

   procedure Save_To_Streamv_Async
      (Self          : not null access Gdk_Pixbuf_Record;
       Stream        : not null access Glib.Output_Stream.Goutput_Stream_Record'Class;
       The_Type      : UTF8_String;
       Option_Keys   : GNAT.Strings.String_List;
       Option_Values : GNAT.Strings.String_List;
       Cancellable   : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback      : Gasync_Ready_Callback);
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

   function Savev
      (Self          : not null access Gdk_Pixbuf_Record;
       Filename      : UTF8_String;
       The_Type      : UTF8_String;
       Option_Keys   : GNAT.Strings.String_List;
       Option_Values : GNAT.Strings.String_List;
       Error         : out Glib.Error.GError) return Boolean;
   --  Vector version of `gdk_pixbuf_save`.
   --  Saves pixbuf to a file in `type`, which is currently "jpeg", "png",
   --  "tiff", "ico" or "bmp".
   --  If Error is set, `FALSE` will be returned.
   --  See [methodGdkpixbuf.Pixbuf.save] for more details.
   --  @param Filename name of file to save.
   --  @param The_Type name of file format.
   --  @param Option_Keys name of options to set
   --  @param Option_Values values for named options
   --  @param Error the return location for a recoverable error
   --  @return whether an error was set

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
       Interp_Type : Gdk_Interp_Type);
   --  Creates a transformation of the source image Src by scaling by Scale_X
   --  and Scale_Y then translating by Offset_X and Offset_Y, then renders the
   --  rectangle (Dest_X, Dest_Y, Dest_Width, Dest_Height) of the resulting
   --  image onto the destination image replacing the previous contents.
   --  Try to use Gdk.Pixbuf.Scale_Simple first; this function is the
   --  industrial-strength power tool you can fall back to, if
   --  Gdk.Pixbuf.Scale_Simple isn't powerful enough.
   --  If the source rectangle overlaps the destination rectangle on the same
   --  pixbuf, it will be overwritten during the scaling which results in
   --  rendering artifacts.
   --  @param Dest the Gdk.Pixbuf.Gdk_Pixbuf into which to render the results
   --  @param Dest_X the left coordinate for region to render
   --  @param Dest_Y the top coordinate for region to render
   --  @param Dest_Width the width of the region to render
   --  @param Dest_Height the height of the region to render
   --  @param Offset_X the offset in the X direction (currently rounded to an
   --  integer)
   --  @param Offset_Y the offset in the Y direction (currently rounded to an
   --  integer)
   --  @param Scale_X the scale factor in the X direction
   --  @param Scale_Y the scale factor in the Y direction
   --  @param Interp_Type the interpolation type for the transformation.

   function Scale_Simple
      (Self        : not null access Gdk_Pixbuf_Record;
       Dest_Width  : Glib.Gint;
       Dest_Height : Glib.Gint;
       Interp_Type : Gdk_Interp_Type) return Gdk_Pixbuf;
   --  Create a new pixbuf containing a copy of `src` scaled to `dest_width` x
   --  `dest_height`.
   --  This function leaves `src` unaffected.
   --  The `interp_type` should be `GDK_INTERP_NEAREST` if you want maximum
   --  speed (but when scaling down `GDK_INTERP_NEAREST` is usually unusably
   --  ugly). The default `interp_type` should be `GDK_INTERP_BILINEAR` which
   --  offers reasonable quality and speed.
   --  You can scale a sub-portion of `src` by creating a sub-pixbuf pointing
   --  into `src`; see [methodGdkpixbuf.Pixbuf.new_subpixbuf].
   --  If `dest_width` and `dest_height` are equal to the width and height of
   --  `src`, this function will return an unscaled copy of `src`.
   --  For more complicated scaling/alpha blending see
   --  [methodGdkpixbuf.Pixbuf.scale] and [methodGdkpixbuf.Pixbuf.composite].
   --  @param Dest_Width the width of destination image
   --  @param Dest_Height the height of destination image
   --  @param Interp_Type the interpolation type for the transformation.
   --  @return the new pixbuf. Has transfer-ownership='full'.

   procedure Get_File_Info_Async
      (Filename    : UTF8_String;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   procedure New_From_Stream_Async
      (Stream      : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
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

   procedure New_From_Stream_At_Scale_Async
      (Stream                : not null access Glib.Input_Stream.Ginput_Stream_Record'Class;
       Width                 : Glib.Gint;
       Height                : Glib.Gint;
       Preserve_Aspect_Ratio : Boolean;
       Cancellable           : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback              : Gasync_Ready_Callback);
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

   procedure Load_Async
      (Self        : not null access Gdk_Pixbuf_Record;
       Size        : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Loads an icon asynchronously. To finish this function, see
   --  Glib.Loadable_Icon.Load_Finish. For the synchronous, blocking version of
   --  this function, see Glib.Loadable_Icon.Load.
   --  @param Size an integer.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied

   ----------------------
   -- GtkAda additions --
   ----------------------

   Colorspace_Property : constant Property_Gdk_Colorspace :=
   Build ("colorspace");

   Pixels_Property : constant Glib.Properties.Property_Address :=
   Glib.Properties.Build ("pixels");

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   function Load
      (Self        : not null access Gdk_Pixbuf_Record;
       Size        : Glib.Gint;
       The_Type    : access UTF8_String := null;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Error       : out Glib.Error.GError)
       return Glib.Input_Stream.Ginput_Stream;

   function Load_Finish
      (Self     : not null access Gdk_Pixbuf_Record;
       Res      : Glib.G_Async_Result;
       The_Type : access UTF8_String := null;
       Error    : out Glib.Error.GError)
       return Glib.Input_Stream.Ginput_Stream;

   ---------------
   -- Functions --
   ---------------

   function Calculate_Rowstride
      (Colorspace      : Gdk_Colorspace;
       Has_Alpha       : Boolean;
       Bits_Per_Sample : Glib.Gint;
       Width           : Glib.Gint;
       Height          : Glib.Gint) return Glib.Gint;
   --  Calculates the rowstride that an image created with those values would
   --  have.
   --  This function is useful for front-ends and backends that want to check
   --  image values without needing to create a `GdkPixbuf`.
   --  Since: gtk+ 2.36.8
   --  @param Colorspace Color space for image
   --  @param Has_Alpha Whether the image should have transparency information
   --  @param Bits_Per_Sample Number of bits per color sample
   --  @param Width Width of image in pixels, must be > 0
   --  @param Height Height of image in pixels, must be > 0
   --  @return the rowstride for the given values, or -1 in case of error.

   function Get_File_Info
      (Filename : UTF8_String;
       Width    : access Glib.Gint := null;
       Height   : access Glib.Gint := null)
       return Gdk.Pixbuf_Format.Gdk_Pixbuf_Format_Access;
   --  Parses an image file far enough to determine its format and size.
   --  Since: gtk+ 2.4
   --  @param Filename The name of the file to identify.
   --  @param Width Return location for the width of the image
   --  @param Height Return location for the height of the image
   --  @return A `GdkPixbufFormat` describing the image format of the file

   function Get_File_Info_Finish
      (Async_Result : Glib.G_Async_Result;
       Width        : out Glib.Gint;
       Height       : out Glib.Gint;
       Error        : out Glib.Error.GError)
       return Gdk.Pixbuf_Format.Gdk_Pixbuf_Format_Access;
   --  Finishes an asynchronous pixbuf parsing operation started with
   --  Gdk.Pixbuf.Get_File_Info_Async.
   --  Since: gtk+ 2.32
   --  @param Async_Result a `GAsyncResult`
   --  @param Width Return location for the width of the image, or `NULL`
   --  @param Height Return location for the height of the image, or `NULL`
   --  @param Error the return location for a recoverable error
   --  @return A `GdkPixbufFormat` describing the image format of the file

   function Get_Formats return Gdk.Pixbuf_Format.Format_List.GSlist;
   --  Obtains the available information about the image formats supported by
   --  GdkPixbuf.
   --  Since: gtk+ 2.2
   --  @return A list of support image formats.

   function Init_Modules
      (Path  : UTF8_String;
       Error : out Glib.Error.GError) return Boolean;
   --  Initalizes the gdk-pixbuf loader modules referenced by the
   --  `loaders.cache` file present inside that directory.
   --  This is to be used by applications that want to ship certain loaders in
   --  a different location from the system ones.
   --  This is needed when the OS or runtime ships a minimal number of loaders
   --  so as to reduce the potential attack surface of carefully crafted image
   --  files, especially for uncommon file types. Applications that require
   --  broader image file types coverage, such as image viewers, would be
   --  expected to ship the gdk-pixbuf modules in a separate location, bundled
   --  with the application in a separate directory from the OS or runtime-
   --  provided modules.
   --  Since: gtk+ 2.40
   --  @param Path Path to directory where the `loaders.cache` is installed
   --  @param Error the return location for a recoverable error

   function Save_To_Stream_Finish
      (Async_Result : Glib.G_Async_Result;
       Error        : out Glib.Error.GError) return Boolean;
   --  Finishes an asynchronous pixbuf save operation started with
   --  gdk_pixbuf_save_to_stream_async.
   --  Since: gtk+ 2.24
   --  @param Async_Result a `GAsyncResult`
   --  @param Error the return location for a recoverable error
   --  @return `TRUE` if the pixbuf was saved successfully, `FALSE` if an
   --  error was set.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Bits_Per_Sample_Property : constant Glib.Properties.Property_Int;
   --  The number of bits per sample.
   --
   --  Currently only 8 bit per sample are supported.

   Has_Alpha_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the pixbuf has an alpha channel.

   Height_Property : constant Glib.Properties.Property_Int;
   --  The number of rows of the pixbuf.

   N_Channels_Property : constant Glib.Properties.Property_Int;
   --  The number of samples per pixel.
   --
   --  Currently, only 3 or 4 samples per pixel are supported.

   Pixel_Bytes_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GLib.Bytes

   Rowstride_Property : constant Glib.Properties.Property_Int;
   --  The number of bytes between the start of a row and the start of the
   --  next row.
   --
   --  This number must (obviously) be at least as large as the width of the
   --  pixbuf.

   Width_Property : constant Glib.Properties.Property_Int;
   --  The number of columns of the pixbuf.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.LoadableIcon"

   package Implements_Gloadable_Icon is new Glib.Types.Implements
     (Glib.Loadable_Icon.Gloadable_Icon, Gdk_Pixbuf_Record, Gdk_Pixbuf);
   function "+"
     (Widget : access Gdk_Pixbuf_Record'Class)
   return Glib.Loadable_Icon.Gloadable_Icon
   renames Implements_Gloadable_Icon.To_Interface;
   function "-"
     (Interf : Glib.Loadable_Icon.Gloadable_Icon)
   return Gdk_Pixbuf
   renames Implements_Gloadable_Icon.To_Object;

private
   Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width");
   Rowstride_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("rowstride");
   Pixel_Bytes_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("pixel-bytes");
   N_Channels_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("n-channels");
   Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("height");
   Has_Alpha_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-alpha");
   Bits_Per_Sample_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("bits-per-sample");
end Gdk.Pixbuf;
