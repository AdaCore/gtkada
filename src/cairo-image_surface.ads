-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

pragma Ada_2005;

package Cairo.Image_Surface is

   --  Image-surface functions

   --   Cairo_Format:
   --   CAIRO_FORMAT_ARGB32: each pixel is a 32-bit quantity, with
   --     alpha in the upper 8 bits, then red, then green, then blue.
   --     The 32-bit quantities are stored native-endian. Pre-multiplied
   --     alpha is used. (That is, 50 transparent red is 0x80800000,
   --     not 0x80ff0000.)
   --   CAIRO_FORMAT_RGB24: each pixel is a 32-bit quantity, with
   --     the upper 8 bits unused. Red, Green, and Blue are stored
   --     in the remaining 24 bits in that order.
   --   CAIRO_FORMAT_A8: each pixel is a 8-bit quantity holding
   --     an alpha value.
   --   CAIRO_FORMAT_A1: each pixel is a 1-bit quantity holding
   --     an alpha value. Pixels are packed together into 32-bit
   --     quantities. The ordering of the bits matches the
   --     endianess of the platform. On a big-endian machine, the
   --     first pixel is in the uppermost bit, on a little-endian
   --     machine the first pixel is in the least-significant bit.
   --   CAIRO_FORMAT_RGB16_565: This format value is deprecated. It has
   --     never been properly implemented in cairo and should not be used
   --     by applications. (since 1.2)
   --
   --   Cairo_format is used to identify the memory format of
   --   image data.
   --
   --   New entries may be added in future versions.
   --

   --  The value of 4 is reserved by a deprecated enum value.
   --     * The next format added must have an explicit value of 5.
   --    CAIRO_FORMAT_RGB16_565 = 4,
   --

   type Cairo_Format is (
      Cairo_Format_Argb32,
      Cairo_Format_Rgb24,
      Cairo_Format_A8,
      Cairo_Format_A1);
   pragma Convention (C, Cairo_Format);

   function Create
     (Format : Cairo_Format;
      Width  : Gint;
      Height : Gint)
      return   Cairo_Surface;
   --  Format: Format of pixels in the surface to create
   --  Width: Width of the surface, in pixels
   --  Height: Height of the surface, in pixels
   --
   --  Creates an image surface of the specified format and
   --  dimensions. Initially the surface contents are all
   --  0. (Specifically, within each pixel, each color or alpha channel
   --  belonging to format will be 0. The contents of bits within a pixel,
   --  but not belonging to the given format are undefined).
   --
   --  Return value: a pointer to the newly created surface. The caller
   --  owns the surface and should call Cairo.Surface.Destroy when done
   --  with it.
   --
   --  This function always returns a valid pointer, but it will return a
   --  pointer to a "nil" surface if an error such as out of memory
   --  occurs. You can use Cairo.Surface.Status to check for this.

   function Cairo_Format_Stride_For_Width
     (Format : Cairo_Format;
      Width  : Gint)
      return   Gint;
   --  Format: A Cairo_Format value
   --  Width: The desired Width of an image surface to be created.
   --
   --  This function provides a stride value that will respect all
   --  alignment requirements of the accelerated image-rendering code
   --  within cairo. Typical usage will be of the form:
   --
   --  <informalexample><programlisting>
   --  int stride;
   --  unsigned char *data;
   --  Cairo_Surface *surface;
   --
   --  stride = Cairo_Format_Stride_For_Width (format, width);
   --  data = malloc (stride * height);
   --  surface = Cairo.Image_Surface.Create_For_Data (data, format,
   --               width, height, stride);
   --  </programlisting></informalexample>
   --
   --  Return value: the appropriate stride to use given the desired
   --  format and width, or -1 if either the format is invalid or the width
   --  too large.
   --
   --  Since: 1.6

   function Create_For_Data
     (Data   : access Guchar;
      Format : Cairo_Format;
      Width  : Gint;
      Height : Gint;
      Stride : Gint)
      return   Cairo_Surface;
   --  Data: a pointer to a buffer supplied by the application in which
   --      to write contents. This pointer must be suitably aligned for any
   --      kind of variable, (for example, a pointer returned by malloc).
   --  Format: the Format of pixels in the buffer
   --  Width: the Width of the image to be stored in the buffer
   --  Height: the Height of the image to be stored in the buffer
   --  Stride: the number of bytes between the start of rows in the
   --      buffer as allocated. This value should always be computed by
   --      Cairo_Format_Stride_For_Width before allocating the data
   --      buffer.
   --
   --  Creates an image surface for the provided pixel data. The output
   --  buffer must be kept around until the Cairo_Surface is destroyed
   --  or Cairo.Surface.Finish is called on the surface.  The initial
   --  contents of data will be used as the initial image contents; you
   --  must explicitly clear the buffer, using, for example,
   --  Cairo_Rectangle and Cairo_Fill if you want it cleared.
   --
   --  Note that the stride may be larger than
   --  width*bytes_per_pixel to provide proper alignment for each pixel
   --  and row. This alignment is required to allow high-performance rendering
   --  within cairo. The correct way to obtain a legal stride value is to
   --  call Cairo_Format_Stride_For_Width with the desired format and
   --  maximum image width value, and the use the resulting stride value
   --  to allocate the data and to create the image surface. See
   --  Cairo_Format_Stride_For_Width for example code.
   --
   --  Return value: a pointer to the newly created surface. The caller
   --  owns the surface and should call Cairo.Surface.Destroy when done
   --  with it.
   --
   --  This function always returns a valid pointer, but it will return a
   --  pointer to a "nil" surface in the case of an error such as out of
   --  memory or an invalid stride value. In case of invalid stride value
   --  the error status of the returned surface will be
   --  CAIRO_STATUS_INVALID_STRIDE.  You can use
   --  Cairo.Surface.Status to check for this.
   --
   --  See Cairo.Surface.Set_User_Data for a means of attaching a
   --  destroy-notification fallback to the surface if necessary.

   function Get_Data (Surface : Cairo_Surface) return access Guchar;
   --  Surface: a Cairo_Image_Surface
   --
   --  Get a pointer to the data of the image surface, for direct
   --  inspection or modification.
   --
   --  Return value: a pointer to the image data of this surface or NULL
   --  if surface is not an image surface, or if Cairo.Surface.Finish
   --  has been called.
   --
   --  Since: 1.2

   function Get_Format (Surface : Cairo_Surface) return Cairo_Format;
   --  Surface: a Cairo_Image_Surface
   --
   --  Get the format of the surface.
   --
   --  Return value: the format of the surface
   --
   --  Since: 1.2

   function Get_Width (Surface : Cairo_Surface) return Gint;
   --  Surface: a Cairo_Image_Surface
   --
   --  Get the width of the image surface in pixels.
   --
   --  Return value: the width of the surface in pixels.

   function Get_Height (Surface : Cairo_Surface) return Gint;
   --  Surface: a Cairo_Image_Surface
   --
   --  Get the height of the image surface in pixels.
   --
   --  Return value: the height of the surface in pixels.

   function Get_Stride (Surface : Cairo_Surface) return Gint;
   --  Surface: a Cairo_Image_Surface
   --
   --  Get the stride of the image surface in bytes
   --
   --  Return value: the stride of the image surface in bytes (or 0 if
   --  surface is not an image surface). The stride is the distance in
   --  bytes from the beginning of one row of the image data to the
   --  beginning of the next row.
   --
   --  Since: 1.2

private

   pragma Import (C, Create, "cairo_image_surface_create");
   pragma Import
     (C,
      Cairo_Format_Stride_For_Width,
      "cairo_format_stride_for_width");
   pragma Import (C, Create_For_Data, "cairo_image_surface_create_for_data");
   pragma Import (C, Get_Data, "cairo_image_surface_get_data");
   pragma Import (C, Get_Format, "cairo_image_surface_get_format");
   pragma Import (C, Get_Width, "cairo_image_surface_get_width");
   pragma Import (C, Get_Height, "cairo_image_surface_get_height");
   pragma Import (C, Get_Stride, "cairo_image_surface_get_stride");

end Cairo.Image_Surface;
