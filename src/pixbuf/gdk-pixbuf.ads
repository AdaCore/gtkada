-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

--  <description>
--  This object provides image manipulation routines.
--
--  The following image formats are known, but some depend on external
--  libraries for the proper loading of files (indicated with * in the list):
--     PNG*, JPEG*, TIFF*, GIF, XPM, PNM, Sun raster file (ras), ico,
--     bmp.
--
--  With this package, you can load images from file, display them on the
--  screen, rescale them and composite them with other images.
--  All the functions fully support alpha channels (opacity).
--
--  Different filters are provided, depending on the quality of output you
--  expect and the speed you need.
--  </description>
--  <c_version>gdk-pixbuf 0.6.0</c_version>

with Glib;         use Glib;
with Gdk.Art.Pixbuf;
with Gdk.Bitmap;
with Gdk.Drawable;
with Gdk.GC;
with Gdk.Rgb;
with Gdk.Color;
with System;
with Interfaces.C.Strings;

package Gdk.Pixbuf is

   type Gdk_Pixbuf is private;
   Null_Pixbuf : constant Gdk_Pixbuf;

   type Alpha_Mode is (Alpha_Bilevel,
                       Alpha_Full);
   --  Alpha compositing mode

   --------------------------
   -- Accessing the fields --
   --------------------------

   function Get_Format (Pixbuf : in Gdk_Pixbuf)
                       return Gdk.Art.Pixbuf.Art_Pix_Format;
   --  Return the format of the image.
   --  This can currently be only RGB, but extensions will be implemented
   --  for gray, cmyk, lab,...

   function Get_N_Channels (Pixbuf : in Gdk_Pixbuf) return Gint;
   --  Number of channels in the image.

   function Get_Has_Alpha (Pixbuf : in Gdk_Pixbuf) return Boolean;
   --  Return True if the image has an alpha channel (opacity information).

   function Get_Bits_Per_Sample (Pixbuf : in Gdk_Pixbuf) return Gint;
   --  Number of bits per color sample.

   function Get_Pixels (Pixbuf : in Gdk_Pixbuf)
                       return Gdk.Rgb.Rgb_Buffer_Access;
   --  Return a pointer to the pixel data of the image.

   function Get_Width (Pixbuf : in Gdk_Pixbuf) return Gint;
   --  Return the width of the image in pixels.

   function Get_Height (Pixbuf : in Gdk_Pixbuf) return Gint;
   --  Return the height of the image in pixels.

   function Get_Rowstride (Pixbuf : in Gdk_Pixbuf) return Gint;
   --  Return the number of bytes between rows in the image data.

   ------------------------
   -- Reference counting --
   ------------------------

   procedure Ref (Pixbuf : in Gdk_Pixbuf);
   --  Increment the reference counting on the image.
   --  The image is destroyed when its reference counting reaches 0.
   --  Note also that most of the time you won't have to call this
   --  function yourself.

   procedure Unref (Pixbuf : in Gdk_Pixbuf);
   --  Decrement the reference counting on the image.

   ----------------------
   -- Libart interface --
   ----------------------

   function Get_Art_Pixbuf (Pixbuf : in Gdk_Pixbuf)
                           return Gdk.Art.Pixbuf.Art_Pixbuf;
   --  Return the underlying structure from libart.

   function New_From_Art_Pixbuf (Pixbuf : in Gdk.Art.Pixbuf.Art_Pixbuf)
                                return Gdk_Pixbuf;
   --  Wrap an art_pixbuf.
   --  The reference counting is initialized to 1.

   function Gdk_New (Width           : in Gint;
                     Height          : in Gint;
                     Format          : in Gdk.Art.Pixbuf.Art_Pix_Format :=
                       Gdk.Art.Pixbuf.Art_Pix_RGB;
                     Has_Alpha       : in Boolean := False;
                     Bits_Per_Sample : in Gint := 8)
                    return Gdk_Pixbuf;
   --  Create a blank pixbuf with an optimal rowstride and a new buffer.
   --  The buffer is allocated, but not cleared.
   --  The reference counting is initialized to 1.

   --------------
   -- Creating --
   --------------

   function New_From_File (Filename : in String) return Gdk_Pixbuf;
   --  Load an image from file.

   function New_From_Xpm_Data (Data : in Interfaces.C.Strings.chars_ptr_array)
                              return Gdk_Pixbuf;
   --  Create an image from a XPM data.

   function Add_Alpha (Pixbuf           : in Gdk_Pixbuf;
                       Substitute_Color : in Boolean;
                       Red              : in Guchar := 0;
                       Green            : in Guchar := 0;
                       Blue             : in Guchar := 0)
                      return Gdk_Pixbuf;
   --  Return a newly allocated image copied from Pixbuf, but with an
   --  extra alpha channel.
   --  If Pixbuf already had an alpha channel, the two images have exactly
   --  the same contents.
   --  If Substitute_Color is True, the color (Red, Green, Blue) is
   --  substituted for zero opacity.
   --  If Substitute_Color is False, Red, Green and Blue are ignored, and a
   --  new color is created with zero opacity.

   ---------------
   -- Rendering --
   ---------------

   procedure Render_Threshold_Alpha (Pixbuf : in Gdk_Pixbuf;
                                     Bitmap : in Gdk.Bitmap.Gdk_Bitmap;
                                     Src_X  : in Gint;
                                     Src_Y  : in Gint;
                                     Dest_X : in Gint;
                                     Dest_Y : in Gint;
                                     Width  : in Gint;
                                     Height : in Gint;
                                     Alpha_Threshold : in Gint);
   --  Take the opacity values in a rectangular portion of a pixbuf and
   --  thresholds them to produce a bi-level alpha mask that can be used as
   --  a clipping mask for a drawable.
   --  Bitmap is the bitmap where the bilevel mask will be painted to.
   --  Alpha_Threshold are the opacity values below which a pixel will be
   --  painted as zero. All other values will be painted as one.

   procedure Render_To_Drawable
     (Pixbuf   : in Gdk_Pixbuf;
      Drawable : in Gdk.Drawable.Gdk_Drawable;
      Gc       : in Gdk.GC.Gdk_GC;
      Src_X    : in Gint;
      Src_Y    : in Gint;
      Dest_X   : in Gint;
      Dest_Y   : in Gint;
      Width    : in Gint;
      Height   : in Gint;
      Dither   : in Gdk.Rgb.Gdk_Rgb_Dither  := Gdk.Rgb.Dither_Normal;
      X_Dither : in Gint := 0;
      Y_Dither : in Gint := 0);
   --  Render a rectangular portion of a pixbuf to a drawable while using the
   --  specified GC. This is done using Gdk.RGB, so the specified drawable
   --  must have the Gdk.RGB visual and colormap.  Note that this function
   --  will ignore the opacity information for images with an alpha channel;
   --  the GC must already have the clipping mask set if you want transparent
   --  regions to show through.
   --
   --  For an explanation of dither offsets, see the Gdk.RGB documentation.  In
   --  brief, the dither offset is important when re-rendering partial regions
   --  of an image to a rendered version of the full image, or for when the
   --  offsets to a base position change, as in scrolling.  The dither matrix
   --  has to be shifted for consistent visual results.  If you do not have
   --  any of these cases, the dither offsets can be both zero.

   procedure Render_To_Drawable_Alpha
     (Pixbuf          : in Gdk_Pixbuf;
      Drawable        : in Gdk.Drawable.Gdk_Drawable;
      Src_X           : in Gint;
      Src_Y           : in Gint;
      Dest_X          : in Gint;
      Dest_Y          : in Gint;
      Width           : in Gint;
      Height          : in Gint;
      Alpha_Threshold : in Gint;
      Dither          : in Gdk.Rgb.Gdk_Rgb_Dither := Gdk.Rgb.Dither_Normal;
      X_Dither        : in Gint := 0;
      Y_Dither        : in Gint := 0);
   --  Render a rectangular portion of a pixbuf to a drawable.
   --  This is done using Gdk.RGB, so the specified drawable must have the
   --  GdkRGB visual and colormap. When used with Alpha_Bilevel, this function
   --  has to create a bitmap out of the thresholded alpha channel of the
   --  image and, it has to set this bitmap as the clipping mask for the GC
   --  used for drawing.  This can be a significant performance penalty
   --  depending on the size and the complexity of the alpha channel of the
   --  image.  If performance is crucial, consider handling the alpha channel
   --  yourself (possibly by caching it in your application) and using
   --  Render_To_Drawable or Gdk.RGB directly instead.
   --
   --  If the image does have opacity information and Alpha_Mode
   --  is Alpha_Bilevel, specifies the threshold value for opacity values

   function Get_From_Drawable
     (Dest   : in Gdk_Pixbuf;
      Src    : in Gdk.Drawable.Gdk_Drawable;
      Cmap   : in Gdk.Color.Gdk_Colormap;
      Src_X  : in Gint;
      Src_Y  : in Gint;
      Dest_X : in Gint;
      Dest_Y : in Gint;
      Width  : in Gint;
      Height : in Gint)
     return Gdk_Pixbuf;
   --  Transfer image data from a Gdk drawable and converts it to an RGB(A)
   --  representation inside a Gdk_Pixbuf.
   --
   --  If the drawable src is a pixmap, then a suitable colormap must be
   --  specified, since pixmaps are just blocks of pixel data without an
   --  associated colormap.
   --  If the drawable is a window, the Cmap argument will be ignored and the
   --  window's own colormap will be used instead.
   --
   --  If the specified destination pixbuf Dest is Null_Pixbuf, then this
   --  function will create an RGB pixbuf with 8 bits per channel and no
   --  alpha, with the same size specified by the Width and Height
   --  arguments. In this case, the Dest_x and Dest_y arguments must be
   --  specified as 0, otherwise the function will return Null_Pixbuf.  If the
   --  specified destination pixbuf is not Null_Pixbuf and it contains alpha
   --  information, then the filled pixels will be set to full opacity.
   --
   --  If the specified drawable is a pixmap, then the requested source
   --  rectangle must be completely contained within the pixmap, otherwise the
   --  function will Null_Pixbuf
   --
   --  If the specified drawable is a window, then it must be viewable, i.e.
   --  all of its ancestors up to the root window must be mapped.  Also, the
   --  specified source rectangle must be completely contained within the
   --  window and within the screen.  If regions of the window are obscured by
   --  noninferior windows, the contents of those regions are undefined.
   --  The contents of regions obscured by inferior windows of a different
   --  depth than that of the source window will also be undefined.
   --
   --  Return value: The same pixbuf as Dest if it was non-NULL, or a
   --  newly-created pixbuf with a reference count of 1 if no destination
   --  pixbuf was specified.

   procedure Copy_Area (Src_Pixbuf  : in Gdk_Pixbuf;
                        Src_X       : in Gint;
                        Src_Y       : in Gint;
                        Width       : in Gint;
                        Height      : in Gint;
                        Dest_Pixbuf : in Gdk_Pixbuf;
                        Dest_X      : in Gint;
                        Dest_Y      : in Gint);
   --  Copy a rectangular area from Src_pixbuf to Dest_pixbuf.
   --  Conversion of pixbuf formats is done automatically.

   -------------
   -- Scaling --
   -------------

   procedure Scale (Src          : in Gdk_Pixbuf;
                    Dest         : in Gdk_Pixbuf;
                    Dest_X       : in Gint;
                    Dest_Y       : in Gint;
                    Dest_Width   : in Gint;
                    Dest_Height  : in Gint;
                    Offset_X     : in Gdouble := 0.0;
                    Offset_Y     : in Gdouble := 0.0;
                    Scale_X      : in Gdouble := 1.0;
                    Scale_Y      : in Gdouble := 1.0;
                    Filter_Level : in Gdk.Art.Pixbuf.Art_Filter_Level :=
                      Gdk.Art.Pixbuf.Filter_Bilinear);
   --  Transform the source image by scaling by Scale_x and Scale_y then
   --  translating by Offset_x and Offset_y.
   --  The image is then rendered in the rectangle (Dest_x, Dest_y,
   --  Dest_width, Dest_height) of the resulting image onto the destination
   --  drawable replacing the previous contents.

   procedure Composite (Src           : in Gdk_Pixbuf;
                        Dest          : in Gdk_Pixbuf;
                        Dest_X        : in Gint;
                        Dest_Y        : in Gint;
                        Dest_Width    : in Gint;
                        Dest_Height   : in Gint;
                        Offset_X      : in Gdouble := 0.0;
                        Offset_Y      : in Gdouble := 0.0;
                        Scale_X       : in Gdouble := 1.0;
                        Scale_Y       : in Gdouble := 1.0;
                        Filter_Level  : in Gdk.Art.Pixbuf.Art_Filter_Level :=
                          Gdk.Art.Pixbuf.Filter_Bilinear;
                        Overall_Alpha : in Gint := 128);
   --  Transform the source image by scaling by Scale_x and Scale_y then
   --  translating by Offset_x and Offset_y, then composite the rectangle
   --  (Dest_X, Dest_Y, Dest_Width, Dest_Height) of the resulting image onto
   --  the destination drawable.  Alpha should be in 0 .. 255

   procedure Composite_Color
     (Src           : in Gdk_Pixbuf;
      Dest          : in Gdk_Pixbuf;
      Dest_X        : in Gint;
      Dest_Y        : in Gint;
      Dest_Width    : in Gint;
      Dest_Height   : in Gint;
      Offset_X      : in Gdouble := 0.0;
      Offset_Y      : in Gdouble := 0.0;
      Scale_X       : in Gdouble := 1.0;
      Scale_Y       : in Gdouble := 1.0;
      Filter_Level  : in Gdk.Art.Pixbuf.Art_Filter_Level :=
        Gdk.Art.Pixbuf.Filter_Bilinear;
      Overall_Alpha : in Gint := 128;
      Check_X       : in Gint := 0;
      Check_Y       : in Gint := 0;
      Check_Size    : in Gint := 0;
      Color1        : in Gdk.Art.Pixbuf.Art_U32 := 0;
      Color2        : in Gdk.Art.Pixbuf.Art_U32 := 0);
   --  Transform the source image by scaling by Scale_x and Scale_y then
   --  translating by Offset_x and Offset_y, then composites the rectangle
   --  (Dest_X, Dest_Y, Dest_Width, Dest_Height) of the resulting image with
   --  a checkboard of the colors Color1 and Color2 and renders it onto the
   --  destination drawable.
   --  The origin of checkboard is at (Check_x, Check_y)
   --  Color1 is the color at the upper left of the check.

   function Scale_Simple (Src           : in Gdk_Pixbuf;
                          Dest_Width    : in Gint;
                          Dest_Height   : in Gint;
                          Filter_Level  : in Gdk.Art.Pixbuf.Art_Filter_Level :=
                            Gdk.Art.Pixbuf.Filter_Bilinear)
                         return Gdk_Pixbuf;
   --  Scale the Src image to Dest_width x Dest_height and render the result
   --  into a new pixbuf.

   function Composite_Color_Simple
     (Src           : in Gdk_Pixbuf;
      Dest_Width    : in Gint;
      Dest_Height   : in Gint;
      Filter_Level  : in Gdk.Art.Pixbuf.Art_Filter_Level :=
        Gdk.Art.Pixbuf.Filter_Bilinear;
      Overall_Alpha : in Gint := 128;
      Color1        : in Gdk.Art.Pixbuf.Art_U32 := 0;
      Color2        : in Gdk.Art.Pixbuf.Art_U32 := 0)
     return Gdk_Pixbuf;
   --  Scale Src to Dest_width x Dest_height and composite the result with
   --  a checkboard of colors Color1 and Color2 and render the result into
   --  a new pixbuf.

private
   type Gdk_Pixbuf is new System.Address;
   Null_Pixbuf : constant Gdk_Pixbuf := Gdk_Pixbuf (System.Null_Address);

   pragma Import (C, Get_Art_Pixbuf, "ada_gdk_pixbuf_get_art_pixbuf");
   pragma Import (C, Get_Format, "gdk_pixbuf_get_format");
   pragma Import (C, Get_N_Channels, "gdk_pixbuf_get_n_channels");
   pragma Import (C, Get_Bits_Per_Sample, "gdk_pixbuf_get_bits_per_sample");
   pragma Import (C, Get_Pixels, "gdk_pixbuf_get_pixels");
   pragma Import (C, Get_Width, "gdk_pixbuf_get_width");
   pragma Import (C, Get_Height, "gdk_pixbuf_get_height");
   pragma Import (C, Get_Rowstride, "gdk_pixbuf_get_rowstride");
   pragma Import (C, Ref, "gdk_pixbuf_ref");
   pragma Import (C, Unref, "gdk_pixbuf_unref");
   pragma Import (C, New_From_Art_Pixbuf, "gdk_pixbuf_new_from_art_pixbuf");
   pragma Import (C, New_From_Xpm_Data, "gdk_pixbuf_new_from_xpm_data");
   pragma Import (C, Copy_Area, "gdk_pixbuf_copy_area");
   pragma Import (C, Scale, "gdk_pixbuf_scale");
   pragma Import (C, Composite, "gdk_pixbuf_composite");
   pragma Import (C, Composite_Color, "gdk_pixbuf_composite_color");
   pragma Import (C, Scale_Simple, "gdk_pixbuf_scale_simple");
   pragma Import (C, Composite_Color_Simple,
                    "gdk_pixbuf_composite_color_simple");
   pragma Import (C, Render_Threshold_Alpha,
                    "gdk_pixbuf_render_threshold_alpha");
   pragma Import (C, Get_From_Drawable, "gdk_pixbuf_get_from_drawable");
end Gdk.Pixbuf;

--  NOT BOUND: gdk_pixbuf_new_from_data
--  NOT BOUND: gdk_pixbuf_render_pixmap_and_mask
--  NOT BOUND: gdk_pixbuf_animation_new_from_file
--  NOT BOUND: gdk_pixbuf_animation_ref
--  NOT BOUND: gdk_pixbuf_animation_unref
--  NOT BOUND: gdk_pixbuf_preinit
--  NOT BOUND: gdk_pixbuf_postinit

