-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
--  screen, re-scale them and composite them with other images.
--  All the functions fully support alpha channels (opacity).
--
--  Different filters are provided, depending on the quality of output you
--  expect and the speed you need.
--  </description>
--  <c_version>gdk-pixbuf 0.8.0</c_version>

with Glib; use Glib;
with Glib.Glist;
pragma Elaborate_All (Glib.Glist);

with Gdk.Bitmap;
with Gdk.Drawable;
with Gdk.Color;
with Gdk.GC;
with Gdk.Pixmap;
with Gdk.Rgb;
with Interfaces.C.Strings;

package Gdk.Pixbuf is

   type Gdk_Pixbuf is new Glib.C_Proxy;
   --  A very efficient client-side pixmap.
   --  This type can be adapted to all the possible screen depths (number of
   --  bits per pixel), and the algorithms are extremely efficient.
   --  You can also load a pixbuf directly from an external file in one of
   --  the standard image formats.

   Null_Pixbuf : constant Gdk_Pixbuf := null;

   type Gdk_Pixbuf_Animation is new Glib.C_Proxy;
   --  Type used for animations.

   type Gdk_Pixbuf_Frame is new Glib.C_Proxy;
   --  An animation is composed of frames.

   type Frame_Action is (Frame_Retain, Frame_Dispose, Frame_Revert);
   --  GIF-like animation overlay modes for frames.

   type Alpha_Mode is (Alpha_Bilevel, Alpha_Full);
   --  Alpha compositing mode.
   --  This indicates how the alpha channel (for opacity) is handled when
   --  rendering.
   for Alpha_Mode'Size use Gint'Size;

   type Gdk_Colorspace is (Colorspace_RGB);
   --  Type of the image.
   --  The only possible value is currently RGB, but extensions will
   --  exist with CMYK, Gray, Lab, ...
   for Gdk_Colorspace'Size use Gint'Size;

   type Gdk_Interp_Type is
     (Interp_Nearest,
      --  Nearest neighbor. It is the fastest and lowest quality.

      Interp_Tiles,
      --  Accurate simulation of the Postscript image operator
      --  without any interpolation enabled; each pixel is rendered as a tiny
      --  parallelogram of solid color, the edges of which are implemented
      --  with anti-aliasing. It resembles nearest neighbor for enlargement,
      --  and bilinear for reduction.

      Interp_Bilinear,
      --  Bilinear interpolation. For enlargement, it is equivalent to
      --  point-sampling the ideal bilinear-interpolated image. For reduction,
      --  it is equivalent to laying down small tiles and integrating over the
      --  coverage area.

      Interp_Hyper
      --  Filter_Hyper is the highest quality reconstruction function. It is
      --  derived from the hyperbolic filters in Wolberg's "Digital Image
      --  Warping," and is formally defined as the hyperbolic-filter sampling
      --  the ideal hyperbolic-filter interpolated image (the filter is
      --  designed to be idempotent for 1:1 pixel mapping). It is the slowest
      --  and highest quality.
     );
   --  Interpolation methods.
   for Gdk_Interp_Type'Size use Gint'Size;

   type GError is new Glib.C_Proxy;
   --  Glib error handling.
   --  ??? Move this type to Glib.Error

   type Pixbuf_Error is
     (Corrupt_Image,
      --  image data hosed

      Insufficient_Memory,
      --  no mem to load image

      Bad_Option_Value,
      --  bad option value passed to save routine

      Unknown_Type,
      --  unsupported image type

      Unsupported_Operation,
      --  unsupported operation (load, save) for image type

      Failed
      --  Operation failed.
     );
   for Pixbuf_Error'Size use Gint'Size;

   type File_Format is (JPEG, PNG);
   --  Posibble formats when saving a file.

   type Image_Quality is range 0 .. 100;
   --  For a JPEG image only, quality of the image in percentage.

   type Alpha_Range is range 0 .. 255;
   --  Valid values for alpha parameters.
   for Alpha_Range'Size use Gint'Size;

   package Frame_List is new Glib.Glist.Generic_List (Gdk_Pixbuf_Frame);
   --  Handling of list of Pixbuf_Frames.

   type Rgb_Buffer is array (Natural) of Glib.Guchar;
   pragma Convention (C, Rgb_Buffer);
   --  See Rgb_Buffer_Access.

   type Rgb_Buffer_Access is access all Rgb_Buffer;
   --  Type used By Get_Pixels below to return an array with no bound checks
   --  tht is comatible with C (also known as a flat array).
   pragma Convention (C, Rgb_Buffer_Access);

   --------------
   -- Get_Type --
   --------------

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gdk_Pixbuf.

   ------------------------
   -- Reference counting --
   ------------------------

   procedure Ref (Pixbuf : Gdk_Pixbuf);
   --  Increment the reference counting on the image.
   --  The image is destroyed when its reference counting reaches 0.
   --  Note also that most of the time you won't have to call this
   --  function yourself.

   procedure Unref (Pixbuf : Gdk_Pixbuf);
   --  Decrement the reference counting on the image.

   --------------------------
   -- Accessing the fields --
   --------------------------

   function Get_Colorspace (Pixbuf : Gdk_Pixbuf) return Gdk_Colorspace;
   --  Query the color space of a pixbuf.

   function Get_N_Channels (Pixbuf : Gdk_Pixbuf) return Gint;
   --  Number of channels in the image.

   function Get_Has_Alpha (Pixbuf : Gdk_Pixbuf) return Boolean;
   --  Return True if the image has an alpha channel (opacity information).

   function Get_Bits_Per_Sample (Pixbuf : Gdk_Pixbuf) return Gint;
   --  Number of bits per color sample.

   function Get_Pixels (Pixbuf : Gdk_Pixbuf) return Rgb_Buffer_Access;
   --  Return a pointer to the pixel data of the image.

   function Get_Width (Pixbuf : Gdk_Pixbuf) return Gint;
   --  Return the width of the image in pixels.

   function Get_Height (Pixbuf : Gdk_Pixbuf) return Gint;
   --  Return the height of the image in pixels.

   function Get_Rowstride (Pixbuf : Gdk_Pixbuf) return Gint;
   --  Return the number of bytes between rows in the image data.

   --------------
   -- Creating --
   --------------

   function Gdk_New
     (Colorspace      : Gdk_Colorspace := Colorspace_RGB;
      Has_Alpha       : Boolean := False;
      Bits_Per_Sample : Gint := 8;
      Width           : Gint;
      Height          : Gint) return Gdk_Pixbuf;
   --  Create a blank pixbuf with an optimal row stride and a new buffer.
   --  The buffer is allocated, but not cleared.
   --  The reference counting is initialized to 1.

   function Copy (Pixbuf : Gdk_Pixbuf) return Gdk_Pixbuf;
   --  Copy a pixbuf.

   function Gdk_New_Subpixbuf
     (Src_Pixbuf : Gdk_Pixbuf;
      Src_X      : Gint;
      Src_Y      : Gint;
      Width      : Gint;
      Height     : Gint) return Gdk_Pixbuf;
   --  Create a pixbuf which points to the pixels of another pixbuf

   procedure Gdk_New_From_File
     (Pixbuf   : out Gdk_Pixbuf;
      Filename : String;
      Error    : out GError);
   --  Load an image from file.

   function Gdk_New_From_Xpm_Data
     (Data : Interfaces.C.Strings.chars_ptr_array) return Gdk_Pixbuf;
   --  Create an image from a XPM data.

   procedure Save
     (Pixbuf   : Gdk_Pixbuf;
      Filename : String;
      Format   : File_Format;
      Error    : out GError;
      Quality  : Image_Quality := Image_Quality'Last);
   --  Save pixbuf to a file.
   --  Quality is only taken into account for JPEG images.

   function Add_Alpha
     (Pixbuf           : Gdk_Pixbuf;
      Substitute_Color : Boolean := False;
      Red              : Guchar := 0;
      Green            : Guchar := 0;
      Blue             : Guchar := 0) return Gdk_Pixbuf;
   --  Add an alpha channel.
   --  Return a newly allocated image copied from Pixbuf, but with an
   --  extra alpha channel.
   --  If Pixbuf already had an alpha channel, the two images have exactly
   --  the same contents.
   --  If Substitute_Color is True, the color (Red, Green, Blue) is
   --  substituted for zero opacity.
   --  If Substitute_Color is False, Red, Green and Blue are ignored, and a
   --  new color is created with zero opacity.

   procedure Copy_Area
     (Src_Pixbuf  : Gdk_Pixbuf;
      Src_X       : Gint;
      Src_Y       : Gint;
      Width       : Gint;
      Height      : Gint;
      Dest_Pixbuf : Gdk_Pixbuf;
      Dest_X      : Gint;
      Dest_Y      : Gint);
   --  Copy a rectangular area from Src_pixbuf to Dest_pixbuf.
   --  Conversion of pixbuf formats is done automatically.

   procedure Saturate_And_Pixelate
     (Src        : Gdk_Pixbuf;
      Dest       : Gdk_Pixbuf;
      Saturation : Gfloat;
      Pixelate   : Boolean := True);
   --  Brighten/darken and optionally make it pixelated-looking.

   ---------------
   -- Rendering --
   ---------------

   procedure Render_Threshold_Alpha
     (Pixbuf          : Gdk_Pixbuf;
      Bitmap          : Gdk.Bitmap.Gdk_Bitmap;
      Src_X           : Gint;
      Src_Y           : Gint;
      Dest_X          : Gint;
      Dest_Y          : Gint;
      Width           : Gint;
      Height          : Gint;
      Alpha_Threshold : Alpha_Range);
   --  Take the opacity values in a rectangular portion of a pixbuf and
   --  thresholds them to produce a bi-level alpha mask that can be used as
   --  a clipping mask for a drawable.
   --  Bitmap is the bitmap where the bilevel mask will be painted to.
   --  Alpha_Threshold are the opacity values below which a pixel will be
   --  painted as zero. All other values will be painted as one.

   procedure Render_To_Drawable
     (Pixbuf   : Gdk_Pixbuf;
      Drawable : Gdk.Drawable.Gdk_Drawable;
      Gc       : Gdk.GC.Gdk_GC;
      Src_X    : Gint;
      Src_Y    : Gint;
      Dest_X   : Gint;
      Dest_Y   : Gint;
      Width    : Gint;
      Height   : Gint;
      Dither   : Gdk.Rgb.Gdk_Rgb_Dither := Gdk.Rgb.Dither_Normal;
      X_Dither : Gint := 0;
      Y_Dither : Gint := 0);
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
     (Pixbuf          : Gdk_Pixbuf;
      Drawable        : Gdk.Drawable.Gdk_Drawable;
      Src_X           : Gint;
      Src_Y           : Gint;
      Dest_X          : Gint;
      Dest_Y          : Gint;
      Width           : Gint;
      Height          : Gint;
      Alpha           : Alpha_Mode;
      Alpha_Threshold : Alpha_Range;
      Dither          : Gdk.Rgb.Gdk_Rgb_Dither := Gdk.Rgb.Dither_Normal;
      X_Dither        : Gint := 0;
      Y_Dither        : Gint := 0);
   --  Render a rectangular portion of a pixbuf to a drawable.
   --  This is done using Gdk.RGB, so the specified drawable must have the
   --  Gdk_RGB visual and colormap. When used with Alpha_Bilevel, this function
   --  has to create a bitmap out of the thresholded alpha channel of the
   --  image and, it has to set this bitmap as the clipping mask for the GC
   --  used for drawing.  This can be a significant performance penalty
   --  depending on the size and the complexity of the alpha channel of the
   --  image. If performance is crucial, consider handling the alpha channel
   --  yourself (possibly by caching it in your application) and using
   --  Render_To_Drawable or Gdk.RGB directly instead.
   --
   --  If the image does have opacity information and Alpha_Mode
   --  is Alpha_Bilevel, specifies the threshold value for opacity values

   procedure Render_Pixmap_And_Mask
     (Pixbuf          : Gdk_Pixbuf;
      Pixmap          : out Gdk.Pixmap.Gdk_Pixmap;
      Mask            : out Gdk.Bitmap.Gdk_Bitmap;
      Alpha_Threshold : out Alpha_Range);

   function Get_From_Drawable
     (Dest   : Gdk_Pixbuf;
      Src    : Gdk.Drawable.Gdk_Drawable;
      Cmap   : Gdk.Color.Gdk_Colormap;
      Src_X  : Gint;
      Src_Y  : Gint;
      Dest_X : Gint;
      Dest_Y : Gint;
      Width  : Gint;
      Height : Gint) return Gdk_Pixbuf;
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
   --  non-inferior windows, the contents of those regions are undefined.
   --  The contents of regions obscured by inferior windows of a different
   --  depth than that of the source window will also be undefined.
   --
   --  Return value: The same pixbuf as Dest if it was non-NULL, or a
   --  newly-created pixbuf with a reference count of 1 if no destination
   --  pixbuf was specified.

   -------------
   -- Scaling --
   -------------

   procedure Scale
     (Src          : Gdk_Pixbuf;
      Dest         : Gdk_Pixbuf;
      Dest_X       : Gint;
      Dest_Y       : Gint;
      Dest_Width   : Gint;
      Dest_Height  : Gint;
      Offset_X     : Gdouble := 0.0;
      Offset_Y     : Gdouble := 0.0;
      Scale_X      : Gdouble := 1.0;
      Scale_Y      : Gdouble := 1.0;
      Inter_Type   : Gdk_Interp_Type := Interp_Bilinear);
   --  Transform the source image by scaling by Scale_x and Scale_y then
   --  translating by Offset_x and Offset_y.
   --  The image is then rendered in the rectangle (Dest_x, Dest_y,
   --  Dest_width, Dest_height) of the resulting image onto the destination
   --  drawable replacing the previous contents.

   procedure Composite
     (Src           : Gdk_Pixbuf;
      Dest          : Gdk_Pixbuf;
      Dest_X        : Gint;
      Dest_Y        : Gint;
      Dest_Width    : Gint;
      Dest_Height   : Gint;
      Offset_X      : Gdouble := 0.0;
      Offset_Y      : Gdouble := 0.0;
      Scale_X       : Gdouble := 1.0;
      Scale_Y       : Gdouble := 1.0;
      Inter_Type    : Gdk_Interp_Type := Interp_Bilinear;
      Overall_Alpha : Alpha_Range := 128);
   --  Transform the source image by scaling by Scale_X and Scale_Y then
   --  translating by Offset_X and Offset_Y, then composite the rectangle
   --  (Dest_X, Dest_Y, Dest_Width, Dest_Height) of the resulting image onto
   --  the destination drawable.

   procedure Composite_Color
     (Src           : Gdk_Pixbuf;
      Dest          : Gdk_Pixbuf;
      Dest_X        : Gint;
      Dest_Y        : Gint;
      Dest_Width    : Gint;
      Dest_Height   : Gint;
      Offset_X      : Gdouble := 0.0;
      Offset_Y      : Gdouble := 0.0;
      Scale_X       : Gdouble := 1.0;
      Scale_Y       : Gdouble := 1.0;
      Inter_Type    : Gdk_Interp_Type := Interp_Bilinear;
      Overall_Alpha : Alpha_Range := 128;
      Check_X       : Gint := 0;
      Check_Y       : Gint := 0;
      Check_Size    : Gint := 0;
      Color1        : Guint32 := 0;
      Color2        : Guint32 := 0);
   --  Transform the source image by scaling by Scale_x and Scale_y then
   --  translating by Offset_x and Offset_y, then composites the rectangle
   --  (Dest_X, Dest_Y, Dest_Width, Dest_Height) of the resulting image with
   --  a checkboard of the colors Color1 and Color2 and renders it onto the
   --  destination drawable.
   --  The origin of checkboard is at (Check_x, Check_y)
   --  Color1 is the color at the upper left of the check.

   function Scale_Simple
     (Src           : Gdk_Pixbuf;
      Dest_Width    : Gint;
      Dest_Height   : Gint;
      Inter_Type    : Gdk_Interp_Type := Interp_Bilinear) return Gdk_Pixbuf;
   --  Scale the Src image to Dest_width x Dest_height and render the result
   --  into a new pixbuf.

   function Composite_Color_Simple
     (Src           : Gdk_Pixbuf;
      Dest_Width    : Gint;
      Dest_Height   : Gint;
      Inter_Type    : Gdk_Interp_Type := Interp_Bilinear;
      Overall_Alpha : Alpha_Range := 128;
      Color1        : Guint32 := 0;
      Color2        : Guint32 := 0) return Gdk_Pixbuf;
   --  Scale Src to Dest_width x Dest_height and composite the result with
   --  a checkboard of colors Color1 and Color2 and render the result into
   --  a new pixbuf.

   -----------------------
   -- Animation support --
   -----------------------

   function Get_Type_Animation return Glib.GType;
   --  Return the internal value associated with a Gdk_Pixbuf_Animation.

   procedure Gdk_New_From_File
     (Animation : out Gdk_Pixbuf_Animation;
      Filename  : String;
      Error     : out GError);
   --  Create a new animation by loading it from a file.
   --  The file format is detected automatically. If the file's format does not
   --  support multi-frame images, then an animation with a single frame will
   --  be created. Possible errors are in the Pixbuf_Error and GFile_Error
   --  domains.
   --  On return, Animation is a newly created animation with a reference count
   --  of 1, or null if any of several error conditions ocurred: the file could
   --  not be opened, there was no loader for the file's format, there was not
   --  enough memory to allocate the image buffer, or the image file contained
   --  invalid data.

   procedure Ref (Animation : Gdk_Pixbuf_Animation);
   --  Increment the reference counting on the animation.

   procedure Unref (Animation : Gdk_Pixbuf_Animation);
   --  Decrement the reference counting on the animation.

   function Get_Width (Animation : Gdk_Pixbuf_Animation) return Gint;
   --  Return the width of the bounding box of a pixbuf animation.

   function Get_Height (Animation : Gdk_Pixbuf_Animation) return Gint;
   --  Return the height of the bounding box of a pixbuf animation.

   function Get_Num_Frames (Animation : Gdk_Pixbuf_Animation) return Gint;
   --  Return the number of frames in a pixbuf animation.

   ---------------------
   -- Frame Accessors --
   ---------------------

   function Get_Pixbuf (Frame : Gdk_Pixbuf_Frame) return Gdk_Pixbuf;
   --  Return the pixbuf of an animation frame.

   function Get_X_Offset (Frame : Gdk_Pixbuf_Frame) return Gint;
   --  Return the X offset from the top left corner of an animation frame.

   function Get_Y_Offset (Frame : Gdk_Pixbuf_Frame) return Gint;
   --  Return the Y offset from the top left corner of an animation frame.

   function Get_Delay_Time (Frame : Gdk_Pixbuf_Frame) return Gint;
   --  Return the delay time in milliseconds of an animation frame.

   function Get_Action (Frame : Gdk_Pixbuf_Frame) return Frame_Action;
   --  Return the overlay action of an animation frame.

   procedure Free (Frame : Gdk_Pixbuf_Frame);
   --  Free a Gdk_Pixbuf_Frame.
   --  Don't do this with frames you got from Gdk_Pixbuf_Animation, usually
   --  the animation owns those (it doesn't make a copy before returning the
   --  frame).

   function Get_Type_Frame return Glib.GType;
   --  Return the internal values associated with a Gdk_Pixbuf_Frame.

private

   pragma Import (C, Get_Type, "gdk_pixbuf_get_type");
   pragma Import (C, Get_Type_Animation, "gdk_pixbuf_animation_get_type");
   pragma Import (C, Get_Type_Frame, "gdk_pixbuf_frame_get_type");
   pragma Import (C, Get_Colorspace, "gdk_pixbuf_get_colorspace");
   pragma Import (C, Get_N_Channels, "gdk_pixbuf_get_n_channels");
   pragma Import (C, Get_Bits_Per_Sample, "gdk_pixbuf_get_bits_per_sample");
   pragma Import (C, Get_Pixels, "gdk_pixbuf_get_pixels");
   pragma Import (C, Get_Rowstride, "gdk_pixbuf_get_rowstride");
   pragma Import (C, Gdk_New_Subpixbuf, "gdk_pixbuf_new_subpixbuf");
   pragma Import (C, Gdk_New_From_Xpm_Data, "gdk_pixbuf_new_from_xpm_data");
   pragma Import (C, Copy_Area, "gdk_pixbuf_copy_area");
   pragma Import (C, Scale, "gdk_pixbuf_scale");
   pragma Import (C, Composite, "gdk_pixbuf_composite");
   pragma Import (C, Composite_Color, "gdk_pixbuf_composite_color");
   pragma Import (C, Render_To_Drawable, "gdk_pixbuf_render_to_drawable");
   pragma Import
     (C, Render_To_Drawable_Alpha, "gdk_pixbuf_render_to_drawable_alpha");
   pragma Import
     (C, Render_Pixmap_And_Mask, "gdk_pixbuf_render_pixmap_and_mask");
   pragma Import (C, Scale_Simple, "gdk_pixbuf_scale_simple");
   pragma Import
     (C, Composite_Color_Simple, "gdk_pixbuf_composite_color_simple");
   pragma Import
     (C, Render_Threshold_Alpha, "gdk_pixbuf_render_threshold_alpha");
   pragma Import (C, Get_From_Drawable, "gdk_pixbuf_get_from_drawable");
   pragma Import (C, Get_Num_Frames, "gdk_pixbuf_animation_get_num_frames");
   pragma Import (C, Get_Pixbuf, "gdk_pixbuf_frame_get_pixbuf");
   pragma Import (C, Get_X_Offset, "gdk_pixbuf_frame_get_x_offset");
   pragma Import (C, Get_Y_Offset, "gdk_pixbuf_frame_get_y_offset");
   pragma Import (C, Get_Delay_Time, "gdk_pixbuf_frame_get_delay_time");
   pragma Import (C, Get_Action, "gdk_pixbuf_frame_get_action");
   pragma Import (C, Free, "gdk_pixbuf_frame_free");

end Gdk.Pixbuf;

--  missing: gdk_pixbuf_new_from_data

