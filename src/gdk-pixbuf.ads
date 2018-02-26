------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

--  <description>
--  This object provides image manipulation routines.
--
--  The following image formats are known, but some depend on external
--  libraries for the proper loading of files (indicated with * in the list):
--     PNG*, JPEG*, TIFF*, GIF, XPM, PNM, Sun raster file (ras), ico,
--     bmp.
--
--  With this package, you can load images from file, display them on the
--  screen, re-scale them and compose them with other images.
--  All the functions fully support alpha channels (opacity).
--
--  Different filters are provided, depending on the quality of output you
--  expect and the speed you need.
--  </description>
--  <c_version>1.3.11</c_version>
--  <group>Gdk, the low-level API</group>
--  <testgtk>create_pixbuf.adb</testgtk>

with System;
with Cairo;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object;
with Gdk.Display;
with Gtkada.Types;

package Gdk.Pixbuf is

   type Gdk_Pixbuf_Record is new Glib.Object.GObject_Record with private;

   type Gdk_Pixbuf is access all Gdk_Pixbuf_Record'Class;
   --  A very efficient client-side pixmap.
   --  This type can be adapted to all the possible screen depths (number of
   --  bits per pixel), and the algorithms are extremely efficient.
   --  You can also load a pixbuf directly from an external file in one of
   --  the standard image formats.

   Null_Pixbuf : constant Gdk_Pixbuf := null;

   type Gdk_Pixbuf_Animation is new Glib.C_Proxy;
   --  Type used for animations.

   type Gdk_Pixbuf_Animation_Iter is new Glib.C_Proxy;
   --  Type used to iterate through an animation.

   type Alpha_Mode is (Alpha_Bilevel, Alpha_Full);
   --  Alpha compositing mode.
   --  This indicates how the alpha channel (for opacity) is handled when
   --  rendering.
   pragma Convention (C, Alpha_Mode);

   type Gdk_Colorspace is (Colorspace_RGB);
   --  Type of the image.
   --  The only possible value is currently RGB, but extensions will
   --  exist with CMYK, Gray, Lab, ...
   pragma Convention (C, Gdk_Colorspace);

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
   pragma Convention (C, Gdk_Interp_Type);

   ------------
   -- Errors --
   ------------

   --  Errors defined in the Pixbuf_Error domain:

   Corrupt_Image         : constant := 0;
   --  image data hosed

   Insufficient_Memory   : constant := 1;
   --  no mem to load image

   Bad_Option            : constant := 2;
   --  bad option passed to save routine

   Unknown_Type          : constant := 3;
   --  unsupported image type

   Unsupported_Operation : constant := 4;
   --  unsupported operation (load, save) for image type

   Failed                : constant := 5;
   --  Operation failed.

   type File_Format is (JPEG, PNG, ICO, BMP);
   --  Possible formats when saving a file.

   type Image_Quality is range 0 .. 100;
   --  For a JPEG image only, quality of the image in percentage.

   type Alpha_Range is range 0 .. 255;
   --  Valid values for alpha parameters.

   pragma Convention (C, Alpha_Range);
   type Gdk_Rgb_Dither is (Dither_None, Dither_Normal, Dither_Max);
   --  The three kinds of dithering that are implemented in this package:
   --  - Dither_None: No dithering will be done
   --  - Dither_Normal: Specifies dithering on 8 bit displays, but not 16-bit.
   --                   Usually the best choice.
   --  - Dither_Max: Specifies dithering on every kind of display
   for Gdk_Rgb_Dither'Size use Glib.Gint'Size;

   type Rgb_Record is record
      Red, Green, Blue : Glib.Guchar;
   end record;
   pragma Convention (C, Rgb_Record);

   --  This is the buffer that will contain the image. You can manipulate each
   --  byte in it independantly, although there is no high level routine
   --  to draw lines, circles, ...
   --  Once you are done drawing into this buffer, you can copy it to any
   --  drawable on the screen, *if* the widget was created with the correct
   --  visual and colormap (see above).

   type Unchecked_Rgb_Buffer is array (Glib.Guint) of Rgb_Record;
   pragma Convention (C, Unchecked_Rgb_Buffer);
   type Rgb_Buffer_Access is access all Unchecked_Rgb_Buffer;
   pragma Convention (C, Rgb_Buffer_Access);
   --  Type used By Get_Pixels to return an array with no
   --  bound checks that is compatible with C (also known as a flat array).

   --------------
   -- Get_Type --
   --------------

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gdk_Pixbuf.

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

   function Gdk_New_From_Data
     (Data              : Guchar_Array_Access;
      Colorspace        : Gdk_Colorspace := Colorspace_RGB;
      Has_Alpha         : Boolean := False;
      Bits_Per_Sample   : Gint := 8;
      Width             : Gint;
      Height            : Gint;
      Rowstride         : Gint;
      Auto_Destroy_Data : Boolean := True) return Gdk_Pixbuf;
   --  Create a pixbuf out of in-memory image data.
   --  Currently only RGB images with 8 bits per sample are supported.
   --  Width and Height must be > 0.
   --  Rowstride is the distance in bytes between row starts.
   --  A typical value is 4*Width when there is an Alpha channel.
   --  If Auto_Destroy_Data is true, passed data will be automatically
   --  freed when the reference count of the pixbuf reaches 1.
   --  Otherwise, data is never freed.

   function Gdk_New_From_Xpm_Data
     (Data : Gtkada.Types.Chars_Ptr_Array) return Gdk_Pixbuf;
   --  Create an image from a XPM data.

   procedure Fill (Pixbuf : Gdk_Pixbuf; Pixel : Guint32);
   --  Fill pixbuf with a given pixel value.

   procedure Save
     (Pixbuf   : Gdk_Pixbuf;
      Filename : String;
      Format   : File_Format;
      Error    : out GError;
      Quality  : Image_Quality := Image_Quality'Last;
      Depth    : Integer := 32);
   --  Save pixbuf to a file.
   --  Quality is only taken into account for JPEG images.
   --  Depth is only taken into account for ICO images and can take the values
   --  16, 24 or 32.
   --  Error is set to null on success, and set to a GError otherwise.

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

   function Get_From_Window
     (Window : Gdk_Window;
      Src_X  : Gint;
      Src_Y  : Gint;
      Width  : Gint;
      Height : Gint) return Gdk_Pixbuf;
   function Get_From_Surface
     (Surface : Cairo.Cairo_Surface;
      Src_X   : Gint;
      Src_Y   : Gint;
      Width   : Gint;
      Height  : Gint) return Gdk_Pixbuf;
   --  Transfers image data from a Gdk_Window and converts it to an RGB(A)
   --  representation inside a Gdk_Pixbuf. In other words, copies
   --  image data from a server-side drawable to a client-side RGB(A) buffer.
   --  This allows you to efficiently read individual pixels on the client
   --  side.
   --
   --  This function will create an RGB pixbuf with 8 bits per channel with
   --  the same size specified by the Width and Height arguments. The pixbuf
   --  will contain an alpha channel if the window contains one.
   --
   --  If the window is off the screen, then there is no image data in the
   --  obscured/offscreen regions to be placed in the pixbuf. The contents
   --  of portions of the pixbuf corresponding to the offscreen region are
   --  undefined.
   --
   --  If the window you're obtaining data from is partially obscured by other
   --  windows, then the contents of the pixbuf areas corresponding to the
   --  obscured regions are undefined.
   --
   --  If the window is not mapped (typically because it's iconified/minimized
   --  or not on the current workspace), then Null_Pixbuf will be returned.
   --
   --  If memory can't be allocated for the return value, Null_Pixbuf
   --  will be returned instead.
   --
   --  (In short, there are several ways this function can fail, and if it
   --   fails it returns Null_Pixbuf; so check the return value.)
   --
   --  Return value: (transfer full): A newly-created pixbuf with a reference
   --      count of 1, or Null_Pixbuf on error

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

   function Is_Static_Image (Animation : Gdk_Pixbuf_Animation) return Boolean;
   --  If you load a file with Gdk_New_From_File and it turns out to be a
   --  plain, unanimated image, then this function will return True.
   --  Use Get_Static_Image to retrieve the image.

   function Get_Static_Image
     (Animation : Gdk_Pixbuf_Animation) return Gdk_Pixbuf;
   --  If an animation is really just a plain image (has only one frame),
   --  this function returns that image. If the animation is an animation,
   --  this function returns a reasonable thing to display as a static
   --  unanimated image, which might be the first frame, or something more
   --  sophisticated. If an animation hasn't loaded any frames yet, this
   --  function will return null.

   function Get_Iter
     (Animation  : Gdk_Pixbuf_Animation;
      Start_Time : GTime_Val_Access := null)
      return Gdk_Pixbuf_Animation_Iter;
   --  Get an iterator for displaying an animation. The iterator provides
   --  the frames that should be displayed at a given time.
   --  It should be freed after use with Unref.
   --
   --  Start_Time would normally come from G_Get_Current_Time, and marks the
   --  beginning of animation playback. After creating an iterator, you should
   --  immediately display the pixbuf returned by Get_Pixbuf. Then, you should
   --  install a timeout (with Timeout_Add) or by some other mechanism to
   --  ensure that you'll update the image after Get_Delay_Time milliseconds.
   --  Each time the image is updated, you should reinstall the timeout with
   --  the new, possibly-changed delay time.
   --
   --  As a shortcut, if Start_Time is equal to null, the result of
   --  G_Get_Current_Time will be used automatically.
   --
   --  To update the image (i.e. possibly change the result of Get_Pixbuf to a
   --  new frame of the animation), call Advance.
   --
   --  If you're using Gdk_Pixbuf_Loader, in addition to updating the image
   --  after the delay time, you should also update it whenever you
   --  receive the area_updated signal and On_Currently_Loading_Frame returns
   --  True. In this case, the frame currently being fed into the loader
   --  has received new data, so needs to be refreshed. The delay time for
   --  a frame may also be modified after an area_updated signal, for
   --  example if the delay time for a frame is encoded in the data after
   --  the frame itself. So your timeout should be reinstalled after any
   --  area_updated signal.
   --
   --  A delay time of -1 is possible, indicating "infinite."

   ---------------
   -- Iterators --
   ---------------

   function Get_Type_Animation_Iter return Glib.GType;
   --  Return the internal value associated with a Gdk_Pixbuf_Animation_Iter.

   procedure Ref (Iter : Gdk_Pixbuf_Animation_Iter);
   --  Increment the reference counting on the iterator.

   procedure Unref (Iter : Gdk_Pixbuf_Animation_Iter);
   --  Decrement the reference counting on the iterator.

   function Get_Delay_Time (Iter : Gdk_Pixbuf_Animation_Iter) return Gint;
   --  Return the number of milliseconds the current pixbuf should be displayed
   --  or -1 if the current pixbuf should be displayed forever. Timeout_Add
   --  conveniently takes a timeout in milliseconds, so you can use a timeout
   --  to schedule the next update.

   function Get_Pixbuf (Iter : Gdk_Pixbuf_Animation_Iter) return Gdk_Pixbuf;
   --  Return the current pixbuf which should be displayed.
   --  The pixbuf will be the same size as the animation itself (Get_Width,
   --  Get_Height). This pixbuf should be displayed for Get_Delay_Time
   --  milliseconds. The caller of this function does not own a reference to
   --  the returned pixbuf; the returned pixbuf will become invalid when the
   --  iterator advances to the next frame, which may happen anytime you call
   --  Advance. Copy the pixbuf to keep it (don't just add a reference), as it
   --  may get recycled as you advance the iterator.

   function On_Currently_Loading_Frame
     (Iter : Gdk_Pixbuf_Animation_Iter) return Boolean;
   --  Used to determine how to respond to the area_updated signal on
   --  Gdk_Pixbuf_Loader when loading an animation. area_updated is emitted
   --  for an area of the frame currently streaming in to the loader. So if
   --  you're on the currently loading frame, you need to redraw the screen for
   --  the updated area.

   function Advance
     (Iter          : Gdk_Pixbuf_Animation_Iter;
      Current_Timer : GTime_Val_Access := null) return Boolean;
   --  Possibly advance an animation to a new frame.
   --  Chooses the frame based on the start time passed to Get_Iter.
   --
   --  Current_Time would normally come from G_Get_Current_Time, and
   --  must be greater than or equal to the time passed to Get_Iter,
   --  and must increase or remain unchanged each time Get_Pixbuf is
   --  called. That is, you can't go backward in time; animations only
   --  play forward.
   --
   --  As a shortcut, pass null for the current time and G_Get_Current_Time
   --  will be invoked on your behalf. So you only need to explicitly pass
   --  Current_Time if you're doing something odd like playing the animation
   --  at double speed.
   --
   --  If this function returns False, there's no need to update the animation
   --  display, assuming the display had been rendered prior to advancing;
   --  if True, you need to call Get_Pixbuf and update the display with the new
   --  pixbuf.

   -------------
   -- Cursors --
   -------------

   procedure Gdk_New_From_Pixbuf
     (Cursor  : out Gdk.Gdk_Cursor;
      Display : Gdk.Display.Gdk_Display := Gdk.Display.Get_Default;
      Pixbuf  : Gdk_Pixbuf;
      X       : Glib.Gint;
      Y       : Glib.Gint);
   --  Create a cursor from a pixbuf.
   --  Not all GDK backends support RGBA cursors. If they are not supported,
   --  a monochrome approximation will be displayed.
   --  The functions gdk.display.supports_cursor_alpha and
   --  gdk.display.supports_cursor_color can be used to determine whether RGBA
   --  cursors are supported;
   --  gdk.display.get_default_cursor_size and
   --  gdk.display.get_maximal_cursor_size give information about cursor sizes.
   --  On the X backend, support for RGBA cursors requires a sufficently new
   --  version of the X Render extension.

   function Get_Image (Cursor : Gdk.Gdk_Cursor) return Gdk_Pixbuf;
   --  Return the image stored in the cursor

   --  <doc_ignore>
   function Convert (P : System.Address) return Gdk_Pixbuf;
   --  </doc_ignore>

private

   type Gdk_Pixbuf_Record is new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gdk_pixbuf_get_type");
   pragma Import (C, Get_Type_Animation, "gdk_pixbuf_animation_get_type");
   pragma Import
     (C, Get_Type_Animation_Iter, "gdk_pixbuf_animation_iter_get_type");
   pragma Import (C, Get_Iter, "gdk_pixbuf_animation_get_iter");
   pragma Import
     (C, Get_Delay_Time, "gdk_pixbuf_animation_iter_get_delay_time");

end Gdk.Pixbuf;
