-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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
--
--  Pixmaps are offscreen drawables. They can be drawn upon with the standard
--  drawing primitives, then copied to another drawable (such as a Gdk_Window)
--  with Gdk.Pixmap.Draw. The depth of a pixmap is the number of bits per
--  pixels. Bitmaps are simply pixmaps with a depth of 1. (That is, they are
--  monochrome bitmaps - each pixel can be either on or off).
--  @pxref{Package_Gdk.Bitmap} for more details on bitmap handling.
--
--  </description>
--  <c_version>1.2.7</c_version>

with Glib; use Glib;
with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Drawable;
with Gdk.Window;
with Gtkada.Types;

package Gdk.Pixmap is

   subtype Gdk_Pixmap is Gdk.Drawable.Gdk_Drawable;
   --  A server-side image.
   --  You can create an empty pixmap, or load if from external files in
   --  bitmap and pixmap format. See Gdk.Pixbuf if you need to load
   --  images in other formats.

   Null_Pixmap : constant Gdk_Pixmap;

   procedure Gdk_New (Pixmap :    out Gdk_Pixmap;
                      Window : in     Gdk.Window.Gdk_Window;
                      Width  : in     Gint;
                      Height : in     Gint;
                      Depth  : in     Gint := -1);
   --  Create a new pixmap with a given size.
   --  Window is used to determine default values for the new pixmap.
   --  Can be eventually null.
   --  Width is the width of the new pixmap in pixels.
   --  Height is the height of the new pixmap in pixels.
   --  Depth is the depth (number of bits per pixel) of the new pixmap.
   --  If -1, and window is not null, the depth of the new pixmap will be
   --  equal to that of window.
   --  Automatically reference the pixmap once.

   procedure Ref (Pixmap : in Gdk_Pixmap);
   --  Add a reference to a pixmap.

   procedure Unref (Pixmap : in out Gdk_Pixmap);
   --  This is the usual way to destroy a pixmap. The memory is freed when
   --  there is no more reference

   procedure Create_From_Data (Pixmap :    out Gdk_Pixmap;
                               Window : in     Gdk.Window.Gdk_Window;
                               Data   : in     String;
                               Width  : in     Gint;
                               Height : in     Gint;
                               Depth  : in     Gint;
                               Fg     : in     Color.Gdk_Color;
                               Bg     : in     Color.Gdk_Color);
   --  Create a pixmap from data in XBM format.
   --  Window is used to determine default values for the new bitmap, can be
   --  null in which case the root window is used.
   --  Data is the XBM data.
   --  Width is the width of the new bitmap in pixels.
   --  Height is the height of the new bitmap in pixels.
   --  Depth is the depth (number of bits per pixel) of the new pixmap.
   --  Fg is the foreground color.
   --  Bg is the background color.

   procedure Create_From_Xpm (Pixmap      :    out Gdk_Pixmap;
                              Window      : in     Gdk.Window.Gdk_Window;
                              Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
                              Transparent : in     Gdk.Color.Gdk_Color;
                              Filename    : in     String);
   --  Create a pixmap from a XPM file.
   --  Window is used to determine default values for the new pixmap.
   --  Mask is a pointer to a place to store a bitmap representing the
   --  transparency mask of the XPM file. Can be null, in which case
   --  transparency will be ignored.
   --  Transparent is the color to be used for the pixels that are transparent
   --  in the input file. Can be null, in which case a default color will be
   --  used.
   --  Filename is the filename of a file containing XPM data.

   procedure Create_From_Xpm (Pixmap      :    out Gdk_Pixmap;
                              Window      : in     Gdk.Window.Gdk_Window;
                              Colormap    : in     Gdk.Color.Gdk_Colormap;
                              Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
                              Transparent : in     Gdk.Color.Gdk_Color;
                              Filename    : in     String);
   --  Create a pixmap from a XPM file using a particular colormap.
   --  Window is used to determine default values for the new pixmap. Can be
   --  null if colormap is given.
   --  Colormap is the Gdk_Colormap that the new pixmap will use. If omitted,
   --  the colormap for window will be used.
   --  Mask is a pointer to a place to store a bitmap representing the
   --  transparency mask of the XPM file. Can be null, in which case
   --  transparency will be ignored.
   --  Transparent is the color to be used for the pixels that are transparent
   --  in the input file. Can be null, in which case a default color will be
   --  used.
   --  Filename is the filename of a file containing XPM data.

   procedure Create_From_Xpm_D
     (Pixmap      :    out Gdk_Pixmap;
      Window      : in     Gdk.Window.Gdk_Window;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : in     Gdk.Color.Gdk_Color;
      Data        : in     Gtkada.Types.Chars_Ptr_Array);
   --  Create a pixmap from data in XPM format.
   --  Window is used to determine default values for the new pixmap.
   --  Mask is a pointer to a place to store a bitmap representing the
   --  transparency mask of the XPM file. Can be null, in which case
   --  transparency will be ignored.
   --  Transparent will be used for the pixels that are transparent in the
   --  input file. Can be null in which case a default color will be used.
   --  Data is a pointer to a string containing the XPM data.

   procedure Create_From_Xpm_D
     (Pixmap      :    out Gdk_Pixmap;
      Window      : in     Gdk.Window.Gdk_Window;
      Colormap    : in     Gdk.Color.Gdk_Colormap;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : in     Gdk.Color.Gdk_Color;
      Data        : in     Gtkada.Types.Chars_Ptr_Array);
   --  Create a pixmap from data in XPM format using a particular colormap.
   --  Window is used to determine default values for the new pixmap.
   --  Colormap is the Gdk_Colormap that the new pixmap will be use. If
   --  omitted, the colormap for window will be used.
   --  Mask is a pointer to a place to store a bitmap representing the
   --  transparency mask of the XPM file. Can be null, in which case
   --  transparency will be ignored.
   --  Transparent will be used for the pixels that are transparent in the
   --  input file. Can be null in which case a default color will be used.
   --  Data is a pointer to a string containing the XPM data.

private
   Null_Pixmap : constant Gdk_Pixmap := null;
   pragma Import (C, Ref, "gdk_pixmap_ref");
   pragma Import (C, Unref, "gdk_pixmap_unref");
end Gdk.Pixmap;
