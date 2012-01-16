------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
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
--
--  Pixmaps are off-screen drawables. They can be drawn upon with the standard
--  drawing primitives, then copied to another drawable (such as a Gdk_Window)
--  with Gdk.Drawable.Draw_Drawable. The depth of a pixmap is the number of
--  bits per pixels. Bitmaps are simply pixmaps with a depth of 1. (That is,
--  they are monochrome bitmaps - each pixel can be either on or off).
--  See Gdk.Bitmap for more details on bitmap handling.
--
--  </description>
--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with Glib; use Glib;
with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Window;
with Gtkada.Types;

package Gdk.Pixmap is

   subtype Gdk_Pixmap is Gdk.Gdk_Pixmap;
   --  A server-side image.
   --  You can create an empty pixmap, or load if from external files in
   --  bitmap and pixmap format. See Gdk.Pixbuf if you need to load
   --  images in other formats.

   Null_Pixmap : constant Gdk_Pixmap;

   procedure Gdk_New
     (Pixmap : out Gdk_Pixmap;
      Window : Gdk.Window.Gdk_Window;
      Width  : Gint;
      Height : Gint;
      Depth  : Gint := -1);
   --  Create a new pixmap with a given size.
   --  Window is used to determine default values for the new pixmap.
   --  Can be eventually null.
   --  Width is the width of the new pixmap in pixels.
   --  Height is the height of the new pixmap in pixels.
   --  Depth is the depth (number of bits per pixel) of the new pixmap.
   --  If -1, and window is not null, the depth of the new pixmap will be
   --  equal to that of window.
   --  Automatically reference the pixmap once.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with Gdk_Pixmap.

   procedure Ref (Pixmap : Gdk_Pixmap);
   --  Add a reference to a pixmap.

   procedure Unref (Pixmap : Gdk_Pixmap);
   --  This is the usual way to destroy a pixmap. The memory is freed when
   --  there is no more reference

   procedure Create_From_Data
     (Pixmap : out Gdk_Pixmap;
      Window : Gdk.Window.Gdk_Window;
      Data   : String;
      Width  : Gint;
      Height : Gint;
      Depth  : Gint;
      Fg     : Color.Gdk_Color;
      Bg     : Color.Gdk_Color);
   --  Create a pixmap from data in XBM format.
   --  Window is used to determine default values for the new bitmap, can be
   --  null in which case the root window is used.
   --  Data is the XBM data.
   --  Width is the width of the new bitmap in pixels.
   --  Height is the height of the new bitmap in pixels.
   --  Depth is the depth (number of bits per pixel) of the new pixmap.
   --  Fg is the foreground color.
   --  Bg is the background color.

   procedure Create_From_Xpm
     (Pixmap      : out Gdk_Pixmap;
      Window      : Gdk.Window.Gdk_Window;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : Gdk.Color.Gdk_Color;
      Filename    : String);
   --  Create a pixmap from a XPM file.
   --  Window is used to determine default values for the new pixmap.
   --  Mask is a pointer to a place to store a bitmap representing the
   --  transparency mask of the XPM file. Can be null, in which case
   --  transparency will be ignored.
   --  Transparent is the color to be used for the pixels that are transparent
   --  in the input file. Can be null, in which case a default color will be
   --  used.
   --  Filename is the filename of a file containing XPM data.

   procedure Create_From_Xpm
     (Pixmap      : out Gdk_Pixmap;
      Window      : Gdk.Window.Gdk_Window;
      Colormap    : Gdk.Color.Gdk_Colormap;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : Gdk.Color.Gdk_Color;
      Filename    : String);
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
     (Pixmap      : out Gdk_Pixmap;
      Window      : Gdk.Window.Gdk_Window;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : Gdk.Color.Gdk_Color;
      Data        : Gtkada.Types.Chars_Ptr_Array);
   --  Create a pixmap from data in XPM format.
   --  Window is used to determine default values for the new pixmap.
   --  Mask is a pointer to a place to store a bitmap representing the
   --  transparency mask of the XPM file. Can be null, in which case
   --  transparency will be ignored.
   --  Transparent will be used for the pixels that are transparent in the
   --  input file. Can be null in which case a default color will be used.
   --  Data is a pointer to a string containing the XPM data.

   procedure Create_From_Xpm_D
     (Pixmap      : out Gdk_Pixmap;
      Window      : Gdk.Window.Gdk_Window;
      Colormap    : Gdk.Color.Gdk_Colormap;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : Gdk.Color.Gdk_Color;
      Data        : Gtkada.Types.Chars_Ptr_Array);
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
   pragma Import (C, Get_Type, "gdk_pixmap_get_type");
   pragma Import (C, Ref, "gdk_drawable_ref");
   pragma Import (C, Unref, "gdk_drawable_unref");
end Gdk.Pixmap;
