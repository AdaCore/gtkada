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
--  See Gdk.Pixmap for more details on pixmap handling.
--
--  </description>
--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with Glib; use Glib;

with Gdk.Window;

package Gdk.Bitmap is

   subtype Gdk_Bitmap is Gdk.Gdk_Bitmap;
   --  A black and white image.
   --  This type is mainly used as a mask when drawing other colored images.
   --  Each pixel can have two values, 0 or 1.

   Null_Bitmap : constant Gdk_Bitmap;

   procedure Gdk_New
     (Bitmap : out Gdk_Bitmap;
      Window : Gdk.Window.Gdk_Window;
      Width  : Gint;
      Height : Gint);
   --  Create a new bitmap with a given size.
   --  Window is used to determine default values for the new bitmap.
   --  Can be eventually null in which case the root window is used.
   --  Width is the width of the new bitmap in pixels.
   --  Height is the height of the new bitmap in pixels.

   procedure Ref (Bitmap : Gdk_Bitmap);
   --  Add a reference to a bitmap.

   procedure Unref (Bitmap : Gdk_Bitmap);
   --  This is the usual way to destroy a bitmap. The memory is freed when
   --  there is no more reference

   procedure Create_From_Data
     (Bitmap : out Gdk_Bitmap;
      Window : Gdk.Window.Gdk_Window;
      Data   : String;
      Width  : Gint;
      Height : Gint);
   --  Create a bitmap from data in XBM format.
   --  Window is used to determine default values for the new bitmap, can be
   --  null in which case the root window is used.
   --  Data is the XBM data.
   --  Width is the width of the new bitmap in pixels.
   --  Height is the height of the new bitmap in pixels.

private
   Null_Bitmap : constant Gdk_Bitmap := null;
   pragma Import (C, Ref, "gdk_drawable_ref");
   pragma Import (C, Unref, "gdk_drawable_unref");
end Gdk.Bitmap;
