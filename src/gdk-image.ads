------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       ----                     Copyright (C) 1998-2012, AdaCore                     --
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

--  The Gdk_Image type represents an area for drawing graphics.
--  It has now been superceded to a large extent by the much more flexible
--  Gdk.RGB functions.
--
--  To create an empty Gdk_Image use Gdk_New. To create an image from part of
--  a Gdk_Window, use Get.
--
--  To draw a Gdk_Image in a Gdk_Window or Gdk_Pixmap use
--  Gdk.Drawable.Draw_Image.

--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with Glib; use Glib;

package Gdk.Image is

   subtype Gdk_Image is Gdk.Gdk_Image;
   Null_Image : constant Gdk_Image;

   type Gdk_Image_Type is (Image_Normal, Image_Shared, Image_Fastest);
   pragma Convention (C, Gdk_Image_Type);

   procedure Gdk_New
     (Image      : out Gdk_Image;
      Image_Type : Gdk_Image_Type;
      Visual     : Gdk.Gdk_Visual;
      Width      : Gint;
      Height     : Gint);

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gdk_Image.

   procedure Get
     (Image    : out Gdk_Image;
      Drawable : Gdk.Gdk_Drawable;
      X        : Gint;
      Y        : Gint;
      Width    : Gint;
      Height   : Gint);

   procedure Ref (Image : Gdk_Image);
   --  Increment the reference counting for the image.

   procedure Unref (Image : Gdk_Image);
   --  Decrement the reference counting for the image.
   --  When this reaches 0, the image is destroyed.

   procedure Destroy (Image : Gdk_Image);
   --  Deprecated. Use Unref instead.

   procedure Put_Pixel
     (Image : Gdk_Image;
      X     : Gint;
      Y     : Gint;
      Pixel : Guint32);

   function Get_Pixel (Image : Gdk_Image; X : Gint; Y : Gint) return Guint32;

private
   Null_Image : constant Gdk_Image := null;
   pragma Import (C, Get_Type, "gdk_image_get_type");
   pragma Import (C, Destroy, "gdk_image_unref");
   pragma Import (C, Ref, "gdk_image_ref");
   pragma Import (C, Unref, "gdk_image_unref");
   pragma Import (C, Get_Pixel, "gdk_image_get_pixel");
   pragma Import (C, Put_Pixel, "gdk_image_put_pixel");
end Gdk.Image;
