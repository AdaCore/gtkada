-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

--  The Gdk_Image type represents an area for drawing graphics.
--  It has now been superceded to a large extent by the much more flexible
--  Gdk.RGB functions.
--
--  To create an empty Gdk_Image use Gdk_New. To create an image from part of
--  a Gdk_Window, use Get.
--
--  To draw a Gdk_Image in a Gdk_Window or Gdk_Pixmap use
--  Gdk.Drawable.Draw_Image.

with Glib; use Glib;
with Gdk.Visual;
with Gdk.Window;

package Gdk.Image is

   type Gdk_Image is new Gdk.C_Proxy;
   Null_Image : constant Gdk_Image;

   type Gdk_Image_Type is (Image_Normal, Image_Shared, Image_Fastest);

   procedure Gdk_New
     (Image      :    out Gdk_Image;
      Image_Type : in     Gdk_Image_Type;
      Visual     : in     Gdk.Visual.Gdk_Visual;
      Width      : in     Gint;
      Height     : in     Gint);

   procedure Get
     (Image  :    out Gdk_Image;
      Window : in     Gdk.Window.Gdk_Window;
      X      : in     Gint;
      Y      : in     Gint;
      Width  : in     Gint;
      Height : in     Gint);

   procedure Put_Pixel
     (Image : in Gdk_Image;
      X     : in Gint;
      Y     : in Gint;
      Pixel : in Guint32);

   function Get_Pixel
     (Image : in Gdk_Image;
      X     : in Gint;
      Y     : in Gint) return Guint32;

   procedure Destroy (Image : in Gdk_Image);

private
   Null_Image : constant Gdk_Image := null;
   pragma Import (C, Destroy, "gdk_image_destroy");
   pragma Import (C, Get_Pixel, "gdk_image_get_pixel");
   pragma Import (C, Put_Pixel, "gdk_image_put_pixel");
end Gdk.Image;
