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
--
--  This package provides a minimal interface to the libart library.
--  It gives access to the basic types used by Gdk.Pixbuf.
--
--  </description>

with Glib;
with System;

package Gdk.Art.Pixbuf is

   type Art_Pix_Format is (Art_Pix_RGB);
   --  type of the image.
   --  The only possible value is currently RGB, but extensions will
   --  exist with CMYK, Gray, Lab, ...

   type Art_Pixbuf is private;
   --  A buffer that contains the image.
   --  It supports alpha channels (transparency).

   type Art_Filter_Level is
     (Filter_Nearest,
      Filter_Tiles,
      Filter_Bilinear,
      Filter_Hyper);
   --  Filter_Nearest is nearest neighbor. It is the fastest and lowest
   --  quality.
   --
   --  Filter_Tiles is an accurate simulation of the Postscript image operator
   --  without any interpolation enabled; each pixel is rendered as a tiny
   --  parallelogram of solid color, the edges of which are implemented
   --  with anti-aliasing. It resembles nearest neighbor for enlargement,
   --  and bilinear for reduction.
   --
   --  Filter_Bilinear is bilinear interpolation. For enlargement, it is
   --  equivalent to point-sampling the ideal bilinear-interpolated
   --  image. For reduction, it is equivalent to laying down small tiles
   --  and integrating over the coverage area.
   --
   --  Filter_Hyper is the highest quality reconstruction function. It is
   --  derived from the hyperbolic filters in Wolberg's "Digital Image
   --  Warping," and is formally defined as the hyperbolic-filter sampling
   --  the ideal hyperbolic-filter interpolated image (the filter is designed
   --  to be idempotent for 1:1 pixel mapping). It is the slowest and highest
   --  quality.
   --
   --  Note: at this stage of implementation, most filter modes are likely
   --  not to be implemented.

   subtype Art_U32 is Glib.Guint32;
   --  32 bits unsigned integer used within libart.

private
   type Art_Pixbuf is new System.Address;
end Gdk.Art.Pixbuf;
