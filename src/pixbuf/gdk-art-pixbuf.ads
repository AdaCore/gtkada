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

   type Art_Filter_Level is (Filter_Nearest,
                             Filter_Tiles,
                             Filter_Bilinear,
                             Filter_Hyper);
   --  Filter_Nearest is nearest neighbor. It is the fastest and lowest
   --  quality.
   --
   --  Filter_Tiles is an accurate simulation of the PostScript image operator
   --  without any interpolation enabled; each pixel is rendered as a tiny
   --  parallelogram of solid color, the edges of which are implemented
   --  with antialiasing. It resembles nearest neighbor for enlargement,
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

private
   type Art_Pixbuf is new System.Address;
end Gdk.Art.Pixbuf;
