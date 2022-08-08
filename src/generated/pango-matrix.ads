------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
--  A structure specifying a transformation between user-space coordinates and
--  device coordinates. The transformation is given by
--
--    x_device = x_user * matrix->xx + y_user * matrix->xy + matrix->x0;
--    y_device = x_user * matrix->yx + y_user * matrix->yy + matrix->y0;
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib; use Glib;

package Pango.Matrix is

   type Pango_Matrix is record
      Xx : Gdouble;
      Xy : Gdouble;
      Yx : Gdouble;
      Yy : Gdouble;
      X0 : Gdouble;
      Y0 : Gdouble;
   end record;
   pragma Convention (C, Pango_Matrix);

   function From_Object_Free (B : access Pango_Matrix) return Pango_Matrix;
   pragma Inline (From_Object_Free);
   --  A structure specifying a transformation between user-space coordinates
   --  and device coordinates. The transformation is given by
   --
   --    x_device = x_user * matrix->xx + y_user * matrix->xy + matrix->x0;
   --    y_device = x_user * matrix->yx + y_user * matrix->yy + matrix->y0;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "pango_matrix_get_type");

   -------------
   -- Methods --
   -------------

   procedure Concat (Self : Pango_Matrix; New_Matrix : Pango_Matrix);
   pragma Import (C, Concat, "pango_matrix_concat");
   --  Changes the transformation represented by Matrix to be the
   --  transformation given by first applying transformation given by
   --  New_Matrix then applying the original transformation.
   --  Since: gtk+ 1.6
   --  "new_matrix": a Pango.Matrix.Pango_Matrix

   function Copy (Self : Pango_Matrix) return Pango_Matrix;
   pragma Import (C, Copy, "pango_matrix_copy");
   --  Copies a Pango.Matrix.Pango_Matrix.
   --  Since: gtk+ 1.6

   procedure Free (Self : Pango_Matrix);
   pragma Import (C, Free, "pango_matrix_free");
   --  Free a Pango.Matrix.Pango_Matrix created with Pango.Matrix.Copy.
   --  Since: gtk+ 1.6

   function Get_Font_Scale_Factor (Self : Pango_Matrix) return Gdouble;
   pragma Import (C, Get_Font_Scale_Factor, "pango_matrix_get_font_scale_factor");
   --  Returns the scale factor of a matrix on the height of the font. That
   --  is, the scale factor in the direction perpendicular to the vector that
   --  the X coordinate is mapped to. If the scale in the X coordinate is
   --  needed as well, use Pango.Matrix.Get_Font_Scale_Factors.
   --  Since: gtk+ 1.12

   procedure Get_Font_Scale_Factors
      (Self   : Pango_Matrix;
       Xscale : out Gdouble;
       Yscale : out Gdouble);
   pragma Import (C, Get_Font_Scale_Factors, "pango_matrix_get_font_scale_factors");
   --  Calculates the scale factor of a matrix on the width and height of the
   --  font. That is, Xscale is the scale factor in the direction of the X
   --  coordinate, and Yscale is the scale factor in the direction
   --  perpendicular to the vector that the X coordinate is mapped to.
   --  Note that output numbers will always be non-negative.
   --  Since: gtk+ 1.38
   --  "xscale": output scale factor in the x direction, or null
   --  "yscale": output scale factor perpendicular to the x direction, or null

   procedure Rotate (Self : in out Pango_Matrix; Degrees : Gdouble);
   pragma Import (C, Rotate, "pango_matrix_rotate");
   --  Changes the transformation represented by Matrix to be the
   --  transformation given by first rotating by Degrees degrees
   --  counter-clockwise then applying the original transformation.
   --  Since: gtk+ 1.6
   --  "degrees": degrees to rotate counter-clockwise

   procedure Scale
      (Self    : in out Pango_Matrix;
       Scale_X : Gdouble;
       Scale_Y : Gdouble);
   pragma Import (C, Scale, "pango_matrix_scale");
   --  Changes the transformation represented by Matrix to be the
   --  transformation given by first scaling by Sx in the X direction and Sy in
   --  the Y direction then applying the original transformation.
   --  Since: gtk+ 1.6
   --  "scale_x": amount to scale by in X direction
   --  "scale_y": amount to scale by in Y direction

   procedure Transform_Distance
      (Self : Pango_Matrix;
       Dx   : in out Gdouble;
       Dy   : in out Gdouble);
   pragma Import (C, Transform_Distance, "pango_matrix_transform_distance");
   --  Transforms the distance vector (Dx,Dy) by Matrix. This is similar to
   --  Pango.Matrix.Transform_Point except that the translation components of
   --  the transformation are ignored. The calculation of the returned vector
   --  is as follows:
   --    dx2 = dx1 * xx + dy1 * xy;
   --    dy2 = dx1 * yx + dy1 * yy;
   --  Affine transformations are position invariant, so the same vector
   --  always transforms to the same vector. If (X1,Y1) transforms to (X2,Y2)
   --  then (X1+Dx1,Y1+Dy1) will transform to (X1+Dx2,Y1+Dy2) for all values of
   --  X1 and X2.
   --  Since: gtk+ 1.16
   --  "dx": in/out X component of a distance vector
   --  "dy": in/out Y component of a distance vector

   procedure Transform_Pixel_Rectangle
      (Self : Pango_Matrix;
       Rect : in out Pango_Rectangle);
   pragma Import (C, Transform_Pixel_Rectangle, "pango_matrix_transform_pixel_rectangle");
   --  First transforms the Rect using Matrix, then calculates the bounding
   --  box of the transformed rectangle. The rectangle should be in device
   --  units (pixels).
   --  This function is useful for example when you want to draw a rotated
   --  Pangolayout to an image buffer, and want to know how large the image
   --  should be and how much you should shift the layout when rendering.
   --  For better accuracy, you should use Pango.Matrix.Transform_Rectangle on
   --  original rectangle in Pango units and convert to pixels afterward using
   --  pango_extents_to_pixels's first argument.
   --  Since: gtk+ 1.16
   --  "rect": in/out bounding box in device units, or null

   procedure Transform_Point
      (Self : Pango_Matrix;
       X    : in out Gdouble;
       Y    : in out Gdouble);
   pragma Import (C, Transform_Point, "pango_matrix_transform_point");
   --  Transforms the point (X, Y) by Matrix.
   --  Since: gtk+ 1.16
   --  "x": in/out X position
   --  "y": in/out Y position

   procedure Transform_Rectangle
      (Self : Pango_Matrix;
       Rect : in out Pango_Rectangle);
   pragma Import (C, Transform_Rectangle, "pango_matrix_transform_rectangle");
   --  First transforms Rect using Matrix, then calculates the bounding box of
   --  the transformed rectangle. The rectangle should be in Pango units.
   --  This function is useful for example when you want to draw a rotated
   --  Pangolayout to an image buffer, and want to know how large the image
   --  should be and how much you should shift the layout when rendering.
   --  If you have a rectangle in device units (pixels), use
   --  Pango.Matrix.Transform_Pixel_Rectangle.
   --  If you have the rectangle in Pango units and want to convert to
   --  transformed pixel bounding box, it is more accurate to transform it
   --  first (using this function) and pass the result to
   --  pango_extents_to_pixels, first argument, for an inclusive rounded
   --  rectangle. However, there are valid reasons that you may want to convert
   --  to pixels first and then transform, for example when the transformed
   --  coordinates may overflow in Pango units (large matrix translation for
   --  example).
   --  Since: gtk+ 1.16
   --  "rect": in/out bounding box in Pango units, or null

   procedure Translate
      (Self : in out Pango_Matrix;
       Tx   : Gdouble;
       Ty   : Gdouble);
   pragma Import (C, Translate, "pango_matrix_translate");
   --  Changes the transformation represented by Matrix to be the
   --  transformation given by first translating by (Tx, Ty) then applying the
   --  original transformation.
   --  Since: gtk+ 1.6
   --  "tx": amount to translate in the X direction
   --  "ty": amount to translate in the Y direction

end Pango.Matrix;
