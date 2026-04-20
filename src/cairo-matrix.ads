------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2026, AdaCore                     --
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

--  Generic matrix operations.
--
--  <c_version>1.8.8</c_version>
--  <group>Cairo</group>

package Cairo.Matrix is

   procedure Init
     (Matrix : access Cairo_Matrix;
      Xx     : Gdouble;
      Yx     : Gdouble;
      Xy     : Gdouble;
      Yy     : Gdouble;
      X0     : Gdouble;
      Y0     : Gdouble);
   --  Sets matrix to be the affine transformation given by
   --  Xx, Yx, Xy, Yy, X0, Y0. The transformation is given
   --  by:
   --
   --     X_new = Xx * X + Xy * Y + X0;
   --     Y_new = Yx * X + Yy * Y + Y0;
   --
   --  @param Matrix a Cairo_Matrix
   --  @param Xx Xx component of the affine transformation
   --  @param Yx Yx component of the affine transformation
   --  @param Xy Xy component of the affine transformation
   --  @param Yy Yy component of the affine transformation
   --  @param X0 X translation component of the affine transformation
   --  @param Y0 Y translation component of the affine transformation

   procedure Init_Identity (Matrix : access Cairo_Matrix);
   --  Modifies matrix to be an identity transformation.
   --
   --  @param Matrix a Cairo_Matrix

   procedure Init_Translate
     (Matrix : access Cairo_Matrix;
      Tx     : Gdouble;
      Ty     : Gdouble);
   --  Initializes matrix to a transformation that translates by Tx and
   --  Ty in the X and Y dimensions, respectively.
   --
   --  @param Matrix a Cairo_Matrix
   --  @param Tx amount to translate in the X direction
   --  @param Ty amount to translate in the Y direction

   procedure Init_Scale
     (Matrix : access Cairo_Matrix;
      Sx     : Gdouble;
      Sy     : Gdouble);
   --  Initializes matrix to a transformation that scales by Sx and Sy
   --  in the X and Y dimensions, respectively.
   --
   --  @param Matrix a Cairo_Matrix
   --  @param Sx scale factor in the X direction
   --  @param Sy scale factor in the Y direction

   procedure Init_Rotate (Matrix : access Cairo_Matrix; Radians : Gdouble);
   --  Initialized matrix to a transformation that rotates by radians.
   --
   --  @param Matrix a Cairo_Matrix
   --  @param Radians angle of rotation, in Radians. The direction of rotation
   --  is defined such that positive angles rotate in the direction from the
   --  positive X axis toward the positive Y axis. With the default axis
   --  orientation of cairo, positive angles rotate in a clockwise direction.

   procedure Translate
     (Matrix : access Cairo_Matrix;
      Tx     : Gdouble;
      Ty     : Gdouble);
   --  Applies a translation by Tx, Ty to the transformation in
   --  matrix. The effect of the new transformation is to first translate
   --  the coordinates by Tx and Ty, then apply the original transformation
   --  to the coordinates.
   --
   --  @param Matrix a Cairo_Matrix
   --  @param Tx amount to translate in the X direction
   --  @param Ty amount to translate in the Y direction

   procedure Scale
     (Matrix : access Cairo_Matrix;
      Sx     : Gdouble;
      Sy     : Gdouble);
   --  Applies scaling by Sx, Sy to the transformation in matrix. The
   --  effect of the new transformation is to first scale the coordinates
   --  by Sx and Sy, then apply the original transformation to the coordinates.
   --
   --  @param Matrix a Cairo_Matrix
   --  @param Sx scale factor in the X direction
   --  @param Sy scale factor in the Y direction

   procedure Rotate (Matrix : access Cairo_Matrix; Radians : Gdouble);
   --  Applies rotation by radians to the transformation in matrix. The effect
   --  of the new transformation is to first rotate the coordinates by radians,
   --  then apply the original transformation to the coordinates.
   --
   --  @param Matrix a Cairo_Matrix
   --  @param Radians angle of rotation, in Radians. The direction of rotation
   --  is defined such that positive angles rotate in the direction from the
   --  positive X axis toward the positive Y axis. With the default axis
   --  orientation of cairo, positive angles rotate in a clockwise direction.

   function Invert (Matrix : access Cairo_Matrix) return Cairo_Status;
   --  Changes matrix to be the inverse of its original value. Not
   --  all transformation matrices have inverses; if the matrix
   --  collapses points together (it is "degenerate"),
   --  then it has no inverse and this function will fail.
   --
   --  @param Matrix a Cairo_Matrix
   --  @return If matrix has an inverse, modifies matrix to be the inverse
   --  matrix and returns Cairo_Status_Success. Otherwise, returns
   --  Cairo_Status_Invalid_Matrix.

   procedure Multiply
     (Result : access Cairo_Matrix;
      A      : access Cairo_Matrix;
      B      : access Cairo_Matrix);
   --  Multiplies the affine transformations in a and b together
   --  and stores the result in result. The effect of the resulting
   --  transformation is to first apply the transformation in a to the
   --  coordinates and then apply the transformation in b to the
   --  coordinates.
   --
   --  It is allowable for result to be identical to either a or b.
   --
   --  @param Result a Cairo_Matrix in which to store the Result
   --  @param A a Cairo_Matrix
   --  @param B a Cairo_Matrix

   procedure Transform_Distance
     (Matrix : access Cairo_Matrix;
      Dx     : access Gdouble;
      Dy     : access Gdouble);
   --  Transforms the distance vector (Dx,Dy) by matrix. This is
   --  similar to Cairo.Matrix.Transform_Point except that the translation
   --  components of the transformation are ignored. The calculation of
   --  the returned vector is as follows:
   --
   --     Dx2 = Dx1 * A + Dy1 * C;
   --     Dy2 = Dx1 * B + Dy1 * D;
   --
   --  Affine transformations are position invariant, so the same vector
   --  always transforms to the same vector. If (X1,Y1) transforms
   --  to (X2,Y2) then (X1+Dx1,Y1+Dy1) will transform to
   --  (X1+Dx2,Y1+Dy2) for all values of X1 and X2.
   --
   --  @param Matrix a Cairo_Matrix
   --  @param Dx X component of a distance vector. An in/out parameter
   --  @param Dy Y component of a distance vector. An in/out parameter

   procedure Transform_Point
     (Matrix : access Cairo_Matrix;
      X      : access Gdouble;
      Y      : access Gdouble);
   --  Transforms the point (X, Y) by matrix.
   --
   --  @param Matrix a Cairo_Matrix
   --  @param X X position. An in/out parameter
   --  @param Y Y position. An in/out parameter

private

   pragma Import (C, Init, "cairo_matrix_init");
   pragma Import (C, Init_Identity, "cairo_matrix_init_identity");
   pragma Import (C, Init_Translate, "cairo_matrix_init_translate");
   pragma Import (C, Init_Scale, "cairo_matrix_init_scale");
   pragma Import (C, Init_Rotate, "cairo_matrix_init_rotate");
   pragma Import (C, Translate, "cairo_matrix_translate");
   pragma Import (C, Scale, "cairo_matrix_scale");
   pragma Import (C, Rotate, "cairo_matrix_rotate");
   pragma Import (C, Invert, "cairo_matrix_invert");
   pragma Import (C, Multiply, "cairo_matrix_multiply");
   pragma Import (C, Transform_Distance, "cairo_matrix_transform_distance");
   pragma Import (C, Transform_Point, "cairo_matrix_transform_point");

end Cairo.Matrix;
