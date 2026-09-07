------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  A structure capable of holding a 4x4 matrix.
--
--  The contents of the Graphene.Matrix.Graphene_Matrix_T structure are
--  private and should never be accessed directly.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                use Glib;
with Graphene.Box;        use Graphene.Box;
with Graphene.Config;     use Graphene.Config;
limited with Graphene.Euler;
with Graphene.Point;      use Graphene.Point;
with Graphene.Point3d;    use Graphene.Point3d;
with Graphene.Quad;       use Graphene.Quad;
with Graphene.Quaternion; use Graphene.Quaternion;
with Graphene.Ray;        use Graphene.Ray;
with Graphene.Rect;       use Graphene.Rect;
with Graphene.Sphere;     use Graphene.Sphere;
with Graphene.Vec3;       use Graphene.Vec3;
with Graphene.Vec4;       use Graphene.Vec4;
with Interfaces.C;        use Interfaces.C;

package Graphene.Matrix is

   type Graphene_Matrix_T is record
      Value : Graphene.Config.Graphene_Simd4x4f;
   end record;
   pragma Convention (C, Graphene_Matrix_T);
   --  A structure capable of holding a 4x4 matrix.
   --
   --  The contents of the Graphene.Matrix.Graphene_Matrix_T structure are
   --  private and should never be accessed directly.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_matrix_get_type");

   -------------
   -- Methods --
   -------------

   function Decompose
      (Self        : not null access Graphene_Matrix_T;
       Translate   : not null access Graphene.Vec3.Graphene_Vec3_T;
       Scale       : not null access Graphene.Vec3.Graphene_Vec3_T;
       Rotate      : not null access Graphene.Quaternion.Graphene_Quaternion_T;
       Shear       : not null access Graphene.Vec3.Graphene_Vec3_T;
       Perspective : not null access Graphene.Vec4.Graphene_Vec4_T)
       return Boolean;
   --  Decomposes a transformation matrix into its component transformations.
   --  The algorithm for decomposing a matrix is taken from the [CSS3
   --  Transforms specification](http://dev.w3.org/csswg/css-transforms/);
   --  specifically, the decomposition code is based on the equivalent code
   --  published in "Graphics Gems II", edited by Jim Arvo, and [available
   --  online](http://web.archive.org/web/20150512160205/http://tog.acm.org/resources/GraphicsGems/gemsii/unmatrix.c).
   --  @param Translate the translation vector
   --  @param Scale the scale vector
   --  @param Rotate the rotation quaternion
   --  @param Shear the shear vector
   --  @param Perspective the perspective vector
   --  @return `true` if the matrix could be decomposed

   function Determinant
      (Self : not null access Graphene_Matrix_T) return Interfaces.C.C_float;
   pragma Import (C, Determinant, "graphene_matrix_determinant");
   --  Computes the determinant of the given matrix.
   --  Since: gtk+ 1.0
   --  @return the value of the determinant

   function Equal
      (Self : not null access Graphene_Matrix_T;
       B    : not null access Graphene_Matrix_T) return Boolean;
   --  Checks whether the two given Graphene.Matrix.Graphene_Matrix_T matrices
   --  are equal.
   --  Since: gtk+ 1.10
   --  @param B a Graphene.Matrix.Graphene_Matrix_T
   --  @return `true` if the two matrices are equal, and `false` otherwise

   function Equal_Fast
      (Self : not null access Graphene_Matrix_T;
       B    : not null access Graphene_Matrix_T) return Boolean;
   --  Checks whether the two given Graphene.Matrix.Graphene_Matrix_T matrices
   --  are byte-by-byte equal.
   --  While this function is faster than Graphene.Matrix.Equal, it can also
   --  return false negatives, so it should be used in conjuction with either
   --  Graphene.Matrix.Equal or Graphene.Matrix.Near. For instance:
   --
   --     if (graphene_matrix_equal_fast (a, b))
   --       {
   --         // matrices are definitely the same
   --       }
   --     else
   --       {
   --         if (graphene_matrix_equal (a, b))
   --           // matrices contain the same values within an epsilon of FLT_EPSILON
   --         else if (graphene_matrix_near (a, b, 0.0001))
   --           // matrices contain the same values within an epsilon of 0.0001
   --         else
   --           // matrices are not equal
   --       }
   --
   --  Since: gtk+ 1.10
   --  @param B a Graphene.Matrix.Graphene_Matrix_T
   --  @return `true` if the matrices are equal. and `false` otherwise

   procedure Free (Self : not null access Graphene_Matrix_T);
   pragma Import (C, Free, "graphene_matrix_free");
   --  Frees the resources allocated by graphene_matrix_alloc.
   --  Since: gtk+ 1.0

   procedure Get_Row
      (Self  : not null access Graphene_Matrix_T;
       Index : Guint;
       Res   : not null access Graphene.Vec4.Graphene_Vec4_T);
   pragma Import (C, Get_Row, "graphene_matrix_get_row");
   --  Retrieves the given row vector at Index_ inside a matrix.
   --  Since: gtk+ 1.0
   --  @param Index the index of the row vector, between 0 and 3
   --  @param Res return location for the Graphene.Vec4.Graphene_Vec4_T that
   --  is used to store the row vector

   function Get_Value
      (Self : not null access Graphene_Matrix_T;
       Row  : Guint;
       Col  : Guint) return Interfaces.C.C_float;
   pragma Import (C, Get_Value, "graphene_matrix_get_value");
   --  Retrieves the value at the given Row and Col index.
   --  Since: gtk+ 1.0
   --  @param Row the row index
   --  @param Col the column index
   --  @return the value at the given indices

   function Get_X_Scale
      (Self : not null access Graphene_Matrix_T) return Interfaces.C.C_float;
   pragma Import (C, Get_X_Scale, "graphene_matrix_get_x_scale");
   --  Retrieves the scaling factor on the X axis in M.
   --  Since: gtk+ 1.0
   --  @return the value of the scaling factor

   function Get_X_Translation
      (Self : not null access Graphene_Matrix_T) return Interfaces.C.C_float;
   pragma Import (C, Get_X_Translation, "graphene_matrix_get_x_translation");
   --  Retrieves the translation component on the X axis from M.
   --  Since: gtk+ 1.10
   --  @return the translation component

   function Get_Y_Scale
      (Self : not null access Graphene_Matrix_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Y_Scale, "graphene_matrix_get_y_scale");
   --  Retrieves the scaling factor on the Y axis in M.
   --  Since: gtk+ 1.0
   --  @return the value of the scaling factor

   function Get_Y_Translation
      (Self : not null access Graphene_Matrix_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Y_Translation, "graphene_matrix_get_y_translation");
   --  Retrieves the translation component on the Y axis from M.
   --  Since: gtk+ 1.10
   --  @return the translation component

   function Get_Z_Scale
      (Self : not null access Graphene_Matrix_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Z_Scale, "graphene_matrix_get_z_scale");
   --  Retrieves the scaling factor on the Z axis in M.
   --  Since: gtk+ 1.0
   --  @return the value of the scaling factor

   function Get_Z_Translation
      (Self : not null access Graphene_Matrix_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Z_Translation, "graphene_matrix_get_z_translation");
   --  Retrieves the translation component on the Z axis from M.
   --  Since: gtk+ 1.10
   --  @return the translation component

   function Init_From_2D
      (Self : not null access Graphene_Matrix_T;
       Xx   : Gdouble;
       Yx   : Gdouble;
       Xy   : Gdouble;
       Yy   : Gdouble;
       X_0  : Gdouble;
       Y_0  : Gdouble) return access Graphene_Matrix_T;
   pragma Import (C, Init_From_2D, "graphene_matrix_init_from_2d");
   --  Initializes a Graphene.Matrix.Graphene_Matrix_T from the values of an
   --  affine transformation matrix.
   --  The arguments map to the following matrix layout:
   --
   --     ⎛ xx  yx ⎞   ⎛  a   b  0 ⎞
   --     ⎜ xy  yy ⎟ = ⎜  c   d  0 ⎟
   --     ⎝ x0  y0 ⎠   ⎝ tx  ty  1 ⎠
   --
   --  This function can be used to convert between an affine matrix type from
   --  other libraries and a Graphene.Matrix.Graphene_Matrix_T.
   --  Since: gtk+ 1.0
   --  @param Xx the xx member
   --  @param Yx the yx member
   --  @param Xy the xy member
   --  @param Yy the yy member
   --  @param X_0 the x0 member
   --  @param Y_0 the y0 member
   --  @return the initialized matrix

   function Init_From_Matrix
      (Self : not null access Graphene_Matrix_T;
       Src  : not null access Graphene_Matrix_T)
       return access Graphene_Matrix_T;
   pragma Import (C, Init_From_Matrix, "graphene_matrix_init_from_matrix");
   --  Initializes a Graphene.Matrix.Graphene_Matrix_T using the values of the
   --  given matrix.
   --  Since: gtk+ 1.0
   --  @param Src a Graphene.Matrix.Graphene_Matrix_T
   --  @return the initialized matrix

   function Init_From_Vec4
      (Self : not null access Graphene_Matrix_T;
       V0   : not null access Graphene.Vec4.Graphene_Vec4_T;
       V1   : not null access Graphene.Vec4.Graphene_Vec4_T;
       V2   : not null access Graphene.Vec4.Graphene_Vec4_T;
       V3   : not null access Graphene.Vec4.Graphene_Vec4_T)
       return access Graphene_Matrix_T;
   pragma Import (C, Init_From_Vec4, "graphene_matrix_init_from_vec4");
   --  Initializes a Graphene.Matrix.Graphene_Matrix_T with the given four row
   --  vectors.
   --  Since: gtk+ 1.0
   --  @param V0 the first row vector
   --  @param V1 the second row vector
   --  @param V2 the third row vector
   --  @param V3 the fourth row vector
   --  @return the initialized matrix

   function Init_Frustum
      (Self   : not null access Graphene_Matrix_T;
       Left   : Interfaces.C.C_float;
       Right  : Interfaces.C.C_float;
       Bottom : Interfaces.C.C_float;
       Top    : Interfaces.C.C_float;
       Z_Near : Interfaces.C.C_float;
       Z_Far  : Interfaces.C.C_float) return access Graphene_Matrix_T;
   pragma Import (C, Init_Frustum, "graphene_matrix_init_frustum");
   --  Initializes a Graphene.Matrix.Graphene_Matrix_T compatible with
   --  Graphene.Frustum.Graphene_Frustum_T.
   --  See also: Graphene.Frustum.Init_From_Matrix
   --  Since: gtk+ 1.2
   --  @param Left distance of the left clipping plane
   --  @param Right distance of the right clipping plane
   --  @param Bottom distance of the bottom clipping plane
   --  @param Top distance of the top clipping plane
   --  @param Z_Near distance of the near clipping plane
   --  @param Z_Far distance of the far clipping plane
   --  @return the initialized matrix

   function Init_Identity
      (Self : not null access Graphene_Matrix_T)
       return access Graphene_Matrix_T;
   pragma Import (C, Init_Identity, "graphene_matrix_init_identity");
   --  Initializes a Graphene.Matrix.Graphene_Matrix_T with the identity
   --  matrix.
   --  Since: gtk+ 1.0
   --  @return the initialized matrix

   function Init_Look_At
      (Self   : not null access Graphene_Matrix_T;
       Eye    : not null access Graphene.Vec3.Graphene_Vec3_T;
       Center : not null access Graphene.Vec3.Graphene_Vec3_T;
       Up     : not null access Graphene.Vec3.Graphene_Vec3_T)
       return access Graphene_Matrix_T;
   pragma Import (C, Init_Look_At, "graphene_matrix_init_look_at");
   --  Initializes a Graphene.Matrix.Graphene_Matrix_T so that it positions
   --  the "camera" at the given Eye coordinates towards an object at the
   --  Center coordinates. The top of the camera is aligned to the direction of
   --  the Up vector.
   --  Before the transform, the camera is assumed to be placed at the origin,
   --  looking towards the negative Z axis, with the top side of the camera
   --  facing in the direction of the Y axis and the right side in the
   --  direction of the X axis.
   --  In theory, one could use M to transform a model of such a camera into
   --  world-space. However, it is more common to use the inverse of M to
   --  transform another object from world coordinates to the view coordinates
   --  of the camera. Typically you would then apply the camera projection
   --  transform to get from view to screen coordinates.
   --  Since: gtk+ 1.0
   --  @param Eye the vector describing the position to look from
   --  @param Center the vector describing the position to look at
   --  @param Up the vector describing the world's upward direction; usually,
   --  this is the Graphene.Vec3.Y_Axis vector
   --  @return the initialized matrix

   function Init_Ortho
      (Self   : not null access Graphene_Matrix_T;
       Left   : Interfaces.C.C_float;
       Right  : Interfaces.C.C_float;
       Top    : Interfaces.C.C_float;
       Bottom : Interfaces.C.C_float;
       Z_Near : Interfaces.C.C_float;
       Z_Far  : Interfaces.C.C_float) return access Graphene_Matrix_T;
   pragma Import (C, Init_Ortho, "graphene_matrix_init_ortho");
   --  Initializes a Graphene.Matrix.Graphene_Matrix_T with an orthographic
   --  projection.
   --  Since: gtk+ 1.0
   --  @param Left the left edge of the clipping plane
   --  @param Right the right edge of the clipping plane
   --  @param Top the top edge of the clipping plane
   --  @param Bottom the bottom edge of the clipping plane
   --  @param Z_Near the distance of the near clipping plane
   --  @param Z_Far the distance of the far clipping plane
   --  @return the initialized matrix

   function Init_Perspective
      (Self   : not null access Graphene_Matrix_T;
       Fovy   : Interfaces.C.C_float;
       Aspect : Interfaces.C.C_float;
       Z_Near : Interfaces.C.C_float;
       Z_Far  : Interfaces.C.C_float) return access Graphene_Matrix_T;
   pragma Import (C, Init_Perspective, "graphene_matrix_init_perspective");
   --  Initializes a Graphene.Matrix.Graphene_Matrix_T with a perspective
   --  projection.
   --  Since: gtk+ 1.0
   --  @param Fovy the field of view angle, in degrees
   --  @param Aspect the aspect value
   --  @param Z_Near the near Z plane
   --  @param Z_Far the far Z plane
   --  @return the initialized matrix

   function Init_Rotate
      (Self  : not null access Graphene_Matrix_T;
       Angle : Interfaces.C.C_float;
       Axis  : not null access Graphene.Vec3.Graphene_Vec3_T)
       return access Graphene_Matrix_T;
   pragma Import (C, Init_Rotate, "graphene_matrix_init_rotate");
   --  Initializes M to represent a rotation of Angle degrees on the axis
   --  represented by the Axis vector.
   --  Since: gtk+ 1.0
   --  @param Angle the rotation angle, in degrees
   --  @param Axis the axis vector as a Graphene.Vec3.Graphene_Vec3_T
   --  @return the initialized matrix

   function Init_Scale
      (Self : not null access Graphene_Matrix_T;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float;
       Z    : Interfaces.C.C_float) return access Graphene_Matrix_T;
   pragma Import (C, Init_Scale, "graphene_matrix_init_scale");
   --  Initializes a Graphene.Matrix.Graphene_Matrix_T with the given scaling
   --  factors.
   --  Since: gtk+ 1.0
   --  @param X the scale factor on the X axis
   --  @param Y the scale factor on the Y axis
   --  @param Z the scale factor on the Z axis
   --  @return the initialized matrix

   function Init_Skew
      (Self   : not null access Graphene_Matrix_T;
       X_Skew : Interfaces.C.C_float;
       Y_Skew : Interfaces.C.C_float) return access Graphene_Matrix_T;
   pragma Import (C, Init_Skew, "graphene_matrix_init_skew");
   --  Initializes a Graphene.Matrix.Graphene_Matrix_T with a skew
   --  transformation with the given factors.
   --  Since: gtk+ 1.0
   --  @param X_Skew skew factor, in radians, on the X axis
   --  @param Y_Skew skew factor, in radians, on the Y axis
   --  @return the initialized matrix

   function Init_Translate
      (Self : not null access Graphene_Matrix_T;
       P    : not null access Graphene.Point3d.Graphene_Point3D_T)
       return access Graphene_Matrix_T;
   pragma Import (C, Init_Translate, "graphene_matrix_init_translate");
   --  Initializes a Graphene.Matrix.Graphene_Matrix_T with a translation to
   --  the given coordinates.
   --  Since: gtk+ 1.0
   --  @param P the translation coordinates
   --  @return the initialized matrix

   procedure Interpolate
      (Self   : not null access Graphene_Matrix_T;
       B      : not null access Graphene_Matrix_T;
       Factor : Gdouble;
       Res    : not null access Graphene_Matrix_T);
   pragma Import (C, Interpolate, "graphene_matrix_interpolate");
   --  Linearly interpolates the two given Graphene.Matrix.Graphene_Matrix_T
   --  by interpolating the decomposed transformations separately.
   --  If either matrix cannot be reduced to their transformations then the
   --  interpolation cannot be performed, and this function will return an
   --  identity matrix.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Matrix.Graphene_Matrix_T
   --  @param Factor the linear interpolation factor
   --  @param Res return location for the interpolated matrix

   function Inverse
      (Self : not null access Graphene_Matrix_T;
       Res  : not null access Graphene_Matrix_T) return Boolean;
   --  Inverts the given matrix.
   --  Since: gtk+ 1.0
   --  @param Res return location for the inverse matrix
   --  @return `true` if the matrix is invertible

   function Is_2D (Self : not null access Graphene_Matrix_T) return Boolean;
   --  Checks whether the given Graphene.Matrix.Graphene_Matrix_T is
   --  compatible with an a 2D affine transformation matrix.
   --  Since: gtk+ 1.0
   --  @return `true` if the matrix is compatible with an affine
   --  transformation matrix

   function Is_Backface_Visible
      (Self : not null access Graphene_Matrix_T) return Boolean;
   --  Checks whether a Graphene.Matrix.Graphene_Matrix_T has a visible back
   --  face.
   --  Since: gtk+ 1.0
   --  @return `true` if the back face of the matrix is visible

   function Is_Identity
      (Self : not null access Graphene_Matrix_T) return Boolean;
   --  Checks whether the given Graphene.Matrix.Graphene_Matrix_T is the
   --  identity matrix.
   --  Since: gtk+ 1.0
   --  @return `true` if the matrix is the identity matrix

   function Is_Singular
      (Self : not null access Graphene_Matrix_T) return Boolean;
   --  Checks whether a matrix is singular.
   --  Since: gtk+ 1.0
   --  @return `true` if the matrix is singular

   procedure Multiply
      (Self : not null access Graphene_Matrix_T;
       B    : not null access Graphene_Matrix_T;
       Res  : not null access Graphene_Matrix_T);
   pragma Import (C, Multiply, "graphene_matrix_multiply");
   --  Multiplies two Graphene.Matrix.Graphene_Matrix_T.
   --  Matrix multiplication is not commutative in general; the order of the
   --  factors matters. The product of this multiplication is (A × B)
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Matrix.Graphene_Matrix_T
   --  @param Res return location for the matrix result

   function Near
      (Self    : not null access Graphene_Matrix_T;
       B       : not null access Graphene_Matrix_T;
       Epsilon : Interfaces.C.C_float) return Boolean;
   --  Compares the two given Graphene.Matrix.Graphene_Matrix_T matrices and
   --  checks whether their values are within the given Epsilon of each other.
   --  Since: gtk+ 1.10
   --  @param B a Graphene.Matrix.Graphene_Matrix_T
   --  @param Epsilon the threshold between the two matrices
   --  @return `true` if the two matrices are near each other, and `false`
   --  otherwise

   procedure Normalize
      (Self : not null access Graphene_Matrix_T;
       Res  : not null access Graphene_Matrix_T);
   pragma Import (C, Normalize, "graphene_matrix_normalize");
   --  Normalizes the given Graphene.Matrix.Graphene_Matrix_T.
   --  Since: gtk+ 1.0
   --  @param Res return location for the normalized matrix

   procedure Perspective
      (Self  : not null access Graphene_Matrix_T;
       Depth : Interfaces.C.C_float;
       Res   : not null access Graphene_Matrix_T);
   pragma Import (C, Perspective, "graphene_matrix_perspective");
   --  Applies a perspective of Depth to the matrix.
   --  Since: gtk+ 1.0
   --  @param Depth the depth of the perspective
   --  @param Res return location for the perspective matrix

   procedure Print (Self : not null access Graphene_Matrix_T);
   pragma Import (C, Print, "graphene_matrix_print");
   --  Prints the contents of a matrix to the standard error stream.
   --  This function is only useful for debugging; there are no guarantees
   --  made on the format of the output.
   --  Since: gtk+ 1.0

   procedure Project_Point
      (Self : not null access Graphene_Matrix_T;
       P    : not null access Graphene.Point.Graphene_Point_T;
       Res  : not null access Graphene.Point.Graphene_Point_T);
   pragma Import (C, Project_Point, "graphene_matrix_project_point");
   --  Projects a Graphene.Point.Graphene_Point_T using the matrix M.
   --  Since: gtk+ 1.0
   --  @param P a Graphene.Point.Graphene_Point_T
   --  @param Res return location for the projected point

   procedure Project_Rect
      (Self : not null access Graphene_Matrix_T;
       R    : not null access Graphene.Rect.Graphene_Rect_T;
       Res  : not null access Graphene.Quad.Graphene_Quad_T);
   pragma Import (C, Project_Rect, "graphene_matrix_project_rect");
   --  Projects all corners of a Graphene.Rect.Graphene_Rect_T using the given
   --  matrix.
   --  See also: Graphene.Matrix.Project_Point
   --  Since: gtk+ 1.2
   --  @param R a Graphene.Rect.Graphene_Rect_T
   --  @param Res return location for the projected rectangle

   procedure Project_Rect_Bounds
      (Self : not null access Graphene_Matrix_T;
       R    : not null access Graphene.Rect.Graphene_Rect_T;
       Res  : not null access Graphene.Rect.Graphene_Rect_T);
   pragma Import (C, Project_Rect_Bounds, "graphene_matrix_project_rect_bounds");
   --  Projects a Graphene.Rect.Graphene_Rect_T using the given matrix.
   --  The resulting rectangle is the axis aligned bounding rectangle capable
   --  of fully containing the projected rectangle.
   --  Since: gtk+ 1.0
   --  @param R a Graphene.Rect.Graphene_Rect_T
   --  @param Res return location for the projected rectangle

   procedure Rotate
      (Self  : not null access Graphene_Matrix_T;
       Angle : Interfaces.C.C_float;
       Axis  : not null access Graphene.Vec3.Graphene_Vec3_T);
   pragma Import (C, Rotate, "graphene_matrix_rotate");
   --  Adds a rotation transformation to M, using the given Angle and Axis
   --  vector.
   --  This is the equivalent of calling Graphene.Matrix.Init_Rotate and then
   --  multiplying the matrix M with the rotation matrix.
   --  Since: gtk+ 1.0
   --  @param Angle the rotation angle, in degrees
   --  @param Axis the rotation axis, as a Graphene.Vec3.Graphene_Vec3_T

   procedure Rotate_Euler
      (Self : not null access Graphene_Matrix_T;
       E    : not null access Graphene.Euler.Graphene_Euler_T);
   pragma Import (C, Rotate_Euler, "graphene_matrix_rotate_euler");
   --  Adds a rotation transformation to M, using the given
   --  Graphene.Euler.Graphene_Euler_T.
   --  Since: gtk+ 1.2
   --  @param E a rotation described by a Graphene.Euler.Graphene_Euler_T

   procedure Rotate_Quaternion
      (Self : not null access Graphene_Matrix_T;
       Q    : not null access Graphene.Quaternion.Graphene_Quaternion_T);
   pragma Import (C, Rotate_Quaternion, "graphene_matrix_rotate_quaternion");
   --  Adds a rotation transformation to M, using the given
   --  Graphene.Quaternion.Graphene_Quaternion_T.
   --  This is the equivalent of calling Graphene.Quaternion.To_Matrix and
   --  then multiplying M with the rotation matrix.
   --  Since: gtk+ 1.2
   --  @param Q a rotation described by a
   --  Graphene.Quaternion.Graphene_Quaternion_T

   procedure Rotate_X
      (Self  : not null access Graphene_Matrix_T;
       Angle : Interfaces.C.C_float);
   pragma Import (C, Rotate_X, "graphene_matrix_rotate_x");
   --  Adds a rotation transformation around the X axis to M, using the given
   --  Angle.
   --  See also: Graphene.Matrix.Rotate
   --  Since: gtk+ 1.0
   --  @param Angle the rotation angle, in degrees

   procedure Rotate_Y
      (Self  : not null access Graphene_Matrix_T;
       Angle : Interfaces.C.C_float);
   pragma Import (C, Rotate_Y, "graphene_matrix_rotate_y");
   --  Adds a rotation transformation around the Y axis to M, using the given
   --  Angle.
   --  See also: Graphene.Matrix.Rotate
   --  Since: gtk+ 1.0
   --  @param Angle the rotation angle, in degrees

   procedure Rotate_Z
      (Self  : not null access Graphene_Matrix_T;
       Angle : Interfaces.C.C_float);
   pragma Import (C, Rotate_Z, "graphene_matrix_rotate_z");
   --  Adds a rotation transformation around the Z axis to M, using the given
   --  Angle.
   --  See also: Graphene.Matrix.Rotate
   --  Since: gtk+ 1.0
   --  @param Angle the rotation angle, in degrees

   procedure Scale
      (Self     : not null access Graphene_Matrix_T;
       Factor_X : Interfaces.C.C_float;
       Factor_Y : Interfaces.C.C_float;
       Factor_Z : Interfaces.C.C_float);
   pragma Import (C, Scale, "graphene_matrix_scale");
   --  Adds a scaling transformation to M, using the three given factors.
   --  This is the equivalent of calling Graphene.Matrix.Init_Scale and then
   --  multiplying the matrix M with the scale matrix.
   --  Since: gtk+ 1.0
   --  @param Factor_X scaling factor on the X axis
   --  @param Factor_Y scaling factor on the Y axis
   --  @param Factor_Z scaling factor on the Z axis

   procedure Skew_Xy
      (Self   : not null access Graphene_Matrix_T;
       Factor : Interfaces.C.C_float);
   pragma Import (C, Skew_Xy, "graphene_matrix_skew_xy");
   --  Adds a skew of Factor on the X and Y axis to the given matrix.
   --  Since: gtk+ 1.0
   --  @param Factor skew factor

   procedure Skew_Xz
      (Self   : not null access Graphene_Matrix_T;
       Factor : Interfaces.C.C_float);
   pragma Import (C, Skew_Xz, "graphene_matrix_skew_xz");
   --  Adds a skew of Factor on the X and Z axis to the given matrix.
   --  Since: gtk+ 1.0
   --  @param Factor skew factor

   procedure Skew_Yz
      (Self   : not null access Graphene_Matrix_T;
       Factor : Interfaces.C.C_float);
   pragma Import (C, Skew_Yz, "graphene_matrix_skew_yz");
   --  Adds a skew of Factor on the Y and Z axis to the given matrix.
   --  Since: gtk+ 1.0
   --  @param Factor skew factor

   function To_2D
      (Self : not null access Graphene_Matrix_T;
       Xx   : out Gdouble;
       Yx   : out Gdouble;
       Xy   : out Gdouble;
       Yy   : out Gdouble;
       X_0  : out Gdouble;
       Y_0  : out Gdouble) return Boolean;
   --  Converts a Graphene.Matrix.Graphene_Matrix_T to an affine
   --  transformation matrix, if the given matrix is compatible.
   --  The returned values have the following layout:
   --
   --     ⎛ xx  yx ⎞   ⎛  a   b  0 ⎞
   --     ⎜ xy  yy ⎟ = ⎜  c   d  0 ⎟
   --     ⎝ x0  y0 ⎠   ⎝ tx  ty  1 ⎠
   --
   --  This function can be used to convert between a
   --  Graphene.Matrix.Graphene_Matrix_T and an affine matrix type from other
   --  libraries.
   --  Since: gtk+ 1.0
   --  @param Xx return location for the xx member
   --  @param Yx return location for the yx member
   --  @param Xy return location for the xy member
   --  @param Yy return location for the yy member
   --  @param X_0 return location for the x0 member
   --  @param Y_0 return location for the y0 member
   --  @return `true` if the matrix is compatible with an affine
   --  transformation matrix

   procedure Transform_Bounds
      (Self : not null access Graphene_Matrix_T;
       R    : not null access Graphene.Rect.Graphene_Rect_T;
       Res  : not null access Graphene.Rect.Graphene_Rect_T);
   pragma Import (C, Transform_Bounds, "graphene_matrix_transform_bounds");
   --  Transforms each corner of a Graphene.Rect.Graphene_Rect_T using the
   --  given matrix M.
   --  The result is the axis aligned bounding rectangle containing the
   --  coplanar quadrilateral.
   --  See also: Graphene.Matrix.Transform_Point
   --  Since: gtk+ 1.0
   --  @param R a Graphene.Rect.Graphene_Rect_T
   --  @param Res return location for the bounds of the transformed rectangle

   procedure Transform_Box
      (Self : not null access Graphene_Matrix_T;
       B    : not null access Graphene.Box.Graphene_Box_T;
       Res  : not null access Graphene.Box.Graphene_Box_T);
   pragma Import (C, Transform_Box, "graphene_matrix_transform_box");
   --  Transforms the vertices of a Graphene.Box.Graphene_Box_T using the
   --  given matrix M.
   --  The result is the axis aligned bounding box containing the transformed
   --  vertices.
   --  Since: gtk+ 1.2
   --  @param B a Graphene.Box.Graphene_Box_T
   --  @param Res return location for the bounds of the transformed box

   procedure Transform_Point
      (Self : not null access Graphene_Matrix_T;
       P    : not null access Graphene.Point.Graphene_Point_T;
       Res  : not null access Graphene.Point.Graphene_Point_T);
   pragma Import (C, Transform_Point, "graphene_matrix_transform_point");
   --  Transforms the given Graphene.Point.Graphene_Point_T using the matrix
   --  M.
   --  Unlike Graphene.Matrix.Transform_Vec3, this function will take into
   --  account the fourth row vector of the Graphene.Matrix.Graphene_Matrix_T
   --  when computing the dot product of each row vector of the matrix.
   --  See also: graphene_simd4x4f_point3_mul
   --  Since: gtk+ 1.0
   --  @param P a Graphene.Point.Graphene_Point_T
   --  @param Res return location for the transformed
   --  Graphene.Point.Graphene_Point_T

   procedure Transform_Point3D
      (Self : not null access Graphene_Matrix_T;
       P    : not null access Graphene.Point3d.Graphene_Point3D_T;
       Res  : not null access Graphene.Point3d.Graphene_Point3D_T);
   pragma Import (C, Transform_Point3D, "graphene_matrix_transform_point3d");
   --  Transforms the given Graphene.Point3d.Graphene_Point3D_T using the
   --  matrix M.
   --  Unlike Graphene.Matrix.Transform_Vec3, this function will take into
   --  account the fourth row vector of the Graphene.Matrix.Graphene_Matrix_T
   --  when computing the dot product of each row vector of the matrix.
   --  See also: graphene_simd4x4f_point3_mul
   --  Since: gtk+ 1.2
   --  @param P a Graphene.Point3d.Graphene_Point3D_T
   --  @param Res return location for the result

   procedure Transform_Ray
      (Self : not null access Graphene_Matrix_T;
       R    : not null access Graphene.Ray.Graphene_Ray_T;
       Res  : not null access Graphene.Ray.Graphene_Ray_T);
   pragma Import (C, Transform_Ray, "graphene_matrix_transform_ray");
   --  Transform a Graphene.Ray.Graphene_Ray_T using the given matrix M.
   --  Since: gtk+ 1.4
   --  @param R a Graphene.Ray.Graphene_Ray_T
   --  @param Res return location for the transformed ray

   procedure Transform_Rect
      (Self : not null access Graphene_Matrix_T;
       R    : not null access Graphene.Rect.Graphene_Rect_T;
       Res  : not null access Graphene.Quad.Graphene_Quad_T);
   pragma Import (C, Transform_Rect, "graphene_matrix_transform_rect");
   --  Transforms each corner of a Graphene.Rect.Graphene_Rect_T using the
   --  given matrix M.
   --  The result is a coplanar quadrilateral.
   --  See also: Graphene.Matrix.Transform_Point
   --  Since: gtk+ 1.0
   --  @param R a Graphene.Rect.Graphene_Rect_T
   --  @param Res return location for the transformed quad

   procedure Transform_Sphere
      (Self : not null access Graphene_Matrix_T;
       S    : not null access Graphene.Sphere.Graphene_Sphere_T;
       Res  : not null access Graphene.Sphere.Graphene_Sphere_T);
   pragma Import (C, Transform_Sphere, "graphene_matrix_transform_sphere");
   --  Transforms a Graphene.Sphere.Graphene_Sphere_T using the given matrix
   --  M. The result is the bounding sphere containing the transformed sphere.
   --  Since: gtk+ 1.2
   --  @param S a Graphene.Sphere.Graphene_Sphere_T
   --  @param Res return location for the bounds of the transformed sphere

   procedure Transform_Vec3
      (Self : not null access Graphene_Matrix_T;
       V    : not null access Graphene.Vec3.Graphene_Vec3_T;
       Res  : not null access Graphene.Vec3.Graphene_Vec3_T);
   pragma Import (C, Transform_Vec3, "graphene_matrix_transform_vec3");
   --  Transforms the given Graphene.Vec3.Graphene_Vec3_T using the matrix M.
   --  This function will multiply the X, Y, and Z row vectors of the matrix M
   --  with the corresponding components of the vector V. The W row vector will
   --  be ignored.
   --  See also: graphene_simd4x4f_vec3_mul
   --  Since: gtk+ 1.0
   --  @param V a Graphene.Vec3.Graphene_Vec3_T
   --  @param Res return location for a Graphene.Vec3.Graphene_Vec3_T

   procedure Transform_Vec4
      (Self : not null access Graphene_Matrix_T;
       V    : not null access Graphene.Vec4.Graphene_Vec4_T;
       Res  : not null access Graphene.Vec4.Graphene_Vec4_T);
   pragma Import (C, Transform_Vec4, "graphene_matrix_transform_vec4");
   --  Transforms the given Graphene.Vec4.Graphene_Vec4_T using the matrix M.
   --  See also: graphene_simd4x4f_vec4_mul
   --  Since: gtk+ 1.0
   --  @param V a Graphene.Vec4.Graphene_Vec4_T
   --  @param Res return location for a Graphene.Vec4.Graphene_Vec4_T

   procedure Translate
      (Self : not null access Graphene_Matrix_T;
       Pos  : not null access Graphene.Point3d.Graphene_Point3D_T);
   pragma Import (C, Translate, "graphene_matrix_translate");
   --  Adds a translation transformation to M using the coordinates of the
   --  given Graphene.Point3d.Graphene_Point3D_T.
   --  This is the equivalent of calling Graphene.Matrix.Init_Translate and
   --  then multiplying M with the translation matrix.
   --  Since: gtk+ 1.0
   --  @param Pos a Graphene.Point3d.Graphene_Point3D_T

   procedure Transpose
      (Self : not null access Graphene_Matrix_T;
       Res  : not null access Graphene_Matrix_T);
   pragma Import (C, Transpose, "graphene_matrix_transpose");
   --  Transposes the given matrix.
   --  Since: gtk+ 1.0
   --  @param Res return location for the transposed matrix

   procedure Unproject_Point3D
      (Self      : not null access Graphene_Matrix_T;
       Modelview : not null access Graphene_Matrix_T;
       Point     : not null access Graphene.Point3d.Graphene_Point3D_T;
       Res       : not null access Graphene.Point3d.Graphene_Point3D_T);
   pragma Import (C, Unproject_Point3D, "graphene_matrix_unproject_point3d");
   --  Unprojects the given Point using the Projection matrix and a Modelview
   --  matrix.
   --  Since: gtk+ 1.2
   --  @param Modelview a Graphene.Matrix.Graphene_Matrix_T for the modelview
   --  matrix; this is the inverse of the modelview used when projecting the
   --  point
   --  @param Point a Graphene.Point3d.Graphene_Point3D_T with the coordinates
   --  of the point
   --  @param Res return location for the unprojected point

   procedure Untransform_Bounds
      (Self   : not null access Graphene_Matrix_T;
       R      : not null access Graphene.Rect.Graphene_Rect_T;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T;
       Res    : not null access Graphene.Rect.Graphene_Rect_T);
   pragma Import (C, Untransform_Bounds, "graphene_matrix_untransform_bounds");
   --  Undoes the transformation on the corners of a
   --  Graphene.Rect.Graphene_Rect_T using the given matrix, within the given
   --  axis aligned rectangular Bounds.
   --  Since: gtk+ 1.0
   --  @param R a Graphene.Rect.Graphene_Rect_T
   --  @param Bounds the bounds of the transformation
   --  @param Res return location for the untransformed rectangle

   function Untransform_Point
      (Self   : not null access Graphene_Matrix_T;
       P      : not null access Graphene.Point.Graphene_Point_T;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T;
       Res    : not null access Graphene.Point.Graphene_Point_T)
       return Boolean;
   --  Undoes the transformation of a Graphene.Point.Graphene_Point_T using
   --  the given matrix, within the given axis aligned rectangular Bounds.
   --  Since: gtk+ 1.0
   --  @param P a Graphene.Point.Graphene_Point_T
   --  @param Bounds the bounds of the transformation
   --  @param Res return location for the untransformed point
   --  @return `true` if the point was successfully untransformed

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Graphene_Matrix_T) return Graphene_Matrix_T;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

end Graphene.Matrix;
