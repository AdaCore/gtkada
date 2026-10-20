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

--  Describes a 3D transform.
--
--  Unlike `graphene_matrix_t`, `GskTransform` retains the steps in how a
--  transform was constructed, and allows inspecting them. It is modeled after
--  the way CSS describes transforms.
--
--  `GskTransform` objects are immutable and cannot be changed after creation.
--  This means code can safely expose them as properties of objects without
--  having to worry about others changing them.
--
--  <gtkada_demo>create_fixed_layout.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.String;      use Glib.String;
with Graphene.Matrix;  use Graphene.Matrix;
with Graphene.Point;   use Graphene.Point;
with Graphene.Point3d; use Graphene.Point3d;
with Graphene.Rect;    use Graphene.Rect;
with Graphene.Vec3;    use Graphene.Vec3;
with Gsk.Enums;        use Gsk.Enums;
with Interfaces.C;     use Interfaces.C;

package Gsk.Transform is

   type Gsk_Transform is new Glib.C_Boxed with null record;
   Null_Gsk_Transform : constant Gsk_Transform;

   function From_Object (Object : System.Address) return Gsk_Transform;
   function From_Object_Free (B : access Gsk_Transform'Class) return Gsk_Transform;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gsk_Transform);
   --  Creates a new identity transform.
   --  This function is meant to be used by language bindings. For C code,
   --  this is equivalent to using `NULL`.

   function Gsk_Transform_New return Gsk_Transform;
   --  Creates a new identity transform.
   --  This function is meant to be used by language bindings. For C code,
   --  this is equivalent to using `NULL`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_transform_get_type");

   -------------
   -- Methods --
   -------------

   function Equal
      (Self   : Gsk_Transform;
       Second : Gsk_Transform) return Boolean;
   --  Checks two transforms for equality.
   --  @param Second the second transform
   --  @return true if the two transforms perform the same operation

   function Get_Category
      (Self : Gsk_Transform) return Gsk.Enums.Gsk_Transform_Category;
   --  Returns the category this transform belongs to.
   --  @return The category of the transform

   function Invert (Self : Gsk_Transform) return Gsk_Transform;
   --  Inverts the given transform.
   --  If Self is not invertible, `NULL` is returned. Note that inverting
   --  `NULL` also returns `NULL`, which is the correct inverse of `NULL`. If
   --  you need to differentiate between those cases, you should check Self is
   --  not `NULL` before calling this function.
   --  This function consumes Self. Use [methodGsk.Transform.ref] first if you
   --  want to keep it around.
   --  @param Self has transfer-ownership='full'.
   --  @return The inverted transform. Has transfer-ownership='full'.

   function Matrix
      (Self   : Gsk_Transform;
       Matrix : not null access Graphene.Matrix.Graphene_Matrix_T)
       return Gsk_Transform;
   --  Multiplies Next with the given Matrix.
   --  This function consumes Next. Use [methodGsk.Transform.ref] first if you
   --  want to keep it around.
   --  @param Self has transfer-ownership='full'.
   --  @param Matrix the matrix to multiply Next with
   --  @return The new transform. Has transfer-ownership='full'.

   function Matrix_2D
      (Self : Gsk_Transform;
       Xx   : Interfaces.C.C_float;
       Yx   : Interfaces.C.C_float;
       Xy   : Interfaces.C.C_float;
       Yy   : Interfaces.C.C_float;
       Dx   : Interfaces.C.C_float;
       Dy   : Interfaces.C.C_float) return Gsk_Transform;
   --  Multiplies Next with the matrix [ xx yx x0; xy yy y0; 0 0 1 ].
   --  The result of calling [methodGsk.Transform.to_2d] on the returned
   --  [structGsk.Transform] should match the input passed to this function.
   --  This function consumes Next. Use [methodGsk.Transform.ref] first if you
   --  want to keep it around.
   --  Since: gtk+ 4.20
   --  @param Self has transfer-ownership='full'.
   --  @param Xx the xx member
   --  @param Yx the yx member
   --  @param Xy the xy member
   --  @param Yy the yy member
   --  @param Dx the x0 member
   --  @param Dy the y0 member
   --  @return The new transform. Has transfer-ownership='full'.

   function Perspective
      (Self  : Gsk_Transform;
       Depth : Interfaces.C.C_float) return Gsk_Transform;
   --  Applies a perspective projection transform.
   --  This transform scales points in X and Y based on their Z value, scaling
   --  points with positive Z values away from the origin, and those with
   --  negative Z values towards the origin. Points on the z=0 plane are
   --  unchanged.
   --  This function consumes Next. Use [methodGsk.Transform.ref] first if you
   --  want to keep it around.
   --  @param Self has transfer-ownership='full'.
   --  @param Depth distance of the z=0 plane. Lower values give a more
   --  flattened pyramid and therefore a more pronounced perspective effect.
   --  @return The new transform. Has transfer-ownership='full'.

   procedure Print (Self : Gsk_Transform; String : Glib.String.Gstring);
   --  Converts the transform into a human-readable representation.
   --  The result of this function can later be parsed with
   --  [funcGsk.Transform.parse].
   --  @param String The string to print into

   function Ref (Self : Gsk_Transform) return Gsk_Transform;
   --  Acquires a reference on the given transform.
   --  @return the transform with an additional reference. Has
   --  transfer-ownership='none'.

   function Rotate
      (Self  : Gsk_Transform;
       Angle : Interfaces.C.C_float) return Gsk_Transform;
   --  Rotates Next by an angle around the Z axis.
   --  The rotation happens around the origin point of (0, 0).
   --  This function consumes Next. Use [methodGsk.Transform.ref] first if you
   --  want to keep it around.
   --  @param Self has transfer-ownership='full'.
   --  @param Angle the rotation angle, in degrees (clockwise)
   --  @return The new transform. Has transfer-ownership='full'.

   function Rotate_3D
      (Self  : Gsk_Transform;
       Angle : Interfaces.C.C_float;
       Axis  : not null access Graphene.Vec3.Graphene_Vec3_T)
       return Gsk_Transform;
   --  Rotates Next Angle degrees around Axis.
   --  For a rotation in 2D space, use [methodGsk.Transform.rotate]
   --  This function consumes Next. Use [methodGsk.Transform.ref] first if you
   --  want to keep it around.
   --  @param Self has transfer-ownership='full'.
   --  @param Angle the rotation angle, in degrees (clockwise)
   --  @param Axis The rotation axis
   --  @return The new transform. Has transfer-ownership='full'.

   function Scale
      (Self     : Gsk_Transform;
       Factor_X : Interfaces.C.C_float;
       Factor_Y : Interfaces.C.C_float) return Gsk_Transform;
   --  Scales Next in 2-dimensional space by the given factors.
   --  Use [methodGsk.Transform.scale_3d] to scale in all 3 dimensions.
   --  This function consumes Next. Use [methodGsk.Transform.ref] first if you
   --  want to keep it around.
   --  @param Self has transfer-ownership='full'.
   --  @param Factor_X scaling factor on the X axis
   --  @param Factor_Y scaling factor on the Y axis
   --  @return The new transform. Has transfer-ownership='full'.

   function Scale_3D
      (Self     : Gsk_Transform;
       Factor_X : Interfaces.C.C_float;
       Factor_Y : Interfaces.C.C_float;
       Factor_Z : Interfaces.C.C_float) return Gsk_Transform;
   --  Scales Next by the given factors.
   --  This function consumes Next. Use [methodGsk.Transform.ref] first if you
   --  want to keep it around.
   --  @param Self has transfer-ownership='full'.
   --  @param Factor_X scaling factor on the X axis
   --  @param Factor_Y scaling factor on the Y axis
   --  @param Factor_Z scaling factor on the Z axis
   --  @return The new transform. Has transfer-ownership='full'.

   function Skew
      (Self   : Gsk_Transform;
       Skew_X : Interfaces.C.C_float;
       Skew_Y : Interfaces.C.C_float) return Gsk_Transform;
   --  Applies a skew transform.
   --  This function consumes Next. Use [methodGsk.Transform.ref] first if you
   --  want to keep it around.
   --  Since: gtk+ 4.6
   --  @param Self has transfer-ownership='full'.
   --  @param Skew_X skew factor, in degrees, on the X axis
   --  @param Skew_Y skew factor, in degrees, on the Y axis
   --  @return The new transform. Has transfer-ownership='full'.

   procedure To_2D
      (Self   : Gsk_Transform;
       Out_Xx : out Interfaces.C.C_float;
       Out_Yx : out Interfaces.C.C_float;
       Out_Xy : out Interfaces.C.C_float;
       Out_Yy : out Interfaces.C.C_float;
       Out_Dx : out Interfaces.C.C_float;
       Out_Dy : out Interfaces.C.C_float);
   --  Converts a transform to a 2D transformation matrix.
   --  Self must be a 2D transformation. If you are not sure, use
   --  Gsk.Transform.Get_Category >= GSK_TRANSFORM_CATEGORY_2D
   --  to check.
   --  The returned values are a subset of the full 4x4 matrix that is
   --  computed by [methodGsk.Transform.to_matrix] and have the following
   --  layout:
   --  ``` | xx yx | | a b 0 | | xy yy | = | c d 0 | | dx dy | | tx ty 1 | ```
   --  This function can be used to convert between a `GskTransform` and a
   --  matrix type from other 2D drawing libraries, in particular Cairo.
   --  @param Out_Xx return location for the xx member
   --  @param Out_Yx return location for the yx member
   --  @param Out_Xy return location for the xy member
   --  @param Out_Yy return location for the yy member
   --  @param Out_Dx return location for the x0 member
   --  @param Out_Dy return location for the y0 member

   procedure To_2D_Components
      (Self        : Gsk_Transform;
       Out_Skew_X  : out Interfaces.C.C_float;
       Out_Skew_Y  : out Interfaces.C.C_float;
       Out_Scale_X : out Interfaces.C.C_float;
       Out_Scale_Y : out Interfaces.C.C_float;
       Out_Angle   : out Interfaces.C.C_float;
       Out_Dx      : out Interfaces.C.C_float;
       Out_Dy      : out Interfaces.C.C_float);
   --  Converts a transform to 2D transformation factors.
   --  To recreate an equivalent transform from the factors returned by this
   --  function, use
   --  gsk_transform_skew ( gsk_transform_scale ( gsk_transform_rotate (
   --  gsk_transform_translate (NULL, &GRAPHENE_POINT_INIT (dx, dy)), angle),
   --  scale_x, scale_y), skew_x, skew_y)
   --  Self must be a 2D transformation. If you are not sure, use
   --  Gsk.Transform.Get_Category >= GSK_TRANSFORM_CATEGORY_2D
   --  to check.
   --  Since: gtk+ 4.6
   --  @param Out_Skew_X return location for the skew factor in the x
   --  direction
   --  @param Out_Skew_Y return location for the skew factor in the y
   --  direction
   --  @param Out_Scale_X return location for the scale factor in the x
   --  direction
   --  @param Out_Scale_Y return location for the scale factor in the y
   --  direction
   --  @param Out_Angle return location for the rotation angle
   --  @param Out_Dx return location for the translation in the x direction
   --  @param Out_Dy return location for the translation in the y direction

   procedure To_Affine
      (Self        : Gsk_Transform;
       Out_Scale_X : out Interfaces.C.C_float;
       Out_Scale_Y : out Interfaces.C.C_float;
       Out_Dx      : out Interfaces.C.C_float;
       Out_Dy      : out Interfaces.C.C_float);
   --  Converts a transform to 2D affine transformation factors.
   --  To recreate an equivalent transform from the factors returned by this
   --  function, use
   --  gsk_transform_scale ( gsk_transform_translate ( NULL, &GRAPHENE_POINT_T
   --  (dx, dy)), sx, sy)
   --  Self must be a 2D affine transformation. If you are not sure, use
   --  Gsk.Transform.Get_Category >= GSK_TRANSFORM_CATEGORY_2D_AFFINE
   --  to check.
   --  @param Out_Scale_X return location for the scale factor in the x
   --  direction
   --  @param Out_Scale_Y return location for the scale factor in the y
   --  direction
   --  @param Out_Dx return location for the translation in the x direction
   --  @param Out_Dy return location for the translation in the y direction

   procedure To_Matrix
      (Self       : Gsk_Transform;
       Out_Matrix : not null access Graphene.Matrix.Graphene_Matrix_T);
   --  Computes the 4x4 matrix for the transform.
   --  The previous value of Out_Matrix will be ignored.
   --  @param Out_Matrix return location for the matrix

   function To_String (Self : Gsk_Transform) return UTF8_String;
   --  Converts the transform into a human-readable string.
   --  The resulting string can be parsed with [funcGsk.Transform.parse].
   --  This is a wrapper around [methodGsk.Transform.print].
   --  @return A new string for Self

   procedure To_Translate
      (Self   : Gsk_Transform;
       Out_Dx : out Interfaces.C.C_float;
       Out_Dy : out Interfaces.C.C_float);
   --  Converts a transform to a translation operation.
   --  Self must be a 2D transformation. If you are not sure, use
   --  Gsk.Transform.Get_Category >= GSK_TRANSFORM_CATEGORY_2D_TRANSLATE
   --  to check.
   --  @param Out_Dx return location for the translation in the x direction
   --  @param Out_Dy return location for the translation in the y direction

   function Transform
      (Self  : Gsk_Transform;
       Other : Gsk_Transform) return Gsk_Transform;
   --  Applies all the operations from Other to Next.
   --  This function consumes Next. Use [methodGsk.Transform.ref] first if you
   --  want to keep it around.
   --  @param Self has transfer-ownership='full'.
   --  @param Other transform to apply
   --  @return The new transform. Has transfer-ownership='full'.

   procedure Transform_Bounds
      (Self     : Gsk_Transform;
       Rect     : not null access Graphene.Rect.Graphene_Rect_T;
       Out_Rect : not null access Graphene.Rect.Graphene_Rect_T);
   --  Transforms a rectangle using the given transform.
   --  The result is the bounding box containing the coplanar quad.
   --  The input and output rect may point to the same rectangle.
   --  @param Rect the rectangle to transform
   --  @param Out_Rect return location for the bounds of the transformed
   --  rectangle

   procedure Transform_Point
      (Self      : Gsk_Transform;
       Point     : not null access Graphene.Point.Graphene_Point_T;
       Out_Point : not null access Graphene.Point.Graphene_Point_T);
   --  Transforms a point using the given transform.
   --  @param Point the point to transform
   --  @param Out_Point return location for the transformed point

   function Translate
      (Self  : Gsk_Transform;
       Point : not null access Graphene.Point.Graphene_Point_T)
       return Gsk_Transform;
   --  Translates Next in 2-dimensional space by Point.
   --  This function consumes Next. Use [methodGsk.Transform.ref] first if you
   --  want to keep it around.
   --  @param Self has transfer-ownership='full'.
   --  @param Point the point to translate the transform by
   --  @return The new transform. Has transfer-ownership='full'.

   function Translate_3D
      (Self  : Gsk_Transform;
       Point : not null access Graphene.Point3d.Graphene_Point3D_T)
       return Gsk_Transform;
   --  Translates Next by Point.
   --  This function consumes Next. Use [methodGsk.Transform.ref] first if you
   --  want to keep it around.
   --  @param Self has transfer-ownership='full'.
   --  @param Point the point to translate the transform by
   --  @return The new transform. Has transfer-ownership='full'.

   procedure Unref (Self : Gsk_Transform);
   --  Releases a reference on the given transform.
   --  If the reference was the last, the resources associated to the Self are
   --  freed.

   ---------------
   -- Functions --
   ---------------

   function Parse
      (String        : UTF8_String;
       Out_Transform : out Gsk_Transform) return Boolean;
   --  Parses a given into a transform.
   --  Strings printed via [methodGsk.Transform.to_string] can be read in
   --  again successfully using this function.
   --  If String does not describe a valid transform, false is returned and
   --  `NULL` is put in Out_Transform.
   --  @param String the string to parse
   --  @param Out_Transform return location for the transform. Has
   --  transfer-ownership='full'.
   --  @return true if String described a valid transform

private
   Null_Gsk_Transform : constant Gsk_Transform :=
      (Glib.C_Boxed with null record);

end Gsk.Transform;
