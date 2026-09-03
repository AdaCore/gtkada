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

--  A quaternion.
--
--  The contents of the Graphene.Quaternion.Graphene_Quaternion_T structure
--  are private and should never be accessed directly.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
limited with Graphene.Euler;
limited with Graphene.Matrix;
with Graphene.Vec3;   use Graphene.Vec3;
with Graphene.Vec4;   use Graphene.Vec4;
with Interfaces.C;    use Interfaces.C;

package Graphene.Quaternion is

   type Graphene_Quaternion_T is record
      X : Interfaces.C.C_float;
      Y : Interfaces.C.C_float;
      Z : Interfaces.C.C_float;
      W : Interfaces.C.C_float;
   end record;
   pragma Convention (C, Graphene_Quaternion_T);
   --  A quaternion.
   --
   --  The contents of the Graphene.Quaternion.Graphene_Quaternion_T structure
   --  are private and should never be accessed directly.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_quaternion_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add
      (Self : not null access Graphene_Quaternion_T;
       B    : not null access Graphene_Quaternion_T;
       Res  : not null access Graphene_Quaternion_T);
   pragma Import (C, Add, "graphene_quaternion_add");
   --  Adds two Graphene.Quaternion.Graphene_Quaternion_T A and B.
   --  Since: gtk+ 1.10
   --  @param B a Graphene.Quaternion.Graphene_Quaternion_T
   --  @param Res the result of the operation

   function Dot
      (Self : not null access Graphene_Quaternion_T;
       B    : not null access Graphene_Quaternion_T)
       return Interfaces.C.C_float;
   pragma Import (C, Dot, "graphene_quaternion_dot");
   --  Computes the dot product of two
   --  Graphene.Quaternion.Graphene_Quaternion_T.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Quaternion.Graphene_Quaternion_T
   --  @return the value of the dot products

   function Equal
      (Self : not null access Graphene_Quaternion_T;
       B    : not null access Graphene_Quaternion_T) return Boolean;
   --  Checks whether the given quaternions are equal.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Quaternion.Graphene_Quaternion_T
   --  @return `true` if the quaternions are equal

   procedure Free (Self : not null access Graphene_Quaternion_T);
   pragma Import (C, Free, "graphene_quaternion_free");
   --  Releases the resources allocated by graphene_quaternion_alloc.
   --  Since: gtk+ 1.0

   function Init
      (Self : not null access Graphene_Quaternion_T;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float;
       Z    : Interfaces.C.C_float;
       W    : Interfaces.C.C_float) return access Graphene_Quaternion_T;
   pragma Import (C, Init, "graphene_quaternion_init");
   --  Initializes a Graphene.Quaternion.Graphene_Quaternion_T using the given
   --  four values.
   --  Since: gtk+ 1.0
   --  @param X the first component of the quaternion
   --  @param Y the second component of the quaternion
   --  @param Z the third component of the quaternion
   --  @param W the fourth component of the quaternion
   --  @return the initialized quaternion

   function Init_From_Angle_Vec3
      (Self  : not null access Graphene_Quaternion_T;
       Angle : Interfaces.C.C_float;
       Axis  : not null access Graphene.Vec3.Graphene_Vec3_T)
       return access Graphene_Quaternion_T;
   pragma Import (C, Init_From_Angle_Vec3, "graphene_quaternion_init_from_angle_vec3");
   --  Initializes a Graphene.Quaternion.Graphene_Quaternion_T using an Angle
   --  on a specific Axis.
   --  Since: gtk+ 1.0
   --  @param Angle the rotation on a given axis, in degrees
   --  @param Axis the axis of rotation, expressed as a vector
   --  @return the initialized quaternion

   function Init_From_Angles
      (Self  : not null access Graphene_Quaternion_T;
       Deg_X : Interfaces.C.C_float;
       Deg_Y : Interfaces.C.C_float;
       Deg_Z : Interfaces.C.C_float) return access Graphene_Quaternion_T;
   pragma Import (C, Init_From_Angles, "graphene_quaternion_init_from_angles");
   --  Initializes a Graphene.Quaternion.Graphene_Quaternion_T using the
   --  values of the [Euler angles](http://en.wikipedia.org/wiki/Euler_angles)
   --  on each axis.
   --  See also: Graphene.Quaternion.Init_From_Euler
   --  Since: gtk+ 1.0
   --  @param Deg_X rotation angle on the X axis (yaw), in degrees
   --  @param Deg_Y rotation angle on the Y axis (pitch), in degrees
   --  @param Deg_Z rotation angle on the Z axis (roll), in degrees
   --  @return the initialized quaternion

   function Init_From_Euler
      (Self : not null access Graphene_Quaternion_T;
       E    : not null access Graphene.Euler.Graphene_Euler_T)
       return access Graphene_Quaternion_T;
   pragma Import (C, Init_From_Euler, "graphene_quaternion_init_from_euler");
   --  Initializes a Graphene.Quaternion.Graphene_Quaternion_T using the given
   --  Graphene.Euler.Graphene_Euler_T.
   --  Since: gtk+ 1.2
   --  @param E a Graphene.Euler.Graphene_Euler_T
   --  @return the initialized Graphene.Quaternion.Graphene_Quaternion_T

   function Init_From_Matrix
      (Self : not null access Graphene_Quaternion_T;
       M    : not null access Graphene.Matrix.Graphene_Matrix_T)
       return access Graphene_Quaternion_T;
   pragma Import (C, Init_From_Matrix, "graphene_quaternion_init_from_matrix");
   --  Initializes a Graphene.Quaternion.Graphene_Quaternion_T using the
   --  rotation components of a transformation matrix.
   --  Since: gtk+ 1.0
   --  @param M a Graphene.Matrix.Graphene_Matrix_T
   --  @return the initialized quaternion

   function Init_From_Quaternion
      (Self : not null access Graphene_Quaternion_T;
       Src  : not null access Graphene_Quaternion_T)
       return access Graphene_Quaternion_T;
   pragma Import (C, Init_From_Quaternion, "graphene_quaternion_init_from_quaternion");
   --  Initializes a Graphene.Quaternion.Graphene_Quaternion_T with the values
   --  from Src.
   --  Since: gtk+ 1.0
   --  @param Src a Graphene.Quaternion.Graphene_Quaternion_T
   --  @return the initialized quaternion

   function Init_From_Radians
      (Self  : not null access Graphene_Quaternion_T;
       Rad_X : Interfaces.C.C_float;
       Rad_Y : Interfaces.C.C_float;
       Rad_Z : Interfaces.C.C_float) return access Graphene_Quaternion_T;
   pragma Import (C, Init_From_Radians, "graphene_quaternion_init_from_radians");
   --  Initializes a Graphene.Quaternion.Graphene_Quaternion_T using the
   --  values of the [Euler angles](http://en.wikipedia.org/wiki/Euler_angles)
   --  on each axis.
   --  See also: Graphene.Quaternion.Init_From_Euler
   --  Since: gtk+ 1.0
   --  @param Rad_X rotation angle on the X axis (yaw), in radians
   --  @param Rad_Y rotation angle on the Y axis (pitch), in radians
   --  @param Rad_Z rotation angle on the Z axis (roll), in radians
   --  @return the initialized quaternion

   function Init_From_Vec4
      (Self : not null access Graphene_Quaternion_T;
       Src  : not null access Graphene.Vec4.Graphene_Vec4_T)
       return access Graphene_Quaternion_T;
   pragma Import (C, Init_From_Vec4, "graphene_quaternion_init_from_vec4");
   --  Initializes a Graphene.Quaternion.Graphene_Quaternion_T with the values
   --  from Src.
   --  Since: gtk+ 1.0
   --  @param Src a Graphene.Vec4.Graphene_Vec4_T
   --  @return the initialized quaternion

   function Init_Identity
      (Self : not null access Graphene_Quaternion_T)
       return access Graphene_Quaternion_T;
   pragma Import (C, Init_Identity, "graphene_quaternion_init_identity");
   --  Initializes a Graphene.Quaternion.Graphene_Quaternion_T using the
   --  identity transformation.
   --  Since: gtk+ 1.0
   --  @return the initialized quaternion

   procedure Invert
      (Self : not null access Graphene_Quaternion_T;
       Res  : not null access Graphene_Quaternion_T);
   pragma Import (C, Invert, "graphene_quaternion_invert");
   --  Inverts a Graphene.Quaternion.Graphene_Quaternion_T, and returns the
   --  conjugate quaternion of Q.
   --  Since: gtk+ 1.0
   --  @param Res return location for the inverted quaternion

   procedure Multiply
      (Self : not null access Graphene_Quaternion_T;
       B    : not null access Graphene_Quaternion_T;
       Res  : not null access Graphene_Quaternion_T);
   pragma Import (C, Multiply, "graphene_quaternion_multiply");
   --  Multiplies two Graphene.Quaternion.Graphene_Quaternion_T A and B.
   --  Since: gtk+ 1.10
   --  @param B a Graphene.Quaternion.Graphene_Quaternion_T
   --  @param Res the result of the operation

   procedure Normalize
      (Self : not null access Graphene_Quaternion_T;
       Res  : not null access Graphene_Quaternion_T);
   pragma Import (C, Normalize, "graphene_quaternion_normalize");
   --  Normalizes a Graphene.Quaternion.Graphene_Quaternion_T.
   --  Since: gtk+ 1.0
   --  @param Res return location for the normalized quaternion

   procedure Scale
      (Self   : not null access Graphene_Quaternion_T;
       Factor : Interfaces.C.C_float;
       Res    : not null access Graphene_Quaternion_T);
   pragma Import (C, Scale, "graphene_quaternion_scale");
   --  Scales all the elements of a Graphene.Quaternion.Graphene_Quaternion_T
   --  Q using the given scalar factor.
   --  Since: gtk+ 1.10
   --  @param Factor a scaling factor
   --  @param Res the result of the operation

   procedure Slerp
      (Self   : not null access Graphene_Quaternion_T;
       B      : not null access Graphene_Quaternion_T;
       Factor : Interfaces.C.C_float;
       Res    : not null access Graphene_Quaternion_T);
   pragma Import (C, Slerp, "graphene_quaternion_slerp");
   --  Interpolates between the two given quaternions using a spherical linear
   --  interpolation, or [SLERP](http://en.wikipedia.org/wiki/Slerp), using the
   --  given interpolation Factor.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Quaternion.Graphene_Quaternion_T
   --  @param Factor the linear interpolation factor
   --  @param Res return location for the interpolated quaternion

   procedure To_Angle_Vec3
      (Self  : not null access Graphene_Quaternion_T;
       Angle : out Interfaces.C.C_float;
       Axis  : not null access Graphene.Vec3.Graphene_Vec3_T);
   pragma Import (C, To_Angle_Vec3, "graphene_quaternion_to_angle_vec3");
   --  Converts a quaternion into an Angle, Axis pair.
   --  Since: gtk+ 1.0
   --  @param Angle return location for the angle, in degrees
   --  @param Axis return location for the rotation axis

   procedure To_Angles
      (Self  : not null access Graphene_Quaternion_T;
       Deg_X : out Interfaces.C.C_float;
       Deg_Y : out Interfaces.C.C_float;
       Deg_Z : out Interfaces.C.C_float);
   pragma Import (C, To_Angles, "graphene_quaternion_to_angles");
   --  Converts a Graphene.Quaternion.Graphene_Quaternion_T to its
   --  corresponding rotations on the [Euler
   --  angles](http://en.wikipedia.org/wiki/Euler_angles) on each axis.
   --  Since: gtk+ 1.2
   --  @param Deg_X return location for the rotation angle on the X axis
   --  (yaw), in degrees
   --  @param Deg_Y return location for the rotation angle on the Y axis
   --  (pitch), in degrees
   --  @param Deg_Z return location for the rotation angle on the Z axis
   --  (roll), in degrees

   procedure To_Matrix
      (Self : not null access Graphene_Quaternion_T;
       M    : not null access Graphene.Matrix.Graphene_Matrix_T);
   pragma Import (C, To_Matrix, "graphene_quaternion_to_matrix");
   --  Converts a quaternion into a transformation matrix expressing the
   --  rotation defined by the Graphene.Quaternion.Graphene_Quaternion_T.
   --  Since: gtk+ 1.0
   --  @param M a Graphene.Matrix.Graphene_Matrix_T

   procedure To_Radians
      (Self  : not null access Graphene_Quaternion_T;
       Rad_X : out Interfaces.C.C_float;
       Rad_Y : out Interfaces.C.C_float;
       Rad_Z : out Interfaces.C.C_float);
   pragma Import (C, To_Radians, "graphene_quaternion_to_radians");
   --  Converts a Graphene.Quaternion.Graphene_Quaternion_T to its
   --  corresponding rotations on the [Euler
   --  angles](http://en.wikipedia.org/wiki/Euler_angles) on each axis.
   --  Since: gtk+ 1.2
   --  @param Rad_X return location for the rotation angle on the X axis
   --  (yaw), in radians
   --  @param Rad_Y return location for the rotation angle on the Y axis
   --  (pitch), in radians
   --  @param Rad_Z return location for the rotation angle on the Z axis
   --  (roll), in radians

   procedure To_Vec4
      (Self : not null access Graphene_Quaternion_T;
       Res  : not null access Graphene.Vec4.Graphene_Vec4_T);
   pragma Import (C, To_Vec4, "graphene_quaternion_to_vec4");
   --  Copies the components of a Graphene.Quaternion.Graphene_Quaternion_T
   --  into a Graphene.Vec4.Graphene_Vec4_T.
   --  Since: gtk+ 1.0
   --  @param Res return location for a Graphene.Vec4.Graphene_Vec4_T

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Graphene_Quaternion_T) return Graphene_Quaternion_T;
   pragma Inline (From_Object_Free);

end Graphene.Quaternion;
