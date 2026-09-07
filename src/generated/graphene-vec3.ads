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

--  A structure capable of holding a vector with three dimensions: x, y, and
--  z.
--
--  The contents of the Graphene.Vec3.Graphene_Vec3_T structure are private
--  and should never be accessed directly.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Graphene.Config; use Graphene.Config;
with Graphene.Vec2;   use Graphene.Vec2;
with Graphene.Vec4;   use Graphene.Vec4;
with Interfaces.C;    use Interfaces.C;

package Graphene.Vec3 is

   type Graphene_Vec3_T is record
      Value : Graphene.Config.Graphene_Simd4f;
   end record;
   pragma Convention (C, Graphene_Vec3_T);
   --  A structure capable of holding a vector with three dimensions: x, y,
   --  and z.
   --
   --  The contents of the Graphene.Vec3.Graphene_Vec3_T structure are private
   --  and should never be accessed directly.

   type Graphene_Vec3_Array is array (Natural range <>) of Graphene_Vec3_T;
   pragma Convention (C, Graphene_Vec3_Array);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_vec3_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add
      (Self : not null access Graphene_Vec3_T;
       B    : not null access Graphene_Vec3_T;
       Res  : not null access Graphene_Vec3_T);
   pragma Import (C, Add, "graphene_vec3_add");
   --  Adds each component of the two given vectors.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec3.Graphene_Vec3_T
   --  @param Res return location for the resulting vector

   procedure Cross
      (Self : not null access Graphene_Vec3_T;
       B    : not null access Graphene_Vec3_T;
       Res  : not null access Graphene_Vec3_T);
   pragma Import (C, Cross, "graphene_vec3_cross");
   --  Computes the cross product of the two given vectors.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec3.Graphene_Vec3_T
   --  @param Res return location for the resulting vector

   procedure Divide
      (Self : not null access Graphene_Vec3_T;
       B    : not null access Graphene_Vec3_T;
       Res  : not null access Graphene_Vec3_T);
   pragma Import (C, Divide, "graphene_vec3_divide");
   --  Divides each component of the first operand A by the corresponding
   --  component of the second operand B, and places the results into the
   --  vector Res.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec3.Graphene_Vec3_T
   --  @param Res return location for the resulting vector

   function Dot
      (Self : not null access Graphene_Vec3_T;
       B    : not null access Graphene_Vec3_T) return Interfaces.C.C_float;
   pragma Import (C, Dot, "graphene_vec3_dot");
   --  Computes the dot product of the two given vectors.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec3.Graphene_Vec3_T
   --  @return the value of the dot product

   function Equal
      (Self : not null access Graphene_Vec3_T;
       V2   : not null access Graphene_Vec3_T) return Boolean;
   --  Checks whether the two given Graphene.Vec3.Graphene_Vec3_T are equal.
   --  Since: gtk+ 1.2
   --  @param V2 a Graphene.Vec3.Graphene_Vec3_T
   --  @return `true` if the two vectors are equal, and false otherwise

   procedure Free (Self : not null access Graphene_Vec3_T);
   pragma Import (C, Free, "graphene_vec3_free");
   --  Frees the resources allocated by V
   --  Since: gtk+ 1.0

   function Get_X
      (Self : not null access Graphene_Vec3_T) return Interfaces.C.C_float;
   pragma Import (C, Get_X, "graphene_vec3_get_x");
   --  Retrieves the first component of the given vector V.
   --  Since: gtk+ 1.0
   --  @return the value of the first component of the vector

   procedure Get_Xy
      (Self : not null access Graphene_Vec3_T;
       Res  : not null access Graphene.Vec2.Graphene_Vec2_T);
   pragma Import (C, Get_Xy, "graphene_vec3_get_xy");
   --  Creates a Graphene.Vec2.Graphene_Vec2_T that contains the first and
   --  second components of the given Graphene.Vec3.Graphene_Vec3_T.
   --  Since: gtk+ 1.0
   --  @param Res return location for a Graphene.Vec2.Graphene_Vec2_T

   procedure Get_Xy0
      (Self : not null access Graphene_Vec3_T;
       Res  : not null access Graphene_Vec3_T);
   pragma Import (C, Get_Xy0, "graphene_vec3_get_xy0");
   --  Creates a Graphene.Vec3.Graphene_Vec3_T that contains the first two
   --  components of the given Graphene.Vec3.Graphene_Vec3_T, and the third
   --  component set to 0.
   --  Since: gtk+ 1.0
   --  @param Res return location for a Graphene.Vec3.Graphene_Vec3_T

   procedure Get_Xyz0
      (Self : not null access Graphene_Vec3_T;
       Res  : not null access Graphene.Vec4.Graphene_Vec4_T);
   pragma Import (C, Get_Xyz0, "graphene_vec3_get_xyz0");
   --  Converts a Graphene.Vec3.Graphene_Vec3_T in a
   --  Graphene.Vec4.Graphene_Vec4_T using 0.0 as the value for the fourth
   --  component of the resulting vector.
   --  Since: gtk+ 1.0
   --  @param Res return location for the vector

   procedure Get_Xyz1
      (Self : not null access Graphene_Vec3_T;
       Res  : not null access Graphene.Vec4.Graphene_Vec4_T);
   pragma Import (C, Get_Xyz1, "graphene_vec3_get_xyz1");
   --  Converts a Graphene.Vec3.Graphene_Vec3_T in a
   --  Graphene.Vec4.Graphene_Vec4_T using 1.0 as the value for the fourth
   --  component of the resulting vector.
   --  Since: gtk+ 1.0
   --  @param Res return location for the vector

   procedure Get_Xyzw
      (Self : not null access Graphene_Vec3_T;
       W    : Interfaces.C.C_float;
       Res  : not null access Graphene.Vec4.Graphene_Vec4_T);
   pragma Import (C, Get_Xyzw, "graphene_vec3_get_xyzw");
   --  Converts a Graphene.Vec3.Graphene_Vec3_T in a
   --  Graphene.Vec4.Graphene_Vec4_T using W as the value of the fourth
   --  component of the resulting vector.
   --  Since: gtk+ 1.0
   --  @param W the value of the W component
   --  @param Res return location for the vector

   function Get_Y
      (Self : not null access Graphene_Vec3_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Y, "graphene_vec3_get_y");
   --  Retrieves the second component of the given vector V.
   --  Since: gtk+ 1.0
   --  @return the value of the second component of the vector

   function Get_Z
      (Self : not null access Graphene_Vec3_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Z, "graphene_vec3_get_z");
   --  Retrieves the third component of the given vector V.
   --  Since: gtk+ 1.0
   --  @return the value of the third component of the vector

   function Init
      (Self : not null access Graphene_Vec3_T;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float;
       Z    : Interfaces.C.C_float) return access Graphene_Vec3_T;
   pragma Import (C, Init, "graphene_vec3_init");
   --  Initializes a Graphene.Vec3.Graphene_Vec3_T using the given values.
   --  This function can be called multiple times.
   --  Since: gtk+ 1.0
   --  @param X the X field of the vector
   --  @param Y the Y field of the vector
   --  @param Z the Z field of the vector
   --  @return a pointer to the initialized vector

   function Init_From_Vec3
      (Self : not null access Graphene_Vec3_T;
       Src  : not null access Graphene_Vec3_T) return access Graphene_Vec3_T;
   pragma Import (C, Init_From_Vec3, "graphene_vec3_init_from_vec3");
   --  Initializes a Graphene.Vec3.Graphene_Vec3_T with the values of another
   --  Graphene.Vec3.Graphene_Vec3_T.
   --  Since: gtk+ 1.0
   --  @param Src a Graphene.Vec3.Graphene_Vec3_T
   --  @return the initialized vector

   procedure Interpolate
      (Self   : not null access Graphene_Vec3_T;
       V2     : not null access Graphene_Vec3_T;
       Factor : Gdouble;
       Res    : not null access Graphene_Vec3_T);
   pragma Import (C, Interpolate, "graphene_vec3_interpolate");
   --  Linearly interpolates V1 and V2 using the given Factor.
   --  Since: gtk+ 1.10
   --  @param V2 a Graphene.Vec3.Graphene_Vec3_T
   --  @param Factor the interpolation factor
   --  @param Res the interpolated vector

   function Length
      (Self : not null access Graphene_Vec3_T) return Interfaces.C.C_float;
   pragma Import (C, Length, "graphene_vec3_length");
   --  Retrieves the length of the given vector V.
   --  Since: gtk+ 1.0
   --  @return the value of the length of the vector

   procedure Max
      (Self : not null access Graphene_Vec3_T;
       B    : not null access Graphene_Vec3_T;
       Res  : not null access Graphene_Vec3_T);
   pragma Import (C, Max, "graphene_vec3_max");
   --  Compares each component of the two given vectors and creates a vector
   --  that contains the maximum values.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec3.Graphene_Vec3_T
   --  @param Res return location for the result vector

   procedure Min
      (Self : not null access Graphene_Vec3_T;
       B    : not null access Graphene_Vec3_T;
       Res  : not null access Graphene_Vec3_T);
   pragma Import (C, Min, "graphene_vec3_min");
   --  Compares each component of the two given vectors and creates a vector
   --  that contains the minimum values.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec3.Graphene_Vec3_T
   --  @param Res return location for the result vector

   procedure Multiply
      (Self : not null access Graphene_Vec3_T;
       B    : not null access Graphene_Vec3_T;
       Res  : not null access Graphene_Vec3_T);
   pragma Import (C, Multiply, "graphene_vec3_multiply");
   --  Multiplies each component of the two given vectors.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec3.Graphene_Vec3_T
   --  @param Res return location for the resulting vector

   function Near
      (Self    : not null access Graphene_Vec3_T;
       V2      : not null access Graphene_Vec3_T;
       Epsilon : Interfaces.C.C_float) return Boolean;
   --  Compares the two given Graphene.Vec3.Graphene_Vec3_T vectors and checks
   --  whether their values are within the given Epsilon.
   --  Since: gtk+ 1.2
   --  @param V2 a Graphene.Vec3.Graphene_Vec3_T
   --  @param Epsilon the threshold between the two vectors
   --  @return `true` if the two vectors are near each other

   procedure Negate
      (Self : not null access Graphene_Vec3_T;
       Res  : not null access Graphene_Vec3_T);
   pragma Import (C, Negate, "graphene_vec3_negate");
   --  Negates the given Graphene.Vec3.Graphene_Vec3_T.
   --  Since: gtk+ 1.2
   --  @param Res return location for the result vector

   procedure Normalize
      (Self : not null access Graphene_Vec3_T;
       Res  : not null access Graphene_Vec3_T);
   pragma Import (C, Normalize, "graphene_vec3_normalize");
   --  Normalizes the given Graphene.Vec3.Graphene_Vec3_T.
   --  Since: gtk+ 1.0
   --  @param Res return location for the normalized vector

   procedure Scale
      (Self   : not null access Graphene_Vec3_T;
       Factor : Interfaces.C.C_float;
       Res    : not null access Graphene_Vec3_T);
   pragma Import (C, Scale, "graphene_vec3_scale");
   --  Multiplies all components of the given vector with the given scalar
   --  Factor.
   --  Since: gtk+ 1.2
   --  @param Factor the scalar factor
   --  @param Res return location for the result vector

   procedure Subtract
      (Self : not null access Graphene_Vec3_T;
       B    : not null access Graphene_Vec3_T;
       Res  : not null access Graphene_Vec3_T);
   pragma Import (C, Subtract, "graphene_vec3_subtract");
   --  Subtracts from each component of the first operand A the corresponding
   --  component of the second operand B and places each result into the
   --  components of Res.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec3.Graphene_Vec3_T
   --  @param Res return location for the resulting vector

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Graphene_Vec3_T) return Graphene_Vec3_T;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   ---------------
   -- Functions --
   ---------------

   function One return access constant Graphene_Vec3_T;
   pragma Import (C, One, "graphene_vec3_one");
   --  Provides a constant pointer to a vector with three components, all sets
   --  to 1.
   --  Since: gtk+ 1.0
   --  @return a constant vector

   function X_Axis return access constant Graphene_Vec3_T;
   pragma Import (C, X_Axis, "graphene_vec3_x_axis");
   --  Provides a constant pointer to a vector with three components with
   --  values set to (1, 0, 0).
   --  Since: gtk+ 1.0
   --  @return a constant vector

   function Y_Axis return access constant Graphene_Vec3_T;
   pragma Import (C, Y_Axis, "graphene_vec3_y_axis");
   --  Provides a constant pointer to a vector with three components with
   --  values set to (0, 1, 0).
   --  Since: gtk+ 1.0
   --  @return a constant vector

   function Z_Axis return access constant Graphene_Vec3_T;
   pragma Import (C, Z_Axis, "graphene_vec3_z_axis");
   --  Provides a constant pointer to a vector with three components with
   --  values set to (0, 0, 1).
   --  Since: gtk+ 1.0
   --  @return a constant vector

   function Zero return access constant Graphene_Vec3_T;
   pragma Import (C, Zero, "graphene_vec3_zero");
   --  Provides a constant pointer to a vector with three components, all sets
   --  to 0.
   --  Since: gtk+ 1.0
   --  @return a constant vector

end Graphene.Vec3;
