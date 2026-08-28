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

--  A structure capable of holding a vector with four dimensions: x, y, z, and
--  w.
--
--  The contents of the Graphene.Vec4.Graphene_Vec4_T structure are private
--  and should never be accessed directly.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Graphene.Config; use Graphene.Config;
with Graphene.Vec2;   use Graphene.Vec2;
limited with Graphene.Vec3;
with Interfaces.C;    use Interfaces.C;

package Graphene.Vec4 is

   type Graphene_Vec4_T is record
      Value : Graphene.Config.Graphene_Simd4f;
   end record;
   pragma Convention (C, Graphene_Vec4_T);

   function From_Object_Free
     (B : not null access Graphene_Vec4_T) return Graphene_Vec4_T;
   pragma Inline (From_Object_Free);
   --  A structure capable of holding a vector with four dimensions: x, y, z,
   --  and w.
   --
   --  The contents of the Graphene.Vec4.Graphene_Vec4_T structure are private
   --  and should never be accessed directly.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_vec4_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add
      (Self : not null access Graphene_Vec4_T;
       B    : not null access Graphene_Vec4_T;
       Res  : not null access Graphene_Vec4_T);
   pragma Import (C, Add, "graphene_vec4_add");
   --  Adds each component of the two given vectors.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec4.Graphene_Vec4_T
   --  @param Res return location for the resulting vector

   procedure Divide
      (Self : not null access Graphene_Vec4_T;
       B    : not null access Graphene_Vec4_T;
       Res  : not null access Graphene_Vec4_T);
   pragma Import (C, Divide, "graphene_vec4_divide");
   --  Divides each component of the first operand A by the corresponding
   --  component of the second operand B, and places the results into the
   --  vector Res.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec4.Graphene_Vec4_T
   --  @param Res return location for the resulting vector

   function Dot
      (Self : not null access Graphene_Vec4_T;
       B    : not null access Graphene_Vec4_T) return Interfaces.C.C_float;
   pragma Import (C, Dot, "graphene_vec4_dot");
   --  Computes the dot product of the two given vectors.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec4.Graphene_Vec4_T
   --  @return the value of the dot product

   function Equal
      (Self : not null access Graphene_Vec4_T;
       V2   : not null access Graphene_Vec4_T) return Boolean;
   --  Checks whether the two given Graphene.Vec4.Graphene_Vec4_T are equal.
   --  Since: gtk+ 1.2
   --  @param V2 a Graphene.Vec4.Graphene_Vec4_T
   --  @return `true` if the two vectors are equal, and false otherwise

   procedure Free (Self : not null access Graphene_Vec4_T);
   pragma Import (C, Free, "graphene_vec4_free");
   --  Frees the resources allocated by V
   --  Since: gtk+ 1.0

   function Get_W
      (Self : not null access Graphene_Vec4_T) return Interfaces.C.C_float;
   pragma Import (C, Get_W, "graphene_vec4_get_w");
   --  Retrieves the value of the fourth component of the given
   --  Graphene.Vec4.Graphene_Vec4_T.
   --  Since: gtk+ 1.0
   --  @return the value of the fourth component

   function Get_X
      (Self : not null access Graphene_Vec4_T) return Interfaces.C.C_float;
   pragma Import (C, Get_X, "graphene_vec4_get_x");
   --  Retrieves the value of the first component of the given
   --  Graphene.Vec4.Graphene_Vec4_T.
   --  Since: gtk+ 1.0
   --  @return the value of the first component

   procedure Get_Xy
      (Self : not null access Graphene_Vec4_T;
       Res  : not null access Graphene.Vec2.Graphene_Vec2_T);
   pragma Import (C, Get_Xy, "graphene_vec4_get_xy");
   --  Creates a Graphene.Vec2.Graphene_Vec2_T that contains the first two
   --  components of the given Graphene.Vec4.Graphene_Vec4_T.
   --  Since: gtk+ 1.0
   --  @param Res return location for a Graphene.Vec2.Graphene_Vec2_T

   procedure Get_Xyz
      (Self : not null access Graphene_Vec4_T;
       Res  : not null access Graphene.Vec3.Graphene_Vec3_T);
   pragma Import (C, Get_Xyz, "graphene_vec4_get_xyz");
   --  Creates a Graphene.Vec3.Graphene_Vec3_T that contains the first three
   --  components of the given Graphene.Vec4.Graphene_Vec4_T.
   --  Since: gtk+ 1.0
   --  @param Res return location for a graphene_vec3_t

   function Get_Y
      (Self : not null access Graphene_Vec4_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Y, "graphene_vec4_get_y");
   --  Retrieves the value of the second component of the given
   --  Graphene.Vec4.Graphene_Vec4_T.
   --  Since: gtk+ 1.0
   --  @return the value of the second component

   function Get_Z
      (Self : not null access Graphene_Vec4_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Z, "graphene_vec4_get_z");
   --  Retrieves the value of the third component of the given
   --  Graphene.Vec4.Graphene_Vec4_T.
   --  Since: gtk+ 1.0
   --  @return the value of the third component

   function Init
      (Self : not null access Graphene_Vec4_T;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float;
       Z    : Interfaces.C.C_float;
       W    : Interfaces.C.C_float) return access Graphene_Vec4_T;
   pragma Import (C, Init, "graphene_vec4_init");
   --  Initializes a Graphene.Vec4.Graphene_Vec4_T using the given values.
   --  This function can be called multiple times.
   --  Since: gtk+ 1.0
   --  @param X the X field of the vector
   --  @param Y the Y field of the vector
   --  @param Z the Z field of the vector
   --  @param W the W field of the vector
   --  @return a pointer to the initialized vector

   function Init_From_Vec2
      (Self : not null access Graphene_Vec4_T;
       Src  : not null access Graphene.Vec2.Graphene_Vec2_T;
       Z    : Interfaces.C.C_float;
       W    : Interfaces.C.C_float) return access Graphene_Vec4_T;
   pragma Import (C, Init_From_Vec2, "graphene_vec4_init_from_vec2");
   --  Initializes a Graphene.Vec4.Graphene_Vec4_T using the components of a
   --  Graphene.Vec2.Graphene_Vec2_T and the values of Z and W.
   --  Since: gtk+ 1.0
   --  @param Src a Graphene.Vec2.Graphene_Vec2_T
   --  @param Z the value for the third component of V
   --  @param W the value for the fourth component of V
   --  @return the initialized vector

   function Init_From_Vec3
      (Self : not null access Graphene_Vec4_T;
       Src  : not null access Graphene.Vec3.Graphene_Vec3_T;
       W    : Interfaces.C.C_float) return access Graphene_Vec4_T;
   pragma Import (C, Init_From_Vec3, "graphene_vec4_init_from_vec3");
   --  Initializes a Graphene.Vec4.Graphene_Vec4_T using the components of a
   --  Graphene.Vec3.Graphene_Vec3_T and the value of W.
   --  Since: gtk+ 1.0
   --  @param Src a Graphene.Vec3.Graphene_Vec3_T
   --  @param W the value for the fourth component of V
   --  @return the initialized vector

   function Init_From_Vec4
      (Self : not null access Graphene_Vec4_T;
       Src  : not null access Graphene_Vec4_T) return access Graphene_Vec4_T;
   pragma Import (C, Init_From_Vec4, "graphene_vec4_init_from_vec4");
   --  Initializes a Graphene.Vec4.Graphene_Vec4_T using the components of
   --  another Graphene.Vec4.Graphene_Vec4_T.
   --  Since: gtk+ 1.0
   --  @param Src a Graphene.Vec4.Graphene_Vec4_T
   --  @return the initialized vector

   procedure Interpolate
      (Self   : not null access Graphene_Vec4_T;
       V2     : not null access Graphene_Vec4_T;
       Factor : Gdouble;
       Res    : not null access Graphene_Vec4_T);
   pragma Import (C, Interpolate, "graphene_vec4_interpolate");
   --  Linearly interpolates V1 and V2 using the given Factor.
   --  Since: gtk+ 1.10
   --  @param V2 a Graphene.Vec4.Graphene_Vec4_T
   --  @param Factor the interpolation factor
   --  @param Res the interpolated vector

   function Length
      (Self : not null access Graphene_Vec4_T) return Interfaces.C.C_float;
   pragma Import (C, Length, "graphene_vec4_length");
   --  Computes the length of the given Graphene.Vec4.Graphene_Vec4_T.
   --  Since: gtk+ 1.0
   --  @return the length of the vector

   procedure Max
      (Self : not null access Graphene_Vec4_T;
       B    : not null access Graphene_Vec4_T;
       Res  : not null access Graphene_Vec4_T);
   pragma Import (C, Max, "graphene_vec4_max");
   --  Compares each component of the two given vectors and creates a vector
   --  that contains the maximum values.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec4.Graphene_Vec4_T
   --  @param Res return location for the result vector

   procedure Min
      (Self : not null access Graphene_Vec4_T;
       B    : not null access Graphene_Vec4_T;
       Res  : not null access Graphene_Vec4_T);
   pragma Import (C, Min, "graphene_vec4_min");
   --  Compares each component of the two given vectors and creates a vector
   --  that contains the minimum values.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec4.Graphene_Vec4_T
   --  @param Res return location for the result vector

   procedure Multiply
      (Self : not null access Graphene_Vec4_T;
       B    : not null access Graphene_Vec4_T;
       Res  : not null access Graphene_Vec4_T);
   pragma Import (C, Multiply, "graphene_vec4_multiply");
   --  Multiplies each component of the two given vectors.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec4.Graphene_Vec4_T
   --  @param Res return location for the resulting vector

   function Near
      (Self    : not null access Graphene_Vec4_T;
       V2      : not null access Graphene_Vec4_T;
       Epsilon : Interfaces.C.C_float) return Boolean;
   --  Compares the two given Graphene.Vec4.Graphene_Vec4_T vectors and checks
   --  whether their values are within the given Epsilon.
   --  Since: gtk+ 1.2
   --  @param V2 a Graphene.Vec4.Graphene_Vec4_T
   --  @param Epsilon the threshold between the two vectors
   --  @return `true` if the two vectors are near each other

   procedure Negate
      (Self : not null access Graphene_Vec4_T;
       Res  : not null access Graphene_Vec4_T);
   pragma Import (C, Negate, "graphene_vec4_negate");
   --  Negates the given Graphene.Vec4.Graphene_Vec4_T.
   --  Since: gtk+ 1.2
   --  @param Res return location for the result vector

   procedure Normalize
      (Self : not null access Graphene_Vec4_T;
       Res  : not null access Graphene_Vec4_T);
   pragma Import (C, Normalize, "graphene_vec4_normalize");
   --  Normalizes the given Graphene.Vec4.Graphene_Vec4_T.
   --  Since: gtk+ 1.0
   --  @param Res return location for the normalized vector

   procedure Scale
      (Self   : not null access Graphene_Vec4_T;
       Factor : Interfaces.C.C_float;
       Res    : not null access Graphene_Vec4_T);
   pragma Import (C, Scale, "graphene_vec4_scale");
   --  Multiplies all components of the given vector with the given scalar
   --  Factor.
   --  Since: gtk+ 1.2
   --  @param Factor the scalar factor
   --  @param Res return location for the result vector

   procedure Subtract
      (Self : not null access Graphene_Vec4_T;
       B    : not null access Graphene_Vec4_T;
       Res  : not null access Graphene_Vec4_T);
   pragma Import (C, Subtract, "graphene_vec4_subtract");
   --  Subtracts from each component of the first operand A the corresponding
   --  component of the second operand B and places each result into the
   --  components of Res.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec4.Graphene_Vec4_T
   --  @param Res return location for the resulting vector

   ---------------
   -- Functions --
   ---------------

   function One return access constant Graphene_Vec4_T;
   pragma Import (C, One, "graphene_vec4_one");
   --  Retrieves a pointer to a Graphene.Vec4.Graphene_Vec4_T with all its
   --  components set to 1.
   --  Since: gtk+ 1.0
   --  @return a constant vector

   function W_Axis return access constant Graphene_Vec4_T;
   pragma Import (C, W_Axis, "graphene_vec4_w_axis");
   --  Retrieves a pointer to a Graphene.Vec4.Graphene_Vec4_T with its
   --  components set to (0, 0, 0, 1).
   --  Since: gtk+ 1.0
   --  @return a constant vector

   function X_Axis return access constant Graphene_Vec4_T;
   pragma Import (C, X_Axis, "graphene_vec4_x_axis");
   --  Retrieves a pointer to a Graphene.Vec4.Graphene_Vec4_T with its
   --  components set to (1, 0, 0, 0).
   --  Since: gtk+ 1.0
   --  @return a constant vector

   function Y_Axis return access constant Graphene_Vec4_T;
   pragma Import (C, Y_Axis, "graphene_vec4_y_axis");
   --  Retrieves a pointer to a Graphene.Vec4.Graphene_Vec4_T with its
   --  components set to (0, 1, 0, 0).
   --  Since: gtk+ 1.0
   --  @return a constant vector

   function Z_Axis return access constant Graphene_Vec4_T;
   pragma Import (C, Z_Axis, "graphene_vec4_z_axis");
   --  Retrieves a pointer to a Graphene.Vec4.Graphene_Vec4_T with its
   --  components set to (0, 0, 1, 0).
   --  Since: gtk+ 1.0
   --  @return a constant vector

   function Zero return access constant Graphene_Vec4_T;
   pragma Import (C, Zero, "graphene_vec4_zero");
   --  Retrieves a pointer to a Graphene.Vec4.Graphene_Vec4_T with all its
   --  components set to 0.
   --  Since: gtk+ 1.0
   --  @return a constant vector

end Graphene.Vec4;
