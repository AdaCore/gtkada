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

--  A structure capable of holding a vector with two dimensions, x and y.
--
--  The contents of the Graphene.Vec2.Graphene_Vec2_T structure are private
--  and should never be accessed directly.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Graphene.Config; use Graphene.Config;
with Interfaces.C;    use Interfaces.C;

package Graphene.Vec2 is

   type Graphene_Vec2_T is record
      Value : Graphene.Config.Graphene_Simd4f;
   end record;
   pragma Convention (C, Graphene_Vec2_T);

   function From_Object_Free
     (B : not null access Graphene_Vec2_T) return Graphene_Vec2_T;
   pragma Inline (From_Object_Free);
   --  A structure capable of holding a vector with two dimensions, x and y.
   --
   --  The contents of the Graphene.Vec2.Graphene_Vec2_T structure are private
   --  and should never be accessed directly.

   type Graphene_Vec2_Array is array (Natural range <>) of Graphene_Vec2_T;
   pragma Convention (C, Graphene_Vec2_Array);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_vec2_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add
      (Self : not null access Graphene_Vec2_T;
       B    : not null access Graphene_Vec2_T;
       Res  : not null access Graphene_Vec2_T);
   pragma Import (C, Add, "graphene_vec2_add");
   --  Adds each component of the two passed vectors and places each result
   --  into the components of Res.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec2.Graphene_Vec2_T
   --  @param Res return location for the result

   procedure Divide
      (Self : not null access Graphene_Vec2_T;
       B    : not null access Graphene_Vec2_T;
       Res  : not null access Graphene_Vec2_T);
   pragma Import (C, Divide, "graphene_vec2_divide");
   --  Divides each component of the first operand A by the corresponding
   --  component of the second operand B, and places the results into the
   --  vector Res.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec2.Graphene_Vec2_T
   --  @param Res return location for the result

   function Dot
      (Self : not null access Graphene_Vec2_T;
       B    : not null access Graphene_Vec2_T) return Interfaces.C.C_float;
   pragma Import (C, Dot, "graphene_vec2_dot");
   --  Computes the dot product of the two given vectors.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec2.Graphene_Vec2_T
   --  @return the dot product of the vectors

   function Equal
      (Self : not null access Graphene_Vec2_T;
       V2   : not null access Graphene_Vec2_T) return Boolean;
   --  Checks whether the two given Graphene.Vec2.Graphene_Vec2_T are equal.
   --  Since: gtk+ 1.2
   --  @param V2 a Graphene.Vec2.Graphene_Vec2_T
   --  @return `true` if the two vectors are equal, and false otherwise

   procedure Free (Self : not null access Graphene_Vec2_T);
   pragma Import (C, Free, "graphene_vec2_free");
   --  Frees the resources allocated by V
   --  Since: gtk+ 1.0

   function Get_X
      (Self : not null access Graphene_Vec2_T) return Interfaces.C.C_float;
   pragma Import (C, Get_X, "graphene_vec2_get_x");
   --  Retrieves the X component of the Graphene.Vec2.Graphene_Vec2_T.
   --  Since: gtk+ 1.0
   --  @return the value of the X component

   function Get_Y
      (Self : not null access Graphene_Vec2_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Y, "graphene_vec2_get_y");
   --  Retrieves the Y component of the Graphene.Vec2.Graphene_Vec2_T.
   --  Since: gtk+ 1.0
   --  @return the value of the Y component

   function Init
      (Self : not null access Graphene_Vec2_T;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float) return access Graphene_Vec2_T;
   pragma Import (C, Init, "graphene_vec2_init");
   --  Initializes a Graphene.Vec2.Graphene_Vec2_T using the given values.
   --  This function can be called multiple times.
   --  Since: gtk+ 1.0
   --  @param X the X field of the vector
   --  @param Y the Y field of the vector
   --  @return the initialized vector

   function Init_From_Vec2
      (Self : not null access Graphene_Vec2_T;
       Src  : not null access Graphene_Vec2_T) return access Graphene_Vec2_T;
   pragma Import (C, Init_From_Vec2, "graphene_vec2_init_from_vec2");
   --  Copies the contents of Src into V.
   --  Since: gtk+ 1.0
   --  @param Src a Graphene.Vec2.Graphene_Vec2_T
   --  @return the initialized vector

   procedure Interpolate
      (Self   : not null access Graphene_Vec2_T;
       V2     : not null access Graphene_Vec2_T;
       Factor : Gdouble;
       Res    : not null access Graphene_Vec2_T);
   pragma Import (C, Interpolate, "graphene_vec2_interpolate");
   --  Linearly interpolates V1 and V2 using the given Factor.
   --  Since: gtk+ 1.10
   --  @param V2 a Graphene.Vec2.Graphene_Vec2_T
   --  @param Factor the interpolation factor
   --  @param Res the interpolated vector

   function Length
      (Self : not null access Graphene_Vec2_T) return Interfaces.C.C_float;
   pragma Import (C, Length, "graphene_vec2_length");
   --  Computes the length of the given vector.
   --  Since: gtk+ 1.0
   --  @return the length of the vector

   procedure Max
      (Self : not null access Graphene_Vec2_T;
       B    : not null access Graphene_Vec2_T;
       Res  : not null access Graphene_Vec2_T);
   pragma Import (C, Max, "graphene_vec2_max");
   --  Compares the two given vectors and places the maximum values of each
   --  component into Res.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec2.Graphene_Vec2_T
   --  @param Res the resulting vector

   procedure Min
      (Self : not null access Graphene_Vec2_T;
       B    : not null access Graphene_Vec2_T;
       Res  : not null access Graphene_Vec2_T);
   pragma Import (C, Min, "graphene_vec2_min");
   --  Compares the two given vectors and places the minimum values of each
   --  component into Res.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec2.Graphene_Vec2_T
   --  @param Res the resulting vector

   procedure Multiply
      (Self : not null access Graphene_Vec2_T;
       B    : not null access Graphene_Vec2_T;
       Res  : not null access Graphene_Vec2_T);
   pragma Import (C, Multiply, "graphene_vec2_multiply");
   --  Multiplies each component of the two passed vectors and places each
   --  result into the components of Res.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec2.Graphene_Vec2_T
   --  @param Res return location for the result

   function Near
      (Self    : not null access Graphene_Vec2_T;
       V2      : not null access Graphene_Vec2_T;
       Epsilon : Interfaces.C.C_float) return Boolean;
   --  Compares the two given Graphene.Vec2.Graphene_Vec2_T vectors and checks
   --  whether their values are within the given Epsilon.
   --  Since: gtk+ 1.2
   --  @param V2 a Graphene.Vec2.Graphene_Vec2_T
   --  @param Epsilon the threshold between the two vectors
   --  @return `true` if the two vectors are near each other

   procedure Negate
      (Self : not null access Graphene_Vec2_T;
       Res  : not null access Graphene_Vec2_T);
   pragma Import (C, Negate, "graphene_vec2_negate");
   --  Negates the given Graphene.Vec2.Graphene_Vec2_T.
   --  Since: gtk+ 1.2
   --  @param Res return location for the result vector

   procedure Normalize
      (Self : not null access Graphene_Vec2_T;
       Res  : not null access Graphene_Vec2_T);
   pragma Import (C, Normalize, "graphene_vec2_normalize");
   --  Computes the normalized vector for the given vector V.
   --  Since: gtk+ 1.0
   --  @param Res return location for the normalized vector

   procedure Scale
      (Self   : not null access Graphene_Vec2_T;
       Factor : Interfaces.C.C_float;
       Res    : not null access Graphene_Vec2_T);
   pragma Import (C, Scale, "graphene_vec2_scale");
   --  Multiplies all components of the given vector with the given scalar
   --  Factor.
   --  Since: gtk+ 1.2
   --  @param Factor the scalar factor
   --  @param Res return location for the result vector

   procedure Subtract
      (Self : not null access Graphene_Vec2_T;
       B    : not null access Graphene_Vec2_T;
       Res  : not null access Graphene_Vec2_T);
   pragma Import (C, Subtract, "graphene_vec2_subtract");
   --  Subtracts from each component of the first operand A the corresponding
   --  component of the second operand B and places each result into the
   --  components of Res.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Vec2.Graphene_Vec2_T
   --  @param Res return location for the result

   ---------------
   -- Functions --
   ---------------

   function One return access constant Graphene_Vec2_T;
   pragma Import (C, One, "graphene_vec2_one");
   --  Retrieves a constant vector with (1, 1) components.
   --  Since: gtk+ 1.0
   --  @return the one vector

   function X_Axis return access constant Graphene_Vec2_T;
   pragma Import (C, X_Axis, "graphene_vec2_x_axis");
   --  Retrieves a constant vector with (1, 0) components.
   --  Since: gtk+ 1.0
   --  @return the X axis vector

   function Y_Axis return access constant Graphene_Vec2_T;
   pragma Import (C, Y_Axis, "graphene_vec2_y_axis");
   --  Retrieves a constant vector with (0, 1) components.
   --  Since: gtk+ 1.0
   --  @return the Y axis vector

   function Zero return access constant Graphene_Vec2_T;
   pragma Import (C, Zero, "graphene_vec2_zero");
   --  Retrieves a constant vector with (0, 0) components.
   --  Since: gtk+ 1.0
   --  @return the zero vector

end Graphene.Vec2;
