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

--  Describe a rotation using Euler angles.
--
--  The contents of the Graphene.Euler.Graphene_Euler_T structure are private
--  and should never be accessed directly.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
limited with Graphene.Matrix;
with Graphene.Quaternion;     use Graphene.Quaternion;
with Graphene.Vec3;           use Graphene.Vec3;
with Interfaces.C;            use Interfaces.C;

package Graphene.Euler is

   type graphene_euler_order_t is (
      Graphene_Euler_Order_Default,
      Graphene_Euler_Order_Xyz,
      Graphene_Euler_Order_Yzx,
      Graphene_Euler_Order_Zxy,
      Graphene_Euler_Order_Xzy,
      Graphene_Euler_Order_Yxz,
      Graphene_Euler_Order_Zyx,
      Graphene_Euler_Order_Sxyz,
      Graphene_Euler_Order_Sxyx,
      Graphene_Euler_Order_Sxzy,
      Graphene_Euler_Order_Sxzx,
      Graphene_Euler_Order_Syzx,
      Graphene_Euler_Order_Syzy,
      Graphene_Euler_Order_Syxz,
      Graphene_Euler_Order_Syxy,
      Graphene_Euler_Order_Szxy,
      Graphene_Euler_Order_Szxz,
      Graphene_Euler_Order_Szyx,
      Graphene_Euler_Order_Szyz,
      Graphene_Euler_Order_Rzyx,
      Graphene_Euler_Order_Rxyx,
      Graphene_Euler_Order_Ryzx,
      Graphene_Euler_Order_Rxzx,
      Graphene_Euler_Order_Rxzy,
      Graphene_Euler_Order_Ryzy,
      Graphene_Euler_Order_Rzxy,
      Graphene_Euler_Order_Ryxy,
      Graphene_Euler_Order_Ryxz,
      Graphene_Euler_Order_Rzxz,
      Graphene_Euler_Order_Rxyz,
      Graphene_Euler_Order_Rzyz);
   pragma Convention (C, graphene_euler_order_t);
   --  Specify the order of the rotations on each axis.
   --
   --  The Graphene.Euler.Graphene_Euler_Order_Default value is special, and
   --  is used as an alias for one of the other orders.

   for graphene_euler_order_t use (
      Graphene_Euler_Order_Default => -1,
      Graphene_Euler_Order_Xyz => 0,
      Graphene_Euler_Order_Yzx => 1,
      Graphene_Euler_Order_Zxy => 2,
      Graphene_Euler_Order_Xzy => 3,
      Graphene_Euler_Order_Yxz => 4,
      Graphene_Euler_Order_Zyx => 5,
      Graphene_Euler_Order_Sxyz => 6,
      Graphene_Euler_Order_Sxyx => 7,
      Graphene_Euler_Order_Sxzy => 8,
      Graphene_Euler_Order_Sxzx => 9,
      Graphene_Euler_Order_Syzx => 10,
      Graphene_Euler_Order_Syzy => 11,
      Graphene_Euler_Order_Syxz => 12,
      Graphene_Euler_Order_Syxy => 13,
      Graphene_Euler_Order_Szxy => 14,
      Graphene_Euler_Order_Szxz => 15,
      Graphene_Euler_Order_Szyx => 16,
      Graphene_Euler_Order_Szyz => 17,
      Graphene_Euler_Order_Rzyx => 18,
      Graphene_Euler_Order_Rxyx => 19,
      Graphene_Euler_Order_Ryzx => 20,
      Graphene_Euler_Order_Rxzx => 21,
      Graphene_Euler_Order_Rxzy => 22,
      Graphene_Euler_Order_Ryzy => 23,
      Graphene_Euler_Order_Rzxy => 24,
      Graphene_Euler_Order_Ryxy => 25,
      Graphene_Euler_Order_Ryxz => 26,
      Graphene_Euler_Order_Rzxz => 27,
      Graphene_Euler_Order_Rxyz => 28,
      Graphene_Euler_Order_Rzyz => 29);

   type Graphene_Euler_T is record
      Angles : Graphene.Vec3.Graphene_Vec3_T;
      Order : graphene_euler_order_t;
   end record;
   pragma Convention (C, Graphene_Euler_T);

   function From_Object_Free
     (B : not null access Graphene_Euler_T) return Graphene_Euler_T;
   pragma Inline (From_Object_Free);
   --  Describe a rotation using Euler angles.
   --
   --  The contents of the Graphene.Euler.Graphene_Euler_T structure are
   --  private and should never be accessed directly.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package graphene_euler_order_t_Properties is
      new Generic_Internal_Discrete_Property (graphene_euler_order_t);
   type Property_graphene_euler_order_t is new graphene_euler_order_t_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_euler_get_type");

   -------------
   -- Methods --
   -------------

   function Equal
      (Self : not null access Graphene_Euler_T;
       B    : not null access Graphene_Euler_T) return Boolean;
   --  Checks if two Graphene.Euler.Graphene_Euler_T are equal.
   --  Since: gtk+ 1.2
   --  @param B a Graphene.Euler.Graphene_Euler_T
   --  @return `true` if the two Graphene.Euler.Graphene_Euler_T are equal

   procedure Free (Self : not null access Graphene_Euler_T);
   pragma Import (C, Free, "graphene_euler_free");
   --  Frees the resources allocated by graphene_euler_alloc.
   --  Since: gtk+ 1.2

   function Get_Alpha
      (Self : not null access Graphene_Euler_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Alpha, "graphene_euler_get_alpha");
   --  Retrieves the first component of the Euler angle vector, depending on
   --  the order of rotation.
   --  See also: Graphene.Euler.Get_X
   --  Since: gtk+ 1.10
   --  @return the first component of the Euler angle vector, in radians

   function Get_Beta
      (Self : not null access Graphene_Euler_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Beta, "graphene_euler_get_beta");
   --  Retrieves the second component of the Euler angle vector, depending on
   --  the order of rotation.
   --  See also: Graphene.Euler.Get_Y
   --  Since: gtk+ 1.10
   --  @return the second component of the Euler angle vector, in radians

   function Get_Gamma
      (Self : not null access Graphene_Euler_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Gamma, "graphene_euler_get_gamma");
   --  Retrieves the third component of the Euler angle vector, depending on
   --  the order of rotation.
   --  See also: Graphene.Euler.Get_Z
   --  Since: gtk+ 1.10
   --  @return the third component of the Euler angle vector, in radians

   function Get_Order
      (Self : not null access Graphene_Euler_T)
       return graphene_euler_order_t;
   pragma Import (C, Get_Order, "graphene_euler_get_order");
   --  Retrieves the order used to apply the rotations described in the
   --  Graphene.Euler.Graphene_Euler_T structure, when converting to and from
   --  other structures, like Graphene.Quaternion.Graphene_Quaternion_T and
   --  Graphene.Matrix.Graphene_Matrix_T.
   --  This function does not return the
   --  Graphene.Euler.Graphene_Euler_Order_Default enumeration value; it will
   --  return the effective order of rotation instead.
   --  Since: gtk+ 1.2
   --  @return the order used to apply the rotations

   function Get_X
      (Self : not null access Graphene_Euler_T) return Interfaces.C.C_float;
   pragma Import (C, Get_X, "graphene_euler_get_x");
   --  Retrieves the rotation angle on the X axis, in degrees.
   --  Since: gtk+ 1.2
   --  @return the rotation angle

   function Get_Y
      (Self : not null access Graphene_Euler_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Y, "graphene_euler_get_y");
   --  Retrieves the rotation angle on the Y axis, in degrees.
   --  Since: gtk+ 1.2
   --  @return the rotation angle

   function Get_Z
      (Self : not null access Graphene_Euler_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Z, "graphene_euler_get_z");
   --  Retrieves the rotation angle on the Z axis, in degrees.
   --  Since: gtk+ 1.2
   --  @return the rotation angle

   function Init
      (Self : not null access Graphene_Euler_T;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float;
       Z    : Interfaces.C.C_float) return access Graphene_Euler_T;
   pragma Import (C, Init, "graphene_euler_init");
   --  Initializes a Graphene.Euler.Graphene_Euler_T using the given angles.
   --  The order of the rotations is
   --  Graphene.Euler.Graphene_Euler_Order_Default.
   --  Since: gtk+ 1.2
   --  @param X rotation angle on the X axis, in degrees
   --  @param Y rotation angle on the Y axis, in degrees
   --  @param Z rotation angle on the Z axis, in degrees
   --  @return the initialized Graphene.Euler.Graphene_Euler_T

   function Init_From_Euler
      (Self : not null access Graphene_Euler_T;
       Src  : access Graphene_Euler_T) return access Graphene_Euler_T;
   pragma Import (C, Init_From_Euler, "graphene_euler_init_from_euler");
   --  Initializes a Graphene.Euler.Graphene_Euler_T using the angles and
   --  order of another Graphene.Euler.Graphene_Euler_T.
   --  If the Graphene.Euler.Graphene_Euler_T Src is null, this function is
   --  equivalent to calling Graphene.Euler.Init with all angles set to 0.
   --  Since: gtk+ 1.2
   --  @param Src a Graphene.Euler.Graphene_Euler_T
   --  @return the initialized Graphene.Euler.Graphene_Euler_T

   function Init_From_Matrix
      (Self  : not null access Graphene_Euler_T;
       M     : access Graphene.Matrix.Graphene_Matrix_T;
       Order : graphene_euler_order_t) return access Graphene_Euler_T;
   pragma Import (C, Init_From_Matrix, "graphene_euler_init_from_matrix");
   --  Initializes a Graphene.Euler.Graphene_Euler_T using the given rotation
   --  matrix.
   --  If the Graphene.Matrix.Graphene_Matrix_T M is null, the
   --  Graphene.Euler.Graphene_Euler_T will be initialized with all angles set
   --  to 0.
   --  Since: gtk+ 1.2
   --  @param M a rotation matrix
   --  @param Order the order used to apply the rotations
   --  @return the initialized Graphene.Euler.Graphene_Euler_T

   function Init_From_Quaternion
      (Self  : not null access Graphene_Euler_T;
       Q     : access Graphene.Quaternion.Graphene_Quaternion_T;
       Order : graphene_euler_order_t) return access Graphene_Euler_T;
   pragma Import (C, Init_From_Quaternion, "graphene_euler_init_from_quaternion");
   --  Initializes a Graphene.Euler.Graphene_Euler_T using the given
   --  normalized quaternion.
   --  If the Graphene.Quaternion.Graphene_Quaternion_T Q is null, the
   --  Graphene.Euler.Graphene_Euler_T will be initialized with all angles set
   --  to 0.
   --  Since: gtk+ 1.2
   --  @param Q a normalized Graphene.Quaternion.Graphene_Quaternion_T
   --  @param Order the order used to apply the rotations
   --  @return the initialized Graphene.Euler.Graphene_Euler_T

   function Init_From_Radians
      (Self  : not null access Graphene_Euler_T;
       X     : Interfaces.C.C_float;
       Y     : Interfaces.C.C_float;
       Z     : Interfaces.C.C_float;
       Order : graphene_euler_order_t) return access Graphene_Euler_T;
   pragma Import (C, Init_From_Radians, "graphene_euler_init_from_radians");
   --  Initializes a Graphene.Euler.Graphene_Euler_T using the given angles
   --  and order of rotation.
   --  Since: gtk+ 1.10
   --  @param X rotation angle on the X axis, in radians
   --  @param Y rotation angle on the Y axis, in radians
   --  @param Z rotation angle on the Z axis, in radians
   --  @param Order order of rotations
   --  @return the initialized Graphene.Euler.Graphene_Euler_T

   function Init_From_Vec3
      (Self  : not null access Graphene_Euler_T;
       V     : access Graphene.Vec3.Graphene_Vec3_T;
       Order : graphene_euler_order_t) return access Graphene_Euler_T;
   pragma Import (C, Init_From_Vec3, "graphene_euler_init_from_vec3");
   --  Initializes a Graphene.Euler.Graphene_Euler_T using the angles
   --  contained in a Graphene.Vec3.Graphene_Vec3_T.
   --  If the Graphene.Vec3.Graphene_Vec3_T V is null, the
   --  Graphene.Euler.Graphene_Euler_T will be initialized with all angles set
   --  to 0.
   --  Since: gtk+ 1.2
   --  @param V a Graphene.Vec3.Graphene_Vec3_T containing the rotation angles
   --  in degrees
   --  @param Order the order used to apply the rotations
   --  @return the initialized Graphene.Euler.Graphene_Euler_T

   function Init_With_Order
      (Self  : not null access Graphene_Euler_T;
       X     : Interfaces.C.C_float;
       Y     : Interfaces.C.C_float;
       Z     : Interfaces.C.C_float;
       Order : graphene_euler_order_t) return access Graphene_Euler_T;
   pragma Import (C, Init_With_Order, "graphene_euler_init_with_order");
   --  Initializes a Graphene.Euler.Graphene_Euler_T with the given angles and
   --  Order.
   --  Since: gtk+ 1.2
   --  @param X rotation angle on the X axis, in degrees
   --  @param Y rotation angle on the Y axis, in degrees
   --  @param Z rotation angle on the Z axis, in degrees
   --  @param Order the order used to apply the rotations
   --  @return the initialized Graphene.Euler.Graphene_Euler_T

   procedure Reorder
      (Self  : not null access Graphene_Euler_T;
       Order : graphene_euler_order_t;
       Res   : not null access Graphene_Euler_T);
   pragma Import (C, Reorder, "graphene_euler_reorder");
   --  Reorders a Graphene.Euler.Graphene_Euler_T using Order.
   --  This function is equivalent to creating a
   --  Graphene.Quaternion.Graphene_Quaternion_T from the given
   --  Graphene.Euler.Graphene_Euler_T, and then converting the quaternion into
   --  another Graphene.Euler.Graphene_Euler_T.
   --  Since: gtk+ 1.2
   --  @param Order the new order
   --  @param Res return location for the reordered
   --  Graphene.Euler.Graphene_Euler_T

   procedure To_Matrix
      (Self : not null access Graphene_Euler_T;
       Res  : not null access Graphene.Matrix.Graphene_Matrix_T);
   pragma Import (C, To_Matrix, "graphene_euler_to_matrix");
   --  Converts a Graphene.Euler.Graphene_Euler_T into a transformation matrix
   --  expressing the extrinsic composition of rotations described by the Euler
   --  angles.
   --  The rotations are applied over the reference frame axes in the order
   --  associated with the Graphene.Euler.Graphene_Euler_T; for instance, if
   --  the order used to initialize E is
   --  Graphene.Euler.Graphene_Euler_Order_Xyz:
   --   * the first rotation moves the body around the X axis with an angle φ
   --  * the second rotation moves the body around the Y axis with an angle of
   --  ϑ * the third rotation moves the body around the Z axis with an angle of
   --  ψ
   --  The rotation sign convention is right-handed, to preserve compatibility
   --  between Euler-based, quaternion-based, and angle-axis-based rotations.
   --  Since: gtk+ 1.2
   --  @param Res return location for a Graphene.Matrix.Graphene_Matrix_T

   procedure To_Quaternion
      (Self : not null access Graphene_Euler_T;
       Res  : not null access Graphene.Quaternion.Graphene_Quaternion_T);
   pragma Import (C, To_Quaternion, "graphene_euler_to_quaternion");
   --  Converts a Graphene.Euler.Graphene_Euler_T into a
   --  Graphene.Quaternion.Graphene_Quaternion_T.
   --  Since: gtk+ 1.10
   --  @param Res return location for a
   --  Graphene.Quaternion.Graphene_Quaternion_T

   procedure To_Vec3
      (Self : not null access Graphene_Euler_T;
       Res  : not null access Graphene.Vec3.Graphene_Vec3_T);
   pragma Import (C, To_Vec3, "graphene_euler_to_vec3");
   --  Retrieves the angles of a Graphene.Euler.Graphene_Euler_T and
   --  initializes a Graphene.Vec3.Graphene_Vec3_T with them.
   --  Since: gtk+ 1.2
   --  @param Res return location for a Graphene.Vec3.Graphene_Vec3_T

end Graphene.Euler;
