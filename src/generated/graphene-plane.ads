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

--  A 2D plane that extends infinitely in a 3D volume.
--
--  The contents of the `graphene_plane_t` are private, and should not be
--  modified directly.

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
limited with Graphene.Matrix;
with Graphene.Point3d; use Graphene.Point3d;
with Graphene.Vec3;    use Graphene.Vec3;
with Graphene.Vec4;    use Graphene.Vec4;
with Interfaces.C;     use Interfaces.C;

package Graphene.Plane is

   type Graphene_Plane_T is record
      Normal : Graphene.Vec3.Graphene_Vec3_T;
      The_Constant : Interfaces.C.C_float;
   end record;
   pragma Convention (C, Graphene_Plane_T);
   --  A 2D plane that extends infinitely in a 3D volume.
   --
   --  The contents of the `graphene_plane_t` are private, and should not be
   --  modified directly.

   type Graphene_Plane_Array is array (Natural range <>) of Graphene_Plane_T;
   pragma Convention (C, Graphene_Plane_Array);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_plane_get_type");

   -------------
   -- Methods --
   -------------

   function Distance
      (Self  : not null access Graphene_Plane_T;
       Point : not null access Graphene.Point3d.Graphene_Point3D_T)
       return Interfaces.C.C_float;
   pragma Import (C, Distance, "graphene_plane_distance");
   --  Computes the distance of Point from a Graphene.Plane.Graphene_Plane_T.
   --  Since: gtk+ 1.2
   --  @param Point a Graphene.Point3d.Graphene_Point3D_T
   --  @return the distance of the given Graphene.Point3d.Graphene_Point3D_T
   --  from the plane

   function Equal
      (Self : not null access Graphene_Plane_T;
       B    : not null access Graphene_Plane_T) return Boolean;
   --  Checks whether the two given Graphene.Plane.Graphene_Plane_T are equal.
   --  Since: gtk+ 1.2
   --  @param B a Graphene.Plane.Graphene_Plane_T
   --  @return `true` if the given planes are equal

   procedure Free (Self : not null access Graphene_Plane_T);
   pragma Import (C, Free, "graphene_plane_free");
   --  Frees the resources allocated by graphene_plane_alloc.
   --  Since: gtk+ 1.2

   function Get_Constant
      (Self : not null access Graphene_Plane_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Constant, "graphene_plane_get_constant");
   --  Retrieves the distance along the normal vector of the given
   --  Graphene.Plane.Graphene_Plane_T from the origin.
   --  Since: gtk+ 1.2
   --  @return the constant value of the plane

   procedure Get_Normal
      (Self   : not null access Graphene_Plane_T;
       Normal : not null access Graphene.Vec3.Graphene_Vec3_T);
   pragma Import (C, Get_Normal, "graphene_plane_get_normal");
   --  Retrieves the normal vector pointing towards the origin of the given
   --  Graphene.Plane.Graphene_Plane_T.
   --  Since: gtk+ 1.2
   --  @param Normal return location for the normal vector

   function Init
      (Self         : not null access Graphene_Plane_T;
       Normal       : access Graphene.Vec3.Graphene_Vec3_T;
       The_Constant : Interfaces.C.C_float) return access Graphene_Plane_T;
   pragma Import (C, Init, "graphene_plane_init");
   --  Initializes the given Graphene.Plane.Graphene_Plane_T using the given
   --  Normal vector and Constant values.
   --  Since: gtk+ 1.2
   --  @param Normal a unit length normal vector defining the plane pointing
   --  towards the origin; if unset, we use the X axis by default
   --  @param The_Constant the distance from the origin to the plane along the
   --  normal vector; the sign determines the half-space occupied by the plane
   --  @return the initialized plane

   function Init_From_Point
      (Self   : not null access Graphene_Plane_T;
       Normal : not null access Graphene.Vec3.Graphene_Vec3_T;
       Point  : not null access Graphene.Point3d.Graphene_Point3D_T)
       return access Graphene_Plane_T;
   pragma Import (C, Init_From_Point, "graphene_plane_init_from_point");
   --  Initializes the given Graphene.Plane.Graphene_Plane_T using the given
   --  normal vector and an arbitrary co-planar point.
   --  Since: gtk+ 1.2
   --  @param Normal a normal vector defining the plane pointing towards the
   --  origin
   --  @param Point a Graphene.Point3d.Graphene_Point3D_T
   --  @return the initialized plane

   function Init_From_Points
      (Self : not null access Graphene_Plane_T;
       A    : not null access Graphene.Point3d.Graphene_Point3D_T;
       B    : not null access Graphene.Point3d.Graphene_Point3D_T;
       C    : not null access Graphene.Point3d.Graphene_Point3D_T)
       return access Graphene_Plane_T;
   pragma Import (C, Init_From_Points, "graphene_plane_init_from_points");
   --  Initializes the given Graphene.Plane.Graphene_Plane_T using the 3
   --  provided co-planar points.
   --  The winding order is counter-clockwise, and determines which direction
   --  the normal vector will point.
   --  Since: gtk+ 1.2
   --  @param A a Graphene.Point3d.Graphene_Point3D_T
   --  @param B a Graphene.Point3d.Graphene_Point3D_T
   --  @param C a Graphene.Point3d.Graphene_Point3D_T
   --  @return the initialized plane

   function Init_From_Vec4
      (Self : not null access Graphene_Plane_T;
       Src  : not null access Graphene.Vec4.Graphene_Vec4_T)
       return access Graphene_Plane_T;
   pragma Import (C, Init_From_Vec4, "graphene_plane_init_from_vec4");
   --  Initializes the given Graphene.Plane.Graphene_Plane_T using the
   --  components of the given Graphene.Vec4.Graphene_Vec4_T vector.
   --  Since: gtk+ 1.2
   --  @param Src a Graphene.Vec4.Graphene_Vec4_T containing the normal vector
   --  in its first three components, and the distance in its fourth component
   --  @return the initialized plane

   procedure Negate
      (Self : not null access Graphene_Plane_T;
       Res  : not null access Graphene_Plane_T);
   pragma Import (C, Negate, "graphene_plane_negate");
   --  Negates the normal vector and constant of a
   --  Graphene.Plane.Graphene_Plane_T, effectively mirroring the plane across
   --  the origin.
   --  Since: gtk+ 1.2
   --  @param Res return location for the negated plane

   procedure Normalize
      (Self : not null access Graphene_Plane_T;
       Res  : not null access Graphene_Plane_T);
   pragma Import (C, Normalize, "graphene_plane_normalize");
   --  Normalizes the vector of the given Graphene.Plane.Graphene_Plane_T, and
   --  adjusts the constant accordingly.
   --  Since: gtk+ 1.2
   --  @param Res return location for the normalized plane

   procedure Transform
      (Self          : not null access Graphene_Plane_T;
       Matrix        : not null access Graphene.Matrix.Graphene_Matrix_T;
       Normal_Matrix : access Graphene.Matrix.Graphene_Matrix_T;
       Res           : not null access Graphene_Plane_T);
   pragma Import (C, Transform, "graphene_plane_transform");
   --  Transforms a Graphene.Plane.Graphene_Plane_T P using the given Matrix
   --  and Normal_Matrix.
   --  If Normal_Matrix is null, a transformation matrix for the plane normal
   --  will be computed from Matrix. If you are transforming multiple planes
   --  using the same Matrix it's recommended to compute the normal matrix
   --  beforehand to avoid incurring in the cost of recomputing it every time.
   --  Since: gtk+ 1.10
   --  @param Matrix a Graphene.Matrix.Graphene_Matrix_T
   --  @param Normal_Matrix a Graphene.Matrix.Graphene_Matrix_T
   --  @param Res the transformed plane

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Graphene_Plane_T) return Graphene_Plane_T;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

end Graphene.Plane;
