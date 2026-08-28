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

--  A ray emitted from an origin in a given direction.
--
--  The contents of the `graphene_ray_t` structure are private, and should not
--  be modified directly.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Graphene.Box;            use Graphene.Box;
with Graphene.Plane;          use Graphene.Plane;
with Graphene.Point3d;        use Graphene.Point3d;
with Graphene.Sphere;         use Graphene.Sphere;
with Graphene.Triangle;       use Graphene.Triangle;
with Graphene.Vec3;           use Graphene.Vec3;
with Interfaces.C;            use Interfaces.C;

package Graphene.Ray is

   type graphene_ray_intersection_kind_t is (
      Graphene_Ray_Intersection_Kind_None,
      Graphene_Ray_Intersection_Kind_Enter,
      Graphene_Ray_Intersection_Kind_Leave);
   pragma Convention (C, graphene_ray_intersection_kind_t);
   --  The type of intersection.

   type Graphene_Ray_T is record
      Origin : Graphene.Vec3.Graphene_Vec3_T;
      Direction : Graphene.Vec3.Graphene_Vec3_T;
   end record;
   pragma Convention (C, Graphene_Ray_T);

   function From_Object_Free
     (B : not null access Graphene_Ray_T) return Graphene_Ray_T;
   pragma Inline (From_Object_Free);
   --  A ray emitted from an origin in a given direction.
   --
   --  The contents of the `graphene_ray_t` structure are private, and should
   --  not be modified directly.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package graphene_ray_intersection_kind_t_Properties is
      new Generic_Internal_Discrete_Property (graphene_ray_intersection_kind_t);
   type Property_graphene_ray_intersection_kind_t is new graphene_ray_intersection_kind_t_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_ray_get_type");

   -------------
   -- Methods --
   -------------

   function Equal
      (Self : not null access Graphene_Ray_T;
       B    : not null access Graphene_Ray_T) return Boolean;
   --  Checks whether the two given Graphene.Ray.Graphene_Ray_T are equal.
   --  Since: gtk+ 1.4
   --  @param B a Graphene.Ray.Graphene_Ray_T
   --  @return `true` if the given rays are equal

   procedure Free (Self : not null access Graphene_Ray_T);
   pragma Import (C, Free, "graphene_ray_free");
   --  Frees the resources allocated by graphene_ray_alloc.
   --  Since: gtk+ 1.4

   procedure Get_Closest_Point_To_Point
      (Self : not null access Graphene_Ray_T;
       P    : not null access Graphene.Point3d.Graphene_Point3D_T;
       Res  : not null access Graphene.Point3d.Graphene_Point3D_T);
   pragma Import (C, Get_Closest_Point_To_Point, "graphene_ray_get_closest_point_to_point");
   --  Computes the point on the given Graphene.Ray.Graphene_Ray_T that is
   --  closest to the given point P.
   --  Since: gtk+ 1.4
   --  @param P a Graphene.Point3d.Graphene_Point3D_T
   --  @param Res return location for the closest point3d

   procedure Get_Direction
      (Self      : not null access Graphene_Ray_T;
       Direction : not null access Graphene.Vec3.Graphene_Vec3_T);
   pragma Import (C, Get_Direction, "graphene_ray_get_direction");
   --  Retrieves the direction of the given Graphene.Ray.Graphene_Ray_T.
   --  Since: gtk+ 1.4
   --  @param Direction return location for the direction

   function Get_Distance_To_Plane
      (Self : not null access Graphene_Ray_T;
       P    : not null access Graphene.Plane.Graphene_Plane_T)
       return Interfaces.C.C_float;
   pragma Import (C, Get_Distance_To_Plane, "graphene_ray_get_distance_to_plane");
   --  Computes the distance of the origin of the given
   --  Graphene.Ray.Graphene_Ray_T from the given plane.
   --  If the ray does not intersect the plane, this function returns
   --  `INFINITY`.
   --  Since: gtk+ 1.4
   --  @param P a Graphene.Plane.Graphene_Plane_T
   --  @return the distance of the origin of the ray from the plane

   function Get_Distance_To_Point
      (Self : not null access Graphene_Ray_T;
       P    : not null access Graphene.Point3d.Graphene_Point3D_T)
       return Interfaces.C.C_float;
   pragma Import (C, Get_Distance_To_Point, "graphene_ray_get_distance_to_point");
   --  Computes the distance of the closest approach between the given
   --  Graphene.Ray.Graphene_Ray_T R and the point P.
   --  The closest approach to a ray from a point is the distance between the
   --  point and the projection of the point on the ray itself.
   --  Since: gtk+ 1.4
   --  @param P a Graphene.Point3d.Graphene_Point3D_T
   --  @return the distance of the point

   procedure Get_Origin
      (Self   : not null access Graphene_Ray_T;
       Origin : not null access Graphene.Point3d.Graphene_Point3D_T);
   pragma Import (C, Get_Origin, "graphene_ray_get_origin");
   --  Retrieves the origin of the given Graphene.Ray.Graphene_Ray_T.
   --  Since: gtk+ 1.4
   --  @param Origin return location for the origin

   procedure Get_Position_At
      (Self     : not null access Graphene_Ray_T;
       T        : Interfaces.C.C_float;
       Position : not null access Graphene.Point3d.Graphene_Point3D_T);
   pragma Import (C, Get_Position_At, "graphene_ray_get_position_at");
   --  Retrieves the coordinates of a point at the distance T along the given
   --  Graphene.Ray.Graphene_Ray_T.
   --  Since: gtk+ 1.4
   --  @param T the distance along the ray
   --  @param Position return location for the position

   function Init
      (Self      : not null access Graphene_Ray_T;
       Origin    : access Graphene.Point3d.Graphene_Point3D_T;
       Direction : access Graphene.Vec3.Graphene_Vec3_T)
       return access Graphene_Ray_T;
   pragma Import (C, Init, "graphene_ray_init");
   --  Initializes the given Graphene.Ray.Graphene_Ray_T using the given
   --  Origin and Direction values.
   --  Since: gtk+ 1.4
   --  @param Origin the origin of the ray
   --  @param Direction the direction vector
   --  @return the initialized ray

   function Init_From_Ray
      (Self : not null access Graphene_Ray_T;
       Src  : not null access Graphene_Ray_T) return access Graphene_Ray_T;
   pragma Import (C, Init_From_Ray, "graphene_ray_init_from_ray");
   --  Initializes the given Graphene.Ray.Graphene_Ray_T using the origin and
   --  direction values of another Graphene.Ray.Graphene_Ray_T.
   --  Since: gtk+ 1.4
   --  @param Src a Graphene.Ray.Graphene_Ray_T
   --  @return the initialized ray

   function Init_From_Vec3
      (Self      : not null access Graphene_Ray_T;
       Origin    : access Graphene.Vec3.Graphene_Vec3_T;
       Direction : access Graphene.Vec3.Graphene_Vec3_T)
       return access Graphene_Ray_T;
   pragma Import (C, Init_From_Vec3, "graphene_ray_init_from_vec3");
   --  Initializes the given Graphene.Ray.Graphene_Ray_T using the given
   --  vectors.
   --  Since: gtk+ 1.4
   --  @param Origin a Graphene.Vec3.Graphene_Vec3_T
   --  @param Direction a Graphene.Vec3.Graphene_Vec3_T
   --  @return the initialized ray

   function Intersect_Box
      (Self  : not null access Graphene_Ray_T;
       B     : not null access Graphene.Box.Graphene_Box_T;
       T_Out : out Interfaces.C.C_float)
       return graphene_ray_intersection_kind_t;
   pragma Import (C, Intersect_Box, "graphene_ray_intersect_box");
   --  Intersects the given Graphene.Ray.Graphene_Ray_T R with the given
   --  Graphene.Box.Graphene_Box_T B.
   --  Since: gtk+ 1.10
   --  @param B a Graphene.Box.Graphene_Box_T
   --  @param T_Out the distance of the point on the ray that intersects the
   --  box
   --  @return the type of intersection

   function Intersect_Sphere
      (Self  : not null access Graphene_Ray_T;
       S     : not null access Graphene.Sphere.Graphene_Sphere_T;
       T_Out : out Interfaces.C.C_float)
       return graphene_ray_intersection_kind_t;
   pragma Import (C, Intersect_Sphere, "graphene_ray_intersect_sphere");
   --  Intersects the given Graphene.Ray.Graphene_Ray_T R with the given
   --  Graphene.Sphere.Graphene_Sphere_T S.
   --  Since: gtk+ 1.10
   --  @param S a Graphene.Sphere.Graphene_Sphere_T
   --  @param T_Out the distance of the point on the ray that intersects the
   --  sphere
   --  @return the type of intersection

   function Intersect_Triangle
      (Self  : not null access Graphene_Ray_T;
       T     : not null access Graphene.Triangle.Graphene_Triangle_T;
       T_Out : out Interfaces.C.C_float)
       return graphene_ray_intersection_kind_t;
   pragma Import (C, Intersect_Triangle, "graphene_ray_intersect_triangle");
   --  Intersects the given Graphene.Ray.Graphene_Ray_T R with the given
   --  Graphene.Triangle.Graphene_Triangle_T T.
   --  Since: gtk+ 1.10
   --  @param T a Graphene.Triangle.Graphene_Triangle_T
   --  @param T_Out the distance of the point on the ray that intersects the
   --  triangle
   --  @return the type of intersection

   function Intersects_Box
      (Self : not null access Graphene_Ray_T;
       B    : not null access Graphene.Box.Graphene_Box_T) return Boolean;
   --  Checks whether the given Graphene.Ray.Graphene_Ray_T R intersects the
   --  given Graphene.Box.Graphene_Box_T B.
   --  See also: Graphene.Ray.Intersect_Box
   --  Since: gtk+ 1.10
   --  @param B a Graphene.Box.Graphene_Box_T
   --  @return `true` if the ray intersects the box

   function Intersects_Sphere
      (Self : not null access Graphene_Ray_T;
       S    : not null access Graphene.Sphere.Graphene_Sphere_T)
       return Boolean;
   --  Checks if the given Graphene.Ray.Graphene_Ray_T R intersects the given
   --  Graphene.Sphere.Graphene_Sphere_T S.
   --  See also: Graphene.Ray.Intersect_Sphere
   --  Since: gtk+ 1.10
   --  @param S a Graphene.Sphere.Graphene_Sphere_T
   --  @return `true` if the ray intersects the sphere

   function Intersects_Triangle
      (Self : not null access Graphene_Ray_T;
       T    : not null access Graphene.Triangle.Graphene_Triangle_T)
       return Boolean;
   --  Checks whether the given Graphene.Ray.Graphene_Ray_T R intersects the
   --  given Graphene.Triangle.Graphene_Triangle_T B.
   --  See also: Graphene.Ray.Intersect_Triangle
   --  Since: gtk+ 1.10
   --  @param T a Graphene.Triangle.Graphene_Triangle_T
   --  @return `true` if the ray intersects the triangle

end Graphene.Ray;
