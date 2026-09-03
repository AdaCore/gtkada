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

--  A triangle.

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Graphene.Box;     use Graphene.Box;
with Graphene.Plane;   use Graphene.Plane;
with Graphene.Point3d; use Graphene.Point3d;
with Graphene.Vec2;    use Graphene.Vec2;
with Graphene.Vec3;    use Graphene.Vec3;
with Interfaces.C;     use Interfaces.C;

package Graphene.Triangle is

   type Graphene_Triangle_T is record
      A : Graphene.Vec3.Graphene_Vec3_T;
      B : Graphene.Vec3.Graphene_Vec3_T;
      C : Graphene.Vec3.Graphene_Vec3_T;
   end record;
   pragma Convention (C, Graphene_Triangle_T);
   --  A triangle.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_triangle_get_type");

   -------------
   -- Methods --
   -------------

   function Contains_Point
      (Self : not null access Graphene_Triangle_T;
       P    : not null access Graphene.Point3d.Graphene_Point3D_T)
       return Boolean;
   --  Checks whether the given triangle T contains the point P.
   --  Since: gtk+ 1.2
   --  @param P a Graphene.Point3d.Graphene_Point3D_T
   --  @return `true` if the point is inside the triangle

   function Equal
      (Self : not null access Graphene_Triangle_T;
       B    : not null access Graphene_Triangle_T) return Boolean;
   --  Checks whether the two given Graphene.Triangle.Graphene_Triangle_T are
   --  equal.
   --  Since: gtk+ 1.2
   --  @param B a Graphene.Triangle.Graphene_Triangle_T
   --  @return `true` if the triangles are equal

   procedure Free (Self : not null access Graphene_Triangle_T);
   pragma Import (C, Free, "graphene_triangle_free");
   --  Frees the resources allocated by graphene_triangle_alloc.
   --  Since: gtk+ 1.2

   function Get_Area
      (Self : not null access Graphene_Triangle_T)
       return Interfaces.C.C_float;
   pragma Import (C, Get_Area, "graphene_triangle_get_area");
   --  Computes the area of the given Graphene.Triangle.Graphene_Triangle_T.
   --  Since: gtk+ 1.2
   --  @return the area of the triangle

   function Get_Barycoords
      (Self : not null access Graphene_Triangle_T;
       P    : access Graphene.Point3d.Graphene_Point3D_T;
       Res  : not null access Graphene.Vec2.Graphene_Vec2_T) return Boolean;
   --  Computes the [barycentric
   --  coordinates](http://en.wikipedia.org/wiki/Barycentric_coordinate_system)
   --  of the given point P.
   --  The point P must lie on the same plane as the triangle T; if the point
   --  is not coplanar, the result of this function is undefined.
   --  If we place the origin in the coordinates of the triangle's A point,
   --  the barycentric coordinates are `u`, which is on the AC vector; and `v`
   --  which is on the AB vector:
   --  ![](triangle-barycentric.png)
   --  The returned Graphene.Vec2.Graphene_Vec2_T contains the following
   --  values, in order:
   --  - `res.x = u` - `res.y = v`
   --  Since: gtk+ 1.2
   --  @param P a Graphene.Point3d.Graphene_Point3D_T
   --  @param Res return location for the vector with the barycentric
   --  coordinates
   --  @return `true` if the barycentric coordinates are valid

   procedure Get_Bounding_Box
      (Self : not null access Graphene_Triangle_T;
       Res  : not null access Graphene.Box.Graphene_Box_T);
   pragma Import (C, Get_Bounding_Box, "graphene_triangle_get_bounding_box");
   --  Computes the bounding box of the given
   --  Graphene.Triangle.Graphene_Triangle_T.
   --  Since: gtk+ 1.2
   --  @param Res return location for the box

   procedure Get_Midpoint
      (Self : not null access Graphene_Triangle_T;
       Res  : not null access Graphene.Point3d.Graphene_Point3D_T);
   pragma Import (C, Get_Midpoint, "graphene_triangle_get_midpoint");
   --  Computes the coordinates of the midpoint of the given
   --  Graphene.Triangle.Graphene_Triangle_T.
   --  The midpoint G is the
   --  [centroid](https://en.wikipedia.org/wiki/CentroidTriangle_centroid) of
   --  the triangle, i.e. the intersection of its medians.
   --  Since: gtk+ 1.2
   --  @param Res return location for the coordinates of the midpoint

   procedure Get_Normal
      (Self : not null access Graphene_Triangle_T;
       Res  : not null access Graphene.Vec3.Graphene_Vec3_T);
   pragma Import (C, Get_Normal, "graphene_triangle_get_normal");
   --  Computes the normal vector of the given
   --  Graphene.Triangle.Graphene_Triangle_T.
   --  Since: gtk+ 1.2
   --  @param Res return location for the normal vector

   procedure Get_Plane
      (Self : not null access Graphene_Triangle_T;
       Res  : not null access Graphene.Plane.Graphene_Plane_T);
   pragma Import (C, Get_Plane, "graphene_triangle_get_plane");
   --  Computes the plane based on the vertices of the given
   --  Graphene.Triangle.Graphene_Triangle_T.
   --  Since: gtk+ 1.2
   --  @param Res return location for the plane

   procedure Get_Points
      (Self : not null access Graphene_Triangle_T;
       A    : not null access Graphene.Point3d.Graphene_Point3D_T;
       B    : not null access Graphene.Point3d.Graphene_Point3D_T;
       C    : not null access Graphene.Point3d.Graphene_Point3D_T);
   pragma Import (C, Get_Points, "graphene_triangle_get_points");
   --  Retrieves the three vertices of the given
   --  Graphene.Triangle.Graphene_Triangle_T and returns their coordinates as
   --  Graphene.Point3d.Graphene_Point3D_T.
   --  Since: gtk+ 1.2
   --  @param A return location for the coordinates of the first vertex
   --  @param B return location for the coordinates of the second vertex
   --  @param C return location for the coordinates of the third vertex

   function Get_Uv
      (Self : not null access Graphene_Triangle_T;
       P    : access Graphene.Point3d.Graphene_Point3D_T;
       Uv_A : not null access Graphene.Vec2.Graphene_Vec2_T;
       Uv_B : not null access Graphene.Vec2.Graphene_Vec2_T;
       Uv_C : not null access Graphene.Vec2.Graphene_Vec2_T;
       Res  : not null access Graphene.Vec2.Graphene_Vec2_T) return Boolean;
   --  Computes the UV coordinates of the given point P.
   --  The point P must lie on the same plane as the triangle T; if the point
   --  is not coplanar, the result of this function is undefined. If P is null,
   --  the point will be set in (0, 0, 0).
   --  The UV coordinates will be placed in the Res vector:
   --  - `res.x = u` - `res.y = v`
   --  See also: Graphene.Triangle.Get_Barycoords
   --  Since: gtk+ 1.10
   --  @param P a Graphene.Point3d.Graphene_Point3D_T
   --  @param Uv_A the UV coordinates of the first point
   --  @param Uv_B the UV coordinates of the second point
   --  @param Uv_C the UV coordinates of the third point
   --  @param Res a vector containing the UV coordinates of the given point P
   --  @return `true` if the coordinates are valid

   procedure Get_Vertices
      (Self : not null access Graphene_Triangle_T;
       A    : not null access Graphene.Vec3.Graphene_Vec3_T;
       B    : not null access Graphene.Vec3.Graphene_Vec3_T;
       C    : not null access Graphene.Vec3.Graphene_Vec3_T);
   pragma Import (C, Get_Vertices, "graphene_triangle_get_vertices");
   --  Retrieves the three vertices of the given
   --  Graphene.Triangle.Graphene_Triangle_T.
   --  Since: gtk+ 1.2
   --  @param A return location for the first vertex
   --  @param B return location for the second vertex
   --  @param C return location for the third vertex

   function Init_From_Point3D
      (Self : not null access Graphene_Triangle_T;
       A    : access Graphene.Point3d.Graphene_Point3D_T;
       B    : access Graphene.Point3d.Graphene_Point3D_T;
       C    : access Graphene.Point3d.Graphene_Point3D_T)
       return access Graphene_Triangle_T;
   pragma Import (C, Init_From_Point3D, "graphene_triangle_init_from_point3d");
   --  Initializes a Graphene.Triangle.Graphene_Triangle_T using the three
   --  given 3D points.
   --  Since: gtk+ 1.2
   --  @param A a Graphene.Point3d.Graphene_Point3D_T
   --  @param B a Graphene.Point3d.Graphene_Point3D_T
   --  @param C a Graphene.Point3d.Graphene_Point3D_T
   --  @return the initialized Graphene.Triangle.Graphene_Triangle_T

   function Init_From_Vec3
      (Self : not null access Graphene_Triangle_T;
       A    : access Graphene.Vec3.Graphene_Vec3_T;
       B    : access Graphene.Vec3.Graphene_Vec3_T;
       C    : access Graphene.Vec3.Graphene_Vec3_T)
       return access Graphene_Triangle_T;
   pragma Import (C, Init_From_Vec3, "graphene_triangle_init_from_vec3");
   --  Initializes a Graphene.Triangle.Graphene_Triangle_T using the three
   --  given vectors.
   --  Since: gtk+ 1.2
   --  @param A a Graphene.Vec3.Graphene_Vec3_T
   --  @param B a Graphene.Vec3.Graphene_Vec3_T
   --  @param C a Graphene.Vec3.Graphene_Vec3_T
   --  @return the initialized Graphene.Triangle.Graphene_Triangle_T

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Graphene_Triangle_T) return Graphene_Triangle_T;
   pragma Inline (From_Object_Free);

end Graphene.Triangle;
