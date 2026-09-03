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

--  A 3D volume delimited by 2D clip planes.
--
--  The contents of the `graphene_frustum_t` are private, and should not be
--  modified directly.

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Graphene.Box;     use Graphene.Box;
with Graphene.Matrix;  use Graphene.Matrix;
with Graphene.Plane;   use Graphene.Plane;
with Graphene.Point3d; use Graphene.Point3d;
with Graphene.Sphere;  use Graphene.Sphere;

package Graphene.Frustum is

   type Graphene_Frustum_T is record
      Planes : Graphene_Plane_Array (1 .. 6);
   end record;
   pragma Convention (C, Graphene_Frustum_T);
   --  A 3D volume delimited by 2D clip planes.
   --
   --  The contents of the `graphene_frustum_t` are private, and should not be
   --  modified directly.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_frustum_get_type");

   -------------
   -- Methods --
   -------------

   function Contains_Point
      (Self  : not null access Graphene_Frustum_T;
       Point : not null access Graphene.Point3d.Graphene_Point3D_T)
       return Boolean;
   --  Checks whether a point is inside the volume defined by the given
   --  Graphene.Frustum.Graphene_Frustum_T.
   --  Since: gtk+ 1.2
   --  @param Point a Graphene.Point3d.Graphene_Point3D_T
   --  @return `true` if the point is inside the frustum

   function Equal
      (Self : not null access Graphene_Frustum_T;
       B    : not null access Graphene_Frustum_T) return Boolean;
   --  Checks whether the two given Graphene.Frustum.Graphene_Frustum_T are
   --  equal.
   --  Since: gtk+ 1.6
   --  @param B a Graphene.Frustum.Graphene_Frustum_T
   --  @return `true` if the given frustums are equal

   procedure Free (Self : not null access Graphene_Frustum_T);
   pragma Import (C, Free, "graphene_frustum_free");
   --  Frees the resources allocated by graphene_frustum_alloc.
   --  Since: gtk+ 1.2

   function Init
      (Self : not null access Graphene_Frustum_T;
       P0   : not null access Graphene.Plane.Graphene_Plane_T;
       P1   : not null access Graphene.Plane.Graphene_Plane_T;
       P2   : not null access Graphene.Plane.Graphene_Plane_T;
       P3   : not null access Graphene.Plane.Graphene_Plane_T;
       P4   : not null access Graphene.Plane.Graphene_Plane_T;
       P5   : not null access Graphene.Plane.Graphene_Plane_T)
       return access Graphene_Frustum_T;
   pragma Import (C, Init, "graphene_frustum_init");
   --  Initializes the given Graphene.Frustum.Graphene_Frustum_T using the
   --  provided clipping planes.
   --  Since: gtk+ 1.2
   --  @param P0 a clipping plane
   --  @param P1 a clipping plane
   --  @param P2 a clipping plane
   --  @param P3 a clipping plane
   --  @param P4 a clipping plane
   --  @param P5 a clipping plane
   --  @return the initialized frustum

   function Init_From_Frustum
      (Self : not null access Graphene_Frustum_T;
       Src  : not null access Graphene_Frustum_T)
       return access Graphene_Frustum_T;
   pragma Import (C, Init_From_Frustum, "graphene_frustum_init_from_frustum");
   --  Initializes the given Graphene.Frustum.Graphene_Frustum_T using the
   --  clipping planes of another Graphene.Frustum.Graphene_Frustum_T.
   --  Since: gtk+ 1.2
   --  @param Src a Graphene.Frustum.Graphene_Frustum_T
   --  @return the initialized frustum

   function Init_From_Matrix
      (Self   : not null access Graphene_Frustum_T;
       Matrix : not null access Graphene.Matrix.Graphene_Matrix_T)
       return access Graphene_Frustum_T;
   pragma Import (C, Init_From_Matrix, "graphene_frustum_init_from_matrix");
   --  Initializes a Graphene.Frustum.Graphene_Frustum_T using the given
   --  Matrix.
   --  Since: gtk+ 1.2
   --  @param Matrix a Graphene.Matrix.Graphene_Matrix_T
   --  @return the initialized frustum

   function Intersects_Box
      (Self : not null access Graphene_Frustum_T;
       Box  : not null access Graphene.Box.Graphene_Box_T) return Boolean;
   --  Checks whether the given Box intersects a plane of a
   --  Graphene.Frustum.Graphene_Frustum_T.
   --  Since: gtk+ 1.2
   --  @param Box a Graphene.Box.Graphene_Box_T
   --  @return `true` if the box intersects the frustum

   function Intersects_Sphere
      (Self   : not null access Graphene_Frustum_T;
       Sphere : not null access Graphene.Sphere.Graphene_Sphere_T)
       return Boolean;
   --  Checks whether the given Sphere intersects a plane of a
   --  Graphene.Frustum.Graphene_Frustum_T.
   --  Since: gtk+ 1.2
   --  @param Sphere a Graphene.Sphere.Graphene_Sphere_T
   --  @return `true` if the sphere intersects the frustum

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Graphene_Frustum_T) return Graphene_Frustum_T;
   pragma Inline (From_Object_Free);

   type Graphene_Plane_Array6 is array (1 .. 6) of Graphene_Plane_T;
   pragma Convention (C, Graphene_Plane_Array6);

   procedure Get_Planes
     (Self   : not null access Graphene_Frustum_T;
      Planes : out Graphene_Plane_Array6);
   pragma Import (C, Get_Planes, "graphene_frustum_get_planes");
   --  Retrieves the planes that define the given
   --  Graphene.Frustum.Graphene_Frustum_T.
   --  Since: gtk+ 1.2
   --  @param Planes return location for an array of 6
   --  Graphene.Plane.Graphene_Plane_T

end Graphene.Frustum;
