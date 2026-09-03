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

--  A sphere, represented by its center and radius.

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Graphene.Box;     use Graphene.Box;
with Graphene.Point3d; use Graphene.Point3d;
with Graphene.Vec3;    use Graphene.Vec3;
with Interfaces.C;     use Interfaces.C;

package Graphene.Sphere is

   type Graphene_Sphere_T is record
      Center : Graphene.Vec3.Graphene_Vec3_T;
      Radius : Interfaces.C.C_float;
   end record;
   pragma Convention (C, Graphene_Sphere_T);
   --  A sphere, represented by its center and radius.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_sphere_get_type");

   -------------
   -- Methods --
   -------------

   function Contains_Point
      (Self  : not null access Graphene_Sphere_T;
       Point : not null access Graphene.Point3d.Graphene_Point3D_T)
       return Boolean;
   --  Checks whether the given Point is contained in the volume of a
   --  Graphene.Sphere.Graphene_Sphere_T.
   --  Since: gtk+ 1.2
   --  @param Point a Graphene.Point3d.Graphene_Point3D_T
   --  @return `true` if the sphere contains the point

   function Distance
      (Self  : not null access Graphene_Sphere_T;
       Point : not null access Graphene.Point3d.Graphene_Point3D_T)
       return Interfaces.C.C_float;
   pragma Import (C, Distance, "graphene_sphere_distance");
   --  Computes the distance of the given Point from the surface of a
   --  Graphene.Sphere.Graphene_Sphere_T.
   --  Since: gtk+ 1.2
   --  @param Point a Graphene.Point3d.Graphene_Point3D_T
   --  @return the distance of the point

   function Equal
      (Self : not null access Graphene_Sphere_T;
       B    : not null access Graphene_Sphere_T) return Boolean;
   --  Checks whether two Graphene.Sphere.Graphene_Sphere_T are equal.
   --  Since: gtk+ 1.2
   --  @param B a Graphene.Sphere.Graphene_Sphere_T
   --  @return `true` if the spheres are equal

   procedure Free (Self : not null access Graphene_Sphere_T);
   pragma Import (C, Free, "graphene_sphere_free");
   --  Frees the resources allocated by graphene_sphere_alloc.
   --  Since: gtk+ 1.2

   procedure Get_Bounding_Box
      (Self : not null access Graphene_Sphere_T;
       Box  : not null access Graphene.Box.Graphene_Box_T);
   pragma Import (C, Get_Bounding_Box, "graphene_sphere_get_bounding_box");
   --  Computes the bounding box capable of containing the given
   --  Graphene.Sphere.Graphene_Sphere_T.
   --  Since: gtk+ 1.2
   --  @param Box return location for the bounding box

   procedure Get_Center
      (Self   : not null access Graphene_Sphere_T;
       Center : not null access Graphene.Point3d.Graphene_Point3D_T);
   pragma Import (C, Get_Center, "graphene_sphere_get_center");
   --  Retrieves the coordinates of the center of a
   --  Graphene.Sphere.Graphene_Sphere_T.
   --  Since: gtk+ 1.2
   --  @param Center return location for the coordinates of the center

   function Get_Radius
      (Self : not null access Graphene_Sphere_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Radius, "graphene_sphere_get_radius");
   --  Retrieves the radius of a Graphene.Sphere.Graphene_Sphere_T.
   --  Since: gtk+ 1.2

   function Init
      (Self   : not null access Graphene_Sphere_T;
       Center : access Graphene.Point3d.Graphene_Point3D_T;
       Radius : Interfaces.C.C_float) return access Graphene_Sphere_T;
   pragma Import (C, Init, "graphene_sphere_init");
   --  Initializes the given Graphene.Sphere.Graphene_Sphere_T with the given
   --  Center and Radius.
   --  Since: gtk+ 1.2
   --  @param Center the coordinates of the center of the sphere, or null for
   --  a center in (0, 0, 0)
   --  @param Radius the radius of the sphere
   --  @return the initialized Graphene.Sphere.Graphene_Sphere_T

   function Is_Empty
      (Self : not null access Graphene_Sphere_T) return Boolean;
   --  Checks whether the sphere has a zero radius.
   --  Since: gtk+ 1.2
   --  @return `true` if the sphere is empty

   procedure Translate
      (Self  : not null access Graphene_Sphere_T;
       Point : not null access Graphene.Point3d.Graphene_Point3D_T;
       Res   : not null access Graphene_Sphere_T);
   pragma Import (C, Translate, "graphene_sphere_translate");
   --  Translates the center of the given Graphene.Sphere.Graphene_Sphere_T
   --  using the Point coordinates as the delta of the translation.
   --  Since: gtk+ 1.2
   --  @param Point the coordinates of the translation
   --  @param Res return location for the translated sphere

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Graphene_Sphere_T) return Graphene_Sphere_T;
   pragma Inline (From_Object_Free);

   function Init_From_Points
     (Self     : not null access Graphene_Sphere_T;
      Points   : Graphene.Point3d.Graphene_Point3d_Array;
      Center   : access Graphene_Point3d_T := null) return access Graphene_Sphere_T;
   --  Initializes the given Graphene.Sphere.Graphene_Sphere_T using the given
   --  array of 3D coordinates so that the sphere includes them.
   --  The center of the sphere can either be specified, or will be center of
   --  the 3D volume that encompasses all Points.
   --  Since: gtk+ 1.2
   --  @param Points an array of graphene_point3d_t
   --  @param Center the center of the sphere
   --  @return the initialized Graphene.Sphere.Graphene_Sphere_T

   function Init_From_Vectors
     (Self    : not null access Graphene_Sphere_T;
      Vectors : Graphene_Vec3_Array;
      Center  : access Graphene.Point3d.Graphene_Point3D_T)
   return access Graphene_Sphere_T;
   --  Initializes the given Graphene.Sphere.Graphene_Sphere_T using the given
   --  array of 3D coordinates so that the sphere includes them.
   --  The center of the sphere can either be specified, or will be center of
   --  the 3D volume that encompasses all Vectors.
   --  Since: gtk+ 1.2
   --  @param Vectors an array of Graphene.Vec3.Graphene_Vec3_T
   --  @param Center the center of the sphere
   --  @return the initialized Graphene.Sphere.Graphene_Sphere_T

end Graphene.Sphere;
