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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");

package body Graphene.Sphere is

   ----------------------
   -- Init_From_Points --
   ----------------------

   function Init_From_Points
     (Self     : not null access Graphene_Sphere_T;
      Points   : Graphene.Point3d.Graphene_Point3d_Array;
      Center   : access Graphene_Point3d_T := null) return access Graphene_Sphere_T
   is
      function Internal
        (Self     : access Graphene_Sphere_T;
         N_Points : Guint;
         Points   : System.Address;
         Center   : access Graphene_Point3d_T) return access Graphene_Sphere_T;
      pragma Import (C, Internal, "graphene_sphere_init_from_points");
   begin
      return Internal (Self, Guint (Points'Length), Points'Address, Center);
   end Init_From_Points;

   -----------------------
   -- Init_From_Vectors --
   -----------------------

   function Init_From_Vectors
     (Self    : not null access Graphene_Sphere_T;
      Vectors : Graphene_Vec3_Array;
      Center  : access Graphene.Point3d.Graphene_Point3D_T)
   return access Graphene_Sphere_T
   is
      function Internal
        (Self      : not null access Graphene_Sphere_T;
         N_Vectors : Guint;
         Vectors   : System.Address;
         Center    : access Graphene.Point3d.Graphene_Point3D_T)
      return access Graphene_Sphere_T;
      pragma Import (C, Internal, "graphene_sphere_init_from_vectors");
   begin
      return Internal (Self, Guint (Vectors'Length), Vectors'Address, Center);
   end Init_From_Vectors;

   --------------------
   -- Contains_Point --
   --------------------

   function Contains_Point
      (Self  : not null access Graphene_Sphere_T;
       Point : not null access Graphene.Point3d.Graphene_Point3D_T)
       return Boolean
   is
      function Internal
         (Self  : access Graphene_Sphere_T;
          Point : access Graphene.Point3d.Graphene_Point3D_T)
          return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_sphere_contains_point");
   begin
      return Internal (Self, Point) /= 0;
   end Contains_Point;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self : not null access Graphene_Sphere_T;
       B    : not null access Graphene_Sphere_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Sphere_T;
          B    : access Graphene_Sphere_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_sphere_equal");
   begin
      return Internal (Self, B) /= 0;
   end Equal;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
      (Self : not null access Graphene_Sphere_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Sphere_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_sphere_is_empty");
   begin
      return Internal (Self) /= 0;
   end Is_Empty;

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Graphene_Sphere_T) return Graphene_Sphere_T
   is
      Result : constant Graphene_Sphere_T := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

end Graphene.Sphere;
