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

package body Graphene.Frustum is

   --------------------
   -- Contains_Point --
   --------------------

   function Contains_Point
      (Self  : not null access Graphene_Frustum_T;
       Point : not null access Graphene.Point3d.Graphene_Point3D_T)
       return Boolean
   is
      function Internal
         (Self  : access Graphene_Frustum_T;
          Point : access Graphene.Point3d.Graphene_Point3D_T)
          return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_frustum_contains_point");
   begin
      return Internal (Self, Point) /= 0;
   end Contains_Point;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self : not null access Graphene_Frustum_T;
       B    : not null access Graphene_Frustum_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Frustum_T;
          B    : access Graphene_Frustum_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_frustum_equal");
   begin
      return Internal (Self, B) /= 0;
   end Equal;

   --------------------
   -- Intersects_Box --
   --------------------

   function Intersects_Box
      (Self : not null access Graphene_Frustum_T;
       Box  : not null access Graphene.Box.Graphene_Box_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Frustum_T;
          Box  : access Graphene.Box.Graphene_Box_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_frustum_intersects_box");
   begin
      return Internal (Self, Box) /= 0;
   end Intersects_Box;

   -----------------------
   -- Intersects_Sphere --
   -----------------------

   function Intersects_Sphere
      (Self   : not null access Graphene_Frustum_T;
       Sphere : not null access Graphene.Sphere.Graphene_Sphere_T)
       return Boolean
   is
      function Internal
         (Self   : access Graphene_Frustum_T;
          Sphere : access Graphene.Sphere.Graphene_Sphere_T)
          return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_frustum_intersects_sphere");
   begin
      return Internal (Self, Sphere) /= 0;
   end Intersects_Sphere;

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Graphene_Frustum_T) return Graphene_Frustum_T
   is
      Result : constant Graphene_Frustum_T := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

end Graphene.Frustum;
