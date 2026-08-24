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

package body Graphene.Ray is

   function From_Object_Free (B : access Graphene_Ray_T) return Graphene_Ray_T is
      Result : constant Graphene_Ray_T := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self : not null access Graphene_Ray_T;
       B    : not null access Graphene_Ray_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Ray_T;
          B    : access Graphene_Ray_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_ray_equal");
   begin
      return Internal (Self, B) /= 0;
   end Equal;

   --------------------
   -- Intersects_Box --
   --------------------

   function Intersects_Box
      (Self : not null access Graphene_Ray_T;
       B    : not null access Graphene.Box.Graphene_Box_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Ray_T;
          B    : access Graphene.Box.Graphene_Box_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_ray_intersects_box");
   begin
      return Internal (Self, B) /= 0;
   end Intersects_Box;

   -----------------------
   -- Intersects_Sphere --
   -----------------------

   function Intersects_Sphere
      (Self : not null access Graphene_Ray_T;
       S    : not null access Graphene.Sphere.Graphene_Sphere_T)
       return Boolean
   is
      function Internal
         (Self : access Graphene_Ray_T;
          S    : access Graphene.Sphere.Graphene_Sphere_T)
          return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_ray_intersects_sphere");
   begin
      return Internal (Self, S) /= 0;
   end Intersects_Sphere;

   -------------------------
   -- Intersects_Triangle --
   -------------------------

   function Intersects_Triangle
      (Self : not null access Graphene_Ray_T;
       T    : not null access Graphene.Triangle.Graphene_Triangle_T)
       return Boolean
   is
      function Internal
         (Self : access Graphene_Ray_T;
          T    : access Graphene.Triangle.Graphene_Triangle_T)
          return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_ray_intersects_triangle");
   begin
      return Internal (Self, T) /= 0;
   end Intersects_Triangle;

end Graphene.Ray;
