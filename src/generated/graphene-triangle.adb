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

package body Graphene.Triangle is

   function From_Object_Free (B : access Graphene_Triangle_T) return Graphene_Triangle_T is
      Result : constant Graphene_Triangle_T := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   --------------------
   -- Contains_Point --
   --------------------

   function Contains_Point
      (Self : not null access Graphene_Triangle_T;
       P    : not null access Graphene.Point3d.Graphene_Point3D_T)
       return Boolean
   is
      function Internal
         (Self : access Graphene_Triangle_T;
          P    : access Graphene.Point3d.Graphene_Point3D_T)
          return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_triangle_contains_point");
   begin
      return Internal (Self, P) /= 0;
   end Contains_Point;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self : not null access Graphene_Triangle_T;
       B    : not null access Graphene_Triangle_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Triangle_T;
          B    : access Graphene_Triangle_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_triangle_equal");
   begin
      return Internal (Self, B) /= 0;
   end Equal;

   --------------------
   -- Get_Barycoords --
   --------------------

   function Get_Barycoords
      (Self : not null access Graphene_Triangle_T;
       P    : access Graphene.Point3d.Graphene_Point3D_T;
       Res  : not null access Graphene.Vec2.Graphene_Vec2_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Triangle_T;
          P    : access Graphene.Point3d.Graphene_Point3D_T;
          Res  : access Graphene.Vec2.Graphene_Vec2_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_triangle_get_barycoords");
   begin
      return Internal (Self, P, Res) /= 0;
   end Get_Barycoords;

   ------------
   -- Get_Uv --
   ------------

   function Get_Uv
      (Self : not null access Graphene_Triangle_T;
       P    : access Graphene.Point3d.Graphene_Point3D_T;
       Uv_A : not null access Graphene.Vec2.Graphene_Vec2_T;
       Uv_B : not null access Graphene.Vec2.Graphene_Vec2_T;
       Uv_C : not null access Graphene.Vec2.Graphene_Vec2_T;
       Res  : not null access Graphene.Vec2.Graphene_Vec2_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Triangle_T;
          P    : access Graphene.Point3d.Graphene_Point3D_T;
          Uv_A : access Graphene.Vec2.Graphene_Vec2_T;
          Uv_B : access Graphene.Vec2.Graphene_Vec2_T;
          Uv_C : access Graphene.Vec2.Graphene_Vec2_T;
          Res  : access Graphene.Vec2.Graphene_Vec2_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_triangle_get_uv");
   begin
      return Internal (Self, P, Uv_A, Uv_B, Uv_C, Res) /= 0;
   end Get_Uv;

end Graphene.Triangle;
