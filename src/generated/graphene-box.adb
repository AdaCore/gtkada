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

package body Graphene.Box is

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Graphene_Box_T) return Graphene_Box_T
   is
      Result : constant Graphene_Box_T := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   ------------------
   -- Get_Vertices --
   ------------------

   procedure Get_Vertices
     (Self     : not null access Graphene_Box_T;
      Vertices : out Graphene_Vec3_Array8)
   is
      procedure Internal
        (Self     : not null access Graphene_Box_T;
         Vertices : System.Address);
      pragma Import (C, Internal, "graphene_box_get_vertices");

   begin
      Internal (Self, Vertices'Address);
   end Get_Vertices;

   ----------------------
   -- Init_From_Points --
   ----------------------

   function Init_From_Points
     (Self   : not null access Graphene_Box_T;
      Points : Graphene.Point3d.Graphene_Point3d_Array) return access Graphene_Box_T
   is
      function Internal
        (Self     : access Graphene_Box_T;
         N_Points : Guint;
         Points   : System.Address) return access Graphene_Box_T;
      pragma Import (C, Internal, "graphene_box_init_from_points");
   begin
      return Internal (Self, Guint (Points'Length), Points'Address);
   end Init_From_Points;

   -----------------------
   -- Init_From_Vectors --
   -----------------------

   function Init_From_Vectors
     (Self    : not null access Graphene_Box_T;
      Vectors : Graphene.Vec3.Graphene_Vec3_Array) return access Graphene_Box_T
   is
      function Internal
        (Self      : access Graphene_Box_T;
         N_Vectors : Guint;
         Vectors   : System.Address) return access Graphene_Box_T;
      pragma Import (C, Internal, "graphene_box_init_from_vectors");
   begin
      return Internal (Self, Guint (Vectors'Length), Vectors'Address);
   end Init_From_Vectors;

   ------------------
   -- Contains_Box --
   ------------------

   function Contains_Box
      (Self : not null access Graphene_Box_T;
       B    : not null access Graphene_Box_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Box_T;
          B    : access Graphene_Box_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_box_contains_box");
   begin
      return Internal (Self, B) /= 0;
   end Contains_Box;

   --------------------
   -- Contains_Point --
   --------------------

   function Contains_Point
      (Self  : not null access Graphene_Box_T;
       Point : not null access Graphene.Point3d.Graphene_Point3D_T)
       return Boolean
   is
      function Internal
         (Self  : access Graphene_Box_T;
          Point : access Graphene.Point3d.Graphene_Point3D_T)
          return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_box_contains_point");
   begin
      return Internal (Self, Point) /= 0;
   end Contains_Point;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self : not null access Graphene_Box_T;
       B    : not null access Graphene_Box_T) return Boolean
   is
      function Internal
         (Self : access Graphene_Box_T;
          B    : access Graphene_Box_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_box_equal");
   begin
      return Internal (Self, B) /= 0;
   end Equal;

   ------------------
   -- Intersection --
   ------------------

   function Intersection
      (Self : not null access Graphene_Box_T;
       B    : not null access Graphene_Box_T;
       Res  : access Graphene_Box_T := null) return Boolean
   is
      function Internal
         (Self : access Graphene_Box_T;
          B    : access Graphene_Box_T;
          Res  : access Graphene_Box_T) return Glib.Gboolean;
      pragma Import (C, Internal, "graphene_box_intersection");
   begin
      return Internal (Self, B, Res) /= 0;
   end Intersection;

end Graphene.Box;
