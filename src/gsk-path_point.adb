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

with Gsk.Path;
with Gsk.Path_Measure;

package body Gsk.Path_Point is
   use type Glib.Gboolean;
   function Equal (Self, Other : access Gsk_Path_Point) return Boolean is
      function Internal
        (Self, Other : access Gsk_Path_Point) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_path_point_equal");
   begin
      return (Internal (Self, Other) /= 0);
   end Equal;
   procedure Get_Position
     (Self     :     access Gsk_Path_Point; Path : Gsk.Path.Gsk_Path;
      Position : out Graphene.Point.Graphene_Point_T)
   is
      procedure Internal
        (Self     :     access Gsk_Path_Point; Path : System.Address;
         Position : out Graphene.Point.Graphene_Point_T);
      pragma Import (C, Internal, "gsk_path_point_get_position");
   begin
      Internal (Self, Glib.Get_Object (Path), Position);
   end Get_Position;
   procedure Get_Tangent
     (Self      :     access Gsk_Path_Point; Path : Gsk.Path.Gsk_Path;
      Direction :     Gsk.Enums.Gsk_Path_Direction;
      Tangent   : out Graphene.Vec2.Graphene_Vec2_T)
   is
      procedure Internal
        (Self      :     access Gsk_Path_Point; Path : System.Address;
         Direction :     Gsk.Enums.Gsk_Path_Direction;
         Tangent   : out Graphene.Vec2.Graphene_Vec2_T);
      pragma Import (C, Internal, "gsk_path_point_get_tangent");
   begin
      Internal (Self, Glib.Get_Object (Path), Direction, Tangent);
   end Get_Tangent;
   function Get_Rotation
     (Self      : access Gsk_Path_Point; Path : Gsk.Path.Gsk_Path;
      Direction : Gsk.Enums.Gsk_Path_Direction) return Interfaces.C.C_float
   is
      function Internal
        (Self      : access Gsk_Path_Point; Path : System.Address;
         Direction : Gsk.Enums.Gsk_Path_Direction) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gsk_path_point_get_rotation");
   begin
      return Internal (Self, Glib.Get_Object (Path), Direction);
   end Get_Rotation;
   function Get_Curvature
     (Self      : access Gsk_Path_Point; Path : Gsk.Path.Gsk_Path;
      Direction : Gsk.Enums.Gsk_Path_Direction;
      Center    : access Graphene.Point.Graphene_Point_T := null)
      return Interfaces.C.C_float
   is
      function Internal
        (Self      : access Gsk_Path_Point; Path : System.Address;
         Direction : Gsk.Enums.Gsk_Path_Direction;
         Center    : access Graphene.Point.Graphene_Point_T)
         return Interfaces.C.C_float;
      pragma Import (C, Internal, "gsk_path_point_get_curvature");
   begin
      return Internal (Self, Glib.Get_Object (Path), Direction, Center);
   end Get_Curvature;
   function Get_Distance
     (Self    : access Gsk_Path_Point;
      Measure : Gsk.Path_Measure.Gsk_Path_Measure) return Interfaces.C.C_float
   is
      function Internal
        (Self : access Gsk_Path_Point; Measure : System.Address)
         return Interfaces.C.C_float;
      pragma Import (C, Internal, "gsk_path_point_get_distance");
   begin
      return Internal (Self, Glib.Get_Object (Measure));
   end Get_Distance;
end Gsk.Path_Point;
