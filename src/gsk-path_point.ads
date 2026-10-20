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

with Glib;
with Interfaces.C;
with System;
with Graphene.Point;
with Graphene.Vec2;
with Gsk.Enums;
limited with Gsk.Path;
limited with Gsk.Path_Measure;

package Gsk.Path_Point is
   --  GskPathPoint is a caller-allocated opaque union: eight pointers of
   --  storage, aligned like graphene_vec4_t (16 bytes). Do not interpret it.
   --  Keep the path alive for as long as any point obtained from it is used.
   type Gsk_Path_Point is private;
   type Gsk_Path_Point_Access is access all Gsk_Path_Point;
   pragma Convention (C, Gsk_Path_Point_Access);

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_path_point_get_type");
   function Copy (Self : access Gsk_Path_Point) return Gsk_Path_Point_Access;
   pragma Import (C, Copy, "gsk_path_point_copy");
   procedure Free (Self : access Gsk_Path_Point);
   pragma Import (C, Free, "gsk_path_point_free");
   --  Free only heap points returned by Copy, never a stack variable.
   function Equal (Self, Other : access Gsk_Path_Point) return Boolean;
   function Compare (Self, Other : access Gsk_Path_Point) return Glib.Gint;
   pragma Import (C, Compare, "gsk_path_point_compare");

   procedure Get_Position
     (Self     :     access Gsk_Path_Point; Path : Gsk.Path.Gsk_Path;
      Position : out Graphene.Point.Graphene_Point_T);
   procedure Get_Tangent
     (Self      :     access Gsk_Path_Point; Path : Gsk.Path.Gsk_Path;
      Direction :     Gsk.Enums.Gsk_Path_Direction;
      Tangent   : out Graphene.Vec2.Graphene_Vec2_T);
   function Get_Rotation
     (Self      : access Gsk_Path_Point; Path : Gsk.Path.Gsk_Path;
      Direction : Gsk.Enums.Gsk_Path_Direction) return Interfaces.C.C_float;
   function Get_Curvature
     (Self      : access Gsk_Path_Point; Path : Gsk.Path.Gsk_Path;
      Direction : Gsk.Enums.Gsk_Path_Direction;
      Center    : access Graphene.Point.Graphene_Point_T := null)
      return Interfaces.C.C_float;
   function Get_Distance
     (Self    : access Gsk_Path_Point;
      Measure : Gsk.Path_Measure.Gsk_Path_Measure) return Interfaces.C.C_float;
private
   type Storage_Array is array (1 .. 8) of System.Address;
   pragma Convention (C, Storage_Array);
   type Gsk_Path_Point is record
      Storage : Storage_Array;
   end record;
   pragma Convention (C, Gsk_Path_Point);
   for Gsk_Path_Point'Alignment use 16;
end Gsk.Path_Point;
