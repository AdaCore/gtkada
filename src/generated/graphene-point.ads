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

--  A point with two coordinates.

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Graphene.Vec2; use Graphene.Vec2;
with Interfaces.C;  use Interfaces.C;

package Graphene.Point is

   type Graphene_Point_T is record
      X : Interfaces.C.C_float;
      Y : Interfaces.C.C_float;
   end record;
   pragma Convention (C, Graphene_Point_T);
   --  A point with two coordinates.

   type Graphene_Point_Array is array (Natural range <>) of Graphene_Point_T;
   pragma Convention (C, Graphene_Point_Array);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_point_get_type");

   -------------
   -- Methods --
   -------------

   function Distance
      (Self : not null access Graphene_Point_T;
       B    : not null access Graphene_Point_T;
       D_X  : access Interfaces.C.C_float := null;
       D_Y  : access Interfaces.C.C_float := null)
       return Interfaces.C.C_float;
   pragma Import (C, Distance, "graphene_point_distance");
   --  Computes the distance between A and B.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Point.Graphene_Point_T
   --  @param D_X distance component on the X axis
   --  @param D_Y distance component on the Y axis
   --  @return the distance between the two points

   function Equal
      (Self : not null access Graphene_Point_T;
       B    : not null access Graphene_Point_T) return Boolean;
   --  Checks if the two points A and B point to the same coordinates.
   --  This function accounts for floating point fluctuations; if you want to
   --  control the fuzziness of the match, you can use Graphene.Point.Near
   --  instead.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Point.Graphene_Point_T
   --  @return `true` if the points have the same coordinates

   procedure Free (Self : not null access Graphene_Point_T);
   pragma Import (C, Free, "graphene_point_free");
   --  Frees the resources allocated by graphene_point_alloc.
   --  Since: gtk+ 1.0

   function Init
      (Self : not null access Graphene_Point_T;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float) return access Graphene_Point_T;
   pragma Import (C, Init, "graphene_point_init");
   --  Initializes P to the given X and Y coordinates.
   --  It's safe to call this function multiple times.
   --  Since: gtk+ 1.0
   --  @param X the X coordinate
   --  @param Y the Y coordinate
   --  @return the initialized point

   function Init_From_Point
      (Self : not null access Graphene_Point_T;
       Src  : not null access Graphene_Point_T)
       return access Graphene_Point_T;
   pragma Import (C, Init_From_Point, "graphene_point_init_from_point");
   --  Initializes P with the same coordinates of Src.
   --  Since: gtk+ 1.0
   --  @param Src the Graphene.Point.Graphene_Point_T to use
   --  @return the initialized point

   function Init_From_Vec2
      (Self : not null access Graphene_Point_T;
       Src  : not null access Graphene.Vec2.Graphene_Vec2_T)
       return access Graphene_Point_T;
   pragma Import (C, Init_From_Vec2, "graphene_point_init_from_vec2");
   --  Initializes P with the coordinates inside the given
   --  Graphene.Vec2.Graphene_Vec2_T.
   --  Since: gtk+ 1.4
   --  @param Src a Graphene.Vec2.Graphene_Vec2_T
   --  @return the initialized point

   procedure Interpolate
      (Self   : not null access Graphene_Point_T;
       B      : not null access Graphene_Point_T;
       Factor : Gdouble;
       Res    : not null access Graphene_Point_T);
   pragma Import (C, Interpolate, "graphene_point_interpolate");
   --  Linearly interpolates the coordinates of A and B using the given
   --  Factor.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Point.Graphene_Point_T
   --  @param Factor the linear interpolation factor
   --  @param Res return location for the interpolated point

   function Near
      (Self    : not null access Graphene_Point_T;
       B       : not null access Graphene_Point_T;
       Epsilon : Interfaces.C.C_float) return Boolean;
   --  Checks whether the two points A and B are within the threshold of
   --  Epsilon.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Point.Graphene_Point_T
   --  @param Epsilon threshold between the two points
   --  @return `true` if the distance is within Epsilon

   procedure To_Vec2
      (Self : not null access Graphene_Point_T;
       V    : not null access Graphene.Vec2.Graphene_Vec2_T);
   pragma Import (C, To_Vec2, "graphene_point_to_vec2");
   --  Stores the coordinates of the given Graphene.Point.Graphene_Point_T
   --  into a Graphene.Vec2.Graphene_Vec2_T.
   --  Since: gtk+ 1.4
   --  @param V return location for the vertex

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Graphene_Point_T) return Graphene_Point_T;
   pragma Inline (From_Object_Free);

   ---------------
   -- Functions --
   ---------------

   function Zero return access constant Graphene_Point_T;
   pragma Import (C, Zero, "graphene_point_zero");
   --  Returns a point fixed at (0, 0).
   --  Since: gtk+ 1.0
   --  @return a fixed point

end Graphene.Point;
