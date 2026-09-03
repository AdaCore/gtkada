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

--  A point with three components: X, Y, and Z.

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Graphene.Rect; use Graphene.Rect;
with Graphene.Vec3; use Graphene.Vec3;
with Interfaces.C;  use Interfaces.C;

package Graphene.Point3d is

   type Graphene_Point3D_T is record
      X : Interfaces.C.C_float;
      Y : Interfaces.C.C_float;
      Z : Interfaces.C.C_float;
   end record;
   pragma Convention (C, Graphene_Point3D_T);
   --  A point with three components: X, Y, and Z.

   type Graphene_Point3D_Array is array (Natural range <>) of Graphene_Point3D_T;
   pragma Convention (C, Graphene_Point3D_Array);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_point3d_get_type");

   -------------
   -- Methods --
   -------------

   procedure Cross
      (Self : not null access Graphene_Point3D_T;
       B    : not null access Graphene_Point3D_T;
       Res  : not null access Graphene_Point3D_T);
   pragma Import (C, Cross, "graphene_point3d_cross");
   --  Computes the cross product of the two given
   --  Graphene.Point3d.Graphene_Point3D_T.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Point3d.Graphene_Point3D_T
   --  @param Res return location for the cross product

   function Distance
      (Self      : not null access Graphene_Point3D_T;
       B         : not null access Graphene_Point3D_T;
       The_Delta : access Graphene.Vec3.Graphene_Vec3_T := null)
       return Interfaces.C.C_float;
   pragma Import (C, Distance, "graphene_point3d_distance");
   --  Computes the distance between the two given
   --  Graphene.Point3d.Graphene_Point3D_T.
   --  Since: gtk+ 1.4
   --  @param B a Graphene.Point3d.Graphene_Point3D_T
   --  @param The_Delta return location for the distance components on the X,
   --  Y, and Z axis
   --  @return the distance between two points

   function Dot
      (Self : not null access Graphene_Point3D_T;
       B    : not null access Graphene_Point3D_T)
       return Interfaces.C.C_float;
   pragma Import (C, Dot, "graphene_point3d_dot");
   --  Computes the dot product of the two given
   --  Graphene.Point3d.Graphene_Point3D_T.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Point3d.Graphene_Point3D_T
   --  @return the value of the dot product

   function Equal
      (Self : not null access Graphene_Point3D_T;
       B    : not null access Graphene_Point3D_T) return Boolean;
   --  Checks whether two given points are equal.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Point3d.Graphene_Point3D_T
   --  @return `true` if the points are equal

   procedure Free (Self : not null access Graphene_Point3D_T);
   pragma Import (C, Free, "graphene_point3d_free");
   --  Frees the resources allocated via graphene_point3d_alloc.
   --  Since: gtk+ 1.0

   function Init
      (Self : not null access Graphene_Point3D_T;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float;
       Z    : Interfaces.C.C_float) return access Graphene_Point3D_T;
   pragma Import (C, Init, "graphene_point3d_init");
   --  Initializes a Graphene.Point3d.Graphene_Point3D_T with the given
   --  coordinates.
   --  Since: gtk+ 1.0
   --  @param X the X coordinate of the point
   --  @param Y the Y coordinate of the point
   --  @param Z the Z coordinate of the point
   --  @return the initialized Graphene.Point3d.Graphene_Point3D_T

   function Init_From_Point
      (Self : not null access Graphene_Point3D_T;
       Src  : not null access Graphene_Point3D_T)
       return access Graphene_Point3D_T;
   pragma Import (C, Init_From_Point, "graphene_point3d_init_from_point");
   --  Initializes a Graphene.Point3d.Graphene_Point3D_T using the coordinates
   --  of another Graphene.Point3d.Graphene_Point3D_T.
   --  Since: gtk+ 1.0
   --  @param Src a Graphene.Point3d.Graphene_Point3D_T
   --  @return the initialized point

   function Init_From_Vec3
      (Self : not null access Graphene_Point3D_T;
       V    : not null access Graphene.Vec3.Graphene_Vec3_T)
       return access Graphene_Point3D_T;
   pragma Import (C, Init_From_Vec3, "graphene_point3d_init_from_vec3");
   --  Initializes a Graphene.Point3d.Graphene_Point3D_T using the components
   --  of a Graphene.Vec3.Graphene_Vec3_T.
   --  Since: gtk+ 1.0
   --  @param V a Graphene.Vec3.Graphene_Vec3_T
   --  @return the initialized Graphene.Point3d.Graphene_Point3D_T

   procedure Interpolate
      (Self   : not null access Graphene_Point3D_T;
       B      : not null access Graphene_Point3D_T;
       Factor : Gdouble;
       Res    : not null access Graphene_Point3D_T);
   pragma Import (C, Interpolate, "graphene_point3d_interpolate");
   --  Linearly interpolates each component of A and B using the provided
   --  Factor, and places the result in Res.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Point3d.Graphene_Point3D_T
   --  @param Factor the interpolation factor
   --  @param Res the return location for the interpolated
   --  Graphene.Point3d.Graphene_Point3D_T

   function Length
      (Self : not null access Graphene_Point3D_T)
       return Interfaces.C.C_float;
   pragma Import (C, Length, "graphene_point3d_length");
   --  Computes the length of the vector represented by the coordinates of the
   --  given Graphene.Point3d.Graphene_Point3D_T.
   --  Since: gtk+ 1.0
   --  @return the length of the vector represented by the point

   function Near
      (Self    : not null access Graphene_Point3D_T;
       B       : not null access Graphene_Point3D_T;
       Epsilon : Interfaces.C.C_float) return Boolean;
   --  Checks whether the two points are near each other, within an Epsilon
   --  factor.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Point3d.Graphene_Point3D_T
   --  @param Epsilon fuzzyness factor
   --  @return `true` if the points are near each other

   procedure Normalize
      (Self : not null access Graphene_Point3D_T;
       Res  : not null access Graphene_Point3D_T);
   pragma Import (C, Normalize, "graphene_point3d_normalize");
   --  Computes the normalization of the vector represented by the coordinates
   --  of the given Graphene.Point3d.Graphene_Point3D_T.
   --  Since: gtk+ 1.0
   --  @param Res return location for the normalized
   --  Graphene.Point3d.Graphene_Point3D_T

   procedure Normalize_Viewport
      (Self     : not null access Graphene_Point3D_T;
       Viewport : not null access Graphene.Rect.Graphene_Rect_T;
       Z_Near   : Interfaces.C.C_float;
       Z_Far    : Interfaces.C.C_float;
       Res      : not null access Graphene_Point3D_T);
   pragma Import (C, Normalize_Viewport, "graphene_point3d_normalize_viewport");
   --  Normalizes the coordinates of a Graphene.Point3d.Graphene_Point3D_T
   --  using the given viewport and clipping planes.
   --  The coordinates of the resulting Graphene.Point3d.Graphene_Point3D_T
   --  will be in the [ -1, 1 ] range.
   --  Since: gtk+ 1.4
   --  @param Viewport a Graphene.Rect.Graphene_Rect_T representing a viewport
   --  @param Z_Near the coordinate of the near clipping plane, or 0 for the
   --  default near clipping plane
   --  @param Z_Far the coordinate of the far clipping plane, or 1 for the
   --  default far clipping plane
   --  @param Res the return location for the normalized
   --  Graphene.Point3d.Graphene_Point3D_T

   procedure Scale
      (Self   : not null access Graphene_Point3D_T;
       Factor : Interfaces.C.C_float;
       Res    : not null access Graphene_Point3D_T);
   pragma Import (C, Scale, "graphene_point3d_scale");
   --  Scales the coordinates of the given Graphene.Point3d.Graphene_Point3D_T
   --  by the given Factor.
   --  Since: gtk+ 1.0
   --  @param Factor the scaling factor
   --  @param Res return location for the scaled point

   procedure To_Vec3
      (Self : not null access Graphene_Point3D_T;
       V    : not null access Graphene.Vec3.Graphene_Vec3_T);
   pragma Import (C, To_Vec3, "graphene_point3d_to_vec3");
   --  Stores the coordinates of a Graphene.Point3d.Graphene_Point3D_T into a
   --  Graphene.Vec3.Graphene_Vec3_T.
   --  Since: gtk+ 1.0
   --  @param V return location for a Graphene.Vec3.Graphene_Vec3_T

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Graphene_Point3D_T) return Graphene_Point3D_T;
   pragma Inline (From_Object_Free);

   ---------------
   -- Functions --
   ---------------

   function Zero return access constant Graphene_Point3D_T;
   pragma Import (C, Zero, "graphene_point3d_zero");
   --  Retrieves a constant point with all three coordinates set to 0.
   --  Since: gtk+ 1.0
   --  @return a zero point

end Graphene.Point3d;
