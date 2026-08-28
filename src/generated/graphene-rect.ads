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

--  The location and size of a rectangle region.
--
--  The width and height of a Graphene.Rect.Graphene_Rect_T can be negative;
--  for instance, a Graphene.Rect.Graphene_Rect_T with an origin of [ 0, 0 ]
--  and a size of [ 10, 10 ] is equivalent to a Graphene.Rect.Graphene_Rect_T
--  with an origin of [ 10, 10 ] and a size of [ -10, -10 ].
--
--  Application code can normalize rectangles using Graphene.Rect.Normalize;
--  this function will ensure that the width and height of a rectangle are
--  positive values. All functions taking a Graphene.Rect.Graphene_Rect_T as an
--  argument will internally operate on a normalized copy; all functions
--  returning a Graphene.Rect.Graphene_Rect_T will always return a normalized
--  rectangle.

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Graphene.Point; use Graphene.Point;
with Graphene.Size;  use Graphene.Size;
with Graphene.Vec2;  use Graphene.Vec2;
with Interfaces.C;   use Interfaces.C;

package Graphene.Rect is

   type Graphene_Rect_T is record
      Origin : Graphene.Point.Graphene_Point_T;
      Size : Graphene.Size.Graphene_Size_T;
   end record;
   pragma Convention (C, Graphene_Rect_T);

   function From_Object_Free
     (B : not null access Graphene_Rect_T) return Graphene_Rect_T;
   pragma Inline (From_Object_Free);
   --  The location and size of a rectangle region.
   --
   --  The width and height of a Graphene.Rect.Graphene_Rect_T can be
   --  negative; for instance, a Graphene.Rect.Graphene_Rect_T with an origin
   --  of [ 0, 0 ] and a size of [ 10, 10 ] is equivalent to a
   --  Graphene.Rect.Graphene_Rect_T with an origin of [ 10, 10 ] and a size of
   --  [ -10, -10 ].
   --
   --  Application code can normalize rectangles using
   --  Graphene.Rect.Normalize; this function will ensure that the width and
   --  height of a rectangle are positive values. All functions taking a
   --  Graphene.Rect.Graphene_Rect_T as an argument will internally operate on
   --  a normalized copy; all functions returning a
   --  Graphene.Rect.Graphene_Rect_T will always return a normalized rectangle.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_rect_get_type");

   -------------
   -- Methods --
   -------------

   function Contains_Point
      (Self : not null access Graphene_Rect_T;
       P    : not null access Graphene.Point.Graphene_Point_T)
       return Boolean;
   --  Checks whether a Graphene.Rect.Graphene_Rect_T contains the given
   --  coordinates.
   --  Since: gtk+ 1.0
   --  @param P a Graphene.Point.Graphene_Point_T
   --  @return `true` if the rectangle contains the point

   function Contains_Rect
      (Self : not null access Graphene_Rect_T;
       B    : not null access Graphene_Rect_T) return Boolean;
   --  Checks whether a Graphene.Rect.Graphene_Rect_T fully contains the given
   --  rectangle.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Rect.Graphene_Rect_T
   --  @return `true` if the rectangle A fully contains B

   function Equal
      (Self : not null access Graphene_Rect_T;
       B    : not null access Graphene_Rect_T) return Boolean;
   --  Checks whether the two given rectangle are equal.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Rect.Graphene_Rect_T
   --  @return `true` if the rectangles are equal

   procedure Expand
      (Self : not null access Graphene_Rect_T;
       P    : not null access Graphene.Point.Graphene_Point_T;
       Res  : not null access Graphene_Rect_T);
   pragma Import (C, Expand, "graphene_rect_expand");
   --  Expands a Graphene.Rect.Graphene_Rect_T to contain the given
   --  Graphene.Point.Graphene_Point_T.
   --  Since: gtk+ 1.4
   --  @param P a Graphene.Point.Graphene_Point_T
   --  @param Res return location for the expanded rectangle

   procedure Free (Self : not null access Graphene_Rect_T);
   pragma Import (C, Free, "graphene_rect_free");
   --  Frees the resources allocated by graphene_rect_alloc.
   --  Since: gtk+ 1.0

   function Get_Area
      (Self : not null access Graphene_Rect_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Area, "graphene_rect_get_area");
   --  Compute the area of given normalized rectangle.
   --  Since: gtk+ 1.10
   --  @return the area of the normalized rectangle

   procedure Get_Bottom_Left
      (Self : not null access Graphene_Rect_T;
       P    : not null access Graphene.Point.Graphene_Point_T);
   pragma Import (C, Get_Bottom_Left, "graphene_rect_get_bottom_left");
   --  Retrieves the coordinates of the bottom-left corner of the given
   --  rectangle.
   --  Since: gtk+ 1.0
   --  @param P return location for a Graphene.Point.Graphene_Point_T

   procedure Get_Bottom_Right
      (Self : not null access Graphene_Rect_T;
       P    : not null access Graphene.Point.Graphene_Point_T);
   pragma Import (C, Get_Bottom_Right, "graphene_rect_get_bottom_right");
   --  Retrieves the coordinates of the bottom-right corner of the given
   --  rectangle.
   --  Since: gtk+ 1.0
   --  @param P return location for a Graphene.Point.Graphene_Point_T

   procedure Get_Center
      (Self : not null access Graphene_Rect_T;
       P    : not null access Graphene.Point.Graphene_Point_T);
   pragma Import (C, Get_Center, "graphene_rect_get_center");
   --  Retrieves the coordinates of the center of the given rectangle.
   --  Since: gtk+ 1.0
   --  @param P return location for a Graphene.Point.Graphene_Point_T

   function Get_Height
      (Self : not null access Graphene_Rect_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Height, "graphene_rect_get_height");
   --  Retrieves the normalized height of the given rectangle.
   --  Since: gtk+ 1.0
   --  @return the normalized height of the rectangle

   procedure Get_Top_Left
      (Self : not null access Graphene_Rect_T;
       P    : not null access Graphene.Point.Graphene_Point_T);
   pragma Import (C, Get_Top_Left, "graphene_rect_get_top_left");
   --  Retrieves the coordinates of the top-left corner of the given
   --  rectangle.
   --  Since: gtk+ 1.0
   --  @param P return location for a Graphene.Point.Graphene_Point_T

   procedure Get_Top_Right
      (Self : not null access Graphene_Rect_T;
       P    : not null access Graphene.Point.Graphene_Point_T);
   pragma Import (C, Get_Top_Right, "graphene_rect_get_top_right");
   --  Retrieves the coordinates of the top-right corner of the given
   --  rectangle.
   --  Since: gtk+ 1.0
   --  @param P return location for a Graphene.Point.Graphene_Point_T

   function Get_Width
      (Self : not null access Graphene_Rect_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Width, "graphene_rect_get_width");
   --  Retrieves the normalized width of the given rectangle.
   --  Since: gtk+ 1.0
   --  @return the normalized width of the rectangle

   function Get_X
      (Self : not null access Graphene_Rect_T) return Interfaces.C.C_float;
   pragma Import (C, Get_X, "graphene_rect_get_x");
   --  Retrieves the normalized X coordinate of the origin of the given
   --  rectangle.
   --  Since: gtk+ 1.0
   --  @return the normalized X coordinate of the rectangle

   function Get_Y
      (Self : not null access Graphene_Rect_T) return Interfaces.C.C_float;
   pragma Import (C, Get_Y, "graphene_rect_get_y");
   --  Retrieves the normalized Y coordinate of the origin of the given
   --  rectangle.
   --  Since: gtk+ 1.0
   --  @return the normalized Y coordinate of the rectangle

   function Init
      (Self   : not null access Graphene_Rect_T;
       X      : Interfaces.C.C_float;
       Y      : Interfaces.C.C_float;
       Width  : Interfaces.C.C_float;
       Height : Interfaces.C.C_float) return access Graphene_Rect_T;
   pragma Import (C, Init, "graphene_rect_init");
   --  Initializes the given Graphene.Rect.Graphene_Rect_T with the given
   --  values.
   --  This function will implicitly normalize the
   --  Graphene.Rect.Graphene_Rect_T before returning.
   --  Since: gtk+ 1.0
   --  @param X the X coordinate of the Graphene_Rect_T.origin
   --  @param Y the Y coordinate of the Graphene_Rect_T.origin
   --  @param Width the width of the Graphene_Rect_T.size
   --  @param Height the height of the Graphene_Rect_T.size
   --  @return the initialized rectangle

   function Init_From_Rect
      (Self : not null access Graphene_Rect_T;
       Src  : not null access Graphene_Rect_T) return access Graphene_Rect_T;
   pragma Import (C, Init_From_Rect, "graphene_rect_init_from_rect");
   --  Initializes R using the given Src rectangle.
   --  This function will implicitly normalize the
   --  Graphene.Rect.Graphene_Rect_T before returning.
   --  Since: gtk+ 1.0
   --  @param Src a Graphene.Rect.Graphene_Rect_T
   --  @return the initialized rectangle

   function Inset
      (Self : not null access Graphene_Rect_T;
       D_X  : Interfaces.C.C_float;
       D_Y  : Interfaces.C.C_float) return access Graphene_Rect_T;
   pragma Import (C, Inset, "graphene_rect_inset");
   --  Changes the given rectangle to be smaller, or larger depending on the
   --  given inset parameters.
   --  To create an inset rectangle, use positive D_X or D_Y values; to create
   --  a larger, encompassing rectangle, use negative D_X or D_Y values.
   --  The origin of the rectangle is offset by D_X and D_Y, while the size is
   --  adjusted by `(2 * D_X, 2 * D_Y)`. If D_X and D_Y are positive values,
   --  the size of the rectangle is decreased; if D_X and D_Y are negative
   --  values, the size of the rectangle is increased.
   --  If the size of the resulting inset rectangle has a negative width or
   --  height then the size will be set to zero.
   --  Since: gtk+ 1.0
   --  @param D_X the horizontal inset
   --  @param D_Y the vertical inset
   --  @return the inset rectangle

   procedure Inset_R
      (Self : not null access Graphene_Rect_T;
       D_X  : Interfaces.C.C_float;
       D_Y  : Interfaces.C.C_float;
       Res  : not null access Graphene_Rect_T);
   pragma Import (C, Inset_R, "graphene_rect_inset_r");
   --  Changes the given rectangle to be smaller, or larger depending on the
   --  given inset parameters.
   --  To create an inset rectangle, use positive D_X or D_Y values; to create
   --  a larger, encompassing rectangle, use negative D_X or D_Y values.
   --  The origin of the rectangle is offset by D_X and D_Y, while the size is
   --  adjusted by `(2 * D_X, 2 * D_Y)`. If D_X and D_Y are positive values,
   --  the size of the rectangle is decreased; if D_X and D_Y are negative
   --  values, the size of the rectangle is increased.
   --  If the size of the resulting inset rectangle has a negative width or
   --  height then the size will be set to zero.
   --  Since: gtk+ 1.4
   --  @param D_X the horizontal inset
   --  @param D_Y the vertical inset
   --  @param Res return location for the inset rectangle

   procedure Interpolate
      (Self   : not null access Graphene_Rect_T;
       B      : not null access Graphene_Rect_T;
       Factor : Gdouble;
       Res    : not null access Graphene_Rect_T);
   pragma Import (C, Interpolate, "graphene_rect_interpolate");
   --  Linearly interpolates the origin and size of the two given rectangles.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Rect.Graphene_Rect_T
   --  @param Factor the linear interpolation factor
   --  @param Res return location for the interpolated rectangle

   function Intersection
      (Self : not null access Graphene_Rect_T;
       B    : not null access Graphene_Rect_T;
       Res  : access Graphene_Rect_T := null) return Boolean;
   --  Computes the intersection of the two given rectangles.
   --  ![](rectangle-intersection.png)
   --  The intersection in the image above is the blue outline.
   --  If the two rectangles do not intersect, Res will contain a degenerate
   --  rectangle with origin in (0, 0) and a size of 0.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Rect.Graphene_Rect_T
   --  @param Res return location for a Graphene.Rect.Graphene_Rect_T
   --  @return `true` if the two rectangles intersect

   function Normalize
      (Self : not null access Graphene_Rect_T) return access Graphene_Rect_T;
   pragma Import (C, Normalize, "graphene_rect_normalize");
   --  Normalizes the passed rectangle.
   --  This function ensures that the size of the rectangle is made of
   --  positive values, and that the origin is the top-left corner of the
   --  rectangle.
   --  Since: gtk+ 1.0
   --  @return the normalized rectangle

   procedure Normalize_R
      (Self : not null access Graphene_Rect_T;
       Res  : not null access Graphene_Rect_T);
   pragma Import (C, Normalize_R, "graphene_rect_normalize_r");
   --  Normalizes the passed rectangle.
   --  This function ensures that the size of the rectangle is made of
   --  positive values, and that the origin is in the top-left corner of the
   --  rectangle.
   --  Since: gtk+ 1.4
   --  @param Res the return location for the normalized rectangle

   function Offset
      (Self : not null access Graphene_Rect_T;
       D_X  : Interfaces.C.C_float;
       D_Y  : Interfaces.C.C_float) return access Graphene_Rect_T;
   pragma Import (C, Offset, "graphene_rect_offset");
   --  Offsets the origin by D_X and D_Y.
   --  The size of the rectangle is unchanged.
   --  Since: gtk+ 1.0
   --  @param D_X the horizontal offset
   --  @param D_Y the vertical offset
   --  @return the offset rectangle

   procedure Offset_R
      (Self : not null access Graphene_Rect_T;
       D_X  : Interfaces.C.C_float;
       D_Y  : Interfaces.C.C_float;
       Res  : not null access Graphene_Rect_T);
   pragma Import (C, Offset_R, "graphene_rect_offset_r");
   --  Offsets the origin of the given rectangle by D_X and D_Y.
   --  The size of the rectangle is left unchanged.
   --  Since: gtk+ 1.4
   --  @param D_X the horizontal offset
   --  @param D_Y the vertical offset
   --  @param Res return location for the offset rectangle

   procedure Round_Extents
      (Self : not null access Graphene_Rect_T;
       Res  : not null access Graphene_Rect_T);
   pragma Import (C, Round_Extents, "graphene_rect_round_extents");
   --  Rounds the origin of the given rectangle to its nearest integer value
   --  and and recompute the size so that the rectangle is large enough to
   --  contain all the conrners of the original rectangle.
   --  This function is the equivalent of calling `floor` on the coordinates
   --  of the origin, and recomputing the size calling `ceil` on the
   --  bottom-right coordinates.
   --  If you want to be sure that the rounded rectangle completely covers the
   --  area that was covered by the original rectangle — i.e. you want to cover
   --  the area including all its corners — this function will make sure that
   --  the size is recomputed taking into account the ceiling of the
   --  coordinates of the bottom-right corner. If the difference between the
   --  original coordinates and the coordinates of the rounded rectangle is
   --  greater than the difference between the original size and and the
   --  rounded size, then the move of the origin would not be compensated by a
   --  move in the anti-origin, leaving the corners of the original rectangle
   --  outside the rounded one.
   --  Since: gtk+ 1.10
   --  @param Res return location for the rectangle with rounded extents

   procedure Scale
      (Self : not null access Graphene_Rect_T;
       S_H  : Interfaces.C.C_float;
       S_V  : Interfaces.C.C_float;
       Res  : not null access Graphene_Rect_T);
   pragma Import (C, Scale, "graphene_rect_scale");
   --  Scales the size and origin of a rectangle horizontaly by S_H, and
   --  vertically by S_V. The result Res is normalized.
   --  Since: gtk+ 1.10
   --  @param S_H horizontal scale factor
   --  @param S_V vertical scale factor
   --  @param Res return location for the scaled rectangle

   procedure Union
      (Self : not null access Graphene_Rect_T;
       B    : not null access Graphene_Rect_T;
       Res  : not null access Graphene_Rect_T);
   pragma Import (C, Union, "graphene_rect_union");
   --  Computes the union of the two given rectangles.
   --  ![](rectangle-union.png)
   --  The union in the image above is the blue outline.
   --  Since: gtk+ 1.0
   --  @param B a Graphene.Rect.Graphene_Rect_T
   --  @param Res return location for a Graphene.Rect.Graphene_Rect_T

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Graphene_Vec2_Array4 is array (1 .. 4) of Graphene.Vec2.Graphene_Vec2_T;

   procedure Get_Vertices
     (Self     : not null access Graphene_Rect_T;
      Vertices : out Graphene_Vec2_Array4);
   --  Computes the four vertices of a Graphene.Rect.Graphene_Rect_T.
   --  Since: gtk+ 1.4
   --  @param Vertices return location for an array of 4
   --  Graphene.Vec2.Graphene_Vec2_T

   ---------------
   -- Functions --
   ---------------

   function Zero return access constant Graphene_Rect_T;
   pragma Import (C, Zero, "graphene_rect_zero");
   --  Returns a degenerate rectangle with origin fixed at (0, 0) and a size
   --  of 0, 0.
   --  Since: gtk+ 1.4
   --  @return a fixed rectangle

end Graphene.Rect;
