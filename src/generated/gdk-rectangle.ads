------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  Represents a rectangle.
--
--  `GdkRectangle` is identical to `cairo_rectangle_t`. Together with Cairo's
--  `cairo_region_t` data type, these are the central types for representing
--  sets of pixels.
--
--  The intersection of two rectangles can be computed with
--  [methodGdk.Rectangle.intersect]; to find the union of two rectangles use
--  [methodGdk.Rectangle.union].
--
--  The `cairo_region_t` type provided by Cairo is usually used for managing
--  non-rectangular clipping of graphical operations.
--
--  The Graphene library has a number of other data types for regions and
--  volumes in 2D and 3D.
--
--  <group>Gdk, the low-level API</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib; use Glib;

package Gdk.Rectangle is

   type Gdk_Rectangle is record
      X : Glib.Gint;
      Y : Glib.Gint;
      Width : Glib.Gint;
      Height : Glib.Gint;
   end record;
   pragma Convention (C, Gdk_Rectangle);

   function From_Object_Free (B : access Gdk_Rectangle) return Gdk_Rectangle;
   pragma Inline (From_Object_Free);
   --  Represents a rectangle.
   --
   --  `GdkRectangle` is identical to `cairo_rectangle_t`. Together with
   --  Cairo's `cairo_region_t` data type, these are the central types for
   --  representing sets of pixels.
   --
   --  The intersection of two rectangles can be computed with
   --  [methodGdk.Rectangle.intersect]; to find the union of two rectangles use
   --  [methodGdk.Rectangle.union].
   --
   --  The `cairo_region_t` type provided by Cairo is usually used for
   --  managing non-rectangular clipping of graphical operations.
   --
   --  The Graphene library has a number of other data types for regions and
   --  volumes in 2D and 3D.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_rectangle_get_type");

   -------------
   -- Methods --
   -------------

   function Contains_Point
      (Self : Gdk_Rectangle;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Boolean;
   --  Returns True if Rect contains the point described by X and Y.
   --  @param X X coordinate
   --  @param Y Y coordinate
   --  @return True if Rect contains the point

   function Equal
      (Self  : Gdk_Rectangle;
       Rect2 : Gdk_Rectangle) return Boolean;
   --  Checks if the two given rectangles are equal.
   --  @param Rect2 a `GdkRectangle`
   --  @return True if the rectangles are equal.

   procedure Intersect
      (Self         : Gdk_Rectangle;
       Src2         : Gdk_Rectangle;
       Dest         : out Gdk_Rectangle;
       Do_Intersect : out Boolean);
   --  Calculates the intersection of two rectangles.
   --  It is allowed for Dest to be the same as either Src1 or Src2. If the
   --  rectangles do not intersect, Dest's width and height is set to 0 and its
   --  x and y values are undefined. If you are only interested in whether the
   --  rectangles intersect, but not in the intersecting area itself, pass null
   --  for Dest.
   --  @param Src2 a `GdkRectangle`
   --  @param Dest return location for the intersection of Src1 and Src2
   --  @return True if the rectangles intersect.

   procedure Union
      (Self : Gdk_Rectangle;
       Src2 : Gdk_Rectangle;
       Dest : out Gdk_Rectangle);
   pragma Import (C, Union, "gdk_rectangle_union");
   --  Calculates the union of two rectangles.
   --  The union of rectangles Src1 and Src2 is the smallest rectangle which
   --  includes both Src1 and Src2 within it. It is allowed for Dest to be the
   --  same as either Src1 or Src2.
   --  Note that this function does not ignore 'empty' rectangles (ie. with
   --  zero width or height).
   --  @param Src2 a `GdkRectangle`
   --  @param Dest return location for the union of Src1 and Src2

end Gdk.Rectangle;
