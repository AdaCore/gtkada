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

--  A rectangular region with rounded corners.
--
--  Application code should normalize rectangles using
--  [methodGsk.RoundedRect.normalize]; this function will ensure that the
--  bounds of the rectangle are normalized and ensure that the corner values
--  are positive and the corners do not overlap.
--
--  All functions taking a `GskRoundedRect` as an argument will internally
--  operate on a normalized copy; all functions returning a `GskRoundedRect`
--  will always return a normalized one.
--
--  The algorithm used for normalizing corner sizes is described in [the CSS
--  specification](https://drafts.csswg.org/css-backgrounds-3/border-radius).

pragma Warnings (Off, "*is already use-visible*");
with Graphene.Point; use Graphene.Point;
with Graphene.Rect;  use Graphene.Rect;
with Graphene.Size;  use Graphene.Size;
with Interfaces.C;   use Interfaces.C;

package Gsk.Rounded_Rect is

   type Gsk_Rounded_Rect is record
      Bounds : Graphene.Rect.Graphene_Rect_T;
      Corner : Graphene_Size_T_Array (1 .. 4);
   end record;
   pragma Convention (C, Gsk_Rounded_Rect);
   --  A rectangular region with rounded corners.
   --
   --  Application code should normalize rectangles using
   --  [methodGsk.RoundedRect.normalize]; this function will ensure that the
   --  bounds of the rectangle are normalized and ensure that the corner values
   --  are positive and the corners do not overlap.
   --
   --  All functions taking a `GskRoundedRect` as an argument will internally
   --  operate on a normalized copy; all functions returning a `GskRoundedRect`
   --  will always return a normalized one.
   --
   --  The algorithm used for normalizing corner sizes is described in [the
   --  CSS
   --  specification](https://drafts.csswg.org/css-backgrounds-3/border-radius).

   -------------
   -- Methods --
   -------------

   function Contains_Point
      (Self  : access Gsk_Rounded_Rect;
       Point : not null access Graphene.Point.Graphene_Point_T)
       return Boolean;
   --  Checks if the given point is inside the rounded rectangle.
   --  @param Point the point to check
   --  @return true if the point is inside the rounded rectangle

   function Contains_Rect
      (Self : access Gsk_Rounded_Rect;
       Rect : not null access Graphene.Rect.Graphene_Rect_T) return Boolean;
   --  Checks if the given rectangle is contained inside the rounded
   --  rectangle.
   --  @param Rect the rectangle to check
   --  @return true if the Rect is fully contained inside the rounded
   --  rectangle

   function Init
      (Self         : access Gsk_Rounded_Rect;
       Bounds       : not null access Graphene.Rect.Graphene_Rect_T;
       Top_Left     : not null access Graphene.Size.Graphene_Size_T;
       Top_Right    : not null access Graphene.Size.Graphene_Size_T;
       Bottom_Right : not null access Graphene.Size.Graphene_Size_T;
       Bottom_Left  : not null access Graphene.Size.Graphene_Size_T)
       return access Gsk_Rounded_Rect;
   pragma Import (C, Init, "gsk_rounded_rect_init");
   --  Initializes a rounded rectangle with the given values.
   --  This function will implicitly normalize the rounded rectangle before
   --  returning.
   --  @param Bounds a `graphene_rect_t` describing the bounds
   --  @param Top_Left the rounding radius of the top left corner
   --  @param Top_Right the rounding radius of the top right corner
   --  @param Bottom_Right the rounding radius of the bottom right corner
   --  @param Bottom_Left the rounding radius of the bottom left corner
   --  @return the initialized rounded rectangle

   function Init_Copy
      (Self : access Gsk_Rounded_Rect;
       Src  : Gsk_Rounded_Rect) return access Gsk_Rounded_Rect;
   pragma Import (C, Init_Copy, "gsk_rounded_rect_init_copy");
   --  Initializes a rounded rectangle with a copy.
   --  This function will not normalize the rounded rectangle, so make sure
   --  the source is normalized.
   --  @param Src another rounded rectangle
   --  @return the initialized rounded rectangle

   function Init_From_Rect
      (Self   : access Gsk_Rounded_Rect;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T;
       Radius : Interfaces.C.C_float) return access Gsk_Rounded_Rect;
   pragma Import (C, Init_From_Rect, "gsk_rounded_rect_init_from_rect");
   --  Initializes a rounded rectangle to the given bounds and sets the radius
   --  of all four corners equally.
   --  @param Bounds a `graphene_rect_t`
   --  @param Radius the border radius
   --  @return the initialized rounded rectangle

   function Intersects_Rect
      (Self : access Gsk_Rounded_Rect;
       Rect : not null access Graphene.Rect.Graphene_Rect_T) return Boolean;
   --  Checks if part a rectangle is contained inside the rounded rectangle.
   --  @param Rect the rectangle to check
   --  @return true if the Rect intersects with the rounded rectangle

   function Is_Rectilinear (Self : access Gsk_Rounded_Rect) return Boolean;
   --  Checks if all corners of a rounded rectangle are right angles and the
   --  rectangle covers all of its bounds.
   --  This information can be used to decide if [ctorGsk.ClipNode.new] or
   --  [ctorGsk.RoundedClipNode.new] should be called.
   --  @return true if the rounded rectangle is rectilinear

   function Normalize
      (Self : access Gsk_Rounded_Rect) return access Gsk_Rounded_Rect;
   pragma Import (C, Normalize, "gsk_rounded_rect_normalize");
   --  Normalizes a rounded rectangle.
   --  This function will ensure that the bounds of the rounded rectangle are
   --  normalized and ensure that the corner values are positive and the
   --  corners do not overlap.
   --  @return the normalized rounded rectangle

   function Offset
      (Self : access Gsk_Rounded_Rect;
       Dx   : Interfaces.C.C_float;
       Dy   : Interfaces.C.C_float) return access Gsk_Rounded_Rect;
   pragma Import (C, Offset, "gsk_rounded_rect_offset");
   --  Offsets the rounded rectangle's origin by Dx and Dy.
   --  The size and corners of the rounded rectangle are unchanged.
   --  @param Dx the horizontal offset
   --  @param Dy the vertical offset
   --  @return the offset rounded rectangle

   function Shrink
      (Self   : access Gsk_Rounded_Rect;
       Top    : Interfaces.C.C_float;
       Right  : Interfaces.C.C_float;
       Bottom : Interfaces.C.C_float;
       Left   : Interfaces.C.C_float) return access Gsk_Rounded_Rect;
   pragma Import (C, Shrink, "gsk_rounded_rect_shrink");
   --  Shrinks (or grows) a rounded rectangle by moving the 4 sides according
   --  to the offsets given.
   --  The corner radii will be changed in a way that tries to keep the center
   --  of the corner circle intact. This emulates CSS behavior.
   --  This function also works for growing rounded rectangles if you pass
   --  negative values for the Top, Right, Bottom or Left.
   --  @param Top how far to move the top side downwards
   --  @param Right how far to move the right side to the left
   --  @param Bottom how far to move the bottom side upwards
   --  @param Left how far to move the left side to the right
   --  @return the resized rounded rectangle

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Gsk_Rounded_Rect) return Gsk_Rounded_Rect;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

end Gsk.Rounded_Rect;
