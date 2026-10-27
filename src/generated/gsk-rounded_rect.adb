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
with Glib; use Glib;

package body Gsk.Rounded_Rect is

   --------------------
   -- Contains_Point --
   --------------------

   function Contains_Point
      (Self  : access Gsk_Rounded_Rect;
       Point : not null access Graphene.Point.Graphene_Point_T)
       return Boolean
   is
      function Internal
         (Self  : access Gsk_Rounded_Rect;
          Point : access Graphene.Point.Graphene_Point_T)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_rounded_rect_contains_point");
   begin
      return Internal (Self, Point) /= 0;
   end Contains_Point;

   -------------------
   -- Contains_Rect --
   -------------------

   function Contains_Rect
      (Self : access Gsk_Rounded_Rect;
       Rect : not null access Graphene.Rect.Graphene_Rect_T) return Boolean
   is
      function Internal
         (Self : access Gsk_Rounded_Rect;
          Rect : access Graphene.Rect.Graphene_Rect_T) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_rounded_rect_contains_rect");
   begin
      return Internal (Self, Rect) /= 0;
   end Contains_Rect;

   ---------------------
   -- Intersects_Rect --
   ---------------------

   function Intersects_Rect
      (Self : access Gsk_Rounded_Rect;
       Rect : not null access Graphene.Rect.Graphene_Rect_T) return Boolean
   is
      function Internal
         (Self : access Gsk_Rounded_Rect;
          Rect : access Graphene.Rect.Graphene_Rect_T) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_rounded_rect_intersects_rect");
   begin
      return Internal (Self, Rect) /= 0;
   end Intersects_Rect;

   --------------------
   -- Is_Rectilinear --
   --------------------

   function Is_Rectilinear (Self : access Gsk_Rounded_Rect) return Boolean is
      function Internal
         (Self : access Gsk_Rounded_Rect) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_rounded_rect_is_rectilinear");
   begin
      return Internal (Self) /= 0;
   end Is_Rectilinear;

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Gsk_Rounded_Rect) return Gsk_Rounded_Rect
   is
      Result : constant Gsk_Rounded_Rect := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

end Gsk.Rounded_Rect;
