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

--  A 4 vertex quadrilateral, as represented by four
--  Graphene.Point.Graphene_Point_T.
--
--  The contents of a Graphene.Quad.Graphene_Quad_T are private and should
--  never be accessed directly.

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Graphene.Point; use Graphene.Point;
with Graphene.Rect;  use Graphene.Rect;

package Graphene.Quad is

   type Graphene_Quad_T is record
      Points : Graphene_Point_Array (1 .. 4);
   end record;
   pragma Convention (C, Graphene_Quad_T);

   function From_Object_Free (B : access Graphene_Quad_T) return Graphene_Quad_T;
   pragma Inline (From_Object_Free);
   --  A 4 vertex quadrilateral, as represented by four
   --  Graphene.Point.Graphene_Point_T.
   --
   --  The contents of a Graphene.Quad.Graphene_Quad_T are private and should
   --  never be accessed directly.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "graphene_quad_get_type");

   -------------
   -- Methods --
   -------------

   procedure Bounds
      (Self : not null access Graphene_Quad_T;
       R    : not null access Graphene.Rect.Graphene_Rect_T);
   pragma Import (C, Bounds, "graphene_quad_bounds");
   --  Computes the bounding rectangle of Q and places it into R.
   --  Since: gtk+ 1.0
   --  @param R return location for a Graphene.Rect.Graphene_Rect_T

   function Contains
      (Self : not null access Graphene_Quad_T;
       P    : not null access Graphene.Point.Graphene_Point_T)
       return Boolean;
   --  Checks if the given Graphene.Quad.Graphene_Quad_T contains the given
   --  Graphene.Point.Graphene_Point_T.
   --  Since: gtk+ 1.0
   --  @param P a Graphene.Point.Graphene_Point_T
   --  @return `true` if the point is inside the Graphene.Quad.Graphene_Quad_T

   procedure Free (Self : not null access Graphene_Quad_T);
   pragma Import (C, Free, "graphene_quad_free");
   --  Frees the resources allocated by graphene_quad_alloc
   --  Since: gtk+ 1.0

   function Get_Point
      (Self  : not null access Graphene_Quad_T;
       Index : Guint) return access constant Graphene.Point.Graphene_Point_T;
   pragma Import (C, Get_Point, "graphene_quad_get_point");
   --  Retrieves the point of a Graphene.Quad.Graphene_Quad_T at the given
   --  index.
   --  Since: gtk+ 1.0
   --  @param Index the index of the point to retrieve
   --  @return a Graphene.Point.Graphene_Point_T

   function Init
      (Self : not null access Graphene_Quad_T;
       P1   : not null access Graphene.Point.Graphene_Point_T;
       P2   : not null access Graphene.Point.Graphene_Point_T;
       P3   : not null access Graphene.Point.Graphene_Point_T;
       P4   : not null access Graphene.Point.Graphene_Point_T)
       return access Graphene_Quad_T;
   pragma Import (C, Init, "graphene_quad_init");
   --  Initializes a Graphene.Quad.Graphene_Quad_T with the given points.
   --  Since: gtk+ 1.0
   --  @param P1 the first point of the quadrilateral
   --  @param P2 the second point of the quadrilateral
   --  @param P3 the third point of the quadrilateral
   --  @param P4 the fourth point of the quadrilateral
   --  @return the initialized Graphene.Quad.Graphene_Quad_T

   function Init_From_Rect
      (Self : not null access Graphene_Quad_T;
       R    : not null access Graphene.Rect.Graphene_Rect_T)
       return access Graphene_Quad_T;
   pragma Import (C, Init_From_Rect, "graphene_quad_init_from_rect");
   --  Initializes a Graphene.Quad.Graphene_Quad_T using the four corners of
   --  the given Graphene.Rect.Graphene_Rect_T.
   --  Since: gtk+ 1.0
   --  @param R a Graphene.Rect.Graphene_Rect_T
   --  @return the initialized Graphene.Quad.Graphene_Quad_T

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Graphene_Point_Array4 is array (1 .. 4) of Graphene_Point_T;
   pragma Convention (C, Graphene_Point_Array4);

   function Init_From_Points
     (Self   : not null access Graphene_Quad_T;
      Points : Graphene_Point_Array4) return access Graphene_Quad_T;
   pragma Import (C, Init_From_Points, "graphene_quad_init_from_points");
   --  Initializes a Graphene.Quad.Graphene_Quad_T using an array of points.
   --  Since: gtk+ 1.2
   --  @param Points an array of 4 Graphene.Point.Graphene_Point_T
   --  @return the initialized Graphene.Quad.Graphene_Quad_T

end Graphene.Quad;
