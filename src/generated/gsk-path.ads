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

--  Describes lines and curves that are more complex than simple rectangles.
--
--  Paths can used for rendering (filling or stroking) and for animations
--  (e.g. as trajectories).
--
--  `GskPath` is an immutable, opaque, reference-counted struct. After
--  creation, you cannot change the types it represents. Instead, new `GskPath`
--  objects have to be created. The [structGsk.PathBuilder] structure is meant
--  to help in this endeavor.
--
--  Conceptually, a path consists of zero or more contours (continuous,
--  connected curves), each of which may or may not be closed. Contours are
--  typically constructed from Bézier segments.
--
--  <picture> <source srcset="path-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="A Path" src="path-light.png"> </picture>
--  <gtkada_demo>create_paths.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;          use Cairo;
with Glib;           use Glib;
with Glib.String;    use Glib.String;
with Graphene.Point; use Graphene.Point;
with Graphene.Rect;  use Graphene.Rect;
with Gsk.Enums;      use Gsk.Enums;
with Gsk.Path_Point; use Gsk.Path_Point;
with Gsk.Stroke;     use Gsk.Stroke;
with Interfaces.C;   use Interfaces.C;

package Gsk.Path is

   type Gsk_Path is new Glib.C_Boxed with null record;
   Null_Gsk_Path : constant Gsk_Path;

   function From_Object (Object : System.Address) return Gsk_Path;
   function From_Object_Free (B : access Gsk_Path'Class) return Gsk_Path;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_path_get_type");

   -------------
   -- Methods --
   -------------

   function Equal (Self : Gsk_Path; Path2 : Gsk_Path) return Boolean;
   --  Returns whether two paths have identical structure.
   --  Note that it is possible to construct paths that render identical even
   --  though they don't have the same structure.
   --  Since: gtk+ 4.22
   --  @param Path2 another path
   --  @return true if Path1 and Path2 have identical structure

   function Get_Bounds
      (Self   : Gsk_Path;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T)
       return Boolean;
   --  Computes the bounds of the given path.
   --  The returned bounds may be larger than necessary, because this function
   --  aims to be fast, not accurate. The bounds are guaranteed to contain the
   --  path. For accurate bounds, use [methodGsk.Path.get_tight_bounds].
   --  It is possible that the returned rectangle has 0 width and/or height.
   --  This can happen when the path only describes a point or an axis-aligned
   --  line.
   --  If the path is empty, false is returned and Bounds are set to
   --  Graphene.Rect.Zero. This is different from the case where the path is a
   --  single point at the origin, where the Bounds will also be set to the
   --  zero rectangle but true will be returned.
   --  Since: gtk+ 4.14
   --  @param Bounds return location for the bounds
   --  @return true if the path has bounds, false if the path is known to be
   --  empty and have no bounds

   function Get_Closest_Point
      (Self      : Gsk_Path;
       Point     : not null access Graphene.Point.Graphene_Point_T;
       Threshold : Interfaces.C.C_float;
       Result    : not null access Gsk.Path_Point.Gsk_Path_Point;
       Distance  : access Interfaces.C.C_float := null) return Boolean;
   --  Computes the closest point on the path to the given point.
   --  If there is no point closer than the given threshold, false is
   --  returned.
   --  Since: gtk+ 4.14
   --  @param Point the point
   --  @param Threshold maximum allowed distance
   --  @param Result return location for the closest point
   --  @param Distance return location for the distance
   --  @return true if Point was set to the closest point on Self, false if no
   --  point is closer than Threshold

   function Get_End_Point
      (Self   : Gsk_Path;
       Result : not null access Gsk.Path_Point.Gsk_Path_Point)
       return Boolean;
   --  Gets the end point of the path.
   --  An empty path has no points, so false is returned in this case.
   --  Since: gtk+ 4.14
   --  @param Result return location for point
   --  @return true if Result was filled

   function Get_Next
      (Self  : Gsk_Path;
       Point : in out Gsk.Path_Point.Gsk_Path_Point) return Boolean;
   --  Moves Point to the next vertex.
   --  An empty path has no points, so false is returned in this case.
   --  Since: gtk+ 4.22
   --  @param Point the current point
   --  @return true if Point was set

   function Get_Previous
      (Self  : Gsk_Path;
       Point : in out Gsk.Path_Point.Gsk_Path_Point) return Boolean;
   --  Moves Point to the previous vertex.
   --  An empty path has no points, so false is returned in this case.
   --  Since: gtk+ 4.22
   --  @param Point the current point
   --  @return true if Point was set

   function Get_Start_Point
      (Self   : Gsk_Path;
       Result : not null access Gsk.Path_Point.Gsk_Path_Point)
       return Boolean;
   --  Gets the start point of the path.
   --  An empty path has no points, so false is returned in this case.
   --  Since: gtk+ 4.14
   --  @param Result return location for point
   --  @return true if Result was filled

   function Get_Stroke_Bounds
      (Self   : Gsk_Path;
       Stroke : Gsk.Stroke.Gsk_Stroke;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T)
       return Boolean;
   --  Computes the bounds for stroking the given path with the given
   --  parameters.
   --  The returned bounds may be larger than necessary, because this function
   --  aims to be fast, not accurate. The bounds are guaranteed to contain the
   --  area affected by the stroke, including protrusions like miters.
   --  Since: gtk+ 4.14
   --  @param Stroke stroke parameters
   --  @param Bounds the bounds to fill in
   --  @return true if the path has bounds, false if the path is known to be
   --  empty and have no bounds.

   function Get_Tight_Bounds
      (Self   : Gsk_Path;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T)
       return Boolean;
   --  Computes the tight bounds of the given path.
   --  This function works harder than [methodGsk.Path.get_bounds] to produce
   --  the smallest possible bounds.
   --  Since: gtk+ 4.22
   --  @param Bounds return location for the bounds
   --  @return true if the path has bounds, false if the path is known to be
   --  empty and have no bounds

   function In_Fill
      (Self      : Gsk_Path;
       Point     : not null access Graphene.Point.Graphene_Point_T;
       Fill_Rule : Gsk.Enums.Gsk_Fill_Rule) return Boolean;
   --  Returns whether a point is inside the fill area of a path.
   --  Note that this function assumes that filling a contour implicitly
   --  closes it.
   --  Since: gtk+ 4.14
   --  @param Point the point to test
   --  @param Fill_Rule the fill rule to follow
   --  @return true if Point is inside

   function Is_Closed (Self : Gsk_Path) return Boolean;
   --  Returns if the path represents a single closed contour.
   --  Since: gtk+ 4.14
   --  @return true if the path is closed

   function Is_Empty (Self : Gsk_Path) return Boolean;
   --  Checks if the path is empty, i.e. contains no lines or curves.
   --  Since: gtk+ 4.14
   --  @return true if the path is empty

   procedure Print (Self : Gsk_Path; String : Glib.String.Gstring);
   --  Converts the path into a human-readable representation.
   --  The string is compatible with (a superset of) [SVG path
   --  syntax](https://www.w3.org/TR/SVG11/paths.htmlPath_Data), see
   --  [funcGsk.Path.parse] for a summary of the syntax.
   --  Since: gtk+ 4.14
   --  @param String the string to print into

   function Ref (Self : Gsk_Path) return Gsk_Path;
   --  Increases the reference count of a path by one.
   --  Since: gtk+ 4.14
   --  @return the passed in `GskPath`. Has transfer-ownership='full'.

   procedure To_Cairo (Self : Gsk_Path; Cr : Cairo.Cairo_Context);
   --  Appends the path to a cairo context for drawing with Cairo.
   --  This may cause some suboptimal conversions to be performed as Cairo
   --  does not support all features of `GskPath`.
   --  This function does not clear the existing Cairo path. Call
   --  cairo_new_path if you want this.
   --  Since: gtk+ 4.14
   --  @param Cr a cairo context

   function To_String (Self : Gsk_Path) return UTF8_String;
   --  Converts the path into a human-readable string.
   --  You can use this function in a debugger to get a quick overview of the
   --  path.
   --  This is a wrapper around [methodGsk.Path.print], see that function for
   --  details.
   --  Since: gtk+ 4.14
   --  @return a new string for Self

   procedure Unref (Self : Gsk_Path);
   --  Decreases the reference count of a path by one.
   --  If the resulting reference count is zero, frees the path.
   --  Since: gtk+ 4.14

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Gsk_Path_Foreach_Func is access function
     (Op : Gsk.Enums.Gsk_Path_Operation;
      Points : access constant Graphene.Point.Graphene_Point_T;
      N_Points : Gsize; Weight : Interfaces.C.C_float;
      User_Data : System.Address) return Gboolean;
   pragma Convention (C, Gsk_Path_Foreach_Func);
   --  Points is a borrowed C array with N_Points elements, valid during
   --  the callback only. Return False (0) to stop iteration.

   function Foreach
     (Self : Gsk_Path; Flags : Gsk.Enums.Gsk_Path_Foreach_Flags;
      Func : Gsk_Path_Foreach_Func;
      User_Data : System.Address := System.Null_Address) return Boolean;

   ---------------
   -- Functions --
   ---------------

   function Parse (String : UTF8_String) return Gsk_Path;
   --  Constructs a path from a serialized form.
   --  The string is expected to be in (a superset of) [SVG path
   --  syntax](https://www.w3.org/TR/SVG11/paths.htmlPath_Data), as e.g.
   --  produced by [methodGsk.Path.to_string].
   --  A high-level summary of the syntax:
   --  - `M x y` Move to `(x, y)` - `L x y` Add a line from the current point
   --  to `(x, y)` - `Q x1 y1 x2 y2` Add a quadratic Bézier from the current
   --  point to `(x2, y2)`, with control point `(x1, y1)` - `C x1 y1 x2 y2 x3
   --  y3` Add a cubic Bézier from the current point to `(x3, y3)`, with
   --  control points `(x1, y1)` and `(x2, y2)` - `Z` Close the contour by
   --  drawing a line back to the start point - `H x` Add a horizontal line
   --  from the current point to the given x value - `V y` Add a vertical line
   --  from the current point to the given y value - `T x2 y2` Add a quadratic
   --  Bézier, using the reflection of the previous segments' control point as
   --  control point - `S x2 y2 x3 y3` Add a cubic Bézier, using the reflection
   --  of the previous segments' second control point as first control point -
   --  `A rx ry r l s x y` Add an elliptical arc from the current point to `(x,
   --  y)` with radii rx and ry. See the SVG documentation for how the other
   --  parameters influence the arc. - `O x1 y1 x2 y2 w` Add a rational
   --  quadratic Bézier from the current point to `(x2, y2)` with control point
   --  `(x1, y1)` and weight `w`.
   --  All the commands have lowercase variants that interpret coordinates
   --  relative to the current point.
   --  The `O` command is an extension that is not supported in SVG.
   --  Since: gtk+ 4.14
   --  @param String a string
   --  @return a new `GskPath`, or `NULL` if String could not be parsed. Has
   --  transfer-ownership='full'.

private
   Null_Gsk_Path : constant Gsk_Path :=
      (Glib.C_Boxed with null record);

end Gsk.Path;
