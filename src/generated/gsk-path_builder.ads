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

--  Constructs `GskPath` objects.
--
--  A path is constructed like this:
--
--  ```c GskPath * construct_path (void) { GskPathBuilder *builder;
--
--  builder = gsk_path_builder_new ();
--
--  // add contours to the path here
--
--  return gsk_path_builder_free_to_path (builder); ```
--
--  Adding contours to the path can be done in two ways. The easiest option is
--  to use the `gsk_path_builder_add_*` group of functions that add predefined
--  contours to the current path, either common shapes like
--  [methodGsk.PathBuilder.add_circle] or by adding from other paths like
--  [methodGsk.PathBuilder.add_path].
--
--  The `gsk_path_builder_add_*` methods always add complete contours, and do
--  not use or modify the current point.
--
--  The other option is to define each line and curve manually with the
--  `gsk_path_builder_*_to` group of functions. You start with a call to
--  [methodGsk.PathBuilder.move_to] to set the starting point and then use
--  multiple calls to any of the drawing functions to move the pen along the
--  plane. Once you are done, you can call [methodGsk.PathBuilder.close] to
--  close the path by connecting it back with a line to the starting point.
--
--  This is similar to how paths are drawn in Cairo.
--
--  Note that `GskPathBuilder` will reduce the degree of added Bézier curves
--  as much as possible, to simplify rendering.
--
--  <gtkada_demo>create_paths.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;          use Cairo;
with Glib;           use Glib;
with Graphene.Point; use Graphene.Point;
with Graphene.Rect;  use Graphene.Rect;
with Gsk.Path;       use Gsk.Path;
with Gsk.Path_Point; use Gsk.Path_Point;
with Interfaces.C;   use Interfaces.C;
with Pango.Layout;   use Pango.Layout;

package Gsk.Path_Builder is

   type Gsk_Path_Builder is new Glib.C_Boxed with null record;
   Null_Gsk_Path_Builder : constant Gsk_Path_Builder;

   function From_Object (Object : System.Address) return Gsk_Path_Builder;
   function From_Object_Free (B : access Gsk_Path_Builder'Class) return Gsk_Path_Builder;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gsk_Path_Builder);
   --  Create a new `GskPathBuilder` object.
   --  The resulting builder would create an empty `GskPath`. Use addition
   --  functions to add types to it.
   --  Since: gtk+ 4.14

   function Gsk_Path_Builder_New return Gsk_Path_Builder;
   --  Create a new `GskPathBuilder` object.
   --  The resulting builder would create an empty `GskPath`. Use addition
   --  functions to add types to it.
   --  Since: gtk+ 4.14

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_path_builder_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Circle
      (Self   : Gsk_Path_Builder;
       Center : not null access Graphene.Point.Graphene_Point_T;
       Radius : Interfaces.C.C_float);
   --  Adds a circle as a new contour.
   --  The path is going around the circle in clockwise direction.
   --  If Radius is zero, the contour will be a closed point.
   --  Since: gtk+ 4.14
   --  @param Center the center of the circle
   --  @param Radius the radius of the circle

   procedure Add_Layout
      (Self   : Gsk_Path_Builder;
       Layout : not null access Pango.Layout.Pango_Layout_Record'Class);
   --  Adds the outlines for the glyphs in Layout to the builder.
   --  Since: gtk+ 4.14
   --  @param Layout the pango layout to add

   procedure Add_Path (Self : Gsk_Path_Builder; Path : Gsk.Path.Gsk_Path);
   --  Appends all of Path to the builder.
   --  Since: gtk+ 4.14
   --  @param Path the path to append

   procedure Add_Rect
      (Self : Gsk_Path_Builder;
       Rect : not null access Graphene.Rect.Graphene_Rect_T);
   --  Adds a rectangle as a new contour.
   --  The path is going around the rectangle in clockwise direction.
   --  If the the width or height are 0, the path will be a closed horizontal
   --  or vertical line. If both are 0, it'll be a closed dot.
   --  Since: gtk+ 4.14
   --  @param Rect the rectangle to create a path for

   procedure Add_Reverse_Path
      (Self : Gsk_Path_Builder;
       Path : Gsk.Path.Gsk_Path);
   --  Appends all of Path to the builder, in reverse order.
   --  Since: gtk+ 4.14
   --  @param Path the path to append

   procedure Add_Segment
      (Self    : Gsk_Path_Builder;
       Path    : Gsk.Path.Gsk_Path;
       Start   : not null access Gsk.Path_Point.Gsk_Path_Point;
       The_End : not null access Gsk.Path_Point.Gsk_Path_Point);
   --  Adds a segment of a path to the builder.
   --  If Start is equal to or after End, the path will first add the segment
   --  from Start to the end of the path, and then add the segment from the
   --  beginning to End. If the path is closed, these segments will be
   --  connected.
   --  Note that this method always adds a path with the given start point and
   --  end point. To add a closed path, use [methodGsk.PathBuilder.add_path].
   --  Since: gtk+ 4.14
   --  @param Path the path to take the segment to
   --  @param Start the point on Path to start at
   --  @param The_End the point on Path to end at

   procedure Arc_To
      (Self : Gsk_Path_Builder;
       X1   : Interfaces.C.C_float;
       Y1   : Interfaces.C.C_float;
       X2   : Interfaces.C.C_float;
       Y2   : Interfaces.C.C_float);
   --  Adds an elliptical arc from the current point to X2, Y2 with X1, Y1
   --  determining the tangent directions.
   --  After this, X2, Y2 will be the new current point.
   --  Note: Two points and their tangents do not determine a unique ellipse,
   --  so GSK just picks one. If you need more precise control, use
   --  [methodGsk.PathBuilder.conic_to] or [methodGsk.PathBuilder.svg_arc_to].
   --  <picture> <source srcset="arc-dark.png" media="(prefers-color-scheme:
   --  dark)"> <img alt="Arc To" src="arc-light.png"> </picture>
   --  Since: gtk+ 4.14
   --  @param X1 x coordinate of first control point
   --  @param Y1 y coordinate of first control point
   --  @param X2 x coordinate of second control point
   --  @param Y2 y coordinate of second control point

   procedure Close (Self : Gsk_Path_Builder);
   --  Ends the current contour with a line back to the start point.
   --  Note that this is different from calling
   --  [methodGsk.PathBuilder.line_to] with the start point in that the contour
   --  will be closed. A closed contour behaves differently from an open one.
   --  When stroking, its start and end point are considered connected, so they
   --  will be joined via the line join, and not ended with line caps.
   --  Since: gtk+ 4.14

   procedure Conic_To
      (Self   : Gsk_Path_Builder;
       X1     : Interfaces.C.C_float;
       Y1     : Interfaces.C.C_float;
       X2     : Interfaces.C.C_float;
       Y2     : Interfaces.C.C_float;
       Weight : Interfaces.C.C_float);
   --  Adds a [conic
   --  curve](https://en.wikipedia.org/wiki/Non-uniform_rational_B-spline) from
   --  the current point to X2, Y2 with the given Weight and X1, Y1 as the
   --  control point.
   --  The weight determines how strongly the curve is pulled towards the
   --  control point. A conic with weight 1 is identical to a quadratic Bézier
   --  curve with the same points.
   --  Conic curves can be used to draw ellipses and circles. They are also
   --  known as rational quadratic Bézier curves.
   --  After this, X2, Y2 will be the new current point.
   --  <picture> <source srcset="conic-dark.png" media="(prefers-color-scheme:
   --  dark)"> <img alt="Conic To" src="conic-light.png"> </picture>
   --  Since: gtk+ 4.14
   --  @param X1 x coordinate of control point
   --  @param Y1 y coordinate of control point
   --  @param X2 x coordinate of the end of the curve
   --  @param Y2 y coordinate of the end of the curve
   --  @param Weight weight of the control point, must be greater than zero

   procedure Cubic_To
      (Self : Gsk_Path_Builder;
       X1   : Interfaces.C.C_float;
       Y1   : Interfaces.C.C_float;
       X2   : Interfaces.C.C_float;
       Y2   : Interfaces.C.C_float;
       X3   : Interfaces.C.C_float;
       Y3   : Interfaces.C.C_float);
   --  Adds a [cubic Bézier
   --  curve](https://en.wikipedia.org/wiki/BC3A9zier_curve) from the current
   --  point to X3, Y3 with X1, Y1 and X2, Y2 as the control points.
   --  After this, X3, Y3 will be the new current point.
   --  <picture> <source srcset="cubic-dark.png" media="(prefers-color-scheme:
   --  dark)"> <img alt="Cubic To" src="cubic-light.png"> </picture>
   --  Since: gtk+ 4.14
   --  @param X1 x coordinate of first control point
   --  @param Y1 y coordinate of first control point
   --  @param X2 x coordinate of second control point
   --  @param Y2 y coordinate of second control point
   --  @param X3 x coordinate of the end of the curve
   --  @param Y3 y coordinate of the end of the curve

   procedure Html_Arc_To
      (Self   : Gsk_Path_Builder;
       X1     : Interfaces.C.C_float;
       Y1     : Interfaces.C.C_float;
       X2     : Interfaces.C.C_float;
       Y2     : Interfaces.C.C_float;
       Radius : Interfaces.C.C_float);
   --  Implements arc-to according to the HTML Canvas spec.
   --  A convenience function that implements the [HTML
   --  arc_to](https://html.spec.whatwg.org/multipage/canvas.htmldom-context-2d-arcto-dev)
   --  functionality.
   --  After this, the current point will be the point where the circle with
   --  the given radius touches the line from X1, Y1 to X2, Y2.
   --  Since: gtk+ 4.14
   --  @param X1 x coordinate of first control point
   --  @param Y1 y coordinate of first control point
   --  @param X2 x coordinate of second control point
   --  @param Y2 y coordinate of second control point
   --  @param Radius radius of the circle

   procedure Line_To
      (Self : Gsk_Path_Builder;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float);
   --  Draws a line from the current point to X, Y and makes it the new
   --  current point.
   --  <picture> <source srcset="line-dark.png" media="(prefers-color-scheme:
   --  dark)"> <img alt="Line To" src="line-light.png"> </picture>
   --  Since: gtk+ 4.14
   --  @param X x coordinate
   --  @param Y y coordinate

   procedure Move_To
      (Self : Gsk_Path_Builder;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float);
   --  Starts a new contour by placing the pen at X, Y.
   --  If this function is called twice in succession, the first call will
   --  result in a contour made up of a single point. The second call will
   --  start a new contour.
   --  Since: gtk+ 4.14
   --  @param X x coordinate
   --  @param Y y coordinate

   procedure Quad_To
      (Self : Gsk_Path_Builder;
       X1   : Interfaces.C.C_float;
       Y1   : Interfaces.C.C_float;
       X2   : Interfaces.C.C_float;
       Y2   : Interfaces.C.C_float);
   --  Adds a [quadratic Bézier
   --  curve](https://en.wikipedia.org/wiki/BC3A9zier_curve) from the current
   --  point to X2, Y2 with X1, Y1 as the control point.
   --  After this, X2, Y2 will be the new current point.
   --  <picture> <source srcset="quad-dark.png" media="(prefers-color-scheme:
   --  dark)"> <img alt="Quad To" src="quad-light.png"> </picture>
   --  Since: gtk+ 4.14
   --  @param X1 x coordinate of control point
   --  @param Y1 y coordinate of control point
   --  @param X2 x coordinate of the end of the curve
   --  @param Y2 y coordinate of the end of the curve

   function Ref (Self : Gsk_Path_Builder) return Gsk_Path_Builder;
   --  Acquires a reference on the given builder.
   --  This function is intended primarily for language bindings.
   --  `GskPathBuilder` objects should not be kept around.
   --  Since: gtk+ 4.14
   --  @return the given path builder with its reference count increased. Has
   --  transfer-ownership='none'.

   procedure Rel_Arc_To
      (Self : Gsk_Path_Builder;
       X1   : Interfaces.C.C_float;
       Y1   : Interfaces.C.C_float;
       X2   : Interfaces.C.C_float;
       Y2   : Interfaces.C.C_float);
   --  Adds an elliptical arc from the current point to X2, Y2 with X1, Y1
   --  determining the tangent directions.
   --  All coordinates are given relative to the current point.
   --  This is the relative version of [methodGsk.PathBuilder.arc_to].
   --  Since: gtk+ 4.14
   --  @param X1 x coordinate of first control point
   --  @param Y1 y coordinate of first control point
   --  @param X2 x coordinate of second control point
   --  @param Y2 y coordinate of second control point

   procedure Rel_Conic_To
      (Self   : Gsk_Path_Builder;
       X1     : Interfaces.C.C_float;
       Y1     : Interfaces.C.C_float;
       X2     : Interfaces.C.C_float;
       Y2     : Interfaces.C.C_float;
       Weight : Interfaces.C.C_float);
   --  Adds a [conic
   --  curve](https://en.wikipedia.org/wiki/Non-uniform_rational_B-spline) from
   --  the current point to X2, Y2 with the given Weight and X1, Y1 as the
   --  control point.
   --  All coordinates are given relative to the current point.
   --  This is the relative version of [methodGsk.PathBuilder.conic_to].
   --  Since: gtk+ 4.14
   --  @param X1 x offset of control point
   --  @param Y1 y offset of control point
   --  @param X2 x offset of the end of the curve
   --  @param Y2 y offset of the end of the curve
   --  @param Weight weight of the curve, must be greater than zero

   procedure Rel_Cubic_To
      (Self : Gsk_Path_Builder;
       X1   : Interfaces.C.C_float;
       Y1   : Interfaces.C.C_float;
       X2   : Interfaces.C.C_float;
       Y2   : Interfaces.C.C_float;
       X3   : Interfaces.C.C_float;
       Y3   : Interfaces.C.C_float);
   --  Adds a [cubic Bézier
   --  curve](https://en.wikipedia.org/wiki/BC3A9zier_curve) from the current
   --  point to X3, Y3 with X1, Y1 and X2, Y2 as the control points.
   --  All coordinates are given relative to the current point.
   --  This is the relative version of [methodGsk.PathBuilder.cubic_to].
   --  Since: gtk+ 4.14
   --  @param X1 x offset of first control point
   --  @param Y1 y offset of first control point
   --  @param X2 x offset of second control point
   --  @param Y2 y offset of second control point
   --  @param X3 x offset of the end of the curve
   --  @param Y3 y offset of the end of the curve

   procedure Rel_Html_Arc_To
      (Self   : Gsk_Path_Builder;
       X1     : Interfaces.C.C_float;
       Y1     : Interfaces.C.C_float;
       X2     : Interfaces.C.C_float;
       Y2     : Interfaces.C.C_float;
       Radius : Interfaces.C.C_float);
   --  Implements arc-to according to the HTML Canvas spec.
   --  All coordinates are given relative to the current point.
   --  This is the relative version of [methodGsk.PathBuilder.html_arc_to].
   --  Since: gtk+ 4.14
   --  @param X1 x coordinate of first control point
   --  @param Y1 y coordinate of first control point
   --  @param X2 x coordinate of second control point
   --  @param Y2 y coordinate of second control point
   --  @param Radius radius of the circle

   procedure Rel_Line_To
      (Self : Gsk_Path_Builder;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float);
   --  Draws a line from the current point to a point offset from it by X, Y
   --  and makes it the new current point.
   --  This is the relative version of [methodGsk.PathBuilder.line_to].
   --  Since: gtk+ 4.14
   --  @param X x offset
   --  @param Y y offset

   procedure Rel_Move_To
      (Self : Gsk_Path_Builder;
       X    : Interfaces.C.C_float;
       Y    : Interfaces.C.C_float);
   --  Starts a new contour by placing the pen at X, Y relative to the current
   --  point.
   --  This is the relative version of [methodGsk.PathBuilder.move_to].
   --  Since: gtk+ 4.14
   --  @param X x offset
   --  @param Y y offset

   procedure Rel_Quad_To
      (Self : Gsk_Path_Builder;
       X1   : Interfaces.C.C_float;
       Y1   : Interfaces.C.C_float;
       X2   : Interfaces.C.C_float;
       Y2   : Interfaces.C.C_float);
   --  Adds a [quadratic Bézier
   --  curve](https://en.wikipedia.org/wiki/BC3A9zier_curve) from the current
   --  point to X2, Y2 with X1, Y1 the control point.
   --  All coordinates are given relative to the current point.
   --  This is the relative version of [methodGsk.PathBuilder.quad_to].
   --  Since: gtk+ 4.14
   --  @param X1 x offset of control point
   --  @param Y1 y offset of control point
   --  @param X2 x offset of the end of the curve
   --  @param Y2 y offset of the end of the curve

   procedure Rel_Svg_Arc_To
      (Self            : Gsk_Path_Builder;
       Rx              : Interfaces.C.C_float;
       Ry              : Interfaces.C.C_float;
       X_Axis_Rotation : Interfaces.C.C_float;
       Large_Arc       : Boolean;
       Positive_Sweep  : Boolean;
       X               : Interfaces.C.C_float;
       Y               : Interfaces.C.C_float);
   --  Implements arc-to according to the SVG spec.
   --  All coordinates are given relative to the current point.
   --  This is the relative version of [methodGsk.PathBuilder.svg_arc_to].
   --  Since: gtk+ 4.14
   --  @param Rx x radius
   --  @param Ry y radius
   --  @param X_Axis_Rotation the rotation of the ellipsis
   --  @param Large_Arc whether to add the large arc
   --  @param Positive_Sweep whether to sweep in the positive direction
   --  @param X x coordinate of the endpoint
   --  @param Y y coordinate of the endpoint

   procedure Svg_Arc_To
      (Self            : Gsk_Path_Builder;
       Rx              : Interfaces.C.C_float;
       Ry              : Interfaces.C.C_float;
       X_Axis_Rotation : Interfaces.C.C_float;
       Large_Arc       : Boolean;
       Positive_Sweep  : Boolean;
       X               : Interfaces.C.C_float;
       Y               : Interfaces.C.C_float);
   --  Implements arc-to according to the SVG spec.
   --  A convenience function that implements the [SVG
   --  arc_to](https://www.w3.org/TR/SVG11/paths.htmlPath_Data_Elliptical_Arc_Commands)
   --  functionality.
   --  After this, X, Y will be the new current point.
   --  Since: gtk+ 4.14
   --  @param Rx x radius
   --  @param Ry y radius
   --  @param X_Axis_Rotation the rotation of the ellipsis
   --  @param Large_Arc whether to add the large arc
   --  @param Positive_Sweep whether to sweep in the positive direction
   --  @param X x coordinate of the endpoint
   --  @param Y y coordinate of the endpoint

   function To_Path (Self : Gsk_Path_Builder) return Gsk.Path.Gsk_Path;
   --  Creates a new path from the given builder.
   --  The given `GskPathBuilder` is reset to the initial state once this
   --  function returns. Calling this function again on the same builder
   --  instance will therefore produce an empty path, not a copy of the same
   --  path.
   --  This function is intended primarily for language bindings. C code
   --  should use [methodGsk.PathBuilder.free_to_path].
   --  Since: gtk+ 4.14
   --  @return the newly created path with all the contours added to the
   --  builder. Has transfer-ownership='full'.

   procedure Unref (Self : Gsk_Path_Builder);
   --  Releases a reference on the given builder.
   --  Since: gtk+ 4.14

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Add_Cairo_Path (Self : Gsk_Path_Builder; Path : Cairo.Cairo_Path_Access);
   --  Copies a path obtained from Cairo.Copy_Path; the caller retains it.

   function Get_Current_Point (Self : Gsk_Path_Builder)
   return Graphene.Point.Graphene_Point_T;
   --  Returns a copy of the builder's current point.

private
   Null_Gsk_Path_Builder : constant Gsk_Path_Builder :=
      (Glib.C_Boxed with null record);

end Gsk.Path_Builder;
