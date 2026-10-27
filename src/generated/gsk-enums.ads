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


pragma Warnings (Off, "*is already use-visible*");
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Gsk.Enums is

   type Gsk_Transform_Category is (
      Gsk_Transform_Category_Unknown,
      Gsk_Transform_Category_Any,
      Gsk_Transform_Category_3D,
      Gsk_Transform_Category_2D,
      Gsk_Transform_Category_2D_Affine,
      Gsk_Transform_Category_2D_Translate,
      Gsk_Transform_Category_Identity);
   pragma Convention (C, Gsk_Transform_Category);
   --  The categories of matrices relevant for GSK and GTK.
   --
   --  Note that any category includes matrices of all later categories. So if
   --  you want to for example check if a matrix is a 2D matrix, `category >=
   --  GSK_TRANSFORM_CATEGORY_2D` is the way to do this.
   --
   --  Also keep in mind that rounding errors may cause matrices to not
   --  conform to their categories. Otherwise, matrix operations done via
   --  multiplication will not worsen categories. So for the matrix
   --  multiplication `C = A * B`, `category(C) = MIN (category(A),
   --  category(B))`.

   type Gsk_Fill_Rule is (
      Gsk_Fill_Rule_Winding,
      Gsk_Fill_Rule_Even_Odd);
   pragma Convention (C, Gsk_Fill_Rule);
   --  Specifies how paths are filled.
   --
   --  Whether or not a point is included in the fill is determined by taking
   --  a ray from that point to infinity and looking at intersections with the
   --  path. The ray can be in any direction, as long as it doesn't pass
   --  through the end point of a segment or have a tricky intersection such as
   --  intersecting tangent to the path.
   --
   --  (Note that filling is not actually implemented in this way. This is
   --  just a description of the rule that is applied.)
   --
   --  New entries may be added in future versions.

   type Gsk_Line_Cap is (
      Gsk_Line_Cap_Butt,
      Gsk_Line_Cap_Round,
      Gsk_Line_Cap_Square);
   pragma Convention (C, Gsk_Line_Cap);
   --  Specifies how to render the start and end points of contours or dashes
   --  when stroking.
   --
   --  The default line cap style is `GSK_LINE_CAP_BUTT`.
   --
   --  New entries may be added in future versions.
   --
   --  <figure> <picture> <source srcset="caps-dark.png"
   --  media="(prefers-color-scheme: dark)"> <img alt="Line Cap Styles"
   --  src="caps-light.png"> </picture> <figcaption>GSK_LINE_CAP_BUTT,
   --  GSK_LINE_CAP_ROUND, GSK_LINE_CAP_SQUARE</figcaption> </figure>

   type Gsk_Line_Join is (
      Gsk_Line_Join_Miter,
      Gsk_Line_Join_Round,
      Gsk_Line_Join_Bevel);
   pragma Convention (C, Gsk_Line_Join);
   --  Specifies how to render the junction of two lines when stroking.
   --
   --  The default line join style is `GSK_LINE_JOIN_MITER`.
   --
   --  New entries may be added in future versions.
   --
   --  <figure> <picture> <source srcset="join-dark.png"
   --  media="(prefers-color-scheme: dark)"> <img alt="Line Join Styles"
   --  src="join-light.png"> </picture> <figcaption>GSK_LINE_JOINT_MITER,
   --  GSK_LINE_JOINT_ROUND, GSK_LINE_JOIN_BEVEL</figcaption> </figure>

   type Gsk_Path_Direction is (
      Gsk_Path_From_Start,
      Gsk_Path_To_Start,
      Gsk_Path_To_End,
      Gsk_Path_From_End);
   pragma Convention (C, Gsk_Path_Direction);
   --  Used to pick one of the four tangents at a given point on the path.
   --
   --  Note that the directions for Gsk_Path_From_Start/Gsk_Path_To_End and
   --  Gsk_Path_To_Start/Gsk_Path_From_End will coincide for smooth points.
   --  Only sharp turns will exhibit four different directions.
   --
   --  <picture> <source srcset="directions-dark.png"
   --  media="(prefers-color-scheme: dark)"> <img alt="Path Tangents"
   --  src="directions-light.png"> </picture>

   type Gsk_Path_Operation is (
      Gsk_Path_Move,
      Gsk_Path_Close,
      Gsk_Path_Line,
      Gsk_Path_Quad,
      Gsk_Path_Cubic,
      Gsk_Path_Conic);
   pragma Convention (C, Gsk_Path_Operation);
   --  Describes the segments of a `GskPath`.
   --
   --  More values may be added in the future.

   type Gsk_Path_Foreach_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gsk_Path_Foreach_Flags);
   --  Flags that can be passed to gsk_path_foreach to influence what kinds of
   --  operations the path is decomposed into.
   --
   --  By default, [methodGsk.Path.foreach] will only emit a path with all
   --  operations flattened to straight lines to allow for maximum
   --  compatibility. The only operations emitted will be `GSK_PATH_MOVE`,
   --  `GSK_PATH_LINE` and `GSK_PATH_CLOSE`.

   Gsk_Path_Foreach_Allow_Only_Lines : constant Gsk_Path_Foreach_Flags := 0;
   Gsk_Path_Foreach_Allow_Quad : constant Gsk_Path_Foreach_Flags := 1;
   Gsk_Path_Foreach_Allow_Cubic : constant Gsk_Path_Foreach_Flags := 2;
   Gsk_Path_Foreach_Allow_Conic : constant Gsk_Path_Foreach_Flags := 4;

   type Gsk_Blend_Mode is (
      Gsk_Blend_Mode_Default,
      Gsk_Blend_Mode_Multiply,
      Gsk_Blend_Mode_Screen,
      Gsk_Blend_Mode_Overlay,
      Gsk_Blend_Mode_Darken,
      Gsk_Blend_Mode_Lighten,
      Gsk_Blend_Mode_Color_Dodge,
      Gsk_Blend_Mode_Color_Burn,
      Gsk_Blend_Mode_Hard_Light,
      Gsk_Blend_Mode_Soft_Light,
      Gsk_Blend_Mode_Difference,
      Gsk_Blend_Mode_Exclusion,
      Gsk_Blend_Mode_Color,
      Gsk_Blend_Mode_Hue,
      Gsk_Blend_Mode_Saturation,
      Gsk_Blend_Mode_Luminosity);
   pragma Convention (C, Gsk_Blend_Mode);
   --  The blend modes available for render nodes.
   --
   --  The implementation of each blend mode is deferred to the rendering
   --  pipeline.
   --
   --  See <https://www.w3.org/TR/compositing-1/blending> for more information
   --  on blending and blend modes.

   type Gsk_Mask_Mode is (
      Gsk_Mask_Mode_Alpha,
      Gsk_Mask_Mode_Inverted_Alpha,
      Gsk_Mask_Mode_Luminance,
      Gsk_Mask_Mode_Inverted_Luminance);
   pragma Convention (C, Gsk_Mask_Mode);
   --  The mask modes available for mask nodes.

   type Gsk_Porter_Duff is (
      Gsk_Porter_Duff_Source,
      Gsk_Porter_Duff_Dest,
      Gsk_Porter_Duff_Source_Over_Dest,
      Gsk_Porter_Duff_Dest_Over_Source,
      Gsk_Porter_Duff_Source_In_Dest,
      Gsk_Porter_Duff_Dest_In_Source,
      Gsk_Porter_Duff_Source_Out_Dest,
      Gsk_Porter_Duff_Dest_Out_Source,
      Gsk_Porter_Duff_Source_Atop_Dest,
      Gsk_Porter_Duff_Dest_Atop_Source,
      Gsk_Porter_Duff_Xor,
      Gsk_Porter_Duff_Clear);
   pragma Convention (C, Gsk_Porter_Duff);
   --  GSK_PORTER_DUFF_SOURCE: GSK_PORTER_DUFF_DEST:
   --  GSK_PORTER_DUFF_SOURCE_OVER_DEST: GSK_PORTER_DUFF_DEST_OVER_SOURCE:
   --  GSK_PORTER_DUFF_SOURCE_IN_DEST: GSK_PORTER_DUFF_DEST_IN_SOURCE:
   --  GSK_PORTER_DUFF_SOURCE_OUT_DEST: GSK_PORTER_DUFF_DEST_OUT_SOURCE:
   --  GSK_PORTER_DUFF_SOURCE_ATOP_DEST: GSK_PORTER_DUFF_DEST_ATOP_SOURCE:
   --  GSK_PORTER_DUFF_XOR: GSK_PORTER_DUFF_CLEAR: The 12 compositing modes
   --  defined by the seminal paper by Thomas Porter and Tom Duff.
   --
   --  They are used in SVG, PDF and in Cairo with `cairo_operator_t`.

   type Gsk_Isolation is mod 2 ** Integer'Size;
   pragma Convention (C, Gsk_Isolation);
   --  These flags describe the types of isolations possible with a
   --  [classGsk.IsolationNode].
   --
   --  More isolation options may be added in the future.

   Gsk_Isolation_None : constant Gsk_Isolation := 0;
   Gsk_Isolation_Background : constant Gsk_Isolation := 1;
   Gsk_Isolation_Copy_Paste : constant Gsk_Isolation := 2;
   Gsk_Isolation_All : constant Gsk_Isolation := -1;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gsk_Transform_Category_Properties is
      new Generic_Internal_Discrete_Property (Gsk_Transform_Category);
   type Property_Gsk_Transform_Category is new Gsk_Transform_Category_Properties.Property;

   package Gsk_Fill_Rule_Properties is
      new Generic_Internal_Discrete_Property (Gsk_Fill_Rule);
   type Property_Gsk_Fill_Rule is new Gsk_Fill_Rule_Properties.Property;

   package Gsk_Line_Cap_Properties is
      new Generic_Internal_Discrete_Property (Gsk_Line_Cap);
   type Property_Gsk_Line_Cap is new Gsk_Line_Cap_Properties.Property;

   package Gsk_Line_Join_Properties is
      new Generic_Internal_Discrete_Property (Gsk_Line_Join);
   type Property_Gsk_Line_Join is new Gsk_Line_Join_Properties.Property;

   package Gsk_Path_Direction_Properties is
      new Generic_Internal_Discrete_Property (Gsk_Path_Direction);
   type Property_Gsk_Path_Direction is new Gsk_Path_Direction_Properties.Property;

   package Gsk_Path_Operation_Properties is
      new Generic_Internal_Discrete_Property (Gsk_Path_Operation);
   type Property_Gsk_Path_Operation is new Gsk_Path_Operation_Properties.Property;

   package Gsk_Path_Foreach_Flags_Properties is
      new Generic_Internal_Flags_Property (Gsk_Path_Foreach_Flags);
   type Property_Gsk_Path_Foreach_Flags is new Gsk_Path_Foreach_Flags_Properties.Property;

   package Gsk_Blend_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gsk_Blend_Mode);
   type Property_Gsk_Blend_Mode is new Gsk_Blend_Mode_Properties.Property;

   package Gsk_Mask_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gsk_Mask_Mode);
   type Property_Gsk_Mask_Mode is new Gsk_Mask_Mode_Properties.Property;

   package Gsk_Porter_Duff_Properties is
      new Generic_Internal_Discrete_Property (Gsk_Porter_Duff);
   type Property_Gsk_Porter_Duff is new Gsk_Porter_Duff_Properties.Property;

   package Gsk_Isolation_Properties is
      new Generic_Internal_Flags_Property (Gsk_Isolation);
   type Property_Gsk_Isolation is new Gsk_Isolation_Properties.Property;

end Gsk.Enums;
