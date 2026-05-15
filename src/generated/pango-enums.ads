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


pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Pango.Enums is

   type Alignment is (
      Pango_Align_Left,
      Pango_Align_Center,
      Pango_Align_Right);
   pragma Convention (C, Alignment);
   --  A Pango.Enums.Alignment describes how to align the lines of a
   --  Pango.Layout.Pango_Layout within the available space. If the
   --  Pango.Layout.Pango_Layout is set to justify using
   --  Pango.Layout.Set_Justify, this only has effect for partial lines.

   type Attr_Type is (
      Pango_Attr_Invalid,
      Pango_Attr_Language,
      Pango_Attr_Family,
      Pango_Attr_Style,
      Pango_Attr_Weight,
      Pango_Attr_Variant,
      Pango_Attr_Stretch,
      Pango_Attr_Size,
      Pango_Attr_Font_Desc,
      Pango_Attr_Foreground,
      Pango_Attr_Background,
      Pango_Attr_Underline,
      Pango_Attr_Strikethrough,
      Pango_Attr_Rise,
      Pango_Attr_Shape,
      Pango_Attr_Scale,
      Pango_Attr_Fallback,
      Pango_Attr_Letter_Spacing,
      Pango_Attr_Underline_Color,
      Pango_Attr_Strikethrough_Color,
      Pango_Attr_Absolute_Size,
      Pango_Attr_Gravity,
      Pango_Attr_Gravity_Hint,
      Pango_Attr_Font_Features,
      Pango_Attr_Foreground_Alpha,
      Pango_Attr_Background_Alpha,
      Pango_Attr_Allow_Breaks,
      Pango_Attr_Show,
      Pango_Attr_Insert_Hyphens,
      Pango_Attr_Overline,
      Pango_Attr_Overline_Color);
   pragma Convention (C, Attr_Type);
   --  The Pango.Enums.Attr_Type distinguishes between different types of
   --  attributes. Along with the predefined values, it is possible to allocate
   --  additional values for custom attributes using pango_attr_type_register.
   --  The predefined values are given below. The type of structure used to
   --  store the attribute is listed in parentheses after the description.

   type Coverage_Level is (
      Pango_Coverage_None,
      Pango_Coverage_Fallback,
      Pango_Coverage_Approximate,
      Pango_Coverage_Exact);
   pragma Convention (C, Coverage_Level);
   --  Used to indicate how well a font can represent a particular Unicode
   --  character point for a particular script.
   --
   --  Since 1.44, only Pango.Enums.Pango_Coverage_None and
   --  Pango.Enums.Pango_Coverage_Exact will be returned.

   type Direction is (
      Pango_Direction_Ltr,
      Pango_Direction_Rtl,
      Pango_Direction_Ttb_Ltr,
      Pango_Direction_Ttb_Rtl,
      Pango_Direction_Weak_Ltr,
      Pango_Direction_Weak_Rtl,
      Pango_Direction_Neutral);
   pragma Convention (C, Direction);
   --  The Pango.Enums.Direction type represents a direction in the Unicode
   --  bidirectional algorithm; not every value in this enumeration makes sense
   --  for every usage of Pango.Enums.Direction; for example, the return value
   --  of pango_unichar_direction and pango_find_base_dir cannot be
   --  Pango.Enums.Pango_Direction_Weak_Ltr or
   --  Pango.Enums.Pango_Direction_Weak_Rtl, since every character is either
   --  neutral or has a strong direction; on the other hand
   --  Pango.Enums.Pango_Direction_Neutral doesn't make sense to pass to
   --  pango_itemize_with_base_dir.
   --
   --  The Pango.Enums.Pango_Direction_Ttb_Ltr,
   --  Pango.Enums.Pango_Direction_Ttb_Rtl values come from an earlier
   --  interpretation of this enumeration as the writing direction of a block
   --  of text and are no longer used; See Pango.Enums.Gravity for how vertical
   --  text is handled in Pango.
   --
   --  If you are interested in text direction, you should really use fribidi
   --  directly. PangoDirection is only retained because it is used in some
   --  public apis.

   type Font_Mask is mod 2 ** Integer'Size;
   pragma Convention (C, Font_Mask);
   --  The bits in a Pango.Enums.Font_Mask correspond to fields in a
   --  Pango.Font.Pango_Font_Description that have been set.

   Pango_Font_Mask_Family : constant Font_Mask := 1;
   Pango_Font_Mask_Style : constant Font_Mask := 2;
   Pango_Font_Mask_Variant : constant Font_Mask := 4;
   Pango_Font_Mask_Weight : constant Font_Mask := 8;
   Pango_Font_Mask_Stretch : constant Font_Mask := 16;
   Pango_Font_Mask_Size : constant Font_Mask := 32;
   Pango_Font_Mask_Gravity : constant Font_Mask := 64;
   Pango_Font_Mask_Variations : constant Font_Mask := 128;

   type Gravity is (
      Pango_Gravity_South,
      Pango_Gravity_East,
      Pango_Gravity_North,
      Pango_Gravity_West,
      Pango_Gravity_Auto);
   pragma Convention (C, Gravity);
   --  The Pango.Enums.Gravity type represents the orientation of glyphs in a
   --  segment of text. This is useful when rendering vertical text layouts. In
   --  those situations, the layout is rotated using a non-identity
   --  PangoMatrix, and then glyph orientation is controlled using
   --  Pango.Enums.Gravity. Not every value in this enumeration makes sense for
   --  every usage of Pango.Enums.Gravity; for example,
   --  Pango.Enums.Pango_Gravity_Auto only can be passed to
   --  Pango.Context.Set_Base_Gravity and can only be returned by
   --  Pango.Context.Get_Base_Gravity.
   --
   --  See also: Pango.Enums.GravityHint

   type GravityHint is (
      Pango_Gravity_Hint_Natural,
      Pango_Gravity_Hint_Strong,
      Pango_Gravity_Hint_Line);
   pragma Convention (C, GravityHint);
   --  The Pango.Enums.GravityHint defines how horizontal scripts should
   --  behave in a vertical context. That is, English excerpt in a vertical
   --  paragraph for example.
   --
   --  See Pango.Enums.Gravity.

   type Stretch is (
      Pango_Stretch_Ultra_Condensed,
      Pango_Stretch_Extra_Condensed,
      Pango_Stretch_Condensed,
      Pango_Stretch_Semi_Condensed,
      Pango_Stretch_Normal,
      Pango_Stretch_Semi_Expanded,
      Pango_Stretch_Expanded,
      Pango_Stretch_Extra_Expanded,
      Pango_Stretch_Ultra_Expanded);
   pragma Convention (C, Stretch);
   --  An enumeration specifying the width of the font relative to other
   --  designs within a family.

   type Style is (
      Pango_Style_Normal,
      Pango_Style_Oblique,
      Pango_Style_Italic);
   pragma Convention (C, Style);
   --  An enumeration specifying the various slant styles possible for a font.

   type Underline is (
      Pango_Underline_None,
      Pango_Underline_Single,
      Pango_Underline_Double,
      Pango_Underline_Low,
      Pango_Underline_Error,
      Pango_Underline_Single_Line,
      Pango_Underline_Double_Line,
      Pango_Underline_Error_Line);
   pragma Convention (C, Underline);
   --  The Pango.Enums.Underline enumeration is used to specify whether text
   --  should be underlined, and if so, the type of underlining.

   type Variant is (
      Pango_Variant_Normal,
      Pango_Variant_Small_Caps);
   pragma Convention (C, Variant);
   --  An enumeration specifying capitalization variant of the font.

   type Weight is (
      Pango_Weight_Thin,
      Pango_Weight_Ultralight,
      Pango_Weight_Light,
      Pango_Weight_Semilight,
      Pango_Weight_Book,
      Pango_Weight_Normal,
      Pango_Weight_Medium,
      Pango_Weight_Semibold,
      Pango_Weight_Bold,
      Pango_Weight_Ultrabold,
      Pango_Weight_Heavy,
      Pango_Weight_Ultraheavy);
   pragma Convention (C, Weight);
   --  An enumeration specifying the weight (boldness) of a font. This is a
   --  numerical value ranging from 100 to 1000, but there are some predefined
   --  values:

   for Weight use (
      Pango_Weight_Thin => 100,
      Pango_Weight_Ultralight => 200,
      Pango_Weight_Light => 300,
      Pango_Weight_Semilight => 350,
      Pango_Weight_Book => 380,
      Pango_Weight_Normal => 400,
      Pango_Weight_Medium => 500,
      Pango_Weight_Semibold => 600,
      Pango_Weight_Bold => 700,
      Pango_Weight_Ultrabold => 800,
      Pango_Weight_Heavy => 900,
      Pango_Weight_Ultraheavy => 1000);

   type Wrap_Mode is (
      Pango_Wrap_Word,
      Pango_Wrap_Char,
      Pango_Wrap_Word_Char);
   pragma Convention (C, Wrap_Mode);
   --  A Pango.Enums.Wrap_Mode describes how to wrap the lines of a
   --  Pango.Layout.Pango_Layout to the desired width.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Alignment_Properties is
      new Generic_Internal_Discrete_Property (Alignment);
   type Property_Alignment is new Alignment_Properties.Property;

   package Attr_Type_Properties is
      new Generic_Internal_Discrete_Property (Attr_Type);
   type Property_Attr_Type is new Attr_Type_Properties.Property;

   package Coverage_Level_Properties is
      new Generic_Internal_Discrete_Property (Coverage_Level);
   type Property_Coverage_Level is new Coverage_Level_Properties.Property;

   package Direction_Properties is
      new Generic_Internal_Discrete_Property (Direction);
   type Property_Direction is new Direction_Properties.Property;

   package Font_Mask_Properties is
      new Generic_Internal_Discrete_Property (Font_Mask);
   type Property_Font_Mask is new Font_Mask_Properties.Property;

   package Gravity_Properties is
      new Generic_Internal_Discrete_Property (Gravity);
   type Property_Gravity is new Gravity_Properties.Property;

   package GravityHint_Properties is
      new Generic_Internal_Discrete_Property (GravityHint);
   type Property_GravityHint is new GravityHint_Properties.Property;

   package Stretch_Properties is
      new Generic_Internal_Discrete_Property (Stretch);
   type Property_Stretch is new Stretch_Properties.Property;

   package Style_Properties is
      new Generic_Internal_Discrete_Property (Style);
   type Property_Style is new Style_Properties.Property;

   package Underline_Properties is
      new Generic_Internal_Discrete_Property (Underline);
   type Property_Underline is new Underline_Properties.Property;

   package Variant_Properties is
      new Generic_Internal_Discrete_Property (Variant);
   type Property_Variant is new Variant_Properties.Property;

   package Weight_Properties is
      new Generic_Internal_Discrete_Property (Weight);
   type Property_Weight is new Weight_Properties.Property;

   package Wrap_Mode_Properties is
      new Generic_Internal_Discrete_Property (Wrap_Mode);
   type Property_Wrap_Mode is new Wrap_Mode_Properties.Property;

   ----------------------
   -- GtkAda additions --
   ----------------------

   Pango_Scale : constant := 1024;
   --  All internal units in Pango are expressed in terms of this unit. A
   --  typical pango size must be divided by Pango_Scale to get the equivalent
   --  in pixels.

   function To_Pixels (Pango_Units : Gint) return Gint;
   --  Convert a size in pango units to pixels. This is a rounding of
   --  Pango_Units divided by Pango_Scale

   --  Enum types are bound with the following algorithm:
   --    + the "Pango" prefix of the type name is stripped
   --    + Each word of the type name is separated by '_'
   --    + the full enum name is kept, but capitalized.

   --  All enums types should be sorted by alphabetical order...

end Pango.Enums;
