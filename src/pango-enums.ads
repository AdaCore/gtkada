------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

--  <group>Pango, font handling</group>

with Glib; use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
pragma Elaborate_All (Glib.Generic_Properties);

package Pango.Enums is

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

   type Alignment is
     (Pango_Alignment_Left,
      Pango_Alignment_Center,
      Pango_Alignment_Right);
   pragma Convention (C, Alignment);

   type Attr_Type is
     (Pango_Attr_Invalid,
      Pango_Attr_Lang,
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
      Pango_Attr_Scale);
   pragma Convention (C, Attr_Type);

   type Coverage_Level is
     (Pango_Underline_None,
      Pango_Underline_Fallback,
      Pango_Underline_Approximate,
      Pango_Underline_Exact);
   pragma Convention (C, Coverage_Level);

   type Direction is
     (Pango_Direction_Ltr,
      Pango_Direction_Rtl,
      Pango_Direction_Ttb_Ltr,
      Pango_Direction_Ttb_Rtl);
   pragma Convention (C, Direction);

   --  There are some PANGO_SCALE_* macros in pango-font.h that are not
   --  bound yet. Are they needed ???

   type Stretch is
     (Pango_Stretch_Ultra_Condensed,
      Pango_Stretch_Extra_Condensed,
      Pango_Stretch_Condensed,
      Pango_Stretch_Semi_Condensed,
      Pango_Stretch_Normal,
      Pango_Stretch_Semi_Expanded,
      Pango_Stretch_Expanded,
      Pango_Stretch_Extra_Expanded,
      Pango_Stretch_Ultra_Expanded);
   pragma Convention (C, Stretch);

   type Style is
     (Pango_Style_Normal,
      Pango_Style_Oblique,
      Pango_Style_Italic);
   pragma Convention (C, Style);

   type Underline is
     (Pango_Underline_None,
      Pango_Underline_Single,
      Pango_Underline_Double,
      Pango_Underline_Low,
      Pango_Underline_Error);
   pragma Convention (C, Underline);

   type Variant is
     (Pango_Variant_Normal,
      Pango_Variant_Small_Caps);
   pragma Convention (C, Variant);

   type Weight is
     (Pango_Weight_Ultralight,
      Pango_Weight_Light,
      Pango_Weight_Normal,
      Pango_Weight_Medium,
      Pango_Weight_Semi_Bold,
      Pango_Weight_Bold,
      Pango_Weight_Ultrabold,
      Pango_Weight_Heavy);
   pragma Convention (C, Weight);

   for Weight use
     (Pango_Weight_Ultralight => 200,
      Pango_Weight_Light      => 300,
      Pango_Weight_Normal     => 400,
      Pango_Weight_Medium     => 500,
      Pango_Weight_Semi_Bold  => 600,
      Pango_Weight_Bold       => 700,
      Pango_Weight_Ultrabold  => 800,
      Pango_Weight_Heavy      => 900);

   type Wrap_Mode is
     (Pango_Wrap_Word,
      Pango_Wrap_Char,
      Pango_Wrap_Word_Char);
   pragma Convention (C, Wrap_Mode);

   ----------------
   -- Properties --
   ----------------
   --  See the package Glib.Properties for more information on how to
   --  use properties

   package Style_Properties is
     new Generic_Internal_Discrete_Property (Style);
   package Weight_Properties is
     new Generic_Internal_Discrete_Property (Weight);
   package Variant_Properties is
     new Generic_Internal_Discrete_Property (Variant);
   package Stretch_Properties is
     new Generic_Internal_Discrete_Property (Stretch);
   package Underline_Properties is
     new Generic_Internal_Discrete_Property (Underline);
   package Wrap_Mode_Properties is
     new Generic_Internal_Discrete_Property (Wrap_Mode);

   type Property_Style is new Style_Properties.Property;
   type Property_Weight is new Weight_Properties.Property;
   type Property_Variant is new Variant_Properties.Property;
   type Property_Stretch is new Stretch_Properties.Property;
   type Property_Underline is new Underline_Properties.Property;
   type Property_Wrap_Mode is new Wrap_Mode_Properties.Property;

end Pango.Enums;
