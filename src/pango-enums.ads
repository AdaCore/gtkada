-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2002 ACT-Europe                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib; use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
pragma Elaborate_All (Glib.Generic_Properties);

package Pango.Enums is

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
      Pango_Underline_Low);
   pragma Convention (C, Underline);

   type Variant is
     (Pango_Variant_Normal,
      Pango_Variant_Small_Caps);
   pragma Convention (C, Variant);

   type Weight is new Gint;
   Pango_Weight_Ultralight : constant Weight := 200;
   Pango_Weight_Light      : constant Weight := 300;
   Pango_Weight_Normal     : constant Weight := 400;
   Pango_Weight_Bold       : constant Weight := 700;
   Pango_Weight_Ultrabold  : constant Weight := 800;
   Pango_Weight_Heavy      : constant Weight := 900;

   type Wrap_Mode is
     (Pango_Wrap_Word,
      Pango_Wrap_Char);
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

   type Property_Style is new Style_Properties.Property;
   type Property_Weight is new Weight_Properties.Property;
   type Property_Variant is new Variant_Properties.Property;
   type Property_Stretch is new Stretch_Properties.Property;
   type Property_Underline is new Underline_Properties.Property;

end Pango.Enums;
