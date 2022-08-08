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

--  <description>
--  The Pango.Font.Pango_Font structure is used to represent a font in a
--  rendering-system-independent matter. To create an implementation of a
--  Pango.Font.Pango_Font, the rendering-system specific code should allocate a
--  larger structure that contains a nested Pango.Font.Pango_Font, fill in the
--  <structfield>klass</structfield> member of the nested Pango.Font.Pango_Font
--  with a pointer to a appropriate Pango_Font_Class, then call pango_font_init
--  on the structure.
--
--  The Pango.Font.Pango_Font structure contains one member which the
--  implementation fills in.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Pango.Enums;             use Pango.Enums;
with Pango.Font_Metrics;      use Pango.Font_Metrics;
with Pango.Language;          use Pango.Language;

package Pango.Font is

   type Pango_Font_Record is new GObject_Record with null record;
   type Pango_Font is access all Pango_Font_Record'Class;

   type Pango_Font_Description is new Glib.C_Proxy;

   ------------------
   -- Constructors --
   ------------------

   function Font_Get_Type return Glib.GType;
   pragma Import (C, Font_Get_Type, "pango_font_get_type");

   procedure Gdk_New (Self : out Pango_Font_Description);
   --  Creates a new font description structure with all fields unset.

   function Pango_Font_Description_New return Pango_Font_Description;
   --  Creates a new font description structure with all fields unset.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "pango_font_description_get_type");

   -------------
   -- Methods --
   -------------

   function Describe
      (Font : not null access Pango_Font_Record'Class)
       return Pango_Font_Description;
   --  Returns a description of the font, with font size set in points. Use
   --  Pango.Font.Describe_With_Absolute_Size if you want the font size in
   --  device units.

   function Describe_With_Absolute_Size
      (Font : not null access Pango_Font_Record'Class)
       return Pango_Font_Description;
   --  Returns a description of the font, with absolute font size set (in
   --  device units). Use Pango.Font.Describe if you want the font size in
   --  points.
   --  Since: gtk+ 1.14

   procedure Get_Glyph_Extents
      (Font         : not null access Pango_Font_Record;
       Glyph        : Pango_Glyph;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle);
   --  Gets the logical and ink extents of a glyph within a font. The
   --  coordinate system for each rectangle has its origin at the base line and
   --  horizontal origin of the character with increasing coordinates extending
   --  to the right and down. The macros PANGO_ASCENT, PANGO_DESCENT,
   --  PANGO_LBEARING, and PANGO_RBEARING can be used to convert from the
   --  extents rectangle to more traditional font metrics. The units of the
   --  rectangles are in 1/PANGO_SCALE of a device unit.
   --  If Font is null, this function gracefully sets some sane values in the
   --  output variables and returns.
   --  "glyph": the glyph index
   --  "ink_rect": rectangle used to store the extents of the glyph as drawn
   --  or null to indicate that the result is not needed.
   --  "logical_rect": rectangle used to store the logical extents of the
   --  glyph or null to indicate that the result is not needed.

   function Get_Metrics
      (Font     : not null access Pango_Font_Record;
       Language : Pango.Language.Pango_Language := Pango.Language.Null_Pango_Language)
       return Pango.Font_Metrics.Pango_Font_Metrics;
   --  Gets overall metric information for a font. Since the metrics may be
   --  substantially different for different scripts, a language tag can be
   --  provided to indicate that the metrics should be retrieved that
   --  correspond to the script(s) used by that language.
   --  If Font is null, this function gracefully sets some sane values in the
   --  output variables and returns.
   --  "language": language tag used to determine which script to get the
   --  metrics for, or null to indicate to get the metrics for the entire font.

   function Has_Char
      (Font : not null access Pango_Font_Record;
       Wc   : Gunichar) return Boolean;
   --  Returns whether the font provides a glyph for this character.
   --  Returns True if Font can render Wc
   --  Since: gtk+ 1.44
   --  "wc": a Unicode character

   function Better_Match
      (Self      : Pango_Font_Description;
       Old_Match : Pango_Font_Description;
       New_Match : Pango_Font_Description) return Boolean;
   --  Determines if the style attributes of New_Match are a closer match for
   --  Desc than those of Old_Match are, or if Old_Match is null, determines if
   --  New_Match is a match at all. Approximate matching is done for weight and
   --  style; other style attributes must match exactly. Style attributes are
   --  all attributes other than family and size-related attributes.
   --  Approximate matching for style considers PANGO_STYLE_OBLIQUE and
   --  PANGO_STYLE_ITALIC as matches, but not as good a match as when the
   --  styles are equal.
   --  Note that Old_Match must match Desc.
   --  "old_match": a Pango.Font.Pango_Font_Description, or null
   --  "new_match": a Pango.Font.Pango_Font_Description

   function Copy
      (Self : Pango_Font_Description) return Pango_Font_Description;
   pragma Import (C, Copy, "pango_font_description_copy");
   --  Make a copy of a Pango.Font.Pango_Font_Description.

   function Copy_Static
      (Self : Pango_Font_Description) return Pango_Font_Description;
   pragma Import (C, Copy_Static, "pango_font_description_copy_static");
   --  Like Pango.Font.Copy, but only a shallow copy is made of the family
   --  name and other allocated fields. The result can only be used until Desc
   --  is modified or freed. This is meant to be used when the copy is only
   --  needed temporarily.

   function Equal
      (Self  : Pango_Font_Description;
       Desc2 : Pango_Font_Description) return Boolean;
   --  Compares two font descriptions for equality. Two font descriptions are
   --  considered equal if the fonts they describe are provably identical. This
   --  means that their masks do not have to match, as long as other fields are
   --  all the same. (Two font descriptions may result in identical fonts being
   --  loaded, but still compare False.)
   --  "desc2": another Pango.Font.Pango_Font_Description

   function Get_Family (Self : Pango_Font_Description) return UTF8_String;
   --  Gets the family name field of a font description. See
   --  Pango.Font.Set_Family.

   procedure Set_Family
      (Self   : Pango_Font_Description;
       Family : UTF8_String);
   --  Sets the family name field of a font description. The family name
   --  represents a family of related font styles, and will resolve to a
   --  particular Pango.Font_Family.Pango_Font_Family. In some uses of
   --  Pango.Font.Pango_Font_Description, it is also possible to use a comma
   --  separated list of family names for this field.
   --  "family": a string representing the family name.

   function Get_Gravity
      (Self : Pango_Font_Description) return Pango.Enums.Gravity;
   pragma Import (C, Get_Gravity, "pango_font_description_get_gravity");
   --  Gets the gravity field of a font description. See
   --  Pango.Font.Set_Gravity.
   --  Since: gtk+ 1.16

   procedure Set_Gravity
      (Self    : Pango_Font_Description;
       Gravity : Pango.Enums.Gravity);
   pragma Import (C, Set_Gravity, "pango_font_description_set_gravity");
   --  Sets the gravity field of a font description. The gravity field
   --  specifies how the glyphs should be rotated. If Gravity is
   --  Pango.Enums.Pango_Gravity_Auto, this actually unsets the gravity mask on
   --  the font description.
   --  This function is seldom useful to the user. Gravity should normally be
   --  set on a Pango.Context.Pango_Context.
   --  Since: gtk+ 1.16
   --  "gravity": the gravity for the font description.

   function Get_Set_Fields
      (Self : Pango_Font_Description) return Pango.Enums.Font_Mask;
   pragma Import (C, Get_Set_Fields, "pango_font_description_get_set_fields");
   --  Determines which fields in a font description have been set.

   function Get_Size (Self : Pango_Font_Description) return Glib.Gint;
   pragma Import (C, Get_Size, "pango_font_description_get_size");
   --  Gets the size field of a font description. See Pango.Font.Set_Size.

   procedure Set_Size (Self : Pango_Font_Description; Size : Glib.Gint);
   pragma Import (C, Set_Size, "pango_font_description_set_size");
   --  Sets the size field of a font description in fractional points. This is
   --  mutually exclusive with Pango.Font.Set_Absolute_Size.
   --  "size": the size of the font in points, scaled by PANGO_SCALE. (That
   --  is, a Size value of 10 * PANGO_SCALE is a 10 point font. The conversion
   --  factor between points and device units depends on system configuration
   --  and the output device. For screen display, a logical DPI of 96 is
   --  common, in which case a 10 point font corresponds to a 10 * (96 / 72) =
   --  13.3 pixel font. Use Pango.Font.Set_Absolute_Size if you need a
   --  particular size in device units.

   function Get_Size_Is_Absolute
      (Self : Pango_Font_Description) return Boolean;
   --  Determines whether the size of the font is in points (not absolute) or
   --  device units (absolute). See Pango.Font.Set_Size and
   --  Pango.Font.Set_Absolute_Size.
   --  Since: gtk+ 1.8

   function Get_Stretch
      (Self : Pango_Font_Description) return Pango.Enums.Stretch;
   pragma Import (C, Get_Stretch, "pango_font_description_get_stretch");
   --  Gets the stretch field of a font description. See
   --  Pango.Font.Set_Stretch.

   procedure Set_Stretch
      (Self    : Pango_Font_Description;
       Stretch : Pango.Enums.Stretch);
   pragma Import (C, Set_Stretch, "pango_font_description_set_stretch");
   --  Sets the stretch field of a font description. The stretch field
   --  specifies how narrow or wide the font should be.
   --  "stretch": the stretch for the font description

   function Get_Style
      (Self : Pango_Font_Description) return Pango.Enums.Style;
   pragma Import (C, Get_Style, "pango_font_description_get_style");
   --  Gets the style field of a Pango.Font.Pango_Font_Description. See
   --  Pango.Font.Set_Style.

   procedure Set_Style
      (Self  : Pango_Font_Description;
       Style : Pango.Enums.Style);
   pragma Import (C, Set_Style, "pango_font_description_set_style");
   --  Sets the style field of a Pango.Font.Pango_Font_Description. The
   --  Pango.Enums.Style enumeration describes whether the font is slanted and
   --  the manner in which it is slanted; it can be either PANGO_STYLE_NORMAL,
   --  PANGO_STYLE_ITALIC, or PANGO_STYLE_OBLIQUE. Most fonts will either have
   --  a italic style or an oblique style, but not both, and font matching in
   --  Pango will match italic specifications with oblique fonts and vice-versa
   --  if an exact match is not found.
   --  "style": the style for the font description

   function Get_Variant
      (Self : Pango_Font_Description) return Pango.Enums.Variant;
   pragma Import (C, Get_Variant, "pango_font_description_get_variant");
   --  Gets the variant field of a Pango.Font.Pango_Font_Description. See
   --  Pango.Font.Set_Variant.

   procedure Set_Variant
      (Self    : Pango_Font_Description;
       Variant : Pango.Enums.Variant);
   pragma Import (C, Set_Variant, "pango_font_description_set_variant");
   --  Sets the variant field of a font description. The Pango.Enums.Variant
   --  can either be Pango.Enums.Pango_Variant_Normal or
   --  Pango.Enums.Pango_Variant_Small_Caps.
   --  "variant": the variant type for the font description.

   function Get_Variations
      (Self : Pango_Font_Description) return UTF8_String;
   --  Gets the variations field of a font description. See
   --  Pango.Font.Set_Variations.
   --  Since: gtk+ 1.42

   procedure Set_Variations
      (Self       : Pango_Font_Description;
       Variations : UTF8_String);
   --  Sets the variations field of a font description. OpenType font
   --  variations allow to select a font instance by specifying values for a
   --  number of axes, such as width or weight.
   --  The format of the variations string is AXIS1=VALUE,AXIS2=VALUE..., with
   --  each AXIS a 4 character tag that identifies a font axis, and each VALUE
   --  a floating point number. Unknown axes are ignored, and values are
   --  clamped to their allowed range.
   --  Pango does not currently have a way to find supported axes of a font.
   --  Both harfbuzz or freetype have API for this.
   --  Since: gtk+ 1.42
   --  "variations": a string representing the variations

   function Get_Weight
      (Self : Pango_Font_Description) return Pango.Enums.Weight;
   pragma Import (C, Get_Weight, "pango_font_description_get_weight");
   --  Gets the weight field of a font description. See Pango.Font.Set_Weight.

   procedure Set_Weight
      (Self   : Pango_Font_Description;
       Weight : Pango.Enums.Weight);
   pragma Import (C, Set_Weight, "pango_font_description_set_weight");
   --  Sets the weight field of a font description. The weight field specifies
   --  how bold or light the font should be. In addition to the values of the
   --  Pango.Enums.Weight enumeration, other intermediate numeric values are
   --  possible.
   --  "weight": the weight for the font description.

   function Hash (Self : Pango_Font_Description) return Guint;
   pragma Import (C, Hash, "pango_font_description_hash");
   --  Computes a hash of a Pango.Font.Pango_Font_Description structure
   --  suitable to be used, for example, as an argument to g_hash_table_new.
   --  The hash value is independent of Desc->mask.

   procedure Merge
      (Self             : Pango_Font_Description;
       Desc_To_Merge    : Pango_Font_Description;
       Replace_Existing : Boolean);
   --  Merges the fields that are set in Desc_To_Merge into the fields in
   --  Desc. If Replace_Existing is False, only fields in Desc that are not
   --  already set are affected. If True, then fields that are already set will
   --  be replaced as well.
   --  If Desc_To_Merge is null, this function performs nothing.
   --  "desc_to_merge": the Pango.Font.Pango_Font_Description to merge from,
   --  or null
   --  "replace_existing": if True, replace fields in Desc with the
   --  corresponding values from Desc_To_Merge, even if they are already exist.

   procedure Merge_Static
      (Self             : Pango_Font_Description;
       Desc_To_Merge    : Pango_Font_Description;
       Replace_Existing : Boolean);
   --  Like Pango.Font.Merge, but only a shallow copy is made of the family
   --  name and other allocated fields. Desc can only be used until
   --  Desc_To_Merge is modified or freed. This is meant to be used when the
   --  merged font description is only needed temporarily.
   --  "desc_to_merge": the Pango.Font.Pango_Font_Description to merge from
   --  "replace_existing": if True, replace fields in Desc with the
   --  corresponding values from Desc_To_Merge, even if they are already exist.

   procedure Set_Absolute_Size
      (Self : Pango_Font_Description;
       Size : Gdouble);
   pragma Import (C, Set_Absolute_Size, "pango_font_description_set_absolute_size");
   --  Sets the size field of a font description, in device units. This is
   --  mutually exclusive with Pango.Font.Set_Size which sets the font size in
   --  points.
   --  Since: gtk+ 1.8
   --  "size": the new size, in Pango units. There are PANGO_SCALE Pango units
   --  in one device unit. For an output backend where a device unit is a
   --  pixel, a Size value of 10 * PANGO_SCALE gives a 10 pixel font.

   procedure Set_Family_Static
      (Self   : Pango_Font_Description;
       Family : UTF8_String);
   --  Like Pango.Font.Set_Family, except that no copy of Family is made. The
   --  caller must make sure that the string passed in stays around until Desc
   --  has been freed or the name is set again. This function can be used if
   --  Family is a static string such as a C string literal, or if Desc is only
   --  needed temporarily.
   --  "family": a string representing the family name.

   procedure Set_Variations_Static
      (Self       : Pango_Font_Description;
       Variations : UTF8_String);
   --  Like Pango.Font.Set_Variations, except that no copy of Variations is
   --  made. The caller must make sure that the string passed in stays around
   --  until Desc has been freed or the name is set again. This function can be
   --  used if Variations is a static string such as a C string literal, or if
   --  Desc is only needed temporarily.
   --  Since: gtk+ 1.42
   --  "variations": a string representing the variations

   function To_Filename (Self : Pango_Font_Description) return UTF8_String;
   --  Creates a filename representation of a font description. The filename
   --  is identical to the result from calling Pango.Font.To_String, but with
   --  underscores instead of characters that are untypical in filenames, and
   --  in lower case only.

   function To_String (Self : Pango_Font_Description) return UTF8_String;
   --  Creates a string representation of a font description. See
   --  Pango.Font.From_String for a description of the format of the string
   --  representation. The family list in the string description will only have
   --  a terminating comma if the last word of the list is a valid style
   --  option.

   procedure Unset_Fields
      (Self     : Pango_Font_Description;
       To_Unset : Pango.Enums.Font_Mask);
   pragma Import (C, Unset_Fields, "pango_font_description_unset_fields");
   --  Unsets some of the fields in a Pango.Font.Pango_Font_Description. The
   --  unset fields will get back to their default values.
   --  "to_unset": bitmask of fields in the Desc to unset.

   ----------------------
   -- GtkAda additions --
   ----------------------

   function To_Font_Description
     (Family_Name : String := "";
      Style       : Pango.Enums.Style := Pango.Enums.Pango_Style_Normal;
      Variant     : Pango.Enums.Variant := Pango.Enums.Pango_Variant_Normal;
      Weight      : Pango.Enums.Weight := Pango.Enums.Pango_Weight_Normal;
      Stretch     : Pango.Enums.Stretch := Pango.Enums.Pango_Stretch_Normal;
      Size        : Gint := 0) return Pango_Font_Description;
   --  Create a new font decription from the given parameters.

   function Get_Style_As_String
     (Self : Pango_Font_Description) return String;
   -- Return the font's style as a string (e.g: 'Oblique').

   function Get_Weight_As_String
     (Self : Pango_Font_Description) return String;
   -- Return the font's style as a string (e.g: 'Oblique').

   function To_Address
     (F : Pango_Font_Description; Add : System.Address) return System.Address;
   package Desc_Properties is new Generic_Internal_Boxed_Property
     (Pango_Font_Description, Get_Type, To_Address);
   type Property_Font_Description is new Desc_Properties.Property;

   procedure Free (Desc : in out Pango_Font_Description);
   --  Deallocate the given font description.

   ---------------
   -- Functions --
   ---------------

   function From_String (Str : UTF8_String) return Pango_Font_Description;
   --  Creates a new font description from a string representation in the form
   --  "\[FAMILY-LIST] \[STYLE-OPTIONS] \[SIZE] \[VARIATIONS]",
   --  where FAMILY-LIST is a comma-separated list of families optionally
   --  terminated by a comma, STYLE_OPTIONS is a whitespace-separated list of
   --  words where each word describes one of style, variant, weight, stretch,
   --  or gravity, and SIZE is a decimal number (size in points) or optionally
   --  followed by the unit modifier "px" for absolute size. VARIATIONS is a
   --  comma-separated list of font variation specifications of the form
   --  "\Axis=value" (the = sign is optional).
   --  The following words are understood as styles: "Normal", "Roman",
   --  "Oblique", "Italic".
   --  The following words are understood as variants: "Small-Caps".
   --  The following words are understood as weights: "Thin", "Ultra-Light",
   --  "Extra-Light", "Light", "Semi-Light", "Demi-Light", "Book", "Regular",
   --  "Medium", "Semi-Bold", "Demi-Bold", "Bold", "Ultra-Bold", "Extra-Bold",
   --  "Heavy", "Black", "Ultra-Black", "Extra-Black".
   --  The following words are understood as stretch values:
   --  "Ultra-Condensed", "Extra-Condensed", "Condensed", "Semi-Condensed",
   --  "Semi-Expanded", "Expanded", "Extra-Expanded", "Ultra-Expanded".
   --  The following words are understood as gravity values: "Not-Rotated",
   --  "South", "Upside-Down", "North", "Rotated-Left", "East",
   --  "Rotated-Right", "West".
   --  Any one of the options may be absent. If FAMILY-LIST is absent, then
   --  the family_name field of the resulting font description will be
   --  initialized to null. If STYLE-OPTIONS is missing, then all style options
   --  will be set to the default values. If SIZE is missing, the size in the
   --  resulting font description will be set to 0.
   --  A typical example:
   --  "Cantarell Italic Light 15 \Wght=200"
   --  "str": string representation of a font description.

end Pango.Font;
