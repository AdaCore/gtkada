-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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
with Pango.Enums;

with Interfaces.C.Strings;

package Pango.Font is

   type Pango_Font_Description_Record is record
      Family_Name : Interfaces.C.Strings.chars_ptr;
      --  It is highly recommended to access to this field via the
      --  Get/Set_Family_Name routines (see below).

      Style       : Pango.Enums.Style;
      Variant     : Pango.Enums.Variant;
      Weight      : Pango.Enums.Weight;
      Stretch     : Pango.Enums.Stretch;
      Size        : Gint;
      --  Strictly, should be Interfaces.C.int, but Gint is simpler to use.
   end record;
   pragma Convention (C, Pango_Font_Description_Record);

   type Pango_Font_Description is access all Pango_Font_Description_Record;
   pragma Convention (C, Pango_Font_Description);

   function Copy (Desc : Pango_Font_Description) return Pango_Font_Description;
   --  Return a newly allocated font description.
   --  This Pango_Font_Description needs to be free'ed after use.

   function Equal
     (Desc1 : Pango_Font_Description;
      Desc2 : Pango_Font_Description) return Boolean;
   --  Return True if the two font descriptions are identical.
   --  Note that two font description may result in identical fonts being
   --  loaded, but still compare False.

   procedure Free (Desc : in out Pango_Font_Description);
   --  Deallocate the given font description.

   function From_String (Str : String) return Pango_Font_Description;
   --  Create a new font description from the given string representation
   --  of the given form: "[FAMILY-LIST] [STYLE-OPTIONS] [SIZE]". Any one
   --  of the options may be omitted.
   --    - FAMILY-LIST is a comma separated list of font families optionally
   --      terminated by a comma. If absent, the font family of the font
   --      that will be used is unspecified.
   --    - STYLE_OPTIONS is a whitespace separated list of words where each
   --      word describes either style, variant, weight, or stretch. Any
   --      unspecified style option is defaulted to "Normal", which
   --      respectively corresponds to Pango_Style_Normal, Pango_Weight_Normal,
   --      Pango_Variant_Normal, and Pango_Stretch_Normal.
   --    - SIZE is a decimal number describing the size of the font in points.
   --      If unspecified, a size of 0 will be used.

   function To_String (Desc : Pango_Font_Description) return String;
   --  Create a string representation of a font description. The format
   --  of the string produced follows the syntax used by From_String.
   --  The family-list in the string description will have a terminating
   --  comma only if the last word of the list is a valid style option.

   function To_Filename (Desc : Pango_Font_Description) return String;
   --  Create a filename representation of a font description. The filename
   --  is identical to the result from calling To_String, but with underscores
   --  instead of characters that are untypical in filenames, and in lower
   --  case only.

   function Get_Family_Name (Desc : Pango_Font_Description) return String;
   --  Returns the Family_Name of the given Pango_Font_Description. This is
   --  a convenience function that converts the chars_ptr into a String.

   procedure Set_Family_Name (Desc : Pango_Font_Description; Name : String);
   --  Sets the Family_Name of the given Pango_Font_Description. This is
   --  a convenience functions that transforms the given string into
   --  the chars_ptr stored in the font description record. It also takes
   --  care of the memory management.

private

   pragma Import (C, Copy, "pango_font_description_copy");

end Pango.Font;
