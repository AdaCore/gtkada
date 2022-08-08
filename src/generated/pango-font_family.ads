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
--  The Pango.Font_Family.Pango_Font_Family structure is used to represent a
--  family of related font faces. The faces in a family share a common design,
--  but differ in slant, weight, width and other aspects.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Pango.Font_Face; use Pango.Font_Face;

package Pango.Font_Family is

   type Pango_Font_Family_Record is new GObject_Record with null record;
   type Pango_Font_Family is access all Pango_Font_Family_Record'Class;

   type Pango_Font_Family_Array is array (Natural range <>) of Pango_Font_Family;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "pango_font_family_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Name
      (Self : not null access Pango_Font_Family_Record) return UTF8_String;
   --  Gets the name of the family. The name is unique among all fonts for the
   --  font backend and can be used in a Pango.Font.Pango_Font_Description to
   --  specify that a face from this family is desired.

   function Is_Monospace
      (Self : not null access Pango_Font_Family_Record) return Boolean;
   --  A monospace font is a font designed for text display where the the
   --  characters form a regular grid. For Western languages this would mean
   --  that the advance width of all characters are the same, but this
   --  categorization also includes Asian fonts which include double-width
   --  characters: characters that occupy two grid cells. g_unichar_iswide
   --  returns a result that indicates whether a character is typically
   --  double-width in a monospace font.
   --  The best way to find out the grid-cell size is to call
   --  Pango.Font_Metrics.Get_Approximate_Digit_Width, since the results of
   --  Pango.Font_Metrics.Get_Approximate_Char_Width may be affected by
   --  double-width characters.
   --  Since: gtk+ 1.4

   function Is_Variable
      (Self : not null access Pango_Font_Family_Record) return Boolean;
   --  A variable font is a font which has axes that can be modified to
   --  produce different faces.
   --  Since: gtk+ 1.44

   function List_Faces
      (Self : not null access Pango_Font_Family_Record)
       return Pango_Font_Face_Array;
   --  Lists the different font faces that make up Family. The faces in a
   --  family share a common design, but differ in slant, weight, width and
   --  other aspects.

end Pango.Font_Family;
