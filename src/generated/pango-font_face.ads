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
--  The Pango.Font_Face.Pango_Font_Face structure is used to represent a group
--  of fonts with the same family, slant, weight, width, but varying sizes.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Pango.Font;  use Pango.Font;

package Pango.Font_Face is

   type Pango_Font_Face_Record is new GObject_Record with null record;
   type Pango_Font_Face is access all Pango_Font_Face_Record'Class;

   type Pango_Font_Face_Array is array (Natural range <>) of Pango_Font_Face;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "pango_font_face_get_type");

   -------------
   -- Methods --
   -------------

   function Describe
      (Self : not null access Pango_Font_Face_Record)
       return Pango.Font.Pango_Font_Description;
   --  Returns the family, style, variant, weight and stretch of a
   --  Pango.Font_Face.Pango_Font_Face. The size field of the resulting font
   --  description will be unset.

   function Get_Face_Name
      (Self : not null access Pango_Font_Face_Record) return UTF8_String;
   --  Gets a name representing the style of this face among the different
   --  faces in the Pango.Font_Family.Pango_Font_Family for the face. This name
   --  is unique among all faces in the family and is suitable for displaying
   --  to users.

   function Is_Synthesized
      (Self : not null access Pango_Font_Face_Record) return Boolean;
   --  Returns whether a Pango.Font_Face.Pango_Font_Face is synthesized by the
   --  underlying font rendering engine from another face, perhaps by shearing,
   --  emboldening, or lightening it.
   --  Since: gtk+ 1.18

   function List_Sizes
      (Self : not null access Pango_Font_Face_Record) return Gint_Array;
   --  List the available sizes for a font. This is only applicable to bitmap
   --  fonts. For scalable fonts, stores null at the location pointed to by
   --  Sizes and 0 at the location pointed to by N_Sizes. The sizes returned
   --  are in Pango units and are sorted in ascending order.
   --  Since: gtk+ 1.4

end Pango.Font_Face;
