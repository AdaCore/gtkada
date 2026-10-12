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

--  The Pango.Color.Pango_Color structure is used to represent a color in an
--  uncalibrated RGB color-space.

pragma Warnings (Off, "*is already use-visible*");
with Glib; use Glib;

package Pango.Color is

   type Pango_Color is record
      Red : Guint16;
      Green : Guint16;
      Blue : Guint16;
   end record;
   pragma Convention (C, Pango_Color);
   --  The Pango.Color.Pango_Color structure is used to represent a color in
   --  an uncalibrated RGB color-space.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "pango_color_get_type");

   -------------
   -- Methods --
   -------------

   function Parse
      (Self : access Pango_Color;
       Spec : UTF8_String) return Boolean;
   --  Fill in the fields of a color from a string specification. The string
   --  can either one of a large set of standard names. (Taken from the CSS
   --  <ulink
   --  url="http://dev.w3.org/csswg/css-color/named-colors">specification</ulink>),
   --  or it can be a hexadecimal value in the form '&num;rgb' '&num;rrggbb'
   --  '&num;rrrgggbbb' or '&num;rrrrggggbbbb' where 'r', 'g' and 'b' are hex
   --  digits of the red, green, and blue components of the color,
   --  respectively. (White in the four forms is '&num;fff' '&num;ffffff'
   --  '&num;fffffffff' and '&num;ffffffffffff')
   --  @param Spec a string specifying the new color
   --  @return True if parsing of the specifier succeeded, otherwise false.

   function Parse_With_Alpha
      (Self  : access Pango_Color;
       Alpha : access Guint16 := null;
       Spec  : UTF8_String) return Boolean;
   --  Fill in the fields of a color from a string specification. The string
   --  can either one of a large set of standard names. (Taken from the CSS
   --  <ulink
   --  url="http://dev.w3.org/csswg/css-color/named-colors">specification</ulink>),
   --  or it can be a hexadecimal value in the form '&num;rgb' '&num;rrggbb'
   --  '&num;rrrgggbbb' or '&num;rrrrggggbbbb' where 'r', 'g' and 'b' are hex
   --  digits of the red, green, and blue components of the color,
   --  respectively. (White in the four forms is '&num;fff' '&num;ffffff'
   --  '&num;fffffffff' and '&num;ffffffffffff')
   --  Additionally, parse strings of the form '&num;rgba', '&num;rrggbbaa',
   --  '&num;rrrrggggbbbbaaaa', if Alpha is not null, and set Alpha to the
   --  value specified by the hex digits for 'a'. If no alpha component is
   --  found in Spec, Alpha is set to 0xffff (for a solid color).
   --  Since: gtk+ 1.46
   --  @param Alpha return location for alpha, or null
   --  @param Spec a string specifying the new color
   --  @return True if parsing of the specifier succeeded, otherwise false.

   function To_String (Self : Pango_Color) return UTF8_String;
   --  Returns a textual specification of Color in the hexadecimal form
   --  `&num;rrrrggggbbbb`, where `r`, `g` and `b` are hex digits representing
   --  the red, green, and blue components respectively.
   --  Since: gtk+ 1.16
   --  @return a newly-allocated text string that must be freed with g_free.

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Pango_Color) return Pango_Color;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

end Pango.Color;
