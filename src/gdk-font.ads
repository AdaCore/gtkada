-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

--  <description>
--  This is the base package for handling fonts.
--  GtkAda knows about bitmap and vectorial fonts, and can draw both.
--  The list of fonts available to you depends on what is installed on
--  your system.
--
--  The name of the font is indicated in the standard X11 fashion, namely:
--  (example extracted from the Xlib manual):
--
--  -adobe-courier-bold-o-normal--10-100-75-75-m-60-iso8859-1
--  where:
--     adobe     : foundry
--     courier   : font family
--     bold      : weight (e.g. bold, medium)
--     o         : slant (e.g. roman, italic, oblique)
--     normal    : set width (e.g. normal, condensed, narrow, double)
--     10        : pixels
--     100       : points (in tenths of a point)
--     75        : horizontal resolution in dpi
--     75        : vertical resolution in dpi
--     m         : spacing (e.g. monospace or proportional)
--     60        : average width (in tenths of a pixel)
--     iso8859-1 : character set
--
--  Any of the fields can have a '*' instead, so that the system will
--  automatically find a font that matches the rest of the string, and won't
--  care about that specific field.
--
--  But the easiest way to select a font is by using some external programs,
--  for instance xfontsel, xlsfont, gfontsel, or even the font selection
--  dialog example in the testgtk/ directory of the GtkAda distribution.
--
--  See also Gtk.Extra.PsFont for a package that processes postscript fonts,
--  with their more usual names, and can easily convert them to standard
--  Gdk_Font structures.
--
--  Some of the functions below should be used only for wide-character strings.
--  This is needed for languages with more than 256 characters.
--
--  Wide character values between 0 and 127 are always identical in meaning to
--  the ASCII character codes.
--  An alternative to wide characters is multi-byte characters, which extend
--  normal char strings to cope with larger character sets. As the name
--  suggests, multi-byte characters use a different number of bytes to store
--  different character codes. For example codes 0-127 (i.e. the ASCII codes)
--  often use just one byte of memory, while other codes may use 2, 3 or even
--  4 bytes. Multi-byte characters have the advantage that they can often be
--  used in an application with little change, since strings are still
--  represented as arrays of char values. However multi-byte strings are much
--  easier to manipulate since the character are all of the same size.
--
--  </description>
--  <c_version>1.2.7</c_version>
--  <screenshot>font</screenshot>

with Glib; use Glib;
with Gdk.Types;

package Gdk.Font is

   type Gdk_Font is new Root_Type with private;
   Null_Font : constant Gdk_Font;

   procedure Load (Font      :    out Gdk_Font;
                   Font_Name : in     String);
   --  Load a new font, given its name.
   --  This is the first step before using a font.
   --  The font is first looked up in the cache, and if it was already
   --  loaded, it is not reloaded again. Thus, it does not harm to call
   --  this function multiple times with the same Font_Name.
   --  Null_Font is returned if the font could not be loaded.

   procedure Fontset_Load (Font         :   out Gdk_Font;
                           Fontset_Name : in    String);
   --  Load a new font set.
   --  Fontset_Name is a comma-separated list of fonts that will be loaded
   --  as part of the fontset.

   procedure Ref (Font : in Gdk_Font);
   --  Increment the reference counter for the font.
   --  You should not make any assumption of the initial value of the fonts
   --  returned by Load or Fontset_Load, since these can be extracted from a
   --  cache.

   procedure Unref (Font : in out Gdk_Font);
   --  Decrement the reference counter for the font.
   --  When this counter reaches 0, the font is deleted from memory.

   function Id (Font : in Gdk_Font) return Gint;
   --  Return the X font id for the font.
   --  This Id will only be needed if you want to call directly X11 functions,
   --  you won't need it with GtkAda.

   function "=" (Fonta, Fontb : in Gdk_Font) return Boolean;
   --  Compare two fonts or two fontsets for equality.
   --  Two fonts are equal if they have the same font Id.
   --  Two fontsets are equal if the name given to Fontset_Load was the same.

   function String_Width (Font : in Gdk_Font;
                          Str  : in String)
                         return Gint;
   --  Return the width in pixels that Str will occupy if drawn with Font.
   --  The value returned is the distance between the origin of the text and
   --  the position at which the next string should be drawn.

   function Text_Width (Font : in Gdk_Font;
                        Text : in String)
                       return Gint;
   --  This is the same function as String_Width.
   --  In C, this function is intended to measure only the width of a part
   --  of the string, but you can simply pass it a substring in Ada.

   function Text_Width (Font : in Gdk_Font;
                        Text : in Gdk.Types.Gdk_WString)
                       return Gint;
   --  Return the width in pixels that Text will occupy on the screen.
   --  This function should be used with strings that contain Unicode
   --  characters

   function Char_Width (Font : in Gdk_Font;
                        Char : in Character)
                       return Gint;
   --  Return the width in pixels occupied by a single character on the screen.
   --  The value returned is the distance between Char's origin on the screen
   --  and the origin of the next character in the string.

   function Char_Width (Font : in Gdk_Font;
                        Char : in Gdk.Types.Gdk_WChar)
                       return Gint;
   --  Return the width in pixels occupied by a single wide-character.

   function String_Measure (Font : in Gdk_Font;
                            Str  : in String)
                           return Gint;
   --  Determine the distance from the origin to the rightmost portion of Str.
   --  This is not the correct value for determining the origin of the next
   --  portion when drawing text in multiple pieces.
   --  See String_Width instead.

   function Text_Measure (Font : in Gdk_Font;
                          Text : in String)
                         return Gint;
   --  Same function a String_Measure.
   --  In C, this function is intended to measure only the width of a part of
   --  the string, but you can simply pass it a substring in Ada.
   --  This is also called the right bearing of the string.

   function Char_Measure (Font : in Gdk_Font;
                          Char : in Character)
                         return Gint;
   --  Return the width in pixels of Char.
   --  As opposed to Char_Width, the value returned is not the distance at
   --  which the next character should be drawn.
   --  This is also called the right bearing of the character.

   procedure String_Extents (Font     : in     Gdk.Font.Gdk_Font;
                             Str      : in     String;
                             Lbearing :    out Gint;
                             Rbearing :    out Gint;
                             Width    :    out Gint;
                             Ascent   :    out Gint;
                             Descent  :    out Gint);
   --  Return the metrics for a given text.
   --  See the picture for more explanations on all the fields.
   --  Lbearing : Origin to left edge of character.
   --  Rbearing : Origin to right edge of character.
   --  Width    : Advance to next character's origin.
   --  Ascent   : Baseline to top edge of character.
   --  Descent  : Baseline to bottom edge of character.

   procedure Text_Extents (Font        : in     Gdk_Font;
                           Text        : in     String;
                           Lbearing    :    out Gint;
                           Rbearing    :    out Gint;
                           Width       :    out Gint;
                           Ascent      :    out Gint;
                           Descent     :    out Gint);
   --  Return all the metrics for a given text.
   --  See the picture for more explanations on all the fields.
   --  in C, this function would be used for part of a string, which you can
   --  simulate in Ada with a substring.

   procedure Text_Extents (Font        : in     Gdk_Font;
                           Text        : in     Gdk.Types.Gdk_WString;
                           Lbearing    :    out Gint;
                           Rbearing    :    out Gint;
                           Width       :    out Gint;
                           Ascent      :    out Gint;
                           Descent     :    out Gint);
   --  Return all the metrics for a given wide-character string.
   --  See the picture for more explanations on the returned values.

   function String_Height (Font : in Gdk_Font;
                           Str  : in String)
                          return Gint;
   --  Return the height in pixels of the string.
   --  This is the total height, and you can not easily tell how this height
   --  is split around the baseline.

   function Text_Height (Font : in Gdk_Font;
                         Str  : in String)
                        return Gint;
   --  Same as String_Height.
   --  In C, this function is intended to measure only the width of a part of
   --  the string, but you can simply pass it a substring in Ada.
   --  This is also called the right bearing of the string.

   function Char_Height (Font : in Gdk_Font;
                         Char : in Character)
                        return Gint;
   --  Return the total height in pixels of a single character.


private
   type Gdk_Font is new Root_Type with null record;
   Null_Font : constant Gdk_Font := (Ptr => System.Null_Address);
end Gdk.Font;
