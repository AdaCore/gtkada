-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2003 ACT-Europe                   --
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
--
--  This package provides functions for handling of unicode characters and
--  utf8 strings. See also Glib.Convert.
--
--  </description>
--  <c_version>2.2.1</c_version>

package Glib.Unicode is
   pragma Preelaborate;

   procedure UTF8_Validate
     (Str         : UTF8_String;
      Valid       : out Boolean;
      Invalid_Pos : out Natural);
   --  Validate a UTF8 string.
   --  Set Valid to True if valid, set Invalid_Pos to first invalid char.

   function Is_Space (Char : Gunichar) return Boolean;
   --  True if Char is a space character

   function Is_Alnum (Char : Gunichar) return Boolean;
   --  True if Char is an alphabetical or numerical character

   function Is_Alpha (Char : Gunichar) return Boolean;
   --  True if Char is an alphabetical character

   function Is_Digit (Char : Gunichar) return Boolean;
   --  True if Char is a digit

   function Is_Lower (Char : Gunichar) return Boolean;
   --  True if Char is a lower-case character

   function Is_Upper (Char : Gunichar) return Boolean;
   --  True if Char is an upper-case character

   function Is_Punct (Char : Gunichar) return Boolean;
   --  True if Char is a punctuation character

   function To_Lower (Char : Gunichar) return Gunichar;
   --  Convert Char to lower cases

   function To_Upper (Char : Gunichar) return Gunichar;
   --  Convert Char to upper cases

private
   pragma Import (C, To_Upper, "g_unichar_toupper");
   pragma Import (C, To_Lower, "g_unichar_tolower");
end Glib.Unicode;
