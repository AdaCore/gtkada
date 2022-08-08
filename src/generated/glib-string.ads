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
--  The GString struct contains the public fields of a GString.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gtkada.Types; use Gtkada.Types;

package Glib.String is

   type Gstring is record
      Str : Gtkada.Types.Chars_Ptr;
      Len : Gsize;
      Allocated_Len : Gsize;
   end record;
   pragma Convention (C, Gstring);

   function From_Object_Free (B : access Gstring) return Gstring;
   pragma Inline (From_Object_Free);
   --  The GString struct contains the public fields of a GString.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_gstring_get_type");

   -------------
   -- Methods --
   -------------

   function Append (Self : Gstring; Val : UTF8_String) return Gstring;
   --  Adds a string onto the end of a Glib.String.Gstring, expanding it if
   --  necessary.
   --  "val": the string to append onto the end of String

   function Append_C (Self : Gstring; C : Gchar) return Gstring;
   pragma Import (C, Append_C, "g_string_append_c");
   --  Adds a byte onto the end of a Glib.String.Gstring, expanding it if
   --  necessary.
   --  "c": the byte to append onto the end of String

   function Append_Len
      (Self : Gstring;
       Val  : UTF8_String;
       Len  : Gssize) return Gstring;
   --  Appends Len bytes of Val to String. Because Len is provided, Val may
   --  contain embedded nuls and need not be nul-terminated.
   --  Since this function does not stop at nul bytes, it is the caller's
   --  responsibility to ensure that Val has at least Len addressable bytes.
   --  "val": bytes to append
   --  "len": number of bytes of Val to use

   function Append_Unichar (Self : Gstring; Wc : Gunichar) return Gstring;
   pragma Import (C, Append_Unichar, "g_string_append_unichar");
   --  Converts a Unicode character into UTF-8, and appends it to the string.
   --  "wc": a Unicode character

   function Append_Uri_Escaped
      (Self                   : Gstring;
       Unescaped              : UTF8_String;
       Reserved_Chars_Allowed : UTF8_String;
       Allow_Utf8             : Boolean) return Gstring;
   --  Appends Unescaped to String, escaped any characters that are reserved
   --  in URIs using URI-style escape sequences.
   --  Since: gtk+ 2.16
   --  "unescaped": a string
   --  "reserved_chars_allowed": a string of reserved characters allowed to be
   --  used, or null
   --  "allow_utf8": set True if the escaped string may include UTF8
   --  characters

   function Ascii_Down (Self : Gstring) return Gstring;
   pragma Import (C, Ascii_Down, "g_string_ascii_down");
   --  Converts all uppercase ASCII letters to lowercase ASCII letters.

   function Ascii_Up (Self : Gstring) return Gstring;
   pragma Import (C, Ascii_Up, "g_string_ascii_up");
   --  Converts all lowercase ASCII letters to uppercase ASCII letters.

   function Assign (Self : Gstring; Rval : UTF8_String) return Gstring;
   --  Copies the bytes from a string into a Glib.String.Gstring, destroying
   --  any previous contents. It is rather like the standard strcpy function,
   --  except that you do not have to worry about having enough space to copy
   --  the string.
   --  "rval": the string to copy into String

   function Down (Self : Gstring) return Gstring;
   pragma Import (C, Down, "g_string_down");
   pragma Obsolescent (Down);
   --  Converts a Glib.String.Gstring to lowercase.
   --  Deprecated since 2.2, 1

   function Equal (Self : Gstring; V2 : Gstring) return Boolean;
   --  Compares two strings for equality, returning True if they are equal.
   --  For use with GHash_Table.
   --  "v2": another Glib.String.Gstring

   function Erase
      (Self : Gstring;
       Pos  : Gssize;
       Len  : Gssize) return Gstring;
   pragma Import (C, Erase, "g_string_erase");
   --  Removes Len bytes from a Glib.String.Gstring, starting at position Pos.
   --  The rest of the Glib.String.Gstring is shifted down to fill the gap.
   --  "pos": the position of the content to remove
   --  "len": the number of bytes to remove, or -1 to remove all following
   --  bytes

   function Free (Self : Gstring; Free_Segment : Boolean) return UTF8_String;
   --  Frees the memory allocated for the Glib.String.Gstring. If Free_Segment
   --  is True it also frees the character data. If it's False, the caller
   --  gains ownership of the buffer and must free it after use with g_free.
   --  "free_segment": if True, the actual character data is freed as well

   function Hash (Self : Gstring) return Guint;
   pragma Import (C, Hash, "g_string_hash");
   --  Creates a hash code for Str; for use with GHash_Table.

   function Insert
      (Self : Gstring;
       Pos  : Gssize;
       Val  : UTF8_String) return Gstring;
   --  Inserts a copy of a string into a Glib.String.Gstring, expanding it if
   --  necessary.
   --  "pos": the position to insert the copy of the string
   --  "val": the string to insert

   function Insert_C
      (Self : Gstring;
       Pos  : Gssize;
       C    : Gchar) return Gstring;
   pragma Import (C, Insert_C, "g_string_insert_c");
   --  Inserts a byte into a Glib.String.Gstring, expanding it if necessary.
   --  "pos": the position to insert the byte
   --  "c": the byte to insert

   function Insert_Len
      (Self : Gstring;
       Pos  : Gssize;
       Val  : UTF8_String;
       Len  : Gssize) return Gstring;
   --  Inserts Len bytes of Val into String at Pos. Because Len is provided,
   --  Val may contain embedded nuls and need not be nul-terminated. If Pos is
   --  -1, bytes are inserted at the end of the string.
   --  Since this function does not stop at nul bytes, it is the caller's
   --  responsibility to ensure that Val has at least Len addressable bytes.
   --  "pos": position in String where insertion should happen, or -1 for at
   --  the end
   --  "val": bytes to insert
   --  "len": number of bytes of Val to insert

   function Insert_Unichar
      (Self : Gstring;
       Pos  : Gssize;
       Wc   : Gunichar) return Gstring;
   pragma Import (C, Insert_Unichar, "g_string_insert_unichar");
   --  Converts a Unicode character into UTF-8, and insert it into the string
   --  at the given position.
   --  "pos": the position at which to insert character, or -1 to append at
   --  the end of the string
   --  "wc": a Unicode character

   function Overwrite
      (Self : Gstring;
       Pos  : Gsize;
       Val  : UTF8_String) return Gstring;
   --  Overwrites part of a string, lengthening it if necessary.
   --  Since: gtk+ 2.14
   --  "pos": the position at which to start overwriting
   --  "val": the string that will overwrite the String starting at Pos

   function Overwrite_Len
      (Self : Gstring;
       Pos  : Gsize;
       Val  : UTF8_String;
       Len  : Gssize) return Gstring;
   --  Overwrites part of a string, lengthening it if necessary. This function
   --  will work with embedded nuls.
   --  Since: gtk+ 2.14
   --  "pos": the position at which to start overwriting
   --  "val": the string that will overwrite the String starting at Pos
   --  "len": the number of bytes to write from Val

   function Prepend (Self : Gstring; Val : UTF8_String) return Gstring;
   --  Adds a string on to the start of a Glib.String.Gstring, expanding it if
   --  necessary.
   --  "val": the string to prepend on the start of String

   function Prepend_C (Self : Gstring; C : Gchar) return Gstring;
   pragma Import (C, Prepend_C, "g_string_prepend_c");
   --  Adds a byte onto the start of a Glib.String.Gstring, expanding it if
   --  necessary.
   --  "c": the byte to prepend on the start of the Glib.String.Gstring

   function Prepend_Len
      (Self : Gstring;
       Val  : UTF8_String;
       Len  : Gssize) return Gstring;
   --  Prepends Len bytes of Val to String. Because Len is provided, Val may
   --  contain embedded nuls and need not be nul-terminated.
   --  Since this function does not stop at nul bytes, it is the caller's
   --  responsibility to ensure that Val has at least Len addressable bytes.
   --  "val": bytes to prepend
   --  "len": number of bytes in Val to prepend

   function Prepend_Unichar (Self : Gstring; Wc : Gunichar) return Gstring;
   pragma Import (C, Prepend_Unichar, "g_string_prepend_unichar");
   --  Converts a Unicode character into UTF-8, and prepends it to the string.
   --  "wc": a Unicode character

   function Set_Size (Self : Gstring; Len : Gsize) return Gstring;
   pragma Import (C, Set_Size, "g_string_set_size");
   --  Sets the length of a Glib.String.Gstring. If the length is less than
   --  the current length, the string will be truncated. If the length is
   --  greater than the current length, the contents of the newly added area
   --  are undefined. (However, as always, string->str[string->len] will be a
   --  nul byte.)
   --  "len": the new length

   function Truncate (Self : Gstring; Len : Gsize) return Gstring;
   pragma Import (C, Truncate, "g_string_truncate");
   --  Cuts off the end of the GString, leaving the first Len bytes.
   --  "len": the new size of String

   function Up (Self : Gstring) return Gstring;
   pragma Import (C, Up, "g_string_up");
   pragma Obsolescent (Up);
   --  Converts a Glib.String.Gstring to uppercase.
   --  Deprecated since 2.2, 1

end Glib.String;
