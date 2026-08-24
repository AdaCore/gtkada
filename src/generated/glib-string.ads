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

--  A `GString` is an object that handles the memory management of a C string.
--
--  The emphasis of `GString` is on text, typically UTF-8. Crucially, the
--  "str" member of a `GString` is guaranteed to have a trailing nul character,
--  and it is therefore always safe to call functions such as `strchr` or
--  `strdup` on it.
--
--  However, a `GString` can also hold arbitrary binary data, because it has a
--  "len" member, which includes any possible embedded nul characters in the
--  data. Conceptually then, `GString` is like a `GByteArray` with the addition
--  of many convenience methods for text, and a guaranteed nul terminator.

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
   --  A `GString` is an object that handles the memory management of a C
   --  string.
   --
   --  The emphasis of `GString` is on text, typically UTF-8. Crucially, the
   --  "str" member of a `GString` is guaranteed to have a trailing nul
   --  character, and it is therefore always safe to call functions such as
   --  `strchr` or `strdup` on it.
   --
   --  However, a `GString` can also hold arbitrary binary data, because it
   --  has a "len" member, which includes any possible embedded nul characters
   --  in the data. Conceptually then, `GString` is like a `GByteArray` with
   --  the addition of many convenience methods for text, and a guaranteed nul
   --  terminator.

   ------------------
   -- Constructors --
   ------------------

   procedure G_New (Self : out Gstring; Init : UTF8_String := "");
   --  Creates a new Glib.String.Gstring, initialized with the given string.
   --  @param Init the initial text to copy into the string, or null to start
   --  with an empty string

   function Gstring_New (Init : UTF8_String := "") return Gstring;
   --  Creates a new Glib.String.Gstring, initialized with the given string.
   --  @param Init the initial text to copy into the string, or null to start
   --  with an empty string

   procedure G_New_Len
      (Self : out Gstring;
       Init : UTF8_String;
       Len  : Gssize);
   --  Creates a new Glib.String.Gstring with Len bytes of the Init buffer.
   --  Because a length is provided, Init need not be nul-terminated, and can
   --  contain embedded nul bytes.
   --  Since this function does not stop at nul bytes, it is the caller's
   --  responsibility to ensure that Init has at least Len addressable bytes.
   --  @param Init initial contents of the string
   --  @param Len length of Init to use

   function Gstring_New_Len
      (Init : UTF8_String;
       Len  : Gssize) return Gstring;
   --  Creates a new Glib.String.Gstring with Len bytes of the Init buffer.
   --  Because a length is provided, Init need not be nul-terminated, and can
   --  contain embedded nul bytes.
   --  Since this function does not stop at nul bytes, it is the caller's
   --  responsibility to ensure that Init has at least Len addressable bytes.
   --  @param Init initial contents of the string
   --  @param Len length of Init to use

   procedure G_New_Take (Self : out Gstring; Init : UTF8_String := "");
   --  Creates a new Glib.String.Gstring, initialized with the given string.
   --  After this call, Init belongs to the Glib.String.Gstring and may no
   --  longer be modified by the caller. The memory of Data has to be
   --  dynamically allocated and will eventually be freed with g_free.
   --  Since: gtk+ 2.78
   --  @param Init initial text used as the string. Ownership of the string is
   --  transferred to the Glib.String.Gstring. Passing null creates an empty
   --  string.

   function Gstring_New_Take (Init : UTF8_String := "") return Gstring;
   --  Creates a new Glib.String.Gstring, initialized with the given string.
   --  After this call, Init belongs to the Glib.String.Gstring and may no
   --  longer be modified by the caller. The memory of Data has to be
   --  dynamically allocated and will eventually be freed with g_free.
   --  Since: gtk+ 2.78
   --  @param Init initial text used as the string. Ownership of the string is
   --  transferred to the Glib.String.Gstring. Passing null creates an empty
   --  string.

   procedure G_Sized_New (Self : out Gstring; Dfl_Size : Gsize);
   --  Creates a new Glib.String.Gstring, with enough space for Dfl_Size
   --  bytes. This is useful if you are going to add a lot of text to the
   --  string and don't want it to be reallocated too often.
   --  @param Dfl_Size the default size of the space allocated to hold the
   --  string

   function Gstring_Sized_New (Dfl_Size : Gsize) return Gstring;
   --  Creates a new Glib.String.Gstring, with enough space for Dfl_Size
   --  bytes. This is useful if you are going to add a lot of text to the
   --  string and don't want it to be reallocated too often.
   --  @param Dfl_Size the default size of the space allocated to hold the
   --  string

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_gstring_get_type");

   -------------
   -- Methods --
   -------------

   function Append (Self : Gstring; Val : UTF8_String) return Gstring;
   --  Adds a string onto the end of a Glib.String.Gstring, expanding it if
   --  necessary.
   --  @param Val the string to append onto the end of String
   --  @return String

   function Append_C (Self : Gstring; C : Gchar) return Gstring;
   pragma Import (C, Append_C, "g_string_append_c");
   --  Adds a byte onto the end of a Glib.String.Gstring, expanding it if
   --  necessary.
   --  @param C the byte to append onto the end of String
   --  @return String

   function Append_Len
      (Self : Gstring;
       Val  : UTF8_String;
       Len  : Gssize) return Gstring;
   --  Appends Len bytes of Val to String.
   --  If Len is positive, Val may contain embedded nuls and need not be
   --  nul-terminated. It is the caller's responsibility to ensure that Val has
   --  at least Len addressable bytes.
   --  If Len is negative, Val must be nul-terminated and Len is considered to
   --  request the entire string length. This makes Glib.String.Append_Len
   --  equivalent to Glib.String.Append.
   --  @param Val bytes to append
   --  @param Len number of bytes of Val to use, or -1 for all of Val
   --  @return String

   function Append_Unichar (Self : Gstring; Wc : Gunichar) return Gstring;
   pragma Import (C, Append_Unichar, "g_string_append_unichar");
   --  Converts a Unicode character into UTF-8, and appends it to the string.
   --  @param Wc a Unicode character
   --  @return String

   function Append_Uri_Escaped
      (Self                   : Gstring;
       Unescaped              : UTF8_String;
       Reserved_Chars_Allowed : UTF8_String;
       Allow_Utf8             : Boolean) return Gstring;
   --  Appends Unescaped to String, escaping any characters that are reserved
   --  in URIs using URI-style escape sequences.
   --  Since: gtk+ 2.16
   --  @param Unescaped a string
   --  @param Reserved_Chars_Allowed a string of reserved characters allowed
   --  to be used, or null
   --  @param Allow_Utf8 set True if the escaped string may include UTF8
   --  characters
   --  @return String

   function Ascii_Down (Self : Gstring) return Gstring;
   pragma Import (C, Ascii_Down, "g_string_ascii_down");
   --  Converts all uppercase ASCII letters to lowercase ASCII letters.
   --  @return passed-in String pointer, with all the uppercase characters
   --  converted to lowercase in place, with semantics that exactly match
   --  g_ascii_tolower.

   function Ascii_Up (Self : Gstring) return Gstring;
   pragma Import (C, Ascii_Up, "g_string_ascii_up");
   --  Converts all lowercase ASCII letters to uppercase ASCII letters.
   --  @return passed-in String pointer, with all the lowercase characters
   --  converted to uppercase in place, with semantics that exactly match
   --  g_ascii_toupper.

   function Assign (Self : Gstring; Rval : UTF8_String) return Gstring;
   --  Copies the bytes from a string into a Glib.String.Gstring, destroying
   --  any previous contents. It is rather like the standard strcpy function,
   --  except that you do not have to worry about having enough space to copy
   --  the string.
   --  @param Rval the string to copy into String
   --  @return String

   function Copy (Self : Gstring) return Gstring;
   pragma Import (C, Copy, "g_string_copy");

   function Down (Self : Gstring) return Gstring;
   pragma Import (C, Down, "g_string_down");
   pragma Obsolescent (Down);
   --  Converts a Glib.String.Gstring to lowercase.
   --  Deprecated since 2.2, 1
   --  @return the Glib.String.Gstring

   function Equal (Self : Gstring; V2 : Gstring) return Boolean;
   --  Compares two strings for equality, returning True if they are equal.
   --  For use with GHash_Table.
   --  @param V2 another Glib.String.Gstring
   --  @return True if the strings are the same length and contain the same
   --  bytes

   function Erase
      (Self : Gstring;
       Pos  : Gssize;
       Len  : Gssize) return Gstring;
   pragma Import (C, Erase, "g_string_erase");
   --  Removes Len bytes from a Glib.String.Gstring, starting at position Pos.
   --  The rest of the Glib.String.Gstring is shifted down to fill the gap.
   --  @param Pos the position of the content to remove
   --  @param Len the number of bytes to remove, or -1 to remove all following
   --  bytes
   --  @return String

   function Free (Self : Gstring; Free_Segment : Boolean) return UTF8_String;
   --  Frees the memory allocated for the Glib.String.Gstring. If Free_Segment
   --  is True it also frees the character data. If it's False, the caller
   --  gains ownership of the buffer and must free it after use with g_free.
   --  Instead of passing False to this function, consider using
   --  Glib.String.Free_And_Steal.
   --  @param Free_Segment if True, the actual character data is freed as well
   --  @return the character data of String (i.e. null if Free_Segment is
   --  True)

   function Free_And_Steal (Self : Gstring) return UTF8_String;
   --  Frees the memory allocated for the Glib.String.Gstring.
   --  The caller gains ownership of the buffer and must free it after use
   --  with g_free.
   --  Since: gtk+ 2.76
   --  @return the character data of String

   function Hash (Self : Gstring) return Guint;
   pragma Import (C, Hash, "g_string_hash");
   --  Creates a hash code for Str; for use with GHash_Table.
   --  @return hash code for Str

   function Insert
      (Self : Gstring;
       Pos  : Gssize;
       Val  : UTF8_String) return Gstring;
   --  Inserts a copy of a string into a Glib.String.Gstring, expanding it if
   --  necessary.
   --  @param Pos the position to insert the copy of the string
   --  @param Val the string to insert
   --  @return String

   function Insert_C
      (Self : Gstring;
       Pos  : Gssize;
       C    : Gchar) return Gstring;
   pragma Import (C, Insert_C, "g_string_insert_c");
   --  Inserts a byte into a Glib.String.Gstring, expanding it if necessary.
   --  @param Pos the position to insert the byte
   --  @param C the byte to insert
   --  @return String

   function Insert_Len
      (Self : Gstring;
       Pos  : Gssize;
       Val  : UTF8_String;
       Len  : Gssize) return Gstring;
   --  Inserts Len bytes of Val into String at Pos.
   --  If Len is positive, Val may contain embedded nuls and need not be
   --  nul-terminated. It is the caller's responsibility to ensure that Val has
   --  at least Len addressable bytes.
   --  If Len is negative, Val must be nul-terminated and Len is considered to
   --  request the entire string length.
   --  If Pos is -1, bytes are inserted at the end of the string.
   --  @param Pos position in String where insertion should happen, or -1 for
   --  at the end
   --  @param Val bytes to insert
   --  @param Len number of bytes of Val to insert, or -1 for all of Val
   --  @return String

   function Insert_Unichar
      (Self : Gstring;
       Pos  : Gssize;
       Wc   : Gunichar) return Gstring;
   pragma Import (C, Insert_Unichar, "g_string_insert_unichar");
   --  Converts a Unicode character into UTF-8, and insert it into the string
   --  at the given position.
   --  @param Pos the position at which to insert character, or -1 to append
   --  at the end of the string
   --  @param Wc a Unicode character
   --  @return String

   function Overwrite
      (Self : Gstring;
       Pos  : Gsize;
       Val  : UTF8_String) return Gstring;
   --  Overwrites part of a string, lengthening it if necessary.
   --  Since: gtk+ 2.14
   --  @param Pos the position at which to start overwriting
   --  @param Val the string that will overwrite the String starting at Pos
   --  @return String

   function Overwrite_Len
      (Self : Gstring;
       Pos  : Gsize;
       Val  : UTF8_String;
       Len  : Gssize) return Gstring;
   --  Overwrites part of a string, lengthening it if necessary. This function
   --  will work with embedded nuls.
   --  Since: gtk+ 2.14
   --  @param Pos the position at which to start overwriting
   --  @param Val the string that will overwrite the String starting at Pos
   --  @param Len the number of bytes to write from Val
   --  @return String

   function Prepend (Self : Gstring; Val : UTF8_String) return Gstring;
   --  Adds a string on to the start of a Glib.String.Gstring, expanding it if
   --  necessary.
   --  @param Val the string to prepend on the start of String
   --  @return String

   function Prepend_C (Self : Gstring; C : Gchar) return Gstring;
   pragma Import (C, Prepend_C, "g_string_prepend_c");
   --  Adds a byte onto the start of a Glib.String.Gstring, expanding it if
   --  necessary.
   --  @param C the byte to prepend on the start of the Glib.String.Gstring
   --  @return String

   function Prepend_Len
      (Self : Gstring;
       Val  : UTF8_String;
       Len  : Gssize) return Gstring;
   --  Prepends Len bytes of Val to String.
   --  If Len is positive, Val may contain embedded nuls and need not be
   --  nul-terminated. It is the caller's responsibility to ensure that Val has
   --  at least Len addressable bytes.
   --  If Len is negative, Val must be nul-terminated and Len is considered to
   --  request the entire string length. This makes Glib.String.Prepend_Len
   --  equivalent to Glib.String.Prepend.
   --  @param Val bytes to prepend
   --  @param Len number of bytes in Val to prepend, or -1 for all of Val
   --  @return String

   function Prepend_Unichar (Self : Gstring; Wc : Gunichar) return Gstring;
   pragma Import (C, Prepend_Unichar, "g_string_prepend_unichar");
   --  Converts a Unicode character into UTF-8, and prepends it to the string.
   --  @param Wc a Unicode character
   --  @return String

   function Replace
      (Self    : Gstring;
       Find    : UTF8_String;
       Replace : UTF8_String;
       Limit   : Guint) return Guint;
   --  Replaces the string Find with the string Replace in a
   --  Glib.String.Gstring up to Limit times. If the number of instances of
   --  Find in the Glib.String.Gstring is less than Limit, all instances are
   --  replaced. If Limit is `0`, all instances of Find are replaced.
   --  If Find is the empty string, since versions 2.69.1 and 2.68.4 the
   --  replacement will be inserted no more than once per possible position
   --  (beginning of string, end of string and between characters). This did
   --  not work correctly in earlier versions.
   --  Since: gtk+ 2.68
   --  @param Find the string to find in String
   --  @param Replace the string to insert in place of Find
   --  @param Limit the maximum instances of Find to replace with Replace, or
   --  `0` for no limit
   --  @return the number of find and replace operations performed.

   function Set_Size (Self : Gstring; Len : Gsize) return Gstring;
   pragma Import (C, Set_Size, "g_string_set_size");
   --  Sets the length of a Glib.String.Gstring. If the length is less than
   --  the current length, the string will be truncated. If the length is
   --  greater than the current length, the contents of the newly added area
   --  are undefined. (However, as always, string->str[string->len] will be a
   --  nul byte.)
   --  @param Len the new length
   --  @return String

   function Truncate (Self : Gstring; Len : Gsize) return Gstring;
   pragma Import (C, Truncate, "g_string_truncate");
   --  Cuts off the end of the GString, leaving the first Len bytes.
   --  @param Len the new size of String
   --  @return String

   function Up (Self : Gstring) return Gstring;
   pragma Import (C, Up, "g_string_up");
   pragma Obsolescent (Up);
   --  Converts a Glib.String.Gstring to uppercase.
   --  Deprecated since 2.2, 1
   --  @return String

end Glib.String;
