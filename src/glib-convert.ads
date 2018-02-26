------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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
--
--  This package provides definitions for string conversions and i18n.
--  See also Glib.Unicode.
--
--  </description>
--  <c_version>1.3.11</c_version>
--  <group>Glib, the general-purpose library</group>

with Glib.Error; use Glib.Error;
with Gtkada.Types;

package Glib.Convert is
   pragma Preelaborate;

   --  Convert_Error domain for GErrors:

   No_Conversion     : constant := 0;
   Illegal_Sequence  : constant := 1;
   Failed            : constant := 2;
   Partial_Input     : constant := 3;
   Bad_URI           : constant := 4;
   Not_Absolute_Path : constant := 5;

   function Convert_Error_Domain return GQuark;
   --  Return the error domain associated with Glib.Convert.

   procedure Convert
     (Str           : String;
      To_Codeset    : String;
      From_Codeset  : String;
      Bytes_Read    : out Natural;
      Bytes_Written : out Natural;
      Error         : GError_Access := null;
      Result        : out String);
   --  Convert a string from one character set to another.
   --
   --  Str:           String to convert
   --  Result:        String converted, if no error.
   --  To_Codeset:    Name of character set into which to convert Str
   --  From_Codeset:  Character set of Str.
   --  Bytes_Read:    Number of bytes in the input string that were
   --                 successfully converted.
   --                 Even if the conversion was successful, this may be
   --                 less than Len if there were partial characters
   --                 at the end of the input. If the error
   --                 Illegal_Sequence occurs, the value
   --                 stored will the byte offset after the last valid
   --                 input sequence.
   --  Bytes_Written: Number of bytes stored in the output buffer.
   --  Error:         Location to store the error occuring, ignored if null.
   --                 Any of the errors in Convert_Error_Domain may occur.

   function Convert
     (Str           : String;
      To_Codeset    : String;
      From_Codeset  : String;
      Error         : GError_Access := null) return String;
   --  Same as above, but return a String directly.

   function Convert
     (Str           : String;
      To_Codeset    : String;
      From_Codeset  : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural;
      Error         : GError_Access := null) return Gtkada.Types.Chars_Ptr;
   --  Same as Convert procedure, but return the result as a C string.

   procedure Locale_To_UTF8
     (OS_String     : String;
      Bytes_Read    : out Natural;
      Bytes_Written : out Natural;
      Error         : GError_Access := null;
      Result        : out String);
   --  Convert a string which is in the encoding used for strings by
   --  the C runtime (usually the same as that used by the operating
   --  system) in the current locale into a UTF-8 string.
   --
   --  OS_String:     A string in the encoding of the current locale
   --  Bytes_Read:    Number of bytes in the input string that were
   --                 successfully converted.
   --                 Even if the conversion was successful, this may be
   --                 less than Len if there were partial characters
   --                 at the end of the input. If the error
   --                 Illegal_Sequence occurs, the value
   --                 stored will the byte offset after the last valid
   --                 input sequence.
   --  Bytes_Written: Number of bytes stored in Result.
   --  Error:         Location to store the error occuring, ignored if null.
   --                 Any of the errors in Convert_Error_Domain may occur.

   function Locale_To_UTF8
     (OS_String     : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural;
      Error         : GError_Access := null) return Gtkada.Types.Chars_Ptr;
   --  Same as procedure Locale_To_UTF8, but return the raw C string for
   --  efficiency. The caller is responsible for freeing the resulting string.

   function Locale_To_UTF8 (OS_String : String) return String;
   --  Same as procedure Locale_To_UTF8, but return only the String.

   procedure Locale_From_UTF8
     (UTF8_String   : String;
      Bytes_Read    : out Natural;
      Bytes_Written : out Natural;
      Error         : GError_Access := null;
      Result        : out String);
   --  Convert a string from UTF-8 to the encoding used for strings by
   --  the C runtime (usually the same as that used by the operating
   --  system) in the current locale.
   --
   --  UTF8_String:   A UTF-8 encoded string
   --  Bytes_Read:    Number of bytes in the input string that were
   --                 successfully converted.
   --                 Even if the conversion was successful, this may be
   --                 less than Len if there were partial characters
   --                 at the end of the input. If the error
   --                 Illegal_Sequence occurs, the value
   --                 stored will the byte offset after the last valid
   --                 input sequence.
   --  Bytes_Written: Number of bytes stored in the output buffer.
   --  Error:         Location to store the error occuring, ignored if null.
   --                 Any of the errors in Convert_Error_Domain may occur.

   function Locale_From_UTF8
     (UTF8_String   : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural;
      Error         : GError_Access := null) return Gtkada.Types.Chars_Ptr;
   --  Same as procedure Locale_From_UTF8, but return the raw C string for
   --  efficiency. The caller is responsible for freeing the resulting string.
   --  Use the C "free" function to free this.

   function Locale_From_UTF8 (UTF8_String : String) return String;
   --  Same as procedure Locale_From_UTF8, but return only the String.

   function Filename_To_UTF8
     (OS_String : String;
      Error     : GError_Access := null) return String;
   --  Convert a string which is in the encoding used for filenames
   --  into a UTF-8 string.

   function Filename_From_UTF8
     (UTF8_String : String;
      Error       : GError_Access := null) return String;
   --  Convert a string from UTF-8 to the encoding used for filenames.

   function Filename_From_URI
     (URI      : String;
      Hostname : access Gtkada.Types.Chars_Ptr;
      Error    : GError_Access := null) return String;
   --  Convert an escaped UTF-8 encoded URI to a local filename in the
   --  encoding used for filenames.
   --
   --  URI:      A uri describing a filename (escaped, encoded in UTF-8).
   --  Hostname: Location to store hostname for the URI.
   --            If there is no hostname in the URI, null will be
   --            stored in this location.
   --  Error:    Location to store the error occuring, ignored if null.
   --            Any of the errors in Convert_Error_Domain may occur.

   function Filename_To_URI
     (Filename : String;
      Hostname : String := "";
      Error    : GError_Access := null) return String;
   --  Convert an absolute filename to an escaped UTF-8 encoded URI.
   --
   --  Filename: An absolute filename specified in the encoding
   --            used for filenames by the operating system.
   --  Hostname: A UTF-8 encoded hostname, or "" for none.
   --  Error:    Location to store the error occuring, ignored if null.
   --            Any of the errors in Convert_Error may occur.

   function Escape_Text (S : String) return String;
   --  Escape the text so that it is interpreted as-is by the Pango markup
   --  language

private

   pragma Import (C, Convert_Error_Domain, "g_convert_error_quark");

end Glib.Convert;
