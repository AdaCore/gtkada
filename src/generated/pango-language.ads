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
--  The Pango.Language.Pango_Language structure is used to represent a
--  language.
--
--  Pango.Language.Pango_Language pointers can be efficiently copied and
--  compared with each other.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Pango.Language is

   type Pango_Language is new Glib.C_Boxed with null record;
   Null_Pango_Language : constant Pango_Language;

   function From_Object (Object : System.Address) return Pango_Language;
   function From_Object_Free (B : access Pango_Language'Class) return Pango_Language;
   pragma Inline (From_Object_Free, From_Object);

   type Pango_Script is (
      Pango_Script_Invalid_Code,
      Pango_Script_Common,
      Pango_Script_Inherited,
      Pango_Script_Arabic,
      Pango_Script_Armenian,
      Pango_Script_Bengali,
      Pango_Script_Bopomofo,
      Pango_Script_Cherokee,
      Pango_Script_Coptic,
      Pango_Script_Cyrillic,
      Pango_Script_Deseret,
      Pango_Script_Devanagari,
      Pango_Script_Ethiopic,
      Pango_Script_Georgian,
      Pango_Script_Gothic,
      Pango_Script_Greek,
      Pango_Script_Gujarati,
      Pango_Script_Gurmukhi,
      Pango_Script_Han,
      Pango_Script_Hangul,
      Pango_Script_Hebrew,
      Pango_Script_Hiragana,
      Pango_Script_Kannada,
      Pango_Script_Katakana,
      Pango_Script_Khmer,
      Pango_Script_Lao,
      Pango_Script_Latin,
      Pango_Script_Malayalam,
      Pango_Script_Mongolian,
      Pango_Script_Myanmar,
      Pango_Script_Ogham,
      Pango_Script_Old_Italic,
      Pango_Script_Oriya,
      Pango_Script_Runic,
      Pango_Script_Sinhala,
      Pango_Script_Syriac,
      Pango_Script_Tamil,
      Pango_Script_Telugu,
      Pango_Script_Thaana,
      Pango_Script_Thai,
      Pango_Script_Tibetan,
      Pango_Script_Canadian_Aboriginal,
      Pango_Script_Yi,
      Pango_Script_Tagalog,
      Pango_Script_Hanunoo,
      Pango_Script_Buhid,
      Pango_Script_Tagbanwa,
      Pango_Script_Braille,
      Pango_Script_Cypriot,
      Pango_Script_Limbu,
      Pango_Script_Osmanya,
      Pango_Script_Shavian,
      Pango_Script_Linear_B,
      Pango_Script_Tai_Le,
      Pango_Script_Ugaritic,
      Pango_Script_New_Tai_Lue,
      Pango_Script_Buginese,
      Pango_Script_Glagolitic,
      Pango_Script_Tifinagh,
      Pango_Script_Syloti_Nagri,
      Pango_Script_Old_Persian,
      Pango_Script_Kharoshthi,
      Pango_Script_Unknown,
      Pango_Script_Balinese,
      Pango_Script_Cuneiform,
      Pango_Script_Phoenician,
      Pango_Script_Phags_Pa,
      Pango_Script_Nko,
      Pango_Script_Kayah_Li,
      Pango_Script_Lepcha,
      Pango_Script_Rejang,
      Pango_Script_Sundanese,
      Pango_Script_Saurashtra,
      Pango_Script_Cham,
      Pango_Script_Ol_Chiki,
      Pango_Script_Vai,
      Pango_Script_Carian,
      Pango_Script_Lycian,
      Pango_Script_Lydian,
      Pango_Script_Batak,
      Pango_Script_Brahmi,
      Pango_Script_Mandaic,
      Pango_Script_Chakma,
      Pango_Script_Meroitic_Cursive,
      Pango_Script_Meroitic_Hieroglyphs,
      Pango_Script_Miao,
      Pango_Script_Sharada,
      Pango_Script_Sora_Sompeng,
      Pango_Script_Takri,
      Pango_Script_Bassa_Vah,
      Pango_Script_Caucasian_Albanian,
      Pango_Script_Duployan,
      Pango_Script_Elbasan,
      Pango_Script_Grantha,
      Pango_Script_Khojki,
      Pango_Script_Khudawadi,
      Pango_Script_Linear_A,
      Pango_Script_Mahajani,
      Pango_Script_Manichaean,
      Pango_Script_Mende_Kikakui,
      Pango_Script_Modi,
      Pango_Script_Mro,
      Pango_Script_Nabataean,
      Pango_Script_Old_North_Arabian,
      Pango_Script_Old_Permic,
      Pango_Script_Pahawh_Hmong,
      Pango_Script_Palmyrene,
      Pango_Script_Pau_Cin_Hau,
      Pango_Script_Psalter_Pahlavi,
      Pango_Script_Siddham,
      Pango_Script_Tirhuta,
      Pango_Script_Warang_Citi,
      Pango_Script_Ahom,
      Pango_Script_Anatolian_Hieroglyphs,
      Pango_Script_Hatran,
      Pango_Script_Multani,
      Pango_Script_Old_Hungarian,
      Pango_Script_Signwriting);
   pragma Convention (C, Pango_Script);
   --  The Pango.Language.Pango_Script enumeration identifies different
   --  writing systems. The values correspond to the names as defined in the
   --  Unicode standard. See <ulink
   --  url="http://www.unicode.org/reports/tr24/">Unicode Standard Annex 24:
   --  Script names</ulink>.
   --
   --  Note that this enumeration is deprecated and will not be updated to
   --  include values in newer versions of the Unicode standard. Applications
   --  should use the GUnicodeScript enumeration instead, whose values are
   --  interchangeable with PangoScript.

   for Pango_Script use (
      Pango_Script_Invalid_Code => -1,
      Pango_Script_Common => 0,
      Pango_Script_Inherited => 1,
      Pango_Script_Arabic => 2,
      Pango_Script_Armenian => 3,
      Pango_Script_Bengali => 4,
      Pango_Script_Bopomofo => 5,
      Pango_Script_Cherokee => 6,
      Pango_Script_Coptic => 7,
      Pango_Script_Cyrillic => 8,
      Pango_Script_Deseret => 9,
      Pango_Script_Devanagari => 10,
      Pango_Script_Ethiopic => 11,
      Pango_Script_Georgian => 12,
      Pango_Script_Gothic => 13,
      Pango_Script_Greek => 14,
      Pango_Script_Gujarati => 15,
      Pango_Script_Gurmukhi => 16,
      Pango_Script_Han => 17,
      Pango_Script_Hangul => 18,
      Pango_Script_Hebrew => 19,
      Pango_Script_Hiragana => 20,
      Pango_Script_Kannada => 21,
      Pango_Script_Katakana => 22,
      Pango_Script_Khmer => 23,
      Pango_Script_Lao => 24,
      Pango_Script_Latin => 25,
      Pango_Script_Malayalam => 26,
      Pango_Script_Mongolian => 27,
      Pango_Script_Myanmar => 28,
      Pango_Script_Ogham => 29,
      Pango_Script_Old_Italic => 30,
      Pango_Script_Oriya => 31,
      Pango_Script_Runic => 32,
      Pango_Script_Sinhala => 33,
      Pango_Script_Syriac => 34,
      Pango_Script_Tamil => 35,
      Pango_Script_Telugu => 36,
      Pango_Script_Thaana => 37,
      Pango_Script_Thai => 38,
      Pango_Script_Tibetan => 39,
      Pango_Script_Canadian_Aboriginal => 40,
      Pango_Script_Yi => 41,
      Pango_Script_Tagalog => 42,
      Pango_Script_Hanunoo => 43,
      Pango_Script_Buhid => 44,
      Pango_Script_Tagbanwa => 45,
      Pango_Script_Braille => 46,
      Pango_Script_Cypriot => 47,
      Pango_Script_Limbu => 48,
      Pango_Script_Osmanya => 49,
      Pango_Script_Shavian => 50,
      Pango_Script_Linear_B => 51,
      Pango_Script_Tai_Le => 52,
      Pango_Script_Ugaritic => 53,
      Pango_Script_New_Tai_Lue => 54,
      Pango_Script_Buginese => 55,
      Pango_Script_Glagolitic => 56,
      Pango_Script_Tifinagh => 57,
      Pango_Script_Syloti_Nagri => 58,
      Pango_Script_Old_Persian => 59,
      Pango_Script_Kharoshthi => 60,
      Pango_Script_Unknown => 61,
      Pango_Script_Balinese => 62,
      Pango_Script_Cuneiform => 63,
      Pango_Script_Phoenician => 64,
      Pango_Script_Phags_Pa => 65,
      Pango_Script_Nko => 66,
      Pango_Script_Kayah_Li => 67,
      Pango_Script_Lepcha => 68,
      Pango_Script_Rejang => 69,
      Pango_Script_Sundanese => 70,
      Pango_Script_Saurashtra => 71,
      Pango_Script_Cham => 72,
      Pango_Script_Ol_Chiki => 73,
      Pango_Script_Vai => 74,
      Pango_Script_Carian => 75,
      Pango_Script_Lycian => 76,
      Pango_Script_Lydian => 77,
      Pango_Script_Batak => 78,
      Pango_Script_Brahmi => 79,
      Pango_Script_Mandaic => 80,
      Pango_Script_Chakma => 81,
      Pango_Script_Meroitic_Cursive => 82,
      Pango_Script_Meroitic_Hieroglyphs => 83,
      Pango_Script_Miao => 84,
      Pango_Script_Sharada => 85,
      Pango_Script_Sora_Sompeng => 86,
      Pango_Script_Takri => 87,
      Pango_Script_Bassa_Vah => 88,
      Pango_Script_Caucasian_Albanian => 89,
      Pango_Script_Duployan => 90,
      Pango_Script_Elbasan => 91,
      Pango_Script_Grantha => 92,
      Pango_Script_Khojki => 93,
      Pango_Script_Khudawadi => 94,
      Pango_Script_Linear_A => 95,
      Pango_Script_Mahajani => 96,
      Pango_Script_Manichaean => 97,
      Pango_Script_Mende_Kikakui => 98,
      Pango_Script_Modi => 99,
      Pango_Script_Mro => 100,
      Pango_Script_Nabataean => 101,
      Pango_Script_Old_North_Arabian => 102,
      Pango_Script_Old_Permic => 103,
      Pango_Script_Pahawh_Hmong => 104,
      Pango_Script_Palmyrene => 105,
      Pango_Script_Pau_Cin_Hau => 106,
      Pango_Script_Psalter_Pahlavi => 107,
      Pango_Script_Siddham => 108,
      Pango_Script_Tirhuta => 109,
      Pango_Script_Warang_Citi => 110,
      Pango_Script_Ahom => 111,
      Pango_Script_Anatolian_Hieroglyphs => 112,
      Pango_Script_Hatran => 113,
      Pango_Script_Multani => 114,
      Pango_Script_Old_Hungarian => 115,
      Pango_Script_Signwriting => 116);

   type Pango_Script_Array is array (Natural range <>) of Pango_Script;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Pango_Script_Properties is
      new Generic_Internal_Discrete_Property (Pango_Script);
   type Property_Pango_Script is new Pango_Script_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "pango_language_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Sample_String (Self : Pango_Language) return UTF8_String;
   --  Get a string that is representative of the characters needed to render
   --  a particular language.
   --  The sample text may be a pangram, but is not necessarily. It is chosen
   --  to be demonstrative of normal text in the language, as well as exposing
   --  font feature requirements unique to the language. It is suitable for use
   --  as sample text in a font selection dialog.
   --  If Language is null, the default language as found by
   --  Pango.Language.Get_Default is used.
   --  If Pango does not have a sample string for Language, the classic "The
   --  quick brown fox..." is returned. This can be detected by comparing the
   --  returned pointer value to that returned for (non-existent) language code
   --  "xx". That is, compare to:
   --    pango_language_get_sample_string (pango_language_from_string ("xx"))

   function Get_Scripts (Self : Pango_Language) return Pango_Script_Array;
   --  Determines the scripts used to to write Language. If nothing is known
   --  about the language tag Language, or if Language is null, then null is
   --  returned. The list of scripts returned starts with the script that the
   --  language uses most and continues to the one it uses least.
   --  The value Num_Script points at will be set to the number of scripts in
   --  the returned array (or zero if null is returned).
   --  Most languages use only one script for writing, but there are some that
   --  use two (Latin and Cyrillic for example), and a few use three (Japanese
   --  for example). Applications should not make any assumptions on the
   --  maximum number of scripts returned though, except that it is positive if
   --  the return value is not null, and it is a small number.
   --  The Pango.Language.Includes_Script function uses this function
   --  internally.
   --  Note: while the return value is declared as PangoScript, the returned
   --  values are from the GUnicodeScript enumeration, which may have more
   --  values. Callers need to handle unknown values.
   --  Since: gtk+ 1.22

   function Includes_Script
      (Self   : Pango_Language;
       Script : Pango_Script) return Boolean;
   --  Determines if Script is one of the scripts used to write Language. The
   --  returned value is conservative; if nothing is known about the language
   --  tag Language, True will be returned, since, as far as Pango knows,
   --  Script might be used to write Language.
   --  This routine is used in Pango's itemization process when determining if
   --  a supplied language tag is relevant to a particular section of text. It
   --  probably is not useful for applications in most circumstances.
   --  This function uses Pango.Language.Get_Scripts internally.
   --  Since: gtk+ 1.4
   --  "script": a Pango.Language.Pango_Script

   function Matches
      (Self       : Pango_Language;
       Range_List : UTF8_String) return Boolean;
   --  Checks if a language tag matches one of the elements in a list of
   --  language ranges. A language tag is considered to match a range in the
   --  list if the range is '*', the range is exactly the tag, or the range is
   --  a prefix of the tag, and the character after it in the tag is '-'.
   --  "range_list": a list of language ranges, separated by ';', ':', ',', or
   --  space characters. Each element must either be '*', or a RFC 3066
   --  language range canonicalized as by Pango.Language.From_String

   function To_String (Self : Pango_Language) return UTF8_String;
   --  Gets the RFC-3066 format string representing the given language tag.

   ---------------
   -- Functions --
   ---------------

   function From_String (Language : UTF8_String := "") return Pango_Language;
   --  Take a RFC-3066 format language tag as a string and convert it to a
   --  Pango.Language.Pango_Language pointer that can be efficiently copied
   --  (copy the pointer) and compared with other language tags (compare the
   --  pointer.)
   --  This function first canonicalizes the string by converting it to
   --  lowercase, mapping '_' to '-', and stripping all characters other than
   --  letters and '-'.
   --  Use Pango.Language.Get_Default if you want to get the
   --  Pango.Language.Pango_Language for the current locale of the process.
   --  "language": a string representing a language tag, or null

   function Get_Default return Pango_Language;
   --  Returns the Pango.Language.Pango_Language for the current locale of the
   --  process. Note that this can change over the life of an application.
   --  On Unix systems, this is the return value is derived from
   --  'setlocale(LC_CTYPE, NULL)', and the user can affect this through the
   --  environment variables LC_ALL, LC_CTYPE or LANG (checked in that order).
   --  The locale string typically is in the form lang_COUNTRY, where lang is
   --  an ISO-639 language code, and COUNTRY is an ISO-3166 country code. For
   --  instance, sv_FI for Swedish as written in Finland or pt_BR for
   --  Portuguese as written in Brazil.
   --  On Windows, the C library does not use any such environment variables,
   --  and setting them won't affect the behavior of functions like ctime. The
   --  user sets the locale through the Regional Options in the Control Panel.
   --  The C library (in the setlocale function) does not use country and
   --  language codes, but country and language names spelled out in English.
   --  However, this function does check the above environment variables, and
   --  does return a Unix-style locale string based on either said environment
   --  variables or the thread's current locale.
   --  Your application should call 'setlocale(LC_ALL, "");' for the user
   --  settings to take effect. Gtk+ does this in its initialization functions
   --  automatically (by calling gtk_set_locale). See 'man setlocale' for more
   --  details.
   --  Since: gtk+ 1.16

private

   Null_Pango_Language : constant Pango_Language := (Glib.C_Boxed with null record);

end Pango.Language;
