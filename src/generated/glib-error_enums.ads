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


pragma Warnings (Off, "*is already use-visible*");
with Glib.Error;              use Glib.Error;
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Glib.Error_Enums is

   type GBookmark_File_Error is (
      G_Bookmark_File_Error_Invalid_Uri,
      G_Bookmark_File_Error_Invalid_Value,
      G_Bookmark_File_Error_App_Not_Registered,
      G_Bookmark_File_Error_Uri_Not_Found,
      G_Bookmark_File_Error_Read,
      G_Bookmark_File_Error_Unknown_Encoding,
      G_Bookmark_File_Error_Write,
      G_Bookmark_File_Error_File_Not_Found);
   pragma Convention (C, GBookmark_File_Error);
   --  Error codes returned by bookmark file parsing.

   GBookmark_File_Error_Name   : constant UTF8_String := "g-bookmark-file-error-quark";
   GBookmark_File_Error_Domain : constant GQuark := Quark_From_String (GBookmark_File_Error_Name);
   --  Used to identify error domain in a GError

   function GBookmark_File_Error_Matches
     (Error : GError; Code : Gint) return Boolean
   is (Error_Matches (Error, GBookmark_File_Error_Domain, Code));
   --  Convenience helper to match error codes

   type GConvert_Error is (
      G_Convert_Error_No_Conversion,
      G_Convert_Error_Illegal_Sequence,
      G_Convert_Error_Failed,
      G_Convert_Error_Partial_Input,
      G_Convert_Error_Bad_Uri,
      G_Convert_Error_Not_Absolute_Path,
      G_Convert_Error_No_Memory,
      G_Convert_Error_Embedded_Nul);
   pragma Convention (C, GConvert_Error);
   --  Error codes returned by character set conversion routines.

   type GFile_Error is (
      G_File_Error_Exist,
      G_File_Error_Isdir,
      G_File_Error_Acces,
      G_File_Error_Nametoolong,
      G_File_Error_Noent,
      G_File_Error_Notdir,
      G_File_Error_Nxio,
      G_File_Error_Nodev,
      G_File_Error_Rofs,
      G_File_Error_Txtbsy,
      G_File_Error_Fault,
      G_File_Error_Loop,
      G_File_Error_Nospc,
      G_File_Error_Nomem,
      G_File_Error_Mfile,
      G_File_Error_Nfile,
      G_File_Error_Badf,
      G_File_Error_Inval,
      G_File_Error_Pipe,
      G_File_Error_Again,
      G_File_Error_Intr,
      G_File_Error_Io,
      G_File_Error_Perm,
      G_File_Error_Nosys,
      G_File_Error_Failed);
   pragma Convention (C, GFile_Error);
   --  Values corresponding to Errno codes returned from file operations on
   --  UNIX. Unlike Errno codes, GFileError values are available on all
   --  systems, even Windows. The exact meaning of each code depends on what
   --  sort of file operation you were performing; the UNIX documentation gives
   --  more details. The following error code descriptions come from the GNU C
   --  Library manual, and are under the copyright of that manual.
   --
   --  It's not very portable to make detailed assumptions about exactly which
   --  errors will be returned from a given operation. Some errors don't occur
   --  on some systems, etc., sometimes there are subtle differences in when a
   --  system will report a given error, etc.

   GFile_Error_Name   : constant UTF8_String := "g-file-error-quark";
   GFile_Error_Domain : constant GQuark := Quark_From_String (GFile_Error_Name);
   --  Used to identify error domain in a GError

   function GFile_Error_Matches
     (Error : GError; Code : Gint) return Boolean
   is (Error_Matches (Error, GFile_Error_Domain, Code));
   --  Convenience helper to match error codes

   type GKey_File_Error is (
      G_Key_File_Error_Unknown_Encoding,
      G_Key_File_Error_Parse,
      G_Key_File_Error_Not_Found,
      G_Key_File_Error_Key_Not_Found,
      G_Key_File_Error_Group_Not_Found,
      G_Key_File_Error_Invalid_Value);
   pragma Convention (C, GKey_File_Error);
   --  Error codes returned by key file parsing.

   GKey_File_Error_Name   : constant UTF8_String := "g-key-file-error-quark";
   GKey_File_Error_Domain : constant GQuark := Quark_From_String (GKey_File_Error_Name);
   --  Used to identify error domain in a GError

   function GKey_File_Error_Matches
     (Error : GError; Code : Gint) return Boolean
   is (Error_Matches (Error, GKey_File_Error_Domain, Code));
   --  Convenience helper to match error codes

   type GMarkup_Error is (
      G_Markup_Error_Bad_Utf8,
      G_Markup_Error_Empty,
      G_Markup_Error_Parse,
      G_Markup_Error_Unknown_Element,
      G_Markup_Error_Unknown_Attribute,
      G_Markup_Error_Invalid_Content,
      G_Markup_Error_Missing_Attribute);
   pragma Convention (C, GMarkup_Error);
   --  Error codes returned by markup parsing.

   GMarkup_Error_Name   : constant UTF8_String := "g-markup-error-quark";
   GMarkup_Error_Domain : constant GQuark := Quark_From_String (GMarkup_Error_Name);
   --  Used to identify error domain in a GError

   function GMarkup_Error_Matches
     (Error : GError; Code : Gint) return Boolean
   is (Error_Matches (Error, GMarkup_Error_Domain, Code));
   --  Convenience helper to match error codes

   type GNumber_Parser_Error is (
      G_Number_Parser_Error_Invalid,
      G_Number_Parser_Error_Out_Of_Bounds);
   pragma Convention (C, GNumber_Parser_Error);
   --  Error codes returned by functions converting a string to a number.

   GNumber_Parser_Error_Name   : constant UTF8_String := "g-number-parser-error-quark";
   GNumber_Parser_Error_Domain : constant GQuark := Quark_From_String (GNumber_Parser_Error_Name);
   --  Used to identify error domain in a GError

   function GNumber_Parser_Error_Matches
     (Error : GError; Code : Gint) return Boolean
   is (Error_Matches (Error, GNumber_Parser_Error_Domain, Code));
   --  Convenience helper to match error codes

   type GOption_Error is (
      G_Option_Error_Unknown_Option,
      G_Option_Error_Bad_Value,
      G_Option_Error_Failed);
   pragma Convention (C, GOption_Error);
   --  Error codes returned by option parsing.

   GOption_Error_Name   : constant UTF8_String := "g-option-context-error-quark";
   GOption_Error_Domain : constant GQuark := Quark_From_String (GOption_Error_Name);
   --  Used to identify error domain in a GError

   function GOption_Error_Matches
     (Error : GError; Code : Gint) return Boolean
   is (Error_Matches (Error, GOption_Error_Domain, Code));
   --  Convenience helper to match error codes

   type GRegex_Error is (
      G_Regex_Error_Compile,
      G_Regex_Error_Optimize,
      G_Regex_Error_Replace,
      G_Regex_Error_Match,
      G_Regex_Error_Internal,
      G_Regex_Error_Stray_Backslash,
      G_Regex_Error_Missing_Control_Char,
      G_Regex_Error_Unrecognized_Escape,
      G_Regex_Error_Quantifiers_Out_Of_Order,
      G_Regex_Error_Quantifier_Too_Big,
      G_Regex_Error_Unterminated_Character_Class,
      G_Regex_Error_Invalid_Escape_In_Character_Class,
      G_Regex_Error_Range_Out_Of_Order,
      G_Regex_Error_Nothing_To_Repeat,
      G_Regex_Error_Unrecognized_Character,
      G_Regex_Error_Posix_Named_Class_Outside_Class,
      G_Regex_Error_Unmatched_Parenthesis,
      G_Regex_Error_Inexistent_Subpattern_Reference,
      G_Regex_Error_Unterminated_Comment,
      G_Regex_Error_Expression_Too_Large,
      G_Regex_Error_Memory_Error,
      G_Regex_Error_Variable_Length_Lookbehind,
      G_Regex_Error_Malformed_Condition,
      G_Regex_Error_Too_Many_Conditional_Branches,
      G_Regex_Error_Assertion_Expected,
      G_Regex_Error_Unknown_Posix_Class_Name,
      G_Regex_Error_Posix_Collating_Elements_Not_Supported,
      G_Regex_Error_Hex_Code_Too_Large,
      G_Regex_Error_Invalid_Condition,
      G_Regex_Error_Single_Byte_Match_In_Lookbehind,
      G_Regex_Error_Infinite_Loop,
      G_Regex_Error_Missing_Subpattern_Name_Terminator,
      G_Regex_Error_Duplicate_Subpattern_Name,
      G_Regex_Error_Malformed_Property,
      G_Regex_Error_Unknown_Property,
      G_Regex_Error_Subpattern_Name_Too_Long,
      G_Regex_Error_Too_Many_Subpatterns,
      G_Regex_Error_Invalid_Octal_Value,
      G_Regex_Error_Too_Many_Branches_In_Define,
      G_Regex_Error_Define_Repetion,
      G_Regex_Error_Inconsistent_Newline_Options,
      G_Regex_Error_Missing_Back_Reference,
      G_Regex_Error_Invalid_Relative_Reference,
      G_Regex_Error_Backtracking_Control_Verb_Argument_Forbidden,
      G_Regex_Error_Unknown_Backtracking_Control_Verb,
      G_Regex_Error_Number_Too_Big,
      G_Regex_Error_Missing_Subpattern_Name,
      G_Regex_Error_Missing_Digit,
      G_Regex_Error_Invalid_Data_Character,
      G_Regex_Error_Extra_Subpattern_Name,
      G_Regex_Error_Backtracking_Control_Verb_Argument_Required,
      G_Regex_Error_Invalid_Control_Char,
      G_Regex_Error_Missing_Name,
      G_Regex_Error_Not_Supported_In_Class,
      G_Regex_Error_Too_Many_Forward_References,
      G_Regex_Error_Name_Too_Long,
      G_Regex_Error_Character_Value_Too_Large);
   pragma Convention (C, GRegex_Error);
   --  Error codes returned by regular expressions functions.

   for GRegex_Error use (
      G_Regex_Error_Compile => 0,
      G_Regex_Error_Optimize => 1,
      G_Regex_Error_Replace => 2,
      G_Regex_Error_Match => 3,
      G_Regex_Error_Internal => 4,
      G_Regex_Error_Stray_Backslash => 101,
      G_Regex_Error_Missing_Control_Char => 102,
      G_Regex_Error_Unrecognized_Escape => 103,
      G_Regex_Error_Quantifiers_Out_Of_Order => 104,
      G_Regex_Error_Quantifier_Too_Big => 105,
      G_Regex_Error_Unterminated_Character_Class => 106,
      G_Regex_Error_Invalid_Escape_In_Character_Class => 107,
      G_Regex_Error_Range_Out_Of_Order => 108,
      G_Regex_Error_Nothing_To_Repeat => 109,
      G_Regex_Error_Unrecognized_Character => 112,
      G_Regex_Error_Posix_Named_Class_Outside_Class => 113,
      G_Regex_Error_Unmatched_Parenthesis => 114,
      G_Regex_Error_Inexistent_Subpattern_Reference => 115,
      G_Regex_Error_Unterminated_Comment => 118,
      G_Regex_Error_Expression_Too_Large => 120,
      G_Regex_Error_Memory_Error => 121,
      G_Regex_Error_Variable_Length_Lookbehind => 125,
      G_Regex_Error_Malformed_Condition => 126,
      G_Regex_Error_Too_Many_Conditional_Branches => 127,
      G_Regex_Error_Assertion_Expected => 128,
      G_Regex_Error_Unknown_Posix_Class_Name => 130,
      G_Regex_Error_Posix_Collating_Elements_Not_Supported => 131,
      G_Regex_Error_Hex_Code_Too_Large => 134,
      G_Regex_Error_Invalid_Condition => 135,
      G_Regex_Error_Single_Byte_Match_In_Lookbehind => 136,
      G_Regex_Error_Infinite_Loop => 140,
      G_Regex_Error_Missing_Subpattern_Name_Terminator => 142,
      G_Regex_Error_Duplicate_Subpattern_Name => 143,
      G_Regex_Error_Malformed_Property => 146,
      G_Regex_Error_Unknown_Property => 147,
      G_Regex_Error_Subpattern_Name_Too_Long => 148,
      G_Regex_Error_Too_Many_Subpatterns => 149,
      G_Regex_Error_Invalid_Octal_Value => 151,
      G_Regex_Error_Too_Many_Branches_In_Define => 154,
      G_Regex_Error_Define_Repetion => 155,
      G_Regex_Error_Inconsistent_Newline_Options => 156,
      G_Regex_Error_Missing_Back_Reference => 157,
      G_Regex_Error_Invalid_Relative_Reference => 158,
      G_Regex_Error_Backtracking_Control_Verb_Argument_Forbidden => 159,
      G_Regex_Error_Unknown_Backtracking_Control_Verb => 160,
      G_Regex_Error_Number_Too_Big => 161,
      G_Regex_Error_Missing_Subpattern_Name => 162,
      G_Regex_Error_Missing_Digit => 163,
      G_Regex_Error_Invalid_Data_Character => 164,
      G_Regex_Error_Extra_Subpattern_Name => 165,
      G_Regex_Error_Backtracking_Control_Verb_Argument_Required => 166,
      G_Regex_Error_Invalid_Control_Char => 168,
      G_Regex_Error_Missing_Name => 169,
      G_Regex_Error_Not_Supported_In_Class => 171,
      G_Regex_Error_Too_Many_Forward_References => 172,
      G_Regex_Error_Name_Too_Long => 175,
      G_Regex_Error_Character_Value_Too_Large => 176);

   GRegex_Error_Name   : constant UTF8_String := "g-regex-error-quark";
   GRegex_Error_Domain : constant GQuark := Quark_From_String (GRegex_Error_Name);
   --  Used to identify error domain in a GError

   function GRegex_Error_Matches
     (Error : GError; Code : Gint) return Boolean
   is (Error_Matches (Error, GRegex_Error_Domain, Code));
   --  Convenience helper to match error codes

   type GShell_Error is (
      G_Shell_Error_Bad_Quoting,
      G_Shell_Error_Empty_String,
      G_Shell_Error_Failed);
   pragma Convention (C, GShell_Error);
   --  Error codes returned by shell functions.

   GShell_Error_Name   : constant UTF8_String := "g-shell-error-quark";
   GShell_Error_Domain : constant GQuark := Quark_From_String (GShell_Error_Name);
   --  Used to identify error domain in a GError

   function GShell_Error_Matches
     (Error : GError; Code : Gint) return Boolean
   is (Error_Matches (Error, GShell_Error_Domain, Code));
   --  Convenience helper to match error codes

   type GThread_Error is (
      G_Thread_Error_Again);
   pragma Convention (C, GThread_Error);
   --  Possible errors of thread related functions.

   type GUri_Error is (
      G_Uri_Error_Failed,
      G_Uri_Error_Bad_Scheme,
      G_Uri_Error_Bad_User,
      G_Uri_Error_Bad_Password,
      G_Uri_Error_Bad_Auth_Params,
      G_Uri_Error_Bad_Host,
      G_Uri_Error_Bad_Port,
      G_Uri_Error_Bad_Path,
      G_Uri_Error_Bad_Query,
      G_Uri_Error_Bad_Fragment);
   pragma Convention (C, GUri_Error);
   --  Error codes returned by Guri.Guri methods.

   type GVariant_Parse_Error is (
      G_Variant_Parse_Error_Failed,
      G_Variant_Parse_Error_Basic_Type_Expected,
      G_Variant_Parse_Error_Cannot_Infer_Type,
      G_Variant_Parse_Error_Definite_Type_Expected,
      G_Variant_Parse_Error_Input_Not_At_End,
      G_Variant_Parse_Error_Invalid_Character,
      G_Variant_Parse_Error_Invalid_Format_String,
      G_Variant_Parse_Error_Invalid_Object_Path,
      G_Variant_Parse_Error_Invalid_Signature,
      G_Variant_Parse_Error_Invalid_Type_String,
      G_Variant_Parse_Error_No_Common_Type,
      G_Variant_Parse_Error_Number_Out_Of_Range,
      G_Variant_Parse_Error_Number_Too_Big,
      G_Variant_Parse_Error_Type_Error,
      G_Variant_Parse_Error_Unexpected_Token,
      G_Variant_Parse_Error_Unknown_Keyword,
      G_Variant_Parse_Error_Unterminated_String_Constant,
      G_Variant_Parse_Error_Value_Expected,
      G_Variant_Parse_Error_Recursion);
   pragma Convention (C, GVariant_Parse_Error);
   --  Error codes returned by parsing text-format GVariants.

   GVariant_Parse_Error_Name   : constant UTF8_String := "g-variant-parse-error-quark";
   GVariant_Parse_Error_Domain : constant GQuark := Quark_From_String (GVariant_Parse_Error_Name);
   --  Used to identify error domain in a GError

   function GVariant_Parse_Error_Matches
     (Error : GError; Code : Gint) return Boolean
   is (Error_Matches (Error, GVariant_Parse_Error_Domain, Code));
   --  Convenience helper to match error codes

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package GBookmark_File_Error_Properties is
      new Generic_Internal_Discrete_Property (GBookmark_File_Error);
   type Property_GBookmark_File_Error is new GBookmark_File_Error_Properties.Property;

   package GConvert_Error_Properties is
      new Generic_Internal_Discrete_Property (GConvert_Error);
   type Property_GConvert_Error is new GConvert_Error_Properties.Property;

   package GFile_Error_Properties is
      new Generic_Internal_Discrete_Property (GFile_Error);
   type Property_GFile_Error is new GFile_Error_Properties.Property;

   package GKey_File_Error_Properties is
      new Generic_Internal_Discrete_Property (GKey_File_Error);
   type Property_GKey_File_Error is new GKey_File_Error_Properties.Property;

   package GMarkup_Error_Properties is
      new Generic_Internal_Discrete_Property (GMarkup_Error);
   type Property_GMarkup_Error is new GMarkup_Error_Properties.Property;

   package GNumber_Parser_Error_Properties is
      new Generic_Internal_Discrete_Property (GNumber_Parser_Error);
   type Property_GNumber_Parser_Error is new GNumber_Parser_Error_Properties.Property;

   package GOption_Error_Properties is
      new Generic_Internal_Discrete_Property (GOption_Error);
   type Property_GOption_Error is new GOption_Error_Properties.Property;

   package GRegex_Error_Properties is
      new Generic_Internal_Discrete_Property (GRegex_Error);
   type Property_GRegex_Error is new GRegex_Error_Properties.Property;

   package GShell_Error_Properties is
      new Generic_Internal_Discrete_Property (GShell_Error);
   type Property_GShell_Error is new GShell_Error_Properties.Property;

   package GThread_Error_Properties is
      new Generic_Internal_Discrete_Property (GThread_Error);
   type Property_GThread_Error is new GThread_Error_Properties.Property;

   package GUri_Error_Properties is
      new Generic_Internal_Discrete_Property (GUri_Error);
   type Property_GUri_Error is new GUri_Error_Properties.Property;

   package GVariant_Parse_Error_Properties is
      new Generic_Internal_Discrete_Property (GVariant_Parse_Error);
   type Property_GVariant_Parse_Error is new GVariant_Parse_Error_Properties.Property;

end Glib.Error_Enums;
