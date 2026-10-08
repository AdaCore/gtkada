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

--  Glib.Settings_Schema_Key.Gsettings_Schema_Key is an opaque data structure
--  and can only be accessed using the following functions.
--
--  <group>GIO</group>
--  <gtkada_demo>create_settings.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib.Variant; use Glib.Variant;

package Glib.Settings_Schema_Key is

   type Gsettings_Schema_Key is new Glib.C_Boxed with null record;
   Null_Gsettings_Schema_Key : constant Gsettings_Schema_Key;

   function From_Object (Object : System.Address) return Gsettings_Schema_Key;
   function From_Object_Free (B : access Gsettings_Schema_Key'Class) return Gsettings_Schema_Key;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_settings_schema_key_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Default_Value
      (Self : Gsettings_Schema_Key) return Glib.Variant.Gvariant;
   --  Gets the default value for Key.
   --  Note that this is the default value according to the schema. System
   --  administrator defaults and lockdown are not visible via this API.
   --  Since: gtk+ 2.40
   --  @return the default value for the key. Has transfer-ownership='full'.

   function Get_Description (Self : Gsettings_Schema_Key) return UTF8_String;
   --  Gets the description for Key.
   --  If no description has been provided in the schema for Key, returns
   --  null.
   --  The description can be one sentence to several paragraphs in length.
   --  Paragraphs are delimited with a double newline. Descriptions can be
   --  translated and the value returned from this function is is the current
   --  locale.
   --  This function is slow. The summary and description information for the
   --  schemas is not stored in the compiled schema database so this function
   --  has to parse all of the source XML files in the schema directory.
   --  Since: gtk+ 2.34
   --  @return the description for Key, or null

   function Get_Name (Self : Gsettings_Schema_Key) return UTF8_String;
   --  Gets the name of Key.
   --  Since: gtk+ 2.44
   --  @return the name of Key.

   function Get_Range
      (Self : Gsettings_Schema_Key) return Glib.Variant.Gvariant;
   --  Queries the range of a key.
   --  This function will return a Glib.Variant.Gvariant that fully describes
   --  the range of values that are valid for Key.
   --  The type of Glib.Variant.Gvariant returned is `(sv)`. The string
   --  describes the type of range restriction in effect. The type and meaning
   --  of the value contained in the variant depends on the string.
   --  If the string is `'type'` then the variant contains an empty array. The
   --  element type of that empty array is the expected type of value and all
   --  values of that type are valid.
   --  If the string is `'enum'` then the variant contains an array
   --  enumerating the possible values. Each item in the array is a possible
   --  valid value and no other values are valid.
   --  If the string is `'flags'` then the variant contains an array. Each
   --  item in the array is a value that may appear zero or one times in an
   --  array to be used as the value for this key. For example, if the variant
   --  contained the array `['x', 'y']` then the valid values for the key would
   --  be `[]`, `['x']`, `['y']`, `['x', 'y']` and `['y', 'x']`.
   --  Finally, if the string is `'range'` then the variant contains a pair of
   --  like-typed values -- the minimum and maximum permissible values for this
   --  key.
   --  This information should not be used by normal programs. It is
   --  considered to be a hint for introspection purposes. Normal programs
   --  should already know what is permitted by their own schema. The format
   --  may change in any way in the future -- but particularly, new forms may
   --  be added to the possibilities described above.
   --  You should free the returned value with Glib.Variant.Unref when it is
   --  no longer needed.
   --  Since: gtk+ 2.40
   --  @return a Glib.Variant.Gvariant describing the range. Has
   --  transfer-ownership='full'.

   function Get_Summary (Self : Gsettings_Schema_Key) return UTF8_String;
   --  Gets the summary for Key.
   --  If no summary has been provided in the schema for Key, returns null.
   --  The summary is a short description of the purpose of the key; usually
   --  one short sentence. Summaries can be translated and the value returned
   --  from this function is is the current locale.
   --  This function is slow. The summary and description information for the
   --  schemas is not stored in the compiled schema database so this function
   --  has to parse all of the source XML files in the schema directory.
   --  Since: gtk+ 2.34
   --  @return the summary for Key, or null

   function Get_Value_Type
      (Self : Gsettings_Schema_Key) return Glib.Variant.Gvariant_Type;
   --  Gets the Glib.Variant.Gvariant_Type of Key.
   --  Since: gtk+ 2.40
   --  @return the type of Key

   function Range_Check
      (Self  : Gsettings_Schema_Key;
       Value : Glib.Variant.Gvariant) return Boolean;
   --  Checks if the given Value is within the permitted range for Key.
   --  It is a programmer error if Value is not of the correct type — you must
   --  check for this first.
   --  Since: gtk+ 2.40
   --  @param Value the value to check
   --  @return True if Value is valid for Key

   function Ref (Self : Gsettings_Schema_Key) return Gsettings_Schema_Key;
   --  Increase the reference count of Key, returning a new reference.
   --  Since: gtk+ 2.40
   --  @return a new reference to Key. Has transfer-ownership='full'.

   procedure Unref (Self : Gsettings_Schema_Key);
   --  Decrease the reference count of Key, possibly freeing it.
   --  Since: gtk+ 2.40

private
   Null_Gsettings_Schema_Key : constant Gsettings_Schema_Key :=
      (Glib.C_Boxed with null record);

end Glib.Settings_Schema_Key;
