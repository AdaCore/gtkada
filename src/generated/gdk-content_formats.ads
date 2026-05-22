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

--  Used to advertise and negotiate the format of content.
--
--  You will encounter `GdkContentFormats` when interacting with objects
--  controlling operations that pass data between different widgets, window or
--  application, like [classGdk.Drag], [classGdk.Drop], [classGdk.Clipboard] or
--  [classGdk.ContentProvider].
--
--  GDK supports content in 2 forms: `GType` and mime type. Using `GTypes` is
--  meant only for in-process content transfers. Mime types are meant to be
--  used for data passing both in-process and out-of-process. The details of
--  how data is passed is described in the documentation of the actual
--  implementations. To transform between the two forms,
--  [classGdk.ContentSerializer] and [classGdk.ContentDeserializer] are used.
--
--  A `GdkContentFormats` describes a set of possible formats content can be
--  exchanged in. It is assumed that this set is ordered. `GTypes` are more
--  important than mime types. Order between different `GTypes` or mime types
--  is the order they were added in, most important first. Functions that care
--  about order, such as [methodGdk.ContentFormats.union], will describe in
--  their documentation how they interpret that order, though in general the
--  order of the first argument is considered the primary order of the result,
--  followed by the order of further arguments.
--
--  For debugging purposes, the function [methodGdk.ContentFormats.to_string]
--  exists. It will print a comma-separated list of formats from most important
--  to least important.
--
--  `GdkContentFormats` is an immutable struct. After creation, you cannot
--  change the types it represents. Instead, new `GdkContentFormats` have to be
--  created. The [structGdk.ContentFormatsBuilder] structure is meant to help
--  in this endeavor.

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings; use GNAT.Strings;
with Glib;         use Glib;
with Glib.String;  use Glib.String;
with System;

package Gdk.Content_Formats is

   type Gdk_Content_Formats is new Glib.C_Boxed with null record;
   Null_Gdk_Content_Formats : constant Gdk_Content_Formats;

   function From_Object (Object : System.Address) return Gdk_Content_Formats;
   function From_Object_Free (B : access Gdk_Content_Formats'Class) return Gdk_Content_Formats;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New
      (Self         : out Gdk_Content_Formats;
       Mime_Types   : GNAT.Strings.String_List;
       N_Mime_Types : Guint);
   --  Creates a new `GdkContentFormats` from an array of mime types.
   --  The mime types must be valid and different from each other or the
   --  behavior of the return value is undefined. If you cannot guarantee this,
   --  use [structGdk.ContentFormatsBuilder] instead.
   --  @param Mime_Types Pointer to an array of mime types
   --  @param N_Mime_Types number of entries in Mime_Types.

   function Gdk_Content_Formats_New
      (Mime_Types   : GNAT.Strings.String_List;
       N_Mime_Types : Guint) return Gdk_Content_Formats;
   --  Creates a new `GdkContentFormats` from an array of mime types.
   --  The mime types must be valid and different from each other or the
   --  behavior of the return value is undefined. If you cannot guarantee this,
   --  use [structGdk.ContentFormatsBuilder] instead.
   --  @param Mime_Types Pointer to an array of mime types
   --  @param N_Mime_Types number of entries in Mime_Types.

   procedure Gdk_New_For_Gtype
      (Self     : out Gdk_Content_Formats;
       The_Type : GType);
   --  Creates a new `GdkContentFormats` for a given `GType`.
   --  @param The_Type a `GType`

   function Gdk_Content_Formats_New_For_Gtype
      (The_Type : GType) return Gdk_Content_Formats;
   --  Creates a new `GdkContentFormats` for a given `GType`.
   --  @param The_Type a `GType`

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_content_formats_get_type");

   -------------
   -- Methods --
   -------------

   function Contain_Gtype
      (Self     : Gdk_Content_Formats;
       The_Type : GType) return Boolean;
   --  Checks if a given `GType` is part of the given Formats.
   --  @param The_Type the `GType` to search for
   --  @return True if the `GType` was found

   function Contain_Mime_Type
      (Self      : Gdk_Content_Formats;
       Mime_Type : UTF8_String) return Boolean;
   --  Checks if a given mime type is part of the given Formats.
   --  @param Mime_Type the mime type to search for
   --  @return True if the mime_type was found

   function Get_Mime_Types
      (Self         : Gdk_Content_Formats;
       N_Mime_Types : access Gsize) return GNAT.Strings.String_List;
   --  Gets the mime types included in Formats.
   --  Note that Formats may not contain any mime types, in particular when
   --  they are empty. In that case null will be returned.
   --  @param N_Mime_Types optional pointer to take the number of mime types
   --  contained in the return value
   --  @return null-terminated array of interned strings of mime types
   --  included in Formats

   function Is_Empty (Self : Gdk_Content_Formats) return Boolean;
   --  Returns whether the content formats contain any formats.
   --  Since: gtk+ 4.18
   --  @return true if Formats contains no mime types and no GTypes

   function Match
      (Self   : Gdk_Content_Formats;
       Second : Gdk_Content_Formats) return Boolean;
   --  Checks if First and Second have any matching formats.
   --  @param Second the `GdkContentFormats` to intersect with
   --  @return True if a matching format was found.

   function Match_Gtype
      (Self   : Gdk_Content_Formats;
       Second : Gdk_Content_Formats) return GType;
   --  Finds the first `GType` from First that is also contained in Second.
   --  If no matching `GType` is found, G_TYPE_INVALID is returned.
   --  @param Second the `GdkContentFormats` to intersect with
   --  @return The first common `GType` or G_TYPE_INVALID if none.

   function Match_Mime_Type
      (Self   : Gdk_Content_Formats;
       Second : Gdk_Content_Formats) return UTF8_String;
   --  Finds the first mime type from First that is also contained in Second.
   --  If no matching mime type is found, null is returned.
   --  @param Second the `GdkContentFormats` to intersect with
   --  @return The first common mime type or null if none

   procedure Print
      (Self   : Gdk_Content_Formats;
       String : Glib.String.Gstring);
   --  Prints the given Formats into a string for human consumption.
   --  The result of this function can later be parsed with
   --  [funcGdk.ContentFormats.parse].
   --  @param String a `GString` to print into

   function Ref (Self : Gdk_Content_Formats) return Gdk_Content_Formats;
   --  Increases the reference count of a `GdkContentFormats` by one.
   --  @return the passed in `GdkContentFormats`.

   function To_String (Self : Gdk_Content_Formats) return UTF8_String;
   --  Prints the given Formats into a human-readable string.
   --  The resulting string can be parsed with [funcGdk.ContentFormats.parse].
   --  This is a small wrapper around [methodGdk.ContentFormats.print] to help
   --  when debugging.
   --  @return a new string

   function Union
      (Self   : Gdk_Content_Formats;
       Second : Gdk_Content_Formats) return Gdk_Content_Formats;
   --  Append all missing types from Second to First, in the order they had in
   --  Second.
   --  @param Second the `GdkContentFormats` to merge from
   --  @return a new `GdkContentFormats`

   function Union_Deserialize_Gtypes
      (Self : Gdk_Content_Formats) return Gdk_Content_Formats;
   --  Add GTypes for mime types in Formats for which deserializers are
   --  registered.
   --  @return a new `GdkContentFormats`

   function Union_Deserialize_Mime_Types
      (Self : Gdk_Content_Formats) return Gdk_Content_Formats;
   --  Add mime types for GTypes in Formats for which deserializers are
   --  registered.
   --  @return a new `GdkContentFormats`

   function Union_Serialize_Gtypes
      (Self : Gdk_Content_Formats) return Gdk_Content_Formats;
   --  Add GTypes for the mime types in Formats for which serializers are
   --  registered.
   --  @return a new `GdkContentFormats`

   function Union_Serialize_Mime_Types
      (Self : Gdk_Content_Formats) return Gdk_Content_Formats;
   --  Add mime types for GTypes in Formats for which serializers are
   --  registered.
   --  @return a new `GdkContentFormats`

   procedure Unref (Self : Gdk_Content_Formats);
   --  Decreases the reference count of a `GdkContentFormats` by one.
   --  If the resulting reference count is zero, frees the formats.

   ---------------
   -- Functions --
   ---------------

   function Parse (String : UTF8_String) return Gdk_Content_Formats;
   --  Parses the given String into `GdkContentFormats` and returns the
   --  formats.
   --  Strings printed via [methodGdk.ContentFormats.to_string] can be read in
   --  again successfully using this function.
   --  If String does not describe valid content formats, null is returned.
   --  Since: gtk+ 4.4
   --  @param String the string to parse
   --  @return the content formats if String is valid

private

   Null_Gdk_Content_Formats : constant Gdk_Content_Formats := (Glib.C_Boxed with null record);

end Gdk.Content_Formats;
