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

--  A `GdkPixbufFormat` contains information about the image format accepted
--  by a module.
--
--  Only modules should access the fields directly, applications should use
--  the `gdk_pixbuf_format_*` family of functions.

pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with GNAT.Strings;             use GNAT.Strings;
with Glib;                     use Glib;
with Glib.GSlist;
with Gtkada.Types;             use Gtkada.Types;
with System;

package Gdk.Pixbuf_Format is

   type Gdk_Pixbuf_Module_Pattern is record
      Prefix : Gtkada.Types.Chars_Ptr;
      Mask : Gtkada.Types.Chars_Ptr;
      Relevance : Glib.Gint;
   end record;
   pragma Convention (C, Gdk_Pixbuf_Module_Pattern);
   --  The signature prefix for a module.
   --
   --  The signature of a module is a set of prefixes. Prefixes are encoded as
   --  pairs of ordinary strings, where the second string, called the mask, if
   --  not `NULL`, must be of the same length as the first one and may contain
   --  ' ', '!', 'x', 'z', and 'n' to indicate bytes that must be matched, not
   --  matched, "don't-care"-bytes, zeros and non-zeros, respectively.
   --
   --  Each prefix has an associated integer that describes the relevance of
   --  the prefix, with 0 meaning a mismatch and 100 a "perfect match".
   --
   --  Starting with gdk-pixbuf 2.8, the first byte of the mask may be '*',
   --  indicating an unanchored pattern that matches not only at the beginning,
   --  but also in the middle. Versions prior to 2.8 will interpret the '*'
   --  like an 'x'.
   --
   --  The signature of a module is stored as an array of
   --  `GdkPixbufModulePatterns`. The array is terminated by a pattern where
   --  the `prefix` is `NULL`.
   --
   --  ```c GdkPixbufModulePattern *signature[] = { { "abcdx", " !x z", 100 },
   --  { "bla", NULL, 90 }, { NULL, NULL, 0 } }; ```
   --
   --  In the example above, the signature matches e.g. "auud\0" with
   --  relevance 100, and "blau" with relevance 90.

   type Gdk_Pixbuf_Format is record
      Name : Gtkada.Types.Chars_Ptr;
      Signature : System.Address := System.Null_Address;
      Domain : Gtkada.Types.Chars_Ptr;
      Description : Gtkada.Types.Chars_Ptr;
      Mime_Types : Gtkada.Types.char_array_access;
      Extensions : Gtkada.Types.char_array_access;
      Flags : Guint32;
      Disabled : Glib.Gboolean;
      License : Gtkada.Types.Chars_Ptr;
   end record;
   pragma Convention (C, Gdk_Pixbuf_Format);
   --  A `GdkPixbufFormat` contains information about the image format
   --  accepted by a module.
   --
   --  Only modules should access the fields directly, applications should use
   --  the `gdk_pixbuf_format_*` family of functions.

   type Gdk_Pixbuf_Format_Access is access all Gdk_Pixbuf_Format;
   pragma Convention (C, Gdk_Pixbuf_Format_Access);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_pixbuf_format_get_type");

   -------------
   -- Methods --
   -------------

   function Copy
      (Self : Gdk_Pixbuf_Format_Access) return Gdk_Pixbuf_Format_Access;
   pragma Import (C, Copy, "gdk_pixbuf_format_copy");
   --  Creates a copy of `format`.
   --  Since: gtk+ 2.22
   --  @return the newly allocated copy of a `GdkPixbufFormat`. Use
   --  Gdk.Pixbuf_Format.Free to free the resources when done

   procedure Free (Self : Gdk_Pixbuf_Format_Access);
   pragma Import (C, Free, "gdk_pixbuf_format_free");
   --  Frees the resources allocated when copying a `GdkPixbufFormat` using
   --  Gdk.Pixbuf_Format.Copy
   --  Since: gtk+ 2.22

   function Get_Description
      (Self : Gdk_Pixbuf_Format_Access) return UTF8_String;
   --  Returns a description of the format.
   --  Since: gtk+ 2.2
   --  @return a description of the format.

   function Get_Extensions
      (Self : Gdk_Pixbuf_Format_Access) return GNAT.Strings.String_List;
   --  Returns the filename extensions typically used for files in the given
   --  format.
   --  Since: gtk+ 2.2
   --  @return an array of filename extensions

   function Get_License (Self : Gdk_Pixbuf_Format_Access) return UTF8_String;
   --  Returns information about the license of the image loader for the
   --  format.
   --  The returned string should be a shorthand for a well known license,
   --  e.g. "LGPL", "GPL", "QPL", "GPL/QPL", or "other" to indicate some other
   --  license.
   --  Since: gtk+ 2.6
   --  @return a string describing the license of the pixbuf format

   function Get_Mime_Types
      (Self : Gdk_Pixbuf_Format_Access) return GNAT.Strings.String_List;
   --  Returns the mime types supported by the format.
   --  Since: gtk+ 2.2
   --  @return an array of mime types

   function Get_Name (Self : Gdk_Pixbuf_Format_Access) return UTF8_String;
   --  Returns the name of the format.
   --  Since: gtk+ 2.2
   --  @return the name of the format.

   function Is_Disabled (Self : Gdk_Pixbuf_Format_Access) return Boolean;
   --  Returns whether this image format is disabled.
   --  See Gdk.Pixbuf_Format.Set_Disabled.
   --  Since: gtk+ 2.6
   --  @return whether this image format is disabled.

   function Is_Save_Option_Supported
      (Self       : Gdk_Pixbuf_Format_Access;
       Option_Key : UTF8_String) return Boolean;
   --  Returns `TRUE` if the save option specified by Option_Key is supported
   --  when saving a pixbuf using the module implementing Format.
   --  See gdk_pixbuf_save for more information about option keys.
   --  Since: gtk+ 2.36
   --  @param Option_Key the name of an option
   --  @return `TRUE` if the specified option is supported

   function Is_Scalable (Self : Gdk_Pixbuf_Format_Access) return Boolean;
   --  Returns whether this image format is scalable.
   --  If a file is in a scalable format, it is preferable to load it at the
   --  desired size, rather than loading it at the default size and scaling the
   --  resulting pixbuf to the desired size.
   --  Since: gtk+ 2.6
   --  @return whether this image format is scalable.

   function Is_Writable (Self : Gdk_Pixbuf_Format_Access) return Boolean;
   --  Returns whether pixbufs can be saved in the given format.
   --  Since: gtk+ 2.2
   --  @return whether pixbufs can be saved in the given format.

   procedure Set_Disabled
      (Self     : Gdk_Pixbuf_Format_Access;
       Disabled : Boolean);
   --  Disables or enables an image format.
   --  If a format is disabled, GdkPixbuf won't use the image loader for this
   --  format to load images.
   --  Applications can use this to avoid using image loaders with an
   --  inappropriate license, see Gdk.Pixbuf_Format.Get_License.
   --  Since: gtk+ 2.6
   --  @param Disabled `TRUE` to disable the format Format

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Gdk_Pixbuf_Module_Pattern) return Gdk_Pixbuf_Module_Pattern;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   function From_Object_Free
     (B : not null access Gdk_Pixbuf_Format) return Gdk_Pixbuf_Format;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   function Convert is new Ada.Unchecked_Conversion
     (Gdk_Pixbuf_Format_Access, System.Address);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Gdk_Pixbuf_Format_Access);
   package Format_List is new Glib.GSlist.Generic_SList
     (Gdk_Pixbuf_Format_Access);

end Gdk.Pixbuf_Format;
