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

--  Defines a part of a CSS document.
--
--  Because sections are nested into one another, you can use
--  [methodCsssection.get_parent] to get the containing region.

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Bytes;  use Glib.Bytes;
with Glib.GFile;  use Glib.GFile;
with Glib.String; use Glib.String;

package Gtk.Css_Section is

   type Gtk_Css_Section is new Glib.C_Boxed with null record;
   Null_Gtk_Css_Section : constant Gtk_Css_Section;

   function From_Object (Object : System.Address) return Gtk_Css_Section;
   function From_Object_Free (B : access Gtk_Css_Section'Class) return Gtk_Css_Section;
   pragma Inline (From_Object_Free, From_Object);

   type Gtk_Css_Location is record
      Bytes : Gsize;
      Chars : Gsize;
      Lines : Gsize;
      Line_Bytes : Gsize;
      Line_Chars : Gsize;
   end record;
   pragma Convention (C, Gtk_Css_Location);
   --  Points at a location inside a CSS stream.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self    : out Gtk_Css_Section;
       File    : Glib.GFile.Gfile;
       Start   : Gtk_Css_Location;
       The_End : Gtk_Css_Location);
   --  Creates a new `GtkCssSection` referring to the section in the given
   --  `file` from the `start` location to the `end` location.
   --  @param File The file this section refers to
   --  @param Start The start location
   --  @param The_End The end location

   function Gtk_Css_Section_New
      (File    : Glib.GFile.Gfile;
       Start   : Gtk_Css_Location;
       The_End : Gtk_Css_Location) return Gtk_Css_Section;
   --  Creates a new `GtkCssSection` referring to the section in the given
   --  `file` from the `start` location to the `end` location.
   --  @param File The file this section refers to
   --  @param Start The start location
   --  @param The_End The end location

   procedure Gtk_New_With_Bytes
      (Self    : out Gtk_Css_Section;
       File    : Glib.GFile.Gfile;
       Bytes   : Glib.Bytes.Gbytes;
       Start   : Gtk_Css_Location;
       The_End : Gtk_Css_Location);
   --  Creates a new `GtkCssSection` referring to the section in the given
   --  `file` or the given `bytes` from the `start` location to the `end`
   --  location.
   --  Since: gtk+ 4.16
   --  @param File The file this section refers to
   --  @param Bytes The bytes this sections refers to
   --  @param Start The start location
   --  @param The_End The end location

   function Gtk_Css_Section_New_With_Bytes
      (File    : Glib.GFile.Gfile;
       Bytes   : Glib.Bytes.Gbytes;
       Start   : Gtk_Css_Location;
       The_End : Gtk_Css_Location) return Gtk_Css_Section;
   --  Creates a new `GtkCssSection` referring to the section in the given
   --  `file` or the given `bytes` from the `start` location to the `end`
   --  location.
   --  Since: gtk+ 4.16
   --  @param File The file this section refers to
   --  @param Bytes The bytes this sections refers to
   --  @param Start The start location
   --  @param The_End The end location

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_css_section_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Bytes (Self : Gtk_Css_Section) return Glib.Bytes.Gbytes;
   --  Gets the bytes that Section was parsed from.
   --  Since: gtk+ 4.16
   --  @return the `GBytes` from which the `section` was parsed. Has
   --  transfer-ownership='none'.

   function Get_End_Location
      (Self : Gtk_Css_Section) return Gtk_Css_Location;
   --  Returns the location in the CSS document where this section ends.
   --  @return The end location of this section

   function Get_File (Self : Gtk_Css_Section) return Glib.GFile.Gfile;
   --  Gets the file that Section was parsed from.
   --  If no such file exists, for example because the CSS was loaded via
   --  [methodGtk.CssProvider.load_from_data], then `NULL` is returned.
   --  @return the `GFile` from which the `section` was parsed

   function Get_Parent (Self : Gtk_Css_Section) return Gtk_Css_Section;
   --  Gets the parent section for the given `section`.
   --  The parent section is the section that contains this `section`. A
   --  special case are sections of type `GTK_CSS_SECTION_DOCUMENT`. Their
   --  parent will either be `NULL` if they are the original CSS document that
   --  was loaded by [methodGtk.CssProvider.load_from_file] or a section of
   --  type `GTK_CSS_SECTION_IMPORT` if it was loaded with an `Import` rule
   --  from a different file.
   --  @return the parent section. Has transfer-ownership='none'.

   function Get_Start_Location
      (Self : Gtk_Css_Section) return Gtk_Css_Location;
   --  Returns the location in the CSS document where this section starts.
   --  @return The start location of this section

   procedure Print (Self : Gtk_Css_Section; String : Glib.String.Gstring);
   --  Prints the `section` into `string` in a human-readable form.
   --  This is a form like `gtk.css:32:1-23` to denote line 32, characters 1
   --  to 23 in the file `gtk.css`.
   --  @param String a `GString` to print to

   function Ref (Self : Gtk_Css_Section) return Gtk_Css_Section;
   --  Increments the reference count on `section`.
   --  @return the CSS section itself. Has transfer-ownership='full'.

   function To_String (Self : Gtk_Css_Section) return UTF8_String;
   --  Prints the section into a human-readable text form using
   --  [methodGtk.CssSection.print].
   --  @return A new string.

   procedure Unref (Self : Gtk_Css_Section);
   --  Decrements the reference count on `section`, freeing the structure if
   --  the reference count reaches 0.
   --  @param Self has transfer-ownership='full'.

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Gtk_Css_Location) return Gtk_Css_Location;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

private
   Null_Gtk_Css_Section : constant Gtk_Css_Section :=
      (Glib.C_Boxed with null record);

end Gtk.Css_Section;
