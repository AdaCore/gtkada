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

--  Filters files by name or mime type.
--
--  `GtkFileFilter` can be used to restrict the files being shown in a file
--  chooser. Files can be filtered based on their name (with
--  [methodGtk.FileFilter.add_pattern] or [methodGtk.FileFilter.add_suffix]) or
--  on their mime type (with [methodGtk.FileFilter.add_mime_type]).
--
--  Filtering by mime types handles aliasing and subclassing of mime types;
--  e.g. a filter for text/plain also matches a file with mime type
--  application/rtf, since application/rtf is a subclass of text/plain. Note
--  that `GtkFileFilter` allows wildcards for the subtype of a mime type, so
--  you can e.g. filter for image/\*.
--
--  Normally, file filters are used by adding them to a file chooser (see
--  [methodGtk.FileDialog.set_filters]), but it is also possible to manually
--  use a file filter on any [classGtk.FilterListModel] containing `GFileInfo`
--  objects.
--
--  # GtkFileFilter as GtkBuildable
--
--  The `GtkFileFilter` implementation of the `GtkBuildable` interface
--  supports adding rules using the `<mime-types>` and `<patterns>` and
--  `<suffixes>` elements and listing the rules within. Specifying a
--  `<mime-type>` or `<pattern>` or `<suffix>` has the same effect as as
--  calling [methodGtk.FileFilter.add_mime_type] or
--  [methodGtk.FileFilter.add_pattern] or [methodGtk.FileFilter.add_suffix].
--
--  An example of a UI definition fragment specifying `GtkFileFilter` rules:
--  ```xml <object class="GtkFileFilter"> <property name="name"
--  translatable="yes">Text and Images</property> <mime-types>
--  <mime-type>text/plain</mime-type> <mime-type>image/ *</mime-type>
--  </mime-types> <patterns> <pattern>*.txt</pattern> </patterns> <suffixes>
--  <suffix>png</suffix> </suffixes> </object> ```

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;    use GNAT.Strings;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Glib.Variant;    use Glib.Variant;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Filter;      use Gtk.Filter;

package Gtk.File_Filter is

   type Gtk_File_Filter_Record is new Gtk_Filter_Record with null record;
   type Gtk_File_Filter is access all Gtk_File_Filter_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_File_Filter);
   procedure Initialize
      (Self : not null access Gtk_File_Filter_Record'Class);
   --  Creates a new `GtkFileFilter` with no rules added to it.
   --  Such a filter doesn't accept any files, so is not particularly useful
   --  until you add rules with [methodGtk.FileFilter.add_mime_type],
   --  [methodGtk.FileFilter.add_pattern], [methodGtk.FileFilter.add_suffix] or
   --  [methodGtk.FileFilter.add_pixbuf_formats].
   --  To create a filter that accepts any file, use: ```c GtkFileFilter
   --  *filter = gtk_file_filter_new (); gtk_file_filter_add_pattern (filter,
   --  "*"); ```
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_File_Filter_New return Gtk_File_Filter;
   --  Creates a new `GtkFileFilter` with no rules added to it.
   --  Such a filter doesn't accept any files, so is not particularly useful
   --  until you add rules with [methodGtk.FileFilter.add_mime_type],
   --  [methodGtk.FileFilter.add_pattern], [methodGtk.FileFilter.add_suffix] or
   --  [methodGtk.FileFilter.add_pixbuf_formats].
   --  To create a filter that accepts any file, use: ```c GtkFileFilter
   --  *filter = gtk_file_filter_new (); gtk_file_filter_add_pattern (filter,
   --  "*"); ```

   procedure Gtk_New_From_Gvariant
      (Self    : out Gtk_File_Filter;
       Variant : Glib.Variant.Gvariant);
   procedure Initialize_From_Gvariant
      (Self    : not null access Gtk_File_Filter_Record'Class;
       Variant : Glib.Variant.Gvariant);
   --  Deserialize a file filter from a `GVariant`.
   --  The variant must be in the format produced by
   --  [methodGtk.FileFilter.to_gvariant].
   --  Initialize_From_Gvariant does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Variant an `a{sv}` `GVariant`

   function Gtk_File_Filter_New_From_Gvariant
      (Variant : Glib.Variant.Gvariant) return Gtk_File_Filter;
   --  Deserialize a file filter from a `GVariant`.
   --  The variant must be in the format produced by
   --  [methodGtk.FileFilter.to_gvariant].
   --  @param Variant an `a{sv}` `GVariant`

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_file_filter_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Mime_Type
      (Self      : not null access Gtk_File_Filter_Record;
       Mime_Type : UTF8_String);
   --  Adds a rule allowing a given mime type.
   --  @param Mime_Type name of a MIME type

   procedure Add_Mime_Types
      (Self       : not null access Gtk_File_Filter_Record;
       Mime_Types : GNAT.Strings.String_List);
   --  Adds a rule allowing a given array of mime types. It can for example be
   --  used with
   --  [Gly.Loader.get_mime_types](https://gnome.pages.gitlab.gnome.org/glycin/libglycin/type_func.Loader.get_mime_types.html).
   --  This is equivalent to calling [methodGtk.FileFilter.add_mime_type] for
   --  all the supported mime types.
   --  Since: gtk+ 4.22
   --  @param Mime_Types a null-terminated array of mime types

   procedure Add_Pattern
      (Self    : not null access Gtk_File_Filter_Record;
       Pattern : UTF8_String);
   --  Adds a rule allowing a shell style glob pattern.
   --  Note that it depends on the platform whether pattern matching ignores
   --  case or not. On Windows, it does, on other platforms, it doesn't.
   --  @param Pattern a shell style glob pattern

   procedure Add_Pixbuf_Formats
      (Self : not null access Gtk_File_Filter_Record);
   pragma Obsolescent (Add_Pixbuf_Formats);
   --  Adds a rule allowing image files in the formats supported by
   --  `GdkPixbuf`.
   --  This is equivalent to calling [methodGtk.FileFilter.add_mime_type] for
   --  all the supported mime types.
   --  Deprecated since 4.20, 1

   procedure Add_Suffix
      (Self   : not null access Gtk_File_Filter_Record;
       Suffix : UTF8_String);
   --  Adds a suffix match rule to a filter.
   --  This is similar to adding a match for the pattern "*.Suffix"
   --  An exaple to filter files with the suffix ".sub": ```c
   --  gtk_file_filter_add_suffix (filter, "sub"); ```
   --  Filters with multiple dots are allowed.
   --  In contrast to pattern matches, suffix matches are *always*
   --  case-insensitive.
   --  Since: gtk+ 4.4
   --  @param Suffix filename suffix to match

   function Get_Attributes
      (Self : not null access Gtk_File_Filter_Record)
       return GNAT.Strings.String_List;
   --  Gets the attributes that need to be filled in for the `GFileInfo`
   --  passed to this filter.
   --  This function will not typically be used by applications; it is
   --  intended for use in file chooser implementation.
   --  @return the attributes

   function Get_Name
      (Self : not null access Gtk_File_Filter_Record) return UTF8_String;
   --  Gets the human-readable name for the filter.
   --  See [methodGtk.FileFilter.set_name].
   --  @return the human-readable name of the filter

   procedure Set_Name
      (Self : not null access Gtk_File_Filter_Record;
       Name : UTF8_String := "");
   --  Sets a human-readable name of the filter.
   --  This is the string that will be displayed in the user interface if
   --  there is a selectable list of filters.
   --  @param Name the human-readable name for the filter

   function To_Gvariant
      (Self : not null access Gtk_File_Filter_Record)
       return Glib.Variant.Gvariant;
   --  Serialize a file filter to an `a{sv}` variant.
   --  @return a new, floating, `GVariant`

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Name_Property : constant Glib.Properties.Property_String;
   --  The human-readable name of the filter.
   --
   --  This is the string that will be displayed in the user interface if
   --  there is a selectable list of filters.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_File_Filter_Record, Gtk_File_Filter);
   function "+"
     (Widget : access Gtk_File_Filter_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_File_Filter
   renames Implements_Gtk_Buildable.To_Object;

private
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
end Gtk.File_Filter;
