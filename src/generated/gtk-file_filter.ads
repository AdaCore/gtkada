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
--  A GtkFileFilter can be used to restrict the files being shown in a
--  Gtk.File_Chooser.Gtk_File_Chooser. Files can be filtered based on their
--  name (with Gtk.File_Filter.Add_Pattern), on their mime type (with
--  Gtk.File_Filter.Add_Mime_Type), or by a custom filter function (with
--  Gtk.File_Filter.Add_Custom).
--
--  Filtering by mime types handles aliasing and subclassing of mime types;
--  e.g. a filter for text/plain also matches a file with mime type
--  application/rtf, since application/rtf is a subclass of text/plain. Note
--  that Gtk.File_Filter.Gtk_File_Filter allows wildcards for the subtype of a
--  mime type, so you can e.g. filter for image/\*.
--
--  Normally, filters are used by adding them to a
--  Gtk.File_Chooser.Gtk_File_Chooser, see Gtk.File_Chooser.Add_Filter, but it
--  is also possible to manually use a filter on a file with
--  Gtk.File_Filter.Filter.
--
--  # GtkFileFilter as GtkBuildable
--
--  The GtkFileFilter implementation of the GtkBuildable interface supports
--  adding rules using the <mime-types>, <patterns> and <applications> elements
--  and listing the rules within. Specifying a <mime-type> or <pattern> has the
--  same effect as as calling Gtk.File_Filter.Add_Mime_Type or
--  Gtk.File_Filter.Add_Pattern.
--
--  An example of a UI definition fragment specifying GtkFileFilter rules: |[
--  <object class="GtkFileFilter"> <mime-types>
--  <mime-type>text/plain</mime-type> <mime-type>image/ *</mime-type>
--  </mime-types> <patterns> <pattern>*.txt</pattern> <pattern>*.png</pattern>
--  </patterns> </object> ]|
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Types;              use Glib.Types;
with Glib.Variant;            use Glib.Variant;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtkada.Types;            use Gtkada.Types;

package Gtk.File_Filter is

   type Gtk_File_Filter_Record is new GObject_Record with null record;
   type Gtk_File_Filter is access all Gtk_File_Filter_Record'Class;

   type Gtk_File_Filter_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_File_Filter_Flags);
   --  These flags indicate what parts of a
   --  Gtk.File_Filter.Gtk_File_Filter_Info struct are filled or need to be
   --  filled.

   File_Filter_Filename : constant Gtk_File_Filter_Flags := 1;
   File_Filter_Uri : constant Gtk_File_Filter_Flags := 2;
   File_Filter_Display_Name : constant Gtk_File_Filter_Flags := 4;
   File_Filter_Mime_Type : constant Gtk_File_Filter_Flags := 8;

   type Gtk_File_Filter_Info is record
      Contains : Gtk_File_Filter_Flags;
      Filename : Gtkada.Types.Chars_Ptr;
      URI : Gtkada.Types.Chars_Ptr;
      Display_Name : Gtkada.Types.Chars_Ptr;
      Mime_Type : Gtkada.Types.Chars_Ptr;
   end record;
   pragma Convention (C, Gtk_File_Filter_Info);

   function From_Object_Free (B : access Gtk_File_Filter_Info) return Gtk_File_Filter_Info;
   pragma Inline (From_Object_Free);
   --  A Gtk.File_Filter.Gtk_File_Filter_Info-struct is used to pass
   --  information about the tested file to Gtk.File_Filter.Filter.

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_File_Filter_Func is access function (Filter_Info : Gtk_File_Filter_Info) return Boolean;
   --  The type of function that is used with custom filters, see
   --  Gtk.File_Filter.Add_Custom.
   --  "filter_info": a Gtk.File_Filter.Gtk_File_Filter_Info that is filled
   --  according to the Needed flags passed to Gtk.File_Filter.Add_Custom

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_File_Filter_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_File_Filter_Flags);
   type Property_Gtk_File_Filter_Flags is new Gtk_File_Filter_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_File_Filter);
   procedure Initialize
      (Self : not null access Gtk_File_Filter_Record'Class);
   --  Creates a new Gtk.File_Filter.Gtk_File_Filter with no rules added to
   --  it. Such a filter doesn't accept any files, so is not particularly
   --  useful until you add rules with Gtk.File_Filter.Add_Mime_Type,
   --  Gtk.File_Filter.Add_Pattern, or Gtk.File_Filter.Add_Custom. To create a
   --  filter that accepts any file, use: |[<!-- language="C" --> GtkFileFilter
   --  *filter = gtk_file_filter_new (); gtk_file_filter_add_pattern (filter,
   --  "*"); ]|
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_File_Filter_New return Gtk_File_Filter;
   --  Creates a new Gtk.File_Filter.Gtk_File_Filter with no rules added to
   --  it. Such a filter doesn't accept any files, so is not particularly
   --  useful until you add rules with Gtk.File_Filter.Add_Mime_Type,
   --  Gtk.File_Filter.Add_Pattern, or Gtk.File_Filter.Add_Custom. To create a
   --  filter that accepts any file, use: |[<!-- language="C" --> GtkFileFilter
   --  *filter = gtk_file_filter_new (); gtk_file_filter_add_pattern (filter,
   --  "*"); ]|
   --  Since: gtk+ 2.4

   procedure Gtk_New_From_Gvariant
      (Self    : out Gtk_File_Filter;
       Variant : Glib.Variant.Gvariant);
   procedure Initialize_From_Gvariant
      (Self    : not null access Gtk_File_Filter_Record'Class;
       Variant : Glib.Variant.Gvariant);
   --  Deserialize a file filter from an a{sv} variant in the format produced
   --  by Gtk.File_Filter.To_Gvariant.
   --  Since: gtk+ 3.22
   --  Initialize_From_Gvariant does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "variant": an a{sv} Glib.Variant.Gvariant

   function Gtk_File_Filter_New_From_Gvariant
      (Variant : Glib.Variant.Gvariant) return Gtk_File_Filter;
   --  Deserialize a file filter from an a{sv} variant in the format produced
   --  by Gtk.File_Filter.To_Gvariant.
   --  Since: gtk+ 3.22
   --  "variant": an a{sv} Glib.Variant.Gvariant

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_file_filter_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Custom
      (Self   : not null access Gtk_File_Filter_Record;
       Needed : Gtk_File_Filter_Flags;
       Func   : Gtk_File_Filter_Func);
   --  Adds rule to a filter that allows files based on a custom callback
   --  function. The bitfield Needed which is passed in provides information
   --  about what sorts of information that the filter function needs; this
   --  allows GTK+ to avoid retrieving expensive information when it isn't
   --  needed by the filter.
   --  Since: gtk+ 2.4
   --  "needed": bitfield of flags indicating the information that the custom
   --  filter function needs.
   --  "func": callback function; if the function returns True, then the file
   --  will be displayed.

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Add_Custom_User_Data is

      type Gtk_File_Filter_Func is access function
        (Filter_Info : Gtk.File_Filter.Gtk_File_Filter_Info;
         Data        : User_Data_Type) return Boolean;
      --  The type of function that is used with custom filters, see
      --  Gtk.File_Filter.Add_Custom.
      --  "filter_info": a Gtk.File_Filter.Gtk_File_Filter_Info that is filled
      --  according to the Needed flags passed to Gtk.File_Filter.Add_Custom
      --  "data": user data passed to Gtk.File_Filter.Add_Custom

      procedure Add_Custom
         (Self   : not null access Gtk.File_Filter.Gtk_File_Filter_Record'Class;
          Needed : Gtk.File_Filter.Gtk_File_Filter_Flags;
          Func   : Gtk_File_Filter_Func;
          Data   : User_Data_Type);
      --  Adds rule to a filter that allows files based on a custom callback
      --  function. The bitfield Needed which is passed in provides information
      --  about what sorts of information that the filter function needs; this
      --  allows GTK+ to avoid retrieving expensive information when it isn't
      --  needed by the filter.
      --  Since: gtk+ 2.4
      --  "needed": bitfield of flags indicating the information that the
      --  custom filter function needs.
      --  "func": callback function; if the function returns True, then the
      --  file will be displayed.
      --  "data": data to pass to Func

   end Add_Custom_User_Data;

   procedure Add_Mime_Type
      (Self      : not null access Gtk_File_Filter_Record;
       Mime_Type : UTF8_String);
   --  Adds a rule allowing a given mime type to Filter.
   --  Since: gtk+ 2.4
   --  "mime_type": name of a MIME type

   procedure Add_Pattern
      (Self    : not null access Gtk_File_Filter_Record;
       Pattern : UTF8_String);
   --  Adds a rule allowing a shell style glob to a filter.
   --  Since: gtk+ 2.4
   --  "pattern": a shell style glob

   procedure Add_Pixbuf_Formats
      (Self : not null access Gtk_File_Filter_Record);
   --  Adds a rule allowing image files in the formats supported by GdkPixbuf.
   --  Since: gtk+ 2.6

   function Filter
      (Self        : not null access Gtk_File_Filter_Record;
       Filter_Info : Gtk_File_Filter_Info) return Boolean;
   --  Tests whether a file should be displayed according to Filter. The
   --  Gtk.File_Filter.Gtk_File_Filter_Info Filter_Info should include the
   --  fields returned from Gtk.File_Filter.Get_Needed.
   --  This function will not typically be used by applications; it is
   --  intended principally for use in the implementation of
   --  Gtk.File_Chooser.Gtk_File_Chooser.
   --  Since: gtk+ 2.4
   --  "filter_info": a Gtk.File_Filter.Gtk_File_Filter_Info containing
   --  information about a file.

   function Get_Name
      (Self : not null access Gtk_File_Filter_Record) return UTF8_String;
   --  Gets the human-readable name for the filter. See
   --  Gtk.File_Filter.Set_Name.
   --  Since: gtk+ 2.4

   procedure Set_Name
      (Self : not null access Gtk_File_Filter_Record;
       Name : UTF8_String := "");
   --  Sets the human-readable name of the filter; this is the string that
   --  will be displayed in the file selector user interface if there is a
   --  selectable list of filters.
   --  Since: gtk+ 2.4
   --  "name": the human-readable-name for the filter, or null to remove any
   --  existing name.

   function Get_Needed
      (Self : not null access Gtk_File_Filter_Record)
       return Gtk_File_Filter_Flags;
   --  Gets the fields that need to be filled in for the
   --  Gtk.File_Filter.Gtk_File_Filter_Info passed to Gtk.File_Filter.Filter
   --  This function will not typically be used by applications; it is
   --  intended principally for use in the implementation of
   --  Gtk.File_Chooser.Gtk_File_Chooser.
   --  Since: gtk+ 2.4

   function To_Gvariant
      (Self : not null access Gtk_File_Filter_Record)
       return Glib.Variant.Gvariant;
   --  Serialize a file filter to an a{sv} variant.
   --  Since: gtk+ 3.22

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

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

end Gtk.File_Filter;
