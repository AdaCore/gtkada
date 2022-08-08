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
--  A Gtk.Recent_Filter.Gtk_Recent_Filter can be used to restrict the files
--  being shown in a Gtk.Recent_Chooser.Gtk_Recent_Chooser. Files can be
--  filtered based on their name (with Gtk.Recent_Filter.Add_Pattern), on their
--  mime type (with Gtk.File_Filter.Add_Mime_Type), on the application that has
--  registered them (with Gtk.Recent_Filter.Add_Application), or by a custom
--  filter function (with Gtk.Recent_Filter.Add_Custom).
--
--  Filtering by mime type handles aliasing and subclassing of mime types;
--  e.g. a filter for text/plain also matches a file with mime type
--  application/rtf, since application/rtf is a subclass of text/plain. Note
--  that Gtk.Recent_Filter.Gtk_Recent_Filter allows wildcards for the subtype
--  of a mime type, so you can e.g. filter for image/\*.
--
--  Normally, filters are used by adding them to a
--  Gtk.Recent_Chooser.Gtk_Recent_Chooser, see Gtk.Recent_Chooser.Add_Filter,
--  but it is also possible to manually use a filter on a file with
--  Gtk.Recent_Filter.Filter.
--
--  Recently used files are supported since GTK+ 2.10.
--
--  ## GtkRecentFilter as GtkBuildable
--
--  The GtkRecentFilter implementation of the GtkBuildable interface supports
--  adding rules using the <mime-types>, <patterns> and <applications> elements
--  and listing the rules within. Specifying a <mime-type>, <pattern> or
--  <application> has the same effect as calling
--  Gtk.Recent_Filter.Add_Mime_Type, Gtk.Recent_Filter.Add_Pattern or
--  Gtk.Recent_Filter.Add_Application.
--
--  An example of a UI definition fragment specifying GtkRecentFilter rules:
--  |[ <object class="GtkRecentFilter"> <mime-types>
--  <mime-type>text/plain</mime-type> <mime-type>image/png</mime-type>
--  </mime-types> <patterns> <pattern>*.txt</pattern> <pattern>*.png</pattern>
--  </patterns> <applications> <application>gimp</application>
--  <application>gedit</application> <application>glade</application>
--  </applications> </object> ]|
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.GSlist;             use Glib.GSlist;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Types;              use Glib.Types;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtkada.Types;            use Gtkada.Types;

package Gtk.Recent_Filter is

   type Gtk_Recent_Filter_Record is new GObject_Record with null record;
   type Gtk_Recent_Filter is access all Gtk_Recent_Filter_Record'Class;

   type Gtk_Recent_Filter_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Recent_Filter_Flags);
   --  These flags indicate what parts of a
   --  Gtk.Recent_Filter.Gtk_Recent_Filter_Info struct are filled or need to be
   --  filled.

   Recent_Filter_Uri : constant Gtk_Recent_Filter_Flags := 1;
   Recent_Filter_Display_Name : constant Gtk_Recent_Filter_Flags := 2;
   Recent_Filter_Mime_Type : constant Gtk_Recent_Filter_Flags := 4;
   Recent_Filter_Application : constant Gtk_Recent_Filter_Flags := 8;
   Recent_Filter_Group : constant Gtk_Recent_Filter_Flags := 16;
   Recent_Filter_Age : constant Gtk_Recent_Filter_Flags := 32;

   type Gtk_Recent_Filter_Info is record
      Contains : Gtk_Recent_Filter_Flags;
      URI : Gtkada.Types.Chars_Ptr;
      Display_Name : Gtkada.Types.Chars_Ptr;
      Mime_Type : Gtkada.Types.Chars_Ptr;
      Applications : Gtkada.Types.char_array_access;
      Groups : Gtkada.Types.char_array_access;
      Age : Glib.Gint := 0;
   end record;
   pragma Convention (C, Gtk_Recent_Filter_Info);

   function From_Object_Free (B : access Gtk_Recent_Filter_Info) return Gtk_Recent_Filter_Info;
   pragma Inline (From_Object_Free);
   --  A GtkRecentFilterInfo struct is used to pass information about the
   --  tested file to Gtk.Recent_Filter.Filter.

   function Convert (R : Gtk.Recent_Filter.Gtk_Recent_Filter) return System.Address;
   function Convert (R : System.Address) return Gtk.Recent_Filter.Gtk_Recent_Filter;
   package Gtk_Recent_Filter_List is new Generic_SList (Gtk.Recent_Filter.Gtk_Recent_Filter);

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Recent_Filter_Func is access function (Filter_Info : Gtk_Recent_Filter_Info) return Boolean;
   --  The type of function that is used with custom filters, see
   --  Gtk.Recent_Filter.Add_Custom.
   --  "filter_info": a Gtk.Recent_Filter.Gtk_Recent_Filter_Info that is
   --  filled according to the Needed flags passed to
   --  Gtk.Recent_Filter.Add_Custom

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Recent_Filter_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Recent_Filter_Flags);
   type Property_Gtk_Recent_Filter_Flags is new Gtk_Recent_Filter_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Filter : out Gtk_Recent_Filter);
   procedure Initialize
      (Filter : not null access Gtk_Recent_Filter_Record'Class);
   --  Creates a new Gtk.Recent_Filter.Gtk_Recent_Filter with no rules added
   --  to it. Such filter does not accept any recently used resources, so is
   --  not particularly useful until you add rules with
   --  Gtk.Recent_Filter.Add_Pattern, Gtk.Recent_Filter.Add_Mime_Type,
   --  Gtk.Recent_Filter.Add_Application, Gtk.Recent_Filter.Add_Age. To create
   --  a filter that accepts any recently used resource, use: |[<!--
   --  language="C" --> GtkRecentFilter *filter = gtk_recent_filter_new ();
   --  gtk_recent_filter_add_pattern (filter, "*"); ]|
   --  Since: gtk+ 2.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Recent_Filter_New return Gtk_Recent_Filter;
   --  Creates a new Gtk.Recent_Filter.Gtk_Recent_Filter with no rules added
   --  to it. Such filter does not accept any recently used resources, so is
   --  not particularly useful until you add rules with
   --  Gtk.Recent_Filter.Add_Pattern, Gtk.Recent_Filter.Add_Mime_Type,
   --  Gtk.Recent_Filter.Add_Application, Gtk.Recent_Filter.Add_Age. To create
   --  a filter that accepts any recently used resource, use: |[<!--
   --  language="C" --> GtkRecentFilter *filter = gtk_recent_filter_new ();
   --  gtk_recent_filter_add_pattern (filter, "*"); ]|
   --  Since: gtk+ 2.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_recent_filter_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Age
      (Filter : not null access Gtk_Recent_Filter_Record;
       Days   : Glib.Gint);
   --  Adds a rule that allows resources based on their age - that is, the
   --  number of days elapsed since they were last modified.
   --  Since: gtk+ 2.10
   --  "days": number of days

   procedure Add_Application
      (Filter      : not null access Gtk_Recent_Filter_Record;
       Application : UTF8_String);
   --  Adds a rule that allows resources based on the name of the application
   --  that has registered them.
   --  Since: gtk+ 2.10
   --  "application": an application name

   procedure Add_Custom
      (Filter       : not null access Gtk_Recent_Filter_Record;
       Needed       : Gtk_Recent_Filter_Flags;
       Func         : Gtk_Recent_Filter_Func;
       Data_Destroy : Glib.G_Destroy_Notify_Address);
   --  Adds a rule to a filter that allows resources based on a custom
   --  callback function. The bitfield Needed which is passed in provides
   --  information about what sorts of information that the filter function
   --  needs; this allows GTK+ to avoid retrieving expensive information when
   --  it isn't needed by the filter.
   --  Since: gtk+ 2.10
   --  "needed": bitfield of flags indicating the information that the custom
   --  filter function needs.
   --  "func": callback function; if the function returns True, then the file
   --  will be displayed.
   --  "data_destroy": function to call to free Data when it is no longer
   --  needed.

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Add_Custom_User_Data is

      type Gtk_Recent_Filter_Func is access function
        (Filter_Info : Gtk.Recent_Filter.Gtk_Recent_Filter_Info;
         User_Data   : User_Data_Type) return Boolean;
      --  The type of function that is used with custom filters, see
      --  Gtk.Recent_Filter.Add_Custom.
      --  "filter_info": a Gtk.Recent_Filter.Gtk_Recent_Filter_Info that is
      --  filled according to the Needed flags passed to
      --  Gtk.Recent_Filter.Add_Custom
      --  "user_data": user data passed to Gtk.Recent_Filter.Add_Custom

      procedure Add_Custom
         (Filter       : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class;
          Needed       : Gtk.Recent_Filter.Gtk_Recent_Filter_Flags;
          Func         : Gtk_Recent_Filter_Func;
          Data         : User_Data_Type;
          Data_Destroy : Glib.G_Destroy_Notify_Address);
      --  Adds a rule to a filter that allows resources based on a custom
      --  callback function. The bitfield Needed which is passed in provides
      --  information about what sorts of information that the filter function
      --  needs; this allows GTK+ to avoid retrieving expensive information
      --  when it isn't needed by the filter.
      --  Since: gtk+ 2.10
      --  "needed": bitfield of flags indicating the information that the
      --  custom filter function needs.
      --  "func": callback function; if the function returns True, then the
      --  file will be displayed.
      --  "data": data to pass to Func
      --  "data_destroy": function to call to free Data when it is no longer
      --  needed.

   end Add_Custom_User_Data;

   procedure Add_Group
      (Filter : not null access Gtk_Recent_Filter_Record;
       Group  : UTF8_String);
   --  Adds a rule that allows resources based on the name of the group to
   --  which they belong
   --  Since: gtk+ 2.10
   --  "group": a group name

   procedure Add_Mime_Type
      (Filter    : not null access Gtk_Recent_Filter_Record;
       Mime_Type : UTF8_String);
   --  Adds a rule that allows resources based on their registered MIME type.
   --  Since: gtk+ 2.10
   --  "mime_type": a MIME type

   procedure Add_Pattern
      (Filter  : not null access Gtk_Recent_Filter_Record;
       Pattern : UTF8_String);
   --  Adds a rule that allows resources based on a pattern matching their
   --  display name.
   --  Since: gtk+ 2.10
   --  "pattern": a file pattern

   procedure Add_Pixbuf_Formats
      (Filter : not null access Gtk_Recent_Filter_Record);
   --  Adds a rule allowing image files in the formats supported by GdkPixbuf.
   --  Since: gtk+ 2.10

   function Filter
      (Filter      : not null access Gtk_Recent_Filter_Record;
       Filter_Info : Gtk_Recent_Filter_Info) return Boolean;
   --  Tests whether a file should be displayed according to Filter. The
   --  Gtk.Recent_Filter.Gtk_Recent_Filter_Info Filter_Info should include the
   --  fields returned from Gtk.Recent_Filter.Get_Needed, and must set the
   --  Gtk.Recent_Filter.Gtk_Recent_Filter_Info.contains field of Filter_Info
   --  to indicate which fields have been set.
   --  This function will not typically be used by applications; it is
   --  intended principally for use in the implementation of
   --  Gtk.Recent_Chooser.Gtk_Recent_Chooser.
   --  Since: gtk+ 2.10
   --  "filter_info": a Gtk.Recent_Filter.Gtk_Recent_Filter_Info containing
   --  information about a recently used resource

   function Get_Name
      (Filter : not null access Gtk_Recent_Filter_Record) return UTF8_String;
   --  Gets the human-readable name for the filter. See
   --  Gtk.Recent_Filter.Set_Name.
   --  Since: gtk+ 2.10

   procedure Set_Name
      (Filter : not null access Gtk_Recent_Filter_Record;
       Name   : UTF8_String);
   --  Sets the human-readable name of the filter; this is the string that
   --  will be displayed in the recently used resources selector user interface
   --  if there is a selectable list of filters.
   --  Since: gtk+ 2.10
   --  "name": then human readable name of Filter

   function Get_Needed
      (Filter : not null access Gtk_Recent_Filter_Record)
       return Gtk_Recent_Filter_Flags;
   --  Gets the fields that need to be filled in for the
   --  Gtk.Recent_Filter.Gtk_Recent_Filter_Info passed to
   --  Gtk.Recent_Filter.Filter
   --  This function will not typically be used by applications; it is
   --  intended principally for use in the implementation of
   --  Gtk.Recent_Chooser.Gtk_Recent_Chooser.
   --  Since: gtk+ 2.10

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Recent_Filter_Record, Gtk_Recent_Filter);
   function "+"
     (Widget : access Gtk_Recent_Filter_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Recent_Filter
   renames Implements_Gtk_Buildable.To_Object;

end Gtk.Recent_Filter;
