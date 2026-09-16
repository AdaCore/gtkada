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

--  Stores page size, orientation and margins for printing.
--
--  The idea is that you can get one of these from the page setup dialog and
--  then pass it to the `GtkPrintOperation` when printing. The benefit of
--  splitting this out of the `GtkPrintSettings` is that these affect the
--  actual layout of the page, and thus need to be set long before user prints.
--
--  ## Margins
--
--  The margins specified in this object are the "print margins", i.e. the
--  parts of the page that the printer cannot print on. These are different
--  from the layout margins that a word processor uses; they are typically used
--  to determine the minimal size for the layout margins.
--
--  To obtain a `GtkPageSetup` use [ctorGtk.PageSetup.new] to get the
--  defaults, or use [funcGtk.print_run_page_setup_dialog] to show the page
--  setup dialog and receive the resulting page setup.
--
--  ## A page setup dialog
--
--  ```c static GtkPrintSettings *settings = NULL; static GtkPageSetup
--  *page_setup = NULL;
--
--  static void do_page_setup (void) { GtkPageSetup *new_page_setup;
--
--  if (settings == NULL) settings = gtk_print_settings_new ();
--
--  new_page_setup = gtk_print_run_page_setup_dialog (GTK_WINDOW
--  (main_window), page_setup, settings);
--
--  if (page_setup) g_object_unref (page_setup);
--
--  page_setup = new_page_setup; } ```

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Error;     use Glib.Error;
with Glib.Key_File;  use Glib.Key_File;
with Glib.Object;    use Glib.Object;
with Glib.Variant;   use Glib.Variant;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Paper_Size; use Gtk.Paper_Size;

package Gtk.Page_Setup is

   type Gtk_Page_Setup_Record is new GObject_Record with null record;
   type Gtk_Page_Setup is access all Gtk_Page_Setup_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Page_Setup);
   procedure Initialize (Self : not null access Gtk_Page_Setup_Record'Class);
   --  Creates a new `GtkPageSetup`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Page_Setup_New return Gtk_Page_Setup;
   --  Creates a new `GtkPageSetup`.

   procedure Gtk_New_From_File
      (Self      : out Gtk_Page_Setup;
       File_Name : UTF8_String;
       Error     : out Glib.Error.GError);
   procedure Initialize_From_File
      (Self      : not null access Gtk_Page_Setup_Record'Class;
       File_Name : UTF8_String;
       Error     : out Glib.Error.GError);
   --  Reads the page setup from the file File_Name.
   --  Returns a new `GtkPageSetup` object with the restored page setup, or
   --  null if an error occurred. See [methodGtk.PageSetup.to_file].
   --  Initialize_From_File does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param File_Name the filename to read the page setup from
   --  @param Error the return location for a recoverable error

   function Gtk_Page_Setup_New_From_File
      (File_Name : UTF8_String;
       Error     : out Glib.Error.GError) return Gtk_Page_Setup;
   --  Reads the page setup from the file File_Name.
   --  Returns a new `GtkPageSetup` object with the restored page setup, or
   --  null if an error occurred. See [methodGtk.PageSetup.to_file].
   --  @param File_Name the filename to read the page setup from
   --  @param Error the return location for a recoverable error

   procedure Gtk_New_From_Gvariant
      (Self    : out Gtk_Page_Setup;
       Variant : Glib.Variant.Gvariant);
   procedure Initialize_From_Gvariant
      (Self    : not null access Gtk_Page_Setup_Record'Class;
       Variant : Glib.Variant.Gvariant);
   --  Desrialize a page setup from an a{sv} variant.
   --  The variant must be in the format produced by
   --  [methodGtk.PageSetup.to_gvariant].
   --  Initialize_From_Gvariant does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Variant an a{sv} `GVariant`

   function Gtk_Page_Setup_New_From_Gvariant
      (Variant : Glib.Variant.Gvariant) return Gtk_Page_Setup;
   --  Desrialize a page setup from an a{sv} variant.
   --  The variant must be in the format produced by
   --  [methodGtk.PageSetup.to_gvariant].
   --  @param Variant an a{sv} `GVariant`

   procedure Gtk_New_From_Key_File
      (Self       : out Gtk_Page_Setup;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "";
       Error      : out Glib.Error.GError);
   procedure Initialize_From_Key_File
      (Self       : not null access Gtk_Page_Setup_Record'Class;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "";
       Error      : out Glib.Error.GError);
   --  Reads the page setup from the group Group_Name in the key file
   --  Key_File.
   --  Returns a new `GtkPageSetup` object with the restored page setup, or
   --  null if an error occurred.
   --  Initialize_From_Key_File does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Key_File the `GKeyFile` to retrieve the page_setup from
   --  @param Group_Name the name of the group in the key_file to read to use
   --  the default name "Page Setup"
   --  @param Error the return location for a recoverable error

   function Gtk_Page_Setup_New_From_Key_File
      (Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "";
       Error      : out Glib.Error.GError) return Gtk_Page_Setup;
   --  Reads the page setup from the group Group_Name in the key file
   --  Key_File.
   --  Returns a new `GtkPageSetup` object with the restored page setup, or
   --  null if an error occurred.
   --  @param Key_File the `GKeyFile` to retrieve the page_setup from
   --  @param Group_Name the name of the group in the key_file to read to use
   --  the default name "Page Setup"
   --  @param Error the return location for a recoverable error

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_page_setup_get_type");

   -------------
   -- Methods --
   -------------

   function Copy
      (Self : not null access Gtk_Page_Setup_Record) return Gtk_Page_Setup;
   --  Copies a `GtkPageSetup`.
   --  @return a copy of Other. Has transfer-ownership='full'.

   function Get_Bottom_Margin
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the bottom margin in units of Unit.
   --  @param Unit the unit for the return value
   --  @return the bottom margin

   procedure Set_Bottom_Margin
      (Self   : not null access Gtk_Page_Setup_Record;
       Margin : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit);
   --  Sets the bottom margin of the `GtkPageSetup`.
   --  @param Margin the new bottom margin in units of Unit
   --  @param Unit the units for Margin

   function Get_Left_Margin
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the left margin in units of Unit.
   --  @param Unit the unit for the return value
   --  @return the left margin

   procedure Set_Left_Margin
      (Self   : not null access Gtk_Page_Setup_Record;
       Margin : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit);
   --  Sets the left margin of the `GtkPageSetup`.
   --  @param Margin the new left margin in units of Unit
   --  @param Unit the units for Margin

   function Get_Orientation
      (Self : not null access Gtk_Page_Setup_Record)
       return Gtk.Enums.Gtk_Page_Orientation;
   --  Gets the page orientation of the `GtkPageSetup`.
   --  @return the page orientation

   procedure Set_Orientation
      (Self        : not null access Gtk_Page_Setup_Record;
       Orientation : Gtk.Enums.Gtk_Page_Orientation);
   --  Sets the page orientation of the `GtkPageSetup`.
   --  @param Orientation a `GtkPageOrientation` value

   function Get_Page_Height
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Returns the page height in units of Unit.
   --  Note that this function takes orientation and margins into
   --  consideration. See [methodGtk.PageSetup.get_paper_height].
   --  @param Unit the unit for the return value
   --  @return the page height.

   function Get_Page_Width
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Returns the page width in units of Unit.
   --  Note that this function takes orientation and margins into
   --  consideration. See [methodGtk.PageSetup.get_paper_width].
   --  @param Unit the unit for the return value
   --  @return the page width.

   function Get_Paper_Height
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Returns the paper height in units of Unit.
   --  Note that this function takes orientation, but not margins into
   --  consideration. See [methodGtk.PageSetup.get_page_height].
   --  @param Unit the unit for the return value
   --  @return the paper height.

   function Get_Paper_Size
      (Self : not null access Gtk_Page_Setup_Record)
       return Gtk.Paper_Size.Gtk_Paper_Size;
   --  Gets the paper size of the `GtkPageSetup`.
   --  @return the paper size. Has transfer-ownership='none'.

   procedure Set_Paper_Size
      (Self : not null access Gtk_Page_Setup_Record;
       Size : Gtk.Paper_Size.Gtk_Paper_Size);
   --  Sets the paper size of the `GtkPageSetup` without changing the margins.
   --  See [methodGtk.PageSetup.set_paper_size_and_default_margins].
   --  @param Size a `GtkPaperSize`

   function Get_Paper_Width
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Returns the paper width in units of Unit.
   --  Note that this function takes orientation, but not margins into
   --  consideration. See [methodGtk.PageSetup.get_page_width].
   --  @param Unit the unit for the return value
   --  @return the paper width.

   function Get_Right_Margin
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the right margin in units of Unit.
   --  @param Unit the unit for the return value
   --  @return the right margin

   procedure Set_Right_Margin
      (Self   : not null access Gtk_Page_Setup_Record;
       Margin : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit);
   --  Sets the right margin of the `GtkPageSetup`.
   --  @param Margin the new right margin in units of Unit
   --  @param Unit the units for Margin

   function Get_Top_Margin
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the top margin in units of Unit.
   --  @param Unit the unit for the return value
   --  @return the top margin

   procedure Set_Top_Margin
      (Self   : not null access Gtk_Page_Setup_Record;
       Margin : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit);
   --  Sets the top margin of the `GtkPageSetup`.
   --  @param Margin the new top margin in units of Unit
   --  @param Unit the units for Margin

   function Load_File
      (Self      : not null access Gtk_Page_Setup_Record;
       File_Name : UTF8_String;
       Error     : out Glib.Error.GError) return Boolean;
   --  Reads the page setup from the file File_Name.
   --  See [methodGtk.PageSetup.to_file].
   --  @param File_Name the filename to read the page setup from
   --  @param Error the return location for a recoverable error
   --  @return True on success

   function Load_Key_File
      (Self       : not null access Gtk_Page_Setup_Record;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "";
       Error      : out Glib.Error.GError) return Boolean;
   --  Reads the page setup from the group Group_Name in the key file
   --  Key_File.
   --  @param Key_File the `GKeyFile` to retrieve the page_setup from
   --  @param Group_Name the name of the group in the key_file to read to use
   --  the default name "Page Setup"
   --  @param Error the return location for a recoverable error
   --  @return True on success

   procedure Set_Paper_Size_And_Default_Margins
      (Self : not null access Gtk_Page_Setup_Record;
       Size : Gtk.Paper_Size.Gtk_Paper_Size);
   --  Sets the paper size of the `GtkPageSetup` and modifies the margins
   --  according to the new paper size.
   --  @param Size a `GtkPaperSize`

   function To_File
      (Self      : not null access Gtk_Page_Setup_Record;
       File_Name : UTF8_String;
       Error     : out Glib.Error.GError) return Boolean;
   --  This function saves the information from Setup to File_Name.
   --  @param File_Name the file to save to
   --  @param Error the return location for a recoverable error
   --  @return True on success

   function To_Gvariant
      (Self : not null access Gtk_Page_Setup_Record)
       return Glib.Variant.Gvariant;
   --  Serialize page setup to an a{sv} variant.
   --  @return a new, floating, `GVariant`. Has transfer-ownership='none'.

   procedure To_Key_File
      (Self       : not null access Gtk_Page_Setup_Record;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "");
   --  This function adds the page setup from Setup to Key_File.
   --  @param Key_File the `GKeyFile` to save the page setup to
   --  @param Group_Name the group to add the settings to in Key_File, or null
   --  to use the default name "Page Setup"

end Gtk.Page_Setup;
