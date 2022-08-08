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
--  A GtkPageSetup object stores the page size, orientation and margins. The
--  idea is that you can get one of these from the page setup dialog and then
--  pass it to the Gtk.Print_Operation.Gtk_Print_Operation when printing. The
--  benefit of splitting this out of the Gtk.Print_Settings.Gtk_Print_Settings
--  is that these affect the actual layout of the page, and thus need to be set
--  long before user prints.
--
--  ## Margins ## {print-margins} The margins specified in this object are the
--  "print margins", i.e. the parts of the page that the printer cannot print
--  on. These are different from the layout margins that a word processor uses;
--  they are typically used to determine the minimal size for the layout
--  margins.
--
--  To obtain a Gtk.Page_Setup.Gtk_Page_Setup use Gtk.Page_Setup.Gtk_New to
--  get the defaults, or use gtk_print_run_page_setup_dialog to show the page
--  setup dialog and receive the resulting page setup.
--
--  ## A page setup dialog
--
--  |[<!-- language="C" --> static GtkPrintSettings *settings = NULL; static
--  GtkPageSetup *page_setup = NULL;
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
--  page_setup = new_page_setup; } ]|
--
--  Printing support was added in GTK+ 2.10.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
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
   --  Creates a new Gtk.Page_Setup.Gtk_Page_Setup.
   --  Since: gtk+ 2.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Page_Setup_New return Gtk_Page_Setup;
   --  Creates a new Gtk.Page_Setup.Gtk_Page_Setup.
   --  Since: gtk+ 2.10

   procedure Gtk_New_From_File
      (Self      : out Gtk_Page_Setup;
       File_Name : UTF8_String);
   procedure Initialize_From_File
      (Self      : not null access Gtk_Page_Setup_Record'Class;
       File_Name : UTF8_String);
   --  Reads the page setup from the file File_Name. Returns a new
   --  Gtk.Page_Setup.Gtk_Page_Setup object with the restored page setup, or
   --  null if an error occurred. See Gtk.Page_Setup.To_File.
   --  Since: gtk+ 2.12
   --  Initialize_From_File does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "file_name": the filename to read the page setup from

   function Gtk_Page_Setup_New_From_File
      (File_Name : UTF8_String) return Gtk_Page_Setup;
   --  Reads the page setup from the file File_Name. Returns a new
   --  Gtk.Page_Setup.Gtk_Page_Setup object with the restored page setup, or
   --  null if an error occurred. See Gtk.Page_Setup.To_File.
   --  Since: gtk+ 2.12
   --  "file_name": the filename to read the page setup from

   procedure Gtk_New_From_Gvariant
      (Self    : out Gtk_Page_Setup;
       Variant : Glib.Variant.Gvariant);
   procedure Initialize_From_Gvariant
      (Self    : not null access Gtk_Page_Setup_Record'Class;
       Variant : Glib.Variant.Gvariant);
   --  Desrialize a page setup from an a{sv} variant in the format produced by
   --  Gtk.Page_Setup.To_Gvariant.
   --  Since: gtk+ 3.22
   --  Initialize_From_Gvariant does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "variant": an a{sv} Glib.Variant.Gvariant

   function Gtk_Page_Setup_New_From_Gvariant
      (Variant : Glib.Variant.Gvariant) return Gtk_Page_Setup;
   --  Desrialize a page setup from an a{sv} variant in the format produced by
   --  Gtk.Page_Setup.To_Gvariant.
   --  Since: gtk+ 3.22
   --  "variant": an a{sv} Glib.Variant.Gvariant

   procedure Gtk_New_From_Key_File
      (Self       : out Gtk_Page_Setup;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "");
   procedure Initialize_From_Key_File
      (Self       : not null access Gtk_Page_Setup_Record'Class;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "");
   --  Reads the page setup from the group Group_Name in the key file
   --  Key_File. Returns a new Gtk.Page_Setup.Gtk_Page_Setup object with the
   --  restored page setup, or null if an error occurred.
   --  Since: gtk+ 2.12
   --  Initialize_From_Key_File does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "key_file": the Gkey.File.Gkey_File to retrieve the page_setup from
   --  "group_name": the name of the group in the key_file to read, or null to
   --  use the default name "Page Setup"

   function Gtk_Page_Setup_New_From_Key_File
      (Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "") return Gtk_Page_Setup;
   --  Reads the page setup from the group Group_Name in the key file
   --  Key_File. Returns a new Gtk.Page_Setup.Gtk_Page_Setup object with the
   --  restored page setup, or null if an error occurred.
   --  Since: gtk+ 2.12
   --  "key_file": the Gkey.File.Gkey_File to retrieve the page_setup from
   --  "group_name": the name of the group in the key_file to read, or null to
   --  use the default name "Page Setup"

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_page_setup_get_type");

   -------------
   -- Methods --
   -------------

   function Copy
      (Self : not null access Gtk_Page_Setup_Record) return Gtk_Page_Setup;
   --  Copies a Gtk.Page_Setup.Gtk_Page_Setup.
   --  Since: gtk+ 2.10

   function Get_Bottom_Margin
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the bottom margin in units of Unit.
   --  Since: gtk+ 2.10
   --  "unit": the unit for the return value

   procedure Set_Bottom_Margin
      (Self   : not null access Gtk_Page_Setup_Record;
       Margin : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit);
   --  Sets the bottom margin of the Gtk.Page_Setup.Gtk_Page_Setup.
   --  Since: gtk+ 2.10
   --  "margin": the new bottom margin in units of Unit
   --  "unit": the units for Margin

   function Get_Left_Margin
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the left margin in units of Unit.
   --  Since: gtk+ 2.10
   --  "unit": the unit for the return value

   procedure Set_Left_Margin
      (Self   : not null access Gtk_Page_Setup_Record;
       Margin : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit);
   --  Sets the left margin of the Gtk.Page_Setup.Gtk_Page_Setup.
   --  Since: gtk+ 2.10
   --  "margin": the new left margin in units of Unit
   --  "unit": the units for Margin

   function Get_Orientation
      (Self : not null access Gtk_Page_Setup_Record)
       return Gtk.Enums.Gtk_Page_Orientation;
   --  Gets the page orientation of the Gtk.Page_Setup.Gtk_Page_Setup.
   --  Since: gtk+ 2.10

   procedure Set_Orientation
      (Self        : not null access Gtk_Page_Setup_Record;
       Orientation : Gtk.Enums.Gtk_Page_Orientation);
   --  Sets the page orientation of the Gtk.Page_Setup.Gtk_Page_Setup.
   --  Since: gtk+ 2.10
   --  "orientation": a Gtk.Enums.Gtk_Page_Orientation value

   function Get_Page_Height
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Returns the page height in units of Unit.
   --  Note that this function takes orientation and margins into
   --  consideration. See Gtk.Page_Setup.Get_Paper_Height.
   --  Since: gtk+ 2.10
   --  "unit": the unit for the return value

   function Get_Page_Width
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Returns the page width in units of Unit.
   --  Note that this function takes orientation and margins into
   --  consideration. See Gtk.Page_Setup.Get_Paper_Width.
   --  Since: gtk+ 2.10
   --  "unit": the unit for the return value

   function Get_Paper_Height
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Returns the paper height in units of Unit.
   --  Note that this function takes orientation, but not margins into
   --  consideration. See Gtk.Page_Setup.Get_Page_Height.
   --  Since: gtk+ 2.10
   --  "unit": the unit for the return value

   function Get_Paper_Size
      (Self : not null access Gtk_Page_Setup_Record)
       return Gtk.Paper_Size.Gtk_Paper_Size;
   --  Gets the paper size of the Gtk.Page_Setup.Gtk_Page_Setup.
   --  Since: gtk+ 2.10

   procedure Set_Paper_Size
      (Self : not null access Gtk_Page_Setup_Record;
       Size : Gtk.Paper_Size.Gtk_Paper_Size);
   --  Sets the paper size of the Gtk.Page_Setup.Gtk_Page_Setup without
   --  changing the margins. See
   --  Gtk.Page_Setup.Set_Paper_Size_And_Default_Margins.
   --  Since: gtk+ 2.10
   --  "size": a Gtk.Paper_Size.Gtk_Paper_Size

   function Get_Paper_Width
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Returns the paper width in units of Unit.
   --  Note that this function takes orientation, but not margins into
   --  consideration. See Gtk.Page_Setup.Get_Page_Width.
   --  Since: gtk+ 2.10
   --  "unit": the unit for the return value

   function Get_Right_Margin
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the right margin in units of Unit.
   --  Since: gtk+ 2.10
   --  "unit": the unit for the return value

   procedure Set_Right_Margin
      (Self   : not null access Gtk_Page_Setup_Record;
       Margin : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit);
   --  Sets the right margin of the Gtk.Page_Setup.Gtk_Page_Setup.
   --  Since: gtk+ 2.10
   --  "margin": the new right margin in units of Unit
   --  "unit": the units for Margin

   function Get_Top_Margin
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the top margin in units of Unit.
   --  Since: gtk+ 2.10
   --  "unit": the unit for the return value

   procedure Set_Top_Margin
      (Self   : not null access Gtk_Page_Setup_Record;
       Margin : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit);
   --  Sets the top margin of the Gtk.Page_Setup.Gtk_Page_Setup.
   --  Since: gtk+ 2.10
   --  "margin": the new top margin in units of Unit
   --  "unit": the units for Margin

   function Load_File
      (Self      : not null access Gtk_Page_Setup_Record;
       File_Name : UTF8_String) return Boolean;
   --  Reads the page setup from the file File_Name. See
   --  Gtk.Page_Setup.To_File.
   --  Since: gtk+ 2.14
   --  "file_name": the filename to read the page setup from

   function Load_Key_File
      (Self       : not null access Gtk_Page_Setup_Record;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "") return Boolean;
   --  Reads the page setup from the group Group_Name in the key file
   --  Key_File.
   --  Since: gtk+ 2.14
   --  "key_file": the Gkey.File.Gkey_File to retrieve the page_setup from
   --  "group_name": the name of the group in the key_file to read, or null to
   --  use the default name "Page Setup"

   procedure Set_Paper_Size_And_Default_Margins
      (Self : not null access Gtk_Page_Setup_Record;
       Size : Gtk.Paper_Size.Gtk_Paper_Size);
   --  Sets the paper size of the Gtk.Page_Setup.Gtk_Page_Setup and modifies
   --  the margins according to the new paper size.
   --  Since: gtk+ 2.10
   --  "size": a Gtk.Paper_Size.Gtk_Paper_Size

   function To_File
      (Self      : not null access Gtk_Page_Setup_Record;
       File_Name : UTF8_String) return Boolean;
   --  This function saves the information from Setup to File_Name.
   --  Since: gtk+ 2.12
   --  "file_name": the file to save to

   function To_Gvariant
      (Self : not null access Gtk_Page_Setup_Record)
       return Glib.Variant.Gvariant;
   --  Serialize page setup to an a{sv} variant.
   --  Since: gtk+ 3.22

   procedure To_Key_File
      (Self       : not null access Gtk_Page_Setup_Record;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "");
   --  This function adds the page setup from Setup to Key_File.
   --  Since: gtk+ 2.12
   --  "key_file": the Gkey.File.Gkey_File to save the page setup to
   --  "group_name": the group to add the settings to in Key_File, or null to
   --  use the default name "Page Setup"

end Gtk.Page_Setup;
