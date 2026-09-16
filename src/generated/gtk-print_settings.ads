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

--  Collects the settings of a print dialog in a system-independent way.
--
--  The main use for this object is that once you've printed you can get a
--  settings object that represents the settings the user chose, and the next
--  time you print you can pass that object in so that the user doesn't have to
--  re-set all his settings.
--
--  Its also possible to enumerate the settings so that you can easily save
--  the settings for the next time your app runs, or even store them in a
--  document. The predefined keys try to use shared values as much as possible
--  so that moving such a document between systems still works.

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Error;     use Glib.Error;
with Glib.Key_File;  use Glib.Key_File;
with Glib.Object;    use Glib.Object;
with Glib.Variant;   use Glib.Variant;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Paper_Size; use Gtk.Paper_Size;

package Gtk.Print_Settings is

   type Gtk_Print_Settings_Record is new GObject_Record with null record;
   type Gtk_Print_Settings is access all Gtk_Print_Settings_Record'Class;

   type Page_Range is record
      First, Last : Glib.gint;
   end record
      with Convention => C;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Print_Settings_Func is access procedure (Key : UTF8_String; Value : UTF8_String);
   --  Function called by [methodGtk.PrintSettings.foreach] on every key/value
   --  pair inside a [classGtk.PrintSettings].
   --  @param Key the setting key
   --  @param Value the setting value

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Print_Settings);
   procedure Initialize
      (Self : not null access Gtk_Print_Settings_Record'Class);
   --  Creates a new `GtkPrintSettings` object.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Print_Settings_New return Gtk_Print_Settings;
   --  Creates a new `GtkPrintSettings` object.

   procedure Gtk_New_From_File
      (Self      : out Gtk_Print_Settings;
       File_Name : UTF8_String;
       Error     : out Glib.Error.GError);
   procedure Initialize_From_File
      (Self      : not null access Gtk_Print_Settings_Record'Class;
       File_Name : UTF8_String;
       Error     : out Glib.Error.GError);
   --  Reads the print settings from File_Name.
   --  Returns a new `GtkPrintSettings` object with the restored settings, or
   --  null if an error occurred. If the file could not be loaded then error is
   --  set to either a `GFileError` or `GKeyFileError`.
   --  See [methodGtk.PrintSettings.to_file].
   --  Initialize_From_File does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param File_Name the filename to read the settings from
   --  @param Error the return location for a recoverable error

   function Gtk_Print_Settings_New_From_File
      (File_Name : UTF8_String;
       Error     : out Glib.Error.GError) return Gtk_Print_Settings;
   --  Reads the print settings from File_Name.
   --  Returns a new `GtkPrintSettings` object with the restored settings, or
   --  null if an error occurred. If the file could not be loaded then error is
   --  set to either a `GFileError` or `GKeyFileError`.
   --  See [methodGtk.PrintSettings.to_file].
   --  @param File_Name the filename to read the settings from
   --  @param Error the return location for a recoverable error

   procedure Gtk_New_From_Gvariant
      (Self    : out Gtk_Print_Settings;
       Variant : Glib.Variant.Gvariant);
   procedure Initialize_From_Gvariant
      (Self    : not null access Gtk_Print_Settings_Record'Class;
       Variant : Glib.Variant.Gvariant);
   --  Deserialize print settings from an a{sv} variant.
   --  The variant must be in the format produced by
   --  [methodGtk.PrintSettings.to_gvariant].
   --  Initialize_From_Gvariant does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Variant an a{sv} `GVariant`

   function Gtk_Print_Settings_New_From_Gvariant
      (Variant : Glib.Variant.Gvariant) return Gtk_Print_Settings;
   --  Deserialize print settings from an a{sv} variant.
   --  The variant must be in the format produced by
   --  [methodGtk.PrintSettings.to_gvariant].
   --  @param Variant an a{sv} `GVariant`

   procedure Gtk_New_From_Key_File
      (Self       : out Gtk_Print_Settings;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "";
       Error      : out Glib.Error.GError);
   procedure Initialize_From_Key_File
      (Self       : not null access Gtk_Print_Settings_Record'Class;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "";
       Error      : out Glib.Error.GError);
   --  Reads the print settings from the group Group_Name in Key_File.
   --  Returns a new `GtkPrintSettings` object with the restored settings, or
   --  null if an error occurred. If the file could not be loaded then error is
   --  set to either `GFileError` or `GKeyFileError`.
   --  Initialize_From_Key_File does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Key_File the `GKeyFile` to retrieve the settings from
   --  @param Group_Name the name of the group to use, or null to use the
   --  default "Print Settings"
   --  @param Error the return location for a recoverable error

   function Gtk_Print_Settings_New_From_Key_File
      (Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "";
       Error      : out Glib.Error.GError) return Gtk_Print_Settings;
   --  Reads the print settings from the group Group_Name in Key_File.
   --  Returns a new `GtkPrintSettings` object with the restored settings, or
   --  null if an error occurred. If the file could not be loaded then error is
   --  set to either `GFileError` or `GKeyFileError`.
   --  @param Key_File the `GKeyFile` to retrieve the settings from
   --  @param Group_Name the name of the group to use, or null to use the
   --  default "Print Settings"
   --  @param Error the return location for a recoverable error

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_print_settings_get_type");

   -------------
   -- Methods --
   -------------

   function Copy
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk_Print_Settings;
   --  Copies a `GtkPrintSettings` object.
   --  @return a newly allocated copy of Other. Has transfer-ownership='full'.

   procedure Foreach
      (Self : not null access Gtk_Print_Settings_Record;
       Func : Gtk_Print_Settings_Func);
   --  Calls Func for each key-value pair of Settings.
   --  @param Func the function to call

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Foreach_User_Data is

      type Gtk_Print_Settings_Func is access procedure
        (Key       : UTF8_String;
         Value     : UTF8_String;
         User_Data : User_Data_Type);
      --  Function called by [methodGtk.PrintSettings.foreach] on every key/value
      --  pair inside a [classGtk.PrintSettings].
      --  @param Key the setting key
      --  @param Value the setting value
      --  @param User_Data The user data provided with the function

      procedure Foreach
         (Self      : not null access Gtk.Print_Settings.Gtk_Print_Settings_Record'Class;
          Func      : Gtk_Print_Settings_Func;
          User_Data : User_Data_Type);
      --  Calls Func for each key-value pair of Settings.
      --  @param Func the function to call
      --  @param User_Data user data for Func

   end Foreach_User_Data;

   function Get
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String) return UTF8_String;
   --  Looks up the string value associated with Key.
   --  @param Key a key
   --  @return the string value for Key

   function Get_Bool
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String) return Boolean;
   --  Returns the boolean represented by the value that is associated with
   --  Key.
   --  The string "true" represents True, any other string False.
   --  @param Key a key
   --  @return True, if Key maps to a true value.

   procedure Set_Bool
      (Self  : not null access Gtk_Print_Settings_Record;
       Key   : UTF8_String;
       Value : Boolean);
   --  Sets Key to a boolean value.
   --  @param Key a key
   --  @param Value a boolean

   function Get_Collate
      (Self : not null access Gtk_Print_Settings_Record) return Boolean;
   --  Gets the value of GTK_PRINT_SETTINGS_COLLATE.
   --  @return whether to collate the printed pages

   procedure Set_Collate
      (Self    : not null access Gtk_Print_Settings_Record;
       Collate : Boolean);
   --  Sets the value of GTK_PRINT_SETTINGS_COLLATE.
   --  @param Collate whether to collate the output

   function Get_Default_Source
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String;
   --  Gets the value of GTK_PRINT_SETTINGS_DEFAULT_SOURCE.
   --  @return the default source

   procedure Set_Default_Source
      (Self           : not null access Gtk_Print_Settings_Record;
       Default_Source : UTF8_String);
   --  Sets the value of GTK_PRINT_SETTINGS_DEFAULT_SOURCE.
   --  @param Default_Source the default source

   function Get_Dither
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String;
   --  Gets the value of GTK_PRINT_SETTINGS_DITHER.
   --  @return the dithering that is used

   procedure Set_Dither
      (Self   : not null access Gtk_Print_Settings_Record;
       Dither : UTF8_String);
   --  Sets the value of GTK_PRINT_SETTINGS_DITHER.
   --  @param Dither the dithering that is used

   function Get_Double
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String) return Gdouble;
   --  Returns the double value associated with Key, or 0.
   --  @param Key a key
   --  @return the double value of Key

   procedure Set_Double
      (Self  : not null access Gtk_Print_Settings_Record;
       Key   : UTF8_String;
       Value : Gdouble);
   --  Sets Key to a double value.
   --  @param Key a key
   --  @param Value a double value

   function Get_Double_With_Default
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String;
       Def  : Gdouble) return Gdouble;
   --  Returns the floating point number represented by the value that is
   --  associated with Key, or Default_Val if the value does not represent a
   --  floating point number.
   --  Floating point numbers are parsed with g_ascii_strtod.
   --  @param Key a key
   --  @param Def the default value
   --  @return the floating point number associated with Key

   function Get_Duplex
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Print_Duplex;
   --  Gets the value of GTK_PRINT_SETTINGS_DUPLEX.
   --  @return whether to print the output in duplex.

   procedure Set_Duplex
      (Self   : not null access Gtk_Print_Settings_Record;
       Duplex : Gtk.Enums.Gtk_Print_Duplex);
   --  Sets the value of GTK_PRINT_SETTINGS_DUPLEX.
   --  @param Duplex a `GtkPrintDuplex` value

   function Get_Finishings
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String;
   --  Gets the value of GTK_PRINT_SETTINGS_FINISHINGS.
   --  @return the finishings

   procedure Set_Finishings
      (Self       : not null access Gtk_Print_Settings_Record;
       Finishings : UTF8_String);
   --  Sets the value of GTK_PRINT_SETTINGS_FINISHINGS.
   --  @param Finishings the finishings

   function Get_Int
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String) return Glib.Gint;
   --  Returns the integer value of Key, or 0.
   --  @param Key a key
   --  @return the integer value of Key

   procedure Set_Int
      (Self  : not null access Gtk_Print_Settings_Record;
       Key   : UTF8_String;
       Value : Glib.Gint);
   --  Sets Key to an integer value.
   --  @param Key a key
   --  @param Value an integer

   function Get_Int_With_Default
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String;
       Def  : Glib.Gint) return Glib.Gint;
   --  Returns the value of Key, interpreted as an integer, or the default
   --  value.
   --  @param Key a key
   --  @param Def the default value
   --  @return the integer value of Key

   function Get_Length
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Returns the value associated with Key, interpreted as a length.
   --  The returned value is converted to Units.
   --  @param Key a key
   --  @param Unit the unit of the return value
   --  @return the length value of Key, converted to Unit

   procedure Set_Length
      (Self  : not null access Gtk_Print_Settings_Record;
       Key   : UTF8_String;
       Value : Gdouble;
       Unit  : Gtk.Enums.Gtk_Unit);
   --  Associates a length in units of Unit with Key.
   --  @param Key a key
   --  @param Value a length
   --  @param Unit the unit of Length

   function Get_Media_Type
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String;
   --  Gets the value of GTK_PRINT_SETTINGS_MEDIA_TYPE.
   --  The set of media types is defined in PWG 5101.1-2002 PWG.
   --  @return the media type

   procedure Set_Media_Type
      (Self       : not null access Gtk_Print_Settings_Record;
       Media_Type : UTF8_String);
   --  Sets the value of GTK_PRINT_SETTINGS_MEDIA_TYPE.
   --  The set of media types is defined in PWG 5101.1-2002 PWG.
   --  @param Media_Type the media type

   function Get_N_Copies
      (Self : not null access Gtk_Print_Settings_Record) return Glib.Gint;
   --  Gets the value of GTK_PRINT_SETTINGS_N_COPIES.
   --  @return the number of copies to print

   procedure Set_N_Copies
      (Self       : not null access Gtk_Print_Settings_Record;
       Num_Copies : Glib.Gint);
   --  Sets the value of GTK_PRINT_SETTINGS_N_COPIES.
   --  @param Num_Copies the number of copies

   function Get_Number_Up
      (Self : not null access Gtk_Print_Settings_Record) return Glib.Gint;
   --  Gets the value of GTK_PRINT_SETTINGS_NUMBER_UP.
   --  @return the number of pages per sheet

   procedure Set_Number_Up
      (Self      : not null access Gtk_Print_Settings_Record;
       Number_Up : Glib.Gint);
   --  Sets the value of GTK_PRINT_SETTINGS_NUMBER_UP.
   --  @param Number_Up the number of pages per sheet

   function Get_Number_Up_Layout
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Number_Up_Layout;
   --  Gets the value of GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT.
   --  @return layout of page in number-up mode

   procedure Set_Number_Up_Layout
      (Self             : not null access Gtk_Print_Settings_Record;
       Number_Up_Layout : Gtk.Enums.Gtk_Number_Up_Layout);
   --  Sets the value of GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT.
   --  @param Number_Up_Layout a `GtkNumberUpLayout` value

   function Get_Orientation
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Page_Orientation;
   --  Get the value of GTK_PRINT_SETTINGS_ORIENTATION, converted to a
   --  `GtkPageOrientation`.
   --  @return the orientation

   procedure Set_Orientation
      (Self        : not null access Gtk_Print_Settings_Record;
       Orientation : Gtk.Enums.Gtk_Page_Orientation);
   --  Sets the value of GTK_PRINT_SETTINGS_ORIENTATION.
   --  @param Orientation a page orientation

   function Get_Output_Bin
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String;
   --  Gets the value of GTK_PRINT_SETTINGS_OUTPUT_BIN.
   --  @return the output bin

   procedure Set_Output_Bin
      (Self       : not null access Gtk_Print_Settings_Record;
       Output_Bin : UTF8_String);
   --  Sets the value of GTK_PRINT_SETTINGS_OUTPUT_BIN.
   --  @param Output_Bin the output bin

   function Get_Page_Ranges
      (Self       : not null access Gtk_Print_Settings_Record;
       Num_Ranges : out Glib.Gint) return Page_Range;
   --  Gets the value of GTK_PRINT_SETTINGS_PAGE_RANGES.
   --  @param Num_Ranges return location for the length of the returned array
   --  @return an array of `GtkPageRange`s. Use g_free to free the array when
   --  it is no longer needed.

   procedure Set_Page_Ranges
      (Self        : not null access Gtk_Print_Settings_Record;
       Page_Ranges : Page_Range;
       Num_Ranges  : Glib.Gint);
   --  Sets the value of GTK_PRINT_SETTINGS_PAGE_RANGES.
   --  @param Page_Ranges an array of `GtkPageRange`s
   --  @param Num_Ranges the length of Page_Ranges

   function Get_Page_Set
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Page_Set;
   --  Gets the value of GTK_PRINT_SETTINGS_PAGE_SET.
   --  @return the set of pages to print

   procedure Set_Page_Set
      (Self     : not null access Gtk_Print_Settings_Record;
       Page_Set : Gtk.Enums.Gtk_Page_Set);
   --  Sets the value of GTK_PRINT_SETTINGS_PAGE_SET.
   --  @param Page_Set a `GtkPageSet` value

   function Get_Paper_Height
      (Self : not null access Gtk_Print_Settings_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the value of GTK_PRINT_SETTINGS_PAPER_HEIGHT, converted to Unit.
   --  @param Unit the unit for the return value
   --  @return the paper height, in units of Unit

   procedure Set_Paper_Height
      (Self   : not null access Gtk_Print_Settings_Record;
       Height : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit);
   --  Sets the value of GTK_PRINT_SETTINGS_PAPER_HEIGHT.
   --  @param Height the paper height
   --  @param Unit the units of Height

   function Get_Paper_Size
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Paper_Size.Gtk_Paper_Size;
   --  Gets the value of GTK_PRINT_SETTINGS_PAPER_FORMAT, converted to a
   --  `GtkPaperSize`.
   --  @return the paper size. Has transfer-ownership='full'.

   procedure Set_Paper_Size
      (Self       : not null access Gtk_Print_Settings_Record;
       Paper_Size : Gtk.Paper_Size.Gtk_Paper_Size);
   --  Sets the value of GTK_PRINT_SETTINGS_PAPER_FORMAT,
   --  GTK_PRINT_SETTINGS_PAPER_WIDTH and GTK_PRINT_SETTINGS_PAPER_HEIGHT.
   --  @param Paper_Size a paper size

   function Get_Paper_Width
      (Self : not null access Gtk_Print_Settings_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the value of GTK_PRINT_SETTINGS_PAPER_WIDTH, converted to Unit.
   --  @param Unit the unit for the return value
   --  @return the paper width, in units of Unit

   procedure Set_Paper_Width
      (Self  : not null access Gtk_Print_Settings_Record;
       Width : Gdouble;
       Unit  : Gtk.Enums.Gtk_Unit);
   --  Sets the value of GTK_PRINT_SETTINGS_PAPER_WIDTH.
   --  @param Width the paper width
   --  @param Unit the units of Width

   function Get_Print_Pages
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Print_Pages;
   --  Gets the value of GTK_PRINT_SETTINGS_PRINT_PAGES.
   --  @return which pages to print

   procedure Set_Print_Pages
      (Self  : not null access Gtk_Print_Settings_Record;
       Pages : Gtk.Enums.Gtk_Print_Pages);
   --  Sets the value of GTK_PRINT_SETTINGS_PRINT_PAGES.
   --  @param Pages a `GtkPrintPages` value

   function Get_Printer
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String;
   --  Convenience function to obtain the value of GTK_PRINT_SETTINGS_PRINTER.
   --  @return the printer name

   procedure Set_Printer
      (Self    : not null access Gtk_Print_Settings_Record;
       Printer : UTF8_String);
   --  Convenience function to set GTK_PRINT_SETTINGS_PRINTER to Printer.
   --  @param Printer the printer name

   function Get_Printer_Lpi
      (Self : not null access Gtk_Print_Settings_Record) return Gdouble;
   --  Gets the value of GTK_PRINT_SETTINGS_PRINTER_LPI.
   --  @return the resolution in lpi (lines per inch)

   procedure Set_Printer_Lpi
      (Self : not null access Gtk_Print_Settings_Record;
       Lpi  : Gdouble);
   --  Sets the value of GTK_PRINT_SETTINGS_PRINTER_LPI.
   --  @param Lpi the resolution in lpi (lines per inch)

   function Get_Quality
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Print_Quality;
   --  Gets the value of GTK_PRINT_SETTINGS_QUALITY.
   --  @return the print quality

   procedure Set_Quality
      (Self    : not null access Gtk_Print_Settings_Record;
       Quality : Gtk.Enums.Gtk_Print_Quality);
   --  Sets the value of GTK_PRINT_SETTINGS_QUALITY.
   --  @param Quality a `GtkPrintQuality` value

   function Get_Resolution
      (Self : not null access Gtk_Print_Settings_Record) return Glib.Gint;
   --  Gets the value of GTK_PRINT_SETTINGS_RESOLUTION.
   --  @return the resolution in dpi

   procedure Set_Resolution
      (Self       : not null access Gtk_Print_Settings_Record;
       Resolution : Glib.Gint);
   --  Sets the values of GTK_PRINT_SETTINGS_RESOLUTION,
   --  GTK_PRINT_SETTINGS_RESOLUTION_X and GTK_PRINT_SETTINGS_RESOLUTION_Y.
   --  @param Resolution the resolution in dpi

   function Get_Resolution_X
      (Self : not null access Gtk_Print_Settings_Record) return Glib.Gint;
   --  Gets the value of GTK_PRINT_SETTINGS_RESOLUTION_X.
   --  @return the horizontal resolution in dpi

   function Get_Resolution_Y
      (Self : not null access Gtk_Print_Settings_Record) return Glib.Gint;
   --  Gets the value of GTK_PRINT_SETTINGS_RESOLUTION_Y.
   --  @return the vertical resolution in dpi

   function Get_Reverse
      (Self : not null access Gtk_Print_Settings_Record) return Boolean;
   --  Gets the value of GTK_PRINT_SETTINGS_REVERSE.
   --  @return whether to reverse the order of the printed pages

   procedure Set_Reverse
      (Self        : not null access Gtk_Print_Settings_Record;
       Gtk_Reverse : Boolean);
   --  Sets the value of GTK_PRINT_SETTINGS_REVERSE.
   --  @param Gtk_Reverse whether to reverse the output

   function Get_Scale
      (Self : not null access Gtk_Print_Settings_Record) return Gdouble;
   --  Gets the value of GTK_PRINT_SETTINGS_SCALE.
   --  @return the scale in percent

   procedure Set_Scale
      (Self  : not null access Gtk_Print_Settings_Record;
       Scale : Gdouble);
   --  Sets the value of GTK_PRINT_SETTINGS_SCALE.
   --  @param Scale the scale in percent

   function Get_Use_Color
      (Self : not null access Gtk_Print_Settings_Record) return Boolean;
   --  Gets the value of GTK_PRINT_SETTINGS_USE_COLOR.
   --  @return whether to use color

   procedure Set_Use_Color
      (Self      : not null access Gtk_Print_Settings_Record;
       Use_Color : Boolean);
   --  Sets the value of GTK_PRINT_SETTINGS_USE_COLOR.
   --  @param Use_Color whether to use color

   function Has_Key
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String) return Boolean;
   --  Returns True, if a value is associated with Key.
   --  @param Key a key
   --  @return True, if Key has a value

   function Load_File
      (Self      : not null access Gtk_Print_Settings_Record;
       File_Name : UTF8_String;
       Error     : out Glib.Error.GError) return Boolean;
   --  Reads the print settings from File_Name.
   --  If the file could not be loaded then error is set to either a
   --  `GFileError` or `GKeyFileError`.
   --  See [methodGtk.PrintSettings.to_file].
   --  @param File_Name the filename to read the settings from
   --  @param Error the return location for a recoverable error
   --  @return True on success

   function Load_Key_File
      (Self       : not null access Gtk_Print_Settings_Record;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "";
       Error      : out Glib.Error.GError) return Boolean;
   --  Reads the print settings from the group Group_Name in Key_File.
   --  If the file could not be loaded then error is set to either a
   --  `GFileError` or `GKeyFileError`.
   --  @param Key_File the `GKeyFile` to retrieve the settings from
   --  @param Group_Name the name of the group to use, or null to use the
   --  default "Print Settings"
   --  @param Error the return location for a recoverable error
   --  @return True on success

   procedure Set
      (Self  : not null access Gtk_Print_Settings_Record;
       Key   : UTF8_String;
       Value : UTF8_String := "");
   --  Associates Value with Key.
   --  @param Key a key
   --  @param Value a string value

   procedure Set_Resolution_Xy
      (Self         : not null access Gtk_Print_Settings_Record;
       Resolution_X : Glib.Gint;
       Resolution_Y : Glib.Gint);
   --  Sets the values of GTK_PRINT_SETTINGS_RESOLUTION,
   --  GTK_PRINT_SETTINGS_RESOLUTION_X and GTK_PRINT_SETTINGS_RESOLUTION_Y.
   --  @param Resolution_X the horizontal resolution in dpi
   --  @param Resolution_Y the vertical resolution in dpi

   function To_File
      (Self      : not null access Gtk_Print_Settings_Record;
       File_Name : UTF8_String;
       Error     : out Glib.Error.GError) return Boolean;
   --  This function saves the print settings from Settings to File_Name.
   --  If the file could not be written then error is set to either a
   --  `GFileError` or `GKeyFileError`.
   --  @param File_Name the file to save to
   --  @param Error the return location for a recoverable error
   --  @return True on success

   function To_Gvariant
      (Self : not null access Gtk_Print_Settings_Record)
       return Glib.Variant.Gvariant;
   --  Serialize print settings to an a{sv} variant.
   --  @return a new, floating, `GVariant`. Has transfer-ownership='none'.

   procedure To_Key_File
      (Self       : not null access Gtk_Print_Settings_Record;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "");
   --  This function adds the print settings from Settings to Key_File.
   --  @param Key_File the `GKeyFile` to save the print settings to
   --  @param Group_Name the group to add the settings to in Key_File, or null
   --  to use the default "Print Settings"

   procedure Unset
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String);
   --  Removes any value associated with Key.
   --  This has the same effect as setting the value to null.
   --  @param Key a key

end Gtk.Print_Settings;
