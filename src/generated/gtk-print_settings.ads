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
--  A GtkPrintSettings object represents the settings of a print dialog in a
--  system-independent way. The main use for this object is that once you've
--  printed you can get a settings object that represents the settings the user
--  chose, and the next time you print you can pass that object in so that the
--  user doesn't have to re-set all his settings.
--
--  Its also possible to enumerate the settings so that you can easily save
--  the settings for the next time your app runs, or even store them in a
--  document. The predefined keys try to use shared values as much as possible
--  so that moving such a document between systems still works.
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

package Gtk.Print_Settings is

   type Gtk_Print_Settings_Record is new GObject_Record with null record;
   type Gtk_Print_Settings is access all Gtk_Print_Settings_Record'Class;

   type Gtk_Page_Range_Record is record
      Range_Start : Gint;
      Range_End   : Gint;
   end record;
   pragma Convention (C, Gtk_Page_Range_Record);
   type Gtk_Page_Range_Array is array (Integer range <>) of Gtk_Page_Range_Record;
   pragma Convention (C, Gtk_Page_Range_Array);
   --  Page range specification(s).

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Print_Settings_Func is access procedure (Key : UTF8_String; Value : UTF8_String);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Print_Settings);
   procedure Initialize
      (Self : not null access Gtk_Print_Settings_Record'Class);
   --  Creates a new Gtk.Print_Settings.Gtk_Print_Settings object.
   --  Since: gtk+ 2.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Print_Settings_New return Gtk_Print_Settings;
   --  Creates a new Gtk.Print_Settings.Gtk_Print_Settings object.
   --  Since: gtk+ 2.10

   procedure Gtk_New_From_File
      (Self      : out Gtk_Print_Settings;
       File_Name : UTF8_String);
   procedure Initialize_From_File
      (Self      : not null access Gtk_Print_Settings_Record'Class;
       File_Name : UTF8_String);
   --  Reads the print settings from File_Name. Returns a new
   --  Gtk.Print_Settings.Gtk_Print_Settings object with the restored settings,
   --  or null if an error occurred. If the file could not be loaded then error
   --  is set to either a GFile_Error or GKey_File_Error. See
   --  Gtk.Print_Settings.To_File.
   --  Since: gtk+ 2.12
   --  Initialize_From_File does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "file_name": the filename to read the settings from

   function Gtk_Print_Settings_New_From_File
      (File_Name : UTF8_String) return Gtk_Print_Settings;
   --  Reads the print settings from File_Name. Returns a new
   --  Gtk.Print_Settings.Gtk_Print_Settings object with the restored settings,
   --  or null if an error occurred. If the file could not be loaded then error
   --  is set to either a GFile_Error or GKey_File_Error. See
   --  Gtk.Print_Settings.To_File.
   --  Since: gtk+ 2.12
   --  "file_name": the filename to read the settings from

   procedure Gtk_New_From_Gvariant
      (Self    : out Gtk_Print_Settings;
       Variant : Glib.Variant.Gvariant);
   procedure Initialize_From_Gvariant
      (Self    : not null access Gtk_Print_Settings_Record'Class;
       Variant : Glib.Variant.Gvariant);
   --  Deserialize print settings from an a{sv} variant in the format produced
   --  by Gtk.Print_Settings.To_Gvariant.
   --  Since: gtk+ 3.22
   --  Initialize_From_Gvariant does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "variant": an a{sv} Glib.Variant.Gvariant

   function Gtk_Print_Settings_New_From_Gvariant
      (Variant : Glib.Variant.Gvariant) return Gtk_Print_Settings;
   --  Deserialize print settings from an a{sv} variant in the format produced
   --  by Gtk.Print_Settings.To_Gvariant.
   --  Since: gtk+ 3.22
   --  "variant": an a{sv} Glib.Variant.Gvariant

   procedure Gtk_New_From_Key_File
      (Self       : out Gtk_Print_Settings;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "");
   procedure Initialize_From_Key_File
      (Self       : not null access Gtk_Print_Settings_Record'Class;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "");
   --  Reads the print settings from the group Group_Name in Key_File. Returns
   --  a new Gtk.Print_Settings.Gtk_Print_Settings object with the restored
   --  settings, or null if an error occurred. If the file could not be loaded
   --  then error is set to either a GFile_Error or GKey_File_Error.
   --  Since: gtk+ 2.12
   --  Initialize_From_Key_File does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "key_file": the Gkey.File.Gkey_File to retrieve the settings from
   --  "group_name": the name of the group to use, or null to use the default
   --  "Print Settings"

   function Gtk_Print_Settings_New_From_Key_File
      (Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "") return Gtk_Print_Settings;
   --  Reads the print settings from the group Group_Name in Key_File. Returns
   --  a new Gtk.Print_Settings.Gtk_Print_Settings object with the restored
   --  settings, or null if an error occurred. If the file could not be loaded
   --  then error is set to either a GFile_Error or GKey_File_Error.
   --  Since: gtk+ 2.12
   --  "key_file": the Gkey.File.Gkey_File to retrieve the settings from
   --  "group_name": the name of the group to use, or null to use the default
   --  "Print Settings"

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_print_settings_get_type");

   -------------
   -- Methods --
   -------------

   function Copy
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk_Print_Settings;
   --  Copies a Gtk.Print_Settings.Gtk_Print_Settings object.
   --  Since: gtk+ 2.10

   procedure Foreach
      (Self : not null access Gtk_Print_Settings_Record;
       Func : Gtk_Print_Settings_Func);
   --  Calls Func for each key-value pair of Settings.
   --  Since: gtk+ 2.10
   --  "func": the function to call

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Foreach_User_Data is

      type Gtk_Print_Settings_Func is access procedure
        (Key       : UTF8_String;
         Value     : UTF8_String;
         User_Data : User_Data_Type);

      procedure Foreach
         (Self      : not null access Gtk.Print_Settings.Gtk_Print_Settings_Record'Class;
          Func      : Gtk_Print_Settings_Func;
          User_Data : User_Data_Type);
      --  Calls Func for each key-value pair of Settings.
      --  Since: gtk+ 2.10
      --  "func": the function to call
      --  "user_data": user data for Func

   end Foreach_User_Data;

   function Get
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String) return UTF8_String;
   --  Looks up the string value associated with Key.
   --  Since: gtk+ 2.10
   --  "key": a key

   function Get_Bool
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String) return Boolean;
   --  Returns the boolean represented by the value that is associated with
   --  Key.
   --  The string "true" represents True, any other string False.
   --  Since: gtk+ 2.10
   --  "key": a key

   procedure Set_Bool
      (Self  : not null access Gtk_Print_Settings_Record;
       Key   : UTF8_String;
       Value : Boolean);
   --  Sets Key to a boolean value.
   --  Since: gtk+ 2.10
   --  "key": a key
   --  "value": a boolean

   function Get_Collate
      (Self : not null access Gtk_Print_Settings_Record) return Boolean;
   --  Gets the value of GTK_PRINT_SETTINGS_COLLATE.
   --  Since: gtk+ 2.10

   procedure Set_Collate
      (Self    : not null access Gtk_Print_Settings_Record;
       Collate : Boolean);
   --  Sets the value of GTK_PRINT_SETTINGS_COLLATE.
   --  Since: gtk+ 2.10
   --  "collate": whether to collate the output

   function Get_Default_Source
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String;
   --  Gets the value of GTK_PRINT_SETTINGS_DEFAULT_SOURCE.
   --  Since: gtk+ 2.10

   procedure Set_Default_Source
      (Self           : not null access Gtk_Print_Settings_Record;
       Default_Source : UTF8_String);
   --  Sets the value of GTK_PRINT_SETTINGS_DEFAULT_SOURCE.
   --  Since: gtk+ 2.10
   --  "default_source": the default source

   function Get_Dither
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String;
   --  Gets the value of GTK_PRINT_SETTINGS_DITHER.
   --  Since: gtk+ 2.10

   procedure Set_Dither
      (Self   : not null access Gtk_Print_Settings_Record;
       Dither : UTF8_String);
   --  Sets the value of GTK_PRINT_SETTINGS_DITHER.
   --  Since: gtk+ 2.10
   --  "dither": the dithering that is used

   function Get_Double
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String) return Gdouble;
   --  Returns the double value associated with Key, or 0.
   --  Since: gtk+ 2.10
   --  "key": a key

   procedure Set_Double
      (Self  : not null access Gtk_Print_Settings_Record;
       Key   : UTF8_String;
       Value : Gdouble);
   --  Sets Key to a double value.
   --  Since: gtk+ 2.10
   --  "key": a key
   --  "value": a double value

   function Get_Double_With_Default
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String;
       Def  : Gdouble) return Gdouble;
   --  Returns the floating point number represented by the value that is
   --  associated with Key, or Default_Val if the value does not represent a
   --  floating point number.
   --  Floating point numbers are parsed with g_ascii_strtod.
   --  Since: gtk+ 2.10
   --  "key": a key
   --  "def": the default value

   function Get_Duplex
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Print_Duplex;
   --  Gets the value of GTK_PRINT_SETTINGS_DUPLEX.
   --  Since: gtk+ 2.10

   procedure Set_Duplex
      (Self   : not null access Gtk_Print_Settings_Record;
       Duplex : Gtk.Enums.Gtk_Print_Duplex);
   --  Sets the value of GTK_PRINT_SETTINGS_DUPLEX.
   --  Since: gtk+ 2.10
   --  "duplex": a Gtk.Enums.Gtk_Print_Duplex value

   function Get_Finishings
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String;
   --  Gets the value of GTK_PRINT_SETTINGS_FINISHINGS.
   --  Since: gtk+ 2.10

   procedure Set_Finishings
      (Self       : not null access Gtk_Print_Settings_Record;
       Finishings : UTF8_String);
   --  Sets the value of GTK_PRINT_SETTINGS_FINISHINGS.
   --  Since: gtk+ 2.10
   --  "finishings": the finishings

   function Get_Int
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String) return Glib.Gint;
   --  Returns the integer value of Key, or 0.
   --  Since: gtk+ 2.10
   --  "key": a key

   procedure Set_Int
      (Self  : not null access Gtk_Print_Settings_Record;
       Key   : UTF8_String;
       Value : Glib.Gint);
   --  Sets Key to an integer value.
   --  Since: gtk+ 2.10
   --  "key": a key
   --  "value": an integer

   function Get_Int_With_Default
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String;
       Def  : Glib.Gint) return Glib.Gint;
   --  Returns the value of Key, interpreted as an integer, or the default
   --  value.
   --  Since: gtk+ 2.10
   --  "key": a key
   --  "def": the default value

   function Get_Length
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Returns the value associated with Key, interpreted as a length. The
   --  returned value is converted to Units.
   --  Since: gtk+ 2.10
   --  "key": a key
   --  "unit": the unit of the return value

   procedure Set_Length
      (Self  : not null access Gtk_Print_Settings_Record;
       Key   : UTF8_String;
       Value : Gdouble;
       Unit  : Gtk.Enums.Gtk_Unit);
   --  Associates a length in units of Unit with Key.
   --  Since: gtk+ 2.10
   --  "key": a key
   --  "value": a length
   --  "unit": the unit of Length

   function Get_Media_Type
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String;
   --  Gets the value of GTK_PRINT_SETTINGS_MEDIA_TYPE.
   --  The set of media types is defined in PWG 5101.1-2002 PWG.
   --  Since: gtk+ 2.10

   procedure Set_Media_Type
      (Self       : not null access Gtk_Print_Settings_Record;
       Media_Type : UTF8_String);
   --  Sets the value of GTK_PRINT_SETTINGS_MEDIA_TYPE.
   --  The set of media types is defined in PWG 5101.1-2002 PWG.
   --  Since: gtk+ 2.10
   --  "media_type": the media type

   function Get_N_Copies
      (Self : not null access Gtk_Print_Settings_Record) return Glib.Gint;
   --  Gets the value of GTK_PRINT_SETTINGS_N_COPIES.
   --  Since: gtk+ 2.10

   procedure Set_N_Copies
      (Self       : not null access Gtk_Print_Settings_Record;
       Num_Copies : Glib.Gint);
   --  Sets the value of GTK_PRINT_SETTINGS_N_COPIES.
   --  Since: gtk+ 2.10
   --  "num_copies": the number of copies

   function Get_Number_Up
      (Self : not null access Gtk_Print_Settings_Record) return Glib.Gint;
   --  Gets the value of GTK_PRINT_SETTINGS_NUMBER_UP.
   --  Since: gtk+ 2.10

   procedure Set_Number_Up
      (Self      : not null access Gtk_Print_Settings_Record;
       Number_Up : Glib.Gint);
   --  Sets the value of GTK_PRINT_SETTINGS_NUMBER_UP.
   --  Since: gtk+ 2.10
   --  "number_up": the number of pages per sheet

   function Get_Number_Up_Layout
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Number_Up_Layout;
   --  Gets the value of GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT.
   --  Since: gtk+ 2.14

   procedure Set_Number_Up_Layout
      (Self             : not null access Gtk_Print_Settings_Record;
       Number_Up_Layout : Gtk.Enums.Gtk_Number_Up_Layout);
   --  Sets the value of GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT.
   --  Since: gtk+ 2.14
   --  "number_up_layout": a Gtk.Enums.Gtk_Number_Up_Layout value

   function Get_Orientation
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Page_Orientation;
   --  Get the value of GTK_PRINT_SETTINGS_ORIENTATION, converted to a
   --  Gtk.Enums.Gtk_Page_Orientation.
   --  Since: gtk+ 2.10

   procedure Set_Orientation
      (Self        : not null access Gtk_Print_Settings_Record;
       Orientation : Gtk.Enums.Gtk_Page_Orientation);
   --  Sets the value of GTK_PRINT_SETTINGS_ORIENTATION.
   --  Since: gtk+ 2.10
   --  "orientation": a page orientation

   function Get_Output_Bin
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String;
   --  Gets the value of GTK_PRINT_SETTINGS_OUTPUT_BIN.
   --  Since: gtk+ 2.10

   procedure Set_Output_Bin
      (Self       : not null access Gtk_Print_Settings_Record;
       Output_Bin : UTF8_String);
   --  Sets the value of GTK_PRINT_SETTINGS_OUTPUT_BIN.
   --  Since: gtk+ 2.10
   --  "output_bin": the output bin

   function Get_Page_Ranges
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk_Page_Range_Array;
   --  Gets the value of GTK_PRINT_SETTINGS_PAGE_RANGES.
   --  Since: gtk+ 2.10

   procedure Set_Page_Ranges
      (Self        : not null access Gtk_Print_Settings_Record;
       Page_Ranges : Gtk_Page_Range_Array);
   --  Sets the value of GTK_PRINT_SETTINGS_PAGE_RANGES.
   --  Since: gtk+ 2.10
   --  "page_ranges": an array of Gtk_Page_Ranges

   function Get_Page_Set
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Page_Set;
   --  Gets the value of GTK_PRINT_SETTINGS_PAGE_SET.
   --  Since: gtk+ 2.10

   procedure Set_Page_Set
      (Self     : not null access Gtk_Print_Settings_Record;
       Page_Set : Gtk.Enums.Gtk_Page_Set);
   --  Sets the value of GTK_PRINT_SETTINGS_PAGE_SET.
   --  Since: gtk+ 2.10
   --  "page_set": a Gtk.Enums.Gtk_Page_Set value

   function Get_Paper_Height
      (Self : not null access Gtk_Print_Settings_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the value of GTK_PRINT_SETTINGS_PAPER_HEIGHT, converted to Unit.
   --  Since: gtk+ 2.10
   --  "unit": the unit for the return value

   procedure Set_Paper_Height
      (Self   : not null access Gtk_Print_Settings_Record;
       Height : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit);
   --  Sets the value of GTK_PRINT_SETTINGS_PAPER_HEIGHT.
   --  Since: gtk+ 2.10
   --  "height": the paper height
   --  "unit": the units of Height

   function Get_Paper_Size
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Paper_Size.Gtk_Paper_Size;
   --  Gets the value of GTK_PRINT_SETTINGS_PAPER_FORMAT, converted to a
   --  Gtk.Paper_Size.Gtk_Paper_Size.
   --  Since: gtk+ 2.10

   procedure Set_Paper_Size
      (Self       : not null access Gtk_Print_Settings_Record;
       Paper_Size : Gtk.Paper_Size.Gtk_Paper_Size);
   --  Sets the value of GTK_PRINT_SETTINGS_PAPER_FORMAT,
   --  GTK_PRINT_SETTINGS_PAPER_WIDTH and GTK_PRINT_SETTINGS_PAPER_HEIGHT.
   --  Since: gtk+ 2.10
   --  "paper_size": a paper size

   function Get_Paper_Width
      (Self : not null access Gtk_Print_Settings_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the value of GTK_PRINT_SETTINGS_PAPER_WIDTH, converted to Unit.
   --  Since: gtk+ 2.10
   --  "unit": the unit for the return value

   procedure Set_Paper_Width
      (Self  : not null access Gtk_Print_Settings_Record;
       Width : Gdouble;
       Unit  : Gtk.Enums.Gtk_Unit);
   --  Sets the value of GTK_PRINT_SETTINGS_PAPER_WIDTH.
   --  Since: gtk+ 2.10
   --  "width": the paper width
   --  "unit": the units of Width

   function Get_Print_Pages
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Print_Pages;
   --  Gets the value of GTK_PRINT_SETTINGS_PRINT_PAGES.
   --  Since: gtk+ 2.10

   procedure Set_Print_Pages
      (Self  : not null access Gtk_Print_Settings_Record;
       Pages : Gtk.Enums.Gtk_Print_Pages);
   --  Sets the value of GTK_PRINT_SETTINGS_PRINT_PAGES.
   --  Since: gtk+ 2.10
   --  "pages": a Gtk.Enums.Gtk_Print_Pages value

   function Get_Printer
      (Self : not null access Gtk_Print_Settings_Record) return UTF8_String;
   --  Convenience function to obtain the value of GTK_PRINT_SETTINGS_PRINTER.
   --  Since: gtk+ 2.10

   procedure Set_Printer
      (Self    : not null access Gtk_Print_Settings_Record;
       Printer : UTF8_String);
   --  Convenience function to set GTK_PRINT_SETTINGS_PRINTER to Printer.
   --  Since: gtk+ 2.10
   --  "printer": the printer name

   function Get_Printer_Lpi
      (Self : not null access Gtk_Print_Settings_Record) return Gdouble;
   --  Gets the value of GTK_PRINT_SETTINGS_PRINTER_LPI.
   --  Since: gtk+ 2.16

   procedure Set_Printer_Lpi
      (Self : not null access Gtk_Print_Settings_Record;
       Lpi  : Gdouble);
   --  Sets the value of GTK_PRINT_SETTINGS_PRINTER_LPI.
   --  Since: gtk+ 2.16
   --  "lpi": the resolution in lpi (lines per inch)

   function Get_Quality
      (Self : not null access Gtk_Print_Settings_Record)
       return Gtk.Enums.Gtk_Print_Quality;
   --  Gets the value of GTK_PRINT_SETTINGS_QUALITY.
   --  Since: gtk+ 2.10

   procedure Set_Quality
      (Self    : not null access Gtk_Print_Settings_Record;
       Quality : Gtk.Enums.Gtk_Print_Quality);
   --  Sets the value of GTK_PRINT_SETTINGS_QUALITY.
   --  Since: gtk+ 2.10
   --  "quality": a Gtk.Enums.Gtk_Print_Quality value

   function Get_Resolution
      (Self : not null access Gtk_Print_Settings_Record) return Glib.Gint;
   --  Gets the value of GTK_PRINT_SETTINGS_RESOLUTION.
   --  Since: gtk+ 2.10

   procedure Set_Resolution
      (Self       : not null access Gtk_Print_Settings_Record;
       Resolution : Glib.Gint);
   --  Sets the values of GTK_PRINT_SETTINGS_RESOLUTION,
   --  GTK_PRINT_SETTINGS_RESOLUTION_X and GTK_PRINT_SETTINGS_RESOLUTION_Y.
   --  Since: gtk+ 2.10
   --  "resolution": the resolution in dpi

   function Get_Resolution_X
      (Self : not null access Gtk_Print_Settings_Record) return Glib.Gint;
   --  Gets the value of GTK_PRINT_SETTINGS_RESOLUTION_X.
   --  Since: gtk+ 2.16

   function Get_Resolution_Y
      (Self : not null access Gtk_Print_Settings_Record) return Glib.Gint;
   --  Gets the value of GTK_PRINT_SETTINGS_RESOLUTION_Y.
   --  Since: gtk+ 2.16

   function Get_Reverse
      (Self : not null access Gtk_Print_Settings_Record) return Boolean;
   --  Gets the value of GTK_PRINT_SETTINGS_REVERSE.
   --  Since: gtk+ 2.10

   procedure Set_Reverse
      (Self        : not null access Gtk_Print_Settings_Record;
       Gtk_Reverse : Boolean);
   --  Sets the value of GTK_PRINT_SETTINGS_REVERSE.
   --  Since: gtk+ 2.10
   --  "reverse": whether to reverse the output

   function Get_Scale
      (Self : not null access Gtk_Print_Settings_Record) return Gdouble;
   --  Gets the value of GTK_PRINT_SETTINGS_SCALE.
   --  Since: gtk+ 2.10

   procedure Set_Scale
      (Self  : not null access Gtk_Print_Settings_Record;
       Scale : Gdouble);
   --  Sets the value of GTK_PRINT_SETTINGS_SCALE.
   --  Since: gtk+ 2.10
   --  "scale": the scale in percent

   function Get_Use_Color
      (Self : not null access Gtk_Print_Settings_Record) return Boolean;
   --  Gets the value of GTK_PRINT_SETTINGS_USE_COLOR.
   --  Since: gtk+ 2.10

   procedure Set_Use_Color
      (Self      : not null access Gtk_Print_Settings_Record;
       Use_Color : Boolean);
   --  Sets the value of GTK_PRINT_SETTINGS_USE_COLOR.
   --  Since: gtk+ 2.10
   --  "use_color": whether to use color

   function Has_Key
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String) return Boolean;
   --  Returns True, if a value is associated with Key.
   --  Since: gtk+ 2.10
   --  "key": a key

   function Load_File
      (Self      : not null access Gtk_Print_Settings_Record;
       File_Name : UTF8_String) return Boolean;
   --  Reads the print settings from File_Name. If the file could not be
   --  loaded then error is set to either a GFile_Error or GKey_File_Error. See
   --  Gtk.Print_Settings.To_File.
   --  Since: gtk+ 2.14
   --  "file_name": the filename to read the settings from

   function Load_Key_File
      (Self       : not null access Gtk_Print_Settings_Record;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "") return Boolean;
   --  Reads the print settings from the group Group_Name in Key_File. If the
   --  file could not be loaded then error is set to either a GFile_Error or
   --  GKey_File_Error.
   --  Since: gtk+ 2.14
   --  "key_file": the Gkey.File.Gkey_File to retrieve the settings from
   --  "group_name": the name of the group to use, or null to use the default
   --  "Print Settings"

   procedure Set
      (Self  : not null access Gtk_Print_Settings_Record;
       Key   : UTF8_String;
       Value : UTF8_String := "");
   --  Associates Value with Key.
   --  Since: gtk+ 2.10
   --  "key": a key
   --  "value": a string value, or null

   procedure Set_Resolution_Xy
      (Self         : not null access Gtk_Print_Settings_Record;
       Resolution_X : Glib.Gint;
       Resolution_Y : Glib.Gint);
   --  Sets the values of GTK_PRINT_SETTINGS_RESOLUTION,
   --  GTK_PRINT_SETTINGS_RESOLUTION_X and GTK_PRINT_SETTINGS_RESOLUTION_Y.
   --  Since: gtk+ 2.16
   --  "resolution_x": the horizontal resolution in dpi
   --  "resolution_y": the vertical resolution in dpi

   function To_File
      (Self      : not null access Gtk_Print_Settings_Record;
       File_Name : UTF8_String) return Boolean;
   --  This function saves the print settings from Settings to File_Name. If
   --  the file could not be loaded then error is set to either a GFile_Error
   --  or GKey_File_Error.
   --  Since: gtk+ 2.12
   --  "file_name": the file to save to

   function To_Gvariant
      (Self : not null access Gtk_Print_Settings_Record)
       return Glib.Variant.Gvariant;
   --  Serialize print settings to an a{sv} variant.
   --  Since: gtk+ 3.22

   procedure To_Key_File
      (Self       : not null access Gtk_Print_Settings_Record;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "");
   --  This function adds the print settings from Settings to Key_File.
   --  Since: gtk+ 2.12
   --  "key_file": the Gkey.File.Gkey_File to save the print settings to
   --  "group_name": the group to add the settings to in Key_File, or null to
   --  use the default "Print Settings"

   procedure Unset
      (Self : not null access Gtk_Print_Settings_Record;
       Key  : UTF8_String);
   --  Removes any value associated with Key. This has the same effect as
   --  setting the value to null.
   --  Since: gtk+ 2.10
   --  "key": a key

end Gtk.Print_Settings;
