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

--  GtkPaperSize handles paper sizes. It uses the standard called [PWG
--  5101.1-2002 PWG: Standard for Media Standardized
--  Names](http://www.pwg.org/standards.html) to name the paper sizes (and to
--  get the data for the page sizes). In addition to standard paper sizes,
--  GtkPaperSize allows to construct custom paper sizes with arbitrary
--  dimensions.
--
--  The Gtk.Paper_Size.Gtk_Paper_Size object stores not only the dimensions
--  (width and height) of a paper size and its name, it also provides default
--  [print margins][print-margins].
--
--  Printing support has been added in GTK+ 2.10.

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Glist;    use Glib.Glist;
with Glib.Key_File; use Glib.Key_File;
with Glib.Variant;  use Glib.Variant;
with Gtk.Enums;     use Gtk.Enums;

package Gtk.Paper_Size is

   type Gtk_Paper_Size is new Glib.C_Boxed with null record;
   Null_Gtk_Paper_Size : constant Gtk_Paper_Size;

   function From_Object (Object : System.Address) return Gtk_Paper_Size;
   function From_Object_Free (B : access Gtk_Paper_Size'Class) return Gtk_Paper_Size;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Widget : out Gtk_Paper_Size; Name : UTF8_String := "");
   --  Creates a new Gtk.Paper_Size.Gtk_Paper_Size object by parsing a [PWG
   --  5101.1-2002](ftp://ftp.pwg.org/pub/pwg/candidates/cs-pwgmsn10-20020226-5101.1.pdf)
   --  paper name.
   --  If Name is null, the default paper size is returned, see
   --  Gtk.Paper_Size.Get_Default.
   --  Since: gtk+ 2.10
   --  @param Name a paper size name, or null

   function Gtk_Paper_Size_New
      (Name : UTF8_String := "") return Gtk_Paper_Size;
   --  Creates a new Gtk.Paper_Size.Gtk_Paper_Size object by parsing a [PWG
   --  5101.1-2002](ftp://ftp.pwg.org/pub/pwg/candidates/cs-pwgmsn10-20020226-5101.1.pdf)
   --  paper name.
   --  If Name is null, the default paper size is returned, see
   --  Gtk.Paper_Size.Get_Default.
   --  Since: gtk+ 2.10
   --  @param Name a paper size name, or null

   procedure Gtk_New_Custom
      (Widget       : out Gtk_Paper_Size;
       Name         : UTF8_String;
       Display_Name : UTF8_String;
       Width        : Gdouble;
       Height       : Gdouble;
       Unit         : Gtk.Enums.Gtk_Unit);
   --  Creates a new Gtk.Paper_Size.Gtk_Paper_Size object with the given
   --  parameters.
   --  Since: gtk+ 2.10
   --  @param Name the paper name
   --  @param Display_Name the human-readable name
   --  @param Width the paper width, in units of Unit
   --  @param Height the paper height, in units of Unit
   --  @param Unit the unit for Width and Height. not Gtk.Enums.None.

   function Gtk_Paper_Size_New_Custom
      (Name         : UTF8_String;
       Display_Name : UTF8_String;
       Width        : Gdouble;
       Height       : Gdouble;
       Unit         : Gtk.Enums.Gtk_Unit) return Gtk_Paper_Size;
   --  Creates a new Gtk.Paper_Size.Gtk_Paper_Size object with the given
   --  parameters.
   --  Since: gtk+ 2.10
   --  @param Name the paper name
   --  @param Display_Name the human-readable name
   --  @param Width the paper width, in units of Unit
   --  @param Height the paper height, in units of Unit
   --  @param Unit the unit for Width and Height. not Gtk.Enums.None.

   procedure Gtk_New_From_Gvariant
      (Widget  : out Gtk_Paper_Size;
       Variant : Glib.Variant.Gvariant);
   --  Deserialize a paper size from an a{sv} variant in the format produced
   --  by Gtk.Paper_Size.To_Gvariant.
   --  Since: gtk+ 3.22
   --  @param Variant an a{sv} Glib.Variant.Gvariant

   function Gtk_Paper_Size_New_From_Gvariant
      (Variant : Glib.Variant.Gvariant) return Gtk_Paper_Size;
   --  Deserialize a paper size from an a{sv} variant in the format produced
   --  by Gtk.Paper_Size.To_Gvariant.
   --  Since: gtk+ 3.22
   --  @param Variant an a{sv} Glib.Variant.Gvariant

   procedure Gtk_New_From_Ipp
      (Widget   : out Gtk_Paper_Size;
       Ipp_Name : UTF8_String;
       Width    : Gdouble;
       Height   : Gdouble);
   --  Creates a new Gtk.Paper_Size.Gtk_Paper_Size object by using IPP
   --  information.
   --  If Ipp_Name is not a recognized paper name, Width and Height are used
   --  to construct a custom Gtk.Paper_Size.Gtk_Paper_Size object.
   --  Since: gtk+ 3.16
   --  @param Ipp_Name an IPP paper name
   --  @param Width the paper width, in points
   --  @param Height the paper height in points

   function Gtk_Paper_Size_New_From_Ipp
      (Ipp_Name : UTF8_String;
       Width    : Gdouble;
       Height   : Gdouble) return Gtk_Paper_Size;
   --  Creates a new Gtk.Paper_Size.Gtk_Paper_Size object by using IPP
   --  information.
   --  If Ipp_Name is not a recognized paper name, Width and Height are used
   --  to construct a custom Gtk.Paper_Size.Gtk_Paper_Size object.
   --  Since: gtk+ 3.16
   --  @param Ipp_Name an IPP paper name
   --  @param Width the paper width, in points
   --  @param Height the paper height in points

   procedure Gtk_New_From_Key_File
      (Widget     : out Gtk_Paper_Size;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "");
   --  Reads a paper size from the group Group_Name in the key file Key_File.
   --  Since: gtk+ 2.12
   --  @param Key_File the Gkey.File.Gkey_File to retrieve the papersize from
   --  @param Group_Name the name of the group in the key file to read, or
   --  null to read the first group

   function Gtk_Paper_Size_New_From_Key_File
      (Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "") return Gtk_Paper_Size;
   --  Reads a paper size from the group Group_Name in the key file Key_File.
   --  Since: gtk+ 2.12
   --  @param Key_File the Gkey.File.Gkey_File to retrieve the papersize from
   --  @param Group_Name the name of the group in the key file to read, or
   --  null to read the first group

   procedure Gtk_New_From_Ppd
      (Widget           : out Gtk_Paper_Size;
       Ppd_Name         : UTF8_String;
       Ppd_Display_Name : UTF8_String;
       Width            : Gdouble;
       Height           : Gdouble);
   --  Creates a new Gtk.Paper_Size.Gtk_Paper_Size object by using PPD
   --  information.
   --  If Ppd_Name is not a recognized PPD paper name, Ppd_Display_Name, Width
   --  and Height are used to construct a custom Gtk.Paper_Size.Gtk_Paper_Size
   --  object.
   --  Since: gtk+ 2.10
   --  @param Ppd_Name a PPD paper name
   --  @param Ppd_Display_Name the corresponding human-readable name
   --  @param Width the paper width, in points
   --  @param Height the paper height in points

   function Gtk_Paper_Size_New_From_Ppd
      (Ppd_Name         : UTF8_String;
       Ppd_Display_Name : UTF8_String;
       Width            : Gdouble;
       Height           : Gdouble) return Gtk_Paper_Size;
   --  Creates a new Gtk.Paper_Size.Gtk_Paper_Size object by using PPD
   --  information.
   --  If Ppd_Name is not a recognized PPD paper name, Ppd_Display_Name, Width
   --  and Height are used to construct a custom Gtk.Paper_Size.Gtk_Paper_Size
   --  object.
   --  Since: gtk+ 2.10
   --  @param Ppd_Name a PPD paper name
   --  @param Ppd_Display_Name the corresponding human-readable name
   --  @param Width the paper width, in points
   --  @param Height the paper height in points

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_paper_size_get_type");

   -------------
   -- Methods --
   -------------

   function Copy (Widget : Gtk_Paper_Size) return Gtk_Paper_Size;
   --  Copies an existing Gtk.Paper_Size.Gtk_Paper_Size.
   --  Since: gtk+ 2.10
   --  @return a copy of Other

   procedure Free (Widget : Gtk_Paper_Size);
   --  Free the given Gtk.Paper_Size.Gtk_Paper_Size object.
   --  Since: gtk+ 2.10

   function Get_Default_Bottom_Margin
      (Widget : Gtk_Paper_Size;
       Unit   : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the default bottom margin for the Gtk.Paper_Size.Gtk_Paper_Size.
   --  Since: gtk+ 2.10
   --  @param Unit the unit for the return value, not Gtk.Enums.None
   --  @return the default bottom margin

   function Get_Default_Left_Margin
      (Widget : Gtk_Paper_Size;
       Unit   : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the default left margin for the Gtk.Paper_Size.Gtk_Paper_Size.
   --  Since: gtk+ 2.10
   --  @param Unit the unit for the return value, not Gtk.Enums.None
   --  @return the default left margin

   function Get_Default_Right_Margin
      (Widget : Gtk_Paper_Size;
       Unit   : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the default right margin for the Gtk.Paper_Size.Gtk_Paper_Size.
   --  Since: gtk+ 2.10
   --  @param Unit the unit for the return value, not Gtk.Enums.None
   --  @return the default right margin

   function Get_Default_Top_Margin
      (Widget : Gtk_Paper_Size;
       Unit   : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the default top margin for the Gtk.Paper_Size.Gtk_Paper_Size.
   --  Since: gtk+ 2.10
   --  @param Unit the unit for the return value, not Gtk.Enums.None
   --  @return the default top margin

   function Get_Display_Name (Widget : Gtk_Paper_Size) return UTF8_String;
   --  Gets the human-readable name of the Gtk.Paper_Size.Gtk_Paper_Size.
   --  Since: gtk+ 2.10
   --  @return the human-readable name of Size

   function Get_Height
      (Widget : Gtk_Paper_Size;
       Unit   : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the paper height of the Gtk.Paper_Size.Gtk_Paper_Size, in units of
   --  Unit.
   --  Since: gtk+ 2.10
   --  @param Unit the unit for the return value, not Gtk.Enums.None
   --  @return the paper height

   function Get_Name (Widget : Gtk_Paper_Size) return UTF8_String;
   --  Gets the name of the Gtk.Paper_Size.Gtk_Paper_Size.
   --  Since: gtk+ 2.10
   --  @return the name of Size

   function Get_Ppd_Name (Widget : Gtk_Paper_Size) return UTF8_String;
   --  Gets the PPD name of the Gtk.Paper_Size.Gtk_Paper_Size, which may be
   --  null.
   --  Since: gtk+ 2.10
   --  @return the PPD name of Size

   function Get_Width
      (Widget : Gtk_Paper_Size;
       Unit   : Gtk.Enums.Gtk_Unit) return Gdouble;
   --  Gets the paper width of the Gtk.Paper_Size.Gtk_Paper_Size, in units of
   --  Unit.
   --  Since: gtk+ 2.10
   --  @param Unit the unit for the return value, not Gtk.Enums.None
   --  @return the paper width

   function Is_Custom (Widget : Gtk_Paper_Size) return Boolean;
   --  Returns True if Size is not a standard paper size.
   --  @return whether Size is a custom paper size.

   function Is_Equal
      (Widget : Gtk_Paper_Size;
       Size2  : Gtk_Paper_Size) return Boolean;
   --  Compares two Gtk.Paper_Size.Gtk_Paper_Size objects.
   --  Since: gtk+ 2.10
   --  @param Size2 another Gtk.Paper_Size.Gtk_Paper_Size object
   --  @return True, if Size1 and Size2 represent the same paper size

   function Is_Ipp (Widget : Gtk_Paper_Size) return Boolean;
   --  Returns True if Size is an IPP standard paper size.
   --  @return whether Size is not an IPP custom paper size.

   procedure Set_Size
      (Widget : Gtk_Paper_Size;
       Width  : Gdouble;
       Height : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit);
   --  Changes the dimensions of a Size to Width x Height.
   --  Since: gtk+ 2.10
   --  @param Width the new width in units of Unit
   --  @param Height the new height in units of Unit
   --  @param Unit the unit for Width and Height

   function To_Gvariant
      (Widget : Gtk_Paper_Size) return Glib.Variant.Gvariant;
   --  Serialize a paper size to an a{sv} variant.
   --  Since: gtk+ 3.22
   --  @return a new, floating, Glib.Variant.Gvariant

   procedure To_Key_File
      (Widget     : Gtk_Paper_Size;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String);
   --  This function adds the paper size from Size to Key_File.
   --  Since: gtk+ 2.12
   --  @param Key_File the Gkey.File.Gkey_File to save the paper size to
   --  @param Group_Name the group to add the settings to in Key_File

   ----------------------
   -- GtkAda additions --
   ----------------------

   --  Common names, from PWG 5101.1-2002 PWG: Standard for Media Standardized
   --  Names
   Gtk_Paper_Name_A3        : constant String := "iso_a3";
   Gtk_Paper_Name_A4        : constant String := "iso_a4";
   Gtk_Paper_Name_A5        : constant String := "iso_a5";
   Gtk_Paper_Name_B5        : constant String := "iso_b5";
   Gtk_Paper_Name_Letter    : constant String := "na_letter";
   Gtk_Paper_Name_Executive : constant String := "na_executive";
   Gtk_Paper_Name_Legal     : constant String := "na_legal";

   ---------------
   -- Functions --
   ---------------

   function Convert (R : Gtk.Paper_Size.Gtk_Paper_Size) return System.Address;
   function Convert (R : System.Address) return Gtk.Paper_Size.Gtk_Paper_Size;
   package Gtk_Paper_Size_Glist is new Generic_List (Gtk.Paper_Size.Gtk_Paper_Size);

   function Get_Default return UTF8_String;
   --  Returns the name of the default paper size, which depends on the
   --  current locale.
   --  Since: gtk+ 2.10
   --  @return the name of the default paper size. The string is owned by GTK+
   --  and should not be modified.

   function Get_Paper_Sizes
      (Include_Custom : Boolean) return Gtk_Paper_Size_Glist.Glist;
   --  Creates a list of known paper sizes.
   --  Since: gtk+ 2.12
   --  @param Include_Custom whether to include custom paper sizes as defined
   --  in the page setup dialog

private

   Null_Gtk_Paper_Size : constant Gtk_Paper_Size := (Glib.C_Boxed with null record);

end Gtk.Paper_Size;
