-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  This package does not provide any new widget.
--  Rather, it gives a set of subprogram for postscript font handling,
--  and is used by most of the Gtk.Extra.* packages.
--
--  The following is the name of the 35 default Adobe fonts:
--   - "Times-Roman",
--   - "Times-Italic",
--   - "Times-Bold",
--   - "Times-BoldItalic",
--   - "AvantGarde-Book",
--   - "AvantGarde-BookOblique",
--   - "AvantGarde-Demi",
--   - "AvantGarde-DemiOblique",
--   - "Bookman-Light",
--   - "Bookman-LightItalic",
--   - "Bookman-Demi",
--   - "Bookman-DemiItalic",
--   - "Courier",
--   - "Courier-Oblique",
--   - "Courier-Bold",
--   - "Courier-BoldOblique",
--   - "Helvetica",
--   - "Helvetica-Oblique",
--   - "Helvetica-Bold",
--   - "Helvetica-BoldOblique",
--   - "Helvetica-Narrow",
--   - "Helvetica-Narrow-Oblique",
--   - "Helvetica-Narrow-Bold",
--   - "Helvetica-Narrow-BoldOblique",
--   - "NewCenturySchoolbook-Roman",
--   - "NewCenturySchoolbook-Italic",
--   - "NewCenturySchoolbook-Bold",
--   - "NewCenturySchoolbook-BoldItalic",
--   - "Palatino-Roman",
--   - "Palatino-Italic",
--   - "Palatino-Bold",
--   - "Palatino-BoldItalic",
--   - "Symbol",
--   - "ZapfChancery-MediumItalic",
--   - "ZapfDingbats",
--  </description>
--  <c_version>gtk+extra0.99.5</c_version>

with Gdk.Font;
with Gtkada.Types;

package Gtk.Extra.PsFont is

   type Gtk_PsFont is new Gdk.C_Proxy;
   --  A postscript font.

   function Getfont (Name   : in String) return Gtk_PsFont;
   --  Return the font structure associated with the font Name.

   function Get_Gdkfont (Name   : in String;
                         Height : in Gint)
                        return      Gdk.Font.Gdk_Font;
   --  Return the Gdk_Font that matches the postscript font Name.
   --  Null_Font is returned if the corresponding font is not found
   --  on your system.

   function Get_Psfontname (Name   : in String) return String;
   --  Return the real postscript name of the font.
   --  In most cases this is the same as Name, except for a few cases.

   procedure Add (Fontname : in String;
                  Psname   : in String;
                  Family   : in String;
                  Xstring  : in Gtkada.Types.Chars_Ptr_Array;
                  Italic   : in Boolean;
                  Bold     : in Boolean);
   --  Add a new font to the list of recognized fonts.
   --  The items in Xstring should be the standard X11 names for the fonts
   --  that match that postscript font (and are used to convert from a
   --  Gtk_PsFont to a Gdk_Font. This should be a Null terminated array.

   function Find_By_Family (Name   : in String;
                            Italic : in Boolean;
                            Bold   : in Boolean)
                           return      Gtk_PsFont;
   --  Return the first postscript font whose family is Name and whose
   --  attributes match Italic and Bold.

   --  procedure Get_Families (Family : out Gtkada.Types.Chars_Ptr_Array;
   --                          Numf   : out Gint);
   --  Return the list of all the font families recognized in that package.

   function Get_Psname (Font : Gtk_PsFont) return String;
   --  Return the name of the font.
   --  This name can be give to Get_GdkFont above to get a font that can be
   --  used anywhere in GtkAda.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined in this package:
   --  </signals>

end Gtk.Extra.PsFont;
