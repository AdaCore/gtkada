------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
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
--
--  This package provides an interface to the color handling facilities in
--  gtk+. It is able to handle any kind of visual (monochrome, greyscale,
--  color with different depths, ...), but provides a common and easy
--  interface for all of them.
--
--  Getting the Red/Green/Blue components can be done through Parse, and is
--  actually recommended, since the exact color generally depends on the
--  visual your application is running on.
--
--  </description>
--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with Glib; use Glib;
with Glib.Object;
with Glib.Values;

package Gdk.Color is

   type Gdk_Color is private;
   --  A color to be displayed on the screen.
   --  Currently, GtkAda only supports the RGB standard, ie each color is
   --  set by its red, green and blue components.
   --  An extra field (Pixel) is the internal representation of the color,
   --  which is set once the color has been allocated.

   type Gdk_Color_Array is array (Natural range <>) of Gdk_Color;
   --  An array of colors.

   type Gdk_Color_Unconstrained_Array is array (Natural) of Gdk_Color;
   pragma Convention (C, Gdk_Color_Unconstrained_Array);
   --  An array of colors as returned by C. This is only useful in a few
   --  low-level subprograms that also pass the size as argument

   function To_Array
      (Colors   : Gdk_Color_Unconstrained_Array;
       N_Colors : Gint) return Gdk_Color_Array;
   --  Return a version of Colors easier to use in Ada.

   Null_Color : constant Gdk_Color;
   --  No color. For most functions, this will select the default color in the
   --  context, although this exact specification depends on the function you
   --  want to use.

   Wrong_Color : exception;
   --  Exception raised when some functions below could not find or allocate
   --  a color on the user's system.

   function Gdk_Color_Type return Glib.GType;
   --  Return the internal gtk+ types associated with a color

   ---------------------------------------------
   -- Setting/Getting the fields of Gdk_Color --
   ---------------------------------------------

   procedure Set_Rgb (Color : out Gdk_Color; Red, Green, Blue : Guint16);
   --  Modify the fields of the color.
   --  You then have to allocate the color with one of the Alloc* functions
   --  above.

   procedure Set_Pixel (Color : in out Gdk_Color; Pixel : Guint32);
   --  This function should almost never be used.

   function Red (Color : Gdk_Color) return Guint16;
   --  Return the Red field of Color.

   function Green (Color : Gdk_Color) return Guint16;
   --  Return the Green field of Color.

   function Blue (Color : Gdk_Color) return Guint16;
   --  Return the Blue field of Color.

   function Pixel (Color : Gdk_Color) return Guint32;
   --  Return the Pixel field of Color.

   ------------------------------------
   -- Creating and Destroying colors --
   ------------------------------------

   procedure Copy (Source : Gdk_Color; Destination : out Gdk_Color);
   --  Copy the Source color to Destination.

   function Parse (Spec : String) return Gdk_Color;
   --  Parse the string Spec, and get its Red/Green/Blue components.
   --  The color is not allocated, and you need to call Alloc_Color.
   --  If the string could not be parsed to an existing color, Wrong_Color is
   --  raised.
   --  The string can be one of :
   --
   --  - "RGB:FF/FF/FF" where the "FF" substrings are respectively the value
   --    of the red, green and blue components. Some other prefixes than RGB
   --    are defined in the X11 definition, please see some X11 documentation
   --    (or the man page XParseColor on unix systems).
   --
   --  - "color_name" which can be any color name defined in the file rgb.txt
   --    of the user's system. You should always check that Wrong_Color was not
   --    raised, in case the color was not known on the user's system. This
   --    string is case insensitive. Color names are not supported on Windows
   --    systems.

   function Equal (Colora, Colorb : Gdk_Color) return Boolean;
   function "=" (Colora, Colorb : Gdk_Color) return Boolean renames Equal;
   --  True if the Red, Green and Blue components of both colors are equal.

   --  <doc_ignore>

   function To_String (Color : Gdk_Color) return String;
   --  Return the RGB values of Color under the form "#RRGGBB".
   --  Directly usable by Parse, see above.

   ----------------
   -- Properties --
   ----------------
   --  See the package Glib.Properties for more information on how to
   --  use properties

   type Property_Gdk_Color is new Glib.Property;

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Gdk_Color;
      Value  : Gdk_Color);

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Gdk_Color) return Gdk_Color;

   procedure Set_Value (Value : in out Glib.Values.GValue; Val : Gdk_Color);
   function  Get_Value (Value : Glib.Values.GValue) return Gdk_Color;
   --  Store or retrieve a color from a value

   --  </doc_ignore>

   --  <doc_ignore>
   function Gdk_Color_Or_Null (Val : System.Address) return System.Address;
   --  Used for the GtkAda binding itself.
   --  Return either a Null_Address or a pointer to Val, depending on
   --  whether Val is the null value for the type.
   --  In all cases, Val is supposed to be an access to the type mentioned in
   --  the name of the subprogram.
   --  In Ada2012, these could be replaced with expression functions instead.
   --  </doc_ignore>

private
   type Gdk_Color is record
      Pixel : Guint32;
      Red   : Guint16;
      Green : Guint16;
      Blue  : Guint16;
   end record;
   pragma Convention (C, Gdk_Color);
   --  The fields are to be chosen between 0 and 65535, not 0 and 255!!!

   Null_Color : constant Gdk_Color := (Guint32'Last, 1, 0, 0);
   --  Note: in the implementation of GtkAda, everytime a color is used, it
   --  is important to test whether this is Null_Color or not. If it is, then
   --  System.Null_Address should be passed to C instead of Null_Color'Address
   --  so that gtk+ can provide a default value for colors.

   pragma Import (C, Gdk_Color_Type, "gdk_color_get_type");

   pragma Inline (Set_Rgb);
   pragma Inline (Set_Pixel);
   pragma Inline (Red);
   pragma Inline (Green);
   pragma Inline (Blue);
   pragma Inline (Pixel);
   pragma Inline (Set_Property);
   pragma Inline (Get_Property);
end Gdk.Color;
