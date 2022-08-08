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
--  A Gdk.RGBA.Gdk_RGBA is used to represent a (possibly translucent) color,
--  in a way that is compatible with cairo's notion of color.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Glib.Values; use Glib.Values;

package Gdk.RGBA is

   type Gdk_RGBA is record
      Red : Gdouble;
      Green : Gdouble;
      Blue : Gdouble;
      Alpha : Gdouble;
   end record;
   pragma Convention (C, Gdk_RGBA);

   function From_Object_Free (B : access Gdk_RGBA) return Gdk_RGBA;
   pragma Inline (From_Object_Free);
   --  A Gdk.RGBA.Gdk_RGBA is used to represent a (possibly translucent)
   --  color, in a way that is compatible with cairo's notion of color.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_rgba_get_type");

   -------------
   -- Methods --
   -------------

   function Copy (Self : Gdk_RGBA) return Gdk_RGBA;
   pragma Import (C, Copy, "gdk_rgba_copy");
   --  Makes a copy of a Gdk.RGBA.Gdk_RGBA.
   --  The result must be freed through Gdk.RGBA.Free.
   --  Since: gtk+ 3.0

   function Equal (Self : Gdk_RGBA; P2 : Gdk_RGBA) return Boolean;
   --  Compares two RGBA colors.
   --  Since: gtk+ 3.0
   --  "p2": another Gdk.RGBA.Gdk_RGBA pointer

   procedure Free (Self : Gdk_RGBA);
   pragma Import (C, Free, "gdk_rgba_free");
   --  Frees a Gdk.RGBA.Gdk_RGBA created with Gdk.RGBA.Copy
   --  Since: gtk+ 3.0

   function Hash (Self : Gdk_RGBA) return Guint;
   pragma Import (C, Hash, "gdk_rgba_hash");
   --  A hash function suitable for using for a hash table that stores
   --  Gdk_RGBAs.
   --  Since: gtk+ 3.0

   procedure Parse
      (Self    : out Gdk_RGBA;
       Spec    : UTF8_String;
       Success : out Boolean);
   --  Parses a textual representation of a color, filling in the Red, Green,
   --  Blue and Alpha fields of the Rgba Gdk.RGBA.Gdk_RGBA.
   --  The string can be either one of: - A standard name (Taken from the X11
   --  rgb.txt file). - A hexadecimal value in the form "\rgb", "\rrggbb",
   --  "\rrrgggbbb" or "\rrrrggggbbbb" - A RGB color in the form "rgb(r,g,b)"
   --  (In this case the color will have full opacity) - A RGBA color in the
   --  form "rgba(r,g,b,a)"
   --  Where "r", "g", "b" and "a" are respectively the red, green, blue and
   --  alpha color values. In the last two cases, "r", "g", and "b" are either
   --  integers in the range 0 to 255 or percentage values in the range 0% to
   --  100%, and a is a floating point value in the range 0 to 1.
   --  Since: gtk+ 3.0
   --  "spec": the string specifying the color

   function To_String (Self : Gdk_RGBA) return UTF8_String;
   --  Returns a textual specification of Rgba in the form `rgb(r,g,b)` or
   --  `rgba(r g,b,a)`, where "r", "g", "b" and "a" represent the red, green,
   --  blue and alpha values respectively. "r", "g", and "b" are represented as
   --  integers in the range 0 to 255, and "a" is represented as a floating
   --  point value in the range 0 to 1.
   --  These string forms are string forms that are supported by the CSS3
   --  colors module, and can be parsed by Gdk.RGBA.Parse.
   --  Note that this string representation may lose some precision, since
   --  "r", "g" and "b" are represented as 8-bit integers. If this is a
   --  concern, you should use a different representation.
   --  Since: gtk+ 3.0

   ----------------------
   -- GtkAda additions --
   ----------------------

   type array_of_Gdk_RGBA is array (Natural range <>) of Gdk_RGBA;

   Null_RGBA  : constant Gdk_RGBA := (0.0, 0.0, 0.0, 0.0);
   Black_RGBA : constant Gdk_RGBA := (0.0, 0.0, 0.0, 1.0);
   White_RGBA : constant Gdk_RGBA := (1.0, 1.0, 1.0, 1.0);

   type Property_RGBA is new Glib.Property;
   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_RGBA;
      Value  : Gdk_RGBA);
   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_RGBA) return Gdk_RGBA;

   function Gdk_RGBA_Or_Null (Val : System.Address) return System.Address;
   --  Used for the GtkAda binding itself.
   --  Return either a Null_Address or a pointer to Val, depending on
   --  whether Val is the null value for the type.
   --  In all cases, Val is supposed to be an access to the type mentioned in
   --  the name of the subprogram.
   --  In Ada2012, these could be replaced with expression functions instead.

   procedure Set_Value (Value : in out Glib.Values.GValue; Val : Gdk_RGBA);
   function  Get_Value (Value : Glib.Values.GValue) return Gdk_RGBA;
   --  Conversion functions for storing a Gdk_RGBA as a GValue.

end Gdk.RGBA;
