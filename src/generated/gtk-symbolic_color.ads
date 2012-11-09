------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
--  GtkSymbolicColor is a boxed type that represents a symbolic color. It is
--  the result of parsing a <link
--  linkend="gtkcssprovider-symbolic-colors">color expression</link>. To obtain
--  the color represented by a GtkSymbolicColor, it has to be resolved with
--  gtk_symbolic_color_resolve, which replaces all symbolic color references by
--  the colors they refer to (in a given context) and evaluates mix, shade and
--  other expressions, resulting in a Gdk.RGBA.Gdk_RGBA value.
--
--  It is not normally necessary to deal directly with Gtk_Symbolic_Colors,
--  since they are mostly used behind the scenes by
--  Gtk.Style_Context.Gtk_Style_Context and Gtk.Css_Provider.Gtk_Css_Provider.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.RGBA; use Gdk.RGBA;
with Glib;     use Glib;

package Gtk.Symbolic_Color is

   type Gtk_Symbolic_Color is new Glib.C_Boxed with null record;

   function From_Object (Object : System.Address) return Gtk_Symbolic_Color;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New_Literal
      (Self  : out Gtk_Symbolic_Color;
       Color : Gdk.RGBA.Gdk_RGBA);
   --  Creates a symbolic color pointing to a literal color.
   --  Since: gtk+ 3.0
   --  "color": a Gdk.RGBA.Gdk_RGBA

   procedure Gtk_New_Name
      (Self : out Gtk_Symbolic_Color;
       Name : UTF8_String);
   --  Creates a symbolic color pointing to an unresolved named color. See
   --  Gtk.Style_Context.Lookup_Color and Gtk.Style_Properties.Lookup_Color.
   --  Since: gtk+ 3.0
   --  "name": color name

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_symbolic_color_get_type");

   -------------
   -- Methods --
   -------------

   function New_Alpha
      (Self   : Gtk_Symbolic_Color;
       Factor : Gdouble) return Gtk_Symbolic_Color;
   --  Creates a symbolic color by modifying the relative alpha value of
   --  Color. A factor < 1.0 would resolve to a more transparent color, while >
   --  1.0 would resolve to a more opaque color.
   --  Since: gtk+ 3.0
   --  "factor": factor to apply to Color alpha

   function New_Mix
      (Self   : Gtk_Symbolic_Color;
       Color2 : Gtk_Symbolic_Color;
       Factor : Gdouble) return Gtk_Symbolic_Color;
   --  Creates a symbolic color defined as a mix of another two colors. a mix
   --  factor of 0 would resolve to Color1, while a factor of 1 would resolve
   --  to Color2.
   --  Since: gtk+ 3.0
   --  "color2": another color to mix
   --  "factor": mix factor

   function New_Shade
      (Self   : Gtk_Symbolic_Color;
       Factor : Gdouble) return Gtk_Symbolic_Color;
   --  Creates a symbolic color defined as a shade of another color. A factor
   --  > 1.0 would resolve to a brighter color, while < 1.0 would resolve to a
   --  darker color.
   --  Since: gtk+ 3.0
   --  "factor": shading factor to apply to Color

   function Ref (Self : Gtk_Symbolic_Color) return Gtk_Symbolic_Color;
   --  Increases the reference count of Color
   --  Since: gtk+ 3.0

   function To_String (Self : Gtk_Symbolic_Color) return UTF8_String;
   --  Converts the given Color to a string representation. This is useful
   --  both for debugging and for serialization of strings. The format of the
   --  string may change between different versions of GTK, but it is
   --  guaranteed that the GTK css parser is able to read the string and create
   --  the same symbolic color from it.

   procedure Unref (Self : Gtk_Symbolic_Color);
   --  Decreases the reference count of Color, freeing its memory if the
   --  reference count reaches 0.
   --  Since: gtk+ 3.0

end Gtk.Symbolic_Color;
