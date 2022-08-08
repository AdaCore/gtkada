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
--  GtkSymbolicColor is a boxed type that represents a symbolic color. It is
--  the result of parsing a [color expression][gtkcssprovider-symbolic-colors].
--  To obtain the color represented by a GtkSymbolicColor, it has to be
--  resolved with gtk_symbolic_color_resolve, which replaces all symbolic color
--  references by the colors they refer to (in a given context) and evaluates
--  mix, shade and other expressions, resulting in a Gdk.RGBA.Gdk_RGBA value.
--
--  It is not normally necessary to deal directly with Gtk_Symbolic_Colors,
--  since they are mostly used behind the scenes by
--  Gtk.Style_Context.Gtk_Style_Context and Gtk.Css_Provider.Gtk_Css_Provider.
--
--  Gtk.Symbolic_Color.Gtk_Symbolic_Color is deprecated. Symbolic colors are
--  considered an implementation detail of GTK+.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.RGBA; use Gdk.RGBA;
with Glib;     use Glib;

package Gtk.Symbolic_Color is

   type Gtk_Symbolic_Color is new Glib.C_Boxed with null record;
   Null_Gtk_Symbolic_Color : constant Gtk_Symbolic_Color;

   function From_Object (Object : System.Address) return Gtk_Symbolic_Color;
   function From_Object_Free (B : access Gtk_Symbolic_Color'Class) return Gtk_Symbolic_Color;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New_Alpha
      (Self   : out Gtk_Symbolic_Color;
       Color  : Gtk_Symbolic_Color;
       Factor : Gdouble);
   --  Creates a symbolic color by modifying the relative alpha value of
   --  Color. A factor < 1.0 would resolve to a more transparent color, while >
   --  1.0 would resolve to a more opaque color.
   --  Since: gtk+ 3.0
   --  "color": another Gtk.Symbolic_Color.Gtk_Symbolic_Color
   --  "factor": factor to apply to Color alpha

   function Gtk_Symbolic_Color_New_Alpha
      (Color  : Gtk_Symbolic_Color;
       Factor : Gdouble) return Gtk_Symbolic_Color;
   --  Creates a symbolic color by modifying the relative alpha value of
   --  Color. A factor < 1.0 would resolve to a more transparent color, while >
   --  1.0 would resolve to a more opaque color.
   --  Since: gtk+ 3.0
   --  "color": another Gtk.Symbolic_Color.Gtk_Symbolic_Color
   --  "factor": factor to apply to Color alpha

   procedure Gtk_New_Literal
      (Self  : out Gtk_Symbolic_Color;
       Color : Gdk.RGBA.Gdk_RGBA);
   --  Creates a symbolic color pointing to a literal color.
   --  Since: gtk+ 3.0
   --  "color": a Gdk.RGBA.Gdk_RGBA

   function Gtk_Symbolic_Color_New_Literal
      (Color : Gdk.RGBA.Gdk_RGBA) return Gtk_Symbolic_Color;
   --  Creates a symbolic color pointing to a literal color.
   --  Since: gtk+ 3.0
   --  "color": a Gdk.RGBA.Gdk_RGBA

   procedure Gtk_New_Mix
      (Self   : out Gtk_Symbolic_Color;
       Color1 : Gtk_Symbolic_Color;
       Color2 : Gtk_Symbolic_Color;
       Factor : Gdouble);
   --  Creates a symbolic color defined as a mix of another two colors. a mix
   --  factor of 0 would resolve to Color1, while a factor of 1 would resolve
   --  to Color2.
   --  Since: gtk+ 3.0
   --  "color1": color to mix
   --  "color2": another color to mix
   --  "factor": mix factor

   function Gtk_Symbolic_Color_New_Mix
      (Color1 : Gtk_Symbolic_Color;
       Color2 : Gtk_Symbolic_Color;
       Factor : Gdouble) return Gtk_Symbolic_Color;
   --  Creates a symbolic color defined as a mix of another two colors. a mix
   --  factor of 0 would resolve to Color1, while a factor of 1 would resolve
   --  to Color2.
   --  Since: gtk+ 3.0
   --  "color1": color to mix
   --  "color2": another color to mix
   --  "factor": mix factor

   procedure Gtk_New_Name
      (Self : out Gtk_Symbolic_Color;
       Name : UTF8_String);
   --  Creates a symbolic color pointing to an unresolved named color. See
   --  Gtk.Style_Context.Lookup_Color and Gtk.Style_Properties.Lookup_Color.
   --  Since: gtk+ 3.0
   --  "name": color name

   function Gtk_Symbolic_Color_New_Name
      (Name : UTF8_String) return Gtk_Symbolic_Color;
   --  Creates a symbolic color pointing to an unresolved named color. See
   --  Gtk.Style_Context.Lookup_Color and Gtk.Style_Properties.Lookup_Color.
   --  Since: gtk+ 3.0
   --  "name": color name

   procedure Gtk_New_Shade
      (Self   : out Gtk_Symbolic_Color;
       Color  : Gtk_Symbolic_Color;
       Factor : Gdouble);
   --  Creates a symbolic color defined as a shade of another color. A factor
   --  > 1.0 would resolve to a brighter color, while < 1.0 would resolve to a
   --  darker color.
   --  Since: gtk+ 3.0
   --  "color": another Gtk.Symbolic_Color.Gtk_Symbolic_Color
   --  "factor": shading factor to apply to Color

   function Gtk_Symbolic_Color_New_Shade
      (Color  : Gtk_Symbolic_Color;
       Factor : Gdouble) return Gtk_Symbolic_Color;
   --  Creates a symbolic color defined as a shade of another color. A factor
   --  > 1.0 would resolve to a brighter color, while < 1.0 would resolve to a
   --  darker color.
   --  Since: gtk+ 3.0
   --  "color": another Gtk.Symbolic_Color.Gtk_Symbolic_Color
   --  "factor": shading factor to apply to Color

   procedure Gtk_New_Win32
      (Self        : out Gtk_Symbolic_Color;
       Theme_Class : UTF8_String;
       Id          : Glib.Gint);
   --  Creates a symbolic color based on the current win32 theme.
   --  Note that while this call is available on all platforms the actual
   --  value returned is not reliable on non-win32 platforms.
   --  Since: gtk+ 3.4
   --  "theme_class": The theme class to pull color from
   --  "id": The color id

   function Gtk_Symbolic_Color_New_Win32
      (Theme_Class : UTF8_String;
       Id          : Glib.Gint) return Gtk_Symbolic_Color;
   --  Creates a symbolic color based on the current win32 theme.
   --  Note that while this call is available on all platforms the actual
   --  value returned is not reliable on non-win32 platforms.
   --  Since: gtk+ 3.4
   --  "theme_class": The theme class to pull color from
   --  "id": The color id

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_symbolic_color_get_type");

   -------------
   -- Methods --
   -------------

   function Ref (Self : Gtk_Symbolic_Color) return Gtk_Symbolic_Color;
   pragma Obsolescent (Ref);
   --  Increases the reference count of Color
   --  Since: gtk+ 3.0
   --  Deprecated since 3.8, 1

   function To_String (Self : Gtk_Symbolic_Color) return UTF8_String;
   pragma Obsolescent (To_String);
   --  Converts the given Color to a string representation. This is useful
   --  both for debugging and for serialization of strings. The format of the
   --  string may change between different versions of GTK, but it is
   --  guaranteed that the GTK css parser is able to read the string and create
   --  the same symbolic color from it.
   --  Deprecated since 3.8, 1

   procedure Unref (Self : Gtk_Symbolic_Color);
   pragma Obsolescent (Unref);
   --  Decreases the reference count of Color, freeing its memory if the
   --  reference count reaches 0.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.8, 1

private

   Null_Gtk_Symbolic_Color : constant Gtk_Symbolic_Color := (Glib.C_Boxed with null record);

end Gtk.Symbolic_Color;
