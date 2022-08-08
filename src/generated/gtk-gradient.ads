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
--  GtkGradient is a boxed type that represents a gradient. It is the result
--  of parsing a [gradient expression][gtkcssprovider-gradients]. To obtain the
--  gradient represented by a GtkGradient, it has to be resolved with
--  Gtk.Gradient.Resolve, which replaces all symbolic color references by the
--  colors they refer to (in a given context) and constructs a cairo_pattern_t
--  value.
--
--  It is not normally necessary to deal directly with Gtk_Gradients, since
--  they are mostly used behind the scenes by
--  Gtk.Style_Context.Gtk_Style_Context and Gtk.Css_Provider.Gtk_Css_Provider.
--
--  Gtk.Gradient.Gtk_Gradient is deprecated. It was used internally by GTK's
--  CSS engine to represent gradients. As its handling is not conforming to
--  modern web standards, it is not used anymore. If you want to use gradients
--  in your own code, please use Cairo directly.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;                use Cairo;
with Glib;                 use Glib;
with Gtk.Style_Context;    use Gtk.Style_Context;
with Gtk.Style_Properties; use Gtk.Style_Properties;
with Gtk.Symbolic_Color;   use Gtk.Symbolic_Color;

package Gtk.Gradient is

   type Gtk_Gradient is new Glib.C_Boxed with null record;
   Null_Gtk_Gradient : constant Gtk_Gradient;

   function From_Object (Object : System.Address) return Gtk_Gradient;
   function From_Object_Free (B : access Gtk_Gradient'Class) return Gtk_Gradient;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New_Linear
      (Self : out Gtk_Gradient;
       X0   : Gdouble;
       Y0   : Gdouble;
       X1   : Gdouble;
       Y1   : Gdouble);
   --  Creates a new linear gradient along the line defined by (x0, y0) and
   --  (x1, y1). Before using the gradient a number of stop colors must be
   --  added through Gtk.Gradient.Add_Color_Stop.
   --  Since: gtk+ 3.0
   --  "x0": X coordinate of the starting point
   --  "y0": Y coordinate of the starting point
   --  "x1": X coordinate of the end point
   --  "y1": Y coordinate of the end point

   function Gtk_Gradient_New_Linear
      (X0 : Gdouble;
       Y0 : Gdouble;
       X1 : Gdouble;
       Y1 : Gdouble) return Gtk_Gradient;
   --  Creates a new linear gradient along the line defined by (x0, y0) and
   --  (x1, y1). Before using the gradient a number of stop colors must be
   --  added through Gtk.Gradient.Add_Color_Stop.
   --  Since: gtk+ 3.0
   --  "x0": X coordinate of the starting point
   --  "y0": Y coordinate of the starting point
   --  "x1": X coordinate of the end point
   --  "y1": Y coordinate of the end point

   procedure Gtk_New_Radial
      (Self    : out Gtk_Gradient;
       X0      : Gdouble;
       Y0      : Gdouble;
       Radius0 : Gdouble;
       X1      : Gdouble;
       Y1      : Gdouble;
       Radius1 : Gdouble);
   --  Creates a new radial gradient along the two circles defined by (x0, y0,
   --  radius0) and (x1, y1, radius1). Before using the gradient a number of
   --  stop colors must be added through Gtk.Gradient.Add_Color_Stop.
   --  Since: gtk+ 3.0
   --  "x0": X coordinate of the start circle
   --  "y0": Y coordinate of the start circle
   --  "radius0": radius of the start circle
   --  "x1": X coordinate of the end circle
   --  "y1": Y coordinate of the end circle
   --  "radius1": radius of the end circle

   function Gtk_Gradient_New_Radial
      (X0      : Gdouble;
       Y0      : Gdouble;
       Radius0 : Gdouble;
       X1      : Gdouble;
       Y1      : Gdouble;
       Radius1 : Gdouble) return Gtk_Gradient;
   --  Creates a new radial gradient along the two circles defined by (x0, y0,
   --  radius0) and (x1, y1, radius1). Before using the gradient a number of
   --  stop colors must be added through Gtk.Gradient.Add_Color_Stop.
   --  Since: gtk+ 3.0
   --  "x0": X coordinate of the start circle
   --  "y0": Y coordinate of the start circle
   --  "radius0": radius of the start circle
   --  "x1": X coordinate of the end circle
   --  "y1": Y coordinate of the end circle
   --  "radius1": radius of the end circle

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_gradient_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Color_Stop
      (Self   : Gtk_Gradient;
       Offset : Gdouble;
       Color  : Gtk.Symbolic_Color.Gtk_Symbolic_Color);
   pragma Obsolescent (Add_Color_Stop);
   --  Adds a stop color to Gradient.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.8, 1
   --  "offset": offset for the color stop
   --  "color": color to use

   function Ref (Self : Gtk_Gradient) return Gtk_Gradient;
   pragma Obsolescent (Ref);
   --  Increases the reference count of Gradient.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.8, 1

   function Resolve
      (Self              : Gtk_Gradient;
       Props             : not null access Gtk.Style_Properties.Gtk_Style_Properties_Record'Class;
       Resolved_Gradient : access Cairo.Cairo_Pattern) return Boolean;
   pragma Obsolescent (Resolve);
   --  If Gradient is resolvable, Resolved_Gradient will be filled in with the
   --  resolved gradient as a cairo_pattern_t, and True will be returned.
   --  Generally, if Gradient can't be resolved, it is due to it being defined
   --  on top of a named color that doesn't exist in Props.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.8, 1
   --  "props": Gtk.Style_Properties.Gtk_Style_Properties to use when
   --  resolving named colors
   --  "resolved_gradient": return location for the resolved pattern

   function Resolve_For_Context
      (Self    : Gtk_Gradient;
       Context : not null access Gtk.Style_Context.Gtk_Style_Context_Record'Class)
       return Cairo.Cairo_Pattern;

   function To_String (Self : Gtk_Gradient) return UTF8_String;
   pragma Obsolescent (To_String);
   --  Creates a string representation for Gradient that is suitable for using
   --  in GTK CSS files.
   --  Deprecated since 3.8, 1

   procedure Unref (Self : Gtk_Gradient);
   pragma Obsolescent (Unref);
   --  Decreases the reference count of Gradient, freeing its memory if the
   --  reference count reaches 0.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.8, 1

private

   Null_Gtk_Gradient : constant Gtk_Gradient := (Glib.C_Boxed with null record);

end Gtk.Gradient;
