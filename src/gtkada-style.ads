------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2011-2013, AdaCore                     --
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
--  Utility functions that mimic Gtk.Style drawing methods, but using Cairo
--  instead of Gdk. This allow smooth transition to Gtk3 that will get rid
--  of those Gdk drawing methods and only use Cairo.
--  </description>
--  <group>Configuration and Themes</group>

with Glib;
with Cairo;
with Pango.Layout;
with Gdk.Color;
with Gdk.Device;
with Gdk.Pixbuf;
with Gdk.RGBA;
with Gdk.Types;
with Gtk.Enums;
with Gtk.Style_Provider;
with Gtk.Widget;

package Gtkada.Style is

   --------------------
   -- Color handling --
   --------------------

   subtype Cairo_Color_Val is Glib.Gdouble range 0.0 .. 1.0;
   --  In cairo, the color components are expressed as percents.

   subtype Cairo_Color is Gdk.RGBA.Gdk_RGBA;

   type HSV_Color is record
      H, S, V, A : Glib.Gdouble;
   end record;
   --  Used when manipulating Cairo color. The HSV color space is useful when
   --  wanting to shade a color (change it's value), (de)saturate it, or modify
   --  its hue.

   type HSLA_Color is record
      Hue        : Glib.Gdouble;
      Saturation : Glib.Gdouble;
      Lightness  : Glib.Gdouble;
      Alpha      : Glib.Gdouble;
   end record;
   --  The Hue is the colour's position on the colour wheel, expressed in
   --  degrees from 0° to 359°, representing the 360° of the wheel; 0°
   --  being red, 180° being red's opposite colour cyan, and so on.
   --  The mapping is:    0.0 => 0°
   --                     1.0 => 360°
   --
   --  Saturation is the intensity of the colour, how dull or bright it is.
   --  The lower the saturation, the duller (greyer) the colour looks. This is
   --  expressed as a percentage, 100% being full saturation, the brightest,
   --  and 0% being no saturation, grey.
   --
   --  Lightness is how light the colour is. Slightly different to saturation.
   --  The more white in the colour the higher its Lightness value, the more
   --  black, the lower its Lightness. So 100% Lightness turns the colour
   --  white, 0% Lightness turns the colour black, and the "pure" colour
   --  would be 50% Lightness.

   function To_HSLA (Color : Gdk.RGBA.Gdk_RGBA) return HSLA_Color;
   function To_RGBA (Color : HSLA_Color) return Gdk.RGBA.Gdk_RGBA;
   function To_HSV (Color   : Cairo_Color) return HSV_Color;
   function To_Cairo (Color : Cairo_Color) return Gdk.RGBA.Gdk_RGBA;
   function To_Cairo (HSV : HSV_Color) return Cairo_Color;
   function To_Cairo (Color : Gdk.Color.Gdk_Color) return Cairo_Color;
   --  Translations between one color definition to another

   procedure Set_Source_Color
     (Cr : Cairo.Cairo_Context; Color : Cairo_Color);

   function To_Hex (Color : Gdk.RGBA.Gdk_RGBA) return String;
   --  Return a hexadecimal approximate representation of the color, of the
   --  form "#rrggbb". This loses the alpha chanel.
   --  The output is suitable for use in a markup (see Gtk.Label.Set_Markup
   --  for instance)

   function Complementary
     (Color : Gdk.RGBA.Gdk_RGBA) return Gdk.RGBA.Gdk_RGBA;
   --  Return the complementary color

   ----------------------------------
   -- Changing lightness of colors --
   ----------------------------------

   subtype Percent is Glib.Gdouble range 0.0 .. 1.0;

   function Shade
     (Color : Gdk.Color.Gdk_Color;
      Value : Percent) return Cairo_Color;
   function Shade
     (Color : Cairo_Color;
      Value : Percent) return Cairo_Color;
   --  Modifies the lightning of the color by the specified value.
   --
   --  Value is a modifier: 0.0 means the color is unchanged, 0.1 means the
   --  color is modified by 10%, and so on.

   function Lighten
     (Color  : Gdk.RGBA.Gdk_RGBA;
      Amount : Percent) return Gdk.RGBA.Gdk_RGBA;
   --  Return a lighter version of the color
   --
   --  Amount is a modifier: 0.0 means the color is unchanged, 0.1 means the
   --  color is modified by 10%, and so on.

   function Shade_Or_Lighten
     (Color  : Gdk.RGBA.Gdk_RGBA;
      Amount : Percent := 0.4) return Gdk.RGBA.Gdk_RGBA;
   --  Return a lighter or darker version of Color, depending on whether color
   --  is rather dark or rather light.
   --
   --  Amount is a modifier: 0.0 means the color is unchanged, 0.1 means the
   --  color is modified by 10%, and so on.

   -----------------------------
   -- Extra path manipulation --
   -----------------------------

   procedure Rounded_Rectangle
     (Cr         : Cairo.Cairo_Context;
      X, Y, W, H : Glib.Gdouble;
      Radius     : Glib.Gdouble);
   --  Draws a rounded rectangle at coordinate X, Y with W and H size.
   --  If Radius > 0, then the corner will be rounded.

   -------------------------
   -- Drawing subprograms --
   -------------------------

   procedure Draw_Shadow
     (Cr                  : Cairo.Cairo_Context;
      Widget              : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Shadow_Type         : Gtk.Enums.Gtk_Shadow_Type;
      X, Y, Width, Height : Glib.Gint;
      Corner_Radius       : Glib.Gdouble := 0.0);
   --  Draws a Frame of size Width x Height at position (X, Y) and (X2, Y2)
   --  using the specified color.
   --  Corner_Radius allows you to draw a rounded frame if set to a value > 0.
   --
   --  Additional drawing styles can be specified by using Cairo.Set_Line_XXXX
   --  on the Cairo_Context before calling this procedure.

   procedure Draw_Rectangle
     (Cr                  : Cairo.Cairo_Context;
      Color               : Cairo_Color;
      Filled              : Boolean;
      X, Y, Width, Height : Glib.Gint;
      Corner_Radius       : Glib.Gdouble := 0.0);
   procedure Draw_Rectangle
     (Cr                  : Cairo.Cairo_Context;
      Color               : Gdk.Color.Gdk_Color;
      Filled              : Boolean;
      X, Y, Width, Height : Glib.Gint;
      Corner_Radius       : Glib.Gdouble := 0.0);
   --  Draws a rectangle of size Width x Height at position (X, Y) and (X2, Y2)
   --  using the specified color.
   --  Corner_Radius allows you to draw a rounded rectangle if set to a
   --  value > 0.0
   --
   --  Additional drawing styles can be specified by using Cairo.Set_Line_XXXX
   --  on the Cairo_Context before calling this procedure.

   procedure Draw_Line
     (Cr             : Cairo.Cairo_Context;
      Color          : Cairo_Color;
      X1, Y1, X2, Y2 : Glib.Gint);
   procedure Draw_Line
     (Cr             : Cairo.Cairo_Context;
      Color          : Gdk.Color.Gdk_Color;
      X1, Y1, X2, Y2 : Glib.Gint);
   --  Draws a line between (X1, Y1) and (X2, Y2) using the specified color.
   --
   --  Additional drawing styles can be specified by using Cairo.Set_Line_XXXX
   --  on the Cairo_Context before calling this procedure.

   procedure Draw_Layout
     (Cr     : Cairo.Cairo_Context;
      Color  : Cairo_Color;
      X, Y   : Glib.Gint;
      Layout : Pango.Layout.Pango_Layout);
   procedure Draw_Layout
     (Cr     : Cairo.Cairo_Context;
      Color  : Gdk.Color.Gdk_Color;
      X, Y   : Glib.Gint;
      Layout : Pango.Layout.Pango_Layout);
   --  Draws the Pango layout at position (X, Y) using Color.

   procedure Draw_Pixbuf
     (Cr     : Cairo.Cairo_Context;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      X, Y   : Glib.Gint);
   --  Draws a pixbuf at coordinate X, Y
   --
   --  Note that Gdk_Pixmap or Gdk_Bitmap are not supported, as those
   --  are server-side images, so depend on a surface attached to a screen.
   --  As a result, those would not be drawn on a non-screen surface (such as
   --  an internal Image_Surface).

   ---------
   -- CSS --
   ---------

   procedure Load_Css_File
     (Path     : String;
      Error    : access procedure (Str : String) := null;
      Priority : Gtk.Style_Provider.Priority);
   procedure Load_Css_String
     (Data     : String;
      Error    : access procedure (Str : String) := null;
      Priority : Gtk.Style_Provider.Priority);
   --  Load CSS file and register it as a default CSS provider for the whole
   --  application.
   --  In case of error, the procedure Error is called if defined.

   ------------------------
   -- Cairo manipulation --
   ------------------------

   function Snapshot
     (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
      return Cairo.Cairo_Surface;
   --  Create a snapshot of the widget.
   --  This allocates a new surface and draws the widget on it. The surface
   --  needs to be destroyed when you are done with it using Surface_Destroy.
   --  The snapshot can be drawn on another surface using
   --      Set_Source_Surface (Cr, the_snapshot, 0.0, 0.0);
   --      Set_Operator (Cr, Cairo_Operator_Source);
   --      Rectangle (Cr, ....);
   --      Cairo.Fill (Cr);
   --  The 0.0 values might need to be adjusted if your widget does not have
   --  its own window. In this case, you should use:
   --      Get_Allocation (Widget, Alloc);
   --      --  and replace 0.0 with Gdouble (Alloc.X)

   procedure Draw_Overlay
     (Widget  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Overlay : in out Cairo.Cairo_Surface;
      Do_Draw : not null access procedure
        (Context : Cairo.Cairo_Context;
         Draw    : Boolean));
   --  Create an overlay on top of widget on which you can draw.
   --  This overlay is created if it doesn't exist (so Overlay must be
   --  initialied to Null_Surface initially). It is displayed on top of the
   --  toplevel window that contains Widget, and will be fully visible until
   --  some of the children of that toplevel window get a "draw" event. At that
   --  point, the children will partially override the overlay. This is not a
   --  problem in practice if the overlay is displayed while performing
   --  operations like a drag-and-drop.
   --  Do_Draw is the callback used to do the actual drawing or erasing of the
   --  overlay (depending on whether Draw is True or False). The context passed
   --  in argument has been properly translated and clipped so that (0, 0) are
   --  the coordinates of the top-left corner of widget.
   --
   --  When Draw is False, the procedure should display a filled rectangle. The
   --  context has already been set up so that filling will in fact redraw what
   --  was previously hidden. This is more efficient that having Draw_Overlay
   --  systematically fill the whole area.

   procedure Delete_Overlay
     (Widget  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Overlay : in out Cairo.Cairo_Surface);
   --  Delete the overlay created by Draw_Overlay, and force a refresh of the
   --  toplevel window.

   procedure Get_Offset
     (Window : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Parent : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y   : out Glib.Gint);
   --  Get the position of Window within parent's window (not that this is not
   --  necessarily the same as the position within Parent if the latter does
   --  not have a window).

   -------------
   -- Devices --
   -------------

   function Get_First_Device
     (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Source : Gdk.Types.Gdk_Input_Source) return Gdk.Device.Gdk_Device;
   --  Return the first device that matches the given source.
   --  This can be used to simulate keyboard events (using Source_Keyboard)
   --  or mouse events (Source_Mouse) for instance.
   --  The returned value (if not null) must be Ref-ed before being assigned
   --  to an event for instance.

end Gtkada.Style;
