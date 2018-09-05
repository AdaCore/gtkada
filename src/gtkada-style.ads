------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2011-2018, AdaCore                     --
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

with Ada.Finalization;
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
with Pango.Enums;
with Pango.Font;

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
   function Create_Rgba_Pattern
      (Color : Cairo_Color) return Cairo.Cairo_Pattern;

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

   -------------------------
   -- Drawing subprograms --
   -------------------------
   --  The following subprograms are better replaced by the use of the
   --  Drawing_Style type below, which provides more configurability.

   procedure Rounded_Rectangle
     (Cr         : Cairo.Cairo_Context;
      X, Y, W, H : Glib.Gdouble;
      Radius     : Glib.Gdouble);
   --  Draws a rounded rectangle at coordinate X, Y with W and H size.
   --  If Radius > 0, then the corner will be rounded.

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
   pragma Obsolescent (Draw_Pixbuf, "Use Draw_Pixbuf_With_Scale instead");
   --  Draws a pixbuf at coordinate X, Y
   --
   --  Note that Gdk_Pixmap or Gdk_Bitmap are not supported, as those
   --  are server-side images, so depend on a surface attached to a screen.
   --  As a result, those would not be drawn on a non-screen surface (such as
   --  an internal Image_Surface).

   procedure Draw_Pixbuf_With_Scale
      (Cr     : Cairo.Cairo_Context;
       Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
       X, Y   : Glib.Gdouble;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class := null);
   procedure Draw_Pixbuf_With_Scale
      (Cr        : Cairo.Cairo_Context;
       Icon_Name : String;
       X, Y      : Glib.Gdouble;
       Size      : Glib.Gint;
       Widget    : access Gtk.Widget.Gtk_Widget_Record'Class := null);
   --  Draw the pixbuf at the specified coordinates.
   --  This takes into account the scale factor on the screen, so that the
   --  image is properly sized on hi-dpi (retina) displays.
   --  The pixbuf must have been created with the proper scale factor as well,
   --  using for instance Gtk.Icon_Theme.Load_Icon_For_Scale. It is often more
   --  convenient to pass the name of the icon, as found in the icon theme,
   --  and let gtk+ create the pixbuf automatically.
   --  The widget is used to compute the appropriate scale factor.

   --------------------
   -- Drawing styles --
   --------------------

   type Drawing_Style is tagged private;
   --  This type provides a set of high-level subprograms to draw on a cairo
   --  context (and thus they can be used when drawing Canvas_Item).
   --
   --  This type is reference-counted: it will be automatically freed when no
   --  longer used in your application. If you store the same instance of
   --  Drawing_Style in two contexts, modifying one will also modify the
   --  other.

   No_Drawing_Style : constant Drawing_Style;

   type Arrow_Head is (None, Open, Solid, Diamond, Circle);
   --  The various styles of arrow heads (filled with Fill style).
   --  When displaying a circle, the Length of the arrow style is the radius of
   --  the circle.

   type Arrow_Style is record
      Head       : Arrow_Head;
      Length     : Glib.Gdouble := 8.0;
      Angle      : Glib.Gdouble := 0.4;
      Stroke     : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Black_RGBA;
      Fill       : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA;
      Line_Width : Glib.Gdouble := 1.0;
   end record;
   No_Arrow_Style : constant Arrow_Style :=
     (None, 0.0, 0.0, Gdk.RGBA.Black_RGBA, Gdk.RGBA.Null_RGBA, 1.0);

   type Symbol_Name is (None, Cross, Strike, Double_Strike);
   type Symbol_Style is record
      Name       : Symbol_Name;
      Stroke     : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Black_RGBA;
      Distance   : Glib.Gdouble := 16.0;
      Line_Width : Glib.Gdouble := 1.0;
   end record;
   No_Symbol : constant Symbol_Style := (None, Gdk.RGBA.Black_RGBA, 16.0, 1.0);
   --  Distance is the distance from the end of the line to the symbol. It
   --  should in general be greater than the length of the arrow (see
   --  Arrow_Style above)

   type Font_Style is record
      Name       : Pango.Font.Pango_Font_Description := null;
      Underline  : Pango.Enums.Underline := Pango.Enums.Pango_Underline_None;
      Strikethrough : Boolean := False;
      Color      : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Black_RGBA;

      Line_Spacing : Glib.Gint := 1;
      --  Space between two lines

      Halign     : Pango.Enums.Alignment := Pango.Enums.Pango_Align_Left;
   end record;
   Default_Font : constant Font_Style := (others => <>);
   --  Some of the attributes like Halign are only taken into account when
   --  using Gtkada.Canvas_View.Text_Item.

   function Copy (Source : Font_Style) return Font_Style;
   --  Return a copy of the given font style.

   type Shadow_Style is record
      Color    : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA;
      X_Offset : Glib.Gdouble := 2.0;
      Y_Offset : Glib.Gdouble := 2.0;
   end record;
   No_Shadow : constant Shadow_Style := (Gdk.RGBA.Null_RGBA, 5.0, 5.0);

   type Point is record
      X, Y : Glib.Gdouble;
   end record;
   type Point_Array is array (Natural range <>) of Point;
   type Point_Array_Access is access all Point_Array;

   function Gtk_New
      (Stroke           : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Black_RGBA;
       Fill             : Cairo.Cairo_Pattern := Cairo.Null_Pattern;
       Font             : Font_Style := Default_Font;
       Line_Width       : Glib.Gdouble := 1.0;
       Dashes           : Cairo.Dash_Array := Cairo.No_Dashes;
       Arrow_From       : Arrow_Style := No_Arrow_Style;
       Arrow_To         : Arrow_Style := No_Arrow_Style;
       Symbol_From      : Symbol_Style := No_Symbol;
       Symbol_To        : Symbol_Style := No_Symbol;
       Shadow           : Shadow_Style := No_Shadow;
       Sloppy           : Boolean := False)
     return Drawing_Style;
   --  Creates a new instance of drawing style.
   --
   --  Sloppy: if true, a sort of approximate drawing is done for lines and
   --   text, so that it looks like the drawing was done by hand.
   --
   --  Fill: this is generated created through the functions in Cairo.Pattern.
   --   The pattern is adopted by the style, which will unreferenced it when
   --   not needed. As such, if the pattern is shared between multiple
   --   styles you need to Ref it.
   --   When the style is used with a Gtkada.Canvas_View.Container_Item or one
   --   of its children, and pattern is a gradient, it should be defined in
   --   the 0.0 .. 1.0 pattern space, and will be resized automatically based
   --   on the computed size of the item.
   --
   --  The style will free the Font.Name object, so you will need to pass the
   --  result of either Pango.Font.Copy or Pango.Font.From_String.

   procedure Draw_Rect
      (Self          : Drawing_Style;
       Cr            : Cairo.Cairo_Context;
       Topleft       : Point;
       Width, Height : Glib.Gdouble;
       Radius        : Glib.Gdouble := 0.0);
   function Path_Rect
      (Self          : Drawing_Style;
       Cr            : Cairo.Cairo_Context;
       Topleft       : Point;
       Width, Height : Glib.Gdouble;
       Radius        : Glib.Gdouble := 0.0) return Boolean;
   --  Draw a rectangle with the given style. If Radius is not null, this is
   --  a rounded rectangle. Draw_Rect will automatically call Finish path
   --  to display te path, but Path_Rect will only prepare it (it returns False
   --  if no path was prepared)

   procedure Draw_Polyline
      (Self        : Drawing_Style;
       Cr          : Cairo.Cairo_Context;
       Points      : Point_Array;
       Close       : Boolean := False;
       Show_Arrows : Boolean := True;
       Relative    : Boolean := False);
   function Path_Polyline
      (Self        : Drawing_Style;
       Cr          : Cairo.Cairo_Context;
       Points      : Point_Array;
       Close       : Boolean := False;
       Relative    : Boolean := False) return Boolean;
   --  Draw a line joining all the points. If Close is true, the last point is
   --  also linked to the first.
   --  If Self defines arrows or symbols on either ends, they are also
   --  displayed if Show_Arrows is True.
   --  Relative should be set to True if the points are relative: the first
   --  point has item coordinates, and all remaining points are relative to the
   --  previous point.

   procedure Draw_Polycurve
     (Self        : Drawing_Style;
      Cr          : Cairo.Cairo_Context;
      Points      : Point_Array;
      Show_Arrows : Boolean := True;
      Relative    : Boolean := False);
   function Path_Polycurve
     (Self        : Drawing_Style;
      Cr          : Cairo.Cairo_Context;
      Points      : Point_Array;
      Relative    : Boolean := False) return Boolean;
   --  Same as Drawpolyline, but draws bezier curves.
   --  Points is an array of both points and control points, as in:
   --     pt1, ctrl1, ctrl2, pt2, ctrl3, ctrl4, pt3, ...

   procedure Draw_Ellipse
     (Self          : Drawing_Style;
      Cr            : Cairo.Cairo_Context;
      Topleft       : Point;
      Width, Height : Glib.Gdouble);
   function Path_Ellipse
     (Self          : Drawing_Style;
      Cr            : Cairo.Cairo_Context;
      Topleft       : Point;
      Width, Height : Glib.Gdouble) return Boolean;
   --  Draw an ellipse inscribed in the specified rectangle

   procedure Draw_Text
     (Self       : Drawing_Style;
      Cr         : Cairo.Cairo_Context;
      Layout     : not null access Pango.Layout.Pango_Layout_Record'Class;
      Topleft    : Point;
      Text       : String;
      Max_Width  : Glib.Gdouble := Glib.Gdouble'First;
      Max_Height : Glib.Gdouble := Glib.Gdouble'First);
   --  Draw text at specific coordinates.
   --  Topleft is the position of the top-left corner for the text, or the
   --  middle line if the text's vertical_align is set to middle.
   --  Max_Width is optional, but is used to resolve alignment.

   procedure Draw_Arrows_And_Symbols
     (Self     : Drawing_Style;
      Cr       : Cairo.Cairo_Context;
      Points   : Point_Array;
      Relative : Boolean := False);
   --  Draw arrow heads and symbols to both ends of the line, based on Self.
   --  This is similar to Polyline, but does not draw the line itself.
   --  Relative should be True if each point are defined relative to the
   --  previous point.

   procedure Finish_Path
      (Self       : Drawing_Style;
       Cr         : Cairo.Cairo_Context;
       Clear_Path : Boolean := True);
   --  This is for use when you are creating your own paths via standard cairo
   --  calls. This will call Stroke and Fill with the appropriate parameters
   --  found in Self.

   procedure Measure_Text
     (Self     : Drawing_Style;
      Layout   : not null access Pango.Layout.Pango_Layout_Record'Class;
      Text     : String;
      Width    : out Glib.Gdouble;
      Height   : out Glib.Gdouble);
   --  Measure the size the text would take on the screen

   function Get_Arrow_From (Self : Drawing_Style) return Arrow_Style;
   function Get_Arrow_To (Self : Drawing_Style) return Arrow_Style;
   function Get_Stroke (Self : Drawing_Style) return Gdk.RGBA.Gdk_RGBA;
   function Get_Line_Width (Self : Drawing_Style) return Glib.Gdouble;
   function Get_Font (Self : Drawing_Style) return Font_Style;
   function Get_Fill (Self : Drawing_Style) return Cairo.Cairo_Pattern;
   function Get_Shadow (Self : Drawing_Style) return Shadow_Style;
   --  Access the various properties of the style

   procedure Set_Fill
     (Self   : Drawing_Style;
      Fill   : Cairo.Cairo_Pattern := Cairo.Null_Pattern);
   procedure Set_Stroke
     (Self   : Drawing_Style;
      Stroke : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Black_RGBA);
   --  Overriding specific attributes of the style

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

private

   type Dash_Array_Access is access all Cairo.Dash_Array;

   type Drawing_Style_Data is record
      Refcount         : Natural := 1;
      Stroke           : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Black_RGBA;
      Fill             : Cairo.Cairo_Pattern := Cairo.Null_Pattern;
      Font             : Font_Style := Default_Font;
      Line_Width       : Glib.Gdouble := 1.0;
      Dashes           : Dash_Array_Access := null;
      Arrow_From       : Arrow_Style := No_Arrow_Style;
      Arrow_To         : Arrow_Style := No_Arrow_Style;
      Symbol_From      : Symbol_Style := No_Symbol;
      Symbol_To        : Symbol_Style := No_Symbol;
      Sloppy           : Boolean := False;
      Shadow           : Shadow_Style := No_Shadow;
   end record;
   type Drawing_Style_Data_Access is access all Drawing_Style_Data;

   Default_Style : constant Drawing_Style_Data := (others => <>);

   type Drawing_Style is new Ada.Finalization.Controlled with record
      Data : Drawing_Style_Data_Access;
   end record;

   overriding procedure Adjust (Self : in out Drawing_Style);
   overriding procedure Finalize (Self : in out Drawing_Style);

   No_Drawing_Style : constant Drawing_Style :=
     (Ada.Finalization.Controlled with Data => null);

end Gtkada.Style;
