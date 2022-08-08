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
--  A GtkPrintContext encapsulates context information that is required when
--  drawing pages for printing, such as the cairo context and important
--  parameters like page size and resolution. It also lets you easily create
--  Pango.Layout.Pango_Layout and Pango.Context.Pango_Context objects that
--  match the font metrics of the cairo surface.
--
--  GtkPrintContext objects gets passed to the
--  Gtk.Print_Operation.Gtk_Print_Operation::begin-print,
--  Gtk.Print_Operation.Gtk_Print_Operation::end-print,
--  Gtk.Print_Operation.Gtk_Print_Operation::request-page-setup and
--  Gtk.Print_Operation.Gtk_Print_Operation::draw-page signals on the
--  Gtk.Print_Operation.Gtk_Print_Operation.
--
--  ## Using GtkPrintContext in a
--  Gtk.Print_Operation.Gtk_Print_Operation::draw-page callback
--
--  |[<!-- language="C" --> static void draw_page (GtkPrintOperation
--  *operation, GtkPrintContext *context, int page_nr) { cairo_t *cr;
--  PangoLayout *layout; PangoFontDescription *desc;
--
--  cr = gtk_print_context_get_cairo_context (context);
--
--  // Draw a red rectangle, as wide as the paper (inside the margins)
--  cairo_set_source_rgb (cr, 1.0, 0, 0); cairo_rectangle (cr, 0, 0,
--  gtk_print_context_get_width (context), 50);
--
--  cairo_fill (cr);
--
--  // Draw some lines cairo_move_to (cr, 20, 10); cairo_line_to (cr, 40, 20);
--  cairo_arc (cr, 60, 60, 20, 0, M_PI); cairo_line_to (cr, 80, 20);
--
--  cairo_set_source_rgb (cr, 0, 0, 0); cairo_set_line_width (cr, 5);
--  cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND); cairo_set_line_join (cr,
--  CAIRO_LINE_JOIN_ROUND);
--
--  cairo_stroke (cr);
--
--  // Draw some text layout = gtk_print_context_create_pango_layout
--  (context); pango_layout_set_text (layout, "Hello World! Printing is easy",
--  -1); desc = pango_font_description_from_string ("sans 28");
--  pango_layout_set_font_description (layout, desc);
--  pango_font_description_free (desc);
--
--  cairo_move_to (cr, 30, 20); pango_cairo_layout_path (cr, layout);
--
--  // Font Outline cairo_set_source_rgb (cr, 0.93, 1.0, 0.47);
--  cairo_set_line_width (cr, 0.5); cairo_stroke_preserve (cr);
--
--  // Font Fill cairo_set_source_rgb (cr, 0, 0.0, 1.0); cairo_fill (cr);
--
--  g_object_unref (layout); } ]|
--
--  Printing support was added in GTK+ 2.10.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;          use Cairo;
with Glib;           use Glib;
with Glib.Object;    use Glib.Object;
with Gtk.Page_Setup; use Gtk.Page_Setup;
with Pango.Context;  use Pango.Context;
with Pango.Font_Map; use Pango.Font_Map;
with Pango.Layout;   use Pango.Layout;

package Gtk.Print_Context is

   type Gtk_Print_Context_Record is new GObject_Record with null record;
   type Gtk_Print_Context is access all Gtk_Print_Context_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_print_context_get_type");

   -------------
   -- Methods --
   -------------

   function Create_Pango_Context
      (Context : not null access Gtk_Print_Context_Record)
       return Pango.Context.Pango_Context;
   --  Creates a new Pango.Context.Pango_Context that can be used with the
   --  Gtk.Print_Context.Gtk_Print_Context.
   --  Since: gtk+ 2.10

   function Create_Pango_Layout
      (Context : not null access Gtk_Print_Context_Record)
       return Pango.Layout.Pango_Layout;
   --  Creates a new Pango.Layout.Pango_Layout that is suitable for use with
   --  the Gtk.Print_Context.Gtk_Print_Context.
   --  Since: gtk+ 2.10

   function Get_Cairo_Context
      (Context : not null access Gtk_Print_Context_Record)
       return Cairo.Cairo_Context;
   --  Obtains the cairo context that is associated with the
   --  Gtk.Print_Context.Gtk_Print_Context.
   --  Since: gtk+ 2.10

   procedure Set_Cairo_Context
      (Context : not null access Gtk_Print_Context_Record;
       Cr      : Cairo.Cairo_Context;
       Dpi_X   : Gdouble;
       Dpi_Y   : Gdouble);
   --  Sets a new cairo context on a print context.
   --  This function is intended to be used when implementing an internal
   --  print preview, it is not needed for printing, since GTK+ itself creates
   --  a suitable cairo context in that case.
   --  Since: gtk+ 2.10
   --  "cr": the cairo context
   --  "dpi_x": the horizontal resolution to use with Cr
   --  "dpi_y": the vertical resolution to use with Cr

   function Get_Dpi_X
      (Context : not null access Gtk_Print_Context_Record) return Gdouble;
   --  Obtains the horizontal resolution of the
   --  Gtk.Print_Context.Gtk_Print_Context, in dots per inch.
   --  Since: gtk+ 2.10

   function Get_Dpi_Y
      (Context : not null access Gtk_Print_Context_Record) return Gdouble;
   --  Obtains the vertical resolution of the
   --  Gtk.Print_Context.Gtk_Print_Context, in dots per inch.
   --  Since: gtk+ 2.10

   function Get_Hard_Margins
      (Context : not null access Gtk_Print_Context_Record;
       Top     : access Gdouble;
       Bottom  : access Gdouble;
       Left    : access Gdouble;
       Right   : access Gdouble) return Boolean;
   --  Obtains the hardware printer margins of the
   --  Gtk.Print_Context.Gtk_Print_Context, in units.
   --  Since: gtk+ 2.20
   --  "top": top hardware printer margin
   --  "bottom": bottom hardware printer margin
   --  "left": left hardware printer margin
   --  "right": right hardware printer margin

   function Get_Height
      (Context : not null access Gtk_Print_Context_Record) return Gdouble;
   --  Obtains the height of the Gtk.Print_Context.Gtk_Print_Context, in
   --  pixels.
   --  Since: gtk+ 2.10

   function Get_Page_Setup
      (Context : not null access Gtk_Print_Context_Record)
       return Gtk.Page_Setup.Gtk_Page_Setup;
   --  Obtains the Gtk.Page_Setup.Gtk_Page_Setup that determines the page
   --  dimensions of the Gtk.Print_Context.Gtk_Print_Context.
   --  Since: gtk+ 2.10

   function Get_Pango_Fontmap
      (Context : not null access Gtk_Print_Context_Record)
       return Pango.Font_Map.Pango_Font_Map;
   --  Returns a Pango.Font_Map.Pango_Font_Map that is suitable for use with
   --  the Gtk.Print_Context.Gtk_Print_Context.
   --  Since: gtk+ 2.10

   function Get_Width
      (Context : not null access Gtk_Print_Context_Record) return Gdouble;
   --  Obtains the width of the Gtk.Print_Context.Gtk_Print_Context, in
   --  pixels.
   --  Since: gtk+ 2.10

end Gtk.Print_Context;
