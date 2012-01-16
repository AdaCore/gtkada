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

--  <description
--  The Gtk.Drawing_Area.Gtk_Drawing_Area widget is used for creating custom
--  user interface elements. It's essentially a blank widget; you can draw on
--  it. After creating a drawing area, the application may want to connect to:
--
--  *
--
--  Mouse and button press signals to respond to input from the user. (Use
--  Gtk.Widget.Add_Event to enable events you wish to receive.)
--
--  *
--
--  The Gtk.Widget.Gtk_Widget::realize signal to take any necessary actions
--  when the widget is instantiated on a particular display. (Create GDK
--  resources in response to this signal.)
--
--  *
--
--  The Gtk.Widget.Gtk_Widget::configure-event signal to take any necessary
--  actions when the widget changes size.
--
--  *
--
--  The Gtk.Widget.Gtk_Widget::draw signal to handle redrawing the contents of
--  the widget.
--
--  The following code portion demonstrates using a drawing area to display a
--  circle in the normal widget foreground color.
--
--  Note that GDK automatically clears the exposed area to the background
--  color before sending the expose event, and that drawing is implicitly
--  clipped to the exposed area.
--
--  == Simple GtkDrawingArea usage ==
--

--     gboolean
--     draw_callback (GtkWidget *widget, cairo_t *cr, gpointer data)
--     {
--        guint width, height;
--        GdkRGBA color;
--        width = gtk_widget_get_allocated_width (widget);
--        height = gtk_widget_get_allocated_height (widget);
--        cairo_arc (cr,
--           width / 2.0, height / 2.0,
--           MIN (width, height) / 2.0,
--           0, 2 * G_PI);
--        gtk_style_context_get_color (gtk_widget_get_style_context (widget),
--           0,
--           &amp;color);
--        gdk_cairo_set_source_rgba (cr, &amp;color);
--        cairo_fill (cr);
--        return FALSE;
--     }
--     [...]
--     GtkWidget &ast;drawing_area = gtk_drawing_area_new (<!-- -->);
--        gtk_widget_set_size_request (drawing_area, 100, 100);
--           g_signal_connect (G_OBJECT (drawing_area), "draw",
--           G_CALLBACK (draw_callback), NULL);
--
--  Draw signals are normally delivered when a drawing area first comes
--  onscreen, or when it's covered by another window and then uncovered. You
--  can also force an expose event by adding to the "damage region" of the
--  drawing area's window; gtk_widget_queue_draw_area and
--  gdk_window_invalidate_rect are equally good ways to do this. You'll then
--  get a draw signal for the invalid region.
--
--  The available routines for drawing are documented on the <link
--  linkend="gdk3-Cairo-Interaction">GDK Drawing Primitives</link> page and the
--  cairo documentation.
--
--  To receive mouse events on a drawing area, you will need to enable them
--  with Gtk.Widget.Add_Event. To receive keyboard events, you will need to set
--  the "can-focus" property on the drawing area, and you should probably draw
--  some user-visible indication that the drawing area is focused. Use
--  gtk_widget_has_focus in your expose event handler to decide whether to draw
--  the focus indicator. See gtk_render_focus for one way to draw focus.
--
--  </description>
--  <description>
--  See also the Double_Buffer widget provided in the GtkAda examples for an
--  advanced example that demonstrates how to use double buffering, to avoid
--  flickering in your drawings.
--
--  </description>
--  <group>Drawing</group>
--  <testgtk>libart_demo.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Drawing_Area is

   type Gtk_Drawing_Area_Record is new Gtk_Widget_Record with null record;
   type Gtk_Drawing_Area is access all Gtk_Drawing_Area_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Drawing_Area : out Gtk_Drawing_Area);
   procedure Initialize
      (Drawing_Area : access Gtk_Drawing_Area_Record'Class);
   --  Creates a new drawing area.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_drawing_area_get_type");

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Drawing_Area_Record, Gtk_Drawing_Area);
   function "+"
     (Widget : access Gtk_Drawing_Area_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Drawing_Area
   renames Implements_Buildable.To_Object;

end Gtk.Drawing_Area;
