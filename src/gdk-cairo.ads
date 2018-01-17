------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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
--  Interfacing between Gdk and Cairo.
--  </description>
--
--  <c_version>2.16.6</c_version>
--  <group>Cairo</group>

with Glib;         use Glib;
with Cairo;        use Cairo;
with Gdk.Color;    use Gdk.Color;
with Gdk.Pixbuf;   use Gdk.Pixbuf;
with Gdk.RGBA;     use Gdk.RGBA;

package Gdk.Cairo is

   function Create (Window : Gdk.Gdk_Window) return Cairo_Context;
   --  Creates a Cairo context for drawing to Drawable.
   --
   --  Note that due to double-buffering, Cairo contexts created
   --  in a GTK+ expose event handler cannot be cached and reused
   --  between different expose events.
   --
   --  Returns a newly created Cairo context. The result should be freed with
   --  Cairo.Destroy.

   procedure Set_Source_Pixbuf
     (Cr       : Cairo_Context;
      Pixbuf   : Gdk_Pixbuf;
      Pixbuf_X : Gdouble;
      Pixbuf_Y : Gdouble);
   --  Cr: a Cairo_Context
   --  Pixbuf: a Gdk_Pixbuf
   --  Pixbuf_X: X coordinate of location to place upper left corner of Pixbuf
   --  Pixbuf_Y: Y coordinate of location to place upper left corner of Pixbuf
   --
   --  Sets the given pixbuf as the source pattern for the Cairo context.
   --  The pattern has an extend mode of CAIRO_EXTEND_NONE and is aligned
   --  so that the origin of Pixbuf is Pixbuf_X, Pixbuf_Y

   function Create_From_Pixbuf
      (Pixbuf  : Gdk_Pixbuf;
       Scale   : Gint;
       For_Window : Gdk.Gdk_Window := null) return Cairo_Surface;
   --  Creates an image surface with the same contents as the pixbuf.
   --  This is similar to the work done by Set_Source_Pixbuf, but allows you
   --  to specify the scale factor for the screen, i.e. adapt to high-dpi
   --  (Retina) screens.
   --  You can then use Gtk.Style_Context.Render_Icon_Surface to paint the
   --  surface onto a Cairo_Context, and then destroy the surface.
   --  See also the convenient wrapper Gtkada.Style.Draw_Pixbuf_With_Scale.

   procedure Set_Source_Color
     (Cr       : Cairo_Context;
      Color    : Gdk_Color);
   procedure Set_Source_RGBA
     (Cr       : Cairo_Context;
      Color    : Gdk.RGBA.Gdk_RGBA);
   --  Set the specified Color as the source of Cr.

private
   pragma Import (C, Set_Source_Color, "gdk_cairo_set_source_color");
end Gdk.Cairo;
