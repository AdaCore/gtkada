------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
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
--  This package provides support for drawing points, lines, arcs and text onto
--  what are called 'drawables'. Drawables, as the name suggests, are things
--  which support drawing onto them, and are either Gdk_Window or
--  Gdk_Pixmap objects.
--
--  Many of the drawing operations take a Gdk_GC argument, which represents a
--  graphics context. This Gdk_GC contains a number of drawing attributes such
--  as foreground color, background color and line width, and is used to
--  reduce the number of arguments needed for each drawing operation.
--  See Gdk.GC for more information.
--
--  </description>
--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with Glib; use Glib;

package Gdk.Drawable is

   subtype Gdk_Drawable is Gdk.Gdk_Drawable;
   --  A screen area that can be drawn upon.

   Null_Drawable : constant Gdk_Drawable;

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gdk_Drawable.

   procedure Get_Size
     (Drawable : Gdk_Drawable;
      Width    : out Gint;
      Height   : out Gint);
   --  Return the width and height of a given drawable.

   procedure Set_Colormap
     (Drawable : Gdk_Drawable; Colormap : Gdk.Gdk_Colormap);

   function Get_Colormap
     (Drawable : Gdk_Drawable) return Gdk.Gdk_Colormap;

   function Get_Visual (Drawable : Gdk_Drawable) return Gdk.Gdk_Visual;

   function Get_Depth (Drawable : Gdk_Drawable) return Gint;

   procedure Ref (Drawable : Gdk_Drawable);

   procedure Unref (Drawable : Gdk_Drawable);

   function Get_Image
     (Drawable : Gdk_Drawable;
      X        : Gint;
      Y        : Gint;
      Width    : Gint;
      Height   : Gint) return Gdk_Image;

   function Get_Clip_Region (Drawable : Gdk_Drawable) return Gdk.Gdk_Region;

   function Get_Visible_Region (Drawable : Gdk_Drawable) return Gdk.Gdk_Region;

private
   Null_Drawable : constant Gdk_Drawable := null;
   pragma Import (C, Get_Type, "gdk_drawable_get_type");
   pragma Import (C, Get_Depth, "gdk_drawable_get_depth");
   pragma Import (C, Ref, "gdk_drawable_ref");
   pragma Import (C, Unref, "gdk_drawable_unref");
   pragma Import (C, Get_Size, "gdk_drawable_get_size");
   pragma Import (C, Get_Colormap, "gdk_drawable_get_colormap");
   pragma Import (C, Get_Visual, "gdk_drawable_get_visual");
   pragma Import (C, Set_Colormap, "gdk_drawable_set_colormap");
   pragma Import (C, Get_Image, "gdk_drawable_get_image");
   pragma Import (C, Get_Clip_Region, "gdk_drawable_get_clip_region");
   pragma Import (C, Get_Visible_Region, "gdk_drawable_get_visible_region");
end Gdk.Drawable;

--  <example>
--  <include>../examples/documentation/draw.adb</include>
--  </example>

--  missing pango related functions:
--  gdk_draw_glyphs
--  gdk_draw_layout_line
--  gdk_draw_layout_line_with_colors
--  gdk_draw_layout_with_colors
