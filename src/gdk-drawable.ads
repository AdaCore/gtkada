-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

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
--  @pxref{Package_Gdk.GC} for more information.
--
--  </description>
--  <c_version>1.2.7</c_version>

with Glib; use Glib;
with Gdk.GC;
with Gdk.Font;
with Gdk.Image;
with Gdk.Types;
with Gdk.Window;

package Gdk.Drawable is

   subtype Gdk_Drawable is Window.Gdk_Window;
   Null_Drawable : constant Gdk_Drawable;

   procedure Copy_Area
     (Dest     : in Gdk_Drawable;
      GC       : in Gdk.GC.Gdk_GC;
      X        : in Gint;
      Y        : in Gint;
      Source   : in Gdk.Window.Gdk_Window;
      Source_X : in Gint;
      Source_Y : in Gint;
      Width    : in Gint := -1;
      Height   : in Gint := -1);
   --  Copy a drawing area.
   --  Dest is the Gdk_Drawable to draw.
   --  X is the x coordinate of the destination within Dest.
   --  X is the y coordinate of the destination within Dest.
   --  Source_X is the left edge of the source rectangle within Source.
   --  Source_Y is the top of the source rectangle within Source.
   --  Width is the width of the area to be copied, or -1 to make the area
   --  extend to the right edge of Source.
   --  Height is the height of the area to be copied, or -1 to make the area
   --  extend to the bottom edge of Source.

   procedure Draw_Point
     (Drawable : in Gdk_Drawable;
      Gc       : in Gdk.GC.Gdk_GC;
      X        : in Gint;
      Y        : in Gint);
   --  Draw a point, using the foreground color and other attributes of the Gc.

   procedure Draw_Points
     (Drawable : in Gdk_Drawable;
      Gc       : in Gdk.GC.Gdk_GC;
      Points   : in Gdk.Types.Gdk_Points_Array);
   --  Draw a number of points.
   --  Use the foreground color and other attributes of the Gc.

   procedure Draw_Line
     (Drawable : in Gdk_Drawable;
      Gc       : in Gdk.GC.Gdk_GC;
      X1       : in Gint;
      Y1       : in Gint;
      X2       : in Gint;
      Y2       : in Gint);
   --  Draw a line, using the foreground color and other attributes of the Gc.
   --  (X1, Y1) is coordinate of the start point.
   --  (X2, Y2) is coordinate of the end point.

   procedure Draw_Lines
     (Drawable : in Gdk_Drawable;
      Gc       : in Gdk.GC.Gdk_GC;
      Points   : in Gdk.Types.Gdk_Points_Array);
   --  Draw a series of lines connecting the given points.
   --  The way in which joins between lines are drawn is determined by the
   --  Cap_Style value in the Gdk_GC. This can be set with
   --  Gdk.Gc.Set_Line_Attributes.

   procedure Draw_Segments
     (Drawable : in Gdk_Drawable;
      Gc       : in Gdk.GC.Gdk_GC;
      Segs     : in Gdk.Types.Gdk_Segments_Array);
   --  Draw a number of unconnected lines.

   procedure Draw_Rectangle
     (Drawable : in Gdk_Drawable;
      Gc       : in Gdk.GC.Gdk_GC;
      Filled   : in Boolean := False;
      X        : in Gint;
      Y        : in Gint;
      Width    : in Gint;
      Height   : in Gint);
   --  Draw a rectangular outline or filled rectangle.
   --  Note that a rectangle drawn filled is 1 pixel smaller in both dimensions
   --  than a rectangle outlined. Calling
   --  Draw_Rectangle (Window, Gc, True, 0, 0, 20, 20) results in a filled
   --  rectangle 20 pixels wide and 20 pixels high. Calling
   --  Draw_Rectangle (Window, Gc, False, 0, 0, 20, 20) results in an outlined
   --  rectangle with corners at (0, 0), (0, 20), (20, 20), and (20, 0), which
   --  makes it 21 pixels wide and 21 pixels high.
   --
   --  (X, Y) represents the coordinate of the top-left edge of the rectangle.

   procedure Draw_Arc
     (Drawable : in Gdk_Drawable;
      Gc       : in Gdk.GC.Gdk_GC;
      Filled   : in Boolean := False;
      X        : in Gint;
      Y        : in Gint;
      Width    : in Gint;
      Height   : in Gint;
      Angle1   : in Gint;
      Angle2   : in Gint);
   --  Draws an arc or a filled 'pie slice'.
   --  The arc is defined by the bounding rectangle of the entire ellipse, and
   --  the start and end angles of the part of the ellipse to be drawn.
   --  Filled is True if the arc should be filled, producing a 'pie slice'.
   --  (X, Y) represent the coordinate of the top-left edge of the bounding
   --  rectangle.
   --  Angle1 is the start angle of the arc, relative to the 3 o'clock
   --  position, counter-clockwise, in 1/64ths of a degree.
   --  Angle2 is the end angle of the arc, relative to angle1, in 1/64ths of a
   --  degree.

   procedure Draw_Polygon
     (Drawable : in Gdk_Drawable;
      Gc       : in Gdk.GC.Gdk_GC;
      Filled   : in Boolean;
      Points   : in Gdk.Types.Gdk_Points_Array);
   --  Draw an outlined or filled polygon.
   --  Filled is True if the polygon should be filled. The polygon is closed
   --  automatically, connecting the last point to the first point if
   --  necessary.
   --  Points is an array of Gdk_Point specifying the points making up the
   --  polygon.

   procedure Draw_Text
     (Drawable    : in Gdk_Drawable;
      Font        : in Gdk.Font.Gdk_Font;
      Gc          : in Gdk.GC.Gdk_GC;
      X           : in Gint;
      Y           : in Gint;
      Text        : in String);
   --  Draw a string in the given font or fontset.
   --  X is the x coordinate of the left edge of the text.
   --  Y is the y coordinate of the baseline of the text.

   procedure Draw_Text
     (Drawable    : in Gdk_Drawable;
      Font        : in Gdk.Font.Gdk_Font;
      Gc          : in Gdk.GC.Gdk_GC;
      X           : in Gint;
      Y           : in Gint;
      Wide_Text   : in Gdk.Types.Gdk_WString);
   --  Draw a wide string in the given font of fontset.
   --  If the font is a 1-byte font, the string is converted into 1-byte
   --  characters (discarding the high bytes) before output.

   procedure Draw_Pixmap
     (Drawable : in Gdk_Drawable;
      Gc       : in Gdk.GC.Gdk_GC;
      Src      : in Gdk_Drawable;
      Xsrc     : in Gint;
      Ysrc     : in Gint;
      Xdest    : in Gint;
      Ydest    : in Gint;
      Width    : in Gint := -1;
      Height   : in Gint := -1);
   --  Draw a pixmap, or a part of a pixmap, onto another drawable.
   --  Src is the source GdkPixmap to draw.
   --  Xsrc is the left edge of the source rectangle within Src.
   --  Ysrc is the top of the source rectangle within Src.
   --  Xdest is the x coordinate of the destination within Src.
   --  Ydest is the y coordinate of the destination within Src.
   --  Width is the width of the area to be copied, or -1 to make the area
   --  extend to the right edge of the source pixmap.
   --  Height is the height of the area to be copied, or -1 to make the area
   --  extend to the bottom edge of the source pixmap.

   procedure Draw_Image
     (Drawable : in Gdk_Drawable;
      Gc       : in Gdk.GC.Gdk_GC;
      Image    : in Gdk.Image.Gdk_Image;
      Xsrc     : in Gint;
      Ysrc     : in Gint;
      Xdest    : in Gint;
      Ydest    : in Gint;
      Width    : in Gint := -1;
      Height   : in Gint := -1);
   --  Draw a Gdk_Image onto a Drawable.
   --  The depth of the Gdk_Image must match the depth of the Gdk_Drawable.
   --  Image is the Gdk_Image to draw.
   --  Xsrc is the left edge of the source rectangle within Image.
   --  Ysrc is the top of the source rectangle within Image.
   --  Xdest is the x coordinate of the destination within Drawable.
   --  Ydest is the y coordinate of the destination within Drawable.
   --  Width is the width of the area to be copied, or -1 to make the area
   --  extend to the right edge of image.
   --  Height is the height of the area to be copied, or -1 to make the area
   --  extend to the bottom edge of image.

private
   Null_Drawable : constant Gdk_Drawable := null;
   pragma Import (C, Copy_Area, "gdk_window_copy_area");
   pragma Import (C, Draw_Line, "gdk_draw_line");
   pragma Import (C, Draw_Pixmap, "gdk_draw_pixmap");
   pragma Import (C, Draw_Point, "gdk_draw_point");
   pragma Import (C, Draw_Image, "gdk_draw_image");
end Gdk.Drawable;

--  <example>
--  <include>../examples/documentation/draw.adb</include>
--  </example>
