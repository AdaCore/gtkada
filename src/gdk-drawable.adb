-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

with Interfaces.C;

package body Gdk.Drawable is

   package C renames Interfaces.C;

   ---------------
   -- Copy_Area --
   ---------------

   procedure Copy_Area (To       : in Gdk_Drawable'Class;
                        GC       : in Gdk.GC.Gdk_GC;
                        To_X     : in Gint;
                        To_Y     : in Gint;
                        From     : in Gdk.Window.Gdk_Window'Class;
                        Source_X : in Gint;
                        Source_Y : in Gint;
                        Width    : in Gint;
                        Height   : in Gint)
   is
      procedure Internal (To : System.Address;
                          GC : System.Address;
                          To_X, To_Y : Gint;
                          From : System.Address;
                          Source_X, Source_Y, Width, Height : Gint);
      pragma Import (C, Internal, "gdk_window_copy_area");
   begin
      Internal (Get_Object (To), Get_Object (GC), To_X, To_Y,
                Get_Object (From), Source_X, Source_Y,
                Width, Height);
   end Copy_Area;

   --------------
   -- Draw_Arc --
   --------------

   procedure Draw_Arc (Drawable : in Gdk_Drawable'Class;
                       Gc       : in Gdk.GC.Gdk_GC;
                       Filled   : in Boolean := False;
                       X        : in Gint;
                       Y        : in Gint;
                       Width    : in Gint;
                       Height   : in Gint;
                       Angle1   : in Gint;
                       Angle2   : in Gint) is
      procedure Internal (Drawable : in System.Address;
                          Gc       : in System.Address;
                          Filled   : in Gint;
                          X, Y     : in Gint;
                          Width    : in Gint;
                          Height   : in Gint;
                          Angle1   : in Gint;
                          Angle2   : in Gint);
      pragma Import (C, Internal, "gdk_draw_arc");
   begin
      Internal (Get_Object (Drawable),
                Get_Object (Gc),
                To_Gint (Filled),
                X, Y, Width, Height, Angle1, Angle2);
   end Draw_Arc;

   ----------------
   -- Draw_Image --
   ----------------

   procedure Draw_Image
      (Drawable : in Gdk_Drawable'Class;
       Gc       : in Gdk.GC.Gdk_GC;
       Image    : in Gdk.Image.Gdk_Image;
       Xsrc     : in Gint;
       Ysrc     : in Gint;
       Xdest    : in Gint;
       Ydest    : in Gint;
       Width    : in Gint;
       Height   : in Gint)
   is
      procedure Internal
         (Drawable : in System.Address;
          Gc       : in System.Address;
          Image    : in System.Address;
          Xsrc     : in Gint;
          Ysrc     : in Gint;
          Xdest    : in Gint;
          Ydest    : in Gint;
          Width    : in Gint;
          Height   : in Gint);
      pragma Import (C, Internal, "gdk_draw_image");
   begin
      Internal (Get_Object (Drawable),
                Get_Object (Gc),
                Get_Object (Image),
                Xsrc,
                Ysrc,
                Xdest,
                Ydest,
                Width,
                Height);
   end Draw_Image;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line (Drawable : in Gdk_Drawable'Class;
                             Gc       : in Gdk.GC.Gdk_GC;
                             X1, Y1   : in Gint;
                             X2, Y2   : in Gint) is
      procedure Internal (Drawable : in System.Address;
                          Gc       : in System.Address;
                          X1, Y1   : in Gint;
                          X2, Y2   : in Gint);
      pragma Import (C, Internal, "gdk_draw_line");
   begin
      Internal (Get_Object (Drawable),
                Get_Object (Gc),
                X1, Y1, X2, Y2);
   end Draw_Line;

   ----------------
   -- Draw_Lines --
   ----------------

   procedure Draw_Lines
      (Drawable : in Gdk_Drawable'Class;
       Gc       : in Gdk.GC.Gdk_GC;
       Points   : in Gdk.Types.Gdk_Points_Array)
   is
      procedure Internal
         (Drawable : in System.Address;
          Gc       : in System.Address;
          Points   : in Gdk.Types.Gdk_Points_Array;
          Npoints  : in Gint);
      pragma Import (C, Internal, "gdk_draw_lines");
   begin
      Internal (Get_Object (Drawable),
                Get_Object (Gc),
                Points,
                Points'Length);
   end Draw_Lines;

   -----------------
   -- Draw_Pixmap --
   -----------------

   procedure Draw_Pixmap (Drawable : in Gdk.Drawable.Gdk_Drawable'Class;
                          Gc       : in Gdk.GC.Gdk_GC;
                          Src      : in Gdk.Drawable.Gdk_Drawable;
                          Xsrc     : in Gint;
                          Ysrc     : in Gint;
                          Xdest    : in Gint;
                          Ydest    : in Gint;
                          Width    : in Gint;
                          Height   : in Gint) is
      procedure Internal (Drawable : in System.Address;
                          Gc       : in System.Address;
                          Src      : in System.Address;
                          Xsrc     : in Gint;
                          Ysrc     : in Gint;
                          Xdest    : in Gint;
                          Ydest    : in Gint;
                          Width    : in Gint;
                          Height   : in Gint);
      pragma Import (C, Internal, "gdk_draw_pixmap");
   begin
      Internal (Get_Object (Drawable), Get_Object (Gc), Get_Object (Src),
                Xsrc, Ysrc, Xdest, Ydest, Width, Height);
   end Draw_Pixmap;

   ----------------
   -- Draw_Point --
   ----------------

   procedure Draw_Point (Drawable : in Gdk_Drawable'Class;
                         Gc       : in Gdk.GC.Gdk_GC;
                         X        : in Gint;
                         Y        : in Gint) is
      procedure Internal (Drawable : in System.Address;
                          Gc       : in System.Address;
                          X, Y     : in Gint);
      pragma Import (C, Internal, "gdk_draw_point");
   begin
      Internal (Get_Object (Drawable),
                Get_Object (Gc),
                X, Y);
   end Draw_Point;

   -----------------
   -- Draw_Points --
   -----------------

   procedure Draw_Points
      (Drawable : in Gdk_Drawable'Class;
       Gc       : in Gdk.GC.Gdk_GC;
       Points   : in Gdk.Types.Gdk_Points_Array)
   is
      procedure Internal
         (Drawable : in System.Address;
          Gc       : in System.Address;
          Points   : in Gdk.Types.Gdk_Points_Array;
          Npoints  : in Gint);
      pragma Import (C, Internal, "gdk_draw_points");
   begin
      Internal (Get_Object (Drawable),
                Get_Object (Gc),
                Points,
                Points'Length);
   end Draw_Points;

   ------------------
   -- Draw_Polygon --
   ------------------

   procedure Draw_Polygon (Drawable : in Gdk_Drawable'Class;
                           Gc       : in Gdk.GC.Gdk_GC;
                           Filled   : in Boolean;
                           Points   : in Gdk.Types.Gdk_Points_Array)
   is
      procedure Internal (Drawable : in System.Address;
                          Gc       : in System.Address;
                          Filled   : in Gint;
                          Points   : in Gdk.Types.Gdk_Points_Array;
                          Npoints  : in Gint);
      pragma Import (C, Internal, "gdk_draw_polygon");
   begin
      Internal (Get_Object (Drawable), Get_Object (Gc),
                Boolean'Pos (Filled), Points, Points'Length);
   end Draw_Polygon;

   --------------------
   -- Draw_Rectangle --
   --------------------

   procedure Draw_Rectangle (Drawable : in Gdk_Drawable'Class;
                             Gc       : in Gdk.GC.Gdk_GC;
                             Filled   : in Boolean := False;
                             X, Y     : in Gint;
                             Width    : in Gint;
                             Height   : in Gint) is
      procedure Internal (Drawable : in System.Address;
                          Gc       : in System.Address;
                          Filled   : in Gint;
                          X, Y     : in Gint;
                          Width    : in Gint;
                          Height   : in Gint);
      pragma Import (C, Internal, "gdk_draw_rectangle");
   begin
      Internal (Get_Object (Drawable),
                Get_Object (Gc),
                To_Gint (Filled),
                X, Y, Width, Height);
   end Draw_Rectangle;

   -------------------
   -- Draw_Segments --
   -------------------

   procedure Draw_Segments
      (Drawable : in Gdk.Drawable.Gdk_Drawable'Class;
       Gc       : in Gdk.GC.Gdk_GC;
       Segs     : in Gdk.Types.Gdk_Segments_Array)
   is
      procedure Internal
         (Drawable : in System.Address;
          Gc       : in System.Address;
          Segs     : in Gdk.Types.Gdk_Segments_Array;
          Nsegs    : in Gint);
      pragma Import (C, Internal, "gdk_draw_segments");
   begin
      Internal (Get_Object (Drawable),
                Get_Object (Gc),
                Segs,
                Segs'Length);
   end Draw_Segments;

   -----------------
   --  Draw_Text  --
   -----------------

   procedure Draw_Text
      (Drawable    : in Gdk_Drawable'Class;
       Font        : in Gdk.Font.Gdk_Font;
       Gc          : in Gdk.GC.Gdk_GC;
       X           : in Gint;
       Y           : in Gint;
       Text        : in String) is
      procedure Internal
         (Drawable    : in System.Address;
          Font        : in System.Address;
          Gc          : in System.Address;
          X           : in Gint;
          Y           : in Gint;
          Text        : in String;
          Text_Length : in Gint);
      pragma Import (C, Internal, "gdk_draw_text");
   begin
      Internal (Get_Object (Drawable),
                Get_Object (Font),
                Get_Object (Gc),
                X,
                Y,
                Text,
                Text'Length);
   end Draw_Text;

   -----------------
   --  Draw_Text  --
   -----------------

   procedure Draw_Text
      (Drawable    : in Gdk_Drawable'Class;
       Font        : in Gdk.Font.Gdk_Font;
       Gc          : in Gdk.GC.Gdk_GC;
       X           : in Gint;
       Y           : in Gint;
       Text        : in Gdk.Types.Gdk_WString) is
      procedure Internal
         (Drawable    : in System.Address;
          Font        : in System.Address;
          Gc          : in System.Address;
          X           : in Gint;
          Y           : in Gint;
          Text        : in C.wchar_array;
          Text_Length : in Gint);
      pragma Import (C, Internal, "gdk_draw_text_wc");
   begin
      Internal (Get_Object (Drawable),
                Get_Object (Font),
                Get_Object (Gc),
                X,
                Y,
                C.To_C (Item => Text, Append_Nul => False),
                Text'Length);
   end Draw_Text;

end Gdk.Drawable;
