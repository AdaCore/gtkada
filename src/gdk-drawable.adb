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

with Interfaces.C;

package body Gdk.Drawable is

   package C renames Interfaces.C;

   --------------
   -- Draw_Arc --
   --------------

   procedure Draw_Arc (Drawable : in Gdk_Drawable;
                       Gc       : in Gdk.GC.Gdk_GC;
                       Filled   : in Boolean := False;
                       X        : in Gint;
                       Y        : in Gint;
                       Width    : in Gint;
                       Height   : in Gint;
                       Angle1   : in Gint;
                       Angle2   : in Gint) is
      procedure Internal (Drawable : in Gdk_Drawable;
                          Gc       : in Gdk.GC.Gdk_GC;
                          Filled   : in Gint;
                          X, Y     : in Gint;
                          Width    : in Gint;
                          Height   : in Gint;
                          Angle1   : in Gint;
                          Angle2   : in Gint);
      pragma Import (C, Internal, "gdk_draw_arc");

   begin
      Internal (Drawable, Gc, Boolean'Pos (Filled),
                X, Y, Width, Height, Angle1, Angle2);
   end Draw_Arc;

   ----------------
   -- Draw_Lines --
   ----------------

   procedure Draw_Lines
     (Drawable : in Gdk_Drawable;
      Gc       : in Gdk.GC.Gdk_GC;
      Points   : in Gdk.Types.Gdk_Points_Array)
   is
      procedure Internal
        (Drawable : in Gdk_Drawable;
         Gc       : in Gdk.GC.Gdk_GC;
         Points   : in Gdk.Types.Gdk_Points_Array;
         Npoints  : in Gint);
      pragma Import (C, Internal, "gdk_draw_lines");

   begin
      Internal (Drawable, Gc, Points, Points'Length);
   end Draw_Lines;

   -----------------
   -- Draw_Points --
   -----------------

   procedure Draw_Points
     (Drawable : in Gdk_Drawable;
      Gc       : in Gdk.GC.Gdk_GC;
      Points   : in Gdk.Types.Gdk_Points_Array)
   is
      procedure Internal
        (Drawable : in Gdk_Drawable;
         Gc       : in Gdk.GC.Gdk_GC;
         Points   : in Gdk.Types.Gdk_Points_Array;
         Npoints  : in Gint);
      pragma Import (C, Internal, "gdk_draw_points");

   begin
      Internal (Drawable, Gc, Points, Points'Length);
   end Draw_Points;

   ------------------
   -- Draw_Polygon --
   ------------------

   procedure Draw_Polygon (Drawable : in Gdk_Drawable;
                           Gc       : in Gdk.GC.Gdk_GC;
                           Filled   : in Boolean;
                           Points   : in Gdk.Types.Gdk_Points_Array)
   is
      procedure Internal (Drawable : in Gdk_Drawable;
                          Gc       : in Gdk.GC.Gdk_GC;
                          Filled   : in Gint;
                          Points   : in Gdk.Types.Gdk_Points_Array;
                          Npoints  : in Gint);
      pragma Import (C, Internal, "gdk_draw_polygon");
   begin
      Internal (Drawable, Gc, Boolean'Pos (Filled), Points, Points'Length);
   end Draw_Polygon;

   --------------------
   -- Draw_Rectangle --
   --------------------

   procedure Draw_Rectangle (Drawable : in Gdk_Drawable;
                             Gc       : in Gdk.GC.Gdk_GC;
                             Filled   : in Boolean := False;
                             X, Y     : in Gint;
                             Width    : in Gint;
                             Height   : in Gint) is
      procedure Internal (Drawable : in Gdk_Drawable;
                          Gc       : in Gdk.GC.Gdk_GC;
                          Filled   : in Gint;
                          X, Y     : in Gint;
                          Width    : in Gint;
                          Height   : in Gint);
      pragma Import (C, Internal, "gdk_draw_rectangle");
   begin
      Internal (Drawable, Gc, Boolean'Pos (Filled), X, Y, Width, Height);
   end Draw_Rectangle;

   -------------------
   -- Draw_Segments --
   -------------------

   procedure Draw_Segments
     (Drawable : in Gdk.Drawable.Gdk_Drawable;
      Gc       : in Gdk.GC.Gdk_GC;
      Segs     : in Gdk.Types.Gdk_Segments_Array)
   is
      procedure Internal
        (Drawable : in Gdk.Drawable.Gdk_Drawable;
         Gc       : in Gdk.GC.Gdk_GC;
         Segs     : in Gdk.Types.Gdk_Segments_Array;
         Nsegs    : in Gint);
      pragma Import (C, Internal, "gdk_draw_segments");

   begin
      Internal (Drawable, Gc, Segs, Segs'Length);
   end Draw_Segments;

   ---------------
   -- Draw_Text --
   ---------------

   procedure Draw_Text
     (Drawable    : in Gdk_Drawable;
      Font        : in Gdk.Font.Gdk_Font;
      Gc          : in Gdk.GC.Gdk_GC;
      X           : in Gint;
      Y           : in Gint;
      Text        : in String)
   is
      procedure Internal
        (Drawable    : in Gdk_Drawable;
         Font        : in Gdk.Font.Gdk_Font;
         Gc          : in Gdk.GC.Gdk_GC;
         X           : in Gint;
         Y           : in Gint;
         Text        : in String;
         Text_Length : in Gint);
      pragma Import (C, Internal, "gdk_draw_text");

   begin
      Internal (Drawable, Font, Gc, X, Y, Text, Text'Length);
   end Draw_Text;

   procedure Draw_Text
      (Drawable    : in Gdk_Drawable;
       Font        : in Gdk.Font.Gdk_Font;
       Gc          : in Gdk.GC.Gdk_GC;
       X           : in Gint;
       Y           : in Gint;
       Wide_Text   : in Gdk.Types.Gdk_WString)
   is
      procedure Internal
        (Drawable    : in Gdk_Drawable;
         Font        : in Gdk.Font.Gdk_Font;
         Gc          : in Gdk.GC.Gdk_GC;
         X           : in Gint;
         Y           : in Gint;
         Text        : in C.wchar_array;
         Text_Length : in Gint);
      pragma Import (C, Internal, "gdk_draw_text_wc");

   begin
      Internal
        (Drawable, Font, Gc, X, Y,
         C.To_C (Item => Wide_Text, Append_Nul => False),
         Wide_Text'Length);
   end Draw_Text;

end Gdk.Drawable;
