-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
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

package body Gdk.Drawable is

   ---------------
   -- Copy_Area --
   ---------------

   procedure Copy_Area (To       : in Gdk_Drawable;
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

   --------------------
   -- Draw_Rectangle --
   --------------------

   procedure Draw_Rectangle (Drawable : in Gdk_Drawable;
                             Gc       : in Gdk.GC.Gdk_GC'Class;
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

   ----------------
   -- Draw_Point --
   ----------------

   procedure Draw_Point (Drawable : in Gdk_Drawable;
                         Gc       : in Gdk.GC.Gdk_GC'Class;
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

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line (Drawable : in Gdk_Drawable;
                             Gc       : in Gdk.GC.Gdk_GC'Class;
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

   --------------
   -- Draw_Arc --
   --------------

   procedure Draw_Arc (Drawable : in Gdk_Drawable;
                       Gc       : in Gdk.GC.Gdk_GC'Class;
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

   -----------------
   -- Draw_Pixmap --
   -----------------

   procedure Draw_Pixmap (Drawable : in Gdk.Drawable.Gdk_Drawable;
                          Gc       : in Gdk.GC.Gdk_GC'Class;
                          Src      : in Gdk.Drawable.Gdk_Drawable'Class;
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

   ------------------
   -- Draw_Polygon --
   ------------------

   procedure Draw_Polygon (Drawable : in Gdk_Drawable;
                           Gc       : in Gdk.GC.Gdk_GC'Class;
                           Filled   : in Boolean;
                           Points   : in Gdk.Point.Gdk_Points_Array)
   is
      procedure Internal (Drawable : in System.Address;
                          Gc       : in System.Address;
                          Filled   : in Gint;
                          Points   : in Gdk.Point.Gdk_Points_Array;
                          Npoints  : in Gint);
      pragma Import (C, Internal, "gdk_draw_polygon");
   begin
      Internal (Get_Object (Drawable), Get_Object (Gc),
                Boolean'Pos (Filled), Points, Points'Length);
   end Draw_Polygon;


end Gdk.Drawable;
