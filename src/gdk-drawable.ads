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

with Glib; use Glib;
with Gdk.GC;
with Gdk.Font;
with Gdk.Image;
with Gdk.Types;
with Gdk.Window;

--  In this package, every subprogram uses 'Class because Drawable is
--  actually a subtype. That way, both the upper class Gdk_Window and
--  the children classes Gdk_Pixmap and Gdk_Drawable can use theses
--  subprograms.

package Gdk.Drawable is

   subtype Gdk_Drawable is Window.Gdk_Window;

   procedure Copy_Area (To       : in Gdk_Drawable'Class;
                        GC       : in Gdk.GC.Gdk_GC;
                        To_X     : in Gint;
                        To_Y     : in Gint;
                        From     : in Gdk.Window.Gdk_Window'Class;
                        Source_X : in Gint;
                        Source_Y : in Gint;
                        Width    : in Gint;
                        Height   : in Gint);

   procedure Draw_Rectangle (Drawable : in Gdk_Drawable'Class;
                             Gc       : in Gdk.GC.Gdk_GC;
                             Filled   : in Boolean := False;
                             X        : in Gint;
                             Y        : in Gint;
                             Width    : in Gint;
                             Height   : in Gint);

   procedure Draw_Point (Drawable : in Gdk_Drawable'Class;
                         Gc       : in Gdk.GC.Gdk_GC;
                         X        : in Gint;
                         Y        : in Gint);

   procedure Draw_Line (Drawable : in Gdk_Drawable'Class;
                        Gc       : in Gdk.GC.Gdk_GC;
                        X1       : in Gint;
                        Y1       : in Gint;
                        X2       : in Gint;
                        Y2       : in Gint);

   procedure Draw_Arc (Drawable : in Gdk_Drawable'Class;
                       Gc       : in Gdk.GC.Gdk_GC;
                       Filled   : in Boolean := False;
                       X        : in Gint;
                       Y        : in Gint;
                       Width    : in Gint;
                       Height   : in Gint;
                       Angle1   : in Gint;
                       Angle2   : in Gint);

   procedure Draw_Polygon (Drawable : in Gdk_Drawable'Class;
                           Gc       : in Gdk.GC.Gdk_GC;
                           Filled   : in Boolean;
                           Points   : in Gdk.Types.Gdk_Points_Array);

   procedure Draw_Text
      (Drawable    : in Gdk_Drawable'Class;
       Font        : in Gdk.Font.Gdk_Font;
       Gc          : in Gdk.GC.Gdk_GC;
       X           : in Gint;
       Y           : in Gint;
       Text        : in String);

   procedure Draw_Text
      (Drawable    : in Gdk_Drawable'Class;
       Font        : in Gdk.Font.Gdk_Font;
       Gc          : in Gdk.GC.Gdk_GC;
       X           : in Gint;
       Y           : in Gint;
       Text        : in Gdk.Types.Gdk_WString);

   procedure Draw_Pixmap
      (Drawable : in Gdk_Drawable'Class;
       Gc       : in Gdk.GC.Gdk_GC;
       Src      : in Gdk_Drawable;
       Xsrc     : in Gint;
       Ysrc     : in Gint;
       Xdest    : in Gint;
       Ydest    : in Gint;
       Width    : in Gint;
       Height   : in Gint);

   procedure Draw_Image
      (Drawable : in Gdk_Drawable'Class;
       Gc       : in Gdk.GC.Gdk_GC;
       Image    : in Gdk.Image.Gdk_Image;
       Xsrc     : in Gint;
       Ysrc     : in Gint;
       Xdest    : in Gint;
       Ydest    : in Gint;
       Width    : in Gint;
       Height   : in Gint);

   procedure Draw_Points
      (Drawable : in Gdk_Drawable'Class;
       Gc       : in Gdk.GC.Gdk_GC;
       Points   : in Gdk.Types.Gdk_Points_Array);

   procedure Draw_Segments
      (Drawable : in Gdk_Drawable'Class;
       Gc       : in Gdk.GC.Gdk_GC;
       Segs     : in Gdk.Types.Gdk_Segments_Array);

   procedure Draw_Lines
      (Drawable : in Gdk_Drawable'Class;
       Gc       : in Gdk.GC.Gdk_GC;
       Points   : in Gdk.Types.Gdk_Points_Array);

end Gdk.Drawable;



