------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2014, AdaCore                     --
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

--  Various support utilities for the drawing of links in GtkAda.Canvas

with Cairo;   use Cairo;
with Glib;    use Glib;

package Gtkada.Canvas.Links is

   type Link_Layout is record
      From_Box          : Cairo_Rectangle;
      From_Bounding_Box : Cairo_Rectangle;
      To_Box            : Cairo_Rectangle;
      To_Bounding_Box   : Cairo_Rectangle;
      Fx, Fy            : Gdouble;
      Tx, Ty            : Gdouble;
   end record;
   --  Various dimensions computed for the layout of links.
   --  It contains the start and end points for the link, as well as the
   --  bounding boxes.

   procedure Draw_Orthogonal_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Show_Annotation : Boolean);
   --  Draw a link on the screen, as possibly several orthogonal lines.
   --  This link includes both an arrow head on its destination, and an
   --  optional text displayed approximatively in its middle.

   procedure Draw_Straight_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Show_Annotation : Boolean);
   --  Draw Link on the screen as a straight line.
   --  This link includes both an arrow head on its destination, and an
   --  optional text displayed approximatively in its middle.

   procedure Draw_Arc_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Offset          : Gdouble;
      Show_Annotation : Boolean);
   --  Draw Link on the screen.
   --  The link is drawn as a curved link (ie there is an extra handle in its
   --  middle).
   --  This link includes both an arrow head on its destination, and an
   --  optional text displayed approximatively in its middle.

   procedure Draw_Self_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Offset          : Gint;
      Show_Annotation : Boolean);
   --  Draw a link when its source and destination items are the same

   procedure Clip_Line
     (Src   : access Canvas_Item_Record'Class;
      Canvas : access Interactive_Canvas_Record'Class;
      To_X  : Glib.Gdouble;
      To_Y  : Glib.Gdouble;
      X_Pos : Glib.Gdouble;
      Y_Pos : Glib.Gdouble;
      Side  : out Item_Side;
      X_Out : out Glib.Gdouble;
      Y_Out : out Glib.Gdouble);
   --  Clip the line that goes from Src at pos (X_Pos, Y_Pos) to (To_X, To_Y)
   --  in world coordinates.
   --  The intersection between that line and the border of Rect is returned
   --  in (X_Out, Y_Out). The result should be in world coordinates.
   --  X_Pos and Y_Pos have the same meaning as Src_X_Pos and Src_Y_Pos in the
   --  link record.
   --  This procedure is called when computing the position for the links
   --  within the default Draw_Link procedure. The default implementation only
   --  works with rectangular items. The computed coordinates are then passed
   --  on directly to Draw_Straight_Line.

end Gtkada.Canvas.Links;
