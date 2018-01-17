------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

with Glib;    use Glib;

package Gtkada.Canvas_View.Links is

   function Circle_From_Bezier
     (Center   : Item_Point;
      Radius   : Glib.Gdouble) return Item_Point_Array;
   --  Return the waypoints needed to draw a circle via a bezier curve in
   --  Draw_Polycurve.

   procedure Compute_Layout_For_Straight_Link
     (Link    : not null access Canvas_Link_Record'Class;
      Context : Draw_Context);
   --  Compute the layout for the link, with a straight line (although
   --  additional waypoints could be added).
   --  The layout is cached in the link itself, and will be used when drawing.

   procedure Compute_Layout_For_Arc_Link
     (Link    : not null access Canvas_Link_Record'Class;
      Context : Draw_Context;
      Offset  : Gdouble := 10.0);
   --  Compute the layout for the link, with a curve link.
   --  The offset can be used to make the link more or less curved, in
   --  particular when there are multiple links between the same two objects,
   --  so that they do not override.

   procedure Compute_Layout_For_Orthogonal_Link
     (Link    : not null access Canvas_Link_Record'Class;
      Context : Draw_Context);
   --  Compute the layout for the link, when it is restricted to vertical and
   --  horizontal lines only.

   procedure Compute_Layout_For_Curve_Link
     (Link    : not null access Canvas_Link_Record'Class;
      Context : Draw_Context);
   --  Compute the layout for the link

   function Prepare_Path
     (Link    : not null access Canvas_Link_Record'Class;
      Context : Draw_Context) return Boolean;
   --  Prepare a cairo path for the link.
   --  The path is not displayed, so this can be used for various purposes like
   --  computing intersections.
   --  The path does not include any of the labels.
   --  Return True if the path could be prepared successfully

   procedure Draw_Link
     (Link     : not null access Canvas_Link_Record'Class;
      Context  : Draw_Context;
      Selected : Boolean);
   --  Draw the link, using the layout computed earlier via one of the
   --  procedures above. This is a help method for the Draw primitive
   --  operation on the link, which should be used preferrably.

   function Compute_Bounding_Box
     (Points   : Item_Point_Array;
      Relative : Boolean := False) return Item_Rectangle;
   --  Compute the minimum rectangle that encloses all points

end Gtkada.Canvas_View.Links;
