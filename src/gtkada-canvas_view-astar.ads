------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                   Copyright (C) 1998-2013 E. Briot                       --
--                     Copyright (C) 2014-2018, AdaCore                     --
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

--  This package implements the A* algorithm for optimal path finding

package Gtkada.Canvas_View.Astar is

   type Coordinate is record
      X, Y : Integer;
   end record;

   type Coordinate_Array is array (Natural range <>) of Coordinate;

   No_Coordinate : constant Coordinate;
   Not_Traversable : constant Integer;

   generic
      type User_Data is private;
      --  extra data passed to all subprograms, typically containing the grid
      --  to explore

      with function Heuristic_Cost
        (Self : User_Data; Parent, From, To : Coordinate) return Integer;
      --  Estimate the cost for the segment from From to To.
      --  Cost is the cost of taking this route.
      --  The point is unwalkable if Cost is negative (or Not_Traversable).
      --  To get accurate path finding, the cost from moving from one point to
      --  another should be greated than the change in Heuristic_Dist for the
      --  same move.
      --  Likewise, the cost should never be 0, or Find_Path can end up with a
      --  path like:
      --      xxxx                    xx..
      --      ...x    instead of      .x..
      --      .xxx                    .x..
      --      .x..                    .x..
      --  To is a neighbor of From, as returned by Next_Point.

      with function Next_Point
        (Self : User_Data;
         From : Coordinate;
         Nth  : Positive) return Coordinate;
      --  Return the Nth-possible successor of From. If there are no more
      --  successor, setreturn No_Coordinate.
      --  This function does not need to trim the possible successors, this is
      --  done by the Heuristic_Cost function above.

      with function Heuristic_Dist
         (Self : User_Data; P1, P2 : Coordinate) return Integer;
      --  Return an evaluation of the distance from P1 to P2. This is the
      --  distance is straight lines, not taking into account the cost of the
      --  moves. This algorithm works better when using the Manhattan distance.

   function Find_Path
     (Self     : User_Data;
      From, To : Coordinate;
      Parent   : Coordinate) return Coordinate_Array;
   --  Return the optimal path from From to To.
   --  Parent is the point that came before From (in case the first segment
   --  is imposed for instance, to get away from the item)

   ----------------
   -- Next point --
   ----------------
   --  Some example functions for Next_Point

   generic
      type User_Data is private;
   function Manhattan_Next_Point
     (Self : User_Data; From : Coordinate; Nth : Positive) return Coordinate;
   --  Points to the four possible neighboards in a horizontal and vertical
   --  grid.

private
   No_Coordinate : constant Coordinate := (Integer'First, Integer'First);
   Not_Traversable : constant Integer := Integer'First;
end Gtkada.Canvas_View.Astar;
