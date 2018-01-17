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

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Multisets;

package body Gtkada.Canvas_View.Astar is

   Abort_Path : constant := 200;
   --  Maximum number of Coordinates to examine before an abort.

   type Star_Coordinate is record
      P    : Coordinate;
      Gval : Integer;  --  How far we have already gone in the A* algorithm
      Hval : Integer;  --  Estimate how far is left in the A* algorith
      Parent : Coordinate;
   end record;

   function "<" (P1, P2 : Star_Coordinate) return Boolean;

   package Coordinate_Set is new Ada.Containers.Ordered_Multisets
     (Element_Type => Star_Coordinate);
   use Coordinate_Set;

   function Hash (P : Coordinate) return Ada.Containers.Hash_Type;
   package Coordinate_Htable is new Ada.Containers.Hashed_Maps
     (Key_Type        => Coordinate,
      Element_Type    => Coordinate,  --  The Coordinate we were coming from
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");
   use Coordinate_Htable;

   function Search
     (Tree : Coordinate_Set.Set; P : Coordinate) return Coordinate_Set.Cursor;
   --  Search in tree for a record matching P

   ----------
   -- Hash --
   ----------

   function Hash (P : Coordinate) return Ada.Containers.Hash_Type is
      --  The technique to assign a unique key to (X, Y) is:
      --      1  2  4  7 11 16 22
      --      3  5  8 12 17 23
      --      6  9 13 18 24
      --     10 14 19 25
      --     15 20 26
      --     21 27
      --     28
      --  The index is computed with:
      --    index (X, Y) = (X + Y - 1) * (X + Y - 2) / 2 + Y

      use Ada.Containers;
      Tmp : constant Integer := P.X + P.Y - 1;
   begin
      return Hash_Type (Tmp * (Tmp - 1) / 2 + P.Y) mod Hash_Type'Last;
   end Hash;

   ---------
   -- "<" --
   ---------

   function "<" (P1, P2 : Star_Coordinate) return Boolean is
   begin
      return P1.Gval + P1.Hval < P2.Gval + P2.Hval;
   end "<";

   ------------
   -- Search --
   ------------

   function Search
     (Tree : Coordinate_Set.Set; P : Coordinate) return Coordinate_Set.Cursor
   is
      Iter : Coordinate_Set.Cursor := First (Tree);
   begin
      while Iter /= Coordinate_Set.No_Element loop
         exit when Element (Iter).P = P;
         Next (Iter);
      end loop;

      return Iter;
   end Search;

   ---------------
   -- Find_Path --
   ---------------

   function Find_Path
     (Self     : User_Data;
      From, To : Coordinate;
      Parent   : Coordinate) return Coordinate_Array
   is
      Open     : Coordinate_Set.Set;
      Visited  : Coordinate_Htable.Map;
      N        : Star_Coordinate;
      Next     : Coordinate;
      Iter     : Coordinate_Set.Cursor;
      Cost     : Integer;
      Count    : Natural;
      Nodes_Removed : Integer := 0;
      Stored   : Coordinate;
      PIter    : Coordinate_Htable.Cursor;
      Success  : Boolean;
   begin
      --  Insert the original node in the open list
      Insert (Open, (P        => From,
                     Gval     => 0,
                     Hval     => Heuristic_Dist (Self, From, To),
                     Parent   => Parent),
              Iter);

      while not Is_Empty (Open) loop
         Iter := First (Open);
         N := Element (Iter);
         Delete (Open, Iter);

         Insert (Visited,
                 Key => N.P,
                 New_Item => N.Parent,
                 Position => PIter,
                 Inserted => Success);

         Nodes_Removed := Nodes_Removed + 1;
         if Nodes_Removed >= Abort_Path then
            return (1 => From, 2 => To);
         end if;

         exit when N.P = To;

         for Num in 1 .. Positive'Last loop
            Next := Next_Point (Self, N.P, Num);
            exit when Next = No_Coordinate;

            if Next /= N.Parent then
               Cost := Heuristic_Cost (Self, N.Parent, N.P, Next);

               if Cost > 0 then
                  --  Is this Coordinate already in the closed list ?
                  if Find (Visited, Next) = Coordinate_Htable.No_Element then
                     Iter := Search (Open, Next);
                     if Iter = Coordinate_Set.No_Element then
                        Insert (Open,
                                (P      => Next,
                                 Gval   => N.Gval + Cost,
                                 Hval   => Heuristic_Dist (Self, Next, To),
                                 Parent => N.P),
                                Iter);
                     elsif N.Gval + Cost < Element (Iter).Gval then
                        Delete (Open, Iter);
                        Insert (Open,
                                (P      => Next,
                                 Gval   => N.Gval + Cost,
                                 Hval   => Heuristic_Dist (Self, Next, To),
                                 Parent => N.P),
                                Iter);
                     end if;
                  end if;
               end if;
            end if;
         end loop;
      end loop;

      PIter := Find (Visited, To);
      Count  := 0;

      if Has_Element (PIter) then
         Stored := To;
         while Stored /= From loop
            Stored := Element (Visited, Stored);
            Count := Count + 1;
         end loop;
      end if;

      if Count = 0 then
         return (From, To);
      else
         declare
            Arr   : Coordinate_Array (1 .. Count + 1);
            Index : Natural := Arr'Last;
         begin
            Stored := To;
            while Stored /= From loop
               Arr (Index) := Stored;
               Stored      := Element (Visited, Stored);
               Index       := Index - 1;
            end loop;

            Arr (1) := From;

            Clear (Open);

            --  Free the memory
            Clear (Visited);
            return Arr;
         end;
      end if;
   end Find_Path;

   --------------------------
   -- Manhattan_Next_Point --
   --------------------------

   function Manhattan_Next_Point
     (Self : User_Data; From : Coordinate; Nth : Positive) return Coordinate
   is
      pragma Unreferenced (Self);
   begin
      case Nth is
         when 1 =>
            return (From.X, From.Y - 1);
         when 2 =>
            return (From.X + 1, From.Y);
         when 3 =>
            return (From.X, From.Y + 1);
         when 4 =>
            return (From.X - 1, From.Y);
         when others =>
            return No_Coordinate;
      end case;
   end Manhattan_Next_Point;

end Gtkada.Canvas_View.Astar;
