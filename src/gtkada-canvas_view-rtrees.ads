------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
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

--  This package provides a R-Tree datastructure.
--  This is an efficient data structure for geospatial queries, like find all
--  objects within a given rectangle.
--  See algorithm in
--     http://www-db.deis.unibo.it/courses/SI-LS/papers/Gut84.pdf

package Gtkada.Canvas_View.Rtrees is

   Default_Min_Children : constant Positive := 3;
   Default_Max_Children : constant Positive := 6;

   type Rtree (Min_Children, Max_Children : Positive) is tagged private;
   --  A data structure for storing geospatial information. It splits the
   --  space into cells that contain a maximum number of objects, and makes it
   --  easy to find out objects within a given region.
   --  Min_Children is the minimum number of children in a cell before it is
   --  merged with a sibling cell.
   --  Max_Children is the maximum number of children in a cell before it is
   --  split.

   function Find
      (Self : Rtree; Rect : Model_Rectangle)
      return Items_Lists.List;
   --  Find all the objects that intersect with the given rectangle.

   procedure Insert
      (Self : in out Rtree;
       Item : not null access Abstract_Item_Record'Class);
   --  Add a new item to the tree. The object must already have a position.

   procedure Clear (Self : in out Rtree);
   --  Remove all nodes from the tree.
   --  The objects are not destroyed.

   function Is_Empty (Self : Rtree) return Boolean;
   --  Whether the rtree is empty

   function Bounding_Box (Self : Rtree) return Model_Rectangle;
   --  Return the minimal rectangle that encloses all the elements in the tree

   procedure For_Each_Object
      (Self     : Rtree;
       Callback : not null access procedure
          (Item : not null access Abstract_Item_Record'Class);
       In_Area  : Model_Rectangle := No_Rectangle);
   --  Executes Callback for each item in the given area (or in the whole tree)

   procedure Dump_Debug (Self : Rtree);
   --  Debug: print the tree.

private

   type Box is tagged;
   type Box_Access is access all Box'Class;
   type Box_Array is array (Natural range <>) of Box_Access;
   type Box (Max_Children_Plus_1 : Natural) is tagged record
      Rect     : Model_Rectangle := (0.0, 0.0, 0.0, 0.0);
      Object   : Abstract_Item;  --  leaf nodes only
      Parent   : Box_Access;
      Children : Box_Array (1 .. Max_Children_Plus_1);
   end record;

   type Rtree (Min_Children, Max_Children : Positive) is tagged record
      Root : Box_Access;
   end record;

end Gtkada.Canvas_View.Rtrees;
