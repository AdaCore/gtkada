------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
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

--  Various support utilities for the models

with Gtkada.Canvas_View.Rtrees;   use Gtkada.Canvas_View.Rtrees;

package Gtkada.Canvas_View.Models is

   ------------
   -- Rtrees --
   ------------
   --  A wrapper around another model, which provides efficient geospatial
   --  queries, like finding the smaller enclosing rectangle for all the
   --  items, or all the items within a given region.
   --  The items are stored in the base model, and the Rtree model adds
   --  an extra data structure on top of it to speed up the queries.
   --  When new items are added to the model, Refresh_Layout *must* be
   --  called to refresh the internal cache (but this should always be done
   --  in any case to refresh the display of links for instance).
   --
   --  Wrapping a model in a Rtree means that the speed of displaying the
   --  canvas (and more importantly scrolling it) now depends on the number
   --  of items on the screen, not the total number of items in the model).
   --  As a result, it is possible to have models with hundreds of
   --  thousands of items.

   generic
      type Base_Model_Record is new Canvas_Model_Record with private;
      --  The underlying implementation of the model (which for instance
      --  provides support for Add and Remove), and for which the Rtree
      --  is used as a wrapper.

   package Rtree_Models is
      type Rtree_Model_Record is new Base_Model_Record with private;
      type Rtree_Model is access all Rtree_Model_Record'Class;

      procedure Gtk_New (Self : out Rtree_Model);
      procedure Initialize
         (Self : not null access Rtree_Model_Record'Class);
      --  Create a new Rtree model.

      overriding procedure For_Each_Item
        (Self     : not null access Rtree_Model_Record;
         Callback : not null access procedure
           (Item : not null access Abstract_Item_Record'Class);
         Selected_Only : Boolean := False;
         Filter        : Item_Kind_Filter := Kind_Any;
         In_Area       : Model_Rectangle := No_Rectangle);
      overriding function Bounding_Box
        (Self   : not null access Rtree_Model_Record;
         Margin : Model_Coordinate := 0.0)
         return Model_Rectangle;
      overriding function Toplevel_Item_At
        (Self    : not null access Rtree_Model_Record;
         Point   : Model_Point;
         Context : Draw_Context) return Abstract_Item;
      overriding procedure Refresh_Layout
         (Self        : not null access Rtree_Model_Record;
          Send_Signal : Boolean := True);

   private
      type Rtree_Model_Record is new Base_Model_Record with record
         Items_Tree, Links_Tree : Rtree
           (Min_Children => Default_Min_Children,
            Max_Children => Default_Max_Children);
      end record;
   end Rtree_Models;

end Gtkada.Canvas_View.Models;
