------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2014, AdaCore                          --
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

package Gtkada.Canvas_View.Models.Layers is

   procedure Layout
     (Self                 : not null access Canvas_Model_Record'Class;
      Horizontal           : Boolean := True;
      Add_Waypoints        : Boolean := False;
      Space_Between_Items  : Gdouble := 10.0;
      Space_Between_Layers : Gdouble := 20.0);
   --  This algorithm is a wrapper for Glib.Graphs.Layouts.Layer_Layout.
   --
   --  It Organizes the items into layers: items in layer n never have an
   --  out-link to any item in a layer 1 .. (n - 1). Within a layer, the
   --  items are then reorganized to try and minimize the edge crossings.
   --
   --  If Add_Waypoints is True, then long edges are split with waypoints.
   --  This generally limits the number of edge crossing, but make moving
   --  items move cumbersome since the waypoints are not moved at the same
   --  time.
   --
   --  It is provided as an example, and might be changed or removed in the
   --  future.

end Gtkada.Canvas_View.Models.Layers;
