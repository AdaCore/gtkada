------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2014, AdaCore                           --
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

generic
   with procedure Get_Size (V : Vertex_Access; Width, Height : out Gdouble);
   --  returns the size of the vertex

   with procedure Set_Position (V : Vertex_Access; X, Y : Gdouble);
   --  Set the position of the item

package Glib.Graphs.Layouts is

   procedure Layer_Layout
     (G                    : in out Graph;
      Horizontal           : Boolean := True;
      Space_Between_Layers : Gdouble := 20.0;
      Space_Between_Items  : Gdouble := 10.0);
   --  Set the position of the vertices so that they are organized into
   --  layers.
   --  For a horizontal layout, a vertex will always be in a column to the
   --  right of all its ancestor vertices.
   --  For a vertical layout, a vertex will always be in a row below all its
   --  ancestor vertices.
   --
   --  This code is provided as an example. It might be changed (or even
   --  removed) in the future.
   --  Such layout algorithms are heuristics, there is no exact algorithm that
   --  would give the perfect layout.

end Glib.Graphs.Layouts;
