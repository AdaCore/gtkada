------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                      --
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
   --  Must be instantiated at library-level

   type Base_Dummy_Vertex is new Vertex with record
      Layer : Integer;
   end record;
   --  A vertex that is not part of the original graph

   generic
      type Dummy_Vertex is new Base_Dummy_Vertex with private;
      --  Type to use for dummy vertices. This parameter can generally be set
      --  to Base_Dummy_Vertex itself, unless you override
      --  Replaced_With_Dummy_Vertices and need custom attributes in dummy
      --  vertices. If you are using a type other than Base_Dummy_Vertex, then
      --  Set_Position will be called on that vertex

      with procedure Replaced_With_Dummy_Vertices
        (Replaced_Edge : Edge_Access;
         Dummies       : Vertices_Array) is null;
      --  The layout algorithm may have to create dummy vertices to break long
      --  edges that span multiple layers. This is used to reserve space for
      --  these edges.
      --  This procedure is called when a long edge is broken down into smaller
      --  edges. The edge is replaced with several edges:
      --     Get_Src (Replaced_Edge) --> Dummies (1) --> Dummies (2) -> ...
      --         -> Get_Dest (Replaced_Edge)
      --
      --  You can use this procedure if you want to do the same replacement in
      --  your own graph, perhaps to show the dummy vertices to the user.
      --  When the procedure is called, the dummy vertices have already been
      --  allocated (of type Dummy_Vertex), but you might need to further
      --  initialize them if you have added specific fields.

   package Layered_Layouts is

      procedure Layout
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
      --  Such layout algorithms are heuristics, there is no exact algorithm
      --  that would give the perfect layout.

   end Layered_Layouts;

end Glib.Graphs.Layouts;
