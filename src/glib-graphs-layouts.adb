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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Tags;                            use Ada.Tags;
with Ada.Unchecked_Deallocation;
with GNAT.Heap_Sort_G;

package body Glib.Graphs.Layouts is

   Preferred_Length : constant := 1;
   --  Number of layers between edge ends (this is for future extension, so
   --  that some edges might be forced to span layers.

   Add_Dummy_Nodes : constant Boolean := True;
   --  Whether to add dummy (invisible node) for edges that span multiple
   --  layers.

   Dummy_Node_Size : constant Gdouble := 4.0;
   --  Size of the dummy nodes (since we also have margins, we might as well
   --  keep those nodes small).

   Default_Layer : constant Integer := 0;

   type Integer_Array is array (Integer range <>) of Integer;
   type Integer_Array_Access is access Integer_Array;
   --  maps vertices to some data

   procedure Make_Acyclic (G : in out Graph);
   --  Make sure the graph is acyclic

   package Vertex_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Vertex_Access);
   use Vertex_Lists;
   type Layer_Info_Array is array (Integer range <>) of Vertex_Lists.List;
   type Layer_Info_Array_Access is access Layer_Info_Array;

   type Layout_Info is record
      Horizontal           : Boolean;
      Space_Between_Layers : Gdouble;
      Space_Between_Items  : Gdouble;

      Min_Layer, Max_Layer : Integer;

      In_Layers  : Layer_Info_Array_Access;
      --  The ordered list of items in each layer

      Layers     : Integer_Array_Access;
      --  For each vertex, its assigned layer
   end record;

   procedure Free (Self : in out Layout_Info);
   --  Free memory used by Self

   function Slack (Info : Layout_Info; Edge : Edge_Access) return Integer;
   --  Returns the slack for an edge. When greater than 0, the edge could
   --  be tightened to lead to a nicer layer

   function Layer (Info : Layout_Info; V : Vertex_Access) return Integer;
   --  Return the layer for a vertex

   procedure Adjust_Positions
     (G          : Graph;
      Info       : Layout_Info);
   --  Adjust the position of the items within their layer.
   --  Items must hav already been ordered, and they are moved a little so that
   --  they tend to align with their parent and child nodes

   procedure Sort_Nodes_Within_Layers
     (G            : Graph;
      Info         : in out Layout_Info);
   --  Sort the nodes within each layer so as to minimize crossing of edges.
   --  To do this, we use a Median or Barycenter Heuristic.
   --  This is also similar to what graphize uses to reorder nodes within a
   --  layer to minimize edge crossing. See for instance:
   --     "The barycenter Heuristic and the reorderable matrix"
   --     Erkki Makinen, Harri Siirtola
   --     http://www.informatica.si/PDF/29-3/
   --        13_Makinen-The%20Barycenter%20Heuristic....pdf
   --
   --  See also
   --     http://www.graphviz.org/Documentation/TSE93.pdf
   --
   --  Basically, for each layer, we order the nodes based on the barycenter
   --  of their neighbor nodes, and repeat for each layer.

   type Weight_Info is record
      Weight : Gdouble;
      Vertex : Vertex_Access;
   end record;
   type Weight_Array is array (Integer range <>) of Weight_Info;

   procedure Sort (W : in out Weight_Array);
   --  sort the array by weight
   --  Precondition: W'First = 0
   --  Sorts 1 .. W'Last elements

   procedure Init_Rank
     (G         : Graph;
      Info      : in out Layout_Info);
   --  Computes an initial feasible ranking (i.e where nodes are
   --  organized such that children nodes are in layers higher than their
   --  parents). This always assigns root nodes (with no in-edges) to
   --  layer 0. This might result in non-tight edges, for instance:
   --       /--F
   --     A -> B -> C -> D
   --     E -----------/
   --
   --  ??? This algorithm requires computation of in-edges, which is
   --  not always available for all types of graphs. Seems that we could
   --  replace it with a DFS, where leaf nodes are assigned to layer 0
   --  (so the ordering would be different, but since we are tightening
   --  edges afterward it doesn't really matter).

   procedure Organize_Nodes
     (G    : Graph;
      Info : in out Layout_Info);
   --  Compute the position of nodes within each layer.
   --  We provide an initial ordering for elements: starting from nodes
   --  at the lowest layer (rightmost or topmost item depending on
   --  layout), we do a breadth-first-search, and add each child in to
   --  its respective layer. This ensures that for the spanning tree at
   --  least there are no edge crossings.

   procedure Rank_Items (G : in out Graph; Info : in out Layout_Info);
   --  Compute the layer for each item

   ----------
   -- Tree --
   ----------

   package Edge_Lists is new Ada.Containers.Doubly_Linked_Lists (Edge_Access);
   use Edge_Lists;

   type Edge_Array is array (Integer range <>) of Edge_Lists.List;

   type Tree (Max_Index : Natural) is record
      Node_Count : Natural := 0;

      Node_In_Tree : Integer_Array (Min_Vertex_Index .. Max_Index) :=
        (others => -1);
      --  This is used to test whether the corresponding node from the graph is
      --  in the tree.
      --  Since the graph might include several disjoint components, the value
      --  in this array indicates which component the node is part of.

      Edges : Edge_Array (Min_Vertex_Index .. Max_Index);
      --  For each vertex, the list of tree edges that start from it.

      Disjoint_Components : Natural := 0;
      --  Number of disjoint sets in thetree
   end record;
   --  A spanning tree for the graph.

   procedure Add_Edge (Self : in out Tree; E : Edge_Access);
   procedure Add_Vertex (Self : in out Tree; V : Vertex_Access);
   --  Add a new edge or vertex to the tree.

   function In_Tree (Self : Tree; V : Vertex_Access) return Boolean;
   --  Whether the vertex is already in the tree

   function Is_Spanning (Self : Tree) return Boolean;
   --  Whether all nodes are in the tree (i.e we have a full spanning tree for
   --  the graph).

   procedure Normalize_Layers (Spanning : Tree; Info : in out Layout_Info);
   --  Normalize the layers so that each independenct component starts at
   --  layer 0. This leads to nicer layout, since independent components
   --  are aligned

   procedure Feasible_Tree
     (G        : Graph;
      Info     : in out Layout_Info;
      Spanning : out Tree);
   --  Computes an initial feasible tree. This is a spanning tree for the
   --  graph so that all of its edges are tight (which for instance will
   --  tighten the link E->D in the example above).
   --  This changes layer assignment for the vertices.

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Layout_Info) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Integer_Array, Integer_Array_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Layer_Info_Array, Layer_Info_Array_Access);

   begin
      Unchecked_Free (Self.In_Layers);
      Unchecked_Free (Self.Layers);
   end Free;

   -----------
   -- Slack --
   -----------

   function Slack (Info : Layout_Info; Edge : Edge_Access) return Integer is
   begin
      return Info.Layers (Get_Index (Get_Dest (Edge)))
        - Info.Layers (Get_Index (Get_Src (Edge)))
        - Preferred_Length;
   end Slack;

   -----------
   -- Layer --
   -----------

   function Layer (Info : Layout_Info; V : Vertex_Access) return Integer is
   begin
      if V.all in Base_Dummy_Vertex'Class then
         return Base_Dummy_Vertex (V.all).Layer;
      else
         return Info.Layers (Get_Index (V));
      end if;
   end Layer;

   --------------
   -- Add_Edge --
   --------------

   procedure Add_Edge (Self : in out Tree; E : Edge_Access) is
      Sindex : constant Integer := Get_Index (Get_Src (E));
   begin
      Add_Vertex (Self, Get_Src (E));
      Add_Vertex (Self, Get_Dest (E));
      Self.Edges (Sindex).Append (E);
   end Add_Edge;

   ----------------
   -- Add_Vertex --
   ----------------

   procedure Add_Vertex (Self : in out Tree; V : Vertex_Access) is
   begin
      if not In_Tree (Self, V) then
         Self.Node_Count := Self.Node_Count + 1;
         Self.Node_In_Tree (Get_Index (V)) := Self.Disjoint_Components;
      end if;
   end Add_Vertex;

   -------------
   -- In_Tree --
   -------------

   function In_Tree (Self : Tree; V : Vertex_Access) return Boolean is
   begin
      return Self.Node_In_Tree (Get_Index (V)) /= -1;
   end In_Tree;

   -----------------
   -- Is_Spanning --
   -----------------

   function Is_Spanning (Self : Tree) return Boolean is
   begin
      return Self.Node_Count = Self.Node_In_Tree'Length;
   end Is_Spanning;

   ------------------
   -- Make_Acyclic --
   ------------------

   procedure Make_Acyclic (G : in out Graph) is
      Acyclic : aliased Boolean;
      Sorted  : constant Depth_Vertices_Array := Depth_First_Search
        (G               => G,
         Acyclic         => Acyclic'Access,
         Reverse_Edge_Cb => Revert_Edge'Access);
      pragma Unreferenced (Sorted);
   begin
      null;
   end Make_Acyclic;

   ----------
   -- Sort --
   ----------

   procedure Sort (W : in out Weight_Array) is
      procedure Move (From, To : Natural);
      function Lt (Op1, Op2 : Natural) return Boolean;

      procedure Move (From, To : Natural) is
      begin
         W (To) := W (From);
      end Move;

      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         return W (Op1).Weight < W (Op2).Weight;
      end Lt;

      package HS is new GNAT.Heap_Sort_G (Move, Lt);
   begin
      HS.Sort (W'Last);
   end Sort;

   ----------------------
   -- Normalize_Layers --
   ----------------------

   procedure Normalize_Layers (Spanning : Tree; Info : in out Layout_Info) is
      Min_Layer : Integer_Array (1 .. Spanning.Disjoint_Components) :=
        (others => Integer'Last);
      --  The minimal layer used for each of the independent components

      Component : Integer;
   begin
      for V in Spanning.Node_In_Tree'Range loop
         Component := Spanning.Node_In_Tree (V);
         Min_Layer (Component) :=
           Integer'Min (Min_Layer (Component), Info.Layers (V));
      end loop;

      for V in Spanning.Node_In_Tree'Range loop
         Component := Spanning.Node_In_Tree (V);
         Info.Layers (V) := Info.Layers (V) - Min_Layer (Component);
      end loop;
   end Normalize_Layers;

   ------------------------------
   -- Sort_Nodes_Within_Layers --
   ------------------------------

   procedure Sort_Nodes_Within_Layers
     (G            : Graph;
      Info         : in out Layout_Info)
   is
      Max_Iterations : constant := 8;
      Max_I          : constant Integer := Max_Index (G);
      Position       : Integer_Array (Min_Vertex_Index .. Max_I);

      procedure Do_Iteration (Layer : Integer; Downward : Boolean);
      procedure Do_Iteration (Layer : Integer; Downward : Boolean) is
         Weights        : Weight_Array (0 .. Max_I + 1);
         C              : Vertex_Lists.Cursor := Info.In_Layers (Layer).First;
         Src, Dest      : Vertex_Access;
         Current_C      : Integer := Weights'First + 1;
         Eit            : Edge_Iterator;
         Total, Count   : Integer;
      begin
         while Has_Element (C) loop
            Dest := Element (C);
            Total := 0;
            Count := 0;

            if Downward then
               Eit := First (G, Src => Dest);
            else
               Eit := First (G, Dest => Dest);
            end if;

            while not At_End (Eit) loop
               if Downward then
                  Src := Get_Dest (Get (Eit));
               else
                  Src := Get_Src (Get (Eit));
               end if;

               --  ignore self-links.
               --  Only take into account tight edges (where nodes are in
               --  adjacent layers), which is the default if we added dummy
               --  nodes.

               if Src /= Dest
                 and then (Add_Dummy_Nodes
                           or else Slack (Info, Get (Eit)) = 0)
               then
                  Total := Total + Position (Get_Index (Src));
                  Count := Count + 1;
               end if;

               Next (Eit);
            end loop;

            if Count = 0 then
               --  leave the item in place
               Weights (Current_C) :=
                 (Gdouble (Position (Get_Index (Dest))), Dest);
            else
               Weights (Current_C) :=
                 (Gdouble (Total) / Gdouble (Count), Dest);
            end if;

            Current_C := Current_C + 1;
            Next (C);
         end loop;

         --  Now sort based on weights

         Sort (Weights (0 .. Current_C - 1));
         Info.In_Layers (Layer).Clear;
         for W in 1 .. Current_C - 1 loop
            Position (Get_Index (Weights (W).Vertex)) := W;
            Info.In_Layers (Layer).Append (Weights (W).Vertex);
         end loop;
      end Do_Iteration;

      C              : Vertex_Lists.Cursor;
      Current_C      : Integer;

   begin
      --  Store the position of elements within each layer

      for L in Info.In_Layers'Range loop
         C         := Info.In_Layers (L).First;
         Current_C := 1;
         while Has_Element (C) loop
            Position (Get_Index (Element (C))) := Current_C;
            Current_C := Current_C + 1;
            Next (C);
         end loop;
      end loop;

      for Iteration in 0 .. Max_Iterations - 1 loop
         if Iteration mod 2 = 0 then
            for L in reverse Info.In_Layers'First .. Info.In_Layers'Last - 1
            loop
               Do_Iteration (L, Downward => True);
            end loop;
         else
            for L in Info.In_Layers'First + 1 .. Info.In_Layers'Last loop
               Do_Iteration (L, Downward => False);
            end loop;
         end if;
      end loop;
   end Sort_Nodes_Within_Layers;

   ----------------------
   -- Adjust_Positions --
   ----------------------

   procedure Adjust_Positions
     (G          : Graph;
      Info       : Layout_Info)
   is
      type Box is record
         X, Y, W, H  : Gdouble;
         Space_After : Gdouble;  --  between item and the next
      end record;
      Boxes     : array (Min_Vertex_Index .. Max_Index (G)) of Box;

      procedure Do_Iteration (Layer : Integer; Downward : Boolean);
      procedure Do_Iteration (Layer : Integer; Downward : Boolean) is
         C        : Vertex_Lists.Cursor := Info.In_Layers (Layer).First;
         Lowest   : Gdouble := Gdouble'First;
         Highest  : Gdouble;
         Total    : Gdouble;
         Count    : Integer;
         New_Pos  : Gdouble;
         Src      : Vertex_Access;
         Eit      : Edge_Iterator;
         Current, Next_Item : Vertex_Access;
         Current_B            : Box;  --  size for Current
         Next_B               : Box;  --  size for Next_Item
         Child_B              : Box;
      begin
         if Has_Element (C) then
            Next_Item := Element (C);
            Next_B := Boxes (Get_Index (Next_Item));
         end if;

         while Next_Item /= null loop
            Total := 0.0;
            Count := 0;

            --  Find the range of coordinates allowed for the current item

            Current   := Next_Item;
            Current_B := Next_B;

            Next (C);
            if Has_Element (C) then
               Next_Item := Element (C);
               Next_B := Boxes (Get_Index (Next_Item));

               if Info.Horizontal then
                  Highest := Next_B.Y;
               else
                  Highest := Next_B.X;
               end if;
            else
               Next_Item := null;
               Highest := Gdouble'Last;
            end if;

            --  Now take a look at all its neighbors, either in previous
            --  or later layers, depending on the iteration

            if Downward then
               Eit := First (G, Src => Current);
            else
               Eit := First (G, Dest => Current);
            end if;

            while not At_End (Eit) loop
               if Downward then
                  Src := Get_Dest (Get (Eit));
               else
                  Src := Get_Src (Get (Eit));
               end if;

               --  ignore self-links.
               --  Only take into account tight edges (where nodes are in
               --  adjacent layers), which is the default if we added dummy
               --  nodes.

               if Src /= Current
                 and then (Add_Dummy_Nodes
                           or else Slack (Info, Get (Eit)) = 0)
               then
                  Child_B := Boxes (Get_Index (Src));
                  Count := Count + 1;

                  if Info.Horizontal then
                     Total := Total + Child_B.Y + Child_B.H / 2.0;
                  else
                     Total := Total + Child_B.X + Child_B.W / 2.0;
                  end if;
               end if;

               Next (Eit);
            end loop;

            if Count /= 0 then
               New_Pos := Total / Gdouble (Count);

               if Info.Horizontal then
                  --  When we compute the highest possible position, we
                  --  do not include space_between_items. This gives a
                  --  chance to still move a vertex that would be blocked
                  --  between two others (which will also move the next
                  --  vertices)

                  New_Pos := New_Pos - Current_B.H / 2.0;
                  New_Pos := Gdouble'Min (New_Pos, Highest - Current_B.H);

               else
                  New_Pos := New_Pos - Current_B.W / 2.0;
                  New_Pos := Gdouble'Min (New_Pos, Highest - Current_B.W);
               end if;
            else
               if Info.Horizontal then
                  New_Pos := Current_B.Y;
               else
                  New_Pos := Current_B.X;
               end if;
            end if;

            New_Pos := Gdouble'Max (Lowest, New_Pos);

            if Info.Horizontal then
               Boxes (Get_Index (Current)).Y := New_Pos;
               Lowest := New_Pos + Current_B.H + Current_B.Space_After;
            else
               Boxes (Get_Index (Current)).X := New_Pos;
               Lowest := New_Pos + Current_B.W + Current_B.Space_After;
            end if;
         end loop;
      end Do_Iteration;

      C2        : Vertex_Lists.Cursor;
      Pos       : Gdouble := 0.0;
      Lowest    : Gdouble;
      Max_Size  : Gdouble;
      V         : Vertex_Access;
      Current_B : Box;  --  size for Current
   begin
      --  Compute the coordinates for each layer, and an initial position for
      --  items within each layer.

      for P in Info.In_Layers'Range loop
         Lowest   := 0.0;
         Max_Size := 0.0;

         C2 := Info.In_Layers (P).First;
         while Has_Element (C2) loop
            V := Element (C2);

            if V.all in Base_Dummy_Vertex'Class then
               Current_B.W := Dummy_Node_Size;
               Current_B.H := Dummy_Node_Size;
               Current_B.Space_After := 0.0;
            else
               Get_Size (V, Width => Current_B.W, Height => Current_B.H);
               Current_B.Space_After := Info.Space_Between_Items;
            end if;

            if Info.Horizontal then
               Max_Size := Gdouble'Max (Max_Size, Current_B.W);
               Current_B.X := Pos;
               Current_B.Y := Lowest;
               Lowest := Lowest + Current_B.H + Current_B.Space_After;
            else
               Max_Size := Gdouble'Max (Max_Size, Current_B.H);
               Current_B.X := Lowest;
               Current_B.Y := Pos;
               Lowest := Lowest + Current_B.W + Current_B.Space_After;
            end if;

            Boxes (Get_Index (V)) := Current_B;

            Next (C2);
         end loop;

         Pos := Pos + Max_Size + Info.Space_Between_Layers;
      end loop;

      --  Try to adjust position of nodes to align with parents and children

      for Iteration in 0 .. 8 loop
         if Iteration mod 2 = 0 then
            for P in
               reverse Info.In_Layers'First .. Info.In_Layers'Last - 1
            loop
               Do_Iteration (P, Downward => True);
            end loop;
         else
            for P in Info.In_Layers'First + 1 .. Info.In_Layers'Last loop
               Do_Iteration (P, Downward => False);
            end loop;
         end if;
      end loop;

      declare
         Vit   : Vertex_Iterator := First (G);
         V     : Vertex_Access;
      begin
         while not At_End (Vit) loop
            V := Get (Vit);
            if V'Tag /= Base_Dummy_Vertex'Tag then
               Current_B := Boxes (Get_Index (V));
               Set_Position (V, Current_B.X, Current_B.Y);
            end if;
            Next (Vit);
         end loop;
      end;
   end Adjust_Positions;

   ---------------
   -- Init_Rank --
   ---------------

   procedure Init_Rank
     (G         : Graph;
      Info      : in out Layout_Info)
   is
      Max_I     : constant Integer := Max_Index (G);
      Vit   : Vertex_Iterator := First (G);
      Queue : array (0 .. Max_I) of Vertex_Access;
      Q_Index : Integer := Queue'First;
      Q_Last  : Integer := Queue'First;
      --  The queue of nodes to visit

      S, D    : Vertex_Access;
      In_Degree : array (0 .. Max_I) of Integer := (others => 0);
      --  Number of remaining in-edges that have not been analyzed for
      --  each node.

      Layer : Integer;
      Eit   : Edge_Iterator;
      Edge  : Edge_Access;
      Deg   : Natural;
   begin
      Info.Min_Layer := Default_Layer;
      Info.Max_Layer := Default_Layer;

      while not At_End (Vit) loop
         S := Get (Vit);

         Deg := 0;
         Eit := First (G, Dest => S);
         while not At_End (Eit) loop
            --  Ignore self links
            if Get_Src (Get (Eit)) /= S then
               Deg := Deg + 1;
            end if;
            Next (Eit);
         end loop;

         In_Degree (Get_Index (S)) := Deg;
         if In_Degree (Get_Index (S)) = 0 then
            Queue (Q_Last) := S;
            Q_Last := Q_Last + 1;
         end if;
         Next (Vit);
      end loop;

      while Q_Index < Q_Last loop
         S := Queue (Q_Index);
         Q_Index := Q_Index + 1;

         --  Compute layer based on ancestors' own layers

         Layer := Default_Layer;
         Eit := First (G, Dest => S);
         while not At_End (Eit) loop
            Edge := Get (Eit);
            Layer := Integer'Max
              (Layer,
               Info.Layers (Get_Index (Get_Src (Edge)))
               + Preferred_Length);
            Next (Eit);
         end loop;

         Info.Layers (Get_Index (S)) := Layer;
         Info.Max_Layer := Integer'Max (Info.Max_Layer, Layer);

         --  Mark all outgoing edges as scanned, which might lead to new
         --  nodes to analyze.

         Eit := First (G, Src => S);
         while not At_End (Eit) loop
            Edge := Get (Eit);
            D := Get_Dest (Edge);
            In_Degree (Get_Index (D)) := In_Degree (Get_Index (D)) - 1;
            if In_Degree (Get_Index (D)) = 0 then
               Queue (Q_Last) := D;
               Q_Last := Q_Last + 1;
            end if;
            Next (Eit);
         end loop;
      end loop;
   end Init_Rank;

   --------------------
   -- Organize_Nodes --
   --------------------

   procedure Organize_Nodes
     (G    : Graph;
      Info : in out Layout_Info)
   is
      Nodes : constant Depth_Vertices_Array := Depth_First_Search (G);
      V     : Vertex_Access;
   begin
      Info.In_Layers := new Layer_Info_Array
        (Info.Min_Layer .. Info.Max_Layer);

      for N in Nodes'Range loop
         V := Nodes (N).Vertex;
         Info.In_Layers (Layer (Info, V)).Append (V);
      end loop;

      Sort_Nodes_Within_Layers (G, Info);
      Adjust_Positions (G,  Info);
   end Organize_Nodes;

   -------------------
   -- Feasible_Tree --
   -------------------

   procedure Feasible_Tree
     (G        : Graph;
      Info     : in out Layout_Info;
      Spanning : out Tree)
   is
      function Add_Edge_And_Recurse
        (E : Edge_Access; V : Vertex_Access) return Boolean;
      function Search (V : Vertex_Access) return Boolean;
      --  These functions return True if the tree is complete at this
      --  point, and therefore we should stop searching.

      procedure Add_Adjacent_Edge;
      --  Add one adjacent edge to the tree, and change vertex layers to
      --  tighten that edge

      --------------------------
      -- Add_Edge_And_Recurse --
      --------------------------

      function Add_Edge_And_Recurse
        (E : Edge_Access; V : Vertex_Access) return Boolean
      is
      begin
         if not In_Tree (Spanning, V) and then Slack (Info, E) = 0 then
            Add_Edge (Spanning, E);
            if Is_Spanning (Spanning) or else Search (V) then
               return True;
            end if;
         end if;
         return False;
      end Add_Edge_And_Recurse;

      ------------
      -- Search --
      ------------

      function Search (V : Vertex_Access) return Boolean is
         Eit : Edge_Iterator;
         E   : Edge_Access;
      begin
         Eit := First (G, Src => V);
         while not At_End (Eit) loop
            E := Get (Eit);
            if Add_Edge_And_Recurse (E, Get_Dest (E)) then
               return True;
            end if;
            Next (Eit);
         end loop;

         Eit := First (G, Dest => V);
         while not At_End (Eit) loop
            E := Get (Eit);
            if Add_Edge_And_Recurse (E, Get_Src (E)) then
               return True;
            end if;
            Next (Eit);
         end loop;

         --  We force the edge into the tree (it might have been an edge
         --  with no in or out edges).
         Add_Vertex (Spanning, V);
         return Is_Spanning (Spanning);
      end Search;

      -----------------------
      -- Add_Adjacent_Edge --
      -----------------------

      procedure Add_Adjacent_Edge is
         Vit : Vertex_Iterator := First (G);
         V   : Vertex_Access;
         Eit : Edge_Iterator;
         E   : Edge_Access;

         Last_Vertex_Not_In_Tree : Vertex_Access;

         Layer_Delta   : Integer;
         Min_Slack     : Integer := Integer'Last;
         Vertex_To_Add : Vertex_Access;
         Edge_To_Add   : Edge_Access;
         Sl            : Integer;

         Dummy : Boolean;
         pragma Unreferenced (Dummy);

      begin
         For_Each_Vertex_Not_In_Tree :
         while not At_End (Vit) loop
            V := Get (Vit);
            if not In_Tree (Spanning, V) then
               Last_Vertex_Not_In_Tree := V;

               Eit := First (G, Src => V);
               while not At_End (Eit) loop
                  E := Get (Eit);
                  if In_Tree (Spanning, Get_Dest (E)) then
                     Sl := Slack (Info, E);
                     if Sl < Min_Slack then
                        Min_Slack := Sl;
                        Vertex_To_Add := V;
                        Edge_To_Add := E;
                        Layer_Delta := -Sl;

                        --  that will be the minimum anyway
                        exit For_Each_Vertex_Not_In_Tree when Sl = 1;
                     end if;
                  end if;
                  Next (Eit);
               end loop;

               Eit := First (G, Dest => V);
               while not At_End (Eit) loop
                  E := Get (Eit);
                  if In_Tree (Spanning, Get_Src (E)) then
                     Sl := Slack (Info, E);
                     if Sl < Min_Slack then
                        Min_Slack := Sl;
                        Vertex_To_Add := V;
                        Edge_To_Add := E;
                        Layer_Delta := Sl;

                        --  that will be the minimum anyway
                        exit For_Each_Vertex_Not_In_Tree when Sl = 1;
                     end if;
                  end if;
                  Next (Eit);
               end loop;
            end if;

            Next (Vit);
         end loop For_Each_Vertex_Not_In_Tree;

         --  Have we found an edge to tighten ?

         if Vertex_To_Add /= null then
            Vit := First (G);
            while not At_End (Vit) loop
               V := Get (Vit);

               --  If the node is in the current component
               if Spanning.Node_In_Tree (Get_Index (V)) =
                 Spanning.Disjoint_Components
               then
                  Info.Layers (Get_Index (V)) :=
                    Info.Layers (Get_Index (V)) + Layer_Delta;
               end if;

               Next (Vit);
            end loop;

            --  Add the edge only after we had adjusted layers
            Add_Edge (Spanning, Edge_To_Add);

            Info.Min_Layer :=
              Integer'Min (Info.Min_Layer, Info.Min_Layer + Layer_Delta);
            Info.Max_Layer :=
              Integer'Max (Info.Max_Layer, Info.Max_Layer + Layer_Delta);

         elsif Last_Vertex_Not_In_Tree /= null then
            --  No adjacent vertex, and yet the tree is not spanning. We
            --  start from a new node.

            Spanning.Disjoint_Components :=
              Spanning.Disjoint_Components + 1;
            Dummy := Search (Last_Vertex_Not_In_Tree);
         end if;
      end Add_Adjacent_Edge;

      Vit   : constant Vertex_Iterator := First (G);
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
   begin
      if At_End (Vit) then
         --  No nodes in graph
         return;
      end if;

      Spanning.Disjoint_Components := 1;

      Dummy := Search (Get (Vit));  --  initial tree (non-spanning)

      while not Is_Spanning (Spanning) loop
         Add_Adjacent_Edge;
      end loop;
   end Feasible_Tree;

   ----------------
   -- Rank_Items --
   ----------------

   procedure Rank_Items (G : in out Graph; Info : in out Layout_Info) is
      Max_I     : constant Integer := Max_Index (G);

      Spanning  : Tree (Max_I);

   begin
      Init_Rank (G, Info);

      Feasible_Tree (G, Info, Spanning);

      --  ??? Should now compute cut values, and adjust layers for edges
      --  with negative cut values. The idea is that a node with for
      --  instance more incoming edges than outgoing edges, should
      --  preferably shorten the incoming edges

      Normalize_Layers (Spanning, Info);

      --  ??? Could balance the layers: when a node can be in multiple
      --  layers (same number of incomding and outgoing edges), it should be
      --  moved to the layer which has the fewest nodes to reduce crowding.

   end Rank_Items;

   ---------------------
   -- Layered_Layouts --
   ---------------------

   package body Layered_Layouts is

      procedure Insert_Dummy_Nodes
        (G : in out Graph; Info : in out Layout_Info);
      --  When an edge spans multiple layers, replace it with a chain of
      --  edges, each of which only connects adjacent layers

      ------------------------
      -- Insert_Dummy_Nodes --
      ------------------------

      procedure Insert_Dummy_Nodes
        (G : in out Graph; Info : in out Layout_Info)
      is
         Eit : Edge_Iterator := First (G);
         E   : Edge_Access;
         V1  : Vertex_Access;
         Start_Layer, End_Layer : Integer;
      begin
         while not At_End (Eit) loop
            E := Get (Eit);
            Next (Eit);

            Start_Layer := Info.Layers (Get_Index (Get_Src (E)));
            End_Layer   := Info.Layers (Get_Index (Get_Dest (E)));

            if Start_Layer < End_Layer - 1 then
               declare
                  Dummies : Vertices_Array
                    (Start_Layer + 1 .. End_Layer - 1);
               begin
                  V1 := Get_Src (E);
                  for Layer in Start_Layer + 1 .. End_Layer - 1 loop
                     --  We can't add the new layer to Layers, since there
                     --  is not enough space there.

                     Dummies (Layer) := new Dummy_Vertex;
                     Base_Dummy_Vertex (Dummies (Layer).all).Layer := Layer;
                     Add_Vertex (G, Dummies (Layer));
                     Add_Edge (G, V1, Dummies (Layer));

                     V1 := Dummies (Layer);
                  end loop;
                  Add_Edge (G, V1, Get_Dest (E));

                  Replaced_With_Dummy_Vertices
                    (Replaced_Edge => E,
                     Dummies      => Dummies);

                  Remove (G, E);
               end;
            end if;
         end loop;
      end Insert_Dummy_Nodes;

      ------------
      -- Layout --
      ------------

      procedure Layout
        (G                    : in out Graph;
         Horizontal           : Boolean := True;
         Space_Between_Layers : Gdouble := 20.0;
         Space_Between_Items  : Gdouble := 10.0)
      is
         Info : Layout_Info;
      begin
         --  If the graph is empty, nothing to do
         if Max_Index (G) = -1 then
            return;
         end if;

         Info.Horizontal           := Horizontal;
         Info.Space_Between_Items  := Space_Between_Items;
         Info.Space_Between_Layers := Space_Between_Layers;

         Info.Layers :=
           new Integer_Array'(Min_Vertex_Index .. Max_Index (G) => 0);

         if not Is_Directed (G) then
            raise Program_Error
              with "Layer layout only applies to directed graphs";
         end if;

         Make_Acyclic (G);
         Rank_Items (G, Info);

         if Add_Dummy_Nodes then
            Insert_Dummy_Nodes (G, Info);
         end if;

         Organize_Nodes (G, Info);
         Free (Info);
      end Layout;

   end Layered_Layouts;

end Glib.Graphs.Layouts;
