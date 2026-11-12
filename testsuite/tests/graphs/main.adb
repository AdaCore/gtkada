--  Exercise the public Glib.Graphs operations: graph mutation, filtered edge
--  iterators, breadth/depth searches, strongly connected components, and
--  Kruskal's spanning tree. This package has no corresponding C graph tests.
with Ada.Command_Line;
with Glib;       use Glib;
with Glib.Test;  use Glib.Test;
with Glib.Graphs; use Glib.Graphs;
with Graph_Test_Support; use Graph_Test_Support;

procedure Main is

   --  Find a BFS result by vertex so checks do not depend on sibling order.
   function Find (Items : Breadth_Vertices_Array; V : Vertex_Access)
      return Breadth_Record
   is
   begin
      for Item of Items loop
         if Item.Vertex = V then
            return Item;
         end if;
      end loop;
      raise Program_Error with "vertex not found in breadth-first result";
   end Find;

   --  Reverse_Edge_Cb must actually reverse the edge for DFS to finish with
   --  an acyclic graph; the callback also records that it was used.
   Reversed_Edges : Natural := 0;
   procedure Reverse_Callback (G : Graph; E : Edge_Access);
   procedure Reverse_Callback (G : Graph; E : Edge_Access) is
   begin
      Reversed_Edges := Reversed_Edges + 1;
      Revert_Edge (G, E);
   end Reverse_Callback;

   procedure Test_Modification_And_Iterators with Convention => C;
   procedure Test_Searches with Convention => C;
   procedure Test_Components_And_Kruskal with Convention => C;

   --  Duplicate endpoints stay adjacent, directed filters select matching
   --  ends, and undirected filters include reverse edges without repeating a
   --  self-loop. This also exercises vertex reordering and both removal paths.
   procedure Test_Modification_And_Iterators is
      G : Graph;
      A : constant Vertex_Access := New_Vertex;
      B : constant Vertex_Access := New_Vertex;
      C : constant Vertex_Access := New_Vertex;
      D : constant Vertex_Access := New_Vertex;
      Supplied : constant Edge_Access := New_Edge;
      It : Edge_Iterator;
      V_It : Vertex_Iterator;
      E : Edge_Access;
      Count, AB_Count : Natural;
   begin
      --  Indices are assigned monotonically, while iteration starts at the
      --  most recently added vertex.
      Assert_Cmpint_Eq (Gint (Max_Index (G)), -1);
      Assert_True (not Is_Directed (G));
      Add_Vertex (G, A);
      Add_Vertex (G, B);
      Add_Vertex (G, C);
      Add_Vertex (G, D);
      Assert_Cmpuint_Eq (Guint (Get_Index (A)), 0);
      Assert_Cmpuint_Eq (Guint (Get_Index (D)), 3);
      Assert_Cmpint_Eq (Gint (Max_Index (G)), 3);

      --  Move operations keep all vertices and only change their iteration
      --  order. Calls for an absent vertex and a one-vertex graph are no-ops.
      V_It := First (G);
      Assert_True (Get (V_It) = D);
      Move_To_Front (G, B);
      V_It := First (G);
      Assert_True (Get (V_It) = B);
      Move_To_Front (G, B);
      Move_To_Back (G, B);
      V_It := First (G);
      Assert_True (Get (V_It) = D);
      Move_To_Back (G, D);
      V_It := First (G);
      Assert_True (Get (V_It) = C);
      Move_To_Back (G, A);
      Move_To_Front (G, A);
      V_It := First (G);
      Assert_True (Get (V_It) = A);
      Move_To_Front (G, C);
      V_It := First (G);
      Assert_True (Get (V_It) = C);
      Move_To_Front (G, null);
      Move_To_Back (G, null);

      --  Add both automatic and caller-supplied edges, including parallel and
      --  reverse links. The custom edge verifies dispatch on explicit removal.
      Set_Directed (G, True);
      Assert_True (Is_Directed (G));
      Add_Edge (G, Supplied, A, B);
      Add_Edge (G, A, B);
      Add_Edge (G, C, B);
      Add_Edge (G, B, A);
      Add_Edge (G, A, C);
      Add_Edge (G, C, A);
      Add_Edge (G, C, C);
      Add_Edge (G, B, B);
      Assert_Cmpuint_Eq (Guint (Out_Degree (G, A)), 3);
      Assert_Cmpuint_Eq (Guint (In_Degree (G, B)), 4);
      Assert_Cmpuint_Eq (Guint (Out_Degree (G, C)), 3);
      Assert_Cmpuint_Eq (Guint (In_Degree (G, C)), 2);
      Assert_True (Get_Src (Supplied) = A);
      Assert_True (Get_Dest (Supplied) = B);

      --  Identical edges report their one-based position within the group.
      It := First (G);
      Count := 0;
      AB_Count := 0;
      while not At_End (It) loop
         E := Get (It);
         Count := Count + 1;
         if Get_Src (E) = A and then Get_Dest (E) = B then
            AB_Count := AB_Count + 1;
            Assert_Cmpuint_Eq (Guint (Repeat_Count (It)),
                               Guint (AB_Count));
         end if;
         Next (It);
      end loop;
      Assert_Cmpuint_Eq (Guint (Count), 8);
      Assert_Cmpuint_Eq (Guint (AB_Count), 2);

      --  An exact directed query finds the parallel group. The undirected
      --  override also finds B-to-A; Src-only traversal includes incoming
      --  edges, while Dest-only traversal includes outgoing edges.
      It := First (G, Src => A, Dest => B);
      Count := 0;
      while not At_End (It) loop
         Assert_True (Get_Src (Get (It)) = A);
         Assert_True (Get_Dest (Get (It)) = B);
         Count := Count + 1;
         Next (It);
      end loop;
      Assert_Cmpuint_Eq (Guint (Count), 2);

      Set_Directed (G, False);
      It := First (G, Src => A, Dest => B);
      Count := 0;
      while not At_End (It) loop
         Count := Count + 1;
         Next (It);
      end loop;
      Assert_Cmpuint_Eq (Guint (Count), 3);
      It := First (G, Src => C, Directed => False);
      Count := 0;
      while not At_End (It) loop
         Count := Count + 1;
         Next (It);
      end loop;
      Assert_Cmpuint_Eq (Guint (Count), 4);
      It := First (G, Dest => B, Directed => False);
      Count := 0;
      while not At_End (It) loop
         Count := Count + 1;
         Next (It);
      end loop;
      Assert_Cmpuint_Eq (Guint (Count), 5);

      --  Removing the custom edge updates both adjacency lists. Removing A
      --  then removes its incoming and outgoing edges and unlinks a middle
      --  vertex node from the graph's vertex list.
      Remove (G, Supplied);
      Assert_Cmpuint_Eq (Guint (Edge_Destroyed), 1);
      Assert_Cmpuint_Eq (Guint (Out_Degree (G, A)), 2);
      Remove (G, A);
      Assert_Cmpuint_Eq (Guint (Vertex_Destroyed), 1);
      Assert_Cmpuint_Eq (Guint (In_Degree (G, B)), 2);

      --  Clear is exercised through Destroy; it frees all remaining vertices
      --  and their edges, leaving the public iterators at their end sentinels.
      Destroy (G);
      V_It := First (G);
      It := First (G, Directed => False);
      Assert_True (At_End (V_It));
      Assert_True (At_End (It));
      Assert_Cmpuint_Eq (Guint (Vertex_Destroyed), 4);
      Assert_Cmpuint_Eq (Guint (Edge_Destroyed), 1);

      --  Null iterators and the empty/singleton reorder paths are valid no-ops.
      Assert_True (At_End (Null_Vertex_Iterator));
      Assert_True (At_End (Null_Edge_Iterator));
      Move_To_Front (G, null);
      Move_To_Back (G, null);
      declare
         H : Graph;
         Lone : constant Vertex_Access := New_Vertex;
      begin
         Add_Vertex (H, Lone);
         Move_To_Front (H, Lone);
         Move_To_Back (H, Lone);
         Destroy (H);
      end;
      Assert_Cmpuint_Eq (Guint (Vertex_Destroyed), 5);
   end Test_Modification_And_Iterators;

   --  BFS returns shortest distances and predecessors. DFS marks a back edge,
   --  can reverse it through its callback, and can search a directed graph as
   --  undirected without changing its edges.
   procedure Test_Searches is
      G : Graph;
      A : constant Vertex_Access := New_Vertex;
      B : constant Vertex_Access := New_Vertex;
      C : constant Vertex_Access := New_Vertex;
      D : constant Vertex_Access := New_Vertex;
      E : constant Vertex_Access := New_Vertex;
      F : constant Vertex_Access := New_Vertex;
      Acyclic : aliased Boolean;
   begin
      Set_Directed (G, True);
      Add_Vertex (G, A);
      Add_Vertex (G, B);
      Add_Vertex (G, C);
      Add_Vertex (G, D);
      Add_Vertex (G, E);
      Add_Vertex (G, F);
      Add_Edge (G, A, B);
      Add_Edge (G, A, C);
      Add_Edge (G, B, D);
      Add_Edge (G, C, D);
      Add_Edge (G, D, B);  --  A directed cycle in the reachable component.

      declare
         Breadth : constant Breadth_Vertices_Array :=
           Breadth_First_Search (G, A);
      begin
         Assert_Cmpuint_Eq (Guint (Breadth'Length), 4);
         Assert_Cmpuint_Eq (Guint (Find (Breadth, A).Distance), 0);
         Assert_True (Find (Breadth, A).Predecessor = null);
         Assert_Cmpuint_Eq (Guint (Find (Breadth, B).Distance), 1);
         Assert_True (Find (Breadth, B).Predecessor = A);
         Assert_Cmpuint_Eq (Guint (Find (Breadth, C).Distance), 1);
         Assert_True (Find (Breadth, C).Predecessor = A);
         Assert_Cmpuint_Eq (Guint (Find (Breadth, D).Distance), 2);
         Assert_True (Find (Breadth, D).Predecessor = B
                      or else Find (Breadth, D).Predecessor = C);
      end;

      --  The optional status reports a cycle, and Is_Acyclic exits as soon as
      --  the depth-first traversal finds the same gray back edge.
      declare
         Depth : constant Depth_Vertices_Array :=
           Depth_First_Search (G, Acyclic'Access);
      begin
         Assert_True (not Acyclic);
         Assert_Cmpuint_Eq (Guint (Depth'Length), 6);
      end;
      Assert_True (not Is_Acyclic (G));

      --  Reverse_Edge_Callback uses the iterator-safe Revert_Edge helper. A
      --  subsequent plain DFS exercises the convenience overload as well.
      Reversed_Edges := 0;
      declare
         Depth : constant Depth_Vertices_Array := Depth_First_Search
           (G, Acyclic'Access,
            Reverse_Edge_Cb => Reverse_Callback'Unrestricted_Access);
      begin
         Assert_True (Acyclic);
         Assert_Cmpuint_Eq (Guint (Depth'Length), 6);
      end;
      Assert_Cmpuint_Gt (Guint (Reversed_Edges), 0);
      Assert_True (Is_Acyclic (G));
      declare
         Depth : constant Depth_Vertices_Array := Depth_First_Search (G);
      begin
         Assert_Cmpuint_Eq (Guint (Depth'Length), 6);
      end;

      --  A root in a disconnected component yields just that vertex; forcing
      --  undirected traversal then reaches all vertices in the weak component.
      declare
         Breadth : constant Breadth_Vertices_Array :=
           Breadth_First_Search (G, E);
         Depth : constant Depth_Vertices_Array := Depth_First_Search
           (G, Acyclic'Access, Force_Undirected => True);
         Edge_It : Edge_Iterator := First (G, Src => D);
      begin
         Assert_Cmpuint_Eq (Guint (Breadth'Length), 1);
         Assert_True (Acyclic);
         Assert_Cmpuint_Eq (Guint (Depth'Length), 6);
         Assert_True (not At_End (Edge_It));
         Assert_True (Get_Src (Get (Edge_It)) = D);
      end;
      Destroy (G);

      --  A self-loop on a discovered non-root vertex exercises BFS's
      --  source-equals-destination path without re-enqueuing its white root.
      declare
         H : Graph;
         Root : constant Vertex_Access := New_Vertex;
         Lone : constant Vertex_Access := New_Vertex;
      begin
         Set_Directed (H, True);
         Add_Vertex (H, Root);
         Add_Vertex (H, Lone);
         Add_Edge (H, Root, Lone);
         Add_Edge (H, Lone, Lone);
         declare
            Breadth : constant Breadth_Vertices_Array :=
              Breadth_First_Search (H, Root);
         begin
            Assert_Cmpuint_Eq (Guint (Breadth'Length), 2);
         end;
         Assert_True (not Is_Acyclic (H));
         Destroy (H);
      end;
   end Test_Searches;

   --  Kosaraju's algorithm groups mutually reachable vertices, both with a
   --  freshly computed DFS and with a caller-supplied DFS. Kruskal returns a
   --  spanning tree while discarding cycle and self-loop edges.
   procedure Test_Components_And_Kruskal is
      G : Graph;
      A : constant Vertex_Access := New_Vertex;
      B : constant Vertex_Access := New_Vertex;
      C : constant Vertex_Access := New_Vertex;
      D : constant Vertex_Access := New_Vertex;
      E : constant Vertex_Access := New_Vertex;
      Components : Connected_Component_List;
      It : Connected_Component_List;
      Acyclic : aliased Boolean;
      Seen_AB, Seen_CD, Seen_E : Boolean;
      Sizes : array (1 .. 3) of Natural;
      Group : Natural;
   begin
      Set_Directed (G, True);
      Add_Vertex (G, A);
      Add_Vertex (G, B);
      Add_Vertex (G, C);
      Add_Vertex (G, D);
      Add_Vertex (G, E);
      Add_Edge (G, A, B);
      Add_Edge (G, B, A);
      Add_Edge (G, B, C);
      Add_Edge (G, C, D);
      Add_Edge (G, D, C);
      Add_Edge (G, E, E);

      Components := Strongly_Connected_Components (G);
      It := Components;
      Group := 0;
      Seen_AB := False;
      Seen_CD := False;
      Seen_E := False;
      while It /= null loop
         Group := Group + 1;
         Sizes (Group) := It.Num_Vertices;
         for I in It.Vertices'Range loop
            declare
               V : constant Vertex_Access := It.Vertices (I);
            begin
            if V = A or else V = B then
               Seen_AB := True;
            elsif V = C or else V = D then
               Seen_CD := True;
            elsif V = E then
               Seen_E := True;
            end if;
            end;
         end loop;
         --  Each component must contain exactly one expected group; a
         --  membership check below distinguishes the two pairs and singleton.
         if It.Num_Vertices = 2 then
            Assert_True
              (((It.Vertices (1) = A or else It.Vertices (1) = B)
                and then (It.Vertices (2) = A or else It.Vertices (2) = B))
               or else
               ((It.Vertices (1) = C or else It.Vertices (1) = D)
                and then (It.Vertices (2) = C or else It.Vertices (2) = D)));
         else
            Assert_Cmpuint_Eq (Guint (It.Num_Vertices), 1);
            Assert_True (It.Vertices (1) = E);
         end if;
         It := It.Next;
      end loop;
      Assert_Cmpuint_Eq (Guint (Group), 3);
      Assert_True (Seen_AB and Seen_CD and Seen_E);
      Assert_True ((Sizes (1) = 1 or else Sizes (2) = 1 or else Sizes (3) = 1));
      Free (Components);
      Assert_True (Components = null);

      --  The second overload reuses a supplied DFS. It must return the same
      --  component sizes, in an order that is intentionally not prescribed.
      declare
         DFS : constant Depth_Vertices_Array :=
           Depth_First_Search (G, Acyclic'Access);
      begin
         Components := Strongly_Connected_Components (G, DFS);
      end;
      It := Components;
      Group := 0;
      while It /= null loop
         Group := Group + 1;
         Assert_True (It.Num_Vertices = 1 or else It.Num_Vertices = 2);
         It := It.Next;
      end loop;
      Assert_Cmpuint_Eq (Guint (Group), 3);
      Free (Components);
      Destroy (G);

      --  The graph is stored with arbitrary edge directions because Kruskal
      --  treats the edge set as undirected and keeps a spanning forest.
      declare
         H : Graph;
         P : constant Vertex_Access := New_Vertex;
         Q : constant Vertex_Access := New_Vertex;
         R : constant Vertex_Access := New_Vertex;
         S : constant Vertex_Access := New_Vertex;
      begin
         Add_Vertex (H, P);
         Add_Vertex (H, Q);
         Add_Vertex (H, R);
         Add_Vertex (H, S);
         Add_Edge (H, P, Q);
         Add_Edge (H, Q, R);
         Add_Edge (H, R, P);  --  Cycle edge is skipped after set merging.
         Add_Edge (H, R, S);
         Add_Edge (H, S, S);  --  Self-loop is also skipped.
         declare
            Tree : constant Edges_Array := Kruskal (H);
         begin
            Assert_Cmpuint_Eq (Guint (Tree'Length), 3);
            for I in Tree'Range loop
               Assert_True (Tree (I) /= null);
               Assert_True (Get_Src (Tree (I)) /= null);
               Assert_True (Get_Dest (Tree (I)) /= null);
            end loop;
         end;
         Destroy (H);
      end;
   end Test_Components_And_Kruskal;

begin
   Glib.Test.Init;
   Add_Func ("/graphs/modification-and-iterators",
             Test_Modification_And_Iterators'Unrestricted_Access);
   Add_Func ("/graphs/searches", Test_Searches'Unrestricted_Access);
   Add_Func ("/graphs/components-and-kruskal",
             Test_Components_And_Kruskal'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
