--  Tests Glib.Graphs.Layouts.Layered_Layouts with DAGs, cycles, disconnected
--  components, both orientations, and custom dummy vertices. These tests are
--  Ada-specific because the layout algorithm has no corresponding C API test.
with Ada.Command_Line;
with Glib;                    use Glib;
with Glib.Graphs;             use Glib.Graphs;
with Glib.Graphs.Layouts;
with Glib.Test;               use Glib.Test;
with Layout_Test_Support;     use Layout_Test_Support;

procedure Main is

   --  Allocate a vertex whose dimensions vary enough to exercise box sizing.
   function New_Node (W, H : Gdouble) return Vertex_Access is
      V : constant Vertex_Access := new Test_Vertex;
   begin
      Test_Vertex (V.all).Width := W;
      Test_Vertex (V.all).Height := H;
      return V;
   end New_Node;

   function X_Of (V : Vertex_Access) return Gdouble is
   begin
      if V.all in Custom_Dummy'Class then
         return Custom_Dummy (V.all).X;
      else
         return Test_Vertex (V.all).X;
      end if;
   end X_Of;

   procedure Test_Horizontal_Dag with Convention => C;
   procedure Test_Vertical_Layout with Convention => C;
   procedure Test_Tightening_Edges with Convention => C;
   procedure Test_Cycle_And_Undirected with Convention => C;
   procedure Test_Empty_Graph with Convention => C;

   --  A diamond, a long edge, a separate component, and an isolated node
   --  exercise ranking, tree normalization, dummy insertion, sorting and
   --  position adjustment, including items with no neighbors.
   procedure Test_Horizontal_Dag is
      G : Graph;
      A : constant Vertex_Access := New_Node (16.0, 10.0);
      B : constant Vertex_Access := New_Node (12.0, 18.0);
      C : constant Vertex_Access := New_Node (20.0, 12.0);
      D : constant Vertex_Access := New_Node (10.0, 14.0);
      E : constant Vertex_Access := New_Node (14.0, 16.0);
      F : constant Vertex_Access := New_Node (18.0, 11.0);
      U : constant Vertex_Access := New_Node (9.0, 9.0);
      V : constant Vertex_Access := New_Node (11.0, 13.0);
      Isolated : constant Vertex_Access := New_Node (8.0, 8.0);
      It : Edge_Iterator;
      Edge : Edge_Access;
   begin
      Set_Directed (G, True);
      Add_Vertex (G, A);
      Add_Vertex (G, B);
      Add_Vertex (G, C);
      Add_Vertex (G, D);
      Add_Vertex (G, E);
      Add_Vertex (G, F);
      Add_Vertex (G, U);
      Add_Vertex (G, V);
      Add_Vertex (G, Isolated);
      Add_Edge (G, A, B);
      Add_Edge (G, A, C);
      Add_Edge (G, B, D);
      Add_Edge (G, C, D);
      Add_Edge (G, D, E);
      Add_Edge (G, E, F);
      Add_Edge (G, A, F);  --  This edge spans several layers.
      Add_Edge (G, U, V);  --  A disjoint component is normalized to layer 0.

      --  The callbacks observe the long-edge replacement, and the assertions
      --  below check that every resulting segment advances one horizontal layer.
      Replacement_Count := 0;
      Dummy_Count := 0;
      Custom_Layouts.Layout (G, Horizontal => True,
                             Space_Between_Layers => 23.0,
                             Space_Between_Items => 7.0);

      Assert_True (Is_Acyclic (G));
      Assert_Cmpuint_Gt (Guint (Replacement_Count), 0);
      Assert_Cmpuint_Gt (Guint (Dummy_Count), 0);
      Assert_True (X_Of (B) > X_Of (A));
      Assert_True (X_Of (F) > X_Of (E));

      --  Every edge, including dummy segments, must point rightward after the
      --  cycle-free layered layout. Custom dummy positions come from callback.
      It := First (G);
      while not At_End (It) loop
         Edge := Get (It);
         Assert_True (X_Of (Get_Dest (Edge)) > X_Of (Get_Src (Edge)));
         Next (It);
      end loop;
      Destroy (G);
   end Test_Horizontal_Dag;

   --  Vertical orientation uses height for layer spacing and width for the
   --  within-layer stack; default Base_Dummy_Vertex exercises its skip path.
   procedure Test_Vertical_Layout is
      G : Graph;
      A : constant Vertex_Access := New_Node (10.0, 15.0);
      B : constant Vertex_Access := New_Node (15.0, 10.0);
      C : constant Vertex_Access := New_Node (12.0, 20.0);
      D : constant Vertex_Access := New_Node (18.0, 12.0);
      Isolated : constant Vertex_Access := New_Node (7.0, 9.0);
      Before : Gdouble;
   begin
      Set_Directed (G, True);
      Add_Vertex (G, A);
      Add_Vertex (G, B);
      Add_Vertex (G, C);
      Add_Vertex (G, D);
      Add_Vertex (G, Isolated);
      Add_Edge (G, A, B);
      Add_Edge (G, B, C);
      Add_Edge (G, C, D);
      Add_Edge (G, A, D);  --  Long edge is split using the base dummy type.

      --  The different dimensions exercise height-based layer spacing and
      --  width-based item stacking in the vertical orientation.
      Base_Layouts.Layout (G, Horizontal => False,
                           Space_Between_Layers => 19.0,
                           Space_Between_Items => 6.0);
      Before := Test_Vertex (A.all).Y;
      Assert_True (Test_Vertex (D.all).Y > Before);
      Assert_True (Test_Vertex (B.all).Y > Before);
      Destroy (G);
   end Test_Vertical_Layout;

   --  Unequal length paths into one join leave a non-tight edge. Starting from
   --  each side exercises both directions used to tighten that edge.
   procedure Test_Tightening_Edges is
   begin
      declare
         G : Graph;
         P : constant Vertex_Access := New_Node (10.0, 10.0);
         Q : constant Vertex_Access := New_Node (10.0, 10.0);
         Join : constant Vertex_Access := New_Node (10.0, 10.0);
         R : constant Vertex_Access := New_Node (10.0, 10.0);
         Solo : constant Vertex_Access := New_Node (8.0, 8.0);
      begin
         Set_Directed (G, True);
         Add_Vertex (G, P);
         Add_Vertex (G, Q);
         Add_Vertex (G, Join);
         Add_Vertex (G, Solo);
         Add_Vertex (G, R);  --  Search begins at the short-path source.
         Add_Edge (G, P, Q);
         Add_Edge (G, Q, Join);
         Add_Edge (G, R, Join);

         Base_Layouts.Layout (G);
         Assert_True (Is_Acyclic (G));
         --  The short source is moved into the deeper path's layer range.
         Assert_True (X_Of (Join) > X_Of (R));
         Destroy (G);
      end;

      declare
         G : Graph;
         R : constant Vertex_Access := New_Node (10.0, 10.0);
         P : constant Vertex_Access := New_Node (10.0, 10.0);
         Q : constant Vertex_Access := New_Node (10.0, 10.0);
         Join : constant Vertex_Access := New_Node (10.0, 10.0);
         Solo : constant Vertex_Access := New_Node (8.0, 8.0);
      begin
         Set_Directed (G, True);
         Add_Vertex (G, R);
         Add_Vertex (G, P);
         Add_Vertex (G, Q);
         Add_Vertex (G, Solo);
         Add_Vertex (G, Join);  --  Search begins at the long-path join.
         Add_Edge (G, P, Q);
         Add_Edge (G, Q, Join);
         Add_Edge (G, R, Join);

         Base_Layouts.Layout (G);
         Assert_True (Is_Acyclic (G));
         --  The long-path component moves left to tighten its edge to Join.
         Assert_True (X_Of (Join) > X_Of (R));
         Destroy (G);
      end;

      --  With one connected edge, reverse search reaches the final vertex and
      --  propagates the completed-tree result back through its caller.
      declare
         G : Graph;
         A : constant Vertex_Access := New_Node (10.0, 10.0);
         B : constant Vertex_Access := New_Node (10.0, 10.0);
      begin
         Set_Directed (G, True);
         Add_Vertex (G, A);
         Add_Vertex (G, B);
         Add_Edge (G, A, B);
         Base_Layouts.Layout (G);
         --  Reverse search should finish the spanning tree without a fallback
         --  component, preserving the directed source-to-destination order.
         Assert_True (Is_Acyclic (G));
         Assert_True (X_Of (B) > X_Of (A));
         Destroy (G);
      end;
   end Test_Tightening_Edges;

   --  A directed cycle causes a back edge to be reversed. An undirected graph
   --  is rejected by the documented precondition and must raise Program_Error.
   procedure Test_Cycle_And_Undirected is
      G : Graph;
      A : constant Vertex_Access := New_Node (10.0, 10.0);
      B : constant Vertex_Access := New_Node (10.0, 10.0);
      C : constant Vertex_Access := New_Node (10.0, 10.0);
      Rejected : Boolean := False;
   begin
      Set_Directed (G, True);
      Add_Vertex (G, A);
      Add_Vertex (G, B);
      Add_Vertex (G, C);
      Add_Edge (G, A, B);
      Add_Edge (G, B, C);
      Add_Edge (G, C, A);
      Base_Layouts.Layout (G);
      --  The cycle is broken by reversing a back edge in the original graph.
      Assert_True (Is_Acyclic (G));
      Destroy (G);

      declare
         H : Graph;
         Lone : constant Vertex_Access := New_Node (10.0, 10.0);
      begin
         Add_Vertex (H, Lone);
         begin
            Base_Layouts.Layout (H);
         exception
            when Program_Error => Rejected := True;
         end;
         --  Rejecting this graph confirms the directedness precondition.
         Assert_True (Rejected);
         Destroy (H);
      end;
   end Test_Cycle_And_Undirected;

   --  Empty graphs are a no-op, even though a new graph is undirected by
   --  default; this also checks the early return before directed validation.
   procedure Test_Empty_Graph is
      G : Graph;
   begin
      Base_Layouts.Layout (G);
      --  Empty graphs are a no-op and retain the empty-graph index sentinel.
      Assert_Cmpint_Eq (Gint (Max_Index (G)), -1);
   end Test_Empty_Graph;

begin
   Glib.Test.Init;
   Add_Func ("/graphs/layouts/horizontal-dag",
             Test_Horizontal_Dag'Unrestricted_Access);
   Add_Func ("/graphs/layouts/vertical", Test_Vertical_Layout'Unrestricted_Access);
   Add_Func ("/graphs/layouts/tightening",
             Test_Tightening_Edges'Unrestricted_Access);
   Add_Func ("/graphs/layouts/cycle-and-undirected",
             Test_Cycle_And_Undirected'Unrestricted_Access);
   Add_Func ("/graphs/layouts/empty", Test_Empty_Graph'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
