with Glib;                use Glib;
with Glib.Graphs;         use Glib.Graphs;
with Test_Graph_Support;  use Test_Graph_Support;

procedure Test_Graph is
   Gr : Graph;
   A, B, C, D, E, F, G, H, I, J, K : Vertex_Access;
begin
   Set_Directed (Gr, True);

   A := new Vertex; Add_Vertex (Gr, A);
   B := new Vertex; Add_Vertex (Gr, B);
   C := new Vertex; Add_Vertex (Gr, C);
   D := new Vertex; Add_Vertex (Gr, D);
   E := new Vertex; Add_Vertex (Gr, E);
   F := new Vertex; Add_Vertex (Gr, F);
   G := new Vertex; Add_Vertex (Gr, G);
   H := new Vertex; Add_Vertex (Gr, H);
   I := new Vertex; Add_Vertex (Gr, I);
   Add_Edge (Gr, A, E);
   Add_Edge (Gr, A, F);
   Add_Edge (Gr, A, B);
   Add_Edge (Gr, E, G);
   Add_Edge (Gr, F, G);
   Add_Edge (Gr, B, C);
   Add_Edge (Gr, C, D);
   Add_Edge (Gr, D, H);
   Add_Edge (Gr, G, H);
   Add_Edge (Gr, I, H);

   --  A second unrelated set of nodes
   J := new Vertex; Add_Vertex (Gr, J);
   K := new Vertex; Add_Vertex (Gr, K);
   Add_Edge (Gr, J, K);

   Layout.Layer_Layout (Gr, Horizontal => False);
end Test_Graph;
