with Glib;                use Glib;
with Glib.Graphs;         use Glib.Graphs;
with Glib.Graphs.Layouts;

package Test_Graph_Support is
   procedure Get_Size (V : Vertex_Access; W, H : out Gdouble);
   procedure Set_Pos (V : Vertex_Access; X, Y : Gdouble);
   package Layout is new Glib.Graphs.Layouts (Get_Size, Set_Pos);
end Test_Graph_Support;
