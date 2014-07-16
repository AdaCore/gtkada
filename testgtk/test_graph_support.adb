package body Test_Graph_Support is

   procedure Get_Size (V : Vertex_Access; W, H : out Gdouble) is
      pragma Unreferenced (V);
   begin
      W := 20.0;
      H := 20.0;
   end Get_Size;

   procedure Set_Pos (V : Vertex_Access; X, Y : Gdouble) is
      pragma Unreferenced (V, X, Y);
   begin
      null;
   end Set_Pos;

end Test_Graph_Support;
