--  Implement graph-test objects with counters for their Destroy hooks.
package body Graph_Test_Support is

   --  Record dispatch to the custom vertex destruction operation.
   procedure Destroy (V : in out Test_Vertex) is
   begin
      Vertex_Destroyed := Vertex_Destroyed + 1;
   end Destroy;

   --  Record dispatch to the custom edge destruction operation.
   procedure Destroy (E : in out Test_Edge) is
   begin
      Edge_Destroyed := Edge_Destroyed + 1;
   end Destroy;

   --  Package-level types can be allocated through the graph's public access
   --  types without a deeper-accessibility mismatch.
   function New_Vertex return Vertex_Access is
   begin
      return new Test_Vertex;
   end New_Vertex;

   function New_Edge return Edge_Access is
   begin
      return new Test_Edge;
   end New_Edge;
end Graph_Test_Support;
