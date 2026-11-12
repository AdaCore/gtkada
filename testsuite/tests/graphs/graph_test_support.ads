--  Shared descendants let the graph test observe dispatching destruction.
--  Their constructors allocate objects at the same scope as the access types.
with Glib.Graphs;

package Graph_Test_Support is
   use Glib.Graphs;

   type Test_Vertex is new Vertex with null record;
   --  Its hook increments Vertex_Destroyed when Remove or Destroy frees it.
   overriding procedure Destroy (V : in out Test_Vertex);

   type Test_Edge is new Edge with null record;
   --  Its hook confirms that caller-supplied edges use dispatching Destroy.
   overriding procedure Destroy (E : in out Test_Edge);

   Vertex_Destroyed : Natural := 0;
   Edge_Destroyed   : Natural := 0;

   --  Allocate graph-owned objects for the test cases.
   function New_Vertex return Vertex_Access;
   function New_Edge return Edge_Access;
end Graph_Test_Support;
