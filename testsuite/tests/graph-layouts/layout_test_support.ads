--  Shared vertex types and callbacks for the graph layout tests.
--  The package instances are library-level, as required by the API.
with Glib; use Glib;
with Glib.Graphs;
with Glib.Graphs.Layouts;

package Layout_Test_Support is
   use Glib.Graphs;

   --  Keep each test node's dimensions and final position for assertions.
   type Test_Vertex is new Vertex with record
      Width, Height : Gdouble := 0.0;
      X, Y          : Gdouble := 0.0;
   end record;

   procedure Get_Size (V : Vertex_Access; Width, Height : out Gdouble);
   procedure Set_Position (V : Vertex_Access; X, Y : Gdouble);

   --  A custom dummy records the position assigned by the layout callback.
   package Layout_Instance is new Glib.Graphs.Layouts
     (Get_Size, Set_Position);

   type Custom_Dummy is new Layout_Instance.Base_Dummy_Vertex with record
      X, Y : Gdouble := 0.0;
   end record;

   procedure Record_Replacement
     (Replaced_Edge : Edge_Access; Dummies : Vertices_Array);

   Replacement_Count : Natural := 0;
   Dummy_Count       : Natural := 0;

   --  Exercise both custom dummy callbacks and the standard dummy type.
   package Custom_Layouts is new Layout_Instance.Layered_Layouts
     (Dummy_Vertex => Custom_Dummy,
      Replaced_With_Dummy_Vertices => Record_Replacement);
   package Base_Layouts is new Layout_Instance.Layered_Layouts
     (Dummy_Vertex => Layout_Instance.Base_Dummy_Vertex);
end Layout_Test_Support;
