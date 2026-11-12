--  Implement the dimension and position callbacks used by graph layouts.
package body Layout_Test_Support is

   procedure Get_Size (V : Vertex_Access; Width, Height : out Gdouble) is
   begin
      Width := Test_Vertex (V.all).Width;
      Height := Test_Vertex (V.all).Height;
   end Get_Size;

   --  The custom dummy subtype is also sent to Set_Position by the API.
   procedure Set_Position (V : Vertex_Access; X, Y : Gdouble) is
   begin
      if V.all in Test_Vertex'Class then
         Test_Vertex (V.all).X := X;
         Test_Vertex (V.all).Y := Y;
      else
         Custom_Dummy (V.all).X := X;
         Custom_Dummy (V.all).Y := Y;
      end if;
   end Set_Position;

   --  Record that a long edge was replaced and how many dummies it needed.
   procedure Record_Replacement
     (Replaced_Edge : Edge_Access; Dummies : Vertices_Array) is
   begin
      if Replaced_Edge /= null then
         Replacement_Count := Replacement_Count + 1;
         Dummy_Count := Dummy_Count + Dummies'Length;
      end if;
   end Record_Replacement;
end Layout_Test_Support;
