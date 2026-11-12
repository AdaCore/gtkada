--  Exercise Gsk.Cairo_Node constructors, Cairo accessors, and runtime wrapping.
--  GTK's C testsuite has no focused cairo-node test to port.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Cairo;
with Cairo.Surface;
with Glib.Test;
with Graphene.Rect;
with Gsk.Cairo_Node;
with Gsk.Cross_Fade_Node;
with Gsk.Enums;
with Gsk.Render_Node;
with System;

procedure Main is
   use Glib.Test;
   use type Cairo.Cairo_Status;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type System.Address;

   --  Freeing a render-node wrapper releases its owned GSK reference.
   procedure Free_Render_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Constructors_And_Cairo_Accessors with Convention => C;
   procedure Test_Runtime_Wrapper with Convention => C;

   --  Both constructors use Initialize, and a later Initialize call is inert.
   procedure Test_Constructors_And_Cairo_Accessors is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 24.0, Height => 18.0));
      Other_Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 40.0, Y => 50.0), (Width => 60.0, Height => 70.0));
      Node : Gsk.Cairo_Node.Gsk_Cairo_Node :=
        Gsk.Cairo_Node.Gsk_Cairo_Node_New (Bounds'Access);
      Procedure_Node : Gsk.Cairo_Node.Gsk_Cairo_Node;
      Context : Cairo.Cairo_Context;
      Surface : Cairo.Cairo_Surface;
      Node_Address : constant System.Address :=
        Gsk.Render_Node.Get_Object (Node);
      Procedure_Address : System.Address;
   begin
      --  The function constructor produces a live Cairo render node.
      Assert_Nonnull (Node_Address);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Node)) =
             Gsk.Enums.Gsk_Cairo_Node);

      --  Get_Draw_Context returns an owned context suitable for drawing.
      Context := Gsk.Cairo_Node.Get_Draw_Context (Node);
      Assert_True (Cairo.Status (Context) = Cairo.Cairo_Status_Success);
      Cairo.Set_Source_Rgb (Context, 0.2, 0.4, 0.6);
      Cairo.Paint (Context);
      Assert_True (Cairo.Status (Context) = Cairo.Cairo_Status_Success);

      --  Get_Surface exposes the valid Cairo surface backing this node.
      Surface := Gsk.Cairo_Node.Get_Surface (Node);
      Assert_True
        (Cairo.Surface.Status (Surface) = Cairo.Cairo_Status_Success);
      Cairo.Destroy (Context);

      --  Gtk_New builds another node; Initialize preserves its original node.
      Gsk.Cairo_Node.Gtk_New (Procedure_Node, Bounds'Access);
      Procedure_Address := Gsk.Render_Node.Get_Object (Procedure_Node);
      Assert_Nonnull (Procedure_Address);
      Gsk.Cairo_Node.Initialize (Procedure_Node, Other_Bounds'Access);
      Assert_True
        (Gsk.Render_Node.Get_Object (Procedure_Node) =
           Procedure_Address);

      --  This attempt to call Initialize on the function-created node also
      --  checks that the constructor's existing C object is retained.
      Gsk.Cairo_Node.Initialize (Node, Other_Bounds'Access);
      Assert_True (Gsk.Render_Node.Get_Object (Node) = Node_Address);

      Free_Render_Node (Gsk.Render_Node.Gsk_Render_Node (Procedure_Node));
      Free_Render_Node (Gsk.Render_Node.Gsk_Render_Node (Node));
   end Test_Constructors_And_Cairo_Accessors;

   --  A child getter recreates the concrete Ada wrapper through Create.
   procedure Test_Runtime_Wrapper is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 12.0, Height => 9.0));
      Node : Gsk.Cairo_Node.Gsk_Cairo_Node :=
        Gsk.Cairo_Node.Gsk_Cairo_Node_New (Bounds'Access);
      Node_Render : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Node);
      Fade : Gsk.Cross_Fade_Node.Gsk_Cross_Fade_Node :=
        Gsk.Cross_Fade_Node.Gsk_Cross_Fade_Node_New
          (Node_Render, Node_Render, 0.5);
      Wrapped : Gsk.Render_Node.Gsk_Render_Node;
      Wrapped_Cairo : Gsk.Cairo_Node.Gsk_Cairo_Node;
      Context : Cairo.Cairo_Context;
   begin
      --  The cross-fade returns an owning generic wrapper for its child.
      Wrapped := Gsk.Cross_Fade_Node.Get_Start_Child (Fade);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Wrapped));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Wrapped) = Gsk.Enums.Gsk_Cairo_Node);

      --  The runtime tag lets Ada safely view the child as Gsk_Cairo_Node.
      Wrapped_Cairo := Gsk.Cairo_Node.Gsk_Cairo_Node (Wrapped);
      Context := Gsk.Cairo_Node.Get_Draw_Context (Wrapped_Cairo);
      Assert_True (Cairo.Status (Context) = Cairo.Cairo_Status_Success);
      Cairo.Destroy (Context);

      Free_Render_Node (Wrapped);
      Free_Render_Node (Gsk.Render_Node.Gsk_Render_Node (Fade));
      Free_Render_Node (Node_Render);
   end Test_Runtime_Wrapper;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gsk/cairo-node/constructors-accessors",
      Test_Constructors_And_Cairo_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/cairo-node/runtime-wrapper",
      Test_Runtime_Wrapper'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
