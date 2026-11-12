--  Exercise the GtkAda stroke-node constructors, getters, and wrapper hook.
--  There is no focused gsk_stroke_node test in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib.Test;
with Graphene.Rect;
with Gsk.Color_Node;
with Gsk.Enums;
with Gsk.Path;
with Gsk.Render_Node;
with Gsk.Stroke;
with Gsk.Stroke_Node;
with System;

procedure Main is
   use Glib.Test;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type System.Address;

   --  Freeing through the classwide access type runs the render-node
   --  finalizer and releases the reference owned by each Ada wrapper.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Constructors_And_Accessors with Convention => C;
   procedure Test_Nested_Child_Create with Convention => C;

   --  Check the child identity and the immutable path and stroke properties.
   procedure Check_Node
     (Node  : Gsk.Stroke_Node.Gsk_Stroke_Node;
      Child : Gsk.Render_Node.Gsk_Render_Node;
      Path  : Gsk.Path.Gsk_Path;
      Stroke : Gsk.Stroke.Gsk_Stroke)
   is
      Actual_Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Stroke_Node.Get_Child (Node);
   begin
      Assert_True
        (Gsk.Render_Node.Get_Object (Actual_Child) =
           Gsk.Render_Node.Get_Object (Child));
      --  The path is borrowed from the node and remains valid with it alive.
      Assert_True (Gsk.Stroke_Node.Get_Path (Node).Equal (Path));
      --  Compare the borrowed stroke before freeing the node that owns it.
      Assert_True (Gsk.Stroke.Equal (Gsk.Stroke_Node.Get_Stroke (Node), Stroke));
      Free_Node (Actual_Child);
   end Check_Node;

   --  Both constructor forms work, and repeated initialization is a no-op.
   procedure Test_Constructors_And_Accessors is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 32.0, Height => 24.0));
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.25, 0.5, 0.75, 1.0);
      Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node
          (Gsk.Color_Node.Gsk_Color_Node_New (Color'Access, Bounds'Access));
      Path : constant Gsk.Path.Gsk_Path :=
        Gsk.Path.Parse ("M 0 0 L 36 0 L 36 28 Z");
      Other_Path : constant Gsk.Path.Gsk_Path :=
        Gsk.Path.Parse ("M 4 4 L 20 4 L 20 16 Z");
      Stroke : constant Gsk.Stroke.Gsk_Stroke := Gsk.Stroke.Gsk_Stroke_New (3.0);
      Other_Stroke : constant Gsk.Stroke.Gsk_Stroke :=
        Gsk.Stroke.Gsk_Stroke_New (6.0);
      --  Keep both input paths and strokes alive until their nodes are freed.
      Node : Gsk.Stroke_Node.Gsk_Stroke_Node :=
        Gsk.Stroke_Node.Gsk_Stroke_Node_New (Child, Path, Stroke);
      Procedure_Node : Gsk.Stroke_Node.Gsk_Stroke_Node;
   begin
      Check_Node (Node, Child, Path, Stroke);

      --  The initialized object keeps its original values after another call.
      Gsk.Stroke_Node.Initialize (Node, Child, Other_Path, Other_Stroke);
      Check_Node (Node, Child, Path, Stroke);

      --  Gtk_New is the procedure-style constructor and stores its own inputs.
      Gsk.Stroke_Node.Gtk_New
        (Procedure_Node, Child, Other_Path, Other_Stroke);
      Check_Node (Procedure_Node, Child, Other_Path, Other_Stroke);

      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Procedure_Node));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Node));
      Free_Node (Child);
      Stroke.Free;
      Other_Stroke.Free;
      Path.Unref;
      Other_Path.Unref;
   end Test_Constructors_And_Accessors;

   --  Retrieving a stroke node as a generic child dispatches to its Create hook.
   procedure Test_Nested_Child_Create is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 48.0, Height => 48.0));
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.8, 0.3, 0.1, 0.9);
      Leaf : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node
          (Gsk.Color_Node.Gsk_Color_Node_New (Color'Access, Bounds'Access));
      Inner_Path : constant Gsk.Path.Gsk_Path :=
        Gsk.Path.Parse ("M 0 0 L 20 0 L 20 20 Z");
      Outer_Path : constant Gsk.Path.Gsk_Path :=
        Gsk.Path.Parse ("M 4 4 L 40 4 L 40 40 Z");
      Inner_Stroke : constant Gsk.Stroke.Gsk_Stroke :=
        Gsk.Stroke.Gsk_Stroke_New (2.0);
      Outer_Stroke : constant Gsk.Stroke.Gsk_Stroke :=
        Gsk.Stroke.Gsk_Stroke_New (5.0);
      Inner : Gsk.Stroke_Node.Gsk_Stroke_Node :=
        Gsk.Stroke_Node.Gsk_Stroke_Node_New (Leaf, Inner_Path, Inner_Stroke);
      Inner_Render : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Inner);
      --  Distinct inputs make the inner node recognizable through its getter.
      Outer : Gsk.Stroke_Node.Gsk_Stroke_Node;
      Wrapped_Child : Gsk.Render_Node.Gsk_Render_Node;
      Wrapped_Stroke : Gsk.Stroke_Node.Gsk_Stroke_Node;
   begin
      Gsk.Stroke_Node.Gtk_New
        (Outer, Inner_Render, Outer_Path, Outer_Stroke);
      --  The generic child wrapper takes a reference released below.
      Wrapped_Child := Gsk.Stroke_Node.Get_Child (Outer);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Wrapped_Child) =
           Gsk.Enums.Gsk_Stroke_Node);
      Assert_True
        (Gsk.Render_Node.Get_Object (Wrapped_Child) =
           Gsk.Render_Node.Get_Object (Inner_Render));

      --  Successful down-conversion confirms the concrete wrapper was created.
      Wrapped_Stroke := Gsk.Stroke_Node.Gsk_Stroke_Node (Wrapped_Child);
      Assert_True (Gsk.Stroke_Node.Get_Path (Wrapped_Stroke).Equal (Inner_Path));
      Assert_True
        (Gsk.Stroke.Equal
           (Gsk.Stroke_Node.Get_Stroke (Wrapped_Stroke), Inner_Stroke));

      Free_Node (Wrapped_Child);
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Outer));
      Free_Node (Inner_Render);
      Free_Node (Leaf);
      Inner_Stroke.Free;
      Outer_Stroke.Free;
      Inner_Path.Unref;
      Outer_Path.Unref;
   end Test_Nested_Child_Create;

begin
   Glib.Test.Init;
   Add_Func
     ("/gsk/stroke-node/constructors-accessors",
      Test_Constructors_And_Accessors'Unrestricted_Access);
   Add_Func
     ("/gsk/stroke-node/nested-child-create",
      Test_Nested_Child_Create'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Run);
end Main;
