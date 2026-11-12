--  Exercise Gsk.Fill_Node constructors, getters, initialization, and wrapper
--  creation. No focused GSK fill-node case exists in testsuite/c_tests/.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib.Test;
with Graphene.Rect;
with Gsk.Color_Node;
with Gsk.Enums;
with Gsk.Fill_Node;
with Gsk.Path;
with Gsk.Render_Node;
with System;

procedure Main is
   use Glib.Test;
   use type Gsk.Enums.Gsk_Fill_Rule;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type System.Address;

   --  Releasing a render-node wrapper runs its finalizer and drops its ref.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Constructors_And_Accessors with Convention => C;
   procedure Test_Nested_Child_Create with Convention => C;

   --  Check the immutable node's three stored properties and its node tag.
   procedure Check_Properties
     (Node : Gsk.Fill_Node.Gsk_Fill_Node;
      Child : Gsk.Render_Node.Gsk_Render_Node;
      Path : Gsk.Path.Gsk_Path;
      Rule : Gsk.Enums.Gsk_Fill_Rule)
   is
      Actual_Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Fill_Node.Get_Child (Node);
   begin
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Node)) = Gsk.Enums.Gsk_Fill_Node);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Actual_Child));
      Assert_True
        (Gsk.Render_Node.Get_Object (Actual_Child) =
           Gsk.Render_Node.Get_Object (Child));
      --  Get_Path returns the node's borrowed immutable path.
      Assert_True (Gsk.Fill_Node.Get_Path (Node).Equal (Path));
      Assert_True (Gsk.Fill_Node.Get_Fill_Rule (Node) = Rule);
      Free_Node (Actual_Child);
   end Check_Properties;

   --  Both constructors preserve their inputs; Initialize is a no-op once
   --  the immutable node has been created.
   procedure Test_Constructors_And_Accessors is
      --  Separate paths make it clear if either constructor stores the wrong
      --  area, while the two rules distinguish the enum accessor values.
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 28.0, Height => 20.0));
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.2, 0.5, 0.8, 1.0);
      Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node
          (Gsk.Color_Node.Gsk_Color_Node_New (Color'Access, Bounds'Access));
      Path : constant Gsk.Path.Gsk_Path := Gsk.Path.Parse ("M 0 0 L 40 0 L 40 30 Z");
      Other_Path : constant Gsk.Path.Gsk_Path :=
        Gsk.Path.Parse ("M 5 5 L 15 5 L 15 15 Z");
      Node : Gsk.Fill_Node.Gsk_Fill_Node :=
        Gsk.Fill_Node.Gsk_Fill_Node_New
          (Child, Path, Gsk.Enums.Gsk_Fill_Rule_Winding);
      Gtk_Node : Gsk.Fill_Node.Gsk_Fill_Node;
   begin
      Check_Properties
        (Node, Child, Path, Gsk.Enums.Gsk_Fill_Rule_Winding);

      --  A second initialization must not replace the first path or rule.
      Gsk.Fill_Node.Initialize
        (Node, Child, Other_Path, Gsk.Enums.Gsk_Fill_Rule_Even_Odd);
      Check_Properties
        (Node, Child, Path, Gsk.Enums.Gsk_Fill_Rule_Winding);

      --  Gtk_New is the procedure-style constructor and accepts the other
      --  fill rule, so the getter verifies that argument as well.
      Gsk.Fill_Node.Gtk_New
        (Gtk_Node, Child, Other_Path, Gsk.Enums.Gsk_Fill_Rule_Even_Odd);
      Check_Properties
        (Gtk_Node, Child, Other_Path, Gsk.Enums.Gsk_Fill_Rule_Even_Odd);

      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Gtk_Node));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Node));
      Free_Node (Child);
      Path.Unref;
      Other_Path.Unref;
   end Test_Constructors_And_Accessors;

   --  A fill node returned as a generic child requires dynamic wrapper
   --  creation, which dispatches to this unit's overriding Create function.
   procedure Test_Nested_Child_Create is
      --  This child is another fill node so dynamic wrapping must reconstruct
      --  the concrete type instead of returning only a generic render node.
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 50.0, Height => 50.0));
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.9, 0.3, 0.1, 0.75);
      Leaf : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node
          (Gsk.Color_Node.Gsk_Color_Node_New (Color'Access, Bounds'Access));
      Inner_Path : constant Gsk.Path.Gsk_Path := Gsk.Path.Parse ("M 0 0 L 20 0 L 20 20 Z");
      Outer_Path : constant Gsk.Path.Gsk_Path := Gsk.Path.Parse ("M 4 4 L 40 4 L 40 40 Z");
      Inner : Gsk.Fill_Node.Gsk_Fill_Node :=
        Gsk.Fill_Node.Gsk_Fill_Node_New
          (Leaf, Inner_Path, Gsk.Enums.Gsk_Fill_Rule_Even_Odd);
      Inner_As_Render : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Inner);
      Outer : Gsk.Fill_Node.Gsk_Fill_Node;
      Wrapped_Child : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Gsk.Fill_Node.Gtk_New
        (Outer, Inner_As_Render, Outer_Path, Gsk.Enums.Gsk_Fill_Rule_Winding);
      Wrapped_Child := Gsk.Fill_Node.Get_Child (Outer);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Wrapped_Child) = Gsk.Enums.Gsk_Fill_Node);
      Assert_True
        (Gsk.Render_Node.Get_Object (Wrapped_Child) =
           Gsk.Render_Node.Get_Object (Inner_As_Render));
      Free_Node (Wrapped_Child);

      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Outer));
      Free_Node (Inner_As_Render);
      Free_Node (Leaf);
      Inner_Path.Unref;
      Outer_Path.Unref;
   end Test_Nested_Child_Create;

begin
   Glib.Test.Init;
   Add_Func
     ("/gsk/fill-node/constructors-accessors",
      Test_Constructors_And_Accessors'Unrestricted_Access);
   Add_Func
     ("/gsk/fill-node/nested-child-create",
      Test_Nested_Child_Create'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Run);
end Main;
