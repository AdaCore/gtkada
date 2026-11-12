--  Exercises Gsk.Transform_Node constructors, getters, and its runtime
--  wrapper factory. There is no matching gsk_transform_node C test to port.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib;
with Glib.Test;
with Graphene.Point;
with Graphene.Rect;
with Gsk.Color_Node;
with Gsk.Enums;
with Gsk.Render_Node;
with Gsk.Transform;
with Gsk.Transform_Node;
with System;

procedure Main is
   use Glib.Test;
   use type Glib.GType;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type System.Address;

   --  Deallocating a node wrapper runs its finalizer and releases its owned
   --  C reference. Get_Child returns such an independently owned wrapper.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Constructors_And_Accessors with Convention => C;

   --  Both constructors preserve their child and transform, and an
   --  Initialize call on an existing node leaves those values unchanged.
   procedure Test_Constructors_And_Accessors is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 24.0, Height => 18.0));
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.6, 1.0);
      Offset : aliased Graphene.Point.Graphene_Point_T := (5.0, 7.0);
      Other_Offset : aliased Graphene.Point.Graphene_Point_T := (2.0, 3.0);
      Child_Node : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New (Color'Access, Bounds'Access);
      Child : constant Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Child_Node);
      Transform : Gsk.Transform.Gsk_Transform :=
        Gsk.Transform.Translate
          (Gsk.Transform.Gsk_Transform_New, Offset'Access);
      Other_Transform : Gsk.Transform.Gsk_Transform :=
        Gsk.Transform.Translate
          (Gsk.Transform.Gsk_Transform_New, Other_Offset'Access);
      Function_Node : Gsk.Transform_Node.Gsk_Transform_Node :=
        Gsk.Transform_Node.Gsk_Transform_Node_New (Child, Transform);
      Procedure_Node : Gsk.Transform_Node.Gsk_Transform_Node;
      Nested_Transform : Gsk.Transform.Gsk_Transform :=
        Gsk.Transform.Translate
          (Gsk.Transform.Gsk_Transform_New, Other_Offset'Access);
      Nested_Node : Gsk.Transform_Node.Gsk_Transform_Node;
      Actual_Child : Gsk.Render_Node.Gsk_Render_Node;
      Actual_Transform : Gsk.Transform.Gsk_Transform;
   begin
      --  The function constructor creates a concrete transform node. Its
      --  transform argument is transferred to the new render node.
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Function_Node));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Function_Node)) =
         Gsk.Enums.Gsk_Transform_Node);
      Assert_True
        (Gsk.Transform_Node.Get_Type /= Gsk.Render_Node.Get_Type);

      --  Initialize documents a no-op for an already-created record. Passing
      --  different values lets the getters verify the original values remain.
      Gsk.Transform_Node.Initialize
        (Function_Node, Child, Other_Transform);
      Actual_Child := Gsk.Transform_Node.Get_Child (Function_Node);
      Assert_True
        (Gsk.Render_Node.Get_Object (Actual_Child) =
         Gsk.Render_Node.Get_Object (Child));
      Actual_Transform := Gsk.Transform_Node.Get_Transform (Function_Node);
      Assert_True (Gsk.Transform.Equal (Actual_Transform, Transform));
      Free_Node (Actual_Child);
      Other_Transform.Unref;

      --  Gtk_New is the procedure-style constructor and owns this transform.
      Gsk.Transform_Node.Gtk_New (Procedure_Node, Child, Nested_Transform);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Procedure_Node));
      Actual_Child := Gsk.Transform_Node.Get_Child (Procedure_Node);
      Assert_True
        (Gsk.Render_Node.Get_Object (Actual_Child) =
         Gsk.Render_Node.Get_Object (Child));
      Free_Node (Actual_Child);

      --  A transform node used as a child is wrapped with its concrete Ada
      --  type by From_Object_None_Ownership, invoking Transform_Node.Create.
      Gsk.Transform_Node.Gtk_New (Nested_Node,
        Gsk.Render_Node.Gsk_Render_Node (Function_Node),
        Gsk.Transform.Translate
          (Gsk.Transform.Gsk_Transform_New, Other_Offset'Access));
      Actual_Child := Gsk.Transform_Node.Get_Child (Nested_Node);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Actual_Child) =
         Gsk.Enums.Gsk_Transform_Node);
      Assert_True
        (Gsk.Render_Node.Get_Object (Actual_Child) =
         Gsk.Render_Node.Get_Object (Function_Node));
      Actual_Transform := Gsk.Transform_Node.Get_Transform
        (Gsk.Transform_Node.Gsk_Transform_Node (Actual_Child));
      Assert_True (Gsk.Transform.Equal (Actual_Transform, Transform));

      --  Release getter wrappers and node wrappers after the parent references
      --  have been established; the original child remains shared by nodes.
      Free_Node (Actual_Child);
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Nested_Node));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Procedure_Node));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Function_Node));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Child_Node));
   end Test_Constructors_And_Accessors;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gsk/transform-node/constructors-accessors",
      Test_Constructors_And_Accessors'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
