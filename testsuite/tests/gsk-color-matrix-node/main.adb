--  Exercise Gsk.Color_Matrix_Node constructors, accessors, and deserialization.
--  No focused test in testsuite/c_tests/ covers this GSK node.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib.Bytes;
with Glib.Test;
with Graphene.Matrix;
with Graphene.Rect;
with Graphene.Vec4;
with Interfaces.C;
with Gsk.Color_Matrix_Node;
with Gsk.Color_Node;
with Gsk.Enums;
with Gsk.Render_Node;
with System;

procedure Main is
   use Glib.Test;
   use type Glib.Gdouble;
   use type Interfaces.C.C_float;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type System.Address;

   --  Render-node wrappers own their GSK reference; deallocation runs the
   --  controlled finalizer and releases that reference.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Constructors_And_Accessors with Convention => C;
   procedure Test_Deserialization_Create with Convention => C;

   --  Compare copied accessor results so the checks cover the stored values,
   --  not just that each accessor returned a non-null pointer.
   procedure Check_Properties
     (Node : Gsk.Color_Matrix_Node.Gsk_Color_Matrix_Node;
      Child : Gsk.Render_Node.Gsk_Render_Node;
      Matrix : not null access Graphene.Matrix.Graphene_Matrix_T;
      Offset : not null access Graphene.Vec4.Graphene_Vec4_T;
      Same_Child : Boolean := True)
   is
      Actual_Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Color_Matrix_Node.Get_Child (Node);
      Actual_Matrix : aliased Graphene.Matrix.Graphene_Matrix_T :=
        Gsk.Color_Matrix_Node.Get_Color_Matrix (Node).all;
      Actual_Offset : aliased Graphene.Vec4.Graphene_Vec4_T :=
        Gsk.Color_Matrix_Node.Get_Color_Offset (Node).all;
   begin
      --  Deserialization recreates children, so compare their kind there;
      --  construction must retain the exact immutable child object.
      if Same_Child then
         Assert_True
           (Gsk.Render_Node.Get_Object (Actual_Child) =
              Gsk.Render_Node.Get_Object (Child));
      else
         Assert_True
           (Gsk.Render_Node.Get_Node_Type (Actual_Child) =
              Gsk.Render_Node.Get_Node_Type (Child));
      end if;
      Assert_True
        (Graphene.Matrix.Equal (Actual_Matrix'Access, Matrix));
      Assert_True
        (Graphene.Vec4.Equal (Actual_Offset'Access, Offset));
      --  From_Object_None_Ownership gives this wrapper its own reference.
      Free_Node (Actual_Child);
   end Check_Properties;

   --  Both constructors create the node through Initialize. A second call to
   --  Initialize must preserve the immutable node's original properties.
   procedure Test_Constructors_And_Accessors is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 24.0, Height => 18.0));
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.6, 1.0);
      Child_Node : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New (Color'Access, Bounds'Access);
      Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Child_Node);
      Matrix, Other_Matrix : aliased Graphene.Matrix.Graphene_Matrix_T;
      Offset, Other_Offset : aliased Graphene.Vec4.Graphene_Vec4_T;
      Matrix_Init : access Graphene.Matrix.Graphene_Matrix_T;
      Other_Matrix_Init : access Graphene.Matrix.Graphene_Matrix_T;
      Offset_Init : access Graphene.Vec4.Graphene_Vec4_T;
      Other_Offset_Init : access Graphene.Vec4.Graphene_Vec4_T;
      Node : Gsk.Color_Matrix_Node.Gsk_Color_Matrix_Node;
      Gtk_Node : Gsk.Color_Matrix_Node.Gsk_Color_Matrix_Node;
   begin
      --  Distinct values make it possible to detect ignored or swapped inputs.
      Matrix_Init := Graphene.Matrix.Init_From_2D
        (Matrix'Access, 1.0, 0.25, -0.5, 1.0, 2.0, -3.0);
      Offset_Init := Graphene.Vec4.Init
        (Offset'Access, 0.1, 0.2, 0.3, 0.4);
      Other_Matrix_Init := Graphene.Matrix.Init_From_2D
        (Other_Matrix'Access, 0.5, 0.0, 0.0, 0.75, 8.0, 9.0);
      Other_Offset_Init := Graphene.Vec4.Init
        (Other_Offset'Access, -0.1, -0.2, -0.3, -0.4);

      --  The function constructor stores its child, matrix, and offset.
      Node := Gsk.Color_Matrix_Node.Gsk_Color_Matrix_Node_New
        (Child, Matrix'Access, Offset'Access);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Node)) =
             Gsk.Enums.Gsk_Color_Matrix_Node);
      Check_Properties (Node, Child, Matrix'Access, Offset'Access);

      --  Initialize is a documented no-op after construction.
      Gsk.Color_Matrix_Node.Initialize
        (Node, Child, Other_Matrix'Access, Other_Offset'Access);
      Check_Properties (Node, Child, Matrix'Access, Offset'Access);

      --  The procedure constructor follows the same initialization path.
      Gsk.Color_Matrix_Node.Gtk_New
        (Gtk_Node, Child, Other_Matrix'Access, Other_Offset'Access);
      Check_Properties
        (Gtk_Node, Child, Other_Matrix'Access, Other_Offset'Access);

      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Gtk_Node));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Node));
      Free_Node (Child);
   end Test_Constructors_And_Accessors;

   --  Deserializing this node dispatches through the concrete Create hook.
   procedure Test_Deserialization_Create is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 4.0, Y => 5.0), (Width => 32.0, Height => 20.0));
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.7, 0.3, 0.1, 0.8);
      Child_Node : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New (Color'Access, Bounds'Access);
      Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Child_Node);
      Matrix : aliased Graphene.Matrix.Graphene_Matrix_T;
      Offset : aliased Graphene.Vec4.Graphene_Vec4_T;
      Matrix_Init : access Graphene.Matrix.Graphene_Matrix_T;
      Offset_Init : access Graphene.Vec4.Graphene_Vec4_T;
      Node : Gsk.Color_Matrix_Node.Gsk_Color_Matrix_Node;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Node : Gsk.Color_Matrix_Node.Gsk_Color_Matrix_Node;
   begin
      Matrix_Init := Graphene.Matrix.Init_From_2D
        (Matrix'Access, 1.0, 0.0, 0.0, 0.5, 6.0, 7.0);
      Offset_Init := Graphene.Vec4.Init
        (Offset'Access, 0.05, 0.15, 0.25, 0.35);
      Node := Gsk.Color_Matrix_Node.Gsk_Color_Matrix_Node_New
        (Child, Matrix'Access, Offset'Access);
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Node));

      --  The restored generic node must carry the color-matrix node tag.
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) =
           Gsk.Enums.Gsk_Color_Matrix_Node);
      Restored_Node :=
        Gsk.Color_Matrix_Node.Gsk_Color_Matrix_Node (Restored);
      Check_Properties
        (Restored_Node, Child, Matrix'Access, Offset'Access,
         Same_Child => False);

      Free_Node (Restored);
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Node));
      Free_Node (Child);
   end Test_Deserialization_Create;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gsk/color-matrix-node/constructors-accessors",
      Test_Constructors_And_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/color-matrix-node/deserialization-create",
      Test_Deserialization_Create'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
