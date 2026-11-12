--  Exercise Gsk.Blur_Node constructors, accessors, and deserialization.
--  testsuite/c_tests has no focused blur-node case to port.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib;
with Glib.Bytes;
with Glib.Test;
with Graphene.Rect;
with Interfaces.C;
with Gsk.Blur_Node;
with Gsk.Color_Node;
with Gsk.Enums;
with Gsk.Render_Node;
with System;

procedure Main is
   use Glib.Test;
   use type Glib.Gsize;
   use type Interfaces.C.C_float;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type System.Address;

   --  Each wrapper owns a reference to its immutable GSK node. Deallocation
   --  through the common class-wide access type runs the render-node finalizer.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Constructors_And_Accessors with Convention => C;
   procedure Test_Deserialization_Create with Convention => C;

   --  Verify both the stored radius and the identity of the original child.
   --  Get_Child returns a new Ada wrapper with its own reference, so release it.
   procedure Check_Properties
     (Node : Gsk.Blur_Node.Gsk_Blur_Node;
      Child : Gsk.Render_Node.Gsk_Render_Node;
      Radius : Interfaces.C.C_float)
   is
      Actual_Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Blur_Node.Get_Child (Node);
   begin
      Assert_True
        (Gsk.Render_Node.Get_Object (Actual_Child) =
           Gsk.Render_Node.Get_Object (Child));
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Blur_Node.Get_Radius (Node)),
         Glib.Gdouble (Radius), 0.001);
      Free_Node (Actual_Child);
   end Check_Properties;

   --  Exercise both constructor forms and the documented Initialize no-op.
   procedure Test_Constructors_And_Accessors is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 28.0, Height => 18.0));
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.3, 0.5, 0.7, 1.0);
      Child_Node : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New (Color'Access, Bounds'Access);
      Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Child_Node);
      Node : Gsk.Blur_Node.Gsk_Blur_Node :=
        Gsk.Blur_Node.Gsk_Blur_Node_New (Child, 4.5);
      Procedure_Node : Gsk.Blur_Node.Gsk_Blur_Node;
   begin
      --  The function constructor creates a blur node with the requested data.
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Node));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Node)) =
             Gsk.Enums.Gsk_Blur_Node);
      Check_Properties (Node, Child, 4.5);

      --  An immutable node ignores Initialize after its first creation.
      Gsk.Blur_Node.Initialize (Node, Child, 17.0);
      Check_Properties (Node, Child, 4.5);

      --  Gtk_New reaches the same Initialize path for a fresh wrapper.
      Gsk.Blur_Node.Gtk_New (Procedure_Node, Child, 8.25);
      Check_Properties (Procedure_Node, Child, 8.25);

      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Procedure_Node));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Node));
      Free_Node (Child);
   end Test_Constructors_And_Accessors;

   --  Deserialization dispatches to the concrete wrapper's Create override.
   procedure Test_Deserialization_Create is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 5.0, Y => 7.0), (Width => 32.0, Height => 24.0));
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.6, 0.4, 0.2, 0.9);
      Child_Node : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New (Color'Access, Bounds'Access);
      Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Child_Node);
      Node : Gsk.Blur_Node.Gsk_Blur_Node :=
        Gsk.Blur_Node.Gsk_Blur_Node_New (Child, 6.75);
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Blur : Gsk.Blur_Node.Gsk_Blur_Node;
      Restored_Child : Gsk.Render_Node.Gsk_Render_Node;
   begin
      --  The in-memory representation avoids dependence on files or a display.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Node));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) = Gsk.Enums.Gsk_Blur_Node);

      --  A successful downcast confirms the dispatching factory used Blur_Node.Create.
      Restored_Blur := Gsk.Blur_Node.Gsk_Blur_Node (Restored);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Blur_Node.Get_Radius (Restored_Blur)),
         6.75, 0.001);
      Restored_Child := Gsk.Blur_Node.Get_Child (Restored_Blur);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored_Child) =
           Gsk.Enums.Gsk_Color_Node);

      Free_Node (Restored_Child);
      Free_Node (Restored);
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Node));
      Free_Node (Child);
   end Test_Deserialization_Create;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gsk/blur-node/constructors-accessors",
      Test_Constructors_And_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/blur-node/deserialization-create",
      Test_Deserialization_Create'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
