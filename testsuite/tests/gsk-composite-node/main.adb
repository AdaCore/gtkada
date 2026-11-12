--  Exercise Gsk.Composite_Node constructors, getters, and wrapper creation.
--  No focused C test for gsk_composite_node_* exists in testsuite/c_tests/.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib.Bytes;
with Glib.Test;
with Graphene.Rect;
with Gsk.Color_Node;
with Gsk.Composite_Node;
with Gsk.Enums;
with Gsk.Render_Node;
with System;

procedure Main is
   use Glib.Test;
   use type Glib.Gsize;
   use type Gsk.Enums.Gsk_Porter_Duff;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type System.Address;

   --  Each node wrapper owns a reference; deallocation runs its finalizer.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Constructors_And_Getters with Convention => C;
   procedure Test_Deserialization_Create with Convention => C;

   --  Assert that the getters preserve both node identities and the operator.
   procedure Check_Properties
     (Node : Gsk.Composite_Node.Gsk_Composite_Node;
      Child, Mask : Gsk.Render_Node.Gsk_Render_Node;
      Op : Gsk.Enums.Gsk_Porter_Duff;
      Same_Objects : Boolean := True)
   is
      Actual_Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Composite_Node.Get_Child (Node);
      Actual_Mask : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Composite_Node.Get_Mask (Node);
   begin
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Actual_Child));
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Actual_Mask));
      if Same_Objects then
         --  Direct construction retains the exact immutable child objects.
         Assert_True
           (Gsk.Render_Node.Get_Object (Actual_Child) =
              Gsk.Render_Node.Get_Object (Child));
         Assert_True
           (Gsk.Render_Node.Get_Object (Actual_Mask) =
              Gsk.Render_Node.Get_Object (Mask));
      else
         --  Deserialization rebuilds nodes, so distinguish them by their colors.
         declare
            Actual_Child_Color : constant Gdk.RGBA.Gdk_RGBA :=
              Gsk.Color_Node.Get_Color
                (Gsk.Color_Node.Gsk_Color_Node (Actual_Child)).all;
            Expected_Child_Color : constant Gdk.RGBA.Gdk_RGBA :=
              Gsk.Color_Node.Get_Color
                (Gsk.Color_Node.Gsk_Color_Node (Child)).all;
            Actual_Mask_Color : constant Gdk.RGBA.Gdk_RGBA :=
              Gsk.Color_Node.Get_Color
                (Gsk.Color_Node.Gsk_Color_Node (Actual_Mask)).all;
            Expected_Mask_Color : constant Gdk.RGBA.Gdk_RGBA :=
              Gsk.Color_Node.Get_Color
                (Gsk.Color_Node.Gsk_Color_Node (Mask)).all;
         begin
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual_Child_Color.Red),
               Glib.Gdouble (Expected_Child_Color.Red), 0.001);
            Assert_Cmpfloat_With_Epsilon
              (Glib.Gdouble (Actual_Mask_Color.Blue),
               Glib.Gdouble (Expected_Mask_Color.Blue), 0.001);
         end;
      end if;
      Assert_True (Gsk.Composite_Node.Get_Operator (Node) = Op);

      --  Get_Child and Get_Mask return newly owned Ada wrappers.
      Free_Node (Actual_Child);
      Free_Node (Actual_Mask);
   end Check_Properties;

   --  Use separate color nodes so a swapped child and mask is observable.
   --  Plain render-node construction does not need a widget or display.
   procedure Make_Inputs
     (Child, Mask : out Gsk.Render_Node.Gsk_Render_Node)
   is
      --  Different bounds and colors make the two inputs easy to identify.
      Child_Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 1.0, Y => 2.0), (Width => 30.0, Height => 25.0));
      Mask_Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 4.0, Y => 6.0), (Width => 18.0, Height => 12.0));
      Child_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.9, 0.2, 0.1, 1.0);
      Mask_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.1, 0.3, 0.8, 0.7);
   begin
      Child := Gsk.Render_Node.Gsk_Render_Node
        (Gsk.Color_Node.Gsk_Color_Node_New
           (Child_Color'Access, Child_Bounds'Access));
      Mask := Gsk.Render_Node.Gsk_Render_Node
        (Gsk.Color_Node.Gsk_Color_Node_New
           (Mask_Color'Access, Mask_Bounds'Access));
   end Make_Inputs;

   --  Cover both public constructor forms and Initialize's created-object guard.
   procedure Test_Constructors_And_Getters is
      Child, Mask : Gsk.Render_Node.Gsk_Render_Node;
      Node : Gsk.Composite_Node.Gsk_Composite_Node;
      Gtk_Node : Gsk.Composite_Node.Gsk_Composite_Node;
      Node_As_Render : Gsk.Render_Node.Gsk_Render_Node;
      Gtk_Node_As_Render : Gsk.Render_Node.Gsk_Render_Node;
   begin
      --  Keep both input wrappers alive while the composite node retains refs.
      Make_Inputs (Child, Mask);

      --  The function constructor records the two inputs and the chosen op.
      Node := Gsk.Composite_Node.Gsk_Composite_Node_New
        (Child, Mask, Gsk.Enums.Gsk_Porter_Duff_Source_Over_Dest);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Node));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Node)) =
             Gsk.Enums.Gsk_Composite_Node);
      Check_Properties
        (Node, Child, Mask, Gsk.Enums.Gsk_Porter_Duff_Source_Over_Dest);

      --  Immutable render nodes ignore initialization after the first call.
      Gsk.Composite_Node.Initialize
        (Node, Mask, Child, Gsk.Enums.Gsk_Porter_Duff_Clear);
      Check_Properties
        (Node, Child, Mask, Gsk.Enums.Gsk_Porter_Duff_Source_Over_Dest);

      --  The procedure constructor initializes a second node independently.
      --  Swapping the nodes and operator verifies all three constructor inputs.
      Gsk.Composite_Node.Gtk_New
        (Gtk_Node, Mask, Child, Gsk.Enums.Gsk_Porter_Duff_Xor);
      Check_Properties (Gtk_Node, Mask, Child, Gsk.Enums.Gsk_Porter_Duff_Xor);

      Node_As_Render := Gsk.Render_Node.Gsk_Render_Node (Node);
      Gtk_Node_As_Render := Gsk.Render_Node.Gsk_Render_Node (Gtk_Node);
      Free_Node (Node_As_Render);
      Free_Node (Gtk_Node_As_Render);
      Free_Node (Child);
      Free_Node (Mask);
   end Test_Constructors_And_Getters;

   --  Deserializing a composite render node dispatches to this unit's Create.
   procedure Test_Deserialization_Create is
      Child, Mask : Gsk.Render_Node.Gsk_Render_Node;
      Node : Gsk.Composite_Node.Gsk_Composite_Node;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Node : Gsk.Composite_Node.Gsk_Composite_Node;
   begin
      --  Serialize in memory so the test needs neither files nor a renderer.
      Make_Inputs (Child, Mask);
      Node := Gsk.Composite_Node.Gsk_Composite_Node_New
        (Child, Mask, Gsk.Enums.Gsk_Porter_Duff_Dest_Atop_Source);
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Node));
      Assert_True (Serialized.Get_Size > 0);

      --  The returned generic wrapper must have the concrete composite type.
      --  Typed conversion here depends on the factory calling Composite_Node.Create.
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) =
           Gsk.Enums.Gsk_Composite_Node);
      Restored_Node := Gsk.Composite_Node.Gsk_Composite_Node (Restored);
      Check_Properties
        (Restored_Node, Child, Mask,
         Gsk.Enums.Gsk_Porter_Duff_Dest_Atop_Source,
         Same_Objects => False);

      Free_Node (Restored);
      declare
         Original : Gsk.Render_Node.Gsk_Render_Node :=
           Gsk.Render_Node.Gsk_Render_Node (Node);
      begin
         Free_Node (Original);
      end;
      Free_Node (Child);
      Free_Node (Mask);
   end Test_Deserialization_Create;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gsk/composite-node/constructors-getters",
      Test_Constructors_And_Getters'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/composite-node/deserialization-create",
      Test_Deserialization_Create'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
