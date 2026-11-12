--  Exercise Gsk.Blend_Node constructors, accessors, and deserialization.
--  The C testsuite has no focused blend-node test to port.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib;
with Glib.Bytes;
with Glib.Test;
with Graphene.Rect;
with Gsk.Blend_Node;
with Gsk.Color_Node;
with Gsk.Enums;
with Gsk.Render_Node;
with System;

procedure Main is
   use Glib.Test;
   use type Glib.Gsize;
   use type Gsk.Enums.Gsk_Blend_Mode;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type System.Address;

   --  Each Ada wrapper owns one GSK reference; freeing through this generic
   --  access type runs the controlled render-node finalizer exactly once.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Constructors_And_Accessors with Convention => C;
   procedure Test_Deserialization_Create with Convention => C;

   --  Both public constructors reach Initialize. The explicit second call
   --  checks its documented no-op behavior after the node already exists.
   procedure Test_Constructors_And_Accessors is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 20.0, Height => 12.0));
      Bottom_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.6, 1.0);
      Top_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.8, 0.3, 0.1, 0.5);
      Bottom : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New
          (Bottom_Color'Access, Bounds'Access);
      Top : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New (Top_Color'Access, Bounds'Access);
      Bottom_Render : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Bottom);
      Top_Render : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Top);
      Blend : Gsk.Blend_Node.Gsk_Blend_Node :=
        Gsk.Blend_Node.Gsk_Blend_Node_New
          (Bottom_Render, Top_Render, Gsk.Enums.Gsk_Blend_Mode_Overlay);
      Procedure_Blend : Gsk.Blend_Node.Gsk_Blend_Node;
      Blend_Render : Gsk.Render_Node.Gsk_Render_Node;
      Procedure_Render : Gsk.Render_Node.Gsk_Render_Node;
      Actual_Bottom : Gsk.Render_Node.Gsk_Render_Node;
      Actual_Top : Gsk.Render_Node.Gsk_Render_Node;
   begin
      --  The function constructor preserves the chosen mode and child order.
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Blend));
      Assert_True
        (Gsk.Blend_Node.Get_Blend_Mode (Blend) =
           Gsk.Enums.Gsk_Blend_Mode_Overlay);
      Actual_Bottom := Gsk.Blend_Node.Get_Bottom_Child (Blend);
      Actual_Top := Gsk.Blend_Node.Get_Top_Child (Blend);
      Assert_True
        (Gsk.Render_Node.Get_Object (Actual_Bottom) =
           Gsk.Render_Node.Get_Object (Bottom_Render));
      Assert_True
        (Gsk.Render_Node.Get_Object (Actual_Top) =
           Gsk.Render_Node.Get_Object (Top_Render));
      Free_Node (Actual_Top);
      Free_Node (Actual_Bottom);

      --  Initialize must leave an already-created immutable node unchanged.
      Gsk.Blend_Node.Initialize
        (Blend, Top_Render, Bottom_Render, Gsk.Enums.Gsk_Blend_Mode_Multiply);
      Assert_True
        (Gsk.Blend_Node.Get_Blend_Mode (Blend) =
           Gsk.Enums.Gsk_Blend_Mode_Overlay);

      --  The procedure constructor exercises Gtk_New with another mode.
      Gsk.Blend_Node.Gtk_New
        (Procedure_Blend, Bottom_Render, Top_Render,
         Gsk.Enums.Gsk_Blend_Mode_Multiply);
      Assert_True
        (Gsk.Blend_Node.Get_Blend_Mode (Procedure_Blend) =
           Gsk.Enums.Gsk_Blend_Mode_Multiply);

      --  A blend node reports the corresponding generic GSK node kind.
      Blend_Render := Gsk.Render_Node.Gsk_Render_Node (Blend);
      Procedure_Render :=
        Gsk.Render_Node.Gsk_Render_Node (Procedure_Blend);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Blend_Render) =
           Gsk.Enums.Gsk_Blend_Node);

      Free_Node (Procedure_Render);
      Free_Node (Blend_Render);
      Free_Node (Top_Render);
      Free_Node (Bottom_Render);
   end Test_Constructors_And_Accessors;

   --  Deserializing a serialized blend node uses GtkAda's dispatching
   --  render-node factory, so this also exercises Blend_Node.Create.
   procedure Test_Deserialization_Create is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 3.0, Y => 5.0), (Width => 24.0, Height => 16.0));
      Bottom_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.1, 0.3, 0.5, 1.0);
      Top_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.7, 0.2, 0.4, 0.7);
      Bottom : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New
          (Bottom_Color'Access, Bounds'Access);
      Top : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New (Top_Color'Access, Bounds'Access);
      Blend : Gsk.Blend_Node.Gsk_Blend_Node :=
        Gsk.Blend_Node.Gsk_Blend_Node_New
          (Gsk.Render_Node.Gsk_Render_Node (Bottom),
           Gsk.Render_Node.Gsk_Render_Node (Top),
           Gsk.Enums.Gsk_Blend_Mode_Screen);
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Blend : Gsk.Blend_Node.Gsk_Blend_Node;
      Child : Gsk.Render_Node.Gsk_Render_Node;
   begin
      --  Bytes stay in memory, so the factory test needs no display or files.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Blend));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) =
           Gsk.Enums.Gsk_Blend_Node);

      --  The dynamic conversion succeeds only if the concrete wrapper was
      --  constructed with the blend-node tag by the dispatching factory.
      Restored_Blend := Gsk.Blend_Node.Gsk_Blend_Node (Restored);
      Assert_True
        (Gsk.Blend_Node.Get_Blend_Mode (Restored_Blend) =
           Gsk.Enums.Gsk_Blend_Mode_Screen);
      Child := Gsk.Blend_Node.Get_Bottom_Child (Restored_Blend);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Child) = Gsk.Enums.Gsk_Color_Node);
      Free_Node (Child);

      Free_Node (Restored);
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Blend));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Top));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Bottom));
   end Test_Deserialization_Create;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gsk/blend-node/constructors-accessors",
      Test_Constructors_And_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/blend-node/deserialization-create",
      Test_Deserialization_Create'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
