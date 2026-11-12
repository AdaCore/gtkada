--  Exercise Gsk.Mask_Node constructors, initialization, and property getters.
--  GTK's C testsuite has no focused test for gsk_mask_node_*; the checks here
--  verify the documented behavior using ordinary immutable render nodes.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib.Test;
with Graphene.Rect;
with Gsk.Color_Node;
with Gsk.Enums;
with Gsk.Mask_Node;
with Gsk.Render_Node;
with Gtk.Main;
with System;

procedure Main is
   use Glib.Test;
   use type Gsk.Enums.Gsk_Mask_Mode;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type System.Address;

   --  Render-node wrappers own their GSK reference; freeing a wrapper runs
   --  its finalizer and releases that reference.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Constructors_And_Getters with Convention => C;

   --  Compare all the public properties and release the getter wrappers.
   procedure Check_Node
     (Node : Gsk.Mask_Node.Gsk_Mask_Node;
      Source, Mask : Gsk.Render_Node.Gsk_Render_Node;
      Mode : Gsk.Enums.Gsk_Mask_Mode)
   is
      Actual_Source : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Mask_Node.Get_Source (Node);
      Actual_Mask : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Mask_Node.Get_Mask (Node);
   begin
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Node));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Node)) =
             Gsk.Enums.Gsk_Mask_Node);
      Assert_True (Gsk.Mask_Node.Get_Mask_Mode (Node) = Mode);
      --  Borrowed C children are wrapped with owned Ada references, so the
      --  returned wrappers can be freed after their identities are checked.
      Assert_True
        (Gsk.Render_Node.Get_Object (Actual_Source) =
           Gsk.Render_Node.Get_Object (Source));
      Assert_True
        (Gsk.Render_Node.Get_Object (Actual_Mask) =
           Gsk.Render_Node.Get_Object (Mask));
      Free_Node (Actual_Source);
      Free_Node (Actual_Mask);
   end Check_Node;

   --  Each constructor delegates to Initialize. A repeated call on a live
   --  immutable node must leave its original children and mode unchanged.
   procedure Test_Constructors_And_Getters is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 1.0, Y => 2.0), (Width => 30.0, Height => 20.0));
      Source_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.8, 0.2, 0.1, 0.9);
      Mask_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.1, 0.6, 0.4, 0.7);
      Other_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.3, 0.2, 0.9, 1.0);
      Source_Node : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New
          (Source_Color'Access, Bounds'Access);
      Mask_Node : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New (Mask_Color'Access, Bounds'Access);
      Other_Node : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New (Other_Color'Access, Bounds'Access);
      Source : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Source_Node);
      Mask : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Mask_Node);
      Other : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Other_Node);
      Node : Gsk.Mask_Node.Gsk_Mask_Node :=
        Gsk.Mask_Node.Gsk_Mask_Node_New
          (Source, Mask, Gsk.Enums.Gsk_Mask_Mode_Alpha);
      Procedure_Node : Gsk.Mask_Node.Gsk_Mask_Node;
   begin
      --  The function constructor stores the requested source, mask, and mode.
      Check_Node (Node, Source, Mask, Gsk.Enums.Gsk_Mask_Mode_Alpha);

      --  Initialize must do nothing after the first construction.
      Gsk.Mask_Node.Initialize
        (Node, Other, Source, Gsk.Enums.Gsk_Mask_Mode_Luminance);
      Check_Node (Node, Source, Mask, Gsk.Enums.Gsk_Mask_Mode_Alpha);

      --  Gtk_New follows the procedure-style constructor path independently.
      Gsk.Mask_Node.Gtk_New
        (Procedure_Node, Mask, Other,
         Gsk.Enums.Gsk_Mask_Mode_Inverted_Luminance);
      Check_Node
        (Procedure_Node, Mask, Other,
         Gsk.Enums.Gsk_Mask_Mode_Inverted_Luminance);

      --  Release wrappers only after both parent nodes have retained the
      --  child references used by the getters above.
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Procedure_Node));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Node));
      Free_Node (Source);
      Free_Node (Mask);
      Free_Node (Other);
   end Test_Constructors_And_Getters;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gsk/mask-node/constructors-getters",
      Test_Constructors_And_Getters'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
