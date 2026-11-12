--  Exercises Gsk.Cross_Fade_Node constructors, initialization, and getters.
--  GTK has no focused C testsuite case for this node, so the checks below
--  verify the documented behavior through the Ada binding.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib.Test;
with Graphene.Rect;
with Gsk.Color_Node;
with Gsk.Cross_Fade_Node;
with Gsk.Enums;
with Gsk.Render_Node;
with Gtk.Main;
with Interfaces.C;
with System;

procedure Main is
   use Glib.Test;
   use type Interfaces.C.C_float;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type System.Address;

   --  Deallocating an Ada render-node wrapper runs its finalizer, which
   --  releases the GskRenderNode reference owned by that wrapper.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Constructors_And_Accessors with Convention => C;

   --  Check the cross-fade's progress and the identity of both child nodes.
   procedure Check_Node
     (Node : Gsk.Cross_Fade_Node.Gsk_Cross_Fade_Node;
      Start, The_End : Gsk.Render_Node.Gsk_Render_Node;
      Progress : Interfaces.C.C_float)
   is
      Actual_Start : Gsk.Render_Node.Gsk_Render_Node;
      Actual_End   : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Node));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Node)) =
             Gsk.Enums.Gsk_Cross_Fade_Node);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Cross_Fade_Node.Get_Progress (Node)),
         Glib.Gdouble (Progress), 0.001);

      --  The getters return borrowed C children, wrapped with owned Ada
      --  references, so release both wrappers after comparing their pointers.
      Actual_Start := Gsk.Cross_Fade_Node.Get_Start_Child (Node);
      Actual_End := Gsk.Cross_Fade_Node.Get_End_Child (Node);
      Assert_True
        (Gsk.Render_Node.Get_Object (Actual_Start) =
           Gsk.Render_Node.Get_Object (Start));
      Assert_True
        (Gsk.Render_Node.Get_Object (Actual_End) =
           Gsk.Render_Node.Get_Object (The_End));
      Free_Node (Actual_Start);
      Free_Node (Actual_End);
   end Check_Node;

   --  Both constructors produce immutable nodes; Initialize on the existing
   --  function-style node must preserve its original children and progress.
   procedure Test_Constructors_And_Accessors is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 40.0, Height => 50.0));
      Start_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.1, 0.3, 0.5, 1.0);
      End_Color   : aliased Gdk.RGBA.Gdk_RGBA := (0.8, 0.4, 0.2, 0.75);
      Third_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.9, 0.2, 0.6, 1.0);
      Start_Node  : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New
          (Start_Color'Access, Bounds'Access);
      End_Node    : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New (End_Color'Access, Bounds'Access);
      Third_Node  : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New
          (Third_Color'Access, Bounds'Access);
      Start_Render : constant Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Start_Node);
      End_Render : constant Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (End_Node);
      Third_Render : constant Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Third_Node);
      Node : Gsk.Cross_Fade_Node.Gsk_Cross_Fade_Node :=
        Gsk.Cross_Fade_Node.Gsk_Cross_Fade_Node_New
          (Start_Render, End_Render, 0.35);
      Procedure_Node : Gsk.Cross_Fade_Node.Gsk_Cross_Fade_Node;
      Nested_Render : Gsk.Render_Node.Gsk_Render_Node;
      Nested_Node : Gsk.Cross_Fade_Node.Gsk_Cross_Fade_Node;
   begin
      --  The function constructor retains both source nodes and clamps the
      --  progress to the documented interval; an in-range value is preserved.
      Check_Node (Node, Start_Render, End_Render, 0.35);

      Gsk.Cross_Fade_Node.Initialize
        (Node, Third_Render, Start_Render, 0.9);
      Check_Node (Node, Start_Render, End_Render, 0.35);

      --  Gtk_New is the procedure-style constructor and accepts separate
      --  children and progress for comparison with the function constructor.
      Gsk.Cross_Fade_Node.Gtk_New
        (Procedure_Node, End_Render, Third_Render, 0.7);
      Check_Node (Procedure_Node, End_Render, Third_Render, 0.7);

      --  A cross-fade used as a child is wrapped back into its concrete Ada
      --  type by Get_Start_Child's runtime type dispatch and Create override.
      Nested_Render := Gsk.Render_Node.Gsk_Render_Node (Node);
      Nested_Node := Gsk.Cross_Fade_Node.Gsk_Cross_Fade_Node_New
        (Nested_Render, Third_Render, 0.6);
      declare
         Wrapped_Child : Gsk.Render_Node.Gsk_Render_Node :=
           Gsk.Cross_Fade_Node.Get_Start_Child (Nested_Node);
      begin
         Assert_True
           (Gsk.Render_Node.Get_Node_Type (Wrapped_Child) =
              Gsk.Enums.Gsk_Cross_Fade_Node);
         Assert_True
           (Gsk.Render_Node.Get_Object (Wrapped_Child) =
              Gsk.Render_Node.Get_Object (Node));
         Free_Node (Wrapped_Child);
      end;

      --  Release the node wrappers after their parent references are no
      --  longer needed; constants above are copies with their own references.
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Nested_Node));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Procedure_Node));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Node));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Start_Node));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (End_Node));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Third_Node));
   end Test_Constructors_And_Accessors;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gsk/cross-fade-node/constructors-accessors",
      Test_Constructors_And_Accessors'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
