--  Exercises Gsk.Copy_Node constructors, initialization, child access, and
--  reconstruction of its Ada wrapper from an existing GskRenderNode.
--  GTK has no C testsuite case dedicated to GskCopyNode, so the checks below
--  verify the documented behavior directly through the Ada binding.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Glib.Test; use Glib.Test;
with Gsk.Color_Node;
with Gsk.Copy_Node;
with Gsk.Enums;
with Gsk.Render_Node;
with Gdk.RGBA;
with Graphene.Rect;
with System;

procedure Main is

   use type System.Address;
   use type Gsk.Enums.Gsk_Render_Node_Type;

   --  Render-node wrappers own a C reference, so each wrapper is released
   --  through its own Ada access type when the test is done with it.
   procedure Free_Render_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);
   procedure Free_Copy_Node is new Ada.Unchecked_Deallocation
     (Gsk.Copy_Node.Gsk_Copy_Node_Record'Class,
      Gsk.Copy_Node.Gsk_Copy_Node);

   procedure Test_Copy_Node
   with Convention => C;

   --  The color nodes are simple, display-independent children that let this
   --  test check pointer identity without creating a renderer or a window.
   procedure Test_Copy_Node is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((0.0, 0.0), (12.0, 8.0));
      Color  : aliased Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.6, 1.0);
      Other_Color : aliased Gdk.RGBA.Gdk_RGBA := (0.8, 0.1, 0.3, 1.0);
      Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node
          (Gsk.Color_Node.Gsk_Color_Node_New (Color'Access, Bounds'Access));
      Other_Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node
          (Gsk.Color_Node.Gsk_Color_Node_New
             (Other_Color'Access, Bounds'Access));
      Function_Node : Gsk.Copy_Node.Gsk_Copy_Node :=
        Gsk.Copy_Node.Gsk_Copy_Node_New (Child);
      Procedure_Node : Gsk.Copy_Node.Gsk_Copy_Node;
   begin
      --  The function constructor stores a copy node containing its child.
      Assert_Nonnull
        (Gsk.Render_Node.Get_Object
           (Gsk.Render_Node.Gsk_Render_Node (Function_Node)));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Function_Node)) =
           Gsk.Enums.Gsk_Copy_Node);

      --  Exercise the procedure constructor and check its borrowed child.
      Gsk.Copy_Node.Gtk_New (Procedure_Node, Child);
      declare
         Read_Child : Gsk.Render_Node.Gsk_Render_Node :=
           Gsk.Copy_Node.Get_Child (Procedure_Node);
      begin
         Assert_True
           (Gsk.Render_Node.Get_Object (Read_Child) =
              Gsk.Render_Node.Get_Object (Child));
         Free_Render_Node (Read_Child);
      end;

      --  Initialize is a no-op for an already-created wrapper; supplying a
      --  different child proves that the original child remains attached.
      Gsk.Copy_Node.Initialize (Procedure_Node, Other_Child);
      declare
         Read_Child : Gsk.Render_Node.Gsk_Render_Node :=
           Gsk.Copy_Node.Get_Child (Procedure_Node);
      begin
         Assert_True
           (Gsk.Render_Node.Get_Object (Read_Child) =
              Gsk.Render_Node.Get_Object (Child));
         Free_Render_Node (Read_Child);
      end;

      --  Wrapping an existing C node asks the render-node factory to create
      --  the concrete Ada type. This covers Gsk.Copy_Node.Create as well as
      --  confirming that the dynamic wrapper keeps the same C object.
      declare
         Rewrapped : Gsk.Render_Node.Gsk_Render_Node :=
           Gsk.Render_Node.From_Object_None_Ownership
             (Gsk.Render_Node.Get_Object
                (Gsk.Render_Node.Gsk_Render_Node (Function_Node)));
      begin
         Assert_True
           (Gsk.Render_Node.Get_Object (Rewrapped) =
              Gsk.Render_Node.Get_Object
                (Gsk.Render_Node.Gsk_Render_Node (Function_Node)));
         Assert_True
           (Gsk.Render_Node.Get_Node_Type (Rewrapped) = Gsk.Enums.Gsk_Copy_Node);
         Free_Render_Node (Rewrapped);
      end;

      --  The child getter also works on the function-constructed wrapper.
      declare
         Read_Child : Gsk.Render_Node.Gsk_Render_Node :=
           Gsk.Copy_Node.Get_Child (Function_Node);
      begin
         Assert_True
           (Gsk.Render_Node.Get_Object (Read_Child) =
              Gsk.Render_Node.Get_Object (Child));
         Free_Render_Node (Read_Child);
      end;

      Free_Copy_Node (Function_Node);
      Free_Copy_Node (Procedure_Node);
      Free_Render_Node (Child);
      Free_Render_Node (Other_Child);
   end Test_Copy_Node;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/gsk/copy-node", Test_Copy_Node'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
