--  Exercises Gsk.Paste_Node constructors, initialization, depth retrieval,
--  and reconstruction of the Ada wrapper from an existing render node.
--  GTK has no C testsuite case dedicated to GskPasteNode, so this checks the
--  documented behavior directly through the Ada binding.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Glib; use type Glib.Gsize;
with Glib.Test; use Glib.Test;
with Graphene.Rect;
with Gsk.Enums;
with Gsk.Paste_Node;
with Gsk.Render_Node;
with System;

procedure Main is

   use type System.Address;
   use type Gsk.Enums.Gsk_Render_Node_Type;

   --  Each render-node wrapper owns a reference and is freed through its
   --  corresponding access type after the assertions have run.
   procedure Free_Render_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);
   procedure Free_Paste_Node is new Ada.Unchecked_Deallocation
     (Gsk.Paste_Node.Gsk_Paste_Node_Record'Class,
      Gsk.Paste_Node.Gsk_Paste_Node);

   procedure Test_Paste_Node
   with Convention => C;

   --  Nonzero, different depths make it clear that each constructor forwards
   --  the requested copy index to GSK.
   procedure Test_Paste_Node is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 24.0, Height => 18.0));
      Other_Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 5.0, Y => 7.0), (Width => 12.0, Height => 9.0));
      Function_Node : Gsk.Paste_Node.Gsk_Paste_Node :=
        Gsk.Paste_Node.Gsk_Paste_Node_New (Bounds'Access, 2);
      Procedure_Node : Gsk.Paste_Node.Gsk_Paste_Node;
   begin
      --  The function constructor returns a live paste render node with the
      --  requested depth.
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Function_Node));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Function_Node)) =
           Gsk.Enums.Gsk_Paste_Node);
      Assert_True (Gsk.Paste_Node.Get_Depth (Function_Node) = 2);

      --  The procedure form initializes a fresh wrapper and stores depth 4.
      Gsk.Paste_Node.Gtk_New (Procedure_Node, Bounds'Access, 4);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Procedure_Node));
      Assert_True (Gsk.Paste_Node.Get_Depth (Procedure_Node) = 4);

      --  Initialize must leave an existing node untouched even when passed
      --  another rectangle and depth.
      Gsk.Paste_Node.Initialize
        (Procedure_Node, Other_Bounds'Access, 6);
      Assert_True (Gsk.Paste_Node.Get_Depth (Procedure_Node) = 4);

      --  Rewrapping an existing node dispatches to Paste_Node.Create while
      --  preserving the underlying C object and its dynamic node type.
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
           (Gsk.Render_Node.Get_Node_Type (Rewrapped) = Gsk.Enums.Gsk_Paste_Node);
         Assert_True
           (Gsk.Paste_Node.Get_Depth (Gsk.Paste_Node.Gsk_Paste_Node (Rewrapped)) = 2);
         Free_Render_Node (Rewrapped);
      end;

      Free_Paste_Node (Procedure_Node);
      Free_Paste_Node (Function_Node);
   end Test_Paste_Node;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gsk/paste-node", Test_Paste_Node'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
