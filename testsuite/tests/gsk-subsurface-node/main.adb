--  Exercises both Gsk.Subsurface_Node constructors, Initialize, getters, and
--  the render-node wrapper factory. No C testsuite case covers this node.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib.Object;
with Glib.Test; use Glib.Test;
with Graphene.Rect;
with Gsk.Render_Node;
with Gsk.Subsurface_Node;
with Gtk.Snapshot;
with System;

procedure Main is
   use type System.Address;

   --  The Ada render-node wrappers own references to their underlying GSK
   --  nodes, so each wrapper allocated by a getter is explicitly deallocated.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);
   procedure Free_Subsurface_Node is new Ada.Unchecked_Deallocation
     (Gsk.Subsurface_Node.Gsk_Subsurface_Node_Record'Class,
      Gsk.Subsurface_Node.Gsk_Subsurface_Node);

   procedure Test_Subsurface_Node with Convention => C;

   --  A snapshot supplies an ordinary immutable child without creating a
   --  window, display surface, or renderer.
   procedure Test_Subsurface_Node is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 20.0, Height => 16.0));
      Color : constant Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.6, 1.0);
      Replacement_Color : constant Gdk.RGBA.Gdk_RGBA :=
        (0.8, 0.3, 0.1, 1.0);
      Snapshot : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Replacement_Snapshot : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Child : Gsk.Render_Node.Gsk_Render_Node;
      Replacement_Child : Gsk.Render_Node.Gsk_Render_Node;
      Function_Node : Gsk.Subsurface_Node.Gsk_Subsurface_Node;
      Procedure_Node : Gsk.Subsurface_Node.Gsk_Subsurface_Node;
      Function_Child : Gsk.Render_Node.Gsk_Render_Node;
      Procedure_Child : Gsk.Render_Node.Gsk_Render_Node;
      Reinitialized_Child : Gsk.Render_Node.Gsk_Render_Node;
      Wrapped_Node : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Snapshot.Append_Color (Color, Bounds'Access);
      Child := Snapshot.To_Node;
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Child));
      Replacement_Snapshot.Append_Color (Replacement_Color, Bounds'Access);
      Replacement_Child := Replacement_Snapshot.To_Node;
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Replacement_Child));

      --  NULL is documented for a missing subsurface and avoids fabricating
      --  GTK's private GdkSubsurface object in this headless unit test.
      Function_Node := Gsk.Subsurface_Node.Gsk_Subsurface_Node_New
        (Child, System.Null_Address);
      Gsk.Subsurface_Node.Gtk_New
        (Procedure_Node, Child, System.Null_Address);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Function_Node));
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Procedure_Node));

      --  Both constructors retain the supplied child and opaque pointer.
      Function_Child := Gsk.Subsurface_Node.Get_Child (Function_Node);
      Procedure_Child := Gsk.Subsurface_Node.Get_Child (Procedure_Node);
      Assert_True
        (Gsk.Render_Node.Get_Object (Function_Child) =
         Gsk.Render_Node.Get_Object (Child));
      Assert_True
        (Gsk.Render_Node.Get_Object (Procedure_Child) =
         Gsk.Render_Node.Get_Object (Child));
      Assert_True
        (Gsk.Subsurface_Node.Get_Subsurface (Procedure_Node) =
         System.Null_Address);

      --  Reinitialization is a no-op; use a different child to check that
      --  the original borrowed child remains attached.
      Gsk.Subsurface_Node.Initialize
        (Procedure_Node, Replacement_Child, System.Null_Address);
      Reinitialized_Child := Gsk.Subsurface_Node.Get_Child (Procedure_Node);
      Assert_True
        (Gsk.Render_Node.Get_Object (Reinitialized_Child) =
         Gsk.Render_Node.Get_Object (Child));

      --  Rewrapping the raw pointer dispatches to the concrete Ada node type
      --  and runs its generated Create function.
      Wrapped_Node := Gsk.Render_Node.From_Object_None_Ownership
        (Gsk.Render_Node.Get_Object (Function_Node));
      Assert_True
        (Gsk.Render_Node.Get_Object (Wrapped_Node) =
         Gsk.Render_Node.Get_Object (Function_Node));

      --  Release getter wrappers before their source nodes and shared child.
      Free_Node (Function_Child);
      Free_Node (Procedure_Child);
      Free_Node (Reinitialized_Child);
      Free_Node (Wrapped_Node);
      Free_Subsurface_Node (Procedure_Node);
      Free_Subsurface_Node (Function_Node);
      Free_Node (Replacement_Child);
      Free_Node (Child);
      Glib.Object.Unref (Glib.Object.GObject (Snapshot));
      Glib.Object.Unref (Glib.Object.GObject (Replacement_Snapshot));
   end Test_Subsurface_Node;

begin
   Glib.Test.Init;
   Add_Func
     ("/gsk/subsurface-node", Test_Subsurface_Node'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
