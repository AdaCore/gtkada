--  Exercises Gsk.Opacity_Node constructors, immutable getters, the no-op
--  Initialize path, and render-node factory dispatch through deserialization.
--  No focused C testsuite case covers gsk_opacity_node.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib;
with Glib.Bytes;
with Glib.Object;
with Glib.Test;
with Graphene.Rect;
with Interfaces.C;
with Gsk.Enums;
with Gsk.Opacity_Node;
with Gsk.Render_Node;
with Gtk.Snapshot;
with System;

procedure Main is
   use Glib.Test;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Glib.GType;
   use type Glib.Gsize;
   use type System.Address;

   --  Each render-node wrapper owns a reference; deallocation runs its
   --  controlled finalizer and releases that reference.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Opacity_Node with Convention => C;

   --  A snapshot color node is a deterministic child and needs no window,
   --  display interaction, or renderer.
   procedure Test_Opacity_Node is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 24.0, Height => 18.0));
      Color : constant Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.6, 1.0);
      Snapshot : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Child : Gsk.Render_Node.Gsk_Render_Node;
      By_Function : Gsk.Opacity_Node.Gsk_Opacity_Node;
      By_Procedure : Gsk.Opacity_Node.Gsk_Opacity_Node;
      Function_Child : Gsk.Render_Node.Gsk_Render_Node;
      Procedure_Child : Gsk.Render_Node.Gsk_Render_Node;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Opacity : Gsk.Opacity_Node.Gsk_Opacity_Node;
      Restored_Child : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Snapshot.Append_Color (Color, Bounds'Access);
      Child := Snapshot.To_Node;
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Child));

      --  Both constructor forms retain the supplied child and opacity.
      By_Function := Gsk.Opacity_Node.Gsk_Opacity_Node_New
        (Child, Interfaces.C.C_float (0.35));
      Gsk.Opacity_Node.Gtk_New
        (By_Procedure, Child, Interfaces.C.C_float (0.65));
      Assert_Nonnull (Gsk.Render_Node.Get_Object (By_Function));
      Assert_Nonnull (Gsk.Render_Node.Get_Object (By_Procedure));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (By_Function)) =
         Gsk.Enums.Gsk_Opacity_Node);
      Assert_True
        (Gsk.Opacity_Node.Get_Type /= Gsk.Render_Node.Get_Type);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Opacity_Node.Get_Opacity (By_Function)),
         0.35, 0.0001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Opacity_Node.Get_Opacity (By_Procedure)),
         0.65, 0.0001);

      --  Initialize must leave an already-created immutable node unchanged.
      Gsk.Opacity_Node.Initialize
        (By_Procedure, Child, Interfaces.C.C_float (0.9));
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Opacity_Node.Get_Opacity (By_Procedure)),
         0.65, 0.0001);

      --  Get_Child returns the same node, wrapped with an owned reference.
      Function_Child := Gsk.Opacity_Node.Get_Child (By_Function);
      Procedure_Child := Gsk.Opacity_Node.Get_Child (By_Procedure);
      Assert_True
        (Gsk.Render_Node.Get_Object (Function_Child) =
         Gsk.Render_Node.Get_Object (Child));
      Assert_True
        (Gsk.Render_Node.Get_Object (Procedure_Child) =
         Gsk.Render_Node.Get_Object (Child));

      --  Deserialization dispatches through Opacity_Node.Create and restores
      --  the node's concrete type and stored properties.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (By_Function));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) =
         Gsk.Enums.Gsk_Opacity_Node);
      Restored_Opacity := Gsk.Opacity_Node.Gsk_Opacity_Node (Restored);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Opacity_Node.Get_Opacity (Restored_Opacity)),
         0.35, 0.0001);
      Restored_Child := Gsk.Opacity_Node.Get_Child (Restored_Opacity);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored_Child) =
         Gsk.Enums.Gsk_Color_Node);

      Free_Node (Restored_Child);
      Free_Node (Restored);
      Free_Node (Function_Child);
      Free_Node (Procedure_Child);
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (By_Procedure));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (By_Function));
      Free_Node (Child);
      Glib.Bytes.Unref (Serialized);
      Glib.Object.Unref (Glib.Object.GObject (Snapshot));
   end Test_Opacity_Node;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gsk/opacity-node", Test_Opacity_Node'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
