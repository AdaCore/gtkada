--  Exercises Gsk.Repeat_Node constructors, getters, and immutable
--  initialization. No focused C testsuite case covers gsk_repeat_node.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib;
with Glib.Bytes;
with Glib.Object;
with Glib.Test; use Glib.Test;
with Graphene.Rect;
with Gsk.Enums;
with Gsk.Repeat_Node;
with Gsk.Render_Node;
with Gtk.Snapshot;
with System;

procedure Main is
   use type Glib.GType;
   use type Glib.Gsize;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type System.Address;

   --  Render-node wrappers own references, including getter results.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Repeat_Node with Convention => C;

   --  Compare every rectangle component so the test checks stored bounds,
   --  rather than only that the getter returned a non-null pointer.
   procedure Assert_Bounds
     (Actual   : access constant Graphene.Rect.Graphene_Rect_T;
      Expected : Graphene.Rect.Graphene_Rect_T)
   is
   begin
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Origin.X), Glib.Gdouble (Expected.Origin.X), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Origin.Y), Glib.Gdouble (Expected.Origin.Y), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Size.Width), Glib.Gdouble (Expected.Size.Width), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Size.Height), Glib.Gdouble (Expected.Size.Height), 0.001);
   end Assert_Bounds;

   --  A snapshot color node supplies a deterministic child without requiring
   --  a window, renderer, or display.
   procedure Test_Repeat_Node is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 1.0, Y => 2.0), (Width => 48.0, Height => 32.0));
      Child_Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 3.0, Y => 4.0), (Width => 12.0, Height => 10.0));
      Replacement_Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 20.0, Y => 21.0), (Width => 5.0, Height => 6.0));
      Color : constant Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.6, 1.0);
      Snapshot : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Child : Gsk.Render_Node.Gsk_Render_Node;
      By_Function : Gsk.Repeat_Node.Gsk_Repeat_Node;
      By_Procedure : Gsk.Repeat_Node.Gsk_Repeat_Node;
      Function_Child : Gsk.Render_Node.Gsk_Render_Node;
      Procedure_Child : Gsk.Render_Node.Gsk_Render_Node;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Repeat : Gsk.Repeat_Node.Gsk_Repeat_Node;
      Restored_Child : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Snapshot.Append_Color (Color, Bounds'Access);
      Child := Snapshot.To_Node;
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Child));

      --  Both constructor forms store their bounds and retain the child.
      By_Function := Gsk.Repeat_Node.Gsk_Repeat_Node_New
        (Bounds'Access, Child, Child_Bounds'Access);
      Gsk.Repeat_Node.Gtk_New (By_Procedure, Bounds'Access, Child, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (By_Function));
      Assert_Nonnull (Gsk.Render_Node.Get_Object (By_Procedure));
      --  The node type confirms each constructor produced the requested GSK node.
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (By_Function)) =
         Gsk.Enums.Gsk_Repeat_Node);
      Assert_True (Gsk.Repeat_Node.Get_Type /= Gsk.Render_Node.Get_Type);
      Assert_Bounds (Gsk.Repeat_Node.Get_Child_Bounds (By_Function), Child_Bounds);
      --  A null Child_Bounds requests the bounds of the child node itself.
      Assert_Bounds (Gsk.Repeat_Node.Get_Child_Bounds (By_Procedure), Bounds);

      --  Initialize leaves an already-created immutable node unchanged.
      Gsk.Repeat_Node.Initialize
        (By_Procedure, Replacement_Bounds'Access, Child,
         Child_Bounds'Access);
      Assert_Bounds (Gsk.Repeat_Node.Get_Child_Bounds (By_Procedure), Bounds);

      --  Get_Child wraps the borrowed C pointer with an owned Ada reference.
      Function_Child := Gsk.Repeat_Node.Get_Child (By_Function);
      Procedure_Child := Gsk.Repeat_Node.Get_Child (By_Procedure);
      Assert_True
        (Gsk.Render_Node.Get_Object (Function_Child) =
         Gsk.Render_Node.Get_Object (Child));
      Assert_True
        (Gsk.Render_Node.Get_Object (Procedure_Child) =
         Gsk.Render_Node.Get_Object (Child));

      --  Deserialization routes the node through its generated Create hook.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (By_Function));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) = Gsk.Enums.Gsk_Repeat_Node);
      Restored_Repeat := Gsk.Repeat_Node.Gsk_Repeat_Node (Restored);
      Assert_Bounds
        (Gsk.Repeat_Node.Get_Child_Bounds (Restored_Repeat), Child_Bounds);
      Restored_Child := Gsk.Repeat_Node.Get_Child (Restored_Repeat);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored_Child) = Gsk.Enums.Gsk_Color_Node);

      Free_Node (Restored_Child);
      Free_Node (Restored);
      Free_Node (Function_Child);
      Free_Node (Procedure_Child);
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (By_Procedure));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (By_Function));
      Free_Node (Child);
      Glib.Bytes.Unref (Serialized);
      Glib.Object.Unref (Glib.Object.GObject (Snapshot));
   end Test_Repeat_Node;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/gsk/repeat-node", Test_Repeat_Node'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
