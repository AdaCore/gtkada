--  Exercises Gsk.Clip_Node constructors, immutable getters, and the
--  render-node dispatch factory through serialization and deserialization.
--  There is no focused C testsuite case for gsk_clip_node to port.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Glib;
with Glib.Bytes;
with Glib.Object;
with Glib.Test;
with Gdk.RGBA;
with Graphene.Rect;
with Gsk.Clip_Node;
with Gsk.Enums;
with Gsk.Render_Node;
with Gtk.Snapshot;
with System;

procedure Main is
   use Glib.Test;
   use type Glib.GType;
   use type Glib.Gsize;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type System.Address;

   --  Render-node wrappers own their C references; deallocation finalizes
   --  each wrapper and releases exactly the reference it owns.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Clip_Node with Convention => C;

   --  The snapshot supplies a valid child without creating a window or
   --  requiring a renderer. Both constructor styles then receive equal data.
   procedure Test_Clip_Node is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 40.0, Height => 30.0));
      Clip : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 5.0, Y => 7.0), (Width => 20.0, Height => 12.0));
      Other_Clip : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 1.0, Y => 2.0), (Width => 3.0, Height => 4.0));
      Color : constant Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.6, 1.0);
      Snapshot : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Child : Gsk.Render_Node.Gsk_Render_Node;
      By_Function : Gsk.Clip_Node.Gsk_Clip_Node;
      By_Procedure : Gsk.Clip_Node.Gsk_Clip_Node;
      Function_Child : Gsk.Render_Node.Gsk_Render_Node;
      Procedure_Child : Gsk.Render_Node.Gsk_Render_Node;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Clip : Gsk.Clip_Node.Gsk_Clip_Node;
   begin
      Snapshot.Append_Color (Color, Bounds'Access);
      Child := Snapshot.To_Node;
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Child));

      By_Function := Gsk.Clip_Node.Gsk_Clip_Node_New
        (Child, Clip'Access);
      Gsk.Clip_Node.Gtk_New (By_Procedure, Child, Clip'Access);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (By_Function));
      Assert_Nonnull (Gsk.Render_Node.Get_Object (By_Procedure));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (By_Function)) =
         Gsk.Enums.Gsk_Clip_Node);
      Assert_True (Gsk.Clip_Node.Get_Type /= Gsk.Render_Node.Get_Type);

      --  Initialize is documented as a no-op after construction; checking
      --  the original clip verifies its already-created guard.
      Gsk.Clip_Node.Initialize (By_Procedure, Child, Other_Clip'Access);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Clip_Node.Get_Clip (By_Procedure).Origin.X),
         5.0, 0.0001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Clip_Node.Get_Clip (By_Procedure).Size.Height),
         12.0, 0.0001);

      --  The child getter returns a borrowed C node wrapped with a new Ada
      --  reference, so compare its object identity and later free that wrapper.
      Function_Child := Gsk.Clip_Node.Get_Child (By_Function);
      Procedure_Child := Gsk.Clip_Node.Get_Child (By_Procedure);
      Assert_True
        (Gsk.Render_Node.Get_Object (Function_Child) =
         Gsk.Render_Node.Get_Object (Child));
      Assert_True
        (Gsk.Render_Node.Get_Object (Procedure_Child) =
         Gsk.Render_Node.Get_Object (Child));

      --  Deserialization uses the dispatching constructor for the concrete
      --  node type, exercising Clip_Node.Create as well as its accessors.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (By_Function));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) = Gsk.Enums.Gsk_Clip_Node);
      Restored_Clip := Gsk.Clip_Node.Gsk_Clip_Node (Restored);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Gsk.Clip_Node.Get_Clip (Restored_Clip).Origin.Y),
         7.0, 0.0001);

      Free_Node (Function_Child);
      Free_Node (Procedure_Child);
      Free_Node (Restored);
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (By_Procedure));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (By_Function));
      Free_Node (Child);
      Glib.Bytes.Unref (Serialized);
      Glib.Object.Unref (Glib.Object.GObject (Snapshot));
   end Test_Clip_Node;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/gsk/clip-node", Test_Clip_Node'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
