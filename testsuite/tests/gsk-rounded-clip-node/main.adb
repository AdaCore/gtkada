--  Exercise rounded clip node constructors, getters, and render-node dispatch.
--  There is no matching focused C test in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.RGBA;
with Glib.Bytes;
with Glib.Object;
with Glib.Test;
with Graphene.Rect;
with Gsk.Enums;
with Gsk.Render_Node;
with Gsk.Rounded_Clip_Node;
with Gsk.Rounded_Rect;
with Gtk.Main;
with Gtk.Snapshot;
with System;

procedure Main is
   use Glib.Test;
   use type Glib.GType;
   use type Glib.Gsize;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type System.Address;

   --  Render-node wrappers own their C reference and release it when freed.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Constructors_And_Getters with Convention => C;
   procedure Test_Deserialization_Create with Convention => C;

   --  Compare bounds and a corner radius to verify the returned clip value.
   procedure Check_Clip
     (Node : Gsk.Rounded_Clip_Node.Gsk_Rounded_Clip_Node;
      Expected : Gsk.Rounded_Rect.Gsk_Rounded_Rect)
   is
      Actual : constant Gsk.Rounded_Rect.Gsk_Rounded_Rect :=
        Gsk.Rounded_Clip_Node.Get_Clip (Node);
   begin
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Bounds.Origin.X),
         Glib.Gdouble (Expected.Bounds.Origin.X), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Bounds.Origin.Y),
         Glib.Gdouble (Expected.Bounds.Origin.Y), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Bounds.Size.Width),
         Glib.Gdouble (Expected.Bounds.Size.Width), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Bounds.Size.Height),
         Glib.Gdouble (Expected.Bounds.Size.Height), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Corner (1).Width),
         Glib.Gdouble (Expected.Corner (1).Width), 0.001);
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual.Corner (1).Height),
         Glib.Gdouble (Expected.Corner (1).Height), 0.001);
   end Check_Clip;

   --  Both constructor styles initialize a fresh wrapper; a later Initialize
   --  call must leave the already-created immutable node unchanged.
   procedure Test_Constructors_And_Getters is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 8.0, Y => 12.0), (Width => 96.0, Height => 48.0));
      Other_Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 1.0, Y => 2.0), (Width => 3.0, Height => 4.0));
      Clip : aliased Gsk.Rounded_Rect.Gsk_Rounded_Rect;
      Other_Clip : aliased Gsk.Rounded_Rect.Gsk_Rounded_Rect;
      Color : constant Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.6, 1.0);
      Snapshot : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Child : Gsk.Render_Node.Gsk_Render_Node;
      By_Function : Gsk.Rounded_Clip_Node.Gsk_Rounded_Clip_Node;
      By_Procedure : Gsk.Rounded_Clip_Node.Gsk_Rounded_Clip_Node;
      Function_Child : Gsk.Render_Node.Gsk_Render_Node;
      Procedure_Child : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Clip := Gsk.Rounded_Rect.Init_From_Rect
        (Clip'Access, Bounds'Access, 6.0).all;
      Other_Clip := Gsk.Rounded_Rect.Init_From_Rect
        (Other_Clip'Access, Other_Bounds'Access, 1.0).all;

      --  Init_From_Rect writes through access parameters; the node constructors
      --  then receive the normalized rounded rectangles by value.
      --  A snapshot creates a valid render child without opening a window.
      Snapshot.Append_Color (Color, Bounds'Access);
      Child := Snapshot.To_Node;
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Child));

      By_Function := Gsk.Rounded_Clip_Node.Gsk_Rounded_Clip_Node_New
        (Child, Clip);
      Gsk.Rounded_Clip_Node.Gtk_New
        (By_Procedure, Child, Clip);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (By_Function));
      Assert_Nonnull (Gsk.Render_Node.Get_Object (By_Procedure));
      --  The concrete node kind confirms that GTK dispatched the right type.
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (By_Function)) =
         Gsk.Enums.Gsk_Rounded_Clip_Node);
      Assert_True
        (Gsk.Rounded_Clip_Node.Get_Type /= Gsk.Render_Node.Get_Type);

      --  Passing different data confirms Initialize's created-object guard.
      Gsk.Rounded_Clip_Node.Initialize
        (By_Procedure, Child, Other_Clip);
      --  Get_Clip returns a record copy, so callers do not own a C allocation.
      Check_Clip (By_Function, Clip);
      Check_Clip (By_Procedure, Clip);

      --  The borrowed C child is wrapped with its own reference; compare object
      --  identity and free each returned wrapper below.
      Function_Child := Gsk.Rounded_Clip_Node.Get_Child (By_Function);
      Procedure_Child := Gsk.Rounded_Clip_Node.Get_Child (By_Procedure);
      Assert_True
        (Gsk.Render_Node.Get_Object (Function_Child) =
         Gsk.Render_Node.Get_Object (Child));
      Assert_True
        (Gsk.Render_Node.Get_Object (Procedure_Child) =
         Gsk.Render_Node.Get_Object (Child));

      --  Each getter wrapper owns a reference independent of its parent node.
      Free_Node (Function_Child);
      Free_Node (Procedure_Child);
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (By_Procedure));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (By_Function));
      Free_Node (Child);
      Glib.Object.Unref (Glib.Object.GObject (Snapshot));
   end Test_Constructors_And_Getters;

   --  Deserialization routes through the concrete Create override for this
   --  node type, then the getters confirm the restored node's data.
   procedure Test_Deserialization_Create is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 3.0, Y => 5.0), (Width => 30.0, Height => 40.0));
      Clip : aliased Gsk.Rounded_Rect.Gsk_Rounded_Rect;
      Color : constant Gdk.RGBA.Gdk_RGBA := (0.35, 0.55, 0.15, 0.65);
      Snapshot : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Child : Gsk.Render_Node.Gsk_Render_Node;
      Node : Gsk.Rounded_Clip_Node.Gsk_Rounded_Clip_Node;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Clip : Gsk.Rounded_Clip_Node.Gsk_Rounded_Clip_Node;
   begin
      Clip := Gsk.Rounded_Rect.Init_From_Rect
        (Clip'Access, Bounds'Access, 3.0).all;
      Snapshot.Append_Color (Color, Bounds'Access);
      Child := Snapshot.To_Node;
      Node := Gsk.Rounded_Clip_Node.Gsk_Rounded_Clip_Node_New
        (Child, Clip);

      --  Recreate from the serialized bytes to reach the rounded-node factory.
      --  Serialize in memory so this test uses no files or user settings.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Node));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) =
         Gsk.Enums.Gsk_Rounded_Clip_Node);
      Restored_Clip := Gsk.Rounded_Clip_Node.Gsk_Rounded_Clip_Node
        (Restored);
      Check_Clip (Restored_Clip, Clip);

      --  Release the deserialized and original nodes before their inputs.
      Free_Node (Restored);
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (Node));
      Free_Node (Child);
      Glib.Bytes.Unref (Serialized);
      Glib.Object.Unref (Glib.Object.GObject (Snapshot));
   end Test_Deserialization_Create;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gsk/rounded-clip-node/constructors-getters",
      Test_Constructors_And_Getters'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/rounded-clip-node/deserialization-create",
      Test_Deserialization_Create'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
