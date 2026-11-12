--  Exercises Gsk.Isolation_Node construction, initialization, getters, and
--  render-node factory dispatch. The C testsuite has no focused isolation
--  node case to port, so the assertions check the documented Ada binding.
with Ada.Command_Line;
with Ada.Unchecked_Deallocation;

with Gdk.RGBA;
with Glib.Bytes;
with Glib.Object;
with Glib.Test; use Glib.Test;
with Graphene.Rect;
with Gsk.Enums;
with Gsk.Isolation_Node;
with Gsk.Render_Node;
with Gtk.Snapshot;
with System;

procedure Main is
   use type Glib.Gsize;
   use type Gsk.Enums.Gsk_Isolation;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type System.Address;

   --  Render-node wrappers release their owned GSK references when freed.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Isolation_Node with Convention => C;

   --  Both constructors wrap the same immutable child and retain the flags.
   --  Serialization also exercises the node's Ada Create factory override.
   procedure Test_Isolation_Node is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 28.0, Height => 19.0));
      Color : constant Gdk.RGBA.Gdk_RGBA := (0.3, 0.5, 0.7, 1.0);
      Snapshot : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Child : Gsk.Render_Node.Gsk_Render_Node;
      By_Function : Gsk.Isolation_Node.Gsk_Isolation_Node;
      By_Procedure : Gsk.Isolation_Node.Gsk_Isolation_Node;
      Function_Child : Gsk.Render_Node.Gsk_Render_Node;
      Procedure_Child : Gsk.Render_Node.Gsk_Render_Node;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Child : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Isolation : Gsk.Isolation_Node.Gsk_Isolation_Node;
      Serialized : Glib.Bytes.Gbytes;
      Flags : constant Gsk.Enums.Gsk_Isolation :=
        Gsk.Enums.Gsk_Isolation_Background or
        Gsk.Enums.Gsk_Isolation_Copy_Paste;
   begin
      --  A snapshot creates a deterministic child without a window or renderer.
      Snapshot.Append_Color (Color, Bounds'Access);
      Child := Snapshot.To_Node;
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Child));

      --  Exercise both public constructors and check their concrete node type.
      By_Function := Gsk.Isolation_Node.Gsk_Isolation_Node_New (Child, Flags);
      Gsk.Isolation_Node.Gtk_New (By_Procedure, Child, Flags);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (By_Function));
      Assert_Nonnull (Gsk.Render_Node.Get_Object (By_Procedure));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (By_Function)) =
         Gsk.Enums.Gsk_Isolation_Node);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (By_Procedure)) =
         Gsk.Enums.Gsk_Isolation_Node);

      --  Initialize on an existing node is documented as a no-op.
      Gsk.Isolation_Node.Initialize
        (By_Procedure, Child, Gsk.Enums.Gsk_Isolation_None);
      Assert_True
        (Gsk.Isolation_Node.Get_Isolations (By_Procedure) = Flags);
      Assert_True
        (Gsk.Isolation_Node.Get_Isolations (By_Function) = Flags);

      --  The child getter returns a wrapper for the same underlying render node.
      Function_Child := Gsk.Isolation_Node.Get_Child (By_Function);
      Procedure_Child := Gsk.Isolation_Node.Get_Child (By_Procedure);
      Assert_True
        (Gsk.Render_Node.Get_Object (Function_Child) =
         Gsk.Render_Node.Get_Object (Child));
      Assert_True
        (Gsk.Render_Node.Get_Object (Procedure_Child) =
         Gsk.Render_Node.Get_Object (Child));

      --  Deserialization rebuilds the typed Ada wrapper through Create.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (By_Function));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) =
         Gsk.Enums.Gsk_Isolation_Node);
      Restored_Isolation := Gsk.Isolation_Node.Gsk_Isolation_Node (Restored);
      Assert_True
        (Gsk.Isolation_Node.Get_Isolations (Restored_Isolation) = Flags);
      Restored_Child := Gsk.Isolation_Node.Get_Child (Restored_Isolation);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored_Child) =
         Gsk.Enums.Gsk_Color_Node);

      --  The getter-created wrappers each own a reference and are freed too.
      Free_Node (Restored_Child);
      Free_Node (Restored);
      Free_Node (Function_Child);
      Free_Node (Procedure_Child);
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (By_Procedure));
      Free_Node (Gsk.Render_Node.Gsk_Render_Node (By_Function));
      Free_Node (Child);
      Glib.Bytes.Unref (Serialized);
      Glib.Object.Unref (Glib.Object.GObject (Snapshot));
   end Test_Isolation_Node;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gsk/isolation-node/constructors-getters-serialization",
      Test_Isolation_Node'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
