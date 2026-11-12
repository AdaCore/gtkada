--  Exercises the Gsk.Debug_Node constructors, getters, already-created
--  Initialize behavior, and render-node factory dispatch. There is no
--  dedicated GTK C testsuite case for gsk_debug_node to port.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with System;

with Glib;
with Glib.Bytes;
with Glib.Object;
with Glib.Test; use Glib.Test;
with Gdk.RGBA;
with Graphene.Rect;
with Gsk.Debug_Node;
with Gsk.Enums;
with Gsk.Render_Node;
with Gtk.Snapshot;

procedure Main is
   use type Glib.GType;
   use type Glib.Gsize;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type System.Address;

   --  Render-node wrappers own their C references and release them when
   --  deallocated; this includes the borrowed child wrappers returned below.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Debug_Node with Convention => C;

   --  A color node made by a snapshot is a deterministic child and needs no
   --  window, display interaction, or renderer.
   procedure Test_Debug_Node is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 24.0, Height => 18.0));
      Color : constant Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.6, 1.0);
      Snapshot : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Child : Gsk.Render_Node.Gsk_Render_Node;
      By_Function : Gsk.Debug_Node.Gsk_Debug_Node;
      By_Procedure : Gsk.Debug_Node.Gsk_Debug_Node;
      Function_Child : Gsk.Render_Node.Gsk_Render_Node;
      Procedure_Child : Gsk.Render_Node.Gsk_Render_Node;
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Debug : Gsk.Debug_Node.Gsk_Debug_Node;
      Restored_Child : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Snapshot.Append_Color (Color, Bounds'Access);
      Child := Snapshot.To_Node;
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Child));

      --  Check both Ada constructors and ensure their results are concrete
      --  debug nodes rather than plain render-node wrappers.
      By_Function := Gsk.Debug_Node.Gsk_Debug_Node_New (Child, "function path");
      Gsk.Debug_Node.Gtk_New (By_Procedure, Child, "procedure path");
      Assert_Nonnull (Gsk.Render_Node.Get_Object (By_Function));
      Assert_Nonnull (Gsk.Render_Node.Get_Object (By_Procedure));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (By_Function)) =
         Gsk.Enums.Gsk_Debug_Node);
      Assert_True (Gsk.Debug_Node.Get_Type /= Gsk.Render_Node.Get_Type);

      --  Initialize is a no-op for a node that already owns a C object.
      Gsk.Debug_Node.Initialize (By_Procedure, Child, "replacement message");
      Assert_True (Gsk.Debug_Node.Get_Message (By_Procedure) = "procedure path");

      --  The getter returns a borrowed child, wrapped with its own C reference.
      Function_Child := Gsk.Debug_Node.Get_Child (By_Function);
      Procedure_Child := Gsk.Debug_Node.Get_Child (By_Procedure);
      Assert_True
        (Gsk.Render_Node.Get_Object (Function_Child) =
         Gsk.Render_Node.Get_Object (Child));
      Assert_True
        (Gsk.Render_Node.Get_Object (Procedure_Child) =
         Gsk.Render_Node.Get_Object (Child));
      Assert_True (Gsk.Debug_Node.Get_Message (By_Function) = "function path");

      --  Deserializing recreates the concrete Ada node through its overriding
      --  Create function and verifies that its message and child survive.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (By_Function));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) = Gsk.Enums.Gsk_Debug_Node);
      Restored_Debug := Gsk.Debug_Node.Gsk_Debug_Node (Restored);
      Assert_True
        (Gsk.Debug_Node.Get_Message (Restored_Debug) = "function path");
      Restored_Child := Gsk.Debug_Node.Get_Child (Restored_Debug);
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
   end Test_Debug_Node;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/gsk/debug-node", Test_Debug_Node'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
