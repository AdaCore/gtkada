--  Exercises the Phase 1 Gtk.Snapshot binding: build a snapshot, drive the
--  transform stack and a clip, append a solid colour, and turn the result
--  into a paintable. The point is to prove the generated package links and
--  runs, not to inspect the produced render node (which needs Gsk).

with Glib;          use Glib;
with Glib.Object;   use Glib.Object;
with Glib.Test;     use Glib.Test;
with Ada.Command_Line;
with System;
with Gdk.Paintable; use Gdk.Paintable;
with Gdk.RGBA;      use Gdk.RGBA;
with Gtk.Main;
with Gtk.Snapshot;  use Gtk.Snapshot;
with Gtkada.Types;  use Gtkada.Types;

procedure Snapshot is

   procedure Test_Paint
   with Convention => C;

   procedure Test_Paint is
      Snap      : constant Gtk_Snapshot := Gtk_Snapshot_New;
      Color     : constant Gdk_RGBA :=
        (Red => 0.20, Green => 0.50, Blue => 0.85, Alpha => 1.0);
      Bounds    : graphene_rect_t :=
        (origin => (x => 0.0, y => 0.0),
         size   => (width => 100.0, height => 100.0));
      Point     : graphene_point_t := (x => 10.0, y => 20.0);
      Size      : graphene_size_t  := (width => 100.0, height => 100.0);
      Paintable : Gdk_Paintable;
   begin
      Assert_Nonnull (Get_Object (Snap));

      --  Transform stack.
      Snap.Save;
      Snap.Translate (Point);
      Snap.Scale (2.0, 2.0);
      Snap.Rotate (45.0);

      --  A clip push must be balanced by a pop.
      Snap.Push_Clip (Bounds);
      Snap.Append_Color (Color, Bounds);
      Snap.Pop;

      Snap.Restore;

      --  Consumes the snapshot and hands back a paintable.
      Paintable := Snap.To_Paintable (Size);
      Assert_Nonnull (System.Address (Paintable));
   end Test_Paint;

begin
   Glib.Test.Init;

   --  Widgets and snapshots cannot be created before GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/snapshot/paint", Test_Paint'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Snapshot;
