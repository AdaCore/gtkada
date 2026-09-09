--  Exercises the Gtk.Snapshot binding: build a snapshot, drive the transform
--  stack and a clip, append a solid colour, and turn the result into a
--  paintable. A second test drives the four Graphene-dependent entry points
--  (translate_3d, rotate_3d, transform_matrix, push_color_matrix). The point
--  is to prove the generated package links and runs -- in particular that the
--  Graphene records marshal with the right layout -- not to inspect the
--  produced render node (which needs Gsk).

with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Glib.Test;        use Glib.Test;
with Ada.Command_Line;
with System;
with Gdk.Paintable;    use Gdk.Paintable;
with Gdk.RGBA;         use Gdk.RGBA;
with Gtk.Main;
with Gtk.Snapshot;     use Gtk.Snapshot;
with Graphene.Rect;    use Graphene.Rect;
with Graphene.Point;   use Graphene.Point;
with Graphene.Size;    use Graphene.Size;

--  Graphene.Matrix, Graphene.Vec3 and Graphene.Vec4 all export an Init
--  family, Equal, Free and Get_X, so they are deliberately left without a
--  `use` clause: the calls below are qualified instead.
with Graphene.Matrix;
with Graphene.Point3d;
with Graphene.Vec3;
with Graphene.Vec4;

procedure Snapshot is

   procedure Test_Paint
   with Convention => C;

   procedure Test_Transform_3D
   with Convention => C;

   procedure Test_Paint is
      Snap      : constant Gtk_Snapshot := Gtk_Snapshot_New;
      Color     : constant Gdk_RGBA :=
        (Red => 0.20, Green => 0.50, Blue => 0.85, Alpha => 1.0);
      Bounds    : aliased Graphene.Rect.Graphene_Rect_T :=
        (origin => (x => 0.0, y => 0.0),
         size   => (width => 100.0, height => 100.0));
      Point     : aliased Graphene.Point.Graphene_Point_T :=
        (x => 10.0, y => 20.0);
      Size      : aliased Graphene.Size.Graphene_Size_T  :=
        (width => 100.0, height => 100.0);
      Paintable : Gdk_Paintable;
   begin
      Assert_Nonnull (Get_Object (Snap));

      --  Transform stack.
      Snap.Save;
      Snap.Translate (Point'Access);
      Snap.Scale (2.0, 2.0);
      Snap.Rotate (45.0);

      --  A clip push must be balanced by a pop.
      Snap.Push_Clip (Bounds'Access);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;

      Snap.Restore;

      --  Consumes the snapshot and hands back a paintable.
      Paintable := Snap.To_Paintable (Size'Access);
      Assert_Nonnull (System.Address (Paintable));
   end Test_Paint;

   procedure Test_Transform_3D is
      Snap   : constant Gtk_Snapshot := Gtk_Snapshot_New;
      Color  : constant Gdk_RGBA :=
        (Red => 0.20, Green => 0.50, Blue => 0.85, Alpha => 1.0);
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        (origin => (x => 0.0, y => 0.0),
         size   => (width => 100.0, height => 100.0));
      Point  : aliased Graphene.Point3d.Graphene_Point3D_T :=
        (X => 10.0, Y => 20.0, Z => 30.0);

      --  Unlike the records above, these three are opaque SIMD wrappers and
      --  cannot be built with an aggregate; they must be filled in by their
      --  Init functions.
      Axis   : aliased Graphene.Vec3.Graphene_Vec3_T;
      Matrix : aliased Graphene.Matrix.Graphene_Matrix_T;
      Offset : aliased Graphene.Vec4.Graphene_Vec4_T;
   begin
      Assert_Nonnull (Get_Object (Snap));

      declare
         --  The Init family has no procedure form: each returns the access it
         --  was handed, which is of no use here.
         Ignored_Axis   : constant access Graphene.Vec3.Graphene_Vec3_T :=
           Graphene.Vec3.Init (Axis'Access, 0.0, 0.0, 1.0);
         Ignored_Matrix : constant access Graphene.Matrix.Graphene_Matrix_T :=
           Graphene.Matrix.Init_Identity (Matrix'Access);
         Ignored_Offset : constant access Graphene.Vec4.Graphene_Vec4_T :=
           Graphene.Vec4.Init (Offset'Access, 0.0, 0.0, 0.0, 0.0);
         pragma Unreferenced (Ignored_Axis, Ignored_Matrix, Ignored_Offset);
      begin
         --  A wrong record layout shows up here, before GTK ever sees it.
         Assert_True (Graphene.Matrix.Is_Identity (Matrix'Access));
         Assert_Cmpfloat_With_Epsilon
           (Gdouble (Graphene.Vec3.Get_Z (Axis'Access)), 1.0, 0.0001);
      end;

      Snap.Save;
      Snap.Translate_3D (Point'Access);
      Snap.Rotate_3D (45.0, Axis'Access);
      Snap.Transform_Matrix (Matrix'Access);

      --  A colour-matrix push must be balanced by a pop, and wants a
      --  non-empty child node underneath it.
      Snap.Push_Color_Matrix (Matrix'Access, Offset'Access);
      Snap.Append_Color (Color, Bounds'Access);
      Snap.Pop;

      Snap.Restore;
   end Test_Transform_3D;

begin
   Glib.Test.Init;

   --  Widgets and snapshots cannot be created before GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/snapshot/paint", Test_Paint'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/snapshot/transform-3d", Test_Transform_3D'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Snapshot;
