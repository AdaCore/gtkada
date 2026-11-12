--  Exercise Gsk.Rounded_Rect's point and rectangle predicates, rectilinear
--  query, and GtkAda copy-and-free helper. No matching C test exists.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;
with System;

with Glib.Test; use Glib.Test;
with Graphene.Point;
with Graphene.Rect;
with Gsk.Rounded_Rect; use Gsk.Rounded_Rect;

procedure Main is
   use type Interfaces.C.C_float;

   --  From_Object_Free releases storage with g_free, so allocate its input
   --  with GLib's allocator and view that storage as the rounded-rect record.
   type Rounded_Rect_Access is access all Gsk_Rounded_Rect;
   function To_Rounded_Rect_Access is new Ada.Unchecked_Conversion
     (System.Address, Rounded_Rect_Access);

   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Near (Actual, Expected : C_float) is
   begin
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual), Glib.Gdouble (Expected), 0.001);
   end Near;

   --  The rounded corner excludes its outermost square, while the center is
   --  included and a point beyond the bounds is excluded.
   procedure Test_Contains_Point with Convention => C;
   procedure Test_Contains_Point is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 100.0, Height => 80.0));
      Rounded : aliased Gsk_Rounded_Rect;
      Center : aliased Graphene.Point.Graphene_Point_T := (50.0, 40.0);
      Cutout : aliased Graphene.Point.Graphene_Point_T := (1.0, 1.0);
      Outside : aliased Graphene.Point.Graphene_Point_T := (110.0, 40.0);
      Ignored : access Gsk_Rounded_Rect;
   begin
      Ignored := Init_From_Rect (Rounded'Access, Bounds'Access, 20.0);
      Assert_True (Contains_Point (Rounded'Access, Center'Access));
      Assert_False (Contains_Point (Rounded'Access, Cutout'Access));
      Assert_False (Contains_Point (Rounded'Access, Outside'Access));
   end Test_Contains_Point;

   --  Containment requires the complete query rectangle to fit inside the
   --  rounded shape, including its rounded corner area.
   procedure Test_Contains_Rect with Convention => C;
   procedure Test_Contains_Rect is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 100.0, Height => 80.0));
      Rounded : aliased Gsk_Rounded_Rect;
      Inner : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 25.0, Y => 20.0), (Width => 20.0, Height => 20.0));
      Corner : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 8.0, Height => 8.0));
      Outside : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 95.0, Y => 10.0), (Width => 10.0, Height => 10.0));
      Ignored : access Gsk_Rounded_Rect;
   begin
      Ignored := Init_From_Rect (Rounded'Access, Bounds'Access, 20.0);
      Assert_True (Contains_Rect (Rounded'Access, Inner'Access));
      Assert_False (Contains_Rect (Rounded'Access, Corner'Access));
      Assert_False (Contains_Rect (Rounded'Access, Outside'Access));
   end Test_Contains_Rect;

   --  A rectangle crossing the right edge intersects; a separated rectangle
   --  does not, covering both predicate results.
   procedure Test_Intersects_Rect with Convention => C;
   procedure Test_Intersects_Rect is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 100.0, Height => 80.0));
      Rounded : aliased Gsk_Rounded_Rect;
      Overlap : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 90.0, Y => 30.0), (Width => 20.0, Height => 20.0));
      Disjoint : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 120.0, Y => 30.0), (Width => 10.0, Height => 10.0));
      Ignored : access Gsk_Rounded_Rect;
   begin
      Ignored := Init_From_Rect (Rounded'Access, Bounds'Access, 20.0);
      Assert_True (Intersects_Rect (Rounded'Access, Overlap'Access));
      Assert_False (Intersects_Rect (Rounded'Access, Disjoint'Access));
   end Test_Intersects_Rect;

   --  Zero radii describe square corners; a positive radius makes the same
   --  bounds non-rectilinear.
   procedure Test_Is_Rectilinear with Convention => C;
   procedure Test_Is_Rectilinear is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 4.0, Y => 6.0), (Width => 50.0, Height => 30.0));
      Square : aliased Gsk_Rounded_Rect;
      Rounded : aliased Gsk_Rounded_Rect;
      Ignored : access Gsk_Rounded_Rect;
   begin
      Ignored := Init_From_Rect (Square'Access, Bounds'Access, 0.0);
      Ignored := Init_From_Rect (Rounded'Access, Bounds'Access, 5.0);
      Assert_True (Is_Rectilinear (Square'Access));
      Assert_False (Is_Rectilinear (Rounded'Access));
   end Test_Is_Rectilinear;

   --  The helper returns a value copy before releasing the GLib allocation.
   procedure Test_Copy_And_Free with Convention => C;
   procedure Test_Copy_And_Free is
      Expected_Bounds : constant Graphene.Rect.Graphene_Rect_T :=
        ((X => 3.0, Y => 7.0), (Width => 25.0, Height => 35.0));
      Bytes : constant size_t :=
        size_t (Gsk_Rounded_Rect'Size / System.Storage_Unit);
      Heap_Rect : constant Rounded_Rect_Access :=
        To_Rounded_Rect_Access (G_Malloc (Bytes));
      Copy : Gsk_Rounded_Rect;
   begin
      --  Fill every field before handing the allocation to the consuming
      --  helper, then inspect the value after the original storage is freed.
      Heap_Rect.all :=
        (Bounds => Expected_Bounds,
         Corner => (others => (Width => 6.0, Height => 8.0)));
      Copy := From_Object_Free (Heap_Rect);
      Near (Copy.Bounds.Origin.X, 3.0);
      Near (Copy.Bounds.Origin.Y, 7.0);
      Near (Copy.Bounds.Size.Width, 25.0);
      Near (Copy.Bounds.Size.Height, 35.0);
      Near (Copy.Corner (1).Width, 6.0);
      Near (Copy.Corner (1).Height, 8.0);
   end Test_Copy_And_Free;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gsk/rounded-rect/contains-point",
      Test_Contains_Point'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/rounded-rect/contains-rect",
      Test_Contains_Rect'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/rounded-rect/intersects-rect",
      Test_Intersects_Rect'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/rounded-rect/is-rectilinear",
      Test_Is_Rectilinear'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/rounded-rect/copy-and-free",
      Test_Copy_And_Free'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
