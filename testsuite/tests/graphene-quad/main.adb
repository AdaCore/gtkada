--  Graphene.Quad geometry and GtkAda copy-and-free helper tests. No matching
--  graphene_quad C test exists in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;       use Glib.Test;
with Graphene.Point;  use Graphene.Point;
with Graphene.Quad;   use Graphene.Quad;
with Graphene.Rect;
with Interfaces.C;    use Interfaces.C;

procedure Main is
   use type Interfaces.C.C_float;
   use type Glib.GType;

   --  From_Object_Free releases a g_malloc allocation, matching the allocator
   --  used by Graphene's heap-returning boxed APIs.
   type Quad_Access is access all Graphene_Quad_T;
   function To_Quad_Access is new Ada.Unchecked_Conversion
     (System.Address, Quad_Access);
   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Check_Float (Actual, Expected : C_float) is
   begin
      --  Graphene stores point coordinates as single-precision floats.
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual), Glib.Gdouble (Expected), 0.0001);
   end Check_Float;

   procedure Check_Point
     (Actual : access constant Graphene_Point_T;
      X, Y   : C_float)
   is
   begin
      Check_Float (Actual.X, X);
      Check_Float (Actual.Y, Y);
   end Check_Point;

   --  Test rectangle initialization, its bounds, and all four returned points.
   procedure Test_Rectangle_Quad with Convention => C;
   procedure Test_Rectangle_Quad is
      Rect : aliased Graphene.Rect.Graphene_Rect_T;
      Quad : aliased Graphene_Quad_T;
      Ignored_Rect : access Graphene.Rect.Graphene_Rect_T;
      Ignored_Quad : access Graphene_Quad_T;
      Bounds : aliased Graphene.Rect.Graphene_Rect_T;
   begin
      --  The type getter should return Graphene's registered boxed type.
      Assert_True (Graphene.Quad.Get_Type /= Glib.GType_Invalid);

      --  Constructing from a rectangle preserves its bounds. Contains is not
      --  called because its current C _Bool to Glib.Gboolean binding is wrong.
      Ignored_Rect := Graphene.Rect.Init (Rect'Access, 0.0, 0.0, 10.0, 8.0);
      Ignored_Quad := Init_From_Rect (Quad'Access, Rect'Access);
      Graphene.Quad.Bounds (Quad'Access, Bounds'Access);
      Check_Float (Bounds.Origin.X, 0.0);
      Check_Float (Bounds.Origin.Y, 0.0);
      Check_Float (Bounds.Size.Width, 10.0);
      Check_Float (Bounds.Size.Height, 8.0);

      --  Graphene numbers the corners from zero; check all four returned
      --  points against the rectangle's expected clockwise corner sequence.
      Check_Point (Get_Point (Quad'Access, 0), 0.0, 0.0);
      Check_Point (Get_Point (Quad'Access, 1), 10.0, 0.0);
      Check_Point (Get_Point (Quad'Access, 2), 10.0, 8.0);
      Check_Point (Get_Point (Quad'Access, 3), 0.0, 8.0);
   end Test_Rectangle_Quad;

   --  Compare both point-based constructors on the same skewed quadrilateral.
   procedure Test_Point_Initializers with Convention => C;
   procedure Test_Point_Initializers is
      P1 : aliased Graphene_Point_T := (-2.0, 1.0);
      P2 : aliased Graphene_Point_T := (6.0, 1.0);
      P3 : aliased Graphene_Point_T := (7.0, 5.0);
      P4 : aliased Graphene_Point_T := (-1.0, 6.0);
      Via_Arguments : aliased Graphene_Quad_T;
      Via_Array : aliased Graphene_Quad_T;
      Points : constant Graphene_Point_Array4 :=
        (1 => P1, 2 => P2, 3 => P3, 4 => P4);
      Ignored : access Graphene_Quad_T;
   begin
      --  Both constructors retain the supplied vertices in the given order.
      Ignored := Init
        (Via_Arguments'Access,
         P1'Access, P2'Access, P3'Access, P4'Access);
      Ignored := Init_From_Points (Via_Array'Access, Points);
      for Index in 0 .. 3 loop
         declare
            Expected : constant Graphene_Point_T := Points (Index + 1);
         begin
            Check_Point
              (Get_Point (Via_Arguments'Access, Glib.Guint (Index)),
               Expected.X, Expected.Y);
            Check_Point
              (Get_Point (Via_Array'Access, Glib.Guint (Index)),
               Expected.X, Expected.Y);
         end;
      end loop;

      --  Bounds for a skewed quadrilateral include each extreme coordinate.
      declare
         Bounds : aliased Graphene.Rect.Graphene_Rect_T;
      begin
         Graphene.Quad.Bounds (Via_Array'Access, Bounds'Access);
         Check_Float (Bounds.Origin.X, -2.0);
         Check_Float (Bounds.Origin.Y, 1.0);
         Check_Float (Bounds.Size.Width, 9.0);
         Check_Float (Bounds.Size.Height, 5.0);
      end;
   end Test_Point_Initializers;

   --  Verify every copied point after the helper releases its source storage.
   procedure Test_Copy_And_Free with Convention => C;
   procedure Test_Copy_And_Free is
      Bytes : constant size_t :=
        size_t (Graphene_Quad_T'Size / System.Storage_Unit);
      Heap_Quad : constant Quad_Access :=
        To_Quad_Access (G_Malloc (Bytes));
      Points : constant Graphene_Point_Array4 :=
        (1 => (-4.5, 8.25), 2 => (2.0, 8.25),
         3 => (2.0, 12.0), 4 => (-4.5, 12.0));
   begin
      --  Distinct nonzero vertices remain observable after the helper copies
      --  the record and releases its GLib allocation.
      for Index in Points'Range loop
         Heap_Quad.all.Points (Index) := Points (Index);
      end loop;
      declare
         Copy : aliased Graphene_Quad_T := From_Object_Free (Heap_Quad);
      begin
         for Index in 0 .. 3 loop
            declare
               Actual : aliased Graphene_Point_T := Copy.Points (Index + 1);
            begin
               Check_Point
                 (Actual'Access,
                  Points (Index + 1).X, Points (Index + 1).Y);
            end;
         end loop;
      end;
   end Test_Copy_And_Free;

begin
   Glib.Test.Init;
   Add_Func
     ("/graphene/quad/rectangle",
      Test_Rectangle_Quad'Unrestricted_Access);
   Add_Func
     ("/graphene/quad/point-initializers",
      Test_Point_Initializers'Unrestricted_Access);
   Add_Func
     ("/graphene/quad/copy-free",
      Test_Copy_And_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
