--  Graphene.Point and Graphene.Point3d tests for coordinate comparisons and
--  GtkAda's copy-and-free helpers. No matching point tests exist in C tests.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;       use Glib.Test;
with Graphene.Point;  use Graphene.Point;
with Graphene.Point3d;
with Interfaces.C;   use Interfaces.C;

procedure Main is
   use type Interfaces.C.C_float;

   --  From_Object_Free releases its input with g_free, so give it storage
   --  allocated by GLib rather than an Ada allocator.
   type Point_Access is access all Graphene_Point_T;
   function To_Point_Access is new Ada.Unchecked_Conversion
     (System.Address, Point_Access);
   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   --  The 3D helper also consumes storage allocated by GLib's allocator.
   type Point3D_Access is access all Graphene.Point3d.Graphene_Point3D_T;
   function To_Point3D_Access is new Ada.Unchecked_Conversion
     (System.Address, Point3D_Access);

   procedure Check_Float (Actual, Expected : C_float) is
   begin
      --  Graphene stores point coordinates as single-precision floats.
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual), Glib.Gdouble (Expected), 0.0001);
   end Check_Float;

   procedure Test_Comparisons with Convention => C;
   procedure Test_Comparisons is
      A : aliased Graphene_Point_T := (1.0, 2.0);
      Same : aliased Graphene_Point_T := (1.0, 2.0);
      Nearby : aliased Graphene_Point_T := (1.03, 2.04);
      Different : aliased Graphene_Point_T := (1.5, 2.0);
   begin
      --  Exact coordinates compare equal; a visibly different coordinate
      --  checks the false result from the same public equality binding.
      Assert_True (Equal (A'Access, Same'Access));
      Assert_False (Equal (A'Access, Different'Access));

      --  Near uses Euclidean distance: the first offset is within 0.1 and
      --  the second is well outside it, exercising both results.
      Assert_True (Near (A'Access, Nearby'Access, 0.1));
      Assert_False (Near (A'Access, Different'Access, 0.1));
   end Test_Comparisons;

   procedure Test_Copy_And_Free with Convention => C;
   procedure Test_Copy_And_Free is
      Bytes : constant size_t :=
        size_t (Graphene_Point_T'Size / System.Storage_Unit);
      Heap_Point : constant Point_Access :=
        To_Point_Access (G_Malloc (Bytes));
   begin
      --  Store nonzero, distinct coordinates so the shallow copy can be
      --  checked after the helper frees the original GLib allocation.
      Heap_Point.all := (-4.5, 8.25);
      declare
         Copy : aliased Graphene_Point_T := From_Object_Free (Heap_Point);
      begin
         Check_Float (Copy.X, -4.5);
         Check_Float (Copy.Y, 8.25);
      end;
   end Test_Copy_And_Free;

   procedure Test_Point3D_Comparisons with Convention => C;
   procedure Test_Point3D_Comparisons is
      A : aliased Graphene.Point3d.Graphene_Point3D_T := (1.0, 2.0, 3.0);
      Same : aliased Graphene.Point3d.Graphene_Point3D_T := (1.0, 2.0, 3.0);
      Nearby : aliased Graphene.Point3d.Graphene_Point3D_T :=
        (1.03, 2.04, 3.0);
      Different : aliased Graphene.Point3d.Graphene_Point3D_T :=
        (1.5, 2.0, 3.0);
   begin
      --  Equal distinguishes identical coordinates from a changed X value.
      Assert_True (Graphene.Point3d.Equal (A'Access, Same'Access));
      Assert_False (Graphene.Point3d.Equal (A'Access, Different'Access));

      --  The nearby point is within the 3D Euclidean tolerance; the other is
      --  outside it, checking both results of the epsilon comparison.
      Assert_True (Graphene.Point3d.Near (A'Access, Nearby'Access, 0.1));
      Assert_False (Graphene.Point3d.Near (A'Access, Different'Access, 0.1));
   end Test_Point3D_Comparisons;

   procedure Test_Point3D_Copy_And_Free with Convention => C;
   procedure Test_Point3D_Copy_And_Free is
      Bytes : constant size_t := size_t
        (Graphene.Point3d.Graphene_Point3D_T'Size / System.Storage_Unit);
      Heap_Point : constant Point3D_Access :=
        To_Point3D_Access (G_Malloc (Bytes));
   begin
      --  Distinct nonzero values make each copied coordinate observable after
      --  From_Object_Free has released the original GLib allocation.
      Heap_Point.all := (-4.5, 8.25, -12.75);
      declare
         Copy : aliased Graphene.Point3d.Graphene_Point3D_T :=
           Graphene.Point3d.From_Object_Free (Heap_Point);
      begin
         Check_Float (Copy.X, -4.5);
         Check_Float (Copy.Y, 8.25);
         Check_Float (Copy.Z, -12.75);
      end;
   end Test_Point3D_Copy_And_Free;

begin
   Glib.Test.Init;
   Add_Func
     ("/graphene/point/comparisons",
      Test_Comparisons'Unrestricted_Access);
   Add_Func
     ("/graphene/point/copy-free",
      Test_Copy_And_Free'Unrestricted_Access);
   Add_Func
     ("/graphene/point/3d-comparisons",
      Test_Point3D_Comparisons'Unrestricted_Access);
   Add_Func
     ("/graphene/point/3d-copy-free",
      Test_Point3D_Copy_And_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
