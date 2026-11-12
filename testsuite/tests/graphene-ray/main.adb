--  Graphene.Ray tests for equality, geometric intersection predicates, and
--  GtkAda's copy-and-free helper. No graphene_ray C test exists to port.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;       use Glib.Test;
with Graphene.Box;
with Graphene.Point3d;
with Graphene.Ray;    use Graphene.Ray;
with Graphene.Sphere;
with Graphene.Triangle;
with Graphene.Vec3;
with Interfaces.C;    use Interfaces.C;

procedure Main is
   use type Interfaces.C.C_float;

   --  From_Object_Free consumes a GLib allocation, like a boxed C result.
   type Ray_Access is access all Graphene_Ray_T;
   function To_Ray_Access is new Ada.Unchecked_Conversion
     (System.Address, Ray_Access);

   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Check_Float (Actual, Expected : C_float) is
   begin
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual), Glib.Gdouble (Expected), 0.0001);
   end Check_Float;

   procedure Test_Copy_And_Free with Convention => C;
   procedure Test_Copy_And_Free is
      --  Use nonzero components so the returned origin and direction are
      --  distinguishable from the record's default, uninitialized storage.
      Origin : aliased Graphene.Point3d.Graphene_Point3D_T :=
        (2.0, -1.0, 4.0);
      Direction : aliased Graphene.Vec3.Graphene_Vec3_T;
      Actual_Origin : aliased Graphene.Point3d.Graphene_Point3D_T;
      Actual_Direction : aliased Graphene.Vec3.Graphene_Vec3_T;
      Bytes : constant size_t :=
        size_t (Graphene_Ray_T'Size / System.Storage_Unit);
      Heap_Ray : constant Ray_Access := To_Ray_Access (G_Malloc (Bytes));
      Ignored_Vec3 : access Graphene.Vec3.Graphene_Vec3_T;
      Ignored_Ray : access Graphene_Ray_T;
   begin
      --  Initialize the allocated ray before the helper copies and frees it.
      Ignored_Vec3 := Graphene.Vec3.Init
        (Direction'Access, 1.0, 0.0, 0.0);
      Ignored_Ray := Init (Heap_Ray, Origin'Access, Direction'Access);
      declare
         Copy : aliased Graphene_Ray_T := From_Object_Free (Heap_Ray);
      begin
         --  Read both copied fields through the public accessors after free.
         Get_Origin (Copy'Access, Actual_Origin'Access);
         Get_Direction (Copy'Access, Actual_Direction'Access);
         Assert_True (Actual_Origin.X = 2.0);
         Assert_True (Actual_Origin.Y = -1.0);
         Assert_True (Actual_Origin.Z = 4.0);
         --  Init normalizes the direction; SIMD implementations can round
         --  even an axis vector, so allow a small floating-point error.
         Check_Float (Graphene.Vec3.Get_X (Actual_Direction'Access), 1.0);
         Check_Float (Graphene.Vec3.Get_Y (Actual_Direction'Access), 0.0);
         Check_Float (Graphene.Vec3.Get_Z (Actual_Direction'Access), 0.0);
      end;
   end Test_Copy_And_Free;

   procedure Test_Equality_And_Intersections with Convention => C;
   procedure Test_Equality_And_Intersections is
      --  This ray passes through the unit box, sphere, and triangle at z = 0.
      Origin : aliased Graphene.Point3d.Graphene_Point3D_T :=
        (0.0, 0.0, -5.0);
      Miss_Origin : aliased Graphene.Point3d.Graphene_Point3D_T :=
        (5.0, 0.0, -5.0);
      Direction : aliased Graphene.Vec3.Graphene_Vec3_T;
      Ray, Same_Ray, Miss_Ray : aliased Graphene_Ray_T;
      Box_Min : aliased Graphene.Point3d.Graphene_Point3D_T :=
        (-1.0, -1.0, -1.0);
      Box_Max : aliased Graphene.Point3d.Graphene_Point3D_T :=
        (1.0, 1.0, 1.0);
      Center : aliased Graphene.Point3d.Graphene_Point3D_T :=
        (0.0, 0.0, 0.0);
      Triangle_A : aliased Graphene.Point3d.Graphene_Point3D_T :=
        (-1.0, -1.0, 0.0);
      Triangle_B : aliased Graphene.Point3d.Graphene_Point3D_T :=
        (1.0, -1.0, 0.0);
      Triangle_C : aliased Graphene.Point3d.Graphene_Point3D_T :=
        (0.0, 1.0, 0.0);
      Box : aliased Graphene.Box.Graphene_Box_T;
      Sphere : aliased Graphene.Sphere.Graphene_Sphere_T;
      Triangle : aliased Graphene.Triangle.Graphene_Triangle_T;
      Ignored_Vec : access Graphene.Vec3.Graphene_Vec3_T;
      Ignored_Ray : access Graphene_Ray_T;
      Ignored_Box : access Graphene.Box.Graphene_Box_T;
      Ignored_Sphere : access Graphene.Sphere.Graphene_Sphere_T;
      Ignored_Triangle : access Graphene.Triangle.Graphene_Triangle_T;
   begin
      --  A copied ray compares equal, while changing its origin makes it
      --  distinct; these exercise both Boolean outcomes from Equal.
      Ignored_Vec := Graphene.Vec3.Init
        (Direction'Access, 0.0, 0.0, 1.0);
      Ignored_Ray := Init (Ray'Access, Origin'Access, Direction'Access);
      Ignored_Ray := Init_From_Ray (Same_Ray'Access, Ray'Access);
      Ignored_Ray := Init (Miss_Ray'Access, Miss_Origin'Access,
                           Direction'Access);
      Assert_True (Equal (Ray'Access, Same_Ray'Access));
      Assert_False (Equal (Ray'Access, Miss_Ray'Access));

      --  The same central ray crosses all three targets. A parallel ray at
      --  x = 5 misses each one, checking both predicate results.
      Ignored_Ray := Init (Miss_Ray'Access, Miss_Origin'Access,
                           Direction'Access);
      Ignored_Box := Graphene.Box.Init
        (Box'Access, Box_Min'Access, Box_Max'Access);
      Ignored_Sphere := Graphene.Sphere.Init
        (Sphere'Access, Center'Access, 1.0);
      Ignored_Triangle := Graphene.Triangle.Init_From_Point3D
        (Triangle'Access, Triangle_A'Access, Triangle_B'Access,
         Triangle_C'Access);
      Assert_True (Intersects_Box (Ray'Access, Box'Access));
      Assert_False (Intersects_Box (Miss_Ray'Access, Box'Access));
      Assert_True (Intersects_Sphere (Ray'Access, Sphere'Access));
      Assert_False (Intersects_Sphere (Miss_Ray'Access, Sphere'Access));
      Assert_True (Intersects_Triangle (Ray'Access, Triangle'Access));
      Assert_False (Intersects_Triangle (Miss_Ray'Access, Triangle'Access));
   end Test_Equality_And_Intersections;

begin
   Glib.Test.Init;
   Add_Func
     ("/graphene/ray/copy-free",
      Test_Copy_And_Free'Unrestricted_Access);
   Add_Func
     ("/graphene/ray/equality-intersections",
      Test_Equality_And_Intersections'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
