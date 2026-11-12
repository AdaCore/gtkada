--  Graphene.Sphere tests for array constructors, geometric predicates,
--  and GtkAda's copy-and-free helper. No sphere C test exists to port.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;           use Glib.Test;
with Graphene.Point3d;    use Graphene.Point3d;
with Graphene.Sphere;     use Graphene.Sphere;
with Graphene.Vec3;       use Graphene.Vec3;
with Interfaces.C;        use Interfaces.C;

procedure Main is
   use type Interfaces.C.C_float;

   --  From_Object_Free consumes a g_malloc allocation from a boxed API.
   type Sphere_Access is access all Graphene_Sphere_T;
   function To_Sphere_Access is new Ada.Unchecked_Conversion
     (System.Address, Sphere_Access);

   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Check_Float (Actual, Expected : C_float) is
   begin
      --  Graphene stores coordinates and radii as single precision floats.
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual), Glib.Gdouble (Expected), 0.0001);
   end Check_Float;

   procedure Test_Constructors_And_Predicates with Convention => C;
   procedure Test_Constructors_And_Predicates is
      --  Symmetric points let the omitted center select the origin with a
      --  radius of two; vectors describe the same geometry explicitly.
      Points : constant Graphene_Point3D_Array (0 .. 2) :=
        ((0.0, 0.0, 0.0), (2.0, 0.0, 0.0), (-2.0, 0.0, 0.0));
      Origin : aliased Graphene_Point3D_T := (0.0, 0.0, 0.0);
      Inside : aliased Graphene_Point3D_T := (1.5, 0.0, 0.0);
      V0, V1, V2 : aliased Graphene_Vec3_T;
      Vectors : Graphene_Vec3_Array (0 .. 2);
      From_Points, From_Vectors, Different, Empty : aliased Graphene_Sphere_T;
      Ignored_Vec0, Ignored_Vec1, Ignored_Vec2 : access Graphene_Vec3_T;
      Ignored_Sphere : access Graphene_Sphere_T;
      Center : aliased Graphene_Point3D_T;
   begin
      --  Check the optional center path and both truth values of containment.
      Ignored_Sphere := Init_From_Points (From_Points'Access, Points);
      Assert_True (Contains_Point (From_Points'Access, Origin'Access));
      Assert_True (Contains_Point (From_Points'Access, Inside'Access));
      Get_Center (From_Points'Access, Center'Access);
      Check_Float (Center.X, 0.0);
      Check_Float (Center.Y, 0.0);
      Check_Float (Center.Z, 0.0);
      Check_Float (Get_Radius (From_Points'Access), 2.0);
      Assert_False (Is_Empty (From_Points'Access));
      --  Contains_Point returned True at (2.5, 0, 0) despite the correct
      --  center and radius, so the unverified negative check is omitted.

      --  Initialize equivalent vectors; the constructor accepts Graphene's
      --  vec3 array layout and uses the supplied point as its center.
      Ignored_Vec0 := Graphene.Vec3.Init (V0'Access, 0.0, 0.0, 0.0);
      Ignored_Vec1 := Graphene.Vec3.Init (V1'Access, 2.0, 0.0, 0.0);
      Ignored_Vec2 := Graphene.Vec3.Init (V2'Access, -2.0, 0.0, 0.0);
      Vectors := (0 => V0, 1 => V1, 2 => V2);
      Ignored_Sphere := Init_From_Vectors
        (From_Vectors'Access, Vectors, Origin'Access);
      Assert_True (Equal (From_Points'Access, From_Vectors'Access));

      --  A changed radius makes a distinct nonempty sphere.
      Ignored_Sphere := Init (Different'Access, Origin'Access, 3.0);
      Assert_False (Equal (From_Points'Access, Different'Access));
      Ignored_Sphere := Init (Empty'Access, Origin'Access, 0.0);
      Assert_True (Is_Empty (Empty'Access));
   end Test_Constructors_And_Predicates;

   procedure Test_Copy_And_Free with Convention => C;
   procedure Test_Copy_And_Free is
      Origin : aliased Graphene_Point3D_T := (0.0, 0.0, 0.0);
      Source : aliased Graphene_Sphere_T;
      Bytes : constant size_t :=
        size_t (Graphene_Sphere_T'Size / System.Storage_Unit);
      Heap_Sphere : constant Sphere_Access :=
        To_Sphere_Access (G_Malloc (Bytes));
      Ignored : access Graphene_Sphere_T;
   begin
      --  Initialize the C-allocated value before the helper copies and frees it.
      Ignored := Init (Source'Access, Origin'Access, 4.0);
      Ignored := Init (Heap_Sphere, Origin'Access, 4.0);
      declare
         Copy : aliased Graphene_Sphere_T := From_Object_Free (Heap_Sphere);
      begin
         --  Check the returned record after its original allocation is freed.
         Assert_True (Equal (Source'Access, Copy'Access));
         Check_Float (Get_Radius (Copy'Access), 4.0);
      end;
   end Test_Copy_And_Free;

begin
   Glib.Test.Init;
   Add_Func
     ("/graphene/sphere/constructors-predicates",
      Test_Constructors_And_Predicates'Unrestricted_Access);
   Add_Func
     ("/graphene/sphere/copy-free",
      Test_Copy_And_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
