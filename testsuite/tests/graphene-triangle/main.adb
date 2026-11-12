--  Graphene.Triangle tests for point containment, equality, barycentric and
--  UV coordinates, and GtkAda's copy-and-free helper. No matching C test
--  exists in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;        use Glib.Test;
with Graphene.Point3d; use Graphene.Point3d;
with Graphene.Triangle; use Graphene.Triangle;
with Graphene.Vec2;    use Graphene.Vec2;
with Interfaces.C;     use Interfaces.C;

procedure Main is
   use type Interfaces.C.C_float;

   --  From_Object_Free consumes GLib-allocated storage, so allocate its
   --  temporary triangle with the same allocator used by GtkAda wrappers.
   type Triangle_Access is access all Graphene_Triangle_T;
   function To_Triangle_Access is new Ada.Unchecked_Conversion
     (System.Address, Triangle_Access);
   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Check_Float (Actual, Expected : C_float) is
   begin
      --  Graphene stores coordinates as single-precision floats.
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual), Glib.Gdouble (Expected), 0.0001);
   end Check_Float;

   procedure Test_Geometry_And_Coordinates with Convention => C;
   procedure Test_Geometry_And_Coordinates is
      A : aliased Graphene_Point3D_T := (0.0, 0.0, 0.0);
      B : aliased Graphene_Point3D_T := (2.0, 0.0, 0.0);
      C : aliased Graphene_Point3D_T := (0.0, 2.0, 0.0);
      D : aliased Graphene_Point3D_T := (0.0, 3.0, 0.0);
      Inside : aliased Graphene_Point3D_T := (0.5, 1.0, 0.0);
      Outside : aliased Graphene_Point3D_T := (2.0, 2.0, 0.0);
      Triangle, Same, Different : aliased Graphene_Triangle_T;
      Barycentric, Uv : aliased Graphene_Vec2_T;
      Uv_A, Uv_B, Uv_C : aliased Graphene_Vec2_T;
      Ignored : access Graphene_Triangle_T;
   begin
      --  This right triangle has area two; the sample is strictly inside and
      --  the second point lies outside its hypotenuse.
      Ignored := Init_From_Point3D
        (Triangle'Access, A'Access, B'Access, C'Access);
      Ignored := Init_From_Point3D (Same'Access, A'Access, B'Access, C'Access);
      Ignored := Init_From_Point3D
        (Different'Access, A'Access, B'Access, D'Access);
      Assert_True (Equal (Triangle'Access, Same'Access));
      Assert_False (Equal (Triangle'Access, Different'Access));
      Assert_True (Contains_Point (Triangle'Access, Inside'Access));
      Assert_False (Contains_Point (Triangle'Access, Outside'Access));

      --  Coordinates are measured from A: barycentric x follows AC and y
      --  follows AB, so this point has values (0.5, 0.25).
      Assert_True
        (Get_Barycoords (Triangle'Access, Inside'Access, Barycentric'Access));
      Check_Float (Get_X (Barycentric'Access), 0.5);
      Check_Float (Get_Y (Barycentric'Access), 0.25);

      --  Distinct UV values on each vertex verify interpolation using both
      --  barycentric components rather than only the validity result.
      declare
         Ignore_A : access Graphene_Vec2_T := Init (Uv_A'Access, 1.0, 2.0);
         Ignore_B : access Graphene_Vec2_T := Init (Uv_B'Access, 3.0, 6.0);
         Ignore_C : access Graphene_Vec2_T := Init (Uv_C'Access, 9.0, 8.0);
      begin
         Assert_True
           (Get_Uv
              (Triangle'Access, Inside'Access,
               Uv_A'Access, Uv_B'Access, Uv_C'Access, Uv'Access));
         Check_Float (Get_X (Uv'Access), 5.5);
         Check_Float (Get_Y (Uv'Access), 6.0);
      end;
   end Test_Geometry_And_Coordinates;

   procedure Test_Copy_And_Free with Convention => C;
   procedure Test_Copy_And_Free is
      A : aliased Graphene_Point3D_T := (-1.0, 2.0, 0.0);
      B : aliased Graphene_Point3D_T := (3.0, 2.0, 0.0);
      C : aliased Graphene_Point3D_T := (-1.0, 5.0, 0.0);
      Source : aliased Graphene_Triangle_T;
      Heap_Triangle : constant Triangle_Access := To_Triangle_Access
        (G_Malloc
           (size_t (Graphene_Triangle_T'Size / System.Storage_Unit)));
      Ignored : access Graphene_Triangle_T;
   begin
      --  Initialize both values before the helper copies and frees the heap
      --  record, then compare the returned Ada value with its source.
      Ignored := Init_From_Point3D
        (Source'Access, A'Access, B'Access, C'Access);
      Ignored := Init_From_Point3D
        (Heap_Triangle, A'Access, B'Access, C'Access);
      declare
         Copy : aliased Graphene_Triangle_T :=
           From_Object_Free (Heap_Triangle);
      begin
         Assert_True (Equal (Source'Access, Copy'Access));
      end;
   end Test_Copy_And_Free;

begin
   Glib.Test.Init;
   Add_Func
     ("/graphene/triangle/geometry-coordinates",
      Test_Geometry_And_Coordinates'Unrestricted_Access);
   Add_Func
     ("/graphene/triangle/copy-free",
      Test_Copy_And_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
