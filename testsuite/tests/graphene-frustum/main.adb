--  Graphene.Frustum tests for initialization, plane retrieval, containment,
--  intersection queries, equality, and GtkAda's copy-and-free helper.
--  No matching frustum test exists in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;        use Glib.Test;
with Graphene.Box;     use Graphene.Box;
with Graphene.Frustum; use Graphene.Frustum;
with Graphene.Matrix;  use Graphene.Matrix;
with Graphene.Plane;   use Graphene.Plane;
with Graphene.Point3d; use Graphene.Point3d;
with Graphene.Sphere;  use Graphene.Sphere;
with Interfaces.C;     use Interfaces.C;

procedure Main is
   --  From_Object_Free expects a frustum allocated with g_malloc.
   type Frustum_Access is access all Graphene_Frustum_T;
   function To_Frustum_Access is new Ada.Unchecked_Conversion
     (System.Address, Frustum_Access);

   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Test_Queries with Convention => C;
   procedure Test_Queries is
      Matrix  : aliased Graphene_Matrix_T;
      Frustum : aliased Graphene_Frustum_T;
      Copy    : aliased Graphene_Frustum_T;
      Origin  : aliased Graphene_Point3D_T := (0.0, 0.0, 0.0);
      Outside : aliased Graphene_Point3D_T := (5.0, 0.0, 0.0);
      Box_Min : aliased Graphene_Point3D_T := (-0.5, -0.5, -0.5);
      Box_Max : aliased Graphene_Point3D_T := (0.5, 0.5, 0.5);
      Far_Min : aliased Graphene_Point3D_T := (5.0, 5.0, 5.0);
      Far_Max : aliased Graphene_Point3D_T := (6.0, 6.0, 6.0);
      Inside_Box, Outside_Box : aliased Graphene_Box_T;
      Inside_Sphere, Outside_Sphere : aliased Graphene_Sphere_T;
      Ignored_Matrix  : access Graphene_Matrix_T;
      Ignored_Frustum : access Graphene_Frustum_T;
      Ignored_Box     : access Graphene_Box_T;
      Ignored_Sphere  : access Graphene_Sphere_T;
   begin
      --  Identity defines a clip volume around the origin for these queries.
      Ignored_Matrix := Init_Identity (Matrix'Access);
      Ignored_Frustum := Init_From_Matrix (Frustum'Access, Matrix'Access);
      Ignored_Frustum := Init_From_Frustum (Copy'Access, Frustum'Access);
      Assert_True (Equal (Frustum'Access, Copy'Access));

      --  A point and box around the origin are inside; translated values lie
      --  outside the finite volume and exercise the negative query results.
      Assert_True (Contains_Point (Frustum'Access, Origin'Access));
      Assert_False (Contains_Point (Frustum'Access, Outside'Access));
      Ignored_Box := Init (Inside_Box'Access, Box_Min'Access, Box_Max'Access);
      Ignored_Box := Init (Outside_Box'Access, Far_Min'Access, Far_Max'Access);
      Assert_True (Intersects_Box (Frustum'Access, Inside_Box'Access));
      Assert_False (Intersects_Box (Frustum'Access, Outside_Box'Access));

      --  Test both intersecting and disjoint spheres using ordinary finite
      --  radii so the result does not depend on boundary rounding.
      Ignored_Sphere := Init (Inside_Sphere'Access, Origin'Access, 0.25);
      Ignored_Sphere := Init (Outside_Sphere'Access, Outside'Access, 0.25);
      Assert_True (Intersects_Sphere (Frustum'Access, Inside_Sphere'Access));
      Assert_False (Intersects_Sphere (Frustum'Access, Outside_Sphere'Access));
   end Test_Queries;

   procedure Test_Planes_And_Copy_Helper with Convention => C;
   procedure Test_Planes_And_Copy_Helper is
      Matrix   : aliased Graphene_Matrix_T;
      Frustum  : aliased Graphene_Frustum_T;
      Rebuilt  : aliased Graphene_Frustum_T;
      Planes   : Graphene_Plane_Array6;
      P0, P1, P2, P3, P4, P5 : aliased Graphene_Plane_T;
      Ignored_Matrix  : access Graphene_Matrix_T;
      Ignored_Frustum : access Graphene_Frustum_T;
      Bytes : constant size_t :=
        size_t (Graphene_Frustum_T'Size / System.Storage_Unit);
      Heap_Frustum : constant Frustum_Access :=
        To_Frustum_Access (G_Malloc (Bytes));
   begin
      --  Retrieve all six planes, then feed those values back to Init. This
      --  checks the C-compatible array type and the six-plane constructor.
      Ignored_Matrix := Init_Identity (Matrix'Access);
      Ignored_Frustum := Init_From_Matrix (Frustum'Access, Matrix'Access);
      Get_Planes (Frustum'Access, Planes);
      P0 := Planes (1);
      P1 := Planes (2);
      P2 := Planes (3);
      P3 := Planes (4);
      P4 := Planes (5);
      P5 := Planes (6);
      Ignored_Frustum := Init
        (Rebuilt'Access, P0'Access, P1'Access, P2'Access,
         P3'Access, P4'Access, P5'Access);
      Assert_True (Equal (Frustum'Access, Rebuilt'Access));

      --  The helper owns and frees its input, so initialize the g_malloc
      --  buffer first and verify the copied value before it leaves scope.
      Ignored_Frustum := Init_From_Frustum (Heap_Frustum, Frustum'Access);
      declare
         Returned : aliased Graphene_Frustum_T :=
           From_Object_Free (Heap_Frustum);
      begin
         Assert_True (Equal (Frustum'Access, Returned'Access));
      end;
   end Test_Planes_And_Copy_Helper;

begin
   Glib.Test.Init;
   Add_Func ("/graphene/frustum/queries", Test_Queries'Unrestricted_Access);
   Add_Func
     ("/graphene/frustum/planes-copy-helper",
      Test_Planes_And_Copy_Helper'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
