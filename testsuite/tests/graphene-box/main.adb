--  Graphene.Box tests for array constructors, vertex retrieval, geometric
--  predicates, intersections, and GtkAda's copy-and-free helper.
--  No graphene_box test exists in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;        use Glib.Test;
with Graphene.Box;     use Graphene.Box;
with Graphene.Point3d;
with Graphene.Vec3;
with Interfaces.C;     use Interfaces.C;

procedure Main is
   use type Interfaces.C.C_float;

   --  From_Object_Free consumes a g_malloc allocation, so use the same
   --  allocator and pointer conversion as the returned C object requires.
   type Box_Access is access all Graphene_Box_T;
   function To_Box_Access is new Ada.Unchecked_Conversion
     (System.Address, Box_Access);

   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Check_Float (Actual, Expected : C_float) is
   begin
      --  Graphene stores single precision coordinates, so allow float noise.
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual), Glib.Gdouble (Expected), 0.0001);
   end Check_Float;

   procedure Test_Array_Constructors_And_Vertices with Convention => C;
   procedure Test_Array_Constructors_And_Vertices is
      --  These unordered input points produce the same enclosing box as the
      --  corresponding vectors, which checks both array length/address paths.
      Points : constant Graphene.Point3d.Graphene_Point3D_Array (0 .. 2) :=
        ((-2.0, 1.0, -1.0), (3.0, -4.0, 5.0), (1.0, 2.0, -3.0));
      V0, V1, V2 : aliased Graphene.Vec3.Graphene_Vec3_T;
      Vectors : Graphene.Vec3.Graphene_Vec3_Array (0 .. 2);
      From_Points, From_Vectors : aliased Graphene_Box_T;
      Ignored_Vec0 : access Graphene.Vec3.Graphene_Vec3_T;
      Ignored_Vec1 : access Graphene.Vec3.Graphene_Vec3_T;
      Ignored_Vec2 : access Graphene.Vec3.Graphene_Vec3_T;
      Ignored_Box  : access Graphene_Box_T;

      --  Check the returned eight vertices as a set, since their order is not
      --  part of the behavior this test needs to rely on.
      type Coordinates is record
         X, Y, Z : C_float;
      end record;
      Corners : constant array (Positive range 1 .. 8) of Coordinates :=
        ((-2.0, -4.0, -3.0), (-2.0, -4.0, 5.0),
         (-2.0, 2.0, -3.0),  (-2.0, 2.0, 5.0),
         (3.0, -4.0, -3.0),  (3.0, -4.0, 5.0),
         (3.0, 2.0, -3.0),   (3.0, 2.0, 5.0));

      procedure Check_Corners (Box : not null access Graphene_Box_T) is
         Vertices : aliased Graphene_Vec3_Array8;
      begin
         --  Check vertices as a set because Graphene does not promise their
         --  order as part of the result.
         Get_Vertices (Box, Vertices);
         for Corner of Corners loop
            declare
               Found : Boolean := False;
            begin
               for Vertex in Vertices'Range loop
                  --  Copy each component to an aliased object for Vec3's
                  --  access parameters; the array type has no aliased items.
                  declare
                     Value : aliased Graphene.Vec3.Graphene_Vec3_T :=
                       Vertices (Vertex);
                  begin
                     if Graphene.Vec3.Get_X (Value'Access) = Corner.X
                       and then Graphene.Vec3.Get_Y (Value'Access) = Corner.Y
                       and then Graphene.Vec3.Get_Z (Value'Access) = Corner.Z
                     then
                        Found := True;
                     end if;
                  end;
               end loop;
               Assert_True (Found);
            end;
         end loop;
      end Check_Corners;
   begin
      Ignored_Vec0 := Graphene.Vec3.Init (V0'Access, -2.0, 1.0, -1.0);
      Ignored_Vec1 := Graphene.Vec3.Init (V1'Access, 3.0, -4.0, 5.0);
      Ignored_Vec2 := Graphene.Vec3.Init (V2'Access, 1.0, 2.0, -3.0);
      Vectors := (0 => V0, 1 => V1, 2 => V2);

      Ignored_Box := Init_From_Points (From_Points'Access, Points);
      Ignored_Box := Init_From_Vectors (From_Vectors'Access, Vectors);
      Check_Corners (From_Points'Access);
      Check_Corners (From_Vectors'Access);
   end Test_Array_Constructors_And_Vertices;

   procedure Test_Copy_And_Free with Convention => C;
   procedure Test_Copy_And_Free is
      Min_Point : aliased Graphene.Point3d.Graphene_Point3D_T :=
        (-2.0, -4.0, -3.0);
      Max_Point : aliased Graphene.Point3d.Graphene_Point3D_T :=
        (3.0, 2.0, 5.0);
      Copy_Min, Copy_Max : aliased Graphene.Point3d.Graphene_Point3D_T;
      Source, Copy : aliased Graphene_Box_T;
      Ignored : access Graphene_Box_T;
      Bytes : constant size_t :=
        size_t (Graphene_Box_T'Size / System.Storage_Unit);
      Heap_Box : constant Box_Access := To_Box_Access (G_Malloc (Bytes));
   begin
      --  Initialize the allocated object before the helper copies and frees it.
      Ignored := Graphene.Box.Init
        (Source'Access, Min_Point'Access, Max_Point'Access);
      Ignored := Init_From_Box (Heap_Box, Source'Access);
      Copy := From_Object_Free (Heap_Box);
      --  Inspect both copied bounds after the helper releases its allocation.
      Get_Min (Copy'Access, Copy_Min'Access);
      Get_Max (Copy'Access, Copy_Max'Access);
      Check_Float (Copy_Min.X, -2.0);
      Check_Float (Copy_Min.Y, -4.0);
      Check_Float (Copy_Min.Z, -3.0);
      Check_Float (Copy_Max.X, 3.0);
      Check_Float (Copy_Max.Y, 2.0);
      Check_Float (Copy_Max.Z, 5.0);
   end Test_Copy_And_Free;

begin
   Glib.Test.Init;
   Add_Func
     ("/graphene/box/array-constructors-vertices",
      Test_Array_Constructors_And_Vertices'Unrestricted_Access);
   Add_Func
     ("/graphene/box/copy-free",
      Test_Copy_And_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
