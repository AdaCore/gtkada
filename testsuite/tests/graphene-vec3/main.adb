--  Test GtkAda's Graphene.Vec3 equality and copy-and-free helpers.
--  No matching C test for these Graphene.Vec3 operations exists in
--  testsuite/c_tests/.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;     use Glib.Test;
with Graphene.Vec3; use Graphene.Vec3;
with Interfaces.C;  use Interfaces.C;

procedure Main is
   --  From_Object_Free releases its input with g_free, so allocate its
   --  temporary vector using GLib's allocator rather than an Ada allocator.
   type Vec3_Access is access all Graphene_Vec3_T;
   function To_Vec3_Access is new Ada.Unchecked_Conversion
     (System.Address, Vec3_Access);
   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   --  Compare stored single-precision components with a small tolerance.
   procedure Check_Float (Actual, Expected : C_float) is
   begin
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual), Glib.Gdouble (Expected), 0.0001);
   end Check_Float;

   --  Exercise the internal copy-and-free conversion on GLib-owned memory.
   procedure Test_Copy_And_Free with Convention => C;
   procedure Test_Copy_And_Free is
      Bytes : constant size_t :=
        size_t (Graphene_Vec3_T'Size / System.Storage_Unit);
      Heap_Vec3 : constant Vec3_Access :=
        To_Vec3_Access (G_Malloc (Bytes));
   begin
      --  Distinct values remain observable after the helper frees its input.
      declare
         Initialized : access Graphene_Vec3_T :=
           Init (Heap_Vec3, -4.5, 8.25, 2.75);
      begin
         Check_Float (Get_X (Initialized), -4.5);
         Check_Float (Get_Y (Initialized), 8.25);
         Check_Float (Get_Z (Initialized), 2.75);
      end;
      declare
         Copy : aliased Graphene_Vec3_T := From_Object_Free (Heap_Vec3);
      begin
         Check_Float (Get_X (Copy'Access), -4.5);
         Check_Float (Get_Y (Copy'Access), 8.25);
         Check_Float (Get_Z (Copy'Access), 2.75);
      end;
   end Test_Copy_And_Free;

   --  Equal returns true for separate vectors with identical components.
   procedure Test_Equality with Convention => C;
   procedure Test_Equality is
      First, Same : aliased Graphene_Vec3_T;
      Ignored : access Graphene_Vec3_T;
   begin
      --  Initialize independent vectors with exactly matching components.
      Ignored := Init (First'Access, 1.0, -2.0, 3.5);
      Ignored := Init (Same'Access, 1.0, -2.0, 3.5);
      Assert_True (Equal (First'Access, Same'Access));
   end Test_Equality;

begin
   Glib.Test.Init;
   Add_Func
     ("/graphene/vec3/copy-free", Test_Copy_And_Free'Unrestricted_Access);
   Add_Func
     ("/graphene/vec3/equality", Test_Equality'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
