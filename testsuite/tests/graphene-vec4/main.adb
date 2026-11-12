--  Test GtkAda's Graphene.Vec4 copy-and-free helper.
--  No matching C test for graphene_vec4 exists in testsuite/c_tests/.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;     use Glib.Test;
with Graphene.Vec4; use Graphene.Vec4;
with Interfaces.C;  use Interfaces.C;

procedure Main is
   --  From_Object_Free releases its input with g_free, so allocate the
   --  temporary vector with GLib's allocator rather than an Ada allocator.
   type Vec4_Access is access all Graphene_Vec4_T;
   function To_Vec4_Access is new Ada.Unchecked_Conversion
     (System.Address, Vec4_Access);
   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   --  Graphene stores vector components as single-precision floats.
   procedure Check_Float (Actual, Expected : C_float) is
   begin
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual), Glib.Gdouble (Expected), 0.0001);
   end Check_Float;

   --  The helper returns a value copy and consumes the GLib allocation.
   procedure Test_Copy_And_Free with Convention => C;
   procedure Test_Copy_And_Free is
      Bytes : constant size_t :=
        size_t (Graphene_Vec4_T'Size / System.Storage_Unit);
      Heap_Vec4 : constant Vec4_Access :=
        To_Vec4_Access (G_Malloc (Bytes));
   begin
      --  Distinct components make it possible to verify the full copy after
      --  the original allocation has been released.
      declare
         Initialized : access Graphene_Vec4_T :=
           Init (Heap_Vec4, -4.5, 8.25, 2.75, -1.5);
      begin
         Check_Float (Get_X (Initialized), -4.5);
         Check_Float (Get_Y (Initialized), 8.25);
         Check_Float (Get_Z (Initialized), 2.75);
         Check_Float (Get_W (Initialized), -1.5);
      end;

      declare
         Copy : aliased Graphene_Vec4_T := From_Object_Free (Heap_Vec4);
      begin
         Check_Float (Get_X (Copy'Access), -4.5);
         Check_Float (Get_Y (Copy'Access), 8.25);
         Check_Float (Get_Z (Copy'Access), 2.75);
         Check_Float (Get_W (Copy'Access), -1.5);
      end;
   end Test_Copy_And_Free;

begin
   Glib.Test.Init;
   Add_Func
     ("/graphene/vec4/copy-free", Test_Copy_And_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
