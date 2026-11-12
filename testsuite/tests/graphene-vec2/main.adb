--  Test Graphene.Vec2 equality and GtkAda's
--  copy-and-free helper. No matching C test exists in testsuite/c_tests/.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;    use Glib.Test;
with Graphene.Vec2; use Graphene.Vec2;
with Interfaces.C; use Interfaces.C;

procedure Main is
   use type Interfaces.C.C_float;

   --  From_Object_Free releases its input with g_free, so allocate its
   --  temporary vector using GLib's allocator rather than an Ada allocator.
   type Vec2_Access is access all Graphene_Vec2_T;
   function To_Vec2_Access is new Ada.Unchecked_Conversion
     (System.Address, Vec2_Access);
   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Check_Float (Actual, Expected : C_float) is
   begin
      --  Graphene stores vector components as single-precision floats.
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual), Glib.Gdouble (Expected), 0.0001);
   end Check_Float;

   procedure Test_Comparisons with Convention => C;
   procedure Test_Comparisons is
      A, Equal_To_A, Different_From_A : aliased Graphene_Vec2_T;
      Ignored : access Graphene_Vec2_T;
   begin
      --  Identical components compare equal, and a visible component change
      --  makes the comparison false while all vectors remain initialized.
      Ignored := Init (A'Access, 1.0, 2.0);
      Ignored := Init (Equal_To_A'Access, 1.0, 2.0);
      Ignored := Init (Different_From_A'Access, 1.5, 2.0);

      Assert_True (Equal (A'Access, Equal_To_A'Access));
      Assert_False (Equal (A'Access, Different_From_A'Access));
   end Test_Comparisons;

   procedure Test_Copy_And_Free with Convention => C;
   procedure Test_Copy_And_Free is
      Bytes : constant size_t :=
        size_t (Graphene_Vec2_T'Size / System.Storage_Unit);
      Heap_Vec2 : constant Vec2_Access :=
        To_Vec2_Access (G_Malloc (Bytes));
   begin
      --  Distinct nonzero values make both copied components observable after
      --  From_Object_Free has released the GLib allocation.
      declare
         Initialized : access Graphene_Vec2_T :=
           Init (Heap_Vec2, -4.5, 8.25);
      begin
         Check_Float (Get_X (Initialized), -4.5);
         Check_Float (Get_Y (Initialized), 8.25);
      end;
      declare
         Copy : aliased Graphene_Vec2_T := From_Object_Free (Heap_Vec2);
      begin
         Check_Float (Get_X (Copy'Access), -4.5);
         Check_Float (Get_Y (Copy'Access), 8.25);
      end;
   end Test_Copy_And_Free;

begin
   Glib.Test.Init;
   Add_Func
     ("/graphene/vec2/comparisons",
      Test_Comparisons'Unrestricted_Access);
   Add_Func
     ("/graphene/vec2/copy-free",
      Test_Copy_And_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
