--  Graphene.Config tests for its vector and matrix copy-and-free helpers.
--  No corresponding C tests were found; the helpers are GtkAda additions.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;       use Glib.Test;
with Graphene.Config; use Graphene.Config;
with Interfaces.C;    use Interfaces.C;

procedure Main is
   use type Interfaces.C.C_float;

   --  The helpers consume buffers allocated with GLib's allocator.
   type Simd4f_Access is access all Graphene_Simd4f;
   type Simd4x4f_Access is access all Graphene_Simd4x4f;
   function To_Simd4f_Access is new Ada.Unchecked_Conversion
     (System.Address, Simd4f_Access);
   function To_Simd4x4f_Access is new Ada.Unchecked_Conversion
     (System.Address, Simd4x4f_Access);

   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Test_Vector_Copy_And_Free with Convention => C;
   procedure Test_Vector_Copy_And_Free is
      Bytes : constant size_t := size_t (Graphene_Simd4f'Size / System.Storage_Unit);
      Buffer : constant Simd4f_Access := To_Simd4f_Access (G_Malloc (Bytes));
   begin
      --  Fill all lanes before the helper copies the value and frees its input.
      Buffer.all := (X => 1.25, Y => -2.5, Z => 3.75, W => -4.0);
      declare
         Copy : constant Graphene_Simd4f := From_Object_Free (Buffer);
      begin
         --  Check each lane so the complete returned record is verified.
         Assert_True (Copy.X = 1.25);
         Assert_True (Copy.Y = -2.5);
         Assert_True (Copy.Z = 3.75);
         Assert_True (Copy.W = -4.0);
      end;
   end Test_Vector_Copy_And_Free;

   procedure Test_Matrix_Copy_And_Free with Convention => C;
   procedure Test_Matrix_Copy_And_Free is
      Bytes : constant size_t := size_t (Graphene_Simd4x4f'Size / System.Storage_Unit);
      Buffer : constant Simd4x4f_Access := To_Simd4x4f_Access (G_Malloc (Bytes));
   begin
      --  Distinct values in all sixteen cells expose truncation or field swaps.
      Buffer.all :=
        (X => (X => 1.0,  Y => 2.0,  Z => 3.0,  W => 4.0),
         Y => (X => 5.0,  Y => 6.0,  Z => 7.0,  W => 8.0),
         Z => (X => 9.0,  Y => 10.0, Z => 11.0, W => 12.0),
         W => (X => 13.0, Y => 14.0, Z => 15.0, W => 16.0));
      declare
         Copy : constant Graphene_Simd4x4f := From_Object_Free (Buffer);
      begin
         --  Check every vector and lane in the copied matrix.
         Assert_True (Copy.X.X = 1.0 and Copy.X.Y = 2.0);
         Assert_True (Copy.X.Z = 3.0 and Copy.X.W = 4.0);
         Assert_True (Copy.Y.X = 5.0 and Copy.Y.Y = 6.0);
         Assert_True (Copy.Y.Z = 7.0 and Copy.Y.W = 8.0);
         Assert_True (Copy.Z.X = 9.0 and Copy.Z.Y = 10.0);
         Assert_True (Copy.Z.Z = 11.0 and Copy.Z.W = 12.0);
         Assert_True (Copy.W.X = 13.0 and Copy.W.Y = 14.0);
         Assert_True (Copy.W.Z = 15.0 and Copy.W.W = 16.0);
      end;
   end Test_Matrix_Copy_And_Free;

begin
   Glib.Test.Init;
   Add_Func ("/graphene/config/vector-copy-free",
             Test_Vector_Copy_And_Free'Unrestricted_Access);
   Add_Func ("/graphene/config/matrix-copy-free",
             Test_Matrix_Copy_And_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
