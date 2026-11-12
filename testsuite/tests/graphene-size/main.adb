--  Graphene.Size test for GtkAda's copy-and-free helper.
--  No matching graphene_size C test exists in testsuite/c_tests/ to port.
--  Equal is omitted because its generated binding uses the wrong C bool type.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;      use Glib.Test;
with Graphene.Size;  use Graphene.Size;
with Interfaces.C;   use Interfaces.C;

procedure Main is
   --  From_Object_Free calls g_free, so its temporary record must come from
   --  GLib's allocator rather than an Ada allocator.
   type Size_Access is access all Graphene_Size_T;
   function To_Size_Access is new Ada.Unchecked_Conversion
     (System.Address, Size_Access);
   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Check_Float (Actual, Expected : C_float) is
   begin
      --  Graphene stores size components as single-precision floats.
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual), Glib.Gdouble (Expected), 0.0001);
   end Check_Float;

   --  From_Object_Free must preserve the copied components and consume the
   --  GLib-allocated input record.
   procedure Test_Copy_And_Free with Convention => C;
   procedure Test_Copy_And_Free is
      Bytes : constant size_t :=
        size_t (Graphene_Size_T'Size / System.Storage_Unit);
      Heap_Size : constant Size_Access :=
        To_Size_Access (G_Malloc (Bytes));
   begin
      --  Distinct nonzero dimensions make both fields observable after free.
      Heap_Size.all := (Width => -4.5, Height => 8.25);
      declare
         Copy : aliased Graphene_Size_T := From_Object_Free (Heap_Size);
      begin
         Check_Float (Copy.Width, -4.5);
         Check_Float (Copy.Height, 8.25);
      end;
   end Test_Copy_And_Free;

begin
   Glib.Test.Init;
   Add_Func
     ("/graphene/size/copy-free",
      Test_Copy_And_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
