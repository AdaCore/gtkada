--  Graphene.Euler test for GtkAda's copy-and-free helper.
--  No graphene_euler test exists in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;       use Glib.Test;
with Graphene.Euler;  use Graphene.Euler;
with Interfaces.C;    use Interfaces.C;

procedure Main is
   --  From_Object_Free releases storage with g_free, so give it a GLib
   --  allocation that is large and aligned enough for the Euler record.
   type Euler_Access is access all Graphene_Euler_T;
   function To_Euler_Access is new Ada.Unchecked_Conversion
     (System.Address, Euler_Access);
   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Test_Copy_And_Free with Convention => C;
   procedure Test_Copy_And_Free is
      Bytes : constant size_t :=
        size_t (Graphene_Euler_T'Size / System.Storage_Unit);
      Heap_Euler : constant Euler_Access :=
        To_Euler_Access (G_Malloc (Bytes));
      Ignored : access Graphene_Euler_T;
   begin
      --  Copy nonzero angles and a nondefault order, then read the returned
      --  record through public accessors after its original allocation is freed.
      Ignored := Init_With_Order
        (Heap_Euler, 10.0, 20.0, 30.0, Graphene_Euler_Order_Zyx);
      declare
         Copy : aliased Graphene_Euler_T := From_Object_Free (Heap_Euler);
      begin
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Get_X (Copy'Access)), 10.0, 0.0001);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Get_Y (Copy'Access)), 20.0, 0.0001);
         Assert_Cmpfloat_With_Epsilon
           (Glib.Gdouble (Get_Z (Copy'Access)), 30.0, 0.0001);
         --  The deprecated Zyx order is returned as its canonical Szyx alias.
         Assert_True (Get_Order (Copy'Access) = Graphene_Euler_Order_Szyx);
      end;
   end Test_Copy_And_Free;

begin
   Glib.Test.Init;
   Add_Func
     ("/graphene/euler/copy-free",
      Test_Copy_And_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
