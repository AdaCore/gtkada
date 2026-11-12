--  Exercise Graphene quaternion equality and GtkAda's copy-and-free helper.
--  No graphene_quaternion test is present in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;              use Glib.Test;
with Graphene.Quaternion;     use Graphene.Quaternion;
with Interfaces.C;           use Interfaces.C;

procedure Main is
   --  From_Object_Free releases its input with g_free, so allocate the
   --  temporary quaternion with GLib's allocator rather than an Ada allocator.
   type Quaternion_Access is access all Graphene_Quaternion_T;
   function To_Quaternion_Access is new Ada.Unchecked_Conversion
     (System.Address, Quaternion_Access);
   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   --  Confirm both equal and unequal values through the public predicate.
   procedure Test_Equality with Convention => C;
   procedure Test_Equality is
      A, Same, Different : aliased Graphene_Quaternion_T;
      Ignored : access Graphene_Quaternion_T;
   begin
      --  Identical components compare equal; changing one component is enough
      --  to make the quaternions unequal.
      Ignored := Init (A'Access, 1.0, -2.0, 3.5, 0.25);
      Ignored := Init (Same'Access, 1.0, -2.0, 3.5, 0.25);
      Ignored := Init (Different'Access, 1.0, -2.0, 3.5, 0.5);
      Assert_True (Equal (A'Access, Same'Access));
      Assert_False (Equal (A'Access, Different'Access));
   end Test_Equality;

   --  Ensure the helper returns a value copy before freeing the GLib buffer.
   procedure Test_Copy_And_Free with Convention => C;
   procedure Test_Copy_And_Free is
      Bytes : constant size_t :=
        size_t (Graphene_Quaternion_T'Size / System.Storage_Unit);
      Heap_Quaternion : constant Quaternion_Access :=
        To_Quaternion_Access (G_Malloc (Bytes));
      Expected : aliased Graphene_Quaternion_T;
      Ignored : access Graphene_Quaternion_T;
   begin
      --  Different component values verify that the full quaternion survives
      --  the copy, and the expected value stays independent of the freed data.
      Ignored := Init (Heap_Quaternion, -4.5, 8.25, 2.75, -1.5);
      Ignored := Init (Expected'Access, -4.5, 8.25, 2.75, -1.5);
      declare
         Copy : aliased Graphene_Quaternion_T :=
           From_Object_Free (Heap_Quaternion);
      begin
         Assert_True (Equal (Copy'Access, Expected'Access));
      end;
   end Test_Copy_And_Free;

begin
   Glib.Test.Init;
   Add_Func ("/graphene/quaternion/equality", Test_Equality'Unrestricted_Access);
   Add_Func
     ("/graphene/quaternion/copy-free",
      Test_Copy_And_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
