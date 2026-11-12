--  Graphene.Plane tests for plane equality and GtkAda's copy-and-free helper.
--  No matching plane test exists in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib.Test;        use Glib.Test;
with Graphene.Plane;   use Graphene.Plane;
with Graphene.Vec3;    use Graphene.Vec3;
with Interfaces.C;     use Interfaces.C;

procedure Main is
   --  From_Object_Free releases with g_free, so allocate its input with
   --  GLib's allocator rather than with an Ada allocator.
   type Plane_Access is access all Graphene_Plane_T;
   function To_Plane_Access is new Ada.Unchecked_Conversion
     (System.Address, Plane_Access);
   function G_Malloc (Size : size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   --  The plane stores C floats, so compare values with a small tolerance.
   procedure Check_Float (Actual, Expected : C_float) is
   begin
      Assert_Cmpfloat_With_Epsilon
        (Glib.Gdouble (Actual), Glib.Gdouble (Expected), 0.0001);
   end Check_Float;

   --  Equal must accept matching values and reject a changed constant.
   procedure Test_Equality with Convention => C;
   procedure Test_Equality is
      Normal : aliased Graphene_Vec3_T;
      First, Same, Different : aliased Graphene_Plane_T;
      Ignored_Normal : access Graphene_Vec3_T;
      Ignored_Plane : access Graphene_Plane_T;
   begin
      --  Use distinct plane records with a shared unit normal and constant.
      Ignored_Normal := Init (Normal'Access, 0.0, 1.0, 0.0);
      Ignored_Plane := Init (First'Access, Normal'Access, 2.5);
      Ignored_Plane := Init (Same'Access, Normal'Access, 2.5);
      Ignored_Plane := Init (Different'Access, Normal'Access, 3.5);
      Assert_True (Equal (First'Access, Same'Access));
      Assert_False (Equal (First'Access, Different'Access));
   end Test_Equality;

   --  From_Object_Free must return a value copy and release its GLib buffer.
   procedure Test_Copy_And_Free with Convention => C;
   procedure Test_Copy_And_Free is
      Normal : aliased Graphene_Vec3_T;
      Ignored_Normal : access Graphene_Vec3_T;
      Bytes : constant size_t :=
        size_t (Graphene_Plane_T'Size / System.Storage_Unit);
      Heap_Plane : constant Plane_Access :=
        To_Plane_Access (G_Malloc (Bytes));
      Ignored_Plane : access Graphene_Plane_T;
   begin
      --  Initialize the allocated record before the helper consumes it.
      Ignored_Normal := Init (Normal'Access, 1.0, 0.0, 0.0);
      Ignored_Plane := Init (Heap_Plane, Normal'Access, -4.25);
      declare
         Copy : aliased Graphene_Plane_T := From_Object_Free (Heap_Plane);
         Returned_Normal : aliased Graphene_Vec3_T;
      begin
         Get_Normal (Copy'Access, Returned_Normal'Access);
         Check_Float (Get_X (Returned_Normal'Access), 1.0);
         Check_Float (Get_Y (Returned_Normal'Access), 0.0);
         Check_Float (Get_Z (Returned_Normal'Access), 0.0);
         Check_Float (Get_Constant (Copy'Access), -4.25);
      end;
   end Test_Copy_And_Free;

begin
   Glib.Test.Init;
   Add_Func ("/graphene/plane/equality", Test_Equality'Unrestricted_Access);
   Add_Func
     ("/graphene/plane/copy-free", Test_Copy_And_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
