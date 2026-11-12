--  Exercise Pango.Matrix's generated wrapper conversion. There is no matching
--  pango_matrix test in the C testsuite; this checks GtkAda's internal helper.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib;      use Glib;
with Glib.Test; use Glib.Test;
with Pango.Matrix; use Pango.Matrix;

procedure Main is
   --  The helper frees its wrapper with g_free, so allocate compatible memory.
   type Matrix_Access is access all Pango_Matrix;
   function To_Matrix_Access is new Ada.Unchecked_Conversion
     (System.Address, Matrix_Access);

   function G_Malloc (Size : Gsize) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Test_From_Object_Free with Convention => C;

   --------------------------
   -- Test_From_Object_Free --
   --------------------------

   --  Verify all six fields survive the conversion that consumes its wrapper.
   procedure Test_From_Object_Free is
      Original : constant Pango_Matrix :=
        (Xx => 1.25, Xy => -2.5, Yx => 3.75, Yy => 4.5, X0 => -6.25, Y0 => 8.0);
      Bytes : constant Gsize :=
        Gsize ((Pango_Matrix'Size + System.Storage_Unit - 1)
               / System.Storage_Unit);
      Wrapper : constant Matrix_Access :=
        To_Matrix_Access (G_Malloc (Bytes));
      Converted : Pango_Matrix;
   begin
      --  Give the helper a fully initialized C-layout record before ownership
      --  transfers to it; afterward only the returned Ada value is inspected.
      Wrapper.all := Original;
      Converted := From_Object_Free (Wrapper);
      Assert_Cmpfloat_Eq (Converted.Xx, Original.Xx);
      Assert_Cmpfloat_Eq (Converted.Xy, Original.Xy);
      Assert_Cmpfloat_Eq (Converted.Yx, Original.Yx);
      Assert_Cmpfloat_Eq (Converted.Yy, Original.Yy);
      Assert_Cmpfloat_Eq (Converted.X0, Original.X0);
      Assert_Cmpfloat_Eq (Converted.Y0, Original.Y0);
   end Test_From_Object_Free;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/pango/matrix/from-object-free",
      Test_From_Object_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
