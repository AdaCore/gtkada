--  Exercise Cairo pattern constructors and their query and configuration API.
--  There is no matching cairo_pattern_* test in testsuite/c_tests/ to port.

with Ada.Command_Line;

with Cairo;
with Cairo.Image_Surface;
with Cairo.Matrix;
with Cairo.Pattern;
with Cairo.Surface;
with Glib; use Glib;
with Glib.Test; use Glib.Test;

use type Cairo.Cairo_Status;
use type Cairo.Pattern.Cairo_Pattern_Type;
use type Cairo.Pattern.Cairo_Extend;
use type Cairo.Pattern.Cairo_Filter;

procedure Main is

   --  Solid constructors report their color and preserve reference counts.
   procedure Test_Solid_Patterns
   with Convention => C;

   --  Surface and gradient constructors each identify the expected pattern.
   procedure Test_Pattern_Types
   with Convention => C;

   --  Linear and radial gradient queries return their defining geometry/stops.
   procedure Test_Gradient_Data
   with Convention => C;

   --  Extend, filter and transformation properties round-trip on a gradient.
   procedure Test_Pattern_Properties
   with Convention => C;

   -------------------------
   -- Test_Solid_Patterns --
   -------------------------

   procedure Test_Solid_Patterns is
      Pattern : Cairo.Cairo_Pattern;
      Red     : aliased Gdouble;
      Green   : aliased Gdouble;
      Blue    : aliased Gdouble;
      Alpha   : aliased Gdouble;
   begin
      Pattern := Cairo.Pattern.Create_Rgb (0.25, 0.5, 0.75);
      Assert_True (Cairo.Pattern.Status (Pattern) = Cairo.Cairo_Status_Success);
      Assert_True
        (Cairo.Pattern.Get_Type (Pattern) =
           Cairo.Pattern.Cairo_Pattern_Type_Solid);
      Assert_Cmpuint_Eq (Cairo.Pattern.Get_Reference_Count (Pattern), 1);
      Assert_True
        (Cairo.Pattern.Get_Rgba
           (Pattern, Red'Access, Green'Access, Blue'Access, Alpha'Access) =
             Cairo.Cairo_Status_Success);
      Assert_Cmpfloat_Eq (Red, 0.25);
      Assert_Cmpfloat_Eq (Green, 0.5);
      Assert_Cmpfloat_Eq (Blue, 0.75);
      Assert_Cmpfloat_Eq (Alpha, 1.0);

      --  Reference returns the same owned pattern with one additional ref.
      declare
         Extra_Reference : constant Cairo.Cairo_Pattern :=
           Cairo.Pattern.Reference (Pattern);
      begin
         Assert_Cmpuint_Eq (Cairo.Pattern.Get_Reference_Count (Pattern), 2);
         Cairo.Pattern.Destroy (Extra_Reference);
      end;
      Assert_Cmpuint_Eq (Cairo.Pattern.Get_Reference_Count (Pattern), 1);
      Cairo.Pattern.Destroy (Pattern);

      Pattern := Cairo.Pattern.Create_Rgba (0.1, 0.2, 0.3, 0.4);
      Assert_True
        (Cairo.Pattern.Get_Rgba
           (Pattern, Red'Access, Green'Access, Blue'Access, Alpha'Access) =
             Cairo.Cairo_Status_Success);
      Assert_Cmpfloat_Eq (Red, 0.1);
      Assert_Cmpfloat_Eq (Green, 0.2);
      Assert_Cmpfloat_Eq (Blue, 0.3);
      Assert_Cmpfloat_Eq (Alpha, 0.4);
      Cairo.Pattern.Destroy (Pattern);
   end Test_Solid_Patterns;

   ------------------------
   -- Test_Pattern_Types --
   ------------------------

   procedure Test_Pattern_Types is
      Surface : constant Cairo.Cairo_Surface :=
        Cairo.Image_Surface.Create
          (Cairo.Image_Surface.Cairo_Format_ARGB32, 8, 6);
      Pattern : Cairo.Cairo_Pattern;
   begin
      Assert_True (Cairo.Surface.Status (Surface) = Cairo.Cairo_Status_Success);
      Pattern := Cairo.Pattern.Create_For_Surface (Surface);
      Assert_True
        (Cairo.Pattern.Get_Type (Pattern) =
           Cairo.Pattern.Cairo_Pattern_Type_Surface);
      Cairo.Pattern.Destroy (Pattern);

      Pattern := Cairo.Pattern.Create_Linear (1.0, 2.0, 3.0, 4.0);
      Assert_True
        (Cairo.Pattern.Get_Type (Pattern) =
           Cairo.Pattern.Cairo_Pattern_Type_Linear);
      Cairo.Pattern.Destroy (Pattern);

      Pattern := Cairo.Pattern.Create_Radial (1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
      Assert_True
        (Cairo.Pattern.Get_Type (Pattern) =
           Cairo.Pattern.Cairo_Pattern_Type_Radial);
      Cairo.Pattern.Destroy (Pattern);
      Cairo.Surface.Destroy (Surface);
   end Test_Pattern_Types;

   -------------------------
   -- Test_Gradient_Data --
   -------------------------

   procedure Test_Gradient_Data is
      Pattern : Cairo.Cairo_Pattern :=
        Cairo.Pattern.Create_Linear (10.0, 20.0, 30.0, 40.0);
      X0      : aliased Gdouble;
      Y0      : aliased Gdouble;
      X1      : aliased Gdouble;
      Y1      : aliased Gdouble;
      Offset  : aliased Gdouble;
      Red     : aliased Gdouble;
      Green   : aliased Gdouble;
      Blue    : aliased Gdouble;
      Alpha   : aliased Gdouble;
      Count   : aliased Gint;
   begin
      Cairo.Pattern.Add_Color_Stop_Rgb (Pattern, 0.0, 1.0, 0.0, 0.0);
      Cairo.Pattern.Add_Color_Stop_Rgba (Pattern, 1.0, 0.0, 0.0, 1.0, 0.5);
      Assert_True
        (Cairo.Pattern.Get_Color_Stop_Count (Pattern, Count'Access) =
           Cairo.Cairo_Status_Success);
      Assert_Cmpint_Eq (Count, 2);
      Assert_True
        (Cairo.Pattern.Get_Color_Stop_Rgba
           (Pattern, 1, Offset'Access, Red'Access, Green'Access,
            Blue'Access, Alpha'Access) = Cairo.Cairo_Status_Success);
      Assert_Cmpfloat_Eq (Offset, 1.0);
      Assert_Cmpfloat_Eq (Red, 0.0);
      Assert_Cmpfloat_Eq (Green, 0.0);
      Assert_Cmpfloat_Eq (Blue, 1.0);
      Assert_Cmpfloat_Eq (Alpha, 0.5);

      Assert_True
        (Cairo.Pattern.Get_Linear_Points
           (Pattern, X0'Access, Y0'Access, X1'Access, Y1'Access) =
             Cairo.Cairo_Status_Success);
      Assert_Cmpfloat_Eq (X0, 10.0);
      Assert_Cmpfloat_Eq (Y0, 20.0);
      Assert_Cmpfloat_Eq (X1, 30.0);
      Assert_Cmpfloat_Eq (Y1, 40.0);
      Cairo.Pattern.Destroy (Pattern);

      Pattern := Cairo.Pattern.Create_Radial (2.0, 3.0, 4.0, 7.0, 8.0, 9.0);
      Assert_True
        (Cairo.Pattern.Get_Radial_Circles
           (Pattern, X0'Access, Y0'Access, Offset'Access,
            X1'Access, Y1'Access, Alpha'Access) = Cairo.Cairo_Status_Success);
      Assert_Cmpfloat_Eq (X0, 2.0);
      Assert_Cmpfloat_Eq (Y0, 3.0);
      Assert_Cmpfloat_Eq (Offset, 4.0);
      Assert_Cmpfloat_Eq (X1, 7.0);
      Assert_Cmpfloat_Eq (Y1, 8.0);
      Assert_Cmpfloat_Eq (Alpha, 9.0);
      Cairo.Pattern.Destroy (Pattern);
   end Test_Gradient_Data;

   ------------------------------
   -- Test_Pattern_Properties --
   ------------------------------

   procedure Test_Pattern_Properties is
      Pattern : constant Cairo.Cairo_Pattern :=
        Cairo.Pattern.Create_Linear (0.0, 0.0, 10.0, 0.0);
      Matrix  : aliased Cairo.Cairo_Matrix;
      Result  : aliased Cairo.Cairo_Matrix;
   begin
      --  Cairo supports these four extend modes on gradient patterns.
      for Mode in Cairo.Pattern.Cairo_Extend loop
         Cairo.Pattern.Set_Extend (Pattern, Mode);
         Assert_True (Cairo.Pattern.Get_Extend (Pattern) = Mode);
      end loop;

      --  Gaussian is documented as unimplemented, so test the usable filters.
      for Mode in Cairo.Pattern.Cairo_Filter range
        Cairo.Pattern.Cairo_Filter_Fast .. Cairo.Pattern.Cairo_Filter_Bilinear
      loop
         Cairo.Pattern.Set_Filter (Pattern, Mode);
         Assert_True (Cairo.Pattern.Get_Filter (Pattern) = Mode);
      end loop;

      Cairo.Matrix.Init (Matrix'Access, 2.0, 0.0, 0.0, 3.0, 7.0, 8.0);
      Cairo.Pattern.Set_Matrix (Pattern, Matrix'Access);
      Cairo.Pattern.Get_Matrix (Pattern, Result'Access);
      Assert_Cmpfloat_Eq (Result.Xx, 2.0);
      Assert_Cmpfloat_Eq (Result.Yy, 3.0);
      Assert_Cmpfloat_Eq (Result.X0, 7.0);
      Assert_Cmpfloat_Eq (Result.Y0, 8.0);
      Cairo.Pattern.Destroy (Pattern);
   end Test_Pattern_Properties;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/cairo-pattern/solid", Test_Solid_Patterns'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cairo-pattern/types", Test_Pattern_Types'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cairo-pattern/gradient-data", Test_Gradient_Data'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cairo-pattern/properties", Test_Pattern_Properties'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
