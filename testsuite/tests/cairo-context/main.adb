--  Exercise the Cairo context wrappers implemented in src/cairo.adb.
--  No matching cairo context wrapper tests are present in testsuite/c_tests/.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;

with Cairo;
with Cairo.Image_Surface;
with Cairo.Surface;
with Glib; use Glib;
with Glib.Test; use Glib.Test;

use type Cairo.Cairo_Status;
use type Cairo.Dash_Array_Access;

procedure Main is

   --  Free the dash array allocated by Cairo.Get_Dash after checking it.
   procedure Free_Dashes is new Ada.Unchecked_Deallocation
     (Cairo.Dash_Array, Cairo.Dash_Array_Access);

   --  Both disabled and active dash patterns round-trip through Cairo.
   procedure Test_Dashes
   with Convention => C;

   --  Path hit tests and current-point queries describe the constructed path.
   procedure Test_Path_Queries
   with Convention => C;

   --  Text selection and rendering helpers accept UTF-8 strings and succeed.
   procedure Test_Text
   with Convention => C;

   -----------------
   -- Test_Dashes --
   -----------------

   procedure Test_Dashes is
      Surface : constant Cairo.Cairo_Surface :=
        Cairo.Image_Surface.Create
          (Cairo.Image_Surface.Cairo_Format_ARGB32, 64, 64);
      Cr      : constant Cairo.Cairo_Context := Cairo.Create (Surface);
      Dashes  : Cairo.Dash_Array_Access;
      Offset  : Gdouble;
      Pattern : constant Cairo.Dash_Array (3 .. 4) := (4.0, 2.0);
   begin
      Assert_True (Cairo.Status (Cr) = Cairo.Cairo_Status_Success);

      --  The empty pattern disables dashing; Get_Dash reports null and zero.
      Cairo.Set_Dash (Cr, Cairo.No_Dashes, 3.0);
      Assert_Cmpint_Eq (Cairo.Get_Dash_Count (Cr), 0);
      Cairo.Get_Dash (Cr, Dashes, Offset);
      Assert_True (Dashes = null);
      Assert_Cmpfloat_Eq (Offset, 0.0);

      --  A nonempty pattern retains its values and offset in user units.
      Cairo.Set_Dash (Cr, Pattern, 1.5);
      Assert_Cmpint_Eq (Cairo.Get_Dash_Count (Cr), 2);
      Cairo.Get_Dash (Cr, Dashes, Offset);
      Assert_True (Dashes /= null);
      Assert_Cmpint_Eq (Dashes'Length, 2);
      Assert_Cmpfloat_Eq (Dashes (1), 4.0);
      Assert_Cmpfloat_Eq (Dashes (2), 2.0);
      Assert_Cmpfloat_Eq (Offset, 1.5);
      Free_Dashes (Dashes);

      Cairo.Destroy (Cr);
      Cairo.Surface.Destroy (Surface);
   end Test_Dashes;

   -----------------------
   -- Test_Path_Queries --
   -----------------------

   procedure Test_Path_Queries is
      Surface : constant Cairo.Cairo_Surface :=
        Cairo.Image_Surface.Create
          (Cairo.Image_Surface.Cairo_Format_ARGB32, 64, 64);
      Cr      : constant Cairo.Cairo_Context := Cairo.Create (Surface);
   begin
      Assert_True (not Cairo.Has_Current_Point (Cr));
      Cairo.Move_To (Cr, 10.0, 10.0);
      Assert_True (Cairo.Has_Current_Point (Cr));
      Cairo.Line_To (Cr, 50.0, 10.0);
      Cairo.Set_Line_Width (Cr, 4.0);

      --  The line's center is stroked while a point well away from it is not.
      Assert_True (Cairo.In_Stroke (Cr, 30.0, 10.0));
      Assert_True (not Cairo.In_Stroke (Cr, 30.0, 20.0));

      Cairo.New_Path (Cr);
      Cairo.Rectangle (Cr, 10.0, 10.0, 20.0, 20.0);
      Assert_True (Cairo.In_Fill (Cr, 20.0, 20.0));
      Assert_True (not Cairo.In_Fill (Cr, 40.0, 40.0));
      Assert_True (Cairo.Has_Current_Point (Cr));

      Cairo.Destroy (Cr);
      Cairo.Surface.Destroy (Surface);
   end Test_Path_Queries;

   ---------------
   -- Test_Text --
   ---------------

   procedure Test_Text is
      Surface : constant Cairo.Cairo_Surface :=
        Cairo.Image_Surface.Create
          (Cairo.Image_Surface.Cairo_Format_ARGB32, 64, 64);
      Cr      : constant Cairo.Cairo_Context := Cairo.Create (Surface);
      X       : aliased Gdouble;
      Y       : aliased Gdouble;
   begin
      Cairo.Select_Font_Face
        (Cr, "sans-serif", Cairo.Cairo_Font_Slant_Normal,
         Cairo.Cairo_Font_Weight_Bold);
      Cairo.Set_Font_Size (Cr, 12.0);
      Cairo.Move_To (Cr, 3.0, 20.0);
      Cairo.Show_Text (Cr, "Cairo");

      --  Show_Text advances the current point by the rendered text width.
      Cairo.Get_Current_Point (Cr, X'Access, Y'Access);
      Assert_True (X > 3.0);
      Assert_Cmpfloat_Eq (Y, 20.0);
      Assert_True (Cairo.Status (Cr) = Cairo.Cairo_Status_Success);

      Cairo.New_Path (Cr);
      Cairo.Move_To (Cr, 3.0, 40.0);
      Cairo.Text_Path (Cr, "Path");
      Assert_True (Cairo.Status (Cr) = Cairo.Cairo_Status_Success);

      Cairo.Destroy (Cr);
      Cairo.Surface.Destroy (Surface);
   end Test_Text;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/cairo/context/dashes", Test_Dashes'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cairo/context/path-queries", Test_Path_Queries'Unrestricted_Access);
   Glib.Test.Add_Func ("/cairo/context/text", Test_Text'Unrestricted_Access);

   --  Return the GLib test runner's status to the testsuite driver.
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
