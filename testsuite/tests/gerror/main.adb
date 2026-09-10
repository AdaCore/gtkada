--  Port of glib's testsuite/c_tests/error.c, narrowed to what Glib.Error
--  actually binds (src/glib-error.ads): Error_New (g_error_new_literal),
--  Error_Copy, Error_Matches, Get_Domain, Get_Code, Get_Message and
--  Error_Free. The C original also exercises g_set_error(_literal),
--  g_propagate_error, g_prefix_error(_literal), g_clear_error, extended
--  error domains (G_DEFINE_EXTENDED_ERROR) and the g_test_expect_message /
--  g_test_trap_subprocess assertion machinery -- none of which GtkAda
--  binds, so those cases (test_overwrite, test_prefix*, test_clear,
--  test_new_valist_invalid*, test_extended*) have no Ada equivalent here.
--
--  No widget is created, so this test needs no display.

with Ada.Command_Line;

with Glib;       use Glib;
with Glib.Error; use Glib.Error;
with Glib.Test;  use Glib.Test;

procedure Main is

   Domain_A : constant GQuark := Quark_From_String ("gtkada-test-error-a");
   Domain_B : constant GQuark := Quark_From_String ("gtkada-test-error-b");
   Code_A   : constant Gint := 0;
   Code_B   : constant Gint := 1;

   procedure Test_Literal
   with Convention => C;

   procedure Test_Copy
   with Convention => C;

   procedure Test_Matches
   with Convention => C;

   ------------------
   -- Test_Literal --
   ------------------

   procedure Test_Literal is
      Error : constant GError := Error_New (Domain_A, Code_A, "%s %d %x");
   begin
      Assert_Error (Error, Domain_A, Code_A);
      Assert_Cmpstr_Eq (Get_Message (Error), "%s %d %x");
      Error_Free (Error);
   end Test_Literal;

   ---------------
   -- Test_Copy --
   ---------------

   procedure Test_Copy is
      Error : constant GError := Error_New (Domain_A, Code_A, "%s %d %x");
      Copy  : constant GError := Error_Copy (Error);
   begin
      Assert_Error (Copy, Domain_A, Code_A);
      Assert_Cmpstr_Eq (Get_Message (Copy), "%s %d %x");

      Error_Free (Error);
      Error_Free (Copy);
   end Test_Copy;

   ------------------
   -- Test_Matches --
   ------------------

   procedure Test_Matches is
      Error : constant GError := Error_New (Domain_A, Code_A, "Oh no!");
   begin
      Assert_True (Error_Matches (Error, Domain_A, Code_A));
      Assert_False (Error_Matches (null, Domain_A, Code_A));
      Assert_False (Error_Matches (Error, Domain_B, Code_A));
      Assert_False (Error_Matches (Error, Domain_A, Code_B));

      Error_Free (Error);
   end Test_Matches;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func ("/error/literal", Test_Literal'Unrestricted_Access);
   Glib.Test.Add_Func ("/error/copy", Test_Copy'Unrestricted_Access);
   Glib.Test.Add_Func ("/error/matches", Test_Matches'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
