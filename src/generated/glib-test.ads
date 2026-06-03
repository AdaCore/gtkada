------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------


pragma Warnings (Off, "*is already use-visible*");
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.Source_Info;
with Glib.Error;
with System;

package Glib.Test is

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Test_Function is access procedure;
   pragma Convention (C, Test_Function);
   --  A test function with no fixture, registered via Add_Func.

   type Test_Data_Function is access procedure (User_Data : System.Address);
   pragma Convention (C, Test_Data_Function);
   --  A test function that receives the user-data pointer passed to
   --  Add_Data_Func.

   procedure Init;
   --  Initialize the GLib testing framework, parsing the test-related
   --  command-line arguments visible through Ada.Command_Line (e.g.
   --  `-p`, `--seed=N`, `--verbose`). Must be called before any other
   --  subprogram in this package.

   procedure Message (Msg : UTF8_String);
   --  Add Msg to the test report.

   ---------------------------------------------------------------------
   --  Assertions
   --
   --  These mirror the g_assert_* macros of the GLib testing framework.
   --  Each evaluates its condition and, on failure, reports through GLib
   --  and aborts the current test (unless non-fatal assertions have been
   --  enabled on the C side with g_test_set_nonfatal_assertions).
   --
   --  The File, Line and Func parameters default to the call site through
   --  GNAT.Source_Info and should normally be left implicit; they make the
   --  reported failure point at the offending line in the test, exactly as
   --  __FILE__/__LINE__ do for the C macros. Expr, where present, is an
   --  optional textual description of the checked expression.
   ---------------------------------------------------------------------

   procedure Assert
     (Condition : Boolean;
      Expr      : UTF8_String := "";
      File      : UTF8_String := GNAT.Source_Info.File;
      Line      : Natural     := GNAT.Source_Info.Line;
      Func      : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   --  g_assert: fail unless Condition is True.

   procedure Assert_True
     (Condition : Boolean;
      Expr      : UTF8_String := "";
      File      : UTF8_String := GNAT.Source_Info.File;
      Line      : Natural     := GNAT.Source_Info.Line;
      Func      : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   --  g_assert_true: fail unless Condition is True.

   procedure Assert_False
     (Condition : Boolean;
      Expr      : UTF8_String := "";
      File      : UTF8_String := GNAT.Source_Info.File;
      Line      : Natural     := GNAT.Source_Info.Line;
      Func      : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   --  g_assert_false: fail unless Condition is False.

   procedure Assert_Null
     (Object : System.Address;
      Expr   : UTF8_String := "";
      File   : UTF8_String := GNAT.Source_Info.File;
      Line   : Natural     := GNAT.Source_Info.Line;
      Func   : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   --  g_assert_null: fail unless Object is the null address.

   procedure Assert_Nonnull
     (Object : System.Address;
      Expr   : UTF8_String := "";
      File   : UTF8_String := GNAT.Source_Info.File;
      Line   : Natural     := GNAT.Source_Info.Line;
      Func   : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   --  g_assert_nonnull: fail if Object is the null address.

   procedure Assert_Not_Reached
     (File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   pragma No_Return (Assert_Not_Reached);
   --  g_assert_not_reached: fail unconditionally; flags code that should
   --  never be executed.

   --  Signed integer comparisons (g_assert_cmpint), one per operator.

   procedure Assert_Cmpint_Eq
     (N1, N2 : Glib.Gint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpint_Ne
     (N1, N2 : Glib.Gint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpint_Lt
     (N1, N2 : Glib.Gint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpint_Le
     (N1, N2 : Glib.Gint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpint_Gt
     (N1, N2 : Glib.Gint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpint_Ge
     (N1, N2 : Glib.Gint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);

   --  Unsigned integer comparisons (g_assert_cmpuint), one per operator.

   procedure Assert_Cmpuint_Eq
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpuint_Ne
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpuint_Lt
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpuint_Le
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpuint_Gt
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpuint_Ge
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);

   --  Hexadecimal unsigned comparisons (g_assert_cmphex); identical to
   --  Assert_Cmpuint_* but the failure message prints the operands in hex.

   procedure Assert_Cmphex_Eq
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmphex_Ne
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmphex_Lt
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmphex_Le
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmphex_Gt
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmphex_Ge
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);

   --  Floating-point comparisons (g_assert_cmpfloat), one per operator.

   procedure Assert_Cmpfloat_Eq
     (N1, N2 : Glib.Gdouble;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpfloat_Ne
     (N1, N2 : Glib.Gdouble;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpfloat_Lt
     (N1, N2 : Glib.Gdouble;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpfloat_Le
     (N1, N2 : Glib.Gdouble;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpfloat_Gt
     (N1, N2 : Glib.Gdouble;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpfloat_Ge
     (N1, N2 : Glib.Gdouble;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);

   procedure Assert_Cmpfloat_With_Epsilon
     (N1, N2, Epsilon : Glib.Gdouble;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   --  g_assert_cmpfloat_with_epsilon: fail unless abs (N1 - N2) < Epsilon.

   --  String comparisons (g_assert_cmpstr), lexicographic, one per operator.

   procedure Assert_Cmpstr_Eq
     (S1, S2 : UTF8_String;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpstr_Ne
     (S1, S2 : UTF8_String;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpstr_Lt
     (S1, S2 : UTF8_String;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpstr_Le
     (S1, S2 : UTF8_String;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpstr_Gt
     (S1, S2 : UTF8_String;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   procedure Assert_Cmpstr_Ge
     (S1, S2 : UTF8_String;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity);

   --  Error assertions.

   procedure Assert_No_Error
     (Error : Glib.Error.GError;
      Expr  : UTF8_String := "";
      File  : UTF8_String := GNAT.Source_Info.File;
      Line  : Natural     := GNAT.Source_Info.Line;
      Func  : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   --  g_assert_no_error: fail unless Error is null.

   procedure Assert_Error
     (Error  : Glib.Error.GError;
      Domain : Glib.GQuark;
      Code   : Glib.Gint;
      Expr   : UTF8_String := "";
      File   : UTF8_String := GNAT.Source_Info.File;
      Line   : Natural     := GNAT.Source_Info.Line;
      Func   : UTF8_String := GNAT.Source_Info.Enclosing_Entity);
   --  g_assert_error: fail unless Error is non-null and matches Domain/Code.

   ---------------
   -- Functions --
   ---------------

   function Run return Ada.Command_Line.Exit_Status;
   pragma Import (C, Run, "g_test_run");
   --  Runs all tests under the toplevel suite which can be retrieved with
   --  g_test_get_root. Similar to g_test_run_suite, the test cases to be run
   --  are filtered according to test path arguments (`-p testpath`) as parsed
   --  by g_test_init. g_test_run_suite or Glib.Test.Run may only be called
   --  once in a program.
   --  In general, the tests and sub-suites within each suite are run in the
   --  order in which they are defined. However, note that prior to GLib 2.36,
   --  there was a bug in the `g_test_add_*` functions which caused them to
   --  create multiple suites with the same name, meaning that if you created
   --  tests "/foo/simple", "/bar/simple", and "/foo/using-bar" in that order,
   --  they would get run in that order (since Glib.Test.Run would run the
   --  first "/foo" suite, then the "/bar" suite, then the second "/foo"
   --  suite). As of 2.36, this bug is fixed, and adding the tests in that
   --  order would result in a running order of "/foo/simple",
   --  "/foo/using-bar", "/bar/simple". If this new ordering is sub-optimal
   --  (because it puts more-complicated tests before simpler ones, making it
   --  harder to figure out exactly what has failed), you can fix it by
   --  changing the test paths to group tests by suite in a way that will
   --  result in the desired running order. Eg, "/simple/foo", "/simple/bar",
   --  "/complex/foo-using-bar".
   --  However, you should never make the actual result of a test depend on
   --  the order that tests are run in. If you need to ensure that some
   --  particular code runs before or after a given test case, use g_test_add,
   --  which lets you specify setup and teardown functions.
   --  If all tests are skipped, this function will return 0 if producing TAP
   --  output, or 77 (treated as "skip test" by Automake) otherwise.
   --  Since: gtk+ 2.16

   procedure Fail;
   pragma Import (C, Fail, "g_test_fail");
   --  Indicates that a test failed. This function can be called multiple
   --  times from the same test. You can use this function if your test failed
   --  in a recoverable way.
   --  Do not use this function if the failure of a test could cause other
   --  tests to malfunction.
   --  Calling this function will not stop the test from running, you need to
   --  return from the test function yourself. So you can produce additional
   --  diagnostic messages or even continue running the test.
   --  If not called from inside a test, this function does nothing.
   --  Since: gtk+ 2.30

   function Failed return Boolean;
   --  Returns whether a test has already failed. This will be the case when
   --  Glib.Test.Fail, g_test_incomplete or Glib.Test.Skip have been called,
   --  but also if an assertion has failed.
   --  This can be useful to return early from a test if continuing after a
   --  failed assertion might be harmful.
   --  The return value of this function is only meaningful if it is called
   --  from inside a test function.
   --  Since: gtk+ 2.38
   --  @return True if the test has failed

   procedure Skip (Msg : UTF8_String := "");
   --  Indicates that a test was skipped.
   --  Calling this function will not stop the test from running, you need to
   --  return from the test function yourself. So you can produce additional
   --  diagnostic messages or even continue running the test.
   --  If not called from inside a test, this function does nothing.
   --  Since: gtk+ 2.38
   --  @param Msg explanation

   procedure Add_Func (Testpath : UTF8_String; Test_Func : Test_Function);
   --  Create a new test case, similar to g_test_create_case. However the test
   --  is assumed to use no fixture, and test suites are automatically created
   --  on the fly and added to the root fixture, based on the slash-separated
   --  portions of Testpath.
   --  If Testpath includes the component "subprocess" anywhere in it, the
   --  test will be skipped by default, and only run if explicitly required via
   --  the `-p` command-line option or g_test_trap_subprocess.
   --  Since: gtk+ 2.16
   --  @param Testpath /-separated test case path name for the test.
   --  @param Test_Func The test function to invoke for this test.

   procedure Add_Data_Func
      (Testpath  : UTF8_String;
       Test_Data : System.Address;
       Test_Func : Test_Data_Function);
   --  Create a new test case, similar to g_test_create_case. However the test
   --  is assumed to use no fixture, and test suites are automatically created
   --  on the fly and added to the root fixture, based on the slash-separated
   --  portions of Testpath. The Test_Data argument will be passed as first
   --  argument to Test_Func.
   --  If Testpath includes the component "subprocess" anywhere in it, the
   --  test will be skipped by default, and only run if explicitly required via
   --  the `-p` command-line option or g_test_trap_subprocess.
   --  Since: gtk+ 2.16
   --  @param Testpath /-separated test case path name for the test.
   --  @param Test_Data Test data argument for the test function.
   --  @param Test_Func The test function to invoke for this test.

end Glib.Test;
