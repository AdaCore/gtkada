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

   ---------------
   -- Functions --
   ---------------

   function Run return Glib.Gint;
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
   --  @return 0 on success, 1 on failure (assuming it returns at all), 0 or
   --  77 if all tests were skipped with Glib.Test.Skip

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
