--  Exercise GtkAda's GLib test helpers, including comparison operators and
--  assertion failures captured in GLib test subprocesses.
with Ada.Command_Line;
with Interfaces.C;
with Glib;
with Glib.Test;
with Gtkada.Types;
with System;
with System.Storage_Elements;

procedure Main is
   use Glib.Test;
   use type System.Address;
   use type Glib.Gboolean;

   --  A non-null marker lets the data callback check what Add_Data_Func passed.
   Marker : constant System.Address :=
     System.Storage_Elements.To_Address (1);

   function Is_Subprocess return Glib.Gboolean;
   pragma Import (C, Is_Subprocess, "g_test_subprocess");

   procedure Trap_Subprocess
     (Testpath : Gtkada.Types.Chars_Ptr;
      Usec     : Interfaces.C.unsigned_long_long;
      Flags    : Interfaces.C.int);
   pragma Import (C, Trap_Subprocess, "g_test_trap_subprocess");

   function Trap_Has_Passed return Glib.Gboolean;
   pragma Import (C, Trap_Has_Passed, "g_test_trap_has_passed");

   procedure Set_Nonfatal_Assertions;
   pragma Import (C, Set_Nonfatal_Assertions, "g_test_set_nonfatal_assertions");

   procedure Test_Comparisons
   with Convention => C;

   procedure Test_Data (User_Data : System.Address)
   with Convention => C;

   procedure Test_Assertion_Failure
   with Convention => C;

   procedure Test_Error_Assertion_Failure
   with Convention => C;

   procedure Test_Not_Reached
   with Convention => C;

   --  Every comparator is given operands that satisfy its documented test.
   procedure Test_Comparisons is
   begin
      Assert_True (True);
      Assert_False (False);
      Assert_Null (System.Null_Address);
      Assert_Nonnull (Marker);
      Assert (True, "a true condition passes");
      Assert_False (Failed, "the current test has not failed");

      Assert_Cmpint_Eq (5, 5);
      Assert_Cmpint_Ne (5, 4);
      Assert_Cmpint_Lt (4, 5);
      Assert_Cmpint_Le (5, 5);
      Assert_Cmpint_Gt (5, 4);
      Assert_Cmpint_Ge (5, 5);

      --  Unsigned and hexadecimal comparisons cover both operand orderings.
      Assert_Cmpuint_Eq (5, 5);
      Assert_Cmpuint_Ne (5, 4);
      Assert_Cmpuint_Lt (4, 5);
      Assert_Cmpuint_Le (5, 5);
      Assert_Cmpuint_Gt (5, 4);
      Assert_Cmpuint_Ge (5, 5);
      Assert_Cmphex_Eq (16#A#, 16#A#);
      Assert_Cmphex_Ne (16#A#, 16#B#);
      Assert_Cmphex_Lt (16#A#, 16#B#);
      Assert_Cmphex_Le (16#A#, 16#A#);
      Assert_Cmphex_Gt (16#B#, 16#A#);
      Assert_Cmphex_Ge (16#A#, 16#A#);

      --  Float and string checks cover each relation, including equality edges.
      Assert_Cmpfloat_Eq (2.0, 2.0);
      Assert_Cmpfloat_Ne (2.0, 3.0);
      Assert_Cmpfloat_Lt (2.0, 3.0);
      Assert_Cmpfloat_Le (2.0, 2.0);
      Assert_Cmpfloat_Gt (3.0, 2.0);
      Assert_Cmpfloat_Ge (2.0, 2.0);
      Assert_Cmpfloat_With_Epsilon (1.0, 1.01, 0.02);
      Assert_Cmpstr_Eq ("same", "same");
      Assert_Cmpstr_Ne ("left", "right");
      Assert_Cmpstr_Lt ("a", "b");
      Assert_Cmpstr_Le ("a", "a");
      Assert_Cmpstr_Gt ("b", "a");
      Assert_Cmpstr_Ge ("a", "a");
      Assert_No_Error (null);
   end Test_Comparisons;

   --  The registered user data is checked in the callback itself.
   procedure Test_Data (User_Data : System.Address) is
   begin
      Assert (User_Data = Marker, "Add_Data_Func preserves its user data");
   end Test_Data;

   --  GLib re-executes this same test as a child; only the child asserts false.
   procedure Test_Assertion_Failure is
   begin
      if Is_Subprocess /= 0 then
         --  Keep the child alive long enough for its coverage trace to flush.
         Set_Nonfatal_Assertions;
         Assert (False, "the child is expected to fail");
      else
         Trap_Subprocess (Gtkada.Types.Null_Ptr, 0, 0);
         Assert_False
           (Trap_Has_Passed /= 0, "a failed assertion fails its subprocess");
      end if;
   end Test_Assertion_Failure;

   --  A null error cannot match a nonzero expected code, so GLib reports it.
   procedure Test_Error_Assertion_Failure is
   begin
      if Is_Subprocess /= 0 then
         --  Nonfatal mode records the assertion failure and exits normally.
         Set_Nonfatal_Assertions;
         Assert_Error (null, 0, 1);
      else
         Trap_Subprocess (Gtkada.Types.Null_Ptr, 0, 0);
         Assert_False
           (Trap_Has_Passed /= 0, "a mismatched error fails its subprocess");
      end if;
   end Test_Error_Assertion_Failure;

   --  Assert_Not_Reached has the same fatal behavior and is checked in a child.
   procedure Test_Not_Reached is
   begin
      if Is_Subprocess /= 0 then
         --  Nonfatal mode also makes the unconditional report return.
         Set_Nonfatal_Assertions;
         Assert_Not_Reached;
      else
         Trap_Subprocess (Gtkada.Types.Null_Ptr, 0, 0);
         Assert_False
           (Trap_Has_Passed /= 0, "not-reached fails its subprocess");
      end if;
   end Test_Not_Reached;

begin
   --  Initialize GLib before registering the ordinary and data-backed cases.
   Glib.Test.Init;
   Add_Func ("/glib-test/comparisons", Test_Comparisons'Unrestricted_Access);
   Add_Data_Func
     ("/glib-test/data", Marker, Test_Data'Unrestricted_Access);
   Add_Func
     ("/glib-test/assertion-failure",
      Test_Assertion_Failure'Unrestricted_Access);
   Add_Func
     ("/glib-test/error-assertion-failure",
      Test_Error_Assertion_Failure'Unrestricted_Access);
   Add_Func ("/glib-test/not-reached", Test_Not_Reached'Unrestricted_Access);

   --  Return GLib's result so an unexpected test failure fails this test.
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
