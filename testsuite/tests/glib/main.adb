--  Exercise Glib's scalar conversions, quarks, type-name helpers, properties,
--  boxed-type registration, and utility functions. No focused C test for
--  these APIs exists in testsuite/c_tests, so cases check documented behavior.

with Ada.Command_Line;
with System;

with Glib;      use Glib;
with Glib.Test; use Glib.Test;
with Glib.Utils;
with Glib_Boxed_Callbacks;

procedure Main is
   use type Glib.GQuark;
   use type Glib.GType;
   use type System.Address;

   --  Call the public GLib operations so registration's callbacks are tested
   --  through the type created by GtkAda's wrapper.
   function Copy_Boxed_Value
     (Boxed_Type : Glib.GType; Boxed : System.Address)
      return System.Address;
   pragma Import (C, Copy_Boxed_Value, "g_boxed_copy");

   procedure Free_Boxed_Value
     (Boxed_Type : Glib.GType; Boxed : System.Address);
   pragma Import (C, Free_Boxed_Value, "g_boxed_free");

   procedure Test_Conversions with Convention => C;
   procedure Test_Quarks with Convention => C;
   procedure Test_Types with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Boxed_Type with Convention => C;
   procedure Test_Utils with Convention => C;

   --  Cover false and true conversions, including GLib's nonzero truth rule.
   procedure Test_Conversions is
      Input  : constant Gboolean_Array := (0 => 0, 1 => 1, 2 => -1);
      Output : constant Boolean_Array := To_Boolean_Array (Input);
   begin
      Assert_True (Output = (0 => False, 1 => True, 2 => True));
      Assert_True (To_Gint (False) = 0);
      Assert_True (To_Gint (True) = 1);
   end Test_Conversions;

   --  A try lookup is unknown before insertion, then matches the created key.
   procedure Test_Quarks is
      Key : constant String := "gtkada-coverage-glib-quark-20261111";
   begin
      Assert_True (Quark_Try_String (Key) = Unknown_Quark);
      Assert_True (Quark_From_String (Key) /= Unknown_Quark);
      Assert_True (Quark_Try_String (Key) = Quark_From_String (Key));
   end Test_Quarks;

   --  Type zero has no name, while registered fundamental types round-trip.
   procedure Test_Types is
      Name : constant String := Type_Name (GType_String);
   begin
      Assert_True (Type_Name (GType_Invalid) = "");
      Assert_True (Name'Length > 0);
      Assert_True (Type_From_Name (Name) = GType_String);
      Assert_True
        (Type_From_Name ("GtkAdaCoverageMissingType20261111") = GType_Invalid);
   end Test_Types;

   --  Build appends NUL only when needed; Property_Name preserves that byte.
   procedure Test_Properties is
      With_Append : constant Property := Build ("gtkada-test-property");
      Already_Terminated : constant Property :=
        Build ("gtkada-test-terminated" & ASCII.NUL);
   begin
      Assert_True
        (Property_Name (With_Append) = "gtkada-test-property" & ASCII.NUL);
      Assert_True
        (Property_Name (Already_Terminated) =
           "gtkada-test-terminated" & ASCII.NUL);
   end Test_Properties;

   --  Register a fresh boxed type, then check its name and callback behavior.
   procedure Test_Boxed_Type is
      Registered : constant GType := Boxed_Type_Register_Static
        ("GtkAdaCoverageBoxed20261111",
         Glib_Boxed_Callbacks.Copy_Boxed'Access,
         Glib_Boxed_Callbacks.Free_Boxed'Access);
      Sample : aliased Integer := 42;
      Copy   : System.Address;
   begin
      Assert_True (Registered /= GType_Invalid);
      Assert_True (Type_Name (Registered) = "GtkAdaCoverageBoxed20261111");
      Assert_True (Type_From_Name ("GtkAdaCoverageBoxed20261111") = Registered);

      Copy := Copy_Boxed_Value (Registered, Sample'Address);
      Assert_True (Copy = Sample'Address);
      Assert_True (Glib_Boxed_Callbacks.Copy_Calls = 1);
      Free_Boxed_Value (Registered, Copy);
      Assert_True (Glib_Boxed_Callbacks.Free_Calls = 1);
   end Test_Boxed_Type;

   --  The compile-time constants describe the GLib version used to build
   --  GtkAda; that version is accepted by the runtime compatibility check.
   --  A later major version is incompatible and must produce a diagnostic.
   procedure Test_Utils is
      Major : constant Guint := Guint (Glib.Utils.Major_Version);
      Minor : constant Guint := Guint (Glib.Utils.Minor_Version);
      Micro : constant Guint := Guint (Glib.Utils.Micro_Version);
   begin
      Assert_Cmpint_Eq (Glib.Utils.Major_Version, 2);
      Assert_Cmpint_Eq (Glib.Utils.Minor_Version, 88);
      Assert_Cmpint_Eq (Glib.Utils.Micro_Version, 0);
      Assert_Cmpstr_Eq (Glib.Utils.Check_Version (Major, Minor, Micro), "");
      Assert_True (Glib.Utils.Check_Version (Major + 1, 0, 0)'Length > 0);

      --  GLib chooses HOME or the account database; either way this must
      --  return a usable directory string in the test environment.
      Assert_True (Glib.Utils.Get_Home_Dir'Length > 0);
   end Test_Utils;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/glib/conversions", Test_Conversions'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib/quarks", Test_Quarks'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib/types", Test_Types'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib/boxed-type", Test_Boxed_Type'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib/utils", Test_Utils'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
