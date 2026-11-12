--  Exercise Glib.Module's path construction and dynamic module wrappers.
--  No focused C module test covers these wrappers; accessor-apis.c opens the
--  process module only to support unrelated GTK type accessor tests.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;
with Glib.Module;
with Glib.Test;
with System;

procedure Main is
   use Glib.Module;
   use Glib.Test;
   use type System.Address;
   use type G_Module;

   procedure Find_Symbol is new Generic_Module_Symbol (System.Address);

   procedure Test_Module_Supported_And_Path
   with Convention => C;
   procedure Test_Open_Symbol_And_Close
   with Convention => C;

   --  The path builder decorates a module name but does not require the file
   --  to exist. Check both the requested directory and undecorated name.
   procedure Test_Module_Supported_And_Path is
      Path : constant String := Module_Build_Path ("test-modules", "sample");
   begin
      Assert_True
        (Path'Length > 0
         and then Ada.Strings.Fixed.Index (Path, "test-modules") = Path'First,
         "built module path includes its directory");
      Assert_True
        (Ada.Strings.Fixed.Index (Path, "sample") > 0,
         "built module path includes the module name");
   end Test_Module_Supported_And_Path;

   --  Open GLib itself, then exercise both symbol lookup outcomes before
   --  closing the handle. GLib is already a required GtkAda runtime library.
   procedure Test_Open_Symbol_And_Close is
      Missing_File : constant String := Ada.Directories.Compose
        (Ada.Directories.Current_Directory, "gtkada-module-missing-for-test");
      Module       : G_Module;
      Missing      : G_Module;
      Symbol       : System.Address;
      Success      : Boolean;
   begin
      if Module_Supported then
         Module := Module_Open ("libglib-2.0.so.0");
         Assert_True (Module /= null);
         Assert_True (Module_Name (Module)'Length > 0);

         Find_Symbol (Module, "g_strdup", Symbol, Success);
         Assert_True (Success);
         Assert_Nonnull (Symbol);

         Find_Symbol
           (Module, "gtkada_symbol_that_does_not_exist_for_test",
            Symbol, Success);
         Assert_False (Success);
         Assert_True (Module_Error'Length > 0);
         Assert_True (Module_Close (Module));

         --  A path in the test's private working directory cannot name a
         --  system module; failure should leave an explanatory GLib error.
         Missing := Module_Open (Missing_File);
         Assert_True (Missing = null);
         Assert_True (Module_Error'Length > 0);
      else
         --  GtkAda's Windows wrappers intentionally report modules unsupported
         --  and return null for an open request.
         Missing := Module_Open ("libglib-2.0.so.0");
         Assert_True (Missing = null);
         Assert_True (Module_Error'Length > 0);
      end if;
   end Test_Open_Symbol_And_Close;

begin
   Glib.Test.Init;
   Add_Func ("/glib-module/supported-and-path",
             Test_Module_Supported_And_Path'Unrestricted_Access);
   Add_Func ("/glib-module/open-symbol-close",
             Test_Open_Symbol_And_Close'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Run);
end Main;
