--  Port the GTK main initialization and version checks from
--  testsuite/c_tests/main.c, and cover Gtk.Main's default-language binding.

with Ada.Command_Line;

with Glib;      use Glib;
with Glib.Test; use Glib.Test;
with Gtk.Main;
with Pango.Language;

procedure Main is

   procedure Test_Init
   with Convention => C;

   procedure Test_Version
   with Convention => C;

   --  GTK's initialization check succeeds under the testsuite's Xvfb server.
   --  Calling Init afterwards also exercises the binding's non-checking entry
   --  point; GTK initialization is safe to request again.
   procedure Test_Init is
   begin
      Gtk.Main.Disable_Setlocale;
      Assert_True (Gtk.Main.Init_Check);
      Gtk.Main.Init;
      Assert_True (Gtk.Main.Init_Check);
   end Test_Init;

   --  The C test compares these values with GTK's compile-time macros. The
   --  Ada binding exposes the runtime getters, so check the fixed GTK major
   --  version and the documented relationship between version and binary age.
   procedure Test_Version is
      Major : constant Guint := Gtk.Main.Get_Major_Version;
      Minor : constant Guint := Gtk.Main.Get_Minor_Version;
      Micro : constant Guint := Gtk.Main.Get_Micro_Version;
      Default_Language : constant Pango.Language.Pango_Language :=
        Gtk.Main.Get_Default_Language;
      Pango_Default : constant Pango.Language.Pango_Language :=
        Pango.Language.Get_Default;
   begin
      Assert_Cmpuint_Eq (Major, 4);
      Assert_Cmpuint_Ge (Minor, 22);
      Assert_True (Gtk.Main.Get_Binary_Age = Minor * 100 + Micro);
      Assert_True (Gtk.Main.Get_Interface_Age <= Gtk.Main.Get_Binary_Age);

      --  Matching the running version returns an empty Ada string because the
      --  underlying C function returns NULL for a compatible version.
      Assert_Cmpstr_Eq
        (Gtk.Main.Check_Version (Major, Minor, Micro), "");

      --  Newer and older incompatible major versions return a diagnostic.
      Assert_True (Gtk.Main.Check_Version (5, 0, 0)'Length > 0);
      Assert_True (Gtk.Main.Check_Version (1, 0, 0)'Length > 0);
      Assert_True (Gtk.Main.Check_Version (3, 1_000, 10)'Length > 0);

      --  GTK documents this value as equivalent to Pango's default language.
      Assert_True (Pango.Language.To_String (Default_Language)'Length > 0);
      Assert_Cmpstr_Eq
        (Pango.Language.To_String (Default_Language),
         Pango.Language.To_String (Pango_Default));
   end Test_Version;

begin
   --  Unlike gtk_test_init in the C test, Glib.Test.Init does not initialize
   --  GTK, allowing Test_Init to exercise Init_Check on an uninitialized GTK.
   Glib.Test.Init;
   Glib.Test.Add_Func ("/main/init", Test_Init'Unrestricted_Access);
   Glib.Test.Add_Func ("/main/version", Test_Version'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
