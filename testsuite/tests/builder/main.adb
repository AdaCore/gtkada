--  Exercises Gtk.Builder's GError-throwing methods (Add_From_String,
--  Add_From_File), in the spirit of glib's testsuite/c_tests/builderparser.c
--  (which drives gtk_builder_add_from_string over a directory of *.ui /
--  *.expected fixture pairs we don't have here) and builder.c (too large --
--  3000+ lines -- to port wholesale).
--
--  Only the pure parse-error / file-not-found paths are covered: actually
--  instantiating an object (even a plain GtkLabel) from valid UI requires
--  GtkBuilder's scope/type-resolution machinery, which has no Ada-side
--  implementation (no GtkBuilderScope binding) and crashes rather than
--  raising a clean error. See discussion in ~/notes/gtk4.
--
--  No widget is created on screen, so this test needs no display.

with Ada.Command_Line;

with Glib;         use Glib;
with Glib.Error;   use Glib.Error;
with Glib.Error_Enums;
with Glib.Test;    use Glib.Test;
with Gtk.Builder;  use Gtk.Builder;

procedure Main is

   Malformed_UI : constant UTF8_String := "this is not a ui definition";
   --  Not XML at all, and references no GTK class: fails at the pure
   --  markup-parse stage, before any object/type resolution is attempted
   --  (which would otherwise hit the missing-GtkBuilderScope crash noted
   --  above).

   procedure Test_Add_From_String_Malformed
   with Convention => C;

   procedure Test_Add_From_File_Missing
   with Convention => C;

   ------------------------------------
   -- Test_Add_From_String_Malformed --
   ------------------------------------

   procedure Test_Add_From_String_Malformed is
      Builder : Gtk_Builder;
      Error   : Glib.Error.GError;
      Success : Boolean;
   begin
      Gtk_New (Builder);

      Success := Builder.Add_From_String (Malformed_UI, Error);

      Assert_False (Success);
      Assert_True (Error /= null);
      Assert_Error
        (Error,
         Glib.Error_Enums.GMarkup_Error_Domain,
         Glib.Gint
           (Glib.Error_Enums.GMarkup_Error'Pos
              (Glib.Error_Enums.G_Markup_Error_Parse)));

      Error_Free (Error);
      Unref (Builder);
   end Test_Add_From_String_Malformed;

   -------------------------------
   -- Test_Add_From_File_Missing --
   -------------------------------

   procedure Test_Add_From_File_Missing is
      Builder : Gtk_Builder;
      Error   : Glib.Error.GError;
      Success : Boolean;
   begin
      Gtk_New (Builder);

      Success := Builder.Add_From_File
        ("/no/such/file-gtkada-builder-test.ui", Error);

      Assert_False (Success);
      Assert_True (Error /= null);

      Error_Free (Error);
      Unref (Builder);
   end Test_Add_From_File_Missing;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func
     ("/builder/add-from-string/malformed",
      Test_Add_From_String_Malformed'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/builder/add-from-file/missing",
      Test_Add_From_File_Missing'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
