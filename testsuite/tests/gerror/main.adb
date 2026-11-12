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
with Glib.Error_Enums; use Glib.Error_Enums;
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

   procedure Test_Enum_Matches
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

   ------------------------
   -- Test_Enum_Matches --
   ------------------------

   procedure Test_Enum_Matches is
      --  These errors use the enum ordinal represented by each contiguous
      --  GLib error domain, matching the code passed to the helper.
      --  The bookmark and filesystem groups have distinct error domains.
      Bookmark_Error : constant GError := Error_New
        (GBookmark_File_Error_Domain,
         Gint (GBookmark_File_Error'Pos (G_Bookmark_File_Error_Read)),
         "bookmark error");
      File_Error : constant GError := Error_New
        (GFile_Error_Domain,
         Gint (GFile_Error'Pos (G_File_Error_Noent)),
         "file error");
      --  Key-file and markup codes come from their parser error domains.
      Key_File_Error : constant GError := Error_New
        (GKey_File_Error_Domain,
         Gint (GKey_File_Error'Pos (G_Key_File_Error_Not_Found)),
         "key file error");
      Markup_Error : constant GError := Error_New
        (GMarkup_Error_Domain,
         Gint (GMarkup_Error'Pos (G_Markup_Error_Empty)),
         "markup error");
      --  Number parsing and option parsing have their own code enumerations.
      Number_Error : constant GError := Error_New
        (GNumber_Parser_Error_Domain,
         Gint (GNumber_Parser_Error'Pos (G_Number_Parser_Error_Invalid)),
         "number parser error");
      Option_Error : constant GError := Error_New
        (GOption_Error_Domain,
         Gint (GOption_Error'Pos (G_Option_Error_Unknown_Option)),
         "option error");
      --  Shell and variant parsing complete the contiguous error domains.
      Shell_Error : constant GError := Error_New
        (GShell_Error_Domain,
         Gint (GShell_Error'Pos (G_Shell_Error_Bad_Quoting)),
         "shell error");
      Variant_Error : constant GError := Error_New
        (GVariant_Parse_Error_Domain,
         Gint (GVariant_Parse_Error'Pos (G_Variant_Parse_Error_Failed)),
         "variant error");
   begin
      --  Each positive assertion checks that its helper recognizes both the
      --  error domain and code; the negative checks reject another code.
      Assert_True (GBookmark_File_Error_Matches
        (Bookmark_Error, G_Bookmark_File_Error_Read));
      Assert_False (GBookmark_File_Error_Matches
        (Bookmark_Error, G_Bookmark_File_Error_Write));
      Assert_True (GFile_Error_Matches (File_Error, G_File_Error_Noent));
      Assert_False (GFile_Error_Matches (File_Error, G_File_Error_Perm));
      Assert_True (GKey_File_Error_Matches
        (Key_File_Error, G_Key_File_Error_Not_Found));
      Assert_False (GKey_File_Error_Matches
        (Key_File_Error, G_Key_File_Error_Parse));
      Assert_True (GMarkup_Error_Matches (Markup_Error, G_Markup_Error_Empty));
      Assert_False (GMarkup_Error_Matches
        (Markup_Error, G_Markup_Error_Parse));
      Assert_True (GNumber_Parser_Error_Matches
        (Number_Error, G_Number_Parser_Error_Invalid));
      Assert_False (GNumber_Parser_Error_Matches
        (Number_Error, G_Number_Parser_Error_Out_Of_Bounds));
      Assert_True (GOption_Error_Matches
        (Option_Error, G_Option_Error_Unknown_Option));
      Assert_False (GOption_Error_Matches (Option_Error, G_Option_Error_Failed));
      Assert_True (GShell_Error_Matches (Shell_Error, G_Shell_Error_Bad_Quoting));
      Assert_False (GShell_Error_Matches
        (Shell_Error, G_Shell_Error_Empty_String));
      Assert_True (GVariant_Parse_Error_Matches
        (Variant_Error, G_Variant_Parse_Error_Failed));
      Assert_False (GVariant_Parse_Error_Matches
        (Variant_Error, G_Variant_Parse_Error_Type_Error));

      Error_Free (Bookmark_Error);
      Error_Free (File_Error);
      Error_Free (Key_File_Error);
      Error_Free (Markup_Error);
      Error_Free (Number_Error);
      Error_Free (Option_Error);
      Error_Free (Shell_Error);
      Error_Free (Variant_Error);
   end Test_Enum_Matches;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func ("/error/literal", Test_Literal'Unrestricted_Access);
   Glib.Test.Add_Func ("/error/copy", Test_Copy'Unrestricted_Access);
   Glib.Test.Add_Func ("/error/matches", Test_Matches'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/error/enum-matches", Test_Enum_Matches'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
