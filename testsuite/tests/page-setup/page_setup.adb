--  Headless checks for Gtk.Page_Setup's geometry and serialization APIs.
--  GtkPageSetup has no corresponding standalone C test in testsuite/c_tests;
--  these cases check the documented GtkAda bindings directly without a
--  printer or a page setup dialog.

with Ada.Command_Line;
with Ada.Directories;
with Glib;
with Glib.Error;       use Glib.Error;
with Glib.Key_File;
with Glib.Test;        use Glib.Test;
with Glib.Variant;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Page_Setup;   use Gtk.Page_Setup;
with Gtk.Paper_Size;   use Gtk.Paper_Size;
with Glib.Object;      use Glib.Object;

procedure Page_Setup is
   use type Glib.Gdouble;

   procedure Test_Constructors_And_Geometry with Convention => C;
   procedure Test_File_Round_Trip with Convention => C;
   procedure Test_Key_File_Round_Trip with Convention => C;
   procedure Test_Gvariant_Round_Trip with Convention => C;

   --  GTK converts many dimensions through points, so compare with a small
   --  tolerance instead of requiring bit-for-bit equality for Gdouble.
   procedure Assert_Approx (Actual, Expected : Glib.Gdouble);

   procedure Assert_Approx (Actual, Expected : Glib.Gdouble) is
   begin
      Assert_True (abs (Actual - Expected) < 0.01);
   end Assert_Approx;

   --  Both Gtk_New forms and every geometry accessor are checked against a
   --  custom paper size, whose dimensions do not depend on a locale.
   procedure Test_Constructors_And_Geometry is
      By_Procedure : Gtk_Page_Setup;
      By_Function  : Gtk_Page_Setup := Gtk_Page_Setup_New;
      Paper        : Gtk_Paper_Size := Gtk_Paper_Size_New_Custom
        ("ada-page-setup", "Ada Page Setup", 500.0, 700.0, Points);
      Duplicate    : Gtk_Page_Setup;
   begin
      Gtk_New (By_Procedure);
      --  A second Initialize call must leave the existing GObject intact.
      Gtk.Page_Setup.Initialize (By_Procedure);
      Assert_True (By_Procedure.Get_Paper_Width (Points) > 0.0);
      Assert_True (By_Function.Get_Paper_Height (Points) > 0.0);

      --  Setting a paper size alone preserves margins; all four setters and
      --  getters are checked in points, then the dimensions in both axes.
      By_Procedure.Set_Left_Margin (10.0, Points);
      By_Procedure.Set_Top_Margin (20.0, Points);
      By_Procedure.Set_Right_Margin (30.0, Points);
      By_Procedure.Set_Bottom_Margin (40.0, Points);
      By_Procedure.Set_Paper_Size (Paper);
      Assert_Approx (By_Procedure.Get_Left_Margin (Points), 10.0);
      Assert_Approx (By_Procedure.Get_Top_Margin (Points), 20.0);
      Assert_Approx (By_Procedure.Get_Right_Margin (Points), 30.0);
      Assert_Approx (By_Procedure.Get_Bottom_Margin (Points), 40.0);
      Assert_Approx (By_Procedure.Get_Paper_Width (Points), 500.0);
      Assert_Approx (By_Procedure.Get_Paper_Height (Points), 700.0);
      Assert_Approx (By_Procedure.Get_Page_Width (Points), 460.0);
      Assert_Approx (By_Procedure.Get_Page_Height (Points), 640.0);

      --  Orientation swaps the paper axes; page width and height then account
      --  for the corresponding vertical and horizontal margin pairs.
      By_Procedure.Set_Orientation (Page_Orientation_Landscape);
      Assert_True
        (By_Procedure.Get_Orientation = Page_Orientation_Landscape);
      Assert_Approx (By_Procedure.Get_Paper_Width (Points), 700.0);
      Assert_Approx (By_Procedure.Get_Paper_Height (Points), 500.0);
      Assert_Approx (By_Procedure.Get_Page_Width (Points), 640.0);
      Assert_Approx (By_Procedure.Get_Page_Height (Points), 460.0);

      --  The paper-size result is borrowed from the setup, so inspect it but
      --  do not free it separately. Copy, by contrast, returns an owner.
      Assert_Cmpstr_Eq
        (Get_Name (By_Procedure.Get_Paper_Size), "ada-page-setup");
      Duplicate := By_Procedure.Copy;
      Assert_True
        (Duplicate.Get_Orientation = Page_Orientation_Landscape);
      Assert_Approx (Duplicate.Get_Left_Margin (Points), 10.0);

      --  This operation also resets margins to those associated with Paper.
      By_Procedure.Set_Paper_Size_And_Default_Margins (Paper);
      Assert_True (By_Procedure.Get_Left_Margin (Points) >= 0.0);
      Assert_True (By_Procedure.Get_Top_Margin (Points) >= 0.0);
      Assert_True (By_Procedure.Get_Right_Margin (Points) >= 0.0);
      Assert_True (By_Procedure.Get_Bottom_Margin (Points) >= 0.0);

      Unref (Duplicate);
      Unref (By_Function);
      Unref (By_Procedure);
      Free (Paper);
   end Test_Constructors_And_Geometry;

   --  The file tests cover serialization, all constructor overloads, both
   --  successful load paths, and recoverable errors for a missing file.
   procedure Test_File_Round_Trip is
      File_Name : constant String := "gtk-page-setup-round-trip.ini";
      Missing   : constant String := "gtk-page-setup-missing.ini";
      Original  : constant Gtk_Page_Setup := Gtk_Page_Setup_New;
      From_Proc : Gtk_Page_Setup;
      From_Func : Gtk_Page_Setup;
      Failed    : Gtk_Page_Setup;
      Loaded    : constant Gtk_Page_Setup := Gtk_Page_Setup_New;
      Error     : GError;
   begin
      Original.Set_Orientation (Page_Orientation_Landscape);
      Original.Set_Left_Margin (13.0, Mm);
      Original.Set_Top_Margin (17.0, Mm);
      Assert_True (Original.To_File (File_Name, Error));
      Assert_No_Error (Error);

      --  Procedure and function constructors both restore the saved values.
      Gtk_New_From_File (From_Proc, File_Name, Error);
      Assert_No_Error (Error);
      From_Func := Gtk_Page_Setup_New_From_File (File_Name, Error);
      Assert_No_Error (Error);
      Assert_True (From_Proc.Get_Orientation = Page_Orientation_Landscape);
      Assert_True (From_Func.Get_Orientation = Page_Orientation_Landscape);
      Assert_Approx (From_Func.Get_Left_Margin (Mm), 13.0);
      Assert_Approx (From_Func.Get_Top_Margin (Mm), 17.0);

      --  Load_File replaces a live setup. Initialize_From_File intentionally
      --  does nothing on an already-created instance and clears Error.
      Assert_True (Loaded.Load_File (File_Name, Error));
      Assert_No_Error (Error);
      Loaded.Set_Left_Margin (99.0, Points);
      Loaded.Initialize_From_File (File_Name, Error);
      Assert_No_Error (Error);
      Assert_Approx (Loaded.Get_Left_Margin (Points), 99.0);

      --  Failed constructors return null; load and save report GError instead
      --  of raising when the path is absent or its parent does not exist.
      Failed := Gtk_Page_Setup_New_From_File (Missing, Error);
      Assert_True (Failed = null);
      Assert_True (Error /= null);
      Error_Free (Error);
      Gtk_New_From_File (Failed, Missing, Error);
      Assert_True (Failed = null);
      Assert_True (Error /= null);
      Error_Free (Error);
      Assert_False (Loaded.Load_File (Missing, Error));
      Assert_True (Error /= null);
      Error_Free (Error);
      Assert_False (Original.To_File ("no-such-directory/page.ini", Error));
      Assert_True (Error /= null);
      Error_Free (Error);

      Ada.Directories.Delete_File (File_Name);
      Unref (Loaded);
      Unref (From_Func);
      Unref (From_Proc);
      Unref (Original);
   end Test_File_Round_Trip;

   --  Key-file checks cover default and explicit groups, both constructor
   --  overloads, in-place load, and the recoverable missing-group paths.
   procedure Test_Key_File_Round_Trip is
      Key_File  : Glib.Key_File.G_Key_File;
      Original  : constant Gtk_Page_Setup := Gtk_Page_Setup_New;
      From_Proc : Gtk_Page_Setup;
      From_Func : Gtk_Page_Setup;
      Failed    : Gtk_Page_Setup;
      Loaded    : constant Gtk_Page_Setup := Gtk_Page_Setup_New;
      Error     : GError;
   begin
      Glib.Key_File.Gtk_New (Key_File);
      Original.Set_Orientation (Page_Orientation_Landscape);
      Original.Set_Right_Margin (22.0, Points);
      Original.To_Key_File (Key_File);
      Original.To_Key_File (Key_File, "Alternate Setup");

      --  An empty group name selects GTK's default "Page Setup" group.
      Gtk_New_From_Key_File (From_Proc, Key_File, Error => Error);
      Assert_No_Error (Error);
      From_Func := Gtk_Page_Setup_New_From_Key_File
        (Key_File, "Alternate Setup", Error);
      Assert_No_Error (Error);
      Assert_True (From_Proc.Get_Orientation = Page_Orientation_Landscape);
      Assert_True (From_Func.Get_Orientation = Page_Orientation_Landscape);
      Assert_Approx (From_Func.Get_Right_Margin (Points), 22.0);

      Assert_True (Loaded.Load_Key_File (Key_File, Error => Error));
      Assert_No_Error (Error);
      Loaded.Set_Right_Margin (88.0, Points);
      Loaded.Initialize_From_Key_File (Key_File, Error => Error);
      Assert_No_Error (Error);
      Assert_Approx (Loaded.Get_Right_Margin (Points), 88.0);

      --  A missing group takes each public error path. Free each GError once
      --  after checking that the Ada wrapper returned the documented failure.
      Failed := Gtk_Page_Setup_New_From_Key_File
        (Key_File, "missing group", Error);
      Assert_True (Failed = null);
      Assert_True (Error /= null);
      Error_Free (Error);
      Gtk_New_From_Key_File
        (Failed, Key_File, "missing group", Error);
      Assert_True (Failed = null);
      Assert_True (Error /= null);
      Error_Free (Error);
      Assert_False
        (Loaded.Load_Key_File (Key_File, "missing group", Error));
      Assert_True (Error /= null);
      Error_Free (Error);

      --  GKeyFile's GtkAda Free binding has an existing abort in cleanup, so
      --  keep this small process-local object until the test process exits.
      Unref (Loaded);
      Unref (From_Func);
      Unref (From_Proc);
      Unref (Original);
   end Test_Key_File_Round_Trip;

   --  GTK's documented a{sv} form is used as the shared input to both Ada
   --  constructors; the preinitialized destination also checks the no-op
   --  branch in Initialize_From_Gvariant.
   procedure Test_Gvariant_Round_Trip is
      Original  : constant Gtk_Page_Setup := Gtk_Page_Setup_New;
      From_Proc : Gtk_Page_Setup;
      From_Func : Gtk_Page_Setup;
   begin
      Original.Set_Orientation (Page_Orientation_Landscape);
      Original.Set_Bottom_Margin (25.0, Points);
      declare
         Variant : constant Glib.Variant.Gvariant := Original.To_Gvariant;
         Existing : constant Gtk_Page_Setup := Gtk_Page_Setup_New;
      begin
         Gtk_New_From_Gvariant (From_Proc, Variant);
         From_Func := Gtk_Page_Setup_New_From_Gvariant (Variant);
         Existing.Set_Bottom_Margin (77.0, Points);
         Existing.Initialize_From_Gvariant (Variant);
         Assert_True (From_Proc.Get_Orientation = Page_Orientation_Landscape);
         Assert_True (From_Func.Get_Orientation = Page_Orientation_Landscape);
         Assert_Approx (From_Func.Get_Bottom_Margin (Points), 25.0);
         Assert_Approx (Existing.Get_Bottom_Margin (Points), 77.0);
         Unref (Existing);
         Glib.Variant.Unref (Variant);
      end;
      Unref (From_Func);
      Unref (From_Proc);
      Unref (Original);
   end Test_Gvariant_Round_Trip;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/page-setup/constructors-and-geometry",
      Test_Constructors_And_Geometry'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/page-setup/file-round-trip",
      Test_File_Round_Trip'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/page-setup/key-file-round-trip",
      Test_Key_File_Round_Trip'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/page-setup/gvariant-round-trip",
      Test_Gvariant_Round_Trip'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Page_Setup;
