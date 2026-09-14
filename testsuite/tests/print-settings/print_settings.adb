--  Headless coverage for Gtk.Print_Settings.
--
--  Gtk_Print_Settings is a pure key/value model, so everything here runs
--  without a printer or a dialog.
--
--  The page-range accessors are the interesting part: the C API returns a
--  `GtkPageRange *` the caller must free and takes a pointer to the first
--  element of an array, so a binding that passed the record by value would
--  corrupt the ranges or read wild memory. The checks below pin the ABI down
--  from both ends -- the Ada array that comes back out, and the textual
--  "page-ranges" key that the C side actually stored.

with Ada.Command_Line;
with Glib;               use Glib;
with Glib.Error;         use Glib.Error;
with Glib.Test;          use Glib.Test;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Main;
with Gtk.Print_Settings; use Gtk.Print_Settings;

procedure Print_Settings is

   procedure Test_Values
   with Convention => C;

   procedure Test_Page_Ranges
   with Convention => C;

   procedure Test_Page_Ranges_Encoding
   with Convention => C;

   procedure Test_Copy
   with Convention => C;

   procedure Test_File_Round_Trip
   with Convention => C;

   -----------------
   -- Test_Values --
   -----------------

   procedure Test_Values is
      Settings : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
   begin
      Settings.Set ("custom-key", "custom-value");
      Assert_True (Settings.Has_Key ("custom-key"));
      Assert_Cmpstr_Eq (Settings.Get ("custom-key"), "custom-value");

      Settings.Unset ("custom-key");
      Assert_False (Settings.Has_Key ("custom-key"));

      Settings.Set_Int ("an-int", 42);
      Assert_Cmpint_Eq (Settings.Get_Int ("an-int"), 42);
      Assert_Cmpint_Eq (Settings.Get_Int_With_Default ("no-such-int", 7), 7);

      Settings.Set_Bool ("a-bool", True);
      Assert_True (Settings.Get_Bool ("a-bool"));

      Settings.Set_Double ("a-double", 1.5);
      Assert_Cmpfloat_Eq (Settings.Get_Double ("a-double"), 1.5);

      Settings.Set_Length ("a-length", 72.0, Points);
      Assert_Cmpfloat_Eq (Settings.Get_Length ("a-length", Inch), 1.0);

      Settings.Set_Printer ("Printer name");
      Assert_Cmpstr_Eq (Settings.Get_Printer, "Printer name");

      Settings.Set_N_Copies (3);
      Assert_Cmpint_Eq (Settings.Get_N_Copies, 3);

      Settings.Set_Collate (True);
      Assert_True (Settings.Get_Collate);

      Settings.Set_Orientation (Page_Orientation_Landscape);
      Assert_True (Settings.Get_Orientation = Page_Orientation_Landscape);

      Settings.Set_Page_Set (Page_Set_Odd);
      Assert_True (Settings.Get_Page_Set = Page_Set_Odd);

      Settings.Set_Print_Pages (Print_Pages_Ranges);
      Assert_True (Settings.Get_Print_Pages = Print_Pages_Ranges);
   end Test_Values;

   ----------------------
   -- Test_Page_Ranges --
   ----------------------

   procedure Test_Page_Ranges is
      Settings : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
      Ranges   : constant Page_Range_Array :=
        ((First => 0, Last => 2), (First => 5, Last => 5),
         (First => 9, Last => 11));
   begin
      --  Nothing has been stored yet: the C getter returns NULL, which must
      --  come back as an empty array rather than as a wild pointer.
      Assert_Cmpint_Eq (Gint (Settings.Get_Page_Ranges'Length), 0);

      Settings.Set_Page_Ranges (Ranges);

      declare
         Got : constant Page_Range_Array := Settings.Get_Page_Ranges;
      begin
         Assert_Cmpint_Eq (Gint (Got'Length), Gint (Ranges'Length));
         for J in Got'Range loop
            Assert_Cmpint_Eq
              (Got (J).First, Ranges (Ranges'First + J - Got'First).First);
            Assert_Cmpint_Eq
              (Got (J).Last, Ranges (Ranges'First + J - Got'First).Last);
         end loop;
      end;

      --  An empty array is legal, and must not make the C side read past the
      --  end of a zero-length object.
      Settings.Set_Page_Ranges ((1 .. 0 => (First => 0, Last => 0)));
      Assert_Cmpint_Eq (Gint (Settings.Get_Page_Ranges'Length), 0);
   end Test_Page_Ranges;

   -------------------------------
   -- Test_Page_Ranges_Encoding --
   -------------------------------

   procedure Test_Page_Ranges_Encoding is
      Settings : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
   begin
      --  GTK stores the ranges as a comma-separated string, collapsing a
      --  single-page range to one number. Reading that key back proves the
      --  values C saw are the ones we passed, independently of how the
      --  getter decodes them.
      Settings.Set_Page_Ranges
        (((First => 0, Last => 2), (First => 5, Last => 5)));
      Assert_Cmpstr_Eq (Settings.Get ("page-ranges"), "0-2,5");

      --  And the other way round: a hand-written key must decode to the
      --  matching ranges.
      Settings.Set ("page-ranges", "2-4,8");

      declare
         Got : constant Page_Range_Array := Settings.Get_Page_Ranges;
      begin
         Assert_Cmpint_Eq (Gint (Got'Length), 2);
         Assert_Cmpint_Eq (Got (Got'First).First, 2);
         Assert_Cmpint_Eq (Got (Got'First).Last, 4);
         Assert_Cmpint_Eq (Got (Got'First + 1).First, 8);
         Assert_Cmpint_Eq (Got (Got'First + 1).Last, 8);
      end;
   end Test_Page_Ranges_Encoding;

   ---------------
   -- Test_Copy --
   ---------------

   procedure Test_Copy is
      Settings : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
   begin
      Settings.Set_Printer ("Original");
      Settings.Set_Page_Ranges ((1 => (First => 3, Last => 4)));

      declare
         Copy : constant Gtk_Print_Settings := Settings.Copy;
         Got  : constant Page_Range_Array := Copy.Get_Page_Ranges;
      begin
         Assert_Cmpstr_Eq (Copy.Get_Printer, "Original");
         Assert_Cmpint_Eq (Gint (Got'Length), 1);
         Assert_Cmpint_Eq (Got (Got'First).First, 3);
         Assert_Cmpint_Eq (Got (Got'First).Last, 4);
      end;
   end Test_Copy;

   --------------------------
   -- Test_File_Round_Trip --
   --------------------------

   procedure Test_File_Round_Trip is
      File     : constant String := "print-settings.ini";
      Settings : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
      Error    : GError;
   begin
      Settings.Set_Printer ("Saved printer");
      Settings.Set_N_Copies (5);
      Settings.Set_Page_Ranges
        (((First => 0, Last => 0), (First => 4, Last => 6)));

      Assert_True (Settings.To_File (File, Error));
      Assert_No_Error (Error);

      declare
         Restored : constant Gtk_Print_Settings :=
           Gtk_Print_Settings_New_From_File (File, Error);
      begin
         Assert_No_Error (Error);
         Assert_Cmpstr_Eq (Restored.Get_Printer, "Saved printer");
         Assert_Cmpint_Eq (Restored.Get_N_Copies, 5);

         declare
            Got : constant Page_Range_Array := Restored.Get_Page_Ranges;
         begin
            Assert_Cmpint_Eq (Gint (Got'Length), 2);
            Assert_Cmpint_Eq (Got (Got'First).First, 0);
            Assert_Cmpint_Eq (Got (Got'First).Last, 0);
            Assert_Cmpint_Eq (Got (Got'First + 1).First, 4);
            Assert_Cmpint_Eq (Got (Got'First + 1).Last, 6);
         end;
      end;
   end Test_File_Round_Trip;

begin
   Glib.Test.Init;

   --  Gtk_Print_Settings is a plain GObject, but Gtk.Main.Init keeps the
   --  setup identical to the other tests.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/print-settings/values", Test_Values'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-settings/page-ranges", Test_Page_Ranges'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-settings/page-ranges-encoding",
      Test_Page_Ranges_Encoding'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-settings/copy", Test_Copy'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-settings/file-round-trip",
      Test_File_Round_Trip'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Print_Settings;
