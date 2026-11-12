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
with Glib.Key_File;      use Glib.Key_File;
with Glib.Test;          use Glib.Test;
with Glib.Variant;       use Glib.Variant;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Main;
with Gtk.Paper_Size;
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

   procedure Test_Additional_Values
   with Convention => C;

   procedure Test_Foreach
   with Convention => C;

   procedure Test_Serialization
   with Convention => C;

   procedure Test_Load_And_Constructor_Errors
   with Convention => C;

   --  Both callback APIs must carry the setting's key and value back into
   --  Ada.  Store callback observations so the assertions run after C has
   --  returned rather than raising an Ada exception through C frames.
   Plain_Count       : Gint := 0;
   Plain_First_Seen  : Boolean := False;
   Plain_Second_Seen : Boolean := False;

   type Callback_State;
   type Callback_State_Access is access all Callback_State;
   type Callback_State is record
      Count       : Gint := 0;
      First_Seen  : Boolean := False;
      Second_Seen : Boolean := False;
   end record;

   package Foreach_With_Data is new Foreach_User_Data
     (Callback_State_Access);

   procedure Plain_Callback (Key : UTF8_String; Value : UTF8_String);
   procedure Data_Callback
     (Key : UTF8_String; Value : UTF8_String; Data : Callback_State_Access);

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

   ----------------------------
   -- Test_Additional_Values --
   ----------------------------

   procedure Test_Additional_Values is
      Settings : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
      Allocated : Gtk_Print_Settings;
   begin
      --  The procedure constructor initializes an out parameter directly.
      Gtk_New (Allocated);
      Allocated.Set ("allocated", "through Gtk_New");
      Assert_Cmpstr_Eq (Allocated.Get ("allocated"), "through Gtk_New");

      --  The default-returning accessors use their fallback when a key is
      --  absent, and parse the stored value when it is present.
      Assert_Cmpint_Eq (Settings.Get_Int_With_Default ("missing-int", 17), 17);
      Assert_Cmpfloat_Eq
        (Settings.Get_Double_With_Default ("missing-double", 2.5), 2.5);
      Assert_False (Settings.Get_Bool ("missing-bool"));
      Settings.Set_Bool ("custom-bool", False);
      Assert_False (Settings.Get_Bool ("custom-bool"));

      --  These are simple print options and need no printer backend.  Pair
      --  every setter with the corresponding getter to check the public API.
      Settings.Set_Default_Source ("tray-2");
      Assert_Cmpstr_Eq (Settings.Get_Default_Source, "tray-2");
      Settings.Set_Dither ("gray");
      Assert_Cmpstr_Eq (Settings.Get_Dither, "gray");
      Settings.Set_Duplex (Print_Duplex_Vertical);
      Assert_True (Settings.Get_Duplex = Print_Duplex_Vertical);
      Settings.Set_Finishings ("staple");
      Assert_Cmpstr_Eq (Settings.Get_Finishings, "staple");
      Settings.Set_Media_Type ("stationery");
      Assert_Cmpstr_Eq (Settings.Get_Media_Type, "stationery");
      Settings.Set_Number_Up (4);
      Assert_Cmpint_Eq (Settings.Get_Number_Up, 4);
      Settings.Set_Number_Up_Layout (Top_To_Bottom_Right_To_Left);
      Assert_True
        (Settings.Get_Number_Up_Layout = Top_To_Bottom_Right_To_Left);
      Settings.Set_Output_Bin ("upper");
      Assert_Cmpstr_Eq (Settings.Get_Output_Bin, "upper");
      Settings.Set_Page_Set (Page_Set_Even);
      Assert_True (Settings.Get_Page_Set = Page_Set_Even);
      Settings.Set_Print_Pages (Print_Pages_Current);
      Assert_True (Settings.Get_Print_Pages = Print_Pages_Current);
      Settings.Set_Printer_Lpi (12.5);
      Assert_Cmpfloat_Eq (Settings.Get_Printer_Lpi, 12.5);
      Settings.Set_Quality (Print_Quality_Draft);
      Assert_True (Settings.Get_Quality = Print_Quality_Draft);
      Settings.Set_Reverse (True);
      Assert_True (Settings.Get_Reverse);
      Settings.Set_Scale (87.5);
      Assert_Cmpfloat_Eq (Settings.Get_Scale, 87.5);
      Settings.Set_Use_Color (False);
      Assert_False (Settings.Get_Use_Color);

      --  Resolution has shared and independent-axis forms.  Check the shared
      --  setter updates both axes, then check the two-axis setter directly.
      Settings.Set_Resolution (600);
      Assert_Cmpint_Eq (Settings.Get_Resolution, 600);
      Assert_Cmpint_Eq (Settings.Get_Resolution_X, 600);
      Assert_Cmpint_Eq (Settings.Get_Resolution_Y, 600);
      Settings.Set_Resolution_Xy (300, 450);
      Assert_Cmpint_Eq (Settings.Get_Resolution_X, 300);
      Assert_Cmpint_Eq (Settings.Get_Resolution_Y, 450);

      --  Paper dimensions are also just stored settings.  A custom size keeps
      --  the test independent of locale and the host's configured paper.
      declare
         Paper : constant Gtk.Paper_Size.Gtk_Paper_Size :=
           Gtk.Paper_Size.Gtk_Paper_Size_New_Custom
             ("gtkada-test-paper", "GtkAda test paper", 500.0, 700.0, Points);
      begin
         Settings.Set_Paper_Size (Paper);
         declare
            Read_Back : constant Gtk.Paper_Size.Gtk_Paper_Size :=
              Settings.Get_Paper_Size;
         begin
            Assert_Cmpstr_Eq
              (Gtk.Paper_Size.Get_Name (Read_Back), "gtkada-test-paper");
            Gtk.Paper_Size.Free (Read_Back);
         end;
         Gtk.Paper_Size.Free (Paper);
      end;

      Settings.Set_Paper_Width (8.0, Inch);
      Settings.Set_Paper_Height (10.0, Inch);
      Assert_Cmpfloat_Eq (Settings.Get_Paper_Width (Inch), 8.0);
      Assert_Cmpfloat_Eq (Settings.Get_Paper_Height (Inch), 10.0);

      Settings.Set ("present", "yes");
      Assert_True (Settings.Has_Key ("present"));
      Assert_False (Settings.Has_Key ("absent"));
   end Test_Additional_Values;

   ------------------
   -- Plain_Callback --
   ------------------

   procedure Plain_Callback (Key : UTF8_String; Value : UTF8_String) is
   begin
      Plain_Count := Plain_Count + 1;
      if Key = "first" and then Value = "one" then
         Plain_First_Seen := True;
      elsif Key = "second" and then Value = "two" then
         Plain_Second_Seen := True;
      end if;
   end Plain_Callback;

   -----------------
   -- Data_Callback --
   -----------------

   procedure Data_Callback
     (Key : UTF8_String; Value : UTF8_String; Data : Callback_State_Access)
   is
   begin
      Data.Count := Data.Count + 1;
      if Key = "first" and then Value = "one" then
         Data.First_Seen := True;
      elsif Key = "second" and then Value = "two" then
         Data.Second_Seen := True;
      end if;
   end Data_Callback;

   ------------------
   -- Test_Foreach --
   ------------------

   procedure Test_Foreach is
      Settings : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
      State    : aliased Callback_State;
   begin
      Settings.Set ("first", "one");
      Settings.Set ("second", "two");

      Plain_Count := 0;
      Plain_First_Seen := False;
      Plain_Second_Seen := False;
      Settings.Foreach (Plain_Callback'Unrestricted_Access);
      Assert_Cmpint_Eq (Plain_Count, 2);
      Assert_True (Plain_First_Seen);
      Assert_True (Plain_Second_Seen);

      Foreach_With_Data.Foreach
        (Settings, Data_Callback'Unrestricted_Access,
         State'Unrestricted_Access);
      Assert_Cmpint_Eq (State.Count, 2);
      Assert_True (State.First_Seen);
      Assert_True (State.Second_Seen);
   end Test_Foreach;

   ------------------------
   -- Test_Serialization --
   ------------------------

   procedure Test_Serialization is
      Settings : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
      Key_File : G_Key_File;
      Error    : GError;
   begin
      Settings.Set_Printer ("saved-with-key-file");
      Settings.Set_N_Copies (6);
      Gtk_New (Key_File);

      --  Empty group names select GTK's default group; an explicit group
      --  exercises the string allocation and named-group path as well.
      Settings.To_Key_File (Key_File);
      Settings.To_Key_File (Key_File, "Custom Print Settings");

      declare
         Default_Copy : constant Gtk_Print_Settings :=
           Gtk_Print_Settings_New_From_Key_File (Key_File, Error => Error);
      begin
         Assert_No_Error (Error);
         Assert_Cmpstr_Eq (Default_Copy.Get_Printer, "saved-with-key-file");
         Assert_Cmpint_Eq (Default_Copy.Get_N_Copies, 6);
      end;

      declare
         Named_Copy : Gtk_Print_Settings;
      begin
         Gtk_New_From_Key_File
           (Named_Copy, Key_File, "Custom Print Settings", Error);
         Assert_No_Error (Error);
         Assert_Cmpstr_Eq (Named_Copy.Get_Printer, "saved-with-key-file");
      end;

      declare
         Loaded : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
      begin
         Assert_True (Loaded.Load_Key_File (Key_File, Error => Error));
         Assert_No_Error (Error);
         Assert_Cmpstr_Eq (Loaded.Get_Printer, "saved-with-key-file");

         Assert_True
           (Loaded.Load_Key_File
              (Key_File, "Custom Print Settings", Error));
         Assert_No_Error (Error);
         Assert_Cmpint_Eq (Loaded.Get_N_Copies, 6);
      end;

      --  The variant conversion preserves values and exercises both public
      --  constructor forms, each of which initializes an Ada wrapper.
      declare
         Variant : constant Gvariant := Settings.To_Gvariant;
         By_Func : constant Gtk_Print_Settings :=
           Gtk_Print_Settings_New_From_Gvariant (Variant);
         By_Proc : Gtk_Print_Settings;
      begin
         Gtk_New_From_Gvariant (By_Proc, Variant);
         Assert_Cmpstr_Eq (By_Func.Get_Printer, "saved-with-key-file");
         Assert_Cmpint_Eq (By_Func.Get_N_Copies, 6);
         Assert_Cmpstr_Eq (By_Proc.Get_Printer, "saved-with-key-file");
      end;

      --  Initializers are documented as no-ops for an already created wrapper.
      Gtk.Print_Settings.Initialize (Settings);
      Settings.Initialize_From_Key_File
        (Key_File, "missing group", Error);
      Assert_No_Error (Error);
      Assert_Cmpstr_Eq (Settings.Get_Printer, "saved-with-key-file");
   end Test_Serialization;

   -------------------------------------
   -- Test_Load_And_Constructor_Errors --
   -------------------------------------

   procedure Test_Load_And_Constructor_Errors is
      Settings : constant Gtk_Print_Settings := Gtk_Print_Settings_New;
      Key_File : G_Key_File;
      Error    : GError;
   begin
      --  Save and load through the file API, then check its recoverable error
      --  paths with a deterministic missing directory.
      Settings.Set_Printer ("file-printer");
      Assert_True (Settings.To_File ("print-settings-load.ini", Error));
      Assert_No_Error (Error);
      Assert_True (Settings.Load_File ("print-settings-load.ini", Error));
      Assert_No_Error (Error);
      Assert_Cmpstr_Eq (Settings.Get_Printer, "file-printer");
      Assert_False
        (Settings.Load_File ("missing-directory/settings.ini", Error));
      Assert_True (Error /= null);

      --  Both constructor styles return a null wrapper and a GError when the
      --  file is absent.  The initializer on an existing object is a no-op.
      declare
         Failed : Gtk_Print_Settings;
      begin
         Gtk_New_From_File
           (Failed, "missing-directory/settings.ini", Error);
         Assert_True (Failed = null);
         Assert_True (Error /= null);
      end;
      declare
         Failed : constant Gtk_Print_Settings :=
           Gtk_Print_Settings_New_From_File
             ("missing-directory/settings.ini", Error);
      begin
         Assert_True (Failed = null);
         Assert_True (Error /= null);
      end;
      Settings.Initialize_From_File
        ("missing-directory/settings.ini", Error);
      Assert_No_Error (Error);
      Assert_Cmpstr_Eq (Settings.Get_Printer, "file-printer");

      --  Missing key-file groups take the binding's error cleanup branches.
      Gtk_New (Key_File);
      Assert_False
        (Settings.Load_Key_File (Key_File, "missing group", Error));
      Assert_True (Error /= null);
      declare
         Failed : Gtk_Print_Settings;
      begin
         Gtk_New_From_Key_File
           (Failed, Key_File, "missing group", Error);
         Assert_True (Failed = null);
         Assert_True (Error /= null);
      end;
      declare
         Failed : constant Gtk_Print_Settings :=
           Gtk_Print_Settings_New_From_Key_File
             (Key_File, "missing group", Error);
      begin
         Assert_True (Failed = null);
         Assert_True (Error /= null);
      end;
   end Test_Load_And_Constructor_Errors;

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
   Glib.Test.Add_Func
     ("/print-settings/additional-values",
      Test_Additional_Values'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-settings/foreach", Test_Foreach'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-settings/serialization",
      Test_Serialization'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/print-settings/load-and-constructor-errors",
      Test_Load_And_Constructor_Errors'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Print_Settings;
