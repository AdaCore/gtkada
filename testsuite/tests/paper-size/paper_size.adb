--  Port the parser, comparison, and unit cases from testsuite/c_tests/papersize.c.
--  The additional cases cover GtkAda's constructor, serialization, list, and
--  conversion helpers without needing a printer or a physical display.

with Ada.Command_Line;
with System;

with Glib.Error;
with Glib.Key_File;
with Glib.Test;    use Glib.Test;
with Glib.Variant;
with Gtk.Enums;
with Gtk.Paper_Size; use Gtk.Paper_Size;

procedure Paper_Size is
   use type Glib.Error.GError;
   use type Glib.Gdouble;
   use type Glib.Guint;
   use type Glib.Variant.Gvariant;
   use type Paper_Size_List.Glist;
   use type System.Address;

   procedure Test_C_Paper_Size_Cases with Convention => C;
   procedure Test_Constructors_And_Margins with Convention => C;
   procedure Test_Key_File with Convention => C;
   procedure Test_Gvariant with Convention => C;
   procedure Test_Paper_Size_List with Convention => C;

   --  These cases preserve the checks in the C paper-size test and exercise
   --  the corresponding Ada getters and equality operation.
   procedure Test_C_Paper_Size_Cases is
      A4 : Gtk_Paper_Size := Gtk_Paper_Size_New (Paper_Name_A4);
      A4_Parsed : Gtk_Paper_Size := Gtk_Paper_Size_New
        ("iso_a4_210x297mm");
      B5 : Gtk_Paper_Size := Gtk_Paper_Size_New (Paper_Name_B5);
      Executive : Gtk_Paper_Size := Gtk_Paper_Size_New (Paper_Name_Executive);
      Custom : Gtk_Paper_Size := Gtk_Paper_Size_New ("custom_w1_20x30in");
      A4_Copy : Gtk_Paper_Size;
   begin
      --  Convert's address overloads are used by generic GtkAda lists, but
      --  exercise them directly here as well and retain a single owner.
      declare
         Address : constant System.Address := Convert (A4);
         Converted : constant Gtk_Paper_Size := Convert (Address);
      begin
         Assert_True (Is_Equal (A4, Converted));
      end;

      --  Verify the standard A4 result and the recognized dimension suffix.
      Assert_Cmpfloat_Eq (Get_Width (A4, Gtk.Enums.Mm), 210.0);
      Assert_Cmpfloat_Eq (Get_Height (A4, Gtk.Enums.Mm), 297.0);
      Assert_Cmpstr_Eq (Get_Name (A4), "iso_a4");
      Assert_Cmpstr_Eq (Get_Display_Name (A4), "A4");
      Assert_Cmpstr_Eq (Get_Ppd_Name (A4), "A4");
      Assert_False (Is_Custom (A4));
      Assert_True (Is_Equal (A4, A4_Parsed));

      --  Check that B5 and Executive retain their standard paper names.
      Assert_Cmpfloat_Eq (Get_Width (B5, Gtk.Enums.Mm), 176.0);
      Assert_Cmpfloat_Eq (Get_Height (B5, Gtk.Enums.Mm), 250.0);
      Assert_Cmpstr_Eq (Get_Name (B5), "iso_b5");
      Assert_Cmpstr_Eq (Get_Display_Name (B5), "B5");
      Assert_Cmpstr_Eq (Get_Ppd_Name (B5), "ISOB5");
      Assert_False (Is_Custom (B5));
      --  C's integer comparisons truncate these fractional millimetre sizes.
      Assert_True
        (Get_Width (Executive, Gtk.Enums.Mm) >= 184.0
         and then Get_Width (Executive, Gtk.Enums.Mm) < 185.0);
      Assert_True
        (Get_Height (Executive, Gtk.Enums.Mm) >= 266.0
         and then Get_Height (Executive, Gtk.Enums.Mm) < 267.0);
      Assert_Cmpstr_Eq (Get_Name (Executive), "na_executive");
      Assert_Cmpstr_Eq (Get_Display_Name (Executive), "Executive");
      Assert_Cmpstr_Eq (Get_Ppd_Name (Executive), "Executive");

      --  The custom PWG-style name reports its parsed name and inch units.
      Assert_Cmpfloat_Eq (Get_Width (Custom, Gtk.Enums.Inch), 20.0);
      Assert_Cmpfloat_Eq (Get_Height (Custom, Gtk.Enums.Inch), 30.0);
      Assert_Cmpstr_Eq (Get_Name (Custom), "custom_w1");
      Assert_Cmpstr_Eq (Get_Display_Name (Custom), "custom_w1");
      Assert_True (Is_Custom (Custom));
      Assert_False (Is_Ipp (Custom));
      Assert_False (Is_Equal (A4, B5));
      Assert_False (Is_Equal (A4, Custom));
      Assert_False (Is_Equal (B5, Custom));

      --  Unit conversions follow the C test's checks, with float tolerances.
      Assert_True
        (Glib.Gdouble (100.0) * Get_Width (A4, Gtk.Enums.Inch) >= 826.0
         and then Glib.Gdouble (100.0) * Get_Width
           (A4, Gtk.Enums.Inch) < 827.0);
      Assert_True
        (Glib.Gdouble (100.0) * Get_Height (A4, Gtk.Enums.Inch) >= 1169.0
         and then Glib.Gdouble (100.0) * Get_Height
           (A4, Gtk.Enums.Inch) < 1170.0);
      Assert_True
        (Get_Width (A4, Gtk.Enums.Points) >= 595.0
         and then Get_Width (A4, Gtk.Enums.Points) < 596.0);
      Assert_True
        (Get_Height (A4, Gtk.Enums.Points) >= 841.0
         and then Get_Height (A4, Gtk.Enums.Points) < 842.0);

      --  Copy returns a separately owned paper size with equal contents.
      A4_Copy := Copy (A4);
      Assert_True (Is_Equal (A4, A4_Copy));
      Free (A4_Copy);
      Free (Custom);
      Free (Executive);
      Free (B5);
      Free (A4_Parsed);
      Free (A4);
   end Test_C_Paper_Size_Cases;

   --  Exercise procedure and function constructor forms, mutation, and all
   --  four default-margin accessors on ordinary paper-size data.
   procedure Test_Constructors_And_Margins is
      By_Procedure : Gtk_Paper_Size;
      By_Function  : Gtk_Paper_Size := Gtk_Paper_Size_New;
      By_Custom_Procedure : Gtk_Paper_Size;
      By_Custom_Function : Gtk_Paper_Size;
      From_Ipp : Gtk_Paper_Size;
      From_Ipp_Function : Gtk_Paper_Size;
      From_Ppd : Gtk_Paper_Size;
      From_Ppd_Procedure : Gtk_Paper_Size;
      Default_Name : constant String := Get_Default;
   begin
      Gtk_New (By_Procedure, Paper_Name_A4);
      Assert_Cmpstr_Eq (Get_Name (By_Procedure), Paper_Name_A4);
      --  An empty name asks GTK for the locale's default; its exact name is
      --  environment dependent, so compare it with GTK's reported default.
      Assert_True (Default_Name'Length > 0);
      Assert_Cmpstr_Eq (Get_Name (By_Function), Default_Name);

      Gtk_New_Custom
        (By_Custom_Procedure, "ada-procedure", "Ada Procedure", 120.0,
         240.0, Gtk.Enums.Points);
      By_Custom_Function := Gtk_Paper_Size_New_Custom
        ("ada-function", "Ada Function", 100.0, 200.0, Gtk.Enums.Points);
      Assert_Cmpstr_Eq (Get_Display_Name (By_Custom_Procedure), "Ada Procedure");
      Assert_Cmpfloat_Eq
        (Get_Width (By_Custom_Procedure, Gtk.Enums.Points), 120.0);
      Assert_Cmpfloat_Eq
        (Get_Height (By_Custom_Procedure, Gtk.Enums.Points), 240.0);
      Assert_Cmpstr_Eq (Get_Name (By_Custom_Function), "ada-function");

      --  Unknown IPP and PPD names use the supplied display name and size.
      Gtk_New_From_Ipp (From_Ipp, "ada-unknown-ipp", 300.0, 400.0);
      From_Ipp_Function := Gtk_Paper_Size_New_From_Ipp
        ("ada-unknown-ipp-function", 350.0, 450.0);
      Gtk_New_From_Ppd
        (From_Ppd_Procedure, "ada-unknown-ppd-procedure", "Ada PPD Proc",
         450.0, 550.0);
      From_Ppd := Gtk_Paper_Size_New_From_Ppd
        ("ada-unknown-ppd", "Ada PPD", 500.0, 600.0);
      Assert_True (Is_Custom (From_Ipp));
      Assert_True (abs (Get_Width (From_Ipp, Gtk.Enums.Points) - 300.0) < 0.01);
      Assert_True (abs (Get_Height (From_Ipp, Gtk.Enums.Points) - 400.0) < 0.01);
      Assert_True (Is_Custom (From_Ipp_Function));
      Assert_True
        (abs (Get_Width (From_Ipp_Function, Gtk.Enums.Points) - 350.0) < 0.01);
      Assert_True
        (abs (Get_Height (From_Ipp_Function, Gtk.Enums.Points) - 450.0) < 0.01);
      Assert_True (Is_Custom (From_Ppd_Procedure));
      Assert_Cmpstr_Eq
        (Get_Display_Name (From_Ppd_Procedure), "Ada PPD Proc");
      Assert_True
        (abs (Get_Width (From_Ppd_Procedure, Gtk.Enums.Points) - 450.0) < 0.01);
      Assert_True
        (abs (Get_Height (From_Ppd_Procedure, Gtk.Enums.Points) - 550.0) < 0.01);
      Assert_True (Is_Custom (From_Ppd));
      Assert_Cmpstr_Eq (Get_Display_Name (From_Ppd), "Ada PPD");
      Assert_True (abs (Get_Width (From_Ppd, Gtk.Enums.Points) - 500.0) < 0.01);
      Assert_True (abs (Get_Height (From_Ppd, Gtk.Enums.Points) - 600.0) < 0.01);

      --  Set_Size changes the dimensions while retaining the unit conversion.
      Set_Size (By_Custom_Function, 210.0, 297.0, Gtk.Enums.Mm);
      Assert_Cmpfloat_Eq
        (Get_Width (By_Custom_Function, Gtk.Enums.Mm), 210.0);
      Assert_Cmpfloat_Eq
        (Get_Height (By_Custom_Function, Gtk.Enums.Mm), 297.0);

      --  The API reports margins in the requested units; standard GTK paper
      --  sizes may choose their values by backend, but all are nonnegative.
      Assert_True
        (Get_Default_Bottom_Margin (By_Procedure, Gtk.Enums.Mm) >= 0.0);
      Assert_True
        (Get_Default_Left_Margin (By_Procedure, Gtk.Enums.Mm) >= 0.0);
      Assert_True
        (Get_Default_Right_Margin (By_Procedure, Gtk.Enums.Mm) >= 0.0);
      Assert_True
        (Get_Default_Top_Margin (By_Procedure, Gtk.Enums.Mm) >= 0.0);

      Free (From_Ppd);
      Free (From_Ppd_Procedure);
      Free (From_Ipp_Function);
      Free (From_Ipp);
      Free (By_Custom_Function);
      Free (By_Custom_Procedure);
      Free (By_Function);
      Free (By_Procedure);
   end Test_Constructors_And_Margins;

   --  Round-trip through a GKeyFile, including both constructor overloads and
   --  the recoverable error path for a group that is not present.
   procedure Test_Key_File is
      Original : Gtk_Paper_Size := Gtk_Paper_Size_New_Custom
        ("round-trip", "Round Trip", 123.0, 456.0, Gtk.Enums.Points);
      Key_File : Glib.Key_File.G_Key_File;
      From_Key_Procedure : Gtk_Paper_Size;
      From_Key_Function : Gtk_Paper_Size;
      Key_Error : Glib.Error.GError;
   begin
      Glib.Key_File.Gtk_New (Key_File);
      To_Key_File (Original, Key_File, "Paper Size");

      --  Empty Group_Name maps to NULL and reads the first key-file group.
      Gtk_New_From_Key_File
        (From_Key_Procedure, Key_File, Error => Key_Error);
      Assert_True (Key_Error = null);
      Assert_True (Is_Equal (Original, From_Key_Procedure));

      --  The function overload uses the explicit group-name path.
      From_Key_Function := Gtk_Paper_Size_New_From_Key_File
        (Key_File, "Paper Size", Key_Error);
      Assert_True (Key_Error = null);
      Assert_True (Is_Equal (Original, From_Key_Function));

      --  A missing group exercises GTK's recoverable-error branch.
      declare
         Missing : Gtk_Paper_Size;
      begin
         Missing := Gtk_Paper_Size_New_From_Key_File
           (Key_File, "missing group", Key_Error);
         Assert_True (Key_Error /= null);
         Glib.Error.Error_Free (Key_Error);
      end;
      Gtk_New_From_Key_File
        (From_Key_Procedure, Key_File, "missing group", Key_Error);
      Assert_True (Key_Error /= null);
      Glib.Error.Error_Free (Key_Error);

      Free (From_Key_Function);
      Free (From_Key_Procedure);
      Free (Original);
      --  Do not free this GKeyFile: the existing glib-key-file test documents
      --  that GtkAda's imported Free path can abort during group cleanup.
   end Test_Key_File;

   --  GTK creates the a{sv} representation; both Ada overloads must recreate
   --  an equal paper size from that documented representation.
   procedure Test_Gvariant is
      Original : Gtk_Paper_Size := Gtk_Paper_Size_New_Custom
        ("variant-round-trip", "Variant Round Trip", 123.0, 456.0,
         Gtk.Enums.Points);
      From_Variant_Procedure : Gtk_Paper_Size;
      From_Variant_Function : Gtk_Paper_Size;
   begin
      declare
         Variant : constant Glib.Variant.Gvariant := To_Gvariant (Original);
      begin
         From_Variant_Function := Gtk_Paper_Size_New_From_Gvariant (Variant);
         Gtk_New_From_Gvariant (From_Variant_Procedure, Variant);
         Assert_True (Is_Equal (Original, From_Variant_Function));
         Assert_True (Is_Equal (Original, From_Variant_Procedure));
         Glib.Variant.Unref (Variant);
      end;

      Free (From_Variant_Procedure);
      Free (From_Variant_Function);
      Free (Original);
   end Test_Gvariant;

   --  The returned list owns each paper-size item; release those items and
   --  then the list nodes after checking that both list modes are available.
   procedure Test_Paper_Size_List is
      function List_Length
        (Include_Custom : Boolean) return Glib.Guint;

      function List_Length
        (Include_Custom : Boolean) return Glib.Guint
      is
         Items : Paper_Size_List.Glist := Get_Paper_Sizes (Include_Custom);
         Link  : Paper_Size_List.Glist := Paper_Size_List.First (Items);
         Count : Glib.Guint := 0;
      begin
         while Link /= Paper_Size_List.Null_List loop
            declare
               Item : constant Gtk_Paper_Size := Paper_Size_List.Get_Data (Link);
            begin
               Assert_True (Get_Name (Item)'Length > 0);
               Free (Item);
            end;
            Count := Count + 1;
            Link := Paper_Size_List.Next (Link);
         end loop;
         Paper_Size_List.Free (Items);
         return Count;
      end List_Length;

      Standard_Count : constant Glib.Guint := List_Length (False);
      All_Count : constant Glib.Guint := List_Length (True);
   begin
      Assert_True (Standard_Count > 0);
      Assert_True (All_Count >= Standard_Count);
   end Test_Paper_Size_List;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/paper-size/c-cases", Test_C_Paper_Size_Cases'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/paper-size/constructors", Test_Constructors_And_Margins'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/paper-size/key-file", Test_Key_File'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/paper-size/gvariant", Test_Gvariant'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/paper-size/list", Test_Paper_Size_List'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Paper_Size;
