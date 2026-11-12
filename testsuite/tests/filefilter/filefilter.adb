--  Gtk.File_Filter tests, porting the direct API cases from
--  testsuite/c_tests/filefilter.c (test_basic, test_pattern, test_suffix,
--  test_mime_type and test_variant_no_name).
--
--  Matching rules are checked with in-memory GFileInfo objects, so the tests
--  need no filesystem files or file chooser.

with Ada.Command_Line;
with GNAT.Strings;

with Glib;             use Glib;
with Glib.File_Info;   use Glib.File_Info;
with Glib.Object;      use Glib.Object;
with Glib.Test;        use Glib.Test;
with Glib.Variant;     use Glib.Variant;

with Gtk.File_Filter;  use Gtk.File_Filter;
with Gtk.Filter;
with Gtkada.Bindings;
with Gtkada.Types;

procedure Filefilter is

   use type Gtk.File_Filter.Gtk_File_Filter;
   use type Glib.Variant.Gvariant;

   procedure Test_Basic with Convention => C;
   procedure Test_Constructor_Families with Convention => C;
   procedure Test_Pattern with Convention => C;
   procedure Test_Suffix with Convention => C;
   procedure Test_Mime_Types with Convention => C;
   procedure Test_Pixbuf_Formats with Convention => C;
   procedure Test_Gvariant_Round_Trip with Convention => C;

   function Content_Type_From_Mime_Type
     (Mime_Type : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
   pragma Import (C, Content_Type_From_Mime_Type,
                  "g_content_type_from_mime_type");

   procedure Set_Content_Type_From_Mime
     (Info : not null access Gfile_Info_Record'Class;
      Mime : String);

   ---------------------
   -- Assert_Attribute --
   ---------------------

   procedure Assert_Attribute
     (Filter : not null access Gtk_File_Filter_Record'Class;
      Name   : String)
   is
      Attributes : GNAT.Strings.String_List := Filter.Get_Attributes;
   begin
      --  Each rule family in these cases needs exactly one GFileInfo field.
      Assert_True (Attributes'Length = 1);
      Assert_Cmpstr_Eq (Attributes (Attributes'First).all, Name);
      GNAT.Strings.Free (Attributes (Attributes'First));
   end Assert_Attribute;

   -------------------------------
   -- Set_Content_Type_From_Mime --
   -------------------------------

   procedure Set_Content_Type_From_Mime
     (Info : not null access Gfile_Info_Record'Class;
      Mime : String)
   is
      Mime_Ptr : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String (Mime);
      Type_Ptr : Gtkada.Types.Chars_Ptr :=
        Content_Type_From_Mime_Type (Mime_Ptr);
      Content_Type : constant String :=
        Gtkada.Bindings.Value_Allowing_Null (Type_Ptr);
   begin
      --  GLib maps MIME names to platform content-type names. Copy the result
      --  into GFileInfo, then release both strings returned/allocated here.
      Assert_True (Content_Type'Length > 0);
      Info.Set_Content_Type (Content_Type);
      Gtkada.Types.Free (Mime_Ptr);
      Gtkada.Types.Free (Type_Ptr);
   end Set_Content_Type_From_Mime;

   ----------------
   -- Test_Basic --
   ----------------

   procedure Test_Basic is
      Filter : constant Gtk_File_Filter := Gtk_File_Filter_New;
   begin
      --  A new filter has no name; setting and clearing it exercises the
      --  binding's nullable-string handling in both directions.
      Assert_True (Filter /= null);
      Assert_Cmpstr_Eq (Filter.Get_Name, "");
      Filter.Set_Name ("test");
      Assert_Cmpstr_Eq (Filter.Get_Name, "test");
      Filter.Set_Name ("");
      Assert_Cmpstr_Eq (Filter.Get_Name, "");

      --  Initialize must leave an already-created wrapper unchanged.
      Filter.Set_Name ("kept");
      Gtk.File_Filter.Initialize (Filter);
      Assert_Cmpstr_Eq (Filter.Get_Name, "kept");
      Unref (Filter);
   end Test_Basic;

   ------------------------------
   -- Test_Constructor_Families --
   ------------------------------

   procedure Test_Constructor_Families is
      By_Function : constant Gtk_File_Filter := Gtk_File_Filter_New;
      By_Procedure : Gtk_File_Filter;
   begin
      --  Exercise both ordinary constructor wrappers and the idempotent
      --  Initialize path on a fresh procedural result.
      Gtk_New (By_Procedure);
      Assert_True (By_Function /= null);
      Assert_True (By_Procedure /= null);
      By_Function.Set_Name ("function constructor");
      By_Procedure.Set_Name ("procedure constructor");
      Gtk.File_Filter.Initialize (By_Procedure);
      Assert_Cmpstr_Eq (By_Procedure.Get_Name, "procedure constructor");
      Unref (By_Function);
      Unref (By_Procedure);
   end Test_Constructor_Families;

   ------------------
   -- Test_Pattern --
   ------------------

   procedure Test_Pattern is
      Filter : constant Gtk_File_Filter := Gtk_File_Filter_New;
      Info   : constant Gfile_Info := Gfile_Info_New;
   begin
      --  This is the C test's case-sensitive glob: the capital D matches only
      --  the display name beginning with D on Unix-like platforms.
      Filter.Add_Pattern ("D*.st[xy]");
      Assert_Attribute (Filter, "standard::display-name");

      Info.Set_Display_Name ("abracadabra");
      Assert_False (Gtk.Filter.Match (Gtk.Filter.Gtk_Filter (Filter), Info));
      Info.Set_Display_Name ("dro.stx");
      Assert_False (Gtk.Filter.Match (Gtk.Filter.Gtk_Filter (Filter), Info));
      Info.Set_Display_Name ("Dro.sty");
      Assert_True (Gtk.Filter.Match (Gtk.Filter.Gtk_Filter (Filter), Info));

      Unref (Info);
      Unref (Filter);
   end Test_Pattern;

   -----------------
   -- Test_Suffix --
   -----------------

   procedure Test_Suffix is
      Filter : constant Gtk_File_Filter := Gtk_File_Filter_New;
      Info   : constant Gfile_Info := Gfile_Info_New;
   begin
      --  Suffix matching is case-insensitive even though glob matching may
      --  depend on the platform's case rules.
      Filter.Add_Suffix ("txt");
      Assert_Attribute (Filter, "standard::display-name");

      Info.Set_Display_Name ("abracadabra");
      Assert_False (Gtk.Filter.Match (Gtk.Filter.Gtk_Filter (Filter), Info));
      Info.Set_Display_Name ("dro.txt");
      Assert_True (Gtk.Filter.Match (Gtk.Filter.Gtk_Filter (Filter), Info));
      Info.Set_Display_Name ("dro.TXT");
      Assert_True (Gtk.Filter.Match (Gtk.Filter.Gtk_Filter (Filter), Info));

      Unref (Info);
      Unref (Filter);
   end Test_Suffix;

   ---------------------
   -- Test_Mime_Types --
   ---------------------

   procedure Test_Mime_Types is
      Filter      : constant Gtk_File_Filter := Gtk_File_Filter_New;
      Info        : constant Gfile_Info := Gfile_Info_New;
      Mime_Types  : GNAT.Strings.String_List :=
        (new String'("image/png"), new String'("image/jpeg"));
   begin
      --  Match behavior follows the C test: unrelated MIME types are rejected,
      --  while image/png is accepted regardless of the display-name suffix.
      Filter.Add_Mime_Type ("image/png");
      Assert_Attribute (Filter, "standard::content-type");

      Info.Set_Display_Name ("abracadabra");
      Set_Content_Type_From_Mime (Info, "text/plain");
      Assert_False (Gtk.Filter.Match (Gtk.Filter.Gtk_Filter (Filter), Info));
      Info.Set_Display_Name ("dro.png");
      Set_Content_Type_From_Mime (Info, "image/png");
      Assert_True (Gtk.Filter.Match (Gtk.Filter.Gtk_Filter (Filter), Info));
      Info.Set_Display_Name ("dro.PNG");
      Assert_True (Gtk.Filter.Match (Gtk.Filter.Gtk_Filter (Filter), Info));

      --  The array form is equivalent to adding each supported MIME type.
      Filter.Add_Mime_Types (Mime_Types);
      Assert_Attribute (Filter, "standard::content-type");
      Set_Content_Type_From_Mime (Info, "image/jpeg");
      Assert_True (Gtk.Filter.Match (Gtk.Filter.Gtk_Filter (Filter), Info));

      --  Add_Mime_Types copies the strings into its C argument array; the
      --  original GNAT.Strings allocations remain owned by this test.
      for Mime_Type of Mime_Types loop
         GNAT.Strings.Free (Mime_Type);
      end loop;
      Unref (Info);
      Unref (Filter);
   end Test_Mime_Types;

   -------------------------
   -- Test_Pixbuf_Formats --
   -------------------------

   procedure Test_Pixbuf_Formats is
      Filter : constant Gtk_File_Filter := Gtk_File_Filter_New;
   begin
      --  Keep coverage for the deprecated convenience rule because it remains
      --  callable and is part of the C filefilter test's serialization cases.
      Filter.Add_Pixbuf_Formats;
      Assert_Attribute (Filter, "standard::content-type");
      Unref (Filter);
   end Test_Pixbuf_Formats;

   ------------------------------
   -- Test_Gvariant_Round_Trip --
   ------------------------------

   procedure Test_Gvariant_Round_Trip is
      Original : constant Gtk_File_Filter := Gtk_File_Filter_New;
   begin
      --  The suffix rule verifies that deserialization preserves real content
      --  and the name checks that a user-supplied label survives the round trip.
      Original.Set_Name ("Text files");
      Original.Add_Suffix ("txt");
      declare
         Variant : constant Gvariant := Original.To_Gvariant;
         By_Function : constant Gtk_File_Filter :=
           Gtk_File_Filter_New_From_Gvariant (Variant);
         By_Procedure : Gtk_File_Filter;
      begin
         Assert_True (Variant /= Null_Gvariant);
         Gtk_New_From_Gvariant (By_Procedure, Variant);
         Assert_Cmpstr_Eq (By_Function.Get_Name, "Text files");
         Assert_Cmpstr_Eq (By_Procedure.Get_Name, "Text files");
         Assert_Attribute (By_Function, "standard::display-name");
         Assert_Attribute (By_Procedure, "standard::display-name");

         --  Calling the initializer on an existing filter is specified as a
         --  no-op, just like the ordinary Initialize method above.
         By_Procedure.Set_Name ("unchanged");
         Gtk.File_Filter.Initialize_From_Gvariant (By_Procedure, Variant);
         Assert_Cmpstr_Eq (By_Procedure.Get_Name, "unchanged");

         Unref (By_Function);
         Unref (By_Procedure);
         Glib.Variant.Unref (Variant);
      end;

      --  The C suite also serializes a filter with no name or rules. GTK
      --  supplies a readable fallback name after deserialization.
      declare
         Nameless : constant Gtk_File_Filter := Gtk_File_Filter_New;
         Variant  : constant Gvariant := Nameless.To_Gvariant;
         By_Function : constant Gtk_File_Filter :=
           Gtk_File_Filter_New_From_Gvariant (Variant);
         By_Procedure : Gtk_File_Filter;
      begin
         Gtk_New_From_Gvariant (By_Procedure, Variant);
         Assert_True (By_Function.Get_Name'Length > 0);
         Assert_True (By_Procedure.Get_Name'Length > 0);
         Unref (By_Function);
         Unref (By_Procedure);
         Glib.Variant.Unref (Variant);
         Unref (Nameless);
      end;
      Unref (Original);
   end Test_Gvariant_Round_Trip;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/filefilter/basic", Test_Basic'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/filefilter/constructors", Test_Constructor_Families'Unrestricted_Access);
   Glib.Test.Add_Func ("/filefilter/pattern", Test_Pattern'Unrestricted_Access);
   Glib.Test.Add_Func ("/filefilter/suffix", Test_Suffix'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/filefilter/mime-types", Test_Mime_Types'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/filefilter/pixbuf-formats", Test_Pixbuf_Formats'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/filefilter/gvariant-round-trip",
      Test_Gvariant_Round_Trip'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Filefilter;
