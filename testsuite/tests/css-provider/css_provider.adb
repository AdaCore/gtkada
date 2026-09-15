--  Gtk.Css_Provider coverage, based on GTK's cssprovider.c tests.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Glib;             use Glib;
with Glib.Bytes;       use Glib.Bytes;
with Glib.GFile;       use Glib.GFile;
with Glib.Object;      use Glib.Object;
with Glib.Test;        use Glib.Test;
with Glib.Types;

with Gtk.Css_Provider; use Gtk.Css_Provider;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Main;

procedure Css_Provider is

   subtype Css_File is Glib.GFile.Gfile;

   Css      : constant UTF8_String := "button { color: blue; }";
   Css_Path : constant String := "provider.css";

   function Contains
     (Text    : String;
      Pattern : String) return Boolean
   is (Ada.Strings.Fixed.Index (Text, Pattern) /= 0);

   function To_Bytes (Text : String) return Guint8_Array;
   procedure Release (File : Css_File);

   procedure Test_Load_From_String with Convention => C;
   procedure Test_Load_From_Data with Convention => C;
   procedure Test_Load_From_Bytes with Convention => C;
   procedure Test_Load_From_Path with Convention => C;
   procedure Test_Load_From_File with Convention => C;

   --------------
   -- To_Bytes --
   --------------

   function To_Bytes (Text : String) return Guint8_Array is
      Result : Guint8_Array (1 .. Text'Length);
      Index  : Natural := Result'First;
   begin
      for Character of Text loop
         Result (Index) := Guint8 (Standard.Character'Pos (Character));
         Index := Index + 1;
      end loop;

      return Result;
   end To_Bytes;

   -------------
   -- Release --
   -------------

   procedure Release (File : Css_File) is
   begin
      if File /= Null_Gfile then
         Unref (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
      end if;
   end Release;

   ---------------------------
   -- Test_Load_From_String --
   ---------------------------

   procedure Test_Load_From_String is
      Provider : constant Gtk_Css_Provider := Gtk_Css_Provider_New;
      Source   : constant UTF8_String :=
        "@media (prefers-color-scheme: light) {"
        & " include-me { color: blue; } }"
        & "@media (prefers-color-scheme: dark) {"
        & " skip-me { color: blue; } }";
   begin
      Set_Property
        (Provider,
         Prefers_Color_Scheme_Property,
         Interface_Color_Scheme_Light);
      Assert_True
        (Gtk.Enums.Get_Property
           (Provider, Prefers_Color_Scheme_Property)
         = Interface_Color_Scheme_Light);

      Provider.Load_From_String (Source);
      declare
         Rendered : constant UTF8_String := Provider.To_String;
      begin
         Assert_True (Contains (Rendered, "include-me"));
         Assert_True (not Contains (Rendered, "skip-me"));
      end;
      Unref (Provider);
   end Test_Load_From_String;

   -------------------------
   -- Test_Load_From_Data --
   -------------------------

   procedure Test_Load_From_Data is
      Provider : constant Gtk_Css_Provider := Gtk_Css_Provider_New;
   begin
      Provider.Load_From_Data (Css);
      Assert_True (Contains (Provider.To_String, "button"));
      Unref (Provider);
   end Test_Load_From_Data;

   --------------------------
   -- Test_Load_From_Bytes --
   --------------------------

   procedure Test_Load_From_Bytes is
      Data     : constant Guint8_Array := To_Bytes (Css);
      Bytes    : constant Gbytes := Gbytes_New (Data, Gsize (Data'Length));
      Provider : Gtk_Css_Provider;
   begin
      Gtk_New (Provider);
      Provider.Load_From_Bytes (Bytes);
      Assert_True (Contains (Provider.To_String, "button"));
      Bytes.Unref;
      Unref (Provider);
   end Test_Load_From_Bytes;

   -------------------------
   -- Test_Load_From_Path --
   -------------------------

   procedure Test_Load_From_Path is
      Provider : constant Gtk_Css_Provider := Gtk_Css_Provider_New;
   begin
      Provider.Load_From_Path (Css_Path);
      Assert_True (Contains (Provider.To_String, "button"));
      Unref (Provider);
   end Test_Load_From_Path;

   -------------------------
   -- Test_Load_From_File --
   -------------------------

   procedure Test_Load_From_File is
      File     : constant Css_File := New_For_Path (Css_Path);
      Provider : constant Gtk_Css_Provider := Gtk_Css_Provider_New;
   begin
      Provider.Load_From_File (File);
      Assert_True (Contains (Provider.To_String, "button"));
      Unref (Provider);
      Release (File);
   end Test_Load_From_File;

begin
   declare
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Css_Path);
      Ada.Text_IO.Put_Line (File, Css);
      Ada.Text_IO.Close (File);
   end;

   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/css-provider/load-from-string",
      Test_Load_From_String'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/css-provider/load-from-data",
      Test_Load_From_Data'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/css-provider/load-from-bytes",
      Test_Load_From_Bytes'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/css-provider/load-from-path",
      Test_Load_From_Path'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/css-provider/load-from-file",
      Test_Load_From_File'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
   Ada.Directories.Delete_File (Css_Path);
end Css_Provider;
