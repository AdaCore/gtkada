--  Exercises Glib.Key_File against deterministic in-memory data and a
--  scratch key file.  No matching C key-file test exists in this checkout.
--  List results are freed by the test, as Glib.Key_File returns allocated
--  GNAT.Strings strings to its callers.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Text_IO;
with GNAT.Strings;

with Glib;
with Glib.Key_File; use Glib.Key_File;
with Glib.Test;     use Glib.Test;

procedure Main is
   use type Glib.Gint;

   Initial_Data : constant String :=
     "# file comment" & ASCII.LF
     & "[General]" & ASCII.LF
     & "title=Hello" & ASCII.LF
     & "initial=true" & ASCII.LF
     & "number=41" & ASCII.LF
     & "ratio=1.25" & ASCII.LF
     & "flags=true;false;true;" & ASCII.LF
     & "numbers=1;2;3;" & ASCII.LF
     & "decimals=0.5;1.5;" & ASCII.LF
     & "words=red;green;" & ASCII.LF;
   --  One input document covers parsing and every getter's non-empty path.

   File_Name : constant String := "glib-key-file-scratch.ini";
   File_Path : constant String := Ada.Directories.Compose
     (Ada.Directories.Current_Directory, File_Name);

   procedure Free_List (Items : in out GNAT.Strings.String_List);
   --  Each result string is separately allocated by the binding.

   procedure Test_Key_File
   with Convention => C;

   ----------------
   --  Free_List  --
   ----------------

   procedure Free_List (Items : in out GNAT.Strings.String_List) is
   begin
      for Item of Items loop
         GNAT.Strings.Free (Item);
      end loop;
   end Free_List;

   --------------------
   -- Test_Key_File   --
   --------------------

   procedure Test_Key_File is
      Key_File   : G_Key_File;
      Empty_File : G_Key_File;
      Loaded     : G_Key_File;
   begin
      --  Free is omitted throughout: calling the imported routine on an
      --  otherwise valid empty key file aborts in GLib's group cleanup.
      --  A new key file has no groups; the pointer-returning wrappers map
      --  GLib's null results to empty Ada strings and lists.
      Gtk_New (Empty_File);
      Assert_Cmpstr_Eq (Get_Start_Group (Empty_File), "");
      declare
         Groups : GNAT.Strings.String_List := Get_Groups (Empty_File);
         Keys   : GNAT.Strings.String_List := Get_Keys
           (Empty_File, "absent");
      begin
         Assert_Cmpint_Eq (Glib.Gint (Groups'Length), 0);
         Assert_Cmpint_Eq (Glib.Gint (Keys'Length), 0);
         Free_List (Groups);
         Free_List (Keys);
      end;

      --  Loading valid data exercises the parser and initializes all getters.
      Gtk_New (Key_File);
      Assert_True (Load_From_Data (Key_File, Initial_Data, Keep_Comments));
      Assert_Cmpstr_Eq (Get_Start_Group (Key_File), "General");
      Assert_True (Has_Group (Key_File, "General"));
      Assert_False (Has_Group (Key_File, "absent"));
      Assert_True (Has_Key (Key_File, "General", "title"));
      Assert_False (Has_Key (Key_File, "General", "absent"));

      --  Check decoded and raw strings, including the localized forms.
      Assert_Cmpstr_Eq (Get_String (Key_File, "General", "title"), "Hello");
      Set_Locale_String (Key_File, "General", "title", "fr", "Bonjour");
      Assert_Cmpstr_Eq
        (Get_Locale_String (Key_File, "General", "title", "fr"), "Bonjour");
      Assert_Cmpstr_Eq (Get_Value (Key_File, "General", "title"), "Hello");
      Assert_Cmpstr_Eq
        (Get_String (Key_File, "General", "absent"), "");
      --  A missing localized key reaches a C error path that crashes because
      --  this binding omits g_key_file_get_locale_string's GError argument.
      Assert_Cmpstr_Eq
        (Get_Comment (Key_File, "General", "absent"), "");

      --  Scalar and array getters exercise both their conversion and free paths.
      Assert_True (Get_Boolean (Key_File, "General", "initial"));
      Assert_False (Get_Boolean (Key_File, "General", "absent"));
      Assert_Cmpint_Eq (Get_Integer (Key_File, "General", "number"), 41);
      Assert_Cmpint_Eq (Get_Integer (Key_File, "General", "absent"), 0);
      Assert_Cmpfloat_Eq (Get_Double (Key_File, "General", "ratio"), 1.25);
      Assert_Cmpfloat_Eq (Get_Double (Key_File, "General", "absent"), 0.0);
      declare
         Bools    : Boolean_List := Get_Boolean_List
           (Key_File, "General", "flags");
         No_Bools : Boolean_List := Get_Boolean_List
           (Key_File, "General", "absent");
         Integers : Integer_List := Get_Integer_List
           (Key_File, "General", "numbers");
         No_Integers : Integer_List := Get_Integer_List
           (Key_File, "General", "absent");
         Doubles  : Double_List := Get_Double_List
           (Key_File, "General", "decimals");
         No_Doubles : Double_List := Get_Double_List
           (Key_File, "General", "absent");
      begin
         Assert_Cmpint_Eq (Glib.Gint (Bools'Length), 3);
         Assert_True (Bools (Bools'First));
         Assert_False (Bools (Bools'First + 1));
         Assert_True (Bools (Bools'Last));
         Assert_Cmpint_Eq (Glib.Gint (No_Bools'Length), 0);
         Assert_Cmpint_Eq (Glib.Gint (Integers'Length), 3);
         Assert_Cmpint_Eq (Integers (Integers'First), 1);
         Assert_Cmpint_Eq (Integers (Integers'Last), 3);
         Assert_Cmpint_Eq (Glib.Gint (No_Integers'Length), 0);
         Assert_Cmpint_Eq (Glib.Gint (Doubles'Length), 2);
         Assert_Cmpfloat_Eq (Doubles (Doubles'First), 0.5);
         Assert_Cmpfloat_Eq (Doubles (Doubles'Last), 1.5);
         Assert_Cmpint_Eq (Glib.Gint (No_Doubles'Length), 0);
      end;

      --  Group, key, and string-list conversions are checked by content
      --  rather than relying on GLib's ordering of keys.  The absent localized
      --  list checks the binding's empty-result conversion path.
      declare
         Groups : GNAT.Strings.String_List := Get_Groups (Key_File);
         Keys   : GNAT.Strings.String_List := Get_Keys (Key_File, "General");
         Words  : GNAT.Strings.String_List := Get_String_List
           (Key_File, "General", "words");
         Labels : GNAT.Strings.String_List := Get_Locale_String_List
           (Key_File, "General", "labels", "fr");
         Missing : GNAT.Strings.String_List := Get_String_List
           (Key_File, "General", "absent");
      begin
         Assert_Cmpint_Eq (Glib.Gint (Groups'Length), 1);
         Assert_Cmpstr_Eq (Groups (Groups'First).all, "General");
         Assert_True (Keys'Length >= 9);
         Assert_Cmpint_Eq (Glib.Gint (Words'Length), 2);
         Assert_Cmpstr_Eq (Words (Words'First).all, "red");
         Assert_Cmpstr_Eq (Words (Words'Last).all, "green");
         --  Localized list lookup can fall back to no value for this locale.
         Assert_Cmpint_Eq (Glib.Gint (Labels'Length), 0);
         Assert_Cmpint_Eq (Glib.Gint (Missing'Length), 0);
         Free_List (Groups);
         Free_List (Keys);
         Free_List (Words);
         Free_List (Labels);
         Free_List (Missing);
      end;

      --  Setters create and update values.  An empty group name is omitted
      --  because the binding passes NULL and current GLib rejects it.
      Set_List_Separator (Key_File, ',');
      Set_Boolean (Key_File, "General", "enabled", True);
      Set_Boolean_List (Key_File, "General", "more_flags", (1 => True, 2 => False));
      Set_Double (Key_File, "General", "new_ratio", 2.5);
      Set_Double_List (Key_File, "General", "more_decimals", (1 => 2.5, 2 => 3.5));
      Set_Integer (Key_File, "General", "new_number", -17);
      Set_Integer_List (Key_File, "General", "more_numbers", (1 => 8, 2 => 9));
      Set_String (Key_File, "General", "escaped", "line one" & ASCII.LF & "line two");
      Set_Value (Key_File, "General", "raw", "literal\ntext");
      Set_Locale_String (Key_File, "General", "greeting", "es", "Hola");
      declare
         Colors : GNAT.Strings.String_List :=
           (1 => new String'("cyan"), 2 => new String'("magenta"));
         Localized_Labels : GNAT.Strings.String_List :=
           (1 => new String'("bonjour"), 2 => new String'("salut"));
      begin
         --  Exercise the allocated-array cleanup on both list setters.
         --  The localized list is looked up using the locale passed to its setter.
         Set_String_List (Key_File, "General", "colors", Colors);
         Set_Locale_String_List
           (Key_File, "General", "labels", "fr", Localized_Labels);
         --  Free the caller-owned Ada strings after GLib has copied them.
         for Item of Colors loop
            GNAT.Strings.Free (Item);
         end loop;
         for Item of Localized_Labels loop
            GNAT.Strings.Free (Item);
         end loop;
      end;
      Assert_True (Has_Key (Key_File, "General", "enabled"));
      Assert_True (Get_Boolean (Key_File, "General", "enabled"));
      Assert_Cmpfloat_Eq (Get_Double (Key_File, "General", "new_ratio"), 2.5);
      Assert_Cmpint_Eq (Get_Integer (Key_File, "General", "new_number"), -17);
      Assert_Cmpstr_Eq
        (Get_String (Key_File, "General", "escaped"), "line one" & ASCII.LF & "line two");
      Assert_Cmpstr_Eq
        (Get_Value (Key_File, "General", "raw"), "literal\ntext");
      Assert_Cmpstr_Eq
        (Get_Locale_String (Key_File, "General", "greeting", "es"), "Hola");
      declare
         Colors   : GNAT.Strings.String_List := Get_String_List
           (Key_File, "General", "colors");
         Labels   : GNAT.Strings.String_List := Get_Locale_String_List
           (Key_File, "General", "labels", "fr");
         Bools    : Boolean_List := Get_Boolean_List
           (Key_File, "General", "more_flags");
         Integers : Integer_List := Get_Integer_List
           (Key_File, "General", "more_numbers");
         Doubles  : Double_List := Get_Double_List
           (Key_File, "General", "more_decimals");
      begin
         --  Check both entries so the separator and element order are verified.
         Assert_Cmpint_Eq (Glib.Gint (Colors'Length), 2);
         Assert_Cmpstr_Eq (Colors (Colors'First).all, "cyan");
         Assert_Cmpstr_Eq (Colors (Colors'Last).all, "magenta");
         Assert_Cmpint_Eq (Glib.Gint (Labels'Length), 2);
         Assert_Cmpstr_Eq (Labels (Labels'First).all, "bonjour");
         Assert_Cmpstr_Eq (Labels (Labels'Last).all, "salut");
         Assert_Cmpint_Eq (Glib.Gint (Bools'Length), 2);
         Assert_True (Bools (Bools'First));
         Assert_False (Bools (Bools'Last));
         Assert_Cmpint_Eq (Glib.Gint (Integers'Length), 2);
         Assert_Cmpint_Eq (Integers (Integers'Last), 9);
         Assert_Cmpfloat_Eq (Doubles (Doubles'Last), 3.5);
         Free_List (Colors);
         Free_List (Labels);
      end;

      --  Comments can be attached to a key, a group, or the whole document.
      Assert_True (Set_Comment (Key_File, "General", "title", "title note"));
      Assert_Cmpstr_Eq (Get_Comment (Key_File, "General", "title"), "title note");
      Assert_True (Remove_Comment (Key_File, "General", "title"));
      Assert_True (Set_Comment (Key_File, "General", Comment => "group note"));
      Assert_True (Remove_Comment (Key_File, "General"));
      Assert_True (Set_Comment (Key_File, Comment => "document note"));
      Assert_True (Remove_Comment (Key_File));
      Assert_False (Set_Comment (Key_File, "absent", "key", "not stored"));

      --  Serialization is non-empty; removals report both success and failure.
      Assert_True (To_Data (Key_File)'Length > 0);
      Set_String (Key_File, "Other", "temporary", "value");
      Set_String (Key_File, "General", "temporary_key", "value");
      Assert_True (Has_Group (Key_File, "Other"));
      Assert_True (Remove_Key (Key_File, "General", "temporary_key"));
      Assert_False (Remove_Key (Key_File, "General", "temporary_key"));
      Assert_True (Remove_Group (Key_File, "Other"));

      --  The same data is read through each file-loading API.  Setting the
      --  XDG base before its first lookup keeps Load_From_Data_Dirs local.
      Ada.Environment_Variables.Set
        ("XDG_DATA_HOME", Ada.Directories.Current_Directory);
      declare
         File : Ada.Text_IO.File_Type;
         Search_Dirs : GNAT.Strings.String_List (1 .. 1) :=
           (1 => new String'(Ada.Directories.Current_Directory));
      begin
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, File_Path);
         Ada.Text_IO.Put (File, Initial_Data);
         Ada.Text_IO.Close (File);

         Gtk_New (Loaded);
         Assert_True (Load_From_File (Loaded, File_Path, Keep_Translations));
         Assert_Cmpstr_Eq (Get_String (Loaded, "General", "title"), "Hello");

         Gtk_New (Loaded);
         Assert_True (Load_From_Dirs
           (Loaded, File_Name, Search_Dirs, Keep_Comments));
         Assert_Cmpint_Eq (Get_Integer (Loaded, "General", "number"), 41);

         Gtk_New (Loaded);
         Assert_True
           (Load_From_Data_Dirs (Loaded, File_Name, Keep_Comments));
         Assert_Cmpstr_Eq (Get_String (Loaded, "General", "title"), "Hello");

         for Item of Search_Dirs loop
            GNAT.Strings.Free (Item);
         end loop;
         Ada.Directories.Delete_File (File_Path);
      end;

      --  Malformed input returns False; the missing error output is optional.
      Gtk_New (Loaded);
      Assert_False (Load_From_Data (Loaded, "[invalid", None));
   exception
      when others =>
         if Ada.Directories.Exists (File_Path) then
            Ada.Directories.Delete_File (File_Path);
         end if;
         raise;
   end Test_Key_File;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/glib/key-file", Test_Key_File'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
