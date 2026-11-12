--  Exercise GLib's GOptionContext bindings. No focused C GOption test exists
--  in testsuite/c_tests; the option parsing in other C tests only handles
--  their own test-runner flags.

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with GNAT.Strings;
with System;

with Glib;
with Glib.Application;
with Glib.Error;
with Glib.Object;
with Glib.Option;
with Glib.Test;
with Gtkada.Bindings;
with Gtkada.Types;

procedure Main is
   use type Glib.Error.GError;
   use type Glib.Gboolean;
   use type Glib.Application.GApplication_Flags;
   use type Gtkada.Types.Chars_Ptr;

   function To_Argv is new Ada.Unchecked_Conversion
     (System.Address, Gtkada.Bindings.chars_ptr_array_access);
   --  Parse's argv overload expects a C pointer to the first array element.

   --  GLib exposes group construction and entry registration in C; these
   --  imports create valid groups for the ownership-taking GtkAda wrappers.
   function New_Option_Group
     (Name, Description, Help_Description : Gtkada.Types.Chars_Ptr;
      User_Data, Destroy_Notify           : System.Address)
      return Glib.Option.GOption_Group;
   pragma Import (C, New_Option_Group, "g_option_group_new");

   procedure Add_Option_Group_Entries
     (Group : Glib.Option.GOption_Group;
      Items : Glib.Option.GOption_Entry_Array);
   pragma Import (C, Add_Option_Group_Entries, "g_option_group_add_entries");

   Destroyed_User_Data : Natural := 0;

   --  The generic callback path owns its user data until the context is freed.
   procedure Destroy_Translation_Data (Data : in out Integer);
   function Translate_With_Data
     (Text : String; Prefix : Integer) return String;

   package Translation_With_Data is new
     Glib.Option.Set_Translate_Func_User_Data
       (Integer, Destroy_Translation_Data);

   procedure Destroy_Translation_Data (Data : in out Integer) is
   begin
      Data := 0;
      Destroyed_User_Data := Destroyed_User_Data + 1;
   end Destroy_Translation_Data;

   function Translate_With_Data
     (Text : String; Prefix : Integer) return String is
   begin
      return Integer'Image (Prefix) & ":" & Text;
   end Translate_With_Data;

   --  A direct translator covers the callback trampoline without user data.
   function Translate (Text : String) return String is
   begin
      return "translated:" & Text;
   end Translate;

   function Contains (Text, Pattern : String) return Boolean is
     (Ada.Strings.Fixed.Index (Text, Pattern) /= 0);

   procedure Parse_Args
     (Context : Glib.Option.Goption_Context;
      Args    : in out Gtkada.Types.Chars_Ptr_Array;
      Success : out Boolean;
      Error   : out Glib.Error.GError)
   is
      Argv : aliased Gtkada.Bindings.chars_ptr_array_access :=
        To_Argv (Args'Address);
   begin
      --  GLib edits argv in place; Args is a null-terminated C-compatible
      --  array whose strings remain owned by this test.
      Glib.Option.Parse (Context, Argv'Access, Success, Error);
   end Parse_Args;

   procedure Test_Context_And_Help with Convention => C;
   procedure Test_Groups with Convention => C;
   procedure Test_Parse_And_Errors with Convention => C;
   procedure Test_Translation with Convention => C;

   --  Exercise context properties, generated help, and main-entry handling.
   procedure Test_Context_And_Help is
      Flag_Value : aliased Glib.Gboolean := 0;
      Count      : aliased Glib.Gint := 0;
      Entries    : Glib.Option.GOption_Entry_Array (0 .. 2);
      Context    : Glib.Option.Goption_Context :=
        Glib.Option.G_New ("FILES");
   begin
      Entries (0) :=
        (Long_Name       => Gtkada.Types.New_String ("verbose"),
         Short_Name      => 'v',
         Flags           => Glib.Option.G_Option_Flag_None,
         Arg             => Glib.Option.G_Option_Arg_None,
         Arg_Data        => Flag_Value'Address,
         Description     => Gtkada.Types.New_String ("Enable verbose mode"),
         Arg_Description => Gtkada.Types.Null_Ptr);
      Entries (1) :=
        (Long_Name       => Gtkada.Types.New_String ("count"),
         Short_Name      => 'c',
         Flags           => Glib.Option.G_Option_Flag_None,
         Arg             => Glib.Option.G_Option_Arg_Int,
         Arg_Data        => Count'Address,
         Description     => Gtkada.Types.New_String ("Set a count"),
         Arg_Description => Gtkada.Types.New_String ("NUMBER"));
      Entries (2) := Glib.Option.Null_GOption_Entry;

      --  The context copies entries into its main group, while help text uses
      --  both descriptions and the parameter label from these entries.
      --  Empty and non-empty strings both map to the documented null/string
      --  forms; getters return the value that was installed.
      Glib.Test.Assert_Cmpstr_Eq
        (Glib.Option.Get_Description (Context), "");
      Glib.Option.Set_Description (Context, "Option context test");
      Glib.Test.Assert_Cmpstr_Eq
        (Glib.Option.Get_Description (Context), "Option context test");
      Glib.Option.Set_Description (Context, "");
      Glib.Test.Assert_Cmpstr_Eq
        (Glib.Option.Get_Description (Context), "");
      Glib.Option.Set_Summary (Context, "Test option parsing");
      Glib.Test.Assert_Cmpstr_Eq
        (Glib.Option.Get_Summary (Context), "Test option parsing");
      Glib.Option.Set_Summary (Context, "");
      Glib.Test.Assert_Cmpstr_Eq (Glib.Option.Get_Summary (Context), "");

      Glib.Test.Assert_True (Glib.Option.Get_Help_Enabled (Context));
      Glib.Option.Set_Help_Enabled (Context, False);
      Glib.Test.Assert_False (Glib.Option.Get_Help_Enabled (Context));
      Glib.Option.Set_Help_Enabled (Context, True);
      Glib.Option.Set_Ignore_Unknown_Options (Context, True);
      Glib.Test.Assert_True
        (Glib.Option.Get_Ignore_Unknown_Options (Context));
      Glib.Option.Set_Ignore_Unknown_Options (Context, False);
      Glib.Test.Assert_False
        (Glib.Option.Get_Ignore_Unknown_Options (Context));
      Glib.Option.Set_Strict_Posix (Context, True);
      Glib.Test.Assert_True (Glib.Option.Get_Strict_Posix (Context));
      Glib.Option.Set_Strict_Posix (Context, False);
      Glib.Test.Assert_False (Glib.Option.Get_Strict_Posix (Context));
      Glib.Option.Set_Translation_Domain (Context, "gtkada-test-domain");

      --  Add_Main_Entries creates the main group and copies the option names.
      Glib.Option.Add_Main_Entries
        (Context, Entries, "gtkada-test-domain");
      Glib.Test.Assert_True
        (Contains (Glib.Option.Get_Help (Context, True, null), "--verbose"));
      Glib.Test.Assert_True
        (Contains (Glib.Option.Get_Help (Context, True, null), "--count"));

      Glib.Option.Free (Context);
      --  Release the strings supplied by Ada after all GLib help calls finish.
      Gtkada.Types.Free (Entries (0).Long_Name);
      Gtkada.Types.Free (Entries (0).Description);
      Gtkada.Types.Free (Entries (1).Long_Name);
      Gtkada.Types.Free (Entries (1).Description);
      Gtkada.Types.Free (Entries (1).Arg_Description);
   end Test_Context_And_Help;

   --  Build valid C groups because GtkAda does not publish a GOptionGroup
   --  constructor; each context takes ownership of its group.
   procedure Test_Groups is
      Added_Flag  : aliased Glib.Gboolean := 0;
      Main_Flag   : aliased Glib.Gboolean := 0;
      Context     : Glib.Option.Goption_Context;
      Group       : Glib.Option.GOption_Group;
      Name        : Gtkada.Types.Chars_Ptr;
      Description : Gtkada.Types.Chars_Ptr;
      Group_Help  : Gtkada.Types.Chars_Ptr;
      Items       : Glib.Option.GOption_Entry_Array (0 .. 1);
   begin
      --  A named group added to the context appears in --help-all output.
      Name := Gtkada.Types.New_String ("extra");
      Description := Gtkada.Types.New_String ("Extra group");
      Group_Help := Gtkada.Types.New_String ("Extra group help");
      Group := New_Option_Group
        (Name, Description, Group_Help, System.Null_Address,
         System.Null_Address);
      Items (0) :=
        (Long_Name       => Gtkada.Types.New_String ("extra-option"),
         Short_Name      => 'e',
         Flags           => Glib.Option.G_Option_Flag_None,
         Arg             => Glib.Option.G_Option_Arg_None,
         Arg_Data        => Added_Flag'Address,
         Description     => Gtkada.Types.New_String ("Extra option"),
         Arg_Description => Gtkada.Types.Null_Ptr);
      Items (1) := Glib.Option.Null_GOption_Entry;
      Add_Option_Group_Entries (Group, Items);
      --  Add_Group transfers the group to this context, so Free owns cleanup.
      Context := Glib.Option.G_New;
      Glib.Option.Add_Group (Context, Group);
      Glib.Test.Assert_True
        (Contains
           (Glib.Option.Get_Help (Context, False, null), "--extra-option"));
      Glib.Option.Free (Context);
      Gtkada.Types.Free (Name);
      Gtkada.Types.Free (Description);
      Gtkada.Types.Free (Group_Help);
      Gtkada.Types.Free (Items (0).Long_Name);
      Gtkada.Types.Free (Items (0).Description);

      --  A main group contributes its options to ordinary --help output.
      --  GLib requires a main group to have no group name.
      Name := Gtkada.Types.Null_Ptr;
      Description := Gtkada.Types.New_String ("Main group");
      Group_Help := Gtkada.Types.New_String ("Main group help");
      Group := New_Option_Group
        (Name, Description, Group_Help, System.Null_Address,
         System.Null_Address);
      Items (0) :=
        (Long_Name       => Gtkada.Types.New_String ("primary-option"),
         Short_Name      => 'p',
         Flags           => Glib.Option.G_Option_Flag_None,
         Arg             => Glib.Option.G_Option_Arg_None,
         Arg_Data        => Main_Flag'Address,
         Description     => Gtkada.Types.New_String ("Primary option"),
         Arg_Description => Gtkada.Types.Null_Ptr);
      Items (1) := Glib.Option.Null_GOption_Entry;
      Add_Option_Group_Entries (Group, Items);
      Context := Glib.Option.G_New;
      Glib.Option.Set_Main_Group (Context, Group);
      Glib.Test.Assert_True
        (Contains
           (Glib.Option.Get_Help (Context, True, null), "--primary-option"));
      Glib.Option.Free (Context);
      Gtkada.Types.Free (Description);
      Gtkada.Types.Free (Group_Help);
      Gtkada.Types.Free (Items (0).Long_Name);
      Gtkada.Types.Free (Items (0).Description);
   end Test_Groups;

   --  Parse real argv arrays and check outputs, error codes, and leftovers.
   procedure Test_Parse_And_Errors is
      Flag_Value : aliased Glib.Gboolean := 0;
      Count      : aliased Glib.Gint := 0;
      Entries    : Glib.Option.GOption_Entry_Array (0 .. 2);
      P0         : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("test");
      P1         : Gtkada.Types.Chars_Ptr;
      P2         : Gtkada.Types.Chars_Ptr;
      Context    : Glib.Option.Goption_Context;
      Success    : Boolean;
      Error      : Glib.Error.GError;

      --  Exclude one argument while preserving options understood by context.
      function Keep_Option (Param : String) return Boolean;
      function Keep_Option (Param : String) return Boolean is
        (Param /= "--discard");

      --  GApplication supplies a real command-line object to this callback.
      function Parse_Command_Line
        (Self         : access Glib.Application.Gapplication_Record'Class;
         Command_Line : not null access
           Glib.Application.Gapplication_Command_Line_Record'Class)
         return Glib.Gint;
      function Parse_Command_Line
        (Self         : access Glib.Application.Gapplication_Record'Class;
         Command_Line : not null access
           Glib.Application.Gapplication_Command_Line_Record'Class)
         return Glib.Gint
      is
         pragma Unreferenced (Self);
         No_Filter_Context : Glib.Option.Goption_Context :=
           Glib.Option.G_New;
      begin
         --  The no-filter overload parses a separate argv copy and leaves
         --  unknown options for a later command-line layer.
         Glib.Option.Add_Main_Entries (No_Filter_Context, Entries);
         Glib.Option.Set_Ignore_Unknown_Options (No_Filter_Context, True);
         Glib.Option.Parse
           (No_Filter_Context, Command_Line,
            Success => Success, Error => Error);
         Glib.Test.Assert_True (Success);
         Glib.Test.Assert_True (Error = null);
         Glib.Test.Assert_True (Flag_Value /= 0);
         Glib.Test.Assert_Cmpint_Eq (Count, 23);
         Glib.Option.Free (No_Filter_Context);

         --  The filtered overload drops --discard before parsing the options.
         Flag_Value := 0;
         Count := 0;
         Glib.Option.Parse
           (Context, Command_Line, Keep_Option'Unrestricted_Access,
            Success, Error);
         Glib.Test.Assert_True (Success);
         Glib.Test.Assert_True (Error = null);
         Glib.Test.Assert_True (Flag_Value /= 0);
         Glib.Test.Assert_Cmpint_Eq (Count, 23);
         Command_Line.Done;
         return 0;
      end Parse_Command_Line;

   begin
      --  Entries point at Ada variables used by GOption to store parsed data.
      Entries (0) :=
        (Long_Name       => Gtkada.Types.New_String ("verbose"),
         Short_Name      => 'v',
         Flags           => Glib.Option.G_Option_Flag_None,
         Arg             => Glib.Option.G_Option_Arg_None,
         Arg_Data        => Flag_Value'Address,
         Description     => Gtkada.Types.New_String ("Enable verbose mode"),
         Arg_Description => Gtkada.Types.Null_Ptr);
      Entries (1) :=
        (Long_Name       => Gtkada.Types.New_String ("count"),
         Short_Name      => 'c',
         Flags           => Glib.Option.G_Option_Flag_None,
         Arg             => Glib.Option.G_Option_Arg_Int,
         Arg_Data        => Count'Address,
         Description     => Gtkada.Types.New_String ("Set a count"),
         Arg_Description => Gtkada.Types.New_String ("NUMBER"));
      Entries (2) := Glib.Option.Null_GOption_Entry;

      --  Retain each original pointer so cleanup is independent of GLib's
      --  in-place argv compaction.
      Context := Glib.Option.G_New;
      Glib.Option.Add_Main_Entries (Context, Entries);
      P1 := Gtkada.Types.New_String ("--verbose");
      P2 := Gtkada.Types.New_String ("--count=17");
      declare
         Args : Gtkada.Types.Chars_Ptr_Array (0 .. 3) :=
           (0 => P0, 1 => P1, 2 => P2, 3 => Gtkada.Types.Null_Ptr);
      begin
         Parse_Args (Context, Args, Success, Error);
         Glib.Test.Assert_True (Success);
         Glib.Test.Assert_True (Error = null);
         Glib.Test.Assert_True (Flag_Value /= 0);
         Glib.Test.Assert_Cmpint_Eq (Count, 17);
         Glib.Test.Assert_True (Args (1) = Gtkada.Types.Null_Ptr);
      end;
      Glib.Option.Free (Context);
      Gtkada.Types.Free (P1);
      Gtkada.Types.Free (P2);

      --  A malformed integer follows GOption's bad-value error path.
      Context := Glib.Option.G_New;
      Glib.Option.Add_Main_Entries (Context, Entries);
      P1 := Gtkada.Types.New_String ("--count=not-a-number");
      declare
         Args : Gtkada.Types.Chars_Ptr_Array (0 .. 2) :=
           (0 => P0, 1 => P1, 2 => Gtkada.Types.Null_Ptr);
      begin
         Parse_Args (Context, Args, Success, Error);
         Glib.Test.Assert_False (Success);
         Glib.Test.Assert_True (Error /= null);
         Glib.Test.Assert_True
           (Glib.Option.GOption_Error_Matches
              (Error, Glib.Option.G_Option_Error_Bad_Value));
         Glib.Test.Assert_False
           (Glib.Option.GOption_Error_Matches
              (Error, Glib.Option.G_Option_Error_Failed));
         --  GOption allocates the parse error and leaves it to the caller.
         Glib.Error.Error_Free (Error);
      end;
      Glib.Option.Free (Context);
      Gtkada.Types.Free (P1);

      --  With the default policy, an unknown switch is a distinct error.
      Context := Glib.Option.G_New;
      Glib.Option.Add_Main_Entries (Context, Entries);
      P1 := Gtkada.Types.New_String ("--future-option");
      declare
         Args : Gtkada.Types.Chars_Ptr_Array (0 .. 2) :=
           (0 => P0, 1 => P1, 2 => Gtkada.Types.Null_Ptr);
      begin
         Parse_Args (Context, Args, Success, Error);
         Glib.Test.Assert_False (Success);
         Glib.Test.Assert_True
           (Glib.Option.GOption_Error_Matches
              (Error, Glib.Option.G_Option_Error_Unknown_Option));
         --  Error objects are independent of the context lifetime.
         Glib.Error.Error_Free (Error);
      end;
      Glib.Option.Free (Context);
      Gtkada.Types.Free (P1);

      --  Controlled argv drives the command-line overload without D-Bus.
      declare
         App : constant Glib.Application.Gapplication :=
           Glib.Application.Gapplication_New
             ("org.gtkada.Coverage.OptionParse",
              Glib.Application.G_Application_Non_Unique or
                Glib.Application.G_Application_Handles_Command_Line);
         Args : GNAT.Strings.String_List :=
           (new String'("glib-option"), new String'("--discard"),
            new String'("--verbose"), new String'("--count=23"));
      begin
         --  Separate app arguments avoid depending on the test runner's argv.
         Context := Glib.Option.G_New;
         Glib.Option.Add_Main_Entries (Context, Entries);
         Flag_Value := 0;
         Count := 0;
         App.On_Command_Line (Parse_Command_Line'Unrestricted_Access);
         Glib.Test.Assert_Cmpint_Eq (App.Run (Args'Length, Args), 0);
         for Arg of Args loop
            --  Run copied these strings; the caller still owns this list.
            GNAT.Strings.Free (Arg);
         end loop;
         Glib.Object.Unref (Glib.Object.GObject (App));
         Glib.Option.Free (Context);
      end;

      --  Unknown options are accepted and remain in argv when requested.
      Context := Glib.Option.G_New;
      Glib.Option.Add_Main_Entries (Context, Entries);
      Glib.Option.Set_Ignore_Unknown_Options (Context, True);
      P1 := Gtkada.Types.New_String ("--future-option");
      declare
         Args : Gtkada.Types.Chars_Ptr_Array (0 .. 2) :=
           (0 => P0, 1 => P1, 2 => Gtkada.Types.Null_Ptr);
      begin
         Parse_Args (Context, Args, Success, Error);
         Glib.Test.Assert_True (Success);
         Glib.Test.Assert_True (Error = null);
         Glib.Test.Assert_True
           (Gtkada.Types.Value (Args (1)) = "--future-option");
         Glib.Test.Assert_True (Args (2) = Gtkada.Types.Null_Ptr);
      end;
      Glib.Option.Free (Context);
      Gtkada.Types.Free (P1);
      Gtkada.Types.Free (P0);
      Gtkada.Types.Free (Entries (0).Long_Name);
      Gtkada.Types.Free (Entries (0).Description);
      Gtkada.Types.Free (Entries (1).Long_Name);
      Gtkada.Types.Free (Entries (1).Description);
      Gtkada.Types.Free (Entries (1).Arg_Description);
   end Test_Parse_And_Errors;

   --  Exercise both callback adapters through help text translation.
   procedure Test_Translation is
      Context : Glib.Option.Goption_Context := Glib.Option.G_New ("FILES");
   begin
      Glib.Option.Set_Summary (Context, "Summary text");
      Glib.Option.Set_Description (Context, "Description text");
      Glib.Option.Set_Translate_Func
        (Context, Translate'Unrestricted_Access);
      Glib.Test.Assert_True
        (Contains
           (Glib.Option.Get_Help (Context, True, null),
            "translated:Summary text"));
      Glib.Test.Assert_True
        (Contains
           (Glib.Option.Get_Help (Context, True, null),
            "translated:Description text"));
      Glib.Option.Set_Translate_Func (Context, null);
      --  Restore the untranslated help path before releasing this context.
      Glib.Option.Free (Context);

      --  This adapter wraps a callback and data object in GLib's destroy
      --  notification; freeing its context must release that object once.
      Context := Glib.Option.G_New;
      Glib.Option.Set_Summary (Context, "User data summary");
      Translation_With_Data.Set_Translate_Func
        (Context, Translate_With_Data'Unrestricted_Access, 27);
      Glib.Test.Assert_True
        (Contains
           (Glib.Option.Get_Help (Context, True, null),
            " 27:User data summary"));
      --  Replacing the callback with null also exercises the no-data path;
      --  GLib destroys the prior closure when the callback is replaced.
      Translation_With_Data.Set_Translate_Func (Context, null, 0);
      --  Destroy runs when GLib replaces the closure, exactly once.
      Glib.Test.Assert_Cmpuint_Eq (Glib.Guint (Destroyed_User_Data), 1);
      Glib.Option.Free (Context);
   end Test_Translation;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/glib-option/context-and-help", Test_Context_And_Help'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-option/groups", Test_Groups'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-option/parse-and-errors", Test_Parse_And_Errors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-option/translation", Test_Translation'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
