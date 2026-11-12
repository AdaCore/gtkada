--  GSettings tests for schema lookup, value conversion, constructors, signals,
--  backend notifications, delayed application and bindings. No equivalent
--  GSettings C test exists in testsuite/c_tests; the schema fixture keeps
--  every case local and repeatable.
with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with GNAT.OS_Lib;
with GNAT.Strings;
with Glib; use Glib;
with Glib.Action;
with Glib.Error;
with Glib.Object;
with Glib.Properties;
with Glib.Settings; use Glib.Settings;
with Glib.Settings_Backend; use Glib.Settings_Backend;
with Glib.Settings_Schema; use Glib.Settings_Schema;
with Glib.Settings_Schema_Key; use Glib.Settings_Schema_Key;
with Glib.Settings_Schema_Source; use Glib.Settings_Schema_Source;
with Glib.Test; use Glib.Test;
with Glib.Types;
with Glib.Variant; use Glib.Variant;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Main;
with System;

procedure Settings is
   use type GNAT.OS_Lib.String_Access;
   use type Glib.Error.GError;
   use type Glib.Action.Gaction;
   use type Glib.Variant.Gvariant;
   use type Glib.Object.GObject;
   use type Glib.Variant.Gvariant_Type;
   Source : Gsettings_Schema_Source;
   Schema : Gsettings_Schema;
   Changed : Natural := 0;
   Slot_Changed : Natural := 0;
   Writable_Changed : Natural := 0;

   function Contains
     (Values : GNAT.Strings.String_List; Expected : String) return Boolean;
   --  Schema and key lists have no defined order, so cases check membership.

   function Contains
     (Values : GNAT.Strings.String_List; Expected : String) return Boolean is
   begin
      for Value of Values loop
         if Value.all = Expected then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   procedure On_Changed
     (Self : access Gsettings_Record'Class; Key : UTF8_String)
   is
   begin
      Assert_True (Has_Key (Schema, Key));
      Assert_True (Self.Is_Writable (Key));
      if Key = "enabled" then
         Changed := Changed + 1;
      end if;
   end On_Changed;

   procedure On_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class; Key : UTF8_String)
   is
   begin
      --  Slot connections pass the supplied slot object as the callback's
      --  first parameter, rather than the GSettings instance.
      Assert_True (Self /= null);
      Assert_Cmpstr_Eq (Key, "enabled");
      Slot_Changed := Slot_Changed + 1;
   end On_Changed_Slot;

   procedure On_Writable
     (Self : access Gsettings_Record'Class; Key : UTF8_String)
   is
   begin
      Assert_True (Self.Is_Writable (Key));
      Writable_Changed := Writable_Changed + 1;
   end On_Writable;

   procedure On_Backend_Writable
     (Self : access Gsettings_Record'Class; Key : UTF8_String)
   is
   begin
      --  A path notification includes every key below the path; count the
      --  watched key only while validating writability for each callback.
      Assert_True (Self.Is_Writable (Key));
      if Key = "enabled" then
         Writable_Changed := Writable_Changed + 1;
      end if;
   end On_Backend_Writable;

   procedure Test_Schemas with Convention => C;
   procedure Test_Values with Convention => C;
   procedure Test_All_Values with Convention => C;
   procedure Test_Constructors with Convention => C;
   procedure Test_Children_And_Lists with Convention => C;
   procedure Test_Actions_And_Signals with Convention => C;
   procedure Test_Delay with Convention => C;
   procedure Test_Bindings with Convention => C;
   procedure Test_Relocatable with Convention => C;
   procedure Test_Source_Error with Convention => C;
   procedure Test_Source_Procedure_Constructor with Convention => C;
   procedure Test_Backend_Notifications with Convention => C;

   procedure Test_Schemas is
      Fixed, Movable : GNAT.Strings.String_List_Access;
      Keys : GNAT.Strings.String_List := List_Keys (Schema);
      Key : Gsettings_Schema_Key := Get_Key (Schema, "enabled");
      Value : Gvariant := Get_Default_Value (Key);
      Range_Key : Gsettings_Schema_Key := Get_Key (Schema, "count");
      Range_Value : Gvariant := Get_Range (Range_Key);
      Copy : Gsettings_Schema := Ref (Schema);
      Key_Copy : Gsettings_Schema_Key := Ref (Key);
      Source_Copy : Gsettings_Schema_Source := Ref (Source);
      --  From_Object_Free consumes a GLib-allocated Ada wrapper, while the
      --  schema key itself keeps its independently owned native reference.
      Wrapped_Key : constant access Gsettings_Schema_Key :=
        new Gsettings_Schema_Key'(Get_Key (Schema, "name"));
      Converted_Key : Gsettings_Schema_Key := From_Object_Free (Wrapped_Key);
      --  The schema source lookup also returns an owned reference. Wrapping
      --  it in an allocated Ada record exercises the schema's matching
      --  From_Object_Free conversion and transfers that native reference.
      Wrapped_Schema : constant access Gsettings_Schema :=
        new Gsettings_Schema'(Lookup (Source, "org.gtkada.test", False));
      Converted_Schema : Gsettings_Schema :=
        From_Object_Free (Wrapped_Schema);
   begin
      List_Schemas (Source, False, Fixed, Movable);
      Assert_True (Contains (Fixed.all, "org.gtkada.test"));
      Assert_True (Contains (Movable.all, "org.gtkada.relocatable"));
      GNAT.Strings.Free (Fixed);
      GNAT.Strings.Free (Movable);
      Assert_True (Contains (Keys, "enabled"));
      Assert_True (Contains (Keys, "large-unsigned"));
      for Text of Keys loop
         GNAT.Strings.Free (Text);
      end loop;
      Assert_Cmpstr_Eq (Get_Id (Copy), "org.gtkada.test");
      Assert_Cmpstr_Eq (Get_Id (Converted_Schema), "org.gtkada.test");
      Assert_Cmpstr_Eq (Get_Path (Schema), "/org/gtkada/test/");
      Assert_True (Has_Key (Schema, "count"));
      Assert_False (Has_Key (Schema, "missing"));
      Assert_Cmpstr_Eq (Get_Name (Key_Copy), "enabled");
      Assert_Cmpstr_Eq (Get_Summary (Key), "Enabled");
      Assert_Cmpstr_Eq (Get_Description (Key), "Enable the test feature.");
      --  The key reports the default value's type; the range query uses the
      --  documented tuple shape for this integer key's explicit bounds.
      Assert_True (Get_Value_Type (Key) = Get_Type (Value));
      Assert_Cmpstr_Eq (Get_Type_String (Value), "b");
      Assert_Cmpstr_Eq (Get_Type_String (Range_Value), "(sv)");
      Assert_Cmpstr_Eq (Get_Name (Converted_Key), "name");
      Assert_True (Get_Boolean (Value));
      Assert_True (Range_Check (Key, Value));
      Assert_False (Is_Null (Source_Copy));
      Assert_True (Is_Null (Lookup (Source, "org.gtkada.missing", False)));
      Unref (Converted_Key);
      Unref (Range_Value);
      Unref (Range_Key);
      Unref (Value);
      Unref (Key_Copy);
      Unref (Key);
      Unref (Converted_Schema);
      Unref (Copy);
      Unref (Source_Copy);
   end Test_Schemas;

   procedure Test_Values is
      Backend : Gsettings_Backend := Memory_New;
      Config : Gsettings := Gsettings_New_Full (Schema, Backend);
      Other : Gsettings := Gsettings_New_Full (Schema, Backend);
      Private_Config : Gsettings;
      Private_Backend : Gsettings_Backend := Memory_New;
      Ok : Boolean;
      Value : Gvariant;
      Words : GNAT.Strings.String_List := Config.Get_Strv ("words");
   begin
      --  Constructors retain both schema and backend. Independent memory
      --  backends isolate writes; instances on the same backend share them.
      Assert_True (Glib.Properties.Get_Property (Config, Backend_Property) =
                   Glib.Object.GObject (Backend));
      Glib.Object.Unref (Glib.Object.GObject (Backend));
      G_New_Full (Private_Config, Schema, Private_Backend);
      Glib.Object.Unref (Glib.Object.GObject (Private_Backend));
      Config.On_Changed (On_Changed'Unrestricted_Access);
      Changed := 0;
      Assert_True (Config.Get_Boolean ("enabled"));
      Assert_Cmpstr_Eq (Config.Get_String ("name"), "Ada");
      Assert_Cmpint_Eq (Config.Get_Int ("count"), 3);
      Assert_Cmpint_Eq (Gint (Words'Length), 2);
      for Text of Words loop
         GNAT.Strings.Free (Text);
      end loop;
      Ok := Config.Set_Boolean ("enabled", False);
      Assert_True (Ok);
      Assert_Cmpint_Eq (Gint (Changed), 1);
      Assert_False (Other.Get_Boolean ("enabled"));
      Assert_True (Private_Config.Get_Boolean ("enabled"));
      Ok := Config.Set_String ("name", "GtkAda");
      Assert_True (Ok);
      Ok := Config.Set_Int ("count", 8);
      Assert_True (Ok);
      Assert_Cmpint_Eq (Other.Get_Int ("count"), 8);
      Value := Config.Get_User_Value ("count");
      Assert_Cmpint_Eq (Gint (Get_Int32 (Value)), 8);
      Unref (Value);
      Value := Config.Get_Default_Value ("count");
      Assert_Cmpint_Eq (Gint (Get_Int32 (Value)), 3);
      Unref (Value);
      Config.Reset ("count");
      Assert_Cmpint_Eq (Other.Get_Int ("count"), 3);
      Value := Config.Get_User_Value ("count");
      Assert_True (Is_Null (Value));
      Glib.Object.Unref (Glib.Object.GObject (Config));
      Glib.Object.Unref (Glib.Object.GObject (Other));
      Glib.Object.Unref (Glib.Object.GObject (Private_Config));
   end Test_Values;

   procedure Test_All_Values is
      Backend : Gsettings_Backend := Memory_New;
      Config : Gsettings := Gsettings_New_Full (Schema, Backend);
      Words : GNAT.Strings.String_List :=
        (1 => new String'("one"), 2 => new String'("two"));
      Value : Gvariant;
      Range_Value : Gvariant;
   begin
      --  Exercise the scalar convenience bindings with schema values whose
      --  types match their C GSettings functions exactly.
      Assert_Cmpint_Eq (Gint (Config.Get_Enum ("mode")), 1);
      Assert_True (Config.Set_Enum ("mode", 2));
      Assert_Cmpint_Eq (Gint (Config.Get_Enum ("mode")), 2);
      Assert_Cmpint_Eq (Gint (Config.Get_Flags ("features")), 3);
      Assert_True (Config.Set_Flags ("features", 4));
      Assert_Cmpint_Eq (Gint (Config.Get_Flags ("features")), 4);
      Assert_True (abs (Config.Get_Double ("measure") - 1.25) < 0.0001);
      Assert_True (Config.Set_Double ("measure", -2.5));
      Assert_True (abs (Config.Get_Double ("measure") + 2.5) < 0.0001);
      Assert_True (Config.Get_Int64 ("signed") = -1_234_567_890_123);
      Assert_True (Config.Set_Int64 ("signed", 7_654_321_098));
      Assert_True (Config.Get_Int64 ("signed") = 7_654_321_098);
      Assert_Cmpuint_Eq (Config.Get_Uint ("unsigned"), 17);
      Assert_True (Config.Set_Uint ("unsigned", 29));
      Assert_Cmpuint_Eq (Config.Get_Uint ("unsigned"), 29);
      Assert_True (Config.Get_Uint64 ("large-unsigned") = 1_234_567_890_123);
      Assert_True (Config.Set_Uint64 ("large-unsigned", 9_876_543_210));
      Assert_True (Config.Get_Uint64 ("large-unsigned") = 9_876_543_210);

      --  The Strv wrapper copies both directions; free the Ada-owned strings
      --  returned by GSettings and the temporary strings passed to Set_Strv.
      Assert_True (Config.Set_Strv ("words", Words));
      for Text of Words loop
         GNAT.Strings.Free (Text);
      end loop;
      declare
         Read_Words : GNAT.Strings.String_List := Config.Get_Strv ("words");
      begin
         Assert_Cmpint_Eq (Gint (Read_Words'Length), 2);
         Assert_Cmpstr_Eq (Read_Words (Read_Words'First).all, "one");
         Assert_Cmpstr_Eq (Read_Words (Read_Words'Last).all, "two");
         for Text of Read_Words loop
            GNAT.Strings.Free (Text);
         end loop;
      end;

      --  Generic GVariant access complements the typed convenience functions.
      Value := Config.Get_Value ("name");
      Assert_Cmpstr_Eq (Get_String (Value), "Ada");
      Unref (Value);
      Value := Gvariant_New_String ("written through variant");
      Assert_True (Config.Set_Value ("name", Value));
      Value := Config.Get_User_Value ("name");
      Assert_Cmpstr_Eq (Get_String (Value), "written through variant");
      Unref (Value);
      Value := Config.Get_Default_Value ("name");
      Assert_Cmpstr_Eq (Get_String (Value), "Ada");
      Unref (Value);

      --  Range_Check accepts values inside the schema range and rejects those
      --  outside it; Get_Range is deprecated but still safe to query.
      Range_Value := Gvariant_New_Int32 (7);
      Assert_True (Config.Range_Check ("count", Range_Value));
      Unref (Range_Value);
      Range_Value := Gvariant_New_Int32 (11);
      Assert_False (Config.Range_Check ("count", Range_Value));
      Unref (Range_Value);
      Range_Value := Config.Get_Range ("count");
      Assert_False (Is_Null (Range_Value));
      Unref (Range_Value);
      Assert_True (Config.Is_Writable ("count"));
      Config.Reset ("name");
      Assert_Cmpstr_Eq (Config.Get_String ("name"), "Ada");
      Glib.Settings.Sync;
      Glib.Object.Unref (Glib.Object.GObject (Config));
      Glib.Object.Unref (Glib.Object.GObject (Backend));
   end Test_All_Values;

   procedure Test_Constructors is
      Backend : Gsettings_Backend := Memory_New;
      Direct : Gsettings;
      Returned : Gsettings := Gsettings_New ("org.gtkada.test");
      Full_Default : Gsettings;
      Full_With_Backend : Gsettings;
      Backend_Out : Gsettings;
      Backend_Direct : Gsettings;
      Backend_Returned : Gsettings;
      Backend_Path_Direct : Gsettings;
      Backend_Path_Returned : Gsettings;
      Path_Direct : Gsettings;
      Path_Returned : Gsettings;
      Init_Default : Gsettings := new Gsettings_Record;
      Init_Full : Gsettings := new Gsettings_Record;
      Init_Backend : Gsettings := new Gsettings_Record;
      Init_Backend_Path : Gsettings := new Gsettings_Record;
      Init_Path : Gsettings := new Gsettings_Record;
   begin
      --  Both the out-parameter and function forms should create usable
      --  objects for the ordinary fixed-path schema.
      G_New (Direct, "org.gtkada.test");
      Assert_True (Direct.Get_Boolean ("enabled"));
      Assert_True (Returned.Get_Boolean ("enabled"));

      --  Full constructors accept either the default backend or an explicit
      --  backend. The custom path form is used with a relocatable schema.
      G_New_Full (Full_Default, Schema, null);
      G_New_Full (Full_With_Backend, Schema, Backend);
      Assert_Cmpstr_Eq (Full_Default.Get_String ("name"), "Ada");
      Assert_Cmpstr_Eq (Full_With_Backend.Get_String ("name"), "Ada");
      G_New_With_Backend (Backend_Out, "org.gtkada.test", Backend);
      Backend_Direct := Gsettings_New_With_Backend ("org.gtkada.test", Backend);
      Backend_Returned := Gsettings_New_With_Backend ("org.gtkada.test", Backend);
      Assert_True (Backend_Out.Get_Boolean ("enabled"));
      Assert_True (Backend_Direct.Get_Boolean ("enabled"));
      Assert_True (Backend_Returned.Get_Boolean ("enabled"));

      --  Fixed schemas require their declared path; relocatable schemas take
      --  the caller's path for both constructor styles.
      G_New_With_Backend_And_Path
        (Backend_Path_Direct, "org.gtkada.test", Backend, "/org/gtkada/test/");
      Backend_Path_Returned := Gsettings_New_With_Backend_And_Path
        ("org.gtkada.test", Backend, "/org/gtkada/test/");
      G_New_With_Path
        (Path_Direct, "org.gtkada.relocatable", "/org/gtkada/direct/");
      Path_Returned := Gsettings_New_With_Path
        ("org.gtkada.relocatable", "/org/gtkada/returned/");
      Assert_False (Path_Direct.Get_Boolean ("enabled"));
      Assert_False (Path_Returned.Get_Boolean ("enabled"));
      Assert_True (Backend_Path_Direct.Get_Boolean ("enabled"));
      Assert_True (Backend_Path_Returned.Get_Boolean ("enabled"));

      --  Initializers populate an allocated Ada wrapper and ignore later
      --  initialization attempts once it already owns a GObject.
      Init_Default.Initialize ("org.gtkada.test");
      Init_Default.Initialize ("org.gtkada.test");
      Init_Full.Initialize_Full (Schema, Backend);
      Init_Backend.Initialize_With_Backend ("org.gtkada.test", Backend);
      Init_Backend_Path.Initialize_With_Backend_And_Path
        ("org.gtkada.test", Backend, "/org/gtkada/test/");
      Init_Path.Initialize_With_Path
        ("org.gtkada.relocatable", "/org/gtkada/initialized/");
      Assert_True (Init_Default.Get_Boolean ("enabled"));
      Assert_Cmpstr_Eq (Init_Full.Get_String ("name"), "Ada");
      Assert_True (Init_Backend.Get_Boolean ("enabled"));
      Assert_True (Init_Backend_Path.Get_Boolean ("enabled"));
      Assert_False (Init_Path.Get_Boolean ("enabled"));

      --  Drop every wrapper reference; the constructors retain their own
      --  backend references until these objects are finalized.
      Glib.Object.Unref (Glib.Object.GObject (Direct));
      Glib.Object.Unref (Glib.Object.GObject (Returned));
      Glib.Object.Unref (Glib.Object.GObject (Full_Default));
      Glib.Object.Unref (Glib.Object.GObject (Full_With_Backend));
      Glib.Object.Unref (Glib.Object.GObject (Backend_Out));
      Glib.Object.Unref (Glib.Object.GObject (Backend_Direct));
      Glib.Object.Unref (Glib.Object.GObject (Backend_Returned));
      Glib.Object.Unref (Glib.Object.GObject (Backend_Path_Direct));
      Glib.Object.Unref (Glib.Object.GObject (Backend_Path_Returned));
      Glib.Object.Unref (Glib.Object.GObject (Path_Direct));
      Glib.Object.Unref (Glib.Object.GObject (Path_Returned));
      Glib.Object.Unref (Glib.Object.GObject (Init_Default));
      Glib.Object.Unref (Glib.Object.GObject (Init_Full));
      Glib.Object.Unref (Glib.Object.GObject (Init_Backend));
      Glib.Object.Unref (Glib.Object.GObject (Init_Backend_Path));
      Glib.Object.Unref (Glib.Object.GObject (Init_Path));
      Glib.Object.Unref (Glib.Object.GObject (Backend));
   end Test_Constructors;

   procedure Test_Children_And_Lists is
      Backend : Gsettings_Backend := Memory_New;
      Config : Gsettings := Gsettings_New_Full (Schema, Backend);
      Child : Gsettings;
      Children : GNAT.Strings.String_List := Config.List_Children;
      Keys : GNAT.Strings.String_List := Config.List_Keys;
      Schema_Children : GNAT.Strings.String_List :=
        Glib.Settings_Schema.List_Children (Schema);
      Schemas : GNAT.Strings.String_List := Glib.Settings.List_Schemas;
      Relocatable : GNAT.Strings.String_List :=
        Glib.Settings.List_Relocatable_Schemas;
   begin
      --  Child names and schema lists are unordered introspection results.
      Assert_True (Contains (Children, "child"));
      --  Schema-level child introspection returns the same child identifiers
      --  as GSettings' instance-level method.
      Assert_Cmpint_Eq (Gint (Schema_Children'Length), 1);
      Assert_True (Contains (Schema_Children, "child"));
      Assert_True (Contains (Keys, "mode"));
      Assert_True (Contains (Keys, "large-unsigned"));
      Assert_True (Contains (Schemas, "org.gtkada.test"));
      Assert_True (Contains (Relocatable, "org.gtkada.relocatable"));

      --  Get_Child returns a separately referenced settings object using the
      --  child schema and the parent instance's backend.
      Child := Config.Get_Child ("child");
      Assert_Cmpstr_Eq (Child.Get_String ("note"), "child default");
      Glib.Object.Unref (Glib.Object.GObject (Child));
      for Text of Children loop
         GNAT.Strings.Free (Text);
      end loop;
      for Text of Keys loop
         GNAT.Strings.Free (Text);
      end loop;
      for Text of Schema_Children loop
         GNAT.Strings.Free (Text);
      end loop;
      for Text of Schemas loop
         GNAT.Strings.Free (Text);
      end loop;
      for Text of Relocatable loop
         GNAT.Strings.Free (Text);
      end loop;
      Glib.Object.Unref (Glib.Object.GObject (Config));
      Glib.Object.Unref (Glib.Object.GObject (Backend));
   end Test_Children_And_Lists;

   procedure Test_Actions_And_Signals is
      Backend : Gsettings_Backend := Memory_New;
      Config : Gsettings := Gsettings_New_Full (Schema, Backend);
      Check : Gtk_Check_Button;
      Action : Glib.Action.Gaction;
      State : Gvariant;
   begin
      --  The slot overload receives the slot object; regular signal handlers
      --  receive the settings object. Reads are made after connecting because
      --  GSettings only notifies keys observed by an active handler.
      Gtk_New (Check);
      Check.Ref_Sink;
      Slot_Changed := 0;
      Writable_Changed := 0;
      Config.On_Changed (On_Changed_Slot'Unrestricted_Access, Check);
      Config.On_Writable_Changed (On_Writable'Unrestricted_Access);
      Config.On_Writable_Changed (On_Changed_Slot'Unrestricted_Access, Check);
      Config.Bind_Writable ("enabled", Check, "sensitive", False);
      Assert_True (Config.Get_Boolean ("enabled"));
      Assert_True (Config.Set_Boolean ("enabled", False));
      Assert_Cmpint_Eq (Gint (Slot_Changed), 1);
      Assert_True (Check.Get_Sensitive);

      --  A boolean settings action mirrors the current key and activation
      --  toggles it through the action interface.
      Assert_True (Config.Set_Boolean ("enabled", True));
      Action := Config.Create_Action ("enabled");
      Assert_True (Action /= Glib.Action.Null_Gaction);
      Assert_Cmpstr_Eq (Glib.Action.Get_Name (Action), "enabled");
      Assert_True (Glib.Action.Get_Enabled (Action));
      State := Glib.Action.Get_State (Action);
      Assert_True (Get_Boolean (State));
      Unref (State);
      Glib.Action.Activate (Action, Null_Gvariant);
      Assert_False (Config.Get_Boolean ("enabled"));

      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (Action)));
      Glib.Object.Unref (Glib.Object.GObject (Config));
      Glib.Object.Unref (Glib.Object.GObject (Backend));
      Check.Unref;
   end Test_Actions_And_Signals;

   procedure Test_Delay is
      Backend : Gsettings_Backend := Memory_New;
      Config : Gsettings := Gsettings_New_Full (Schema, Backend);
      Other : Gsettings := Gsettings_New_Full (Schema, Backend);
      Ok : Boolean;
   begin
      Glib.Object.Unref (Glib.Object.GObject (Backend));
      Config.The_Delay;
      Ok := Config.Set_Int ("count", 5);
      Assert_True (Ok);
      Assert_True (Config.Get_Has_Unapplied);
      Assert_Cmpint_Eq (Config.Get_Int ("count"), 5);
      Assert_Cmpint_Eq (Other.Get_Int ("count"), 3);
      Config.Revert;
      Assert_False (Config.Get_Has_Unapplied);
      Assert_Cmpint_Eq (Config.Get_Int ("count"), 3);
      Ok := Config.Set_Int ("count", 9);
      Assert_True (Ok);
      Config.Apply;
      Assert_False (Config.Get_Has_Unapplied);
      Assert_Cmpint_Eq (Other.Get_Int ("count"), 9);
      Glib.Object.Unref (Glib.Object.GObject (Config));
      Glib.Object.Unref (Glib.Object.GObject (Other));
   end Test_Delay;

   procedure Test_Bindings is
      Backend : Gsettings_Backend := Memory_New;
      Config : Gsettings := Gsettings_New_Full (Schema, Backend);
      Check : Gtk_Check_Button;
      Ok : Boolean;
   begin
      Glib.Object.Unref (Glib.Object.GObject (Backend));
      Gtk_New (Check);
      Check.Ref_Sink;
      Config.Bind ("enabled", Check, "active", Glib.Settings.Default);
      Assert_True (Check.Get_Active);
      Check.Set_Active (False);
      Assert_False (Config.Get_Boolean ("enabled"));
      Ok := Config.Set_Boolean ("enabled", True);
      Assert_True (Ok);
      Assert_True (Check.Get_Active);
      Glib.Settings.Unbind (Check, "active");
      Check.Set_Active (False);
      Assert_True (Config.Get_Boolean ("enabled"));
      Glib.Object.Unref (Glib.Object.GObject (Config));
      Check.Unref;
   end Test_Bindings;

   procedure Test_Relocatable is
      Reloc : Gsettings_Schema := Lookup (Source, "org.gtkada.relocatable", False);
      Backend : Gsettings_Backend := Memory_New;
      Config : Gsettings := Gsettings_New_Full (Reloc, Backend, "/org/gtkada/instance/");
   begin
      Assert_Cmpstr_Eq (Get_Path (Reloc), "");
      Unref (Reloc);
      Glib.Object.Unref (Glib.Object.GObject (Backend));
      Assert_False (Config.Get_Boolean ("enabled"));
      Glib.Object.Unref (Glib.Object.GObject (Config));
   end Test_Relocatable;

   procedure Test_Source_Error is
      Error : Glib.Error.GError;
      Missing : Gsettings_Schema_Source := Gsettings_Schema_Source_New_From_Directory
        ("no-such-schema-directory", Get_Default, False, Error);
   begin
      Assert_True (Is_Null (Missing));
      Assert_True (Error /= null);
      Glib.Error.Error_Free (Error);
   end Test_Source_Error;

   procedure Test_Source_Procedure_Constructor is
      Procedure_Source : Gsettings_Schema_Source;
      Found : Gsettings_Schema;
      Error : Glib.Error.GError;
   begin
      --  Check that the out-parameter constructor opens the compiled fixture.
      --  A null parent keeps this lookup scoped to the fixture directory.
      G_New_From_Directory
        (Procedure_Source, ".", From_Object (System.Null_Address), False, Error);
      Assert_True (Error = null);
      Assert_False (Is_Null (Procedure_Source));
      Found := Lookup (Procedure_Source, "org.gtkada.test", False);
      Assert_False (Is_Null (Found));
      --  Both the lookup result and the newly created source own references.
      Unref (Found);
      Unref (Procedure_Source);
   end Test_Source_Procedure_Constructor;

   procedure Test_Backend_Notifications is
      Backend : Gsettings_Backend := Memory_New;
      Default_Backend : Gsettings_Backend :=
        Glib.Settings_Backend.Get_Default;
      Config : Gsettings := Gsettings_New_Full (Schema, Backend);
      Items : GNAT.Strings.String_List :=
        (1 => new String'("enabled"));
   begin
      --  Get_Default returns an owned backend reference, while Config keeps
      --  its own reference to the private memory backend.
      Assert_True (Default_Backend /= null);
      Config.On_Changed (On_Changed'Unrestricted_Access);
      Config.On_Writable_Changed
        (On_Backend_Writable'Unrestricted_Access);
      Assert_True (Config.Get_Boolean ("enabled"));
      Changed := 0;
      Writable_Changed := 0;

      --  The backend methods take absolute keys and paths. Each notification
      --  reaches GSettings synchronously for this already-read key.
      Glib.Settings_Backend.Changed
        (Backend, "/org/gtkada/test/enabled", System.Null_Address);
      Assert_Cmpint_Eq (Gint (Changed), 1);
      Glib.Settings_Backend.Keys_Changed
        (Backend, "/org/gtkada/test/", Items, System.Null_Address);
      Assert_Cmpint_Eq (Gint (Changed), 2);
      Glib.Settings_Backend.Path_Changed
        (Backend, "/org/gtkada/test/", System.Null_Address);
      Assert_Cmpint_Eq (Gint (Changed), 3);

      --  Writability notifications tell callers to recheck Is_Writable.
      Glib.Settings_Backend.Writable_Changed
        (Backend, "/org/gtkada/test/enabled");
      Assert_Cmpint_Eq (Gint (Writable_Changed), 1);
      Glib.Settings_Backend.Path_Writable_Changed
        (Backend, "/org/gtkada/test/");
      Assert_Cmpint_Eq (Gint (Writable_Changed), 2);

      for Text of Items loop
         GNAT.Strings.Free (Text);
      end loop;
      Glib.Object.Unref (Glib.Object.GObject (Config));
      Glib.Object.Unref (Glib.Object.GObject (Backend));
      Glib.Object.Unref (Glib.Object.GObject (Default_Backend));
   end Test_Backend_Notifications;

   Compiler : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Locate_Exec_On_Path ("glib-compile-schemas");
   Args : GNAT.OS_Lib.Argument_List := (new String'("--strict"), new String'("."));
   Status : Integer;
   Error : Glib.Error.GError;
begin
   Glib.Test.Init;
   Assert_True (Compiler /= null);
   Status := GNAT.OS_Lib.Spawn (Compiler.all, Args);
   GNAT.OS_Lib.Free (Compiler);
   for Arg of Args loop
      GNAT.OS_Lib.Free (Arg);
   end loop;
   Assert_Cmpint_Eq (Gint (Status), 0);
   --  Make this test's compiled schema visible to constructors that consult
   --  the default schema source, before GTK or GSettings initializes it.
   Ada.Environment_Variables.Set
     ("GSETTINGS_SCHEMA_DIR", Ada.Directories.Current_Directory);
   Gtk.Main.Init;
   Source := Gsettings_Schema_Source_New_From_Directory
     (".", From_Object (System.Null_Address), False, Error);
   Assert_True (Error = null);
   Schema := Lookup (Source, "org.gtkada.test", False);
   Assert_False (Is_Null (Schema));
   Add_Func ("/settings/schemas", Test_Schemas'Unrestricted_Access);
   Add_Func ("/settings/values", Test_Values'Unrestricted_Access);
   Add_Func ("/settings/all-values", Test_All_Values'Unrestricted_Access);
   Add_Func ("/settings/constructors", Test_Constructors'Unrestricted_Access);
   Add_Func ("/settings/children-lists", Test_Children_And_Lists'Unrestricted_Access);
   Add_Func ("/settings/actions-signals", Test_Actions_And_Signals'Unrestricted_Access);
   Add_Func ("/settings/delay", Test_Delay'Unrestricted_Access);
   Add_Func ("/settings/bindings", Test_Bindings'Unrestricted_Access);
   Add_Func ("/settings/relocatable", Test_Relocatable'Unrestricted_Access);
   Add_Func ("/settings/source-error", Test_Source_Error'Unrestricted_Access);
   Add_Func ("/settings/source-procedure-constructor",
     Test_Source_Procedure_Constructor'Unrestricted_Access);
   Add_Func ("/settings/backend-notifications", Test_Backend_Notifications'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Run);
   Unref (Schema);
   Unref (Source);
end Settings;
