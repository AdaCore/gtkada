with Ada.Command_Line;
with GNAT.OS_Lib;
with GNAT.Strings;
with Glib; use Glib;
with Glib.Error;
with Glib.Object;
with Glib.Properties;
with Glib.Settings; use Glib.Settings;
with Glib.Settings_Backend; use Glib.Settings_Backend;
with Glib.Settings_Schema; use Glib.Settings_Schema;
with Glib.Settings_Schema_Key; use Glib.Settings_Schema_Key;
with Glib.Settings_Schema_Source; use Glib.Settings_Schema_Source;
with Glib.Test; use Glib.Test;
with Glib.Variant; use Glib.Variant;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Main;
with System;

procedure Settings is
   use type GNAT.OS_Lib.String_Access;
   use type Glib.Error.GError;
   use type Glib.Object.GObject;
   Source : Gsettings_Schema_Source;
   Schema : Gsettings_Schema;
   Changed : Natural := 0;

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

   procedure Test_Schemas with Convention => C;
   procedure Test_Values with Convention => C;
   procedure Test_Delay with Convention => C;
   procedure Test_Bindings with Convention => C;
   procedure Test_Relocatable with Convention => C;
   procedure Test_Source_Error with Convention => C;

   procedure Test_Schemas is
      Fixed, Movable : GNAT.Strings.String_List_Access;
      Keys : GNAT.Strings.String_List := List_Keys (Schema);
      Key : Gsettings_Schema_Key := Get_Key (Schema, "enabled");
      Value : Gvariant := Get_Default_Value (Key);
      Copy : Gsettings_Schema := Ref (Schema);
      Key_Copy : Gsettings_Schema_Key := Ref (Key);
      Source_Copy : Gsettings_Schema_Source := Ref (Source);
   begin
      List_Schemas (Source, False, Fixed, Movable);
      Assert_Cmpint_Eq (Gint (Fixed'Length), 1);
      Assert_Cmpstr_Eq (Fixed (Fixed'First).all, "org.gtkada.test");
      Assert_Cmpint_Eq (Gint (Movable'Length), 1);
      Assert_Cmpstr_Eq (Movable (Movable'First).all, "org.gtkada.relocatable");
      GNAT.Strings.Free (Fixed);
      GNAT.Strings.Free (Movable);
      Assert_Cmpint_Eq (Gint (Keys'Length), 4);
      for Text of Keys loop
         GNAT.Strings.Free (Text);
      end loop;
      Assert_Cmpstr_Eq (Get_Id (Copy), "org.gtkada.test");
      Assert_Cmpstr_Eq (Get_Path (Schema), "/org/gtkada/test/");
      Assert_True (Has_Key (Schema, "count"));
      Assert_False (Has_Key (Schema, "missing"));
      Assert_Cmpstr_Eq (Get_Name (Key_Copy), "enabled");
      Assert_Cmpstr_Eq (Get_Summary (Key), "Enabled");
      Assert_Cmpstr_Eq (Get_Description (Key), "Enable the test feature.");
      Assert_True (Get_Boolean (Value));
      Assert_True (Range_Check (Key, Value));
      Assert_False (Is_Null (Source_Copy));
      Assert_True (Is_Null (Lookup (Source, "org.gtkada.missing", False)));
      Unref (Value);
      Unref (Key_Copy);
      Unref (Key);
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

   Compiler : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Locate_Exec_On_Path ("glib-compile-schemas");
   Args : GNAT.OS_Lib.Argument_List := (new String'("--strict"), new String'("."));
   Status : Integer;
   Error : Glib.Error.GError;
begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Assert_True (Compiler /= null);
   Status := GNAT.OS_Lib.Spawn (Compiler.all, Args);
   GNAT.OS_Lib.Free (Compiler);
   for Arg of Args loop
      GNAT.OS_Lib.Free (Arg);
   end loop;
   Assert_Cmpint_Eq (Gint (Status), 0);
   Source := Gsettings_Schema_Source_New_From_Directory
     (".", From_Object (System.Null_Address), False, Error);
   Assert_True (Error = null);
   Schema := Lookup (Source, "org.gtkada.test", False);
   Assert_False (Is_Null (Schema));
   Add_Func ("/settings/schemas", Test_Schemas'Unrestricted_Access);
   Add_Func ("/settings/values", Test_Values'Unrestricted_Access);
   Add_Func ("/settings/delay", Test_Delay'Unrestricted_Access);
   Add_Func ("/settings/bindings", Test_Bindings'Unrestricted_Access);
   Add_Func ("/settings/relocatable", Test_Relocatable'Unrestricted_Access);
   Add_Func ("/settings/source-error", Test_Source_Error'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Run);
   Unref (Schema);
   Unref (Source);
end Settings;
