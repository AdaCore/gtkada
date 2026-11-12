--  Exercise the public GApplication and GApplicationCommandLine bindings.
--  GLib's C tests do not include a focused GApplication test in this checkout;
--  these cases check the lifecycle, action interfaces and command-line data.

with Ada.Command_Line;
with GNAT.Strings;
with System;

with Glib;                 use Glib;
with Glib.Action;
with Glib.Action_Group;
with Glib.Action_Map;
with Glib.Application;     use Glib.Application;
with Glib.GFile;
with Glib.Input_Stream;
with Glib.Object;
with Glib.Option;
with Glib.Simple_Action;
with Glib.Test;            use Glib.Test;
with Glib.Types;
with Glib.Variant;
with Gtkada.Types;

procedure Main is
   use type Glib.Application.Gapplication;
   use type Glib.GFile.Gfile;
   use type Glib.Action.Gaction;
   use type Glib.Input_Stream.Ginput_Stream;
   use type Glib.Variant.Gvariant;
   use type Glib.Variant.Gvariant_Type;
   use type System.Address;

   Startup_Calls       : Natural := 0;
   Activate_Calls     : Natural := 0;
   Shutdown_Calls     : Natural := 0;
   Command_Line_Calls : Natural := 0;
   Slot_Calls         : Natural := 0;
   Process_Slot_Calls : Natural := 0;
   Action_Added_Calls  : Natural := 0;
   Action_Enable_Calls : Natural := 0;
   Action_Remove_Calls : Natural := 0;
   Action_State_Calls  : Natural := 0;
   Name_Lost_Calls      : Natural := 0;
   Name_Lost_Slot_Calls : Natural := 0;

   --  Emit the GObject signal directly to exercise both handler marshallers
   --  without needing a second D-Bus application to take over the name.
   procedure Emit_Name_Lost
     (Instance     : System.Address;
      Signal_Name  : Gtkada.Types.Chars_Ptr;
      Return_Value : access Glib.Gboolean);
   pragma Import (C, Emit_Name_Lost, "g_signal_emit_by_name");

   function Name_Lost_Callback
     (Self : access Gapplication_Record'Class) return Boolean;

   function Name_Lost_Callback
     (Self : access Gapplication_Record'Class) return Boolean is
   begin
      Assert_True (Self /= null);
      Name_Lost_Calls := Name_Lost_Calls + 1;
      --  False keeps the boolean signal accumulator running the slot handler.
      return False;
   end Name_Lost_Callback;

   function Name_Lost_Slot_Callback
     (Self : access Glib.Object.GObject_Record'Class) return Boolean;

   function Name_Lost_Slot_Callback
     (Self : access Glib.Object.GObject_Record'Class) return Boolean is
   begin
      Assert_True (Self /= null);
      Name_Lost_Slot_Calls := Name_Lost_Slot_Calls + 1;
      return True;
   end Name_Lost_Slot_Callback;

   --  The GActionGroup emitter methods are checked through their documented
   --  signals, so each body must report the action name and payload supplied.
   procedure Group_Action_Added
     (Self        : Glib.Action_Group.Gaction_Group;
      Action_Name : UTF8_String);

   procedure Group_Action_Added
     (Self        : Glib.Action_Group.Gaction_Group;
      Action_Name : UTF8_String) is
   begin
      Assert_Cmpstr_Eq (Action_Name, "signal-probe");
      Action_Added_Calls := Action_Added_Calls + 1;
   end Group_Action_Added;

   procedure Group_Action_Enabled
     (Self        : Glib.Action_Group.Gaction_Group;
      Action_Name : UTF8_String;
      Enabled     : Boolean);

   procedure Group_Action_Enabled
     (Self        : Glib.Action_Group.Gaction_Group;
      Action_Name : UTF8_String;
      Enabled     : Boolean) is
   begin
      Assert_Cmpstr_Eq (Action_Name, "signal-probe");
      Assert_True (Enabled);
      Action_Enable_Calls := Action_Enable_Calls + 1;
   end Group_Action_Enabled;

   procedure Group_Action_Removed
     (Self        : Glib.Action_Group.Gaction_Group;
      Action_Name : UTF8_String);

   procedure Group_Action_Removed
     (Self        : Glib.Action_Group.Gaction_Group;
      Action_Name : UTF8_String) is
   begin
      Assert_Cmpstr_Eq (Action_Name, "signal-probe");
      Action_Remove_Calls := Action_Remove_Calls + 1;
   end Group_Action_Removed;

   procedure Group_Action_State
     (Self        : Glib.Action_Group.Gaction_Group;
      Action_Name : UTF8_String;
      State       : Glib.Variant.Gvariant);

   procedure Group_Action_State
     (Self        : Glib.Action_Group.Gaction_Group;
      Action_Name : UTF8_String;
      State       : Glib.Variant.Gvariant) is
   begin
      Assert_Cmpstr_Eq (Action_Name, "signal-probe");
      Assert_Cmpstr_Eq (Glib.Variant.Get_String (State), "busy");
      Action_State_Calls := Action_State_Calls + 1;
   end Group_Action_State;

   --  Registration emits startup; activation and Run exercise the other
   --  lifecycle signals without creating windows or requiring a session bus.
   procedure Startup_Callback
     (Self : access Gapplication_Record'Class)
   ;

   procedure Startup_Callback
     (Self : access Gapplication_Record'Class) is
   begin
      Assert_True (Self /= null);
      Startup_Calls := Startup_Calls + 1;
   end Startup_Callback;

   procedure Activate_Callback
     (Self : access Gapplication_Record'Class)
   ;

   procedure Activate_Callback
     (Self : access Gapplication_Record'Class) is
   begin
      Assert_True (Self /= null);
      Activate_Calls := Activate_Calls + 1;
      --  Quit makes the no-argument Run case return immediately.
      Self.Quit;
   end Activate_Callback;

   procedure Shutdown_Callback
     (Self : access Gapplication_Record'Class)
   ;

   procedure Shutdown_Callback
     (Self : access Gapplication_Record'Class) is
   begin
      Assert_True (Self /= null);
      Shutdown_Calls := Shutdown_Calls + 1;
   end Shutdown_Callback;

   procedure Slot_Void_Callback
     (Self : access Glib.Object.GObject_Record'Class)
   ;

   procedure Slot_Void_Callback
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Slot_Calls := Slot_Calls + 1;
   end Slot_Void_Callback;

   function Command_Line_Callback
     (Self         : access Gapplication_Record'Class;
      Command_Line : not null access Gapplication_Command_Line_Record'Class)
      return Gint;

   function Command_Line_Callback
     (Self         : access Gapplication_Record'Class;
      Command_Line : not null access Gapplication_Command_Line_Record'Class)
      return Gint
   is
      Args        : GNAT.Strings.String_List := Command_Line.Get_Arguments;
      Environment : GNAT.Strings.String_List := Command_Line.Get_Environ;
      File        : Glib.GFile.Gfile;
      Input       : Glib.Input_Stream.Ginput_Stream;
      Platform    : Glib.Variant.Gvariant;
      Path        : constant UTF8_String := Command_Line.Getenv ("PATH");
      Found_Path  : Boolean := False;
   begin
      --  The local invocation preserves argv[0] and the positional filename.
      Assert_True (Self /= null);
      Assert_True (Args'Length = 2);
      Assert_Cmpstr_Eq (Args (Args'First).all, "glib-application-test");
      Assert_Cmpstr_Eq (Args (Args'Last).all, "sample.txt");

      --  Command-line metadata comes from this process, so cwd and PATH exist.
      Assert_True (Command_Line.Get_Cwd'Length > 0);
      Assert_True (Path'Length > 0);
      for Value of Environment loop
         if Value.all'Length >= 5 and then Value.all (1 .. 5) = "PATH=" then
            Found_Path := True;
         end if;
      end loop;
      Assert_True (Found_Path);

      --  Relative command-line names resolve against the invoking process cwd.
      File := Command_Line.Create_File_For_Arg ("sample.txt");
      Assert_True (File /= Glib.GFile.Null_Gfile);
      Assert_Cmpstr_Eq (Glib.GFile.Get_Basename (File), "sample.txt");
      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));

      --  This process is the primary instance; local invocations have platform
      --  metadata and may or may not expose stdin depending on the platform.
      Assert_False (Command_Line.Get_Is_Remote);
      Platform := Command_Line.Get_Platform_Data;
      --  GLib documents platform data as null for a local invocation.
      Assert_True (Platform = Glib.Variant.Null_Gvariant);
      Input := Command_Line.Get_Stdin;
      Assert_True (Input = null or else Input.Get_Object /= System.Null_Address);
      if Input /= null then
         Glib.Object.Unref (Glib.Object.GObject (Input));
      end if;

      --  Check the exit-status accessors and exercise both literal output
      --  helpers with empty strings so the unattended test remains quiet.
      Assert_Cmpint_Eq (Command_Line.Get_Exit_Status, 0);
      Command_Line.Set_Exit_Status (23);
      Assert_Cmpint_Eq (Command_Line.Get_Exit_Status, 23);
      Command_Line.Print_Literal ("");
      Command_Line.Printerr_Literal ("");
      Assert_Cmpint_Eq (Command_Line.Get_Exit_Status, 23);

      for Arg of Args loop
         GNAT.Strings.Free (Arg);
      end loop;
      for Value of Environment loop
         GNAT.Strings.Free (Value);
      end loop;
      Command_Line_Calls := Command_Line_Calls + 1;
      Command_Line.Done;
      return Command_Line.Get_Exit_Status;
   end Command_Line_Callback;

   --  This slot form is used by the process-argv test to exercise the
   --  GObject-plus-command-line marshaller separately from the plain handler.
   function Process_Args_Slot_Callback
     (Self         : access Glib.Object.GObject_Record'Class;
      Command_Line : not null access Gapplication_Command_Line_Record'Class)
      return Gint;

   function Process_Args_Slot_Callback
     (Self         : access Glib.Object.GObject_Record'Class;
      Command_Line : not null access Gapplication_Command_Line_Record'Class)
      return Gint
   is
      Args : GNAT.Strings.String_List := Command_Line.Get_Arguments;
   begin
      --  The testsuite runner passes its own switches to the executable; a
      --  command-line handler keeps this Run overload from parsing them.
      Assert_True (Self /= null);
      Assert_True (Args'Length > 0);
      for Arg of Args loop
         GNAT.Strings.Free (Arg);
      end loop;
      Process_Slot_Calls := Process_Slot_Calls + 1;
      return 0;
   end Process_Args_Slot_Callback;

   --  Check construction, mutable properties, registration, default selection,
   --  signal delivery, and balanced application holds.
   procedure Test_Lifecycle_And_Properties
   with Convention => C;

   procedure Test_Lifecycle_And_Properties is
      App       : constant Gapplication := Gapplication_New
        ("org.gtkada.Coverage.Application",
         G_Application_Non_Unique);
      Empty_App : Gapplication;
   begin
      --  The constructor stores IDs and flags; an unset ID is valid when the
      --  non-unique flag is used, and G_New covers the out-parameter form.
      Assert_True (Id_Is_Valid ("org.gtkada.Coverage.Application"));
      Assert_False (Id_Is_Valid ("bad-id"));
      Assert_Cmpstr_Eq (App.Get_Application_Id, "org.gtkada.Coverage.Application");
      Assert_True (App.Get_Resource_Base_Path = "/org/gtkada/Coverage/Application");
      Assert_True
        (App.Get_Flags = G_Application_Non_Unique);
      Assert_Cmpuint_Eq (App.Get_Inactivity_Timeout, 0);
      Assert_False (App.Get_Is_Registered);
      Assert_False (App.Get_Is_Busy);

      G_New (Empty_App, "", G_Application_Non_Unique);
      Assert_Cmpstr_Eq (Empty_App.Get_Application_Id, "");
      Empty_App.Set_Application_Id ("org.gtkada.Coverage.Empty");
      Assert_Cmpstr_Eq (Empty_App.Get_Application_Id, "org.gtkada.Coverage.Empty");
      Empty_App.Set_Application_Id ("");
      Assert_Cmpstr_Eq (Empty_App.Get_Application_Id, "");

      --  Setters are read back before registration, as required by GApplication.
      --  IDs are restored before register so the test never claims a bus name.
      App.Set_Application_Id ("");
      Assert_Cmpstr_Eq (App.Get_Application_Id, "");
      App.Set_Application_Id ("org.gtkada.Coverage.Application");
      App.Set_Flags (G_Application_Non_Unique or G_Application_Handles_Command_Line);
      Assert_True
        (App.Get_Flags =
           (G_Application_Non_Unique or G_Application_Handles_Command_Line));
      App.Set_Inactivity_Timeout (250);
      Assert_Cmpuint_Eq (App.Get_Inactivity_Timeout, 250);
      App.Set_Resource_Base_Path ("/org/gtkada/coverage");
      Assert_Cmpstr_Eq (App.Get_Resource_Base_Path, "/org/gtkada/coverage");
      App.Set_Resource_Base_Path ("");
      Assert_Cmpstr_Eq (App.Get_Resource_Base_Path, "");
      App.Set_Version ("1.2.3");
      Assert_Cmpstr_Eq (App.Get_Version, "1.2.3");

      --  A false busy-property value binds without marking the application
      --  busy. Busy transitions after registration require a session bus.
      declare
         Busy_Target : constant Glib.Simple_Action.Gsimple_Action :=
           Glib.Simple_Action.Gsimple_Action_New
             ("busy-target", Glib.Variant.Gvariant_Type_String);
      begin
         Busy_Target.Set_Enabled (False);
         App.Bind_Busy_Property (Busy_Target.Get_Object, "enabled");
         Assert_False (Busy_Target.Get_Enabled);
         App.Unbind_Busy_Property (Busy_Target.Get_Object, "enabled");
         Glib.Object.Unref (Glib.Object.GObject (Busy_Target));
      end;

      --  These strings are parsed by GApplication's option context at Run.
      --  The two calls cover present and absent argument-description strings.
      App.Add_Main_Option
        ("mode", 'm', Glib.Option.G_Option_Flag_None,
         Glib.Option.G_Option_Arg_String, "Choose a mode", "MODE");
      App.Add_Main_Option
        ("verbose", 'v', Glib.Option.G_Option_Flag_None,
         Glib.Option.G_Option_Arg_None, "Enable verbose output");
      App.Set_Option_Context_Description ("Application test options");
      App.Set_Option_Context_Description ("");
      App.Set_Option_Context_Parameter_String ("FILES");
      App.Set_Option_Context_Parameter_String ("");
      App.Set_Option_Context_Summary ("GApplication binding test");
      App.Set_Option_Context_Summary ("");
      --  Registering a non-unique application avoids any external D-Bus service.
      --  Startup runs during registration; activate is driven explicitly below.
      App.On_Startup (Startup_Callback'Unrestricted_Access);
      App.On_Startup (Slot_Void_Callback'Unrestricted_Access, App);
      App.On_Activate (Activate_Callback'Unrestricted_Access);
      App.On_Activate (Slot_Void_Callback'Unrestricted_Access, App);
      App.On_Name_Lost (Name_Lost_Callback'Unrestricted_Access);
      App.On_Name_Lost (Name_Lost_Slot_Callback'Unrestricted_Access, App);
      Assert_True (App.Register (null));
      Assert_True (App.Get_Is_Registered);
      Assert_False (App.Get_Is_Remote);
      Assert_True (App.Get_Dbus_Object_Path'Length = 0
                   or else App.Get_Dbus_Object_Path (1) = '/');
      Assert_True (Startup_Calls = 1);
      Assert_True (Slot_Calls = 1);

      declare
         Signal_Name : Gtkada.Types.Chars_Ptr :=
           Gtkada.Types.New_String ("name-lost");
         Result : aliased Glib.Gboolean := 0;
      begin
         Emit_Name_Lost (App.Get_Object, Signal_Name, Result'Access);
         Assert_True (Result /= 0);
         Assert_True (Name_Lost_Calls = 1);
         Assert_True (Name_Lost_Slot_Calls = 1);
         Gtkada.Types.Free (Signal_Name);
      end;

      --  The process default is a non-owning reference and can be cleared.
      --  Clear it before releasing App so no process-global pointer is stale.
      App.Set_Default;
      Assert_True (Get_Default.Get_Object = App.Get_Object);
      Set_Default (null);
      Assert_True (Get_Default = null);

      --  Activation callbacks and use-count operations do not need a main loop.
      --  The callback quits the application, matching normal signal handling.
      App.Activate;
      Assert_True (Activate_Calls = 1);
      Assert_True (Slot_Calls = 2);
      App.Hold;
      App.Release;

      --  Quit is safe before Run and is used for lifecycle cleanup here.
      App.Quit;
      Glib.Object.Unref (Glib.Object.GObject (App));
      Glib.Object.Unref (Glib.Object.GObject (Empty_App));
   end Test_Lifecycle_And_Properties;

   --  Test the application as both GActionMap and GActionGroup, including
   --  action metadata queries, state changes, batch entries and signal emitters.
   procedure Test_Action_Interfaces
   with Convention => C;

   procedure Test_Action_Interfaces is
      App   : constant Gapplication := Gapplication_New
        ("org.gtkada.Coverage.Actions", G_Application_Non_Unique);
      State : constant Glib.Variant.Gvariant :=
        Glib.Variant.Gvariant_New_String ("ready");
      Hint  : constant Glib.Variant.Gvariant :=
        Glib.Variant.Gvariant_New_String ("ready or busy");
      Changed : constant Glib.Variant.Gvariant :=
        Glib.Variant.Ref_Sink
          (Glib.Variant.Gvariant_New_String ("busy"));
      Action : constant Glib.Simple_Action.Gsimple_Action :=
        Glib.Simple_Action.Gsimple_Action_New_Stateful
          ("mode", Glib.Variant.Gvariant_Type_String, State);
      Entries : constant Glib.Action_Map.GAction_Entry_Array :=
        (0 => Glib.Action_Map.Build ("entry-one"),
         1 => Glib.Action_Map.Build ("entry-two"));
      Enabled       : Boolean;
      Parameter_Ty  : aliased Glib.Variant.Gvariant_Type;
      State_Ty      : aliased Glib.Variant.Gvariant_Type;
      Queried_Hint  : aliased Glib.Variant.Gvariant;
      Queried_State : aliased Glib.Variant.Gvariant;
      Value         : Glib.Variant.Gvariant;
   begin
      --  The application implements both GActionMap and GActionGroup.
      --  Register first because GApplication only exposes action details then.
      Assert_True (App.Register (null));
      Assert_True (App.Get_Is_Registered);
      App.Add_Action (Glib.Simple_Action."+" (Action));
      Assert_True (App.Has_Action ("mode"));
      Action.Set_State_Hint (Hint);
      Assert_True
        (App.Lookup_Action ("mode") = Glib.Simple_Action."+" (Action));

      --  Entry helpers exercise batch add/remove in addition to the single
      --  action methods above. The helper entries contain no signal callbacks.
      --  Their names are checked before and after removal to verify both calls.
      App.Add_Action_Entries (Entries);
      Assert_True (App.Has_Action ("entry-one"));
      Assert_True (App.Has_Action ("entry-two"));
      declare
         Actions : GNAT.Strings.String_List := App.List_Actions;
      begin
         Assert_True (Actions'Length >= 3);
         for Name of Actions loop
            GNAT.Strings.Free (Name);
         end loop;
      end;
      App.Remove_Action_Entries (Entries, Entries'Length);
      Assert_False (App.Has_Action ("entry-one"));
      Assert_False (App.Has_Action ("entry-two"));

      --  Query_Action exercises every optional output and its owned variants.
      Assert_True (App.Get_Action_Enabled ("mode"));
      Assert_True
        (Glib.Variant.Peek_String
           (App.Get_Action_Parameter_Type ("mode")) = "s");
      Assert_True
        (Glib.Variant.Peek_String (App.Get_Action_State_Type ("mode")) = "s");
      Value := App.Get_Action_State ("mode");
      Assert_True (Value.Get_String = "ready");
      Glib.Variant.Unref (Value);
      Value := App.Get_Action_State_Hint ("mode");
      Assert_True (Value.Get_String = "ready or busy");
      Glib.Variant.Unref (Value);

      Assert_True
        (App.Query_Action
           ("mode", Enabled, Parameter_Ty'Access, State_Ty'Access,
            Queried_Hint'Access, Queried_State'Access));
      Assert_True (Enabled);
      Assert_True (Glib.Variant.Peek_String (Parameter_Ty) = "s");
      Assert_True (Glib.Variant.Peek_String (State_Ty) = "s");
      Assert_True (Queried_Hint.Get_String = "ready or busy");
      Assert_True (Queried_State.Get_String = "ready");
      Glib.Variant.Unref (Queried_Hint);
      Glib.Variant.Unref (Queried_State);

      --  Exercise changes, activation and removal through the action group.
      App.Change_Action_State ("mode", Changed);
      Value := App.Get_Action_State ("mode");
      Assert_Cmpstr_Eq (Value.Get_String, "busy");
      Glib.Variant.Unref (Value);
      App.Activate_Action ("mode", Changed);
      App.Remove_Action ("mode");
      Assert_False (App.Has_Action ("mode"));

      --  These four public interface helpers emit the documented
      --  GActionGroup signals and preserve their values for listeners.
      declare
         Probe : constant Glib.Simple_Action.Gsimple_Action :=
           Glib.Simple_Action.Gsimple_Action_New_Stateful
             ("signal-probe", Glib.Variant.Gvariant_Type_String,
              Glib.Variant.Gvariant_New_String ("ready"));
         Group : constant Glib.Action_Group.Gaction_Group :=
           Glib.Application."+" (App);
      begin
         App.Add_Action (Glib.Simple_Action."+" (Probe));
         Glib.Action_Group.On_Action_Added
           (Group, Group_Action_Added'Unrestricted_Access);
         Glib.Action_Group.On_Action_Enabled_Changed
           (Group, Group_Action_Enabled'Unrestricted_Access);
         Glib.Action_Group.On_Action_Removed
           (Group, Group_Action_Removed'Unrestricted_Access);
         Glib.Action_Group.On_Action_State_Changed
           (Group, Group_Action_State'Unrestricted_Access);
         App.Action_Added ("signal-probe");
         App.Action_Enabled_Changed ("signal-probe", True);
         App.Action_State_Changed ("signal-probe", Changed);
         App.Action_Removed ("signal-probe");
         Assert_True (Action_Added_Calls = 1);
         Assert_True (Action_Enable_Calls = 1);
         Assert_True (Action_State_Calls = 1);
         Assert_True (Action_Remove_Calls = 1);
         Glib.Object.Unref (Glib.Object.GObject (Probe));
      end;

      Glib.Variant.Unref (Changed);
      Glib.Object.Unref (Glib.Object.GObject (Action));
      Glib.Object.Unref (Glib.Object.GObject (App));
   end Test_Action_Interfaces;

   --  Run with controlled argv and inspect command-line arguments, cwd,
   --  environment, file resolution, stdin availability and exit status.
   procedure Test_Command_Line_And_Run
   with Convention => C;

   procedure Test_Command_Line_And_Run is
      App : constant Gapplication := Gapplication_New
        ("org.gtkada.Coverage.CommandLine",
         G_Application_Non_Unique or G_Application_Handles_Command_Line
           or G_Application_Send_Environment);
      Args : GNAT.Strings.String_List :=
        (new String'("glib-application-test"), new String'("sample.txt"));
   begin
      --  The ordinary callback checks command-line data and status handling.
      App.On_Command_Line (Command_Line_Callback'Unrestricted_Access);
      App.On_Startup (Startup_Callback'Unrestricted_Access);
      App.On_Shutdown (Shutdown_Callback'Unrestricted_Access);
      App.On_Shutdown (Slot_Void_Callback'Unrestricted_Access, App);

      --  Run accepts argv as a GNAT.Strings list and returns command status.
      --  Free the caller's list after Run; the binding copies it for C.
      Assert_Cmpint_Eq (App.Run (Args'Length, Args), 23);
      Assert_True (Command_Line_Calls = 1);
      Assert_True (Shutdown_Calls = 1);
      Assert_True (Slot_Calls >= 3);
      for Arg of Args loop
         GNAT.Strings.Free (Arg);
      end loop;
      Glib.Object.Unref (Glib.Object.GObject (App));
   end Test_Command_Line_And_Run;

   --  Exercise the overload that reads the test executable's actual argc/argv.
   procedure Test_Run_Using_Process_Arguments
   with Convention => C;

   procedure Test_Run_Using_Process_Arguments is
      App : constant Gapplication := Gapplication_New
        ("org.gtkada.Coverage.ProcessArgs",
         G_Application_Non_Unique or G_Application_Handles_Command_Line);
   begin
      --  This overload obtains argc/argv from the process. A slot callback
      --  accepts the runner's switches and covers the GObject-style marshaller.
      --  It avoids GApplication's option parser rejecting the test-runner flags.
      App.On_Command_Line
        (Process_Args_Slot_Callback'Unrestricted_Access, App);
      App.On_Shutdown (Shutdown_Callback'Unrestricted_Access);
      Assert_Cmpint_Eq (App.Run, 0);
      Assert_True (Activate_Calls = 1);
      Assert_True (Shutdown_Calls = 2);
      Assert_True (Process_Slot_Calls = 1);
      Glib.Object.Unref (Glib.Object.GObject (App));
   end Test_Run_Using_Process_Arguments;

begin
   Glib.Test.Init;
   Add_Func ("/glib-application/lifecycle-properties",
             Test_Lifecycle_And_Properties'Unrestricted_Access);
   Add_Func ("/glib-application/actions",
             Test_Action_Interfaces'Unrestricted_Access);
   Add_Func ("/glib-application/command-line",
             Test_Command_Line_And_Run'Unrestricted_Access);
   Add_Func ("/glib-application/process-arguments",
             Test_Run_Using_Process_Arguments'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Run);
end Main;
