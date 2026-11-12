--  Exercise Glib.Spawn's environment, exit-status, command-line, and PID APIs.
--  No focused C test for these bindings exists in testsuite/c_tests.
with Ada.Command_Line;
with Ada.Strings.Fixed;
with GNAT.Strings;

with Glib.Error;
with Glib.Spawn; use Glib.Spawn;
with Glib.Test;  use Glib.Test;
with Gtkada.Types; use Gtkada.Types;

procedure Main is
   use type GNAT.Strings.String_Access;
   use type Glib.Error.GError;
   use type Glib.Gboolean;
   use type Glib.Spawn.GPid;

   --  The child mode exits immediately so the parent can exercise a real PID.
   Child_Argument : constant String := "--glib-spawn-child";

   type No_User_Data is null record;
   function Spawn_Async is new Generic_Spawn_Async (No_User_Data);

   procedure Test_Environment with Convention => C;
   procedure Test_Exit_Status with Convention => C;
   procedure Test_Command_Line_Error with Convention => C;
   procedure Test_Close_Pid with Convention => C;

   --  Get_Environ returns individually allocated strings owned by the caller.
   procedure Test_Environment is
      Items : GNAT.Strings.String_List := Get_Environ;
   begin
      --  Each item is a nonempty variable name followed by an equals sign.
      Assert_True (Items'Length > 0);
      for Index in Items'Range loop
         Assert_True (Items (Index) /= null);
         Assert_True
           (Ada.Strings.Fixed.Index (Items (Index).all, "=") > 1);
         GNAT.Strings.Free (Items (Index));
      end loop;
   end Test_Environment;

   --  Status zero succeeds; a nonzero child status produces a GError.
   procedure Test_Exit_Status is
      Success_Error : aliased Glib.Error.GError := null;
      Failure_Error : aliased Glib.Error.GError := null;
   begin
      --  Zero is the successful process status on all supported platforms.
      Assert_True (Spawn_Check_Exit_Status (0, Success_Error'Access));
      Assert_True (Success_Error = null);

      --  Status one is abnormal whether it is an exit code or wait status.
      Assert_True
        (not Spawn_Check_Exit_Status (1, Failure_Error'Access));
      Assert_True (Failure_Error /= null);
      Glib.Error.Error_Free (Failure_Error);
   end Test_Exit_Status;

   --  A unique missing executable exercises the documented launch-error path.
   procedure Test_Command_Line_Error is
      Error : aliased Glib.Error.GError := null;
      Command : Chars_Ptr := New_String
        ("gtkada-glib-spawn-no-such-command-20261111");
      Started : Boolean;
   begin
      Started := Spawn_Command_Line_Async (Command, Error'Access);
      Assert_True (not Started);
      Assert_True (Error /= null);
      --  A nonexistent executable is reported in GLib's spawn-error domain.
      Assert_True
        (GSpawn_Error_Matches (Error, G_Spawn_Error_Noent));
      Glib.Error.Error_Free (Error);
      Free (Command);
   end Test_Command_Line_Error;

   --  Spawn this executable in child mode and close the returned process ref.
   procedure Test_Close_Pid is
      Argv : aliased Chars_Ptr_Array :=
        New_String (Ada.Command_Line.Command_Name)
        + Child_Argument + Null_Ptr;
      Child : aliased GPid := 0;
      Error : aliased Glib.Error.GError := null;
      Started : Glib.Gboolean;
   begin
      Started := Spawn_Async
        (Argv => Argv'Access,
         Envp => null,
         Flags => G_Spawn_Default,
         Child_Setup => null,
         Data => null,
         Child_Pid => Child'Access,
         Error => Error'Access);
      Assert_True (Started /= 0);
      Assert_True (Error = null);
      --  A nonzero process reference confirms that a real child was spawned.
      Assert_True (Child /= 0);

      --  The process can finish immediately; its reference is still valid to
      --  close on platforms where GPid owns an operating-system resource.
      Spawn_Close_Pid (Child);
      Free (Argv);
   end Test_Close_Pid;

begin
   --  This invocation is only the short-lived child of Test_Close_Pid.
   if Ada.Command_Line.Argument_Count = 1
     and then Ada.Command_Line.Argument (1) = Child_Argument
   then
      return;
   end if;

   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/glib-spawn/environment", Test_Environment'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-spawn/exit-status", Test_Exit_Status'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-spawn/command-line-error",
      Test_Command_Line_Error'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-spawn/close-pid", Test_Close_Pid'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
