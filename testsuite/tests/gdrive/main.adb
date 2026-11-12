--  GDrive tests use a deterministic Ada GObject implementation of the
--  interface. The GTK/GLib C tests do not provide a portable drive fixture.
--  This checks the GDrive wrappers, async adapters, and all four signal pairs.

with Ada.Command_Line;
with Ada.Exceptions;
with GNAT.Strings;
with System;

with Drive_Mock;
with Glib;
with Glib.Error;
with Glib.GDrives;
with Glib.GEnums;
with Glib.GVolumes;
with Glib.Main;
with Glib.Object;
with Glib.Test;
with Gtkada.Bindings;

procedure Main is

   use type Glib.Error.GError;
   use type Glib.GDrives.Gdrive;
   use type Glib.Gint;
   use type Glib.GVolumes.Gvolume_GList;
   use type Drive_Mock.Drive;
   use type System.Address;

   Drive_Object : Drive_Mock.Drive;
   Drive        : Glib.GDrives.Gdrive := Glib.GDrives.Null_Gdrive;
   Test_Status  : Ada.Command_Line.Exit_Status;

   type Async_Kind is (Poll, Eject, Start, Stop);
   Current_Async : Async_Kind := Poll;
   Callback_Done : Boolean := False;
   Finish_Result : Boolean := False;
   Finish_Error  : Glib.Error.GError := null;
   Callback_Count : Natural := 0;
   User_Callback_Count : Natural := 0;
   User_Destroy_Count : Natural := 0;
   User_Value : Integer := 0;
   Raise_From_User_Callback : Boolean := False;

   Changed_Drive_Count, Changed_Object_Count : Natural := 0;
   Disconnected_Drive_Count, Disconnected_Object_Count : Natural := 0;
   Eject_Drive_Count, Eject_Object_Count : Natural := 0;
   Stop_Drive_Count, Stop_Object_Count : Natural := 0;
   Signal_Exception_Count : Natural := 0;

   procedure On_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Result        : Glib.G_Async_Result);
   procedure On_User_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Result        : Glib.G_Async_Result;
      Data          : Integer);

   procedure Destroy_User_Data (Data : in out Integer);

   package Poll_With_User_Data is new
     Glib.GDrives.Poll_For_Media_User_Data
       (User_Data_Type => Integer,
        Destroy        => Destroy_User_Data);

   procedure Test_Metadata;
   procedure Test_Async_Operations;
   procedure Test_User_Data_Callback;
   procedure Test_Signals;
   pragma Convention (C, Test_Metadata);
   pragma Convention (C, Test_Async_Operations);
   pragma Convention (C, Test_User_Data_Callback);
   pragma Convention (C, Test_Signals);

   procedure On_Changed_Drive (Self : Glib.GDrives.Gdrive);
   procedure On_Changed_Object
     (Self : access Glib.Object.GObject_Record'Class);
   procedure On_Disconnected_Drive (Self : Glib.GDrives.Gdrive);
   procedure On_Disconnected_Object
     (Self : access Glib.Object.GObject_Record'Class);
   procedure On_Eject_Drive (Self : Glib.GDrives.Gdrive);
   procedure On_Eject_Object
     (Self : access Glib.Object.GObject_Record'Class);
   procedure On_Stop_Drive (Self : Glib.GDrives.Gdrive);
   procedure On_Stop_Object
     (Self : access Glib.Object.GObject_Record'Class);
   procedure On_Changed_Raises (Self : Glib.GDrives.Gdrive);
   procedure On_Disconnected_Raises
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Capture_Signal_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Emit_By_Name (Object : System.Address; Name : String);
   pragma Import (C, Emit_By_Name, "ada_g_signal_emit_by_name");

   ----------------
   -- On_Ready --
   ----------------

   procedure On_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Result        : Glib.G_Async_Result)
   is
      pragma Unreferenced (Source_Object);
   begin
      Callback_Count := Callback_Count + 1;
      Callback_Done := True;
      case Current_Async is
         when Poll =>
            Finish_Result := Glib.GDrives.Poll_For_Media_Finish
              (Drive, Result, Finish_Error);
         when Eject =>
            Finish_Result := Glib.GDrives.Eject_With_Operation_Finish
              (Drive, Result, Finish_Error);
         when Start =>
            Finish_Result := Glib.GDrives.Start_Finish
              (Drive, Result, Finish_Error);
         when Stop =>
            Finish_Result := Glib.GDrives.Stop_Finish
              (Drive, Result, Finish_Error);
      end case;
   end On_Ready;

   ---------------------
   -- On_User_Ready --
   ---------------------

   procedure On_User_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Result        : Glib.G_Async_Result;
      Data          : Integer)
   is
      pragma Unreferenced (Source_Object, Result);
   begin
      User_Callback_Count := User_Callback_Count + 1;
      User_Value := Data;
      if Raise_From_User_Callback then
         raise Program_Error;
      end if;
   end On_User_Ready;

   -----------------------
   -- Destroy_User_Data --
   -----------------------

   procedure Destroy_User_Data (Data : in out Integer) is
      pragma Unreferenced (Data);
   begin
      User_Destroy_Count := User_Destroy_Count + 1;
   end Destroy_User_Data;

   --------------------
   -- Test_Metadata --
   --------------------

   --  Check fixed drive metadata and booleans, including empty volume and
   --  identifier lists, so string and interface-list conversions are tested.
   procedure Test_Metadata is
      Ids    : GNAT.Strings.String_List :=
        Glib.GDrives.Enumerate_Identifiers (Drive);
      Volumes : constant Glib.GVolumes.GVolume_GList :=
        Glib.GDrives.Get_Volumes (Drive);
   begin
      Glib.Test.Assert_True (Glib.GDrives.Can_Eject (Drive));
      Glib.Test.Assert_True (Glib.GDrives.Can_Poll_For_Media (Drive));
      Glib.Test.Assert_True (Glib.GDrives.Can_Start (Drive));
      Glib.Test.Assert_True (Glib.GDrives.Can_Start_Degraded (Drive));
      Glib.Test.Assert_True (Glib.GDrives.Can_Stop (Drive));
      Glib.Test.Assert_True (Glib.GDrives.Has_Media (Drive));
      Glib.Test.Assert_False (Glib.GDrives.Has_Volumes (Drive));
      Glib.Test.Assert_True (Glib.GDrives.Is_Media_Check_Automatic (Drive));
      Glib.Test.Assert_True (Glib.GDrives.Is_Media_Removable (Drive));
      Glib.Test.Assert_True (Glib.GDrives.Is_Removable (Drive));
      Glib.Test.Assert_True (Glib.GDrives.Get_Name (Drive) = "Coverage Drive");
      Glib.Test.Assert_True
        (Glib.GDrives.Get_Identifier (Drive, "unix-device") = "test-device");
      Glib.Test.Assert_True
        (Glib.GDrives.Get_Sort_Key (Drive) = "drive-sort-key");
      Glib.Test.Assert_Cmpint_Eq
        (Glib.Gint (Glib.GVolumes.GVolume_List.Length (Volumes)), 0);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Ids'Length), 0);
      for Index in Ids'Range loop
         GNAT.Strings.Free (Ids (Index));
      end loop;
      Glib.Test.Assert_True (Glib.GDrives."+" (Drive) = Drive);
   end Test_Metadata;

   -----------------------------
   -- Test_Async_Operations --
   -----------------------------

   --  Exercise both null and live callback paths, then verify each matching
   --  finish result and the error copied from a failed poll operation.
   procedure Test_Async_Operations is
      Error : Glib.Error.GError;

      procedure Wait_For_Callback (Expected : Boolean := True) is
      begin
         while not Callback_Done loop
            if not Glib.Main.Main_Context_Iteration (May_Block => False) then
               null;
            end if;
         end loop;
         Glib.Test.Assert_True (Finish_Result = Expected);
         if Expected then
            Glib.Test.Assert_True (Finish_Error = null);
         else
            Glib.Test.Assert_True (Finish_Error /= null);
         end if;
      end Wait_For_Callback;
   begin
      --  A null callback is explicitly supported for each async operation.
      Glib.GDrives.Poll_For_Media (Drive, null, null);
      Glib.GDrives.Eject_With_Operation
        (Drive, Glib.GEnums.G_Mount_Unmount_None, null, null, null);
      Glib.GDrives.Start
        (Drive, Glib.GEnums.G_Drive_Start_None, null, null, null);
      Glib.GDrives.Stop
        (Drive, Glib.GEnums.G_Mount_Unmount_None, null, null, null);

      Current_Async := Poll;
      Callback_Done := False;
      Finish_Result := False;
      Glib.GDrives.Poll_For_Media
        (Drive, null, On_Ready'Unrestricted_Access);
      Wait_For_Callback;

      Current_Async := Eject;
      Callback_Done := False;
      Finish_Result := False;
      Glib.GDrives.Eject_With_Operation
        (Drive, Glib.GEnums.G_Mount_Unmount_None, null, null,
         On_Ready'Unrestricted_Access);
      Wait_For_Callback;

      Current_Async := Start;
      Callback_Done := False;
      Finish_Result := False;
      Glib.GDrives.Start
        (Drive, Glib.GEnums.G_Drive_Start_None, null, null,
         On_Ready'Unrestricted_Access);
      Wait_For_Callback;

      Current_Async := Stop;
      Callback_Done := False;
      Finish_Result := False;
      Glib.GDrives.Stop
        (Drive, Glib.GEnums.G_Mount_Unmount_None, null, null,
         On_Ready'Unrestricted_Access);
      Wait_For_Callback;

      --  Exercise the false result and verify that its GError is copied out.
      Drive_Mock.Set_Finish_Succeeds (False);
      Current_Async := Poll;
      Callback_Done := False;
      Finish_Result := True;
      Finish_Error := null;
      Glib.GDrives.Poll_For_Media
        (Drive, null, On_Ready'Unrestricted_Access);
      Wait_For_Callback (Expected => False);
      Error := Finish_Error;
      Glib.Test.Assert_True (Error /= null);
      Glib.Error.Error_Free (Error);
      Finish_Error := null;
      Drive_Mock.Set_Finish_Succeeds (True);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Callback_Count), 5);
      Glib.Test.Assert_True (Drive_Mock.Poll_Count >= 2);
   end Test_Async_Operations;

   -----------------------------
   -- Test_User_Data_Callback --
   -----------------------------

   --  Check generic callback data delivery and destruction on success and
   --  when client code raises, plus the no-callback path.
   procedure Test_User_Data_Callback is
   begin
      User_Callback_Count := 0;
      User_Destroy_Count := 0;
      User_Value := 0;
      Raise_From_User_Callback := False;
      Poll_With_User_Data.Poll_For_Media
        (Drive, null, On_User_Ready'Unrestricted_Access, 42);
      while User_Destroy_Count = 0 loop
         if not Glib.Main.Main_Context_Iteration (May_Block => False) then
            null;
         end if;
      end loop;
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (User_Callback_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (User_Value), 42);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (User_Destroy_Count), 1);

      --  The generic adapter frees its closure even if a client callback
      --  raises; the exception is contained at the C callback boundary.
      User_Callback_Count := 0;
      User_Destroy_Count := 0;
      Raise_From_User_Callback := True;
      Poll_With_User_Data.Poll_For_Media
        (Drive, null, On_User_Ready'Unrestricted_Access, 7);
      while User_Destroy_Count = 0 loop
         if not Glib.Main.Main_Context_Iteration (May_Block => False) then
            null;
         end if;
      end loop;
      Raise_From_User_Callback := False;
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (User_Callback_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (User_Destroy_Count), 1);

      --  A null generic callback bypasses closure allocation and dispatch.
      Poll_With_User_Data.Poll_For_Media (Drive, null, null, 0);
   end Test_User_Data_Callback;

   -------------------
   -- Signal callbacks --
   -------------------

   procedure On_Changed_Drive (Self : Glib.GDrives.Gdrive) is
      pragma Unreferenced (Self);
   begin
      Changed_Drive_Count := Changed_Drive_Count + 1;
   end On_Changed_Drive;

   procedure On_Changed_Object
     (Self : access Glib.Object.GObject_Record'Class)
   is
   begin
      if Self /= null then
         Changed_Object_Count := Changed_Object_Count + 1;
      end if;
   end On_Changed_Object;

   procedure On_Disconnected_Drive (Self : Glib.GDrives.Gdrive) is
      pragma Unreferenced (Self);
   begin
      Disconnected_Drive_Count := Disconnected_Drive_Count + 1;
   end On_Disconnected_Drive;

   procedure On_Disconnected_Object
     (Self : access Glib.Object.GObject_Record'Class)
   is
   begin
      if Self /= null then
         Disconnected_Object_Count := Disconnected_Object_Count + 1;
      end if;
   end On_Disconnected_Object;

   procedure On_Eject_Drive (Self : Glib.GDrives.Gdrive) is
      pragma Unreferenced (Self);
   begin
      Eject_Drive_Count := Eject_Drive_Count + 1;
   end On_Eject_Drive;

   procedure On_Eject_Object
     (Self : access Glib.Object.GObject_Record'Class)
   is
   begin
      if Self /= null then
         Eject_Object_Count := Eject_Object_Count + 1;
      end if;
   end On_Eject_Object;

   procedure On_Stop_Drive (Self : Glib.GDrives.Gdrive) is
      pragma Unreferenced (Self);
   begin
      Stop_Drive_Count := Stop_Drive_Count + 1;
   end On_Stop_Drive;

   procedure On_Stop_Object
     (Self : access Glib.Object.GObject_Record'Class)
   is
   begin
      if Self /= null then
         Stop_Object_Count := Stop_Object_Count + 1;
      end if;
   end On_Stop_Object;

   -------------------------
   -- Deliberate exceptions --
   -------------------------

   procedure On_Changed_Raises (Self : Glib.GDrives.Gdrive) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "signal callback test";
   end On_Changed_Raises;

   procedure On_Disconnected_Raises
     (Self : access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "slot callback test";
   end On_Disconnected_Raises;

   procedure Capture_Signal_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (Occurrence);
   begin
      --  GtkAda contains callback exceptions at the C boundary and delegates
      --  them to this handler instead of propagating them through GLib.
      Signal_Exception_Count := Signal_Exception_Count + 1;
   end Capture_Signal_Exception;

   -------------------
   -- Test_Signals --
   -------------------

   --  Verify the drive-typed and slot-based handlers for each GDrive signal,
   --  including exception containment at both marshaller boundaries.
   procedure Test_Signals is
   begin
      --  Each GDrive signal has both a drive-typed callback and the generic
      --  GObject callback with an explicit slot. Emitting proves both
      --  marshallers recover the expected callback and slot values.
      Glib.GDrives.On_Changed (Drive, On_Changed_Drive'Unrestricted_Access);
      Glib.GDrives.On_Changed
        (Drive, On_Changed_Object'Unrestricted_Access, Drive_Object.all'Access,
         After => True);
      Glib.GDrives.On_Disconnected
        (Drive, On_Disconnected_Drive'Unrestricted_Access);
      Glib.GDrives.On_Disconnected
        (Drive, On_Disconnected_Object'Unrestricted_Access,
         Drive_Object.all'Access);
      Glib.GDrives.On_Eject_Button
        (Drive, On_Eject_Drive'Unrestricted_Access);
      Glib.GDrives.On_Eject_Button
        (Drive, On_Eject_Object'Unrestricted_Access, Drive_Object.all'Access);
      Glib.GDrives.On_Stop_Button
        (Drive, On_Stop_Drive'Unrestricted_Access);
      Glib.GDrives.On_Stop_Button
        (Drive, On_Stop_Object'Unrestricted_Access, Drive_Object.all'Access);
      Glib.GDrives.On_Changed
        (Drive, On_Changed_Raises'Unrestricted_Access);
      Glib.GDrives.On_Disconnected
        (Drive, On_Disconnected_Raises'Unrestricted_Access,
         Drive_Object.all'Access);

      Gtkada.Bindings.Set_On_Exception
        (Capture_Signal_Exception'Unrestricted_Access);

      Emit_By_Name
        (Drive_Mock.Raw_Object (Drive_Object), "changed" & ASCII.NUL);
      Emit_By_Name
        (Drive_Mock.Raw_Object (Drive_Object), "disconnected" & ASCII.NUL);
      Emit_By_Name
        (Drive_Mock.Raw_Object (Drive_Object), "eject-button" & ASCII.NUL);
      Emit_By_Name
        (Drive_Mock.Raw_Object (Drive_Object), "stop-button" & ASCII.NUL);

      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Changed_Drive_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Changed_Object_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Disconnected_Drive_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Disconnected_Object_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Eject_Drive_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Eject_Object_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Stop_Drive_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Stop_Object_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Signal_Exception_Count), 2);
   end Test_Signals;

begin
   Drive_Mock.Create (Drive_Object);
   Drive := Drive_Mock.As_Interface (Drive_Object);
   Glib.Test.Init;
   Glib.Test.Add_Func ("/gdrive/metadata", Test_Metadata'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdrive/async-operations", Test_Async_Operations'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdrive/user-data-callback", Test_User_Data_Callback'Unrestricted_Access);
   Glib.Test.Add_Func ("/gdrive/signals", Test_Signals'Unrestricted_Access);
   Test_Status := Glib.Test.Run;
   Drive_Mock.Destroy (Drive_Object);
   Ada.Command_Line.Set_Exit_Status (Test_Status);
end Main;
