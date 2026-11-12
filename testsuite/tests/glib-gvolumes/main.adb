--  GVolume tests use a deterministic Ada GObject implementation.
--  There is no portable C volume fixture, so this checks metadata, async
--  wrappers and both signal callback forms without touching real media.

with Ada.Command_Line;
with Ada.Exceptions;
with GNAT.Strings;
with System;

with Glib;
with Glib.Error;
with Glib.GEnums;
with Glib.GVolumes;
with Glib.Main;
with Glib.Object;
with Glib.Test;
with Gtkada.Bindings;
with Volume_Mock;

procedure Main is

   use type Glib.Error.GError;
   use type Glib.GVolumes.Gvolume;
   use type Glib.Gint;
   use type System.Address;

   Mock   : Volume_Mock.Volume;
   Volume : Glib.GVolumes.Gvolume := Glib.GVolumes.Null_Gvolume;

   type Async_Kind is (Eject_Kind, Mount_Kind);
   Current_Async : Async_Kind := Eject_Kind;
   Async_Done    : Boolean := False;
   Async_Success : Boolean := False;
   Async_Error   : Glib.Error.GError := null;

   Changed_Volume_Count, Changed_Slot_Count : Natural := 0;
   Removed_Volume_Count, Removed_Slot_Count : Natural := 0;
   Exception_Count : Natural := 0;

   procedure On_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Result        : Glib.G_Async_Result);

   --  Each test checks observable results from the public interface wrapper.
   procedure Test_Metadata with Convention => C;
   procedure Test_Async_Operations with Convention => C;
   procedure Test_Signals with Convention => C;

   procedure On_Changed_Volume (Self : Glib.GVolumes.Gvolume);
   procedure On_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class);
   procedure On_Removed_Volume (Self : Glib.GVolumes.Gvolume);
   procedure On_Removed_Slot
     (Self : access Glib.Object.GObject_Record'Class);
   procedure On_Changed_Raises (Self : Glib.GVolumes.Gvolume);
   procedure On_Removed_Raises
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Capture_Signal_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   --  This GLib helper emits interface signals without a device monitor.
   procedure Emit_By_Name (Object : System.Address; Name : String);
   pragma Import (C, Emit_By_Name, "ada_g_signal_emit_by_name");

   ----------------
   -- On_Ready --
   ----------------

   --  The Ada adapter converts GTask's source object and result before
   --  dispatching here; finish methods then report a checked result.
   procedure On_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Result        : Glib.G_Async_Result)
   is
      pragma Unreferenced (Source_Object);
   begin
      case Current_Async is
         when Eject_Kind =>
            Async_Success := Glib.GVolumes.Eject_With_Operation_Finish
              (Volume, Result, Async_Error);
         when Mount_Kind =>
            Async_Success := Glib.GVolumes.Mount_Finish
              (Volume, Result, Async_Error);
      end case;
      Async_Done := True;
   end On_Ready;

   ------------------
   -- Test_Metadata --
   ------------------

   --  Check fixed capability flags, owned strings, nullable sort key,
   --  identifier-array conversion, and both interface conversions.
   procedure Test_Metadata is
      Ids : GNAT.Strings.String_List :=
        Glib.GVolumes.Enumerate_Identifiers (Volume);
      Raw : constant System.Address := Glib.GVolumes.Convert (Volume);
   begin
      Glib.Test.Assert_True (Glib.GVolumes.Can_Eject (Volume));
      Glib.Test.Assert_True (Glib.GVolumes.Can_Mount (Volume));
      Glib.Test.Assert_True (Glib.GVolumes.Should_Automount (Volume));
      Glib.Test.Assert_Cmpstr_Eq
        (Glib.GVolumes.Get_Name (Volume), "Coverage Volume");
      Glib.Test.Assert_Cmpstr_Eq
        (Glib.GVolumes.Get_Identifier (Volume, "uuid"), "volume-test-id");
      Glib.Test.Assert_Cmpstr_Eq (Glib.GVolumes.Get_Uuid (Volume),
                                  "volume-test-uuid");
      Glib.Test.Assert_Cmpstr_Eq (Glib.GVolumes.Get_Sort_Key (Volume), "");
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Ids'Length), 0);
      Glib.Test.Assert_True (Raw = Volume_Mock.Raw_Object (Mock));
      Glib.Test.Assert_True (Glib.GVolumes.Convert (Raw) = Volume);
      Glib.Test.Assert_True (Glib.GVolumes."+" (Volume) = Volume);
   end Test_Metadata;

   ----------------------------
   -- Test_Async_Operations --
   ----------------------------

   --  Null callbacks cover fire-and-forget branches. Live callbacks exercise
   --  the adapter and finish wrappers, including their GError copy-out path.
   procedure Test_Async_Operations is

      procedure Wait_For_Result (Expected : Boolean) is
      begin
         while not Async_Done loop
            if not Glib.Main.Main_Context_Iteration (May_Block => False) then
               null;
            end if;
         end loop;
         Glib.Test.Assert_True (Async_Success = Expected);
         if Expected then
            Glib.Test.Assert_True (Async_Error = null);
         else
            Glib.Test.Assert_True (Async_Error /= null);
            Glib.Error.Error_Free (Async_Error);
            Async_Error := null;
         end if;
      end Wait_For_Result;

      procedure Start_And_Wait
        (Kind : Async_Kind; Expected : Boolean := True)
      is
      begin
         Current_Async := Kind;
         Async_Done := False;
         Async_Success := False;
         Async_Error := null;
         case Kind is
            when Eject_Kind =>
               Glib.GVolumes.Eject_With_Operation
                 (Volume, Glib.GEnums.G_Mount_Unmount_None,
                  null, null, On_Ready'Unrestricted_Access);
            when Mount_Kind =>
               Glib.GVolumes.Mount
                 (Volume, Glib.GEnums.G_Mount_Mount_None,
                  null, null, On_Ready'Unrestricted_Access);
         end case;
         Wait_For_Result (Expected);
      end Start_And_Wait;

   begin
      --  Both asynchronous methods explicitly support a null callback.
      Glib.GVolumes.Eject_With_Operation
        (Volume, Glib.GEnums.G_Mount_Unmount_None, null, null, null);
      Glib.GVolumes.Mount
        (Volume, Glib.GEnums.G_Mount_Mount_None, null, null, null);

      Start_And_Wait (Eject_Kind);
      Start_And_Wait (Mount_Kind);
      Glib.Test.Assert_Cmpint_Eq
        (Glib.Gint (Volume_Mock.Eject_Count), 2);
      Glib.Test.Assert_Cmpint_Eq
        (Glib.Gint (Volume_Mock.Mount_Count), 2);

      --  A failed finish result verifies Boolean and error propagation.
      Volume_Mock.Set_Finish_Succeeds (False);
      Start_And_Wait (Eject_Kind, Expected => False);
      Start_And_Wait (Mount_Kind, Expected => False);
      Volume_Mock.Set_Finish_Succeeds (True);
   end Test_Async_Operations;

   ------------------------
   -- Signal callbacks --
   ------------------------

   procedure On_Changed_Volume (Self : Glib.GVolumes.Gvolume) is
   begin
      Glib.Test.Assert_True (Self = Volume);
      Changed_Volume_Count := Changed_Volume_Count + 1;
   end On_Changed_Volume;

   procedure On_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class)
   is
   begin
      Glib.Test.Assert_True
        (Glib.Object.Get_Object (Self) = Volume_Mock.Raw_Object (Mock));
      Changed_Slot_Count := Changed_Slot_Count + 1;
   end On_Changed_Slot;

   procedure On_Removed_Volume (Self : Glib.GVolumes.Gvolume) is
   begin
      Glib.Test.Assert_True (Self = Volume);
      Removed_Volume_Count := Removed_Volume_Count + 1;
   end On_Removed_Volume;

   procedure On_Removed_Slot
     (Self : access Glib.Object.GObject_Record'Class)
   is
   begin
      Glib.Test.Assert_True
        (Glib.Object.Get_Object (Self) = Volume_Mock.Raw_Object (Mock));
      Removed_Slot_Count := Removed_Slot_Count + 1;
   end On_Removed_Slot;

   --  Deliberate exceptions cover the marshallers' boundary handler.
   procedure On_Changed_Raises (Self : Glib.GVolumes.Gvolume) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "typed GVolume signal callback";
   end On_Changed_Raises;

   procedure On_Removed_Raises
     (Self : access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "slot GVolume signal callback";
   end On_Removed_Raises;

   procedure Capture_Signal_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (Occurrence);
   begin
      Exception_Count := Exception_Count + 1;
   end Capture_Signal_Exception;

   ------------------
   -- Test_Signals --
   ------------------

   --  Emit both GVolume signals through typed and slot callbacks. The two
   --  deliberate exceptions verify that exceptions are contained at C.
   procedure Test_Signals is
      Object : constant System.Address := Volume_Mock.Raw_Object (Mock);
   begin
      Glib.GVolumes.On_Changed
        (Volume, On_Changed_Volume'Unrestricted_Access);
      Glib.GVolumes.On_Changed
        (Volume, On_Changed_Slot'Unrestricted_Access, Mock.all'Access,
         After => True);
      Glib.GVolumes.On_Removed
        (Volume, On_Removed_Volume'Unrestricted_Access);
      Glib.GVolumes.On_Removed
        (Volume, On_Removed_Slot'Unrestricted_Access, Mock.all'Access);

      Glib.GVolumes.On_Changed
        (Volume, On_Changed_Raises'Unrestricted_Access);
      Glib.GVolumes.On_Removed
        (Volume, On_Removed_Raises'Unrestricted_Access, Mock.all'Access);
      Gtkada.Bindings.Set_On_Exception
        (Capture_Signal_Exception'Unrestricted_Access);

      Emit_By_Name (Object, "changed" & ASCII.NUL);
      Emit_By_Name (Object, "removed" & ASCII.NUL);

      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Changed_Volume_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Changed_Slot_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Removed_Volume_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Removed_Slot_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Exception_Count), 2);
   end Test_Signals;

begin
   Volume_Mock.Create (Mock);
   Volume := Volume_Mock.As_Interface (Mock);
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gvolumes/metadata", Test_Metadata'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gvolumes/async-operations",
      Test_Async_Operations'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gvolumes/signals", Test_Signals'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
   Volume_Mock.Destroy (Mock);
end Main;
