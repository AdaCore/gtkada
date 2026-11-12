--  Glib.GMounts tests use a deterministic GObject implementation of GMount.
--  No C GMount test supplies a portable mount, so the mock checks interface
--  accessors, async wrappers, finish methods, and signal callbacks directly.

with Ada.Command_Line;
with Ada.Exceptions;
with GNAT.Strings;
with System;

with Glib;
with Glib.Error;
with Glib.GEnums;
with Glib.GMounts;
with Glib.Main;
with Glib.Object;
with Glib.Test;
with Gtkada.Bindings;
with Mount_Mock;

procedure Main is

   use type Glib.Error.GError;
   use type Glib.GMounts.Gmount;
   use type Glib.Object.GObject;
   use type Glib.Gint;
   use type System.Address;

   Mock  : Mount_Mock.Mount;
   Mount : Glib.GMounts.Gmount := Glib.GMounts.Null_Gmount;

   type Async_Kind is (Eject_Kind, Guess_Kind, Remount_Kind, Unmount_Kind);
   Current_Async  : Async_Kind := Eject_Kind;
   Async_Done     : Boolean := False;
   Async_Success  : Boolean := False;
   Async_Error    : Glib.Error.GError := null;
   Content_Count  : Natural := 0;

   Changed_Mount_Count, Changed_Slot_Count : Natural := 0;
   Pre_Mount_Count, Pre_Slot_Count         : Natural := 0;
   Unmounted_Mount_Count, Unmounted_Slot_Count : Natural := 0;
   Exception_Count : Natural := 0;

   procedure On_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Result        : Glib.G_Async_Result);

   procedure Test_Metadata with Convention => C;
   procedure Test_Content_Type with Convention => C;
   procedure Test_Async_Operations with Convention => C;
   procedure Test_Signals with Convention => C;

   procedure Emit_By_Name (Object : System.Address; Name : String);
   pragma Import (C, Emit_By_Name, "ada_g_signal_emit_by_name");

   procedure Free_Content_Types (Items : in out GNAT.Strings.String_List);

   -------------------------
   -- Free_Content_Types --
   -------------------------

   procedure Free_Content_Types (Items : in out GNAT.Strings.String_List) is
   begin
      for Index in Items'Range loop
         GNAT.Strings.Free (Items (Index));
      end loop;
   end Free_Content_Types;

   ----------------
   -- On_Ready --
   ----------------

   --  The library callback adapter supplies a GObject and GAsyncResult; this
   --  dispatcher calls the matching public finish routine for each request.
   procedure On_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Result        : Glib.G_Async_Result)
   is
      pragma Unreferenced (Source_Object);
   begin
      case Current_Async is
         when Eject_Kind =>
            Async_Success := Glib.GMounts.Eject_With_Operation_Finish
              (Mount, Result, Async_Error);
         when Guess_Kind =>
            declare
               Items : GNAT.Strings.String_List :=
                 Glib.GMounts.Guess_Content_Type_Finish
                   (Mount, Result, Async_Error);
            begin
               Content_Count := Items'Length;
               Free_Content_Types (Items);
               Async_Success := Async_Error = null;
            end;
         when Remount_Kind =>
            Async_Success := Glib.GMounts.Remount_Finish
              (Mount, Result, Async_Error);
         when Unmount_Kind =>
            Async_Success := Glib.GMounts.Unmount_With_Operation_Finish
              (Mount, Result, Async_Error);
      end case;
      Async_Done := True;
   end On_Ready;

   ------------------
   -- Test_Metadata --
   ------------------

   --  Check both true capability flags, owned strings, nullable sort key,
   --  unary interface conversion, and the temporary shadow count.
   procedure Test_Metadata is
   begin
      Glib.Test.Assert_True (Glib.GMounts.Can_Eject (Mount));
      Glib.Test.Assert_True (Glib.GMounts.Can_Unmount (Mount));
      Glib.Test.Assert_Cmpstr_Eq
        (Glib.GMounts.Get_Name (Mount), "Coverage Mount");
      Glib.Test.Assert_Cmpstr_Eq (Glib.GMounts.Get_Sort_Key (Mount), "");
      Glib.Test.Assert_Cmpstr_Eq
        (Glib.GMounts.Get_Uuid (Mount), "coverage-mount-uuid");
      Glib.Test.Assert_True (Glib.GMounts."+" (Mount) = Mount);

      Glib.Test.Assert_False (Glib.GMounts.Is_Shadowed (Mount));
      Glib.GMounts.Shadow (Mount);
      Glib.Test.Assert_True (Glib.GMounts.Is_Shadowed (Mount));
      Glib.GMounts.Unshadow (Mount);
      Glib.Test.Assert_False (Glib.GMounts.Is_Shadowed (Mount));
   end Test_Metadata;

   ------------------------
   -- Test_Content_Type --
   ------------------------

   --  An empty content list is valid. Both successful and failed sync results
   --  exercise the binding's string-vector conversion and GError copy-out.
   procedure Test_Content_Type is
      Error : Glib.Error.GError;
   begin
      Mount_Mock.Set_Sync_Succeeds (True);
      declare
         Items : GNAT.Strings.String_List := Glib.GMounts.Guess_Content_Type_Sync
           (Mount, Force_Rescan => True, Cancellable => null, Error => Error);
      begin
         Glib.Test.Assert_True (Error = null);
         Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Items'Length), 0);
         Free_Content_Types (Items);
      end;

      Mount_Mock.Set_Sync_Succeeds (False);
      declare
         Items : GNAT.Strings.String_List := Glib.GMounts.Guess_Content_Type_Sync
           (Mount, Force_Rescan => False, Cancellable => null, Error => Error);
      begin
         Glib.Test.Assert_True (Error /= null);
         Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Items'Length), 0);
         Free_Content_Types (Items);
      end;
      Glib.Error.Error_Free (Error);
   end Test_Content_Type;

   ----------------------------
   -- Test_Async_Operations --
   ----------------------------

   --  Each operation is called with a null callback and with a live callback.
   --  The mock queues only live callbacks, so no real mount state can change.
   procedure Test_Async_Operations is

      procedure Wait_For_Result (Expected : Boolean := True) is
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
         Content_Count := 0;
         case Kind is
            when Eject_Kind =>
               Glib.GMounts.Eject_With_Operation
                 (Mount, Glib.GEnums.G_Mount_Unmount_None,
                  null, null, On_Ready'Unrestricted_Access);
            when Guess_Kind =>
               Glib.GMounts.Guess_Content_Type
                 (Mount, Force_Rescan => True, Cancellable => null,
                  Callback => On_Ready'Unrestricted_Access);
            when Remount_Kind =>
               Glib.GMounts.Remount
                 (Mount, Glib.GEnums.G_Mount_Mount_None,
                  null, null, On_Ready'Unrestricted_Access);
            when Unmount_Kind =>
               Glib.GMounts.Unmount_With_Operation
                 (Mount, Glib.GEnums.G_Mount_Unmount_None,
                  null, null, On_Ready'Unrestricted_Access);
         end case;
         Wait_For_Result (Expected);
         if Kind = Guess_Kind then
            Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Content_Count), 0);
         end if;
      end Start_And_Wait;

   begin
      --  The null callback branch is supported by all four async wrappers.
      Glib.GMounts.Eject_With_Operation
        (Mount, Glib.GEnums.G_Mount_Unmount_None, null, null, null);
      Glib.GMounts.Guess_Content_Type
        (Mount, Force_Rescan => False, Cancellable => null, Callback => null);
      Glib.GMounts.Remount
        (Mount, Glib.GEnums.G_Mount_Mount_None, null, null, null);
      Glib.GMounts.Unmount_With_Operation
        (Mount, Glib.GEnums.G_Mount_Unmount_None, null, null, null);

      Start_And_Wait (Eject_Kind);
      Start_And_Wait (Guess_Kind);
      Start_And_Wait (Remount_Kind);
      Start_And_Wait (Unmount_Kind);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Mount_Mock.Eject_Count), 2);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Mount_Mock.Guess_Count), 2);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Mount_Mock.Remount_Count), 2);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Mount_Mock.Unmount_Count), 2);

      --  A failed finish result proves each wrapper returns its Boolean and
      --  transfers the returned GError to the Ada caller.
      Mount_Mock.Set_Finish_Succeeds (False);
      Start_And_Wait (Eject_Kind, Expected => False);
      Start_And_Wait (Guess_Kind, Expected => False);
      Start_And_Wait (Remount_Kind, Expected => False);
      Start_And_Wait (Unmount_Kind, Expected => False);
      Mount_Mock.Set_Finish_Succeeds (True);
   end Test_Async_Operations;

   -------------------
   -- Signal handlers --
   -------------------

   --  Typed callbacks receive the GMount interface; slot callbacks receive
   --  its underlying GObject, matching the two generated marshaller paths.
   procedure On_Changed_Mount (Self : Glib.GMounts.Gmount) is
   begin
      Glib.Test.Assert_True (Self = Mount);
      Changed_Mount_Count := Changed_Mount_Count + 1;
   end On_Changed_Mount;

   procedure On_Changed_Slot
     (Self : access Glib.Object.GObject_Record'Class)
   is
   begin
      Glib.Test.Assert_True
        (Glib.Object.Get_Object (Self) = Mount_Mock.Raw_Object (Mock));
      Changed_Slot_Count := Changed_Slot_Count + 1;
   end On_Changed_Slot;

   procedure On_Pre_Unmount_Mount (Self : Glib.GMounts.Gmount) is
   begin
      Glib.Test.Assert_True (Self = Mount);
      Pre_Mount_Count := Pre_Mount_Count + 1;
   end On_Pre_Unmount_Mount;

   procedure On_Pre_Unmount_Slot
     (Self : access Glib.Object.GObject_Record'Class)
   is
   begin
      Glib.Test.Assert_True
        (Glib.Object.Get_Object (Self) = Mount_Mock.Raw_Object (Mock));
      Pre_Slot_Count := Pre_Slot_Count + 1;
   end On_Pre_Unmount_Slot;

   procedure On_Unmounted_Mount (Self : Glib.GMounts.Gmount) is
   begin
      Glib.Test.Assert_True (Self = Mount);
      Unmounted_Mount_Count := Unmounted_Mount_Count + 1;
   end On_Unmounted_Mount;

   procedure On_Unmounted_Slot
     (Self : access Glib.Object.GObject_Record'Class)
   is
   begin
      Glib.Test.Assert_True
        (Glib.Object.Get_Object (Self) = Mount_Mock.Raw_Object (Mock));
      Unmounted_Slot_Count := Unmounted_Slot_Count + 1;
   end On_Unmounted_Slot;

   procedure On_Changed_Raises (Self : Glib.GMounts.Gmount) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "typed GMount signal callback";
   end On_Changed_Raises;

   procedure On_Pre_Unmount_Raises
     (Self : access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "slot GMount signal callback";
   end On_Pre_Unmount_Raises;

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

   --  Emit all three interface signals and verify typed, slot, and exception
   --  callback dispatch through the two GMount signal marshallers.
   procedure Test_Signals is
      Object : constant System.Address := Mount_Mock.Raw_Object (Mock);
   begin
      Glib.GMounts.On_Changed
        (Mount, On_Changed_Mount'Unrestricted_Access);
      Glib.GMounts.On_Changed
        (Mount, On_Changed_Slot'Unrestricted_Access, Mock.all'Access,
         After => True);
      Glib.GMounts.On_Pre_Unmount
        (Mount, On_Pre_Unmount_Mount'Unrestricted_Access);
      Glib.GMounts.On_Pre_Unmount
        (Mount, On_Pre_Unmount_Slot'Unrestricted_Access, Mock.all'Access);
      Glib.GMounts.On_Unmounted
        (Mount, On_Unmounted_Mount'Unrestricted_Access);
      Glib.GMounts.On_Unmounted
        (Mount, On_Unmounted_Slot'Unrestricted_Access, Mock.all'Access);

      --  GtkAda catches exceptions at the C callback boundary and sends them
      --  to this handler, so the deliberate failures cannot escape to GLib.
      Glib.GMounts.On_Changed
        (Mount, On_Changed_Raises'Unrestricted_Access);
      Glib.GMounts.On_Pre_Unmount
        (Mount, On_Pre_Unmount_Raises'Unrestricted_Access, Mock.all'Access);
      Gtkada.Bindings.Set_On_Exception
        (Capture_Signal_Exception'Unrestricted_Access);

      Emit_By_Name (Object, "changed" & ASCII.NUL);
      Emit_By_Name (Object, "pre-unmount" & ASCII.NUL);
      Emit_By_Name (Object, "unmounted" & ASCII.NUL);

      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Changed_Mount_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Changed_Slot_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Pre_Mount_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Pre_Slot_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Unmounted_Mount_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Unmounted_Slot_Count), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Exception_Count), 2);
   end Test_Signals;

begin
   Mount_Mock.Create (Mock);
   Mount := Mount_Mock.As_Interface (Mock);
   Glib.Test.Init;
   Glib.Test.Add_Func ("/gmounts/metadata", Test_Metadata'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gmounts/content-type", Test_Content_Type'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gmounts/async-operations",
      Test_Async_Operations'Unrestricted_Access);
   Glib.Test.Add_Func ("/gmounts/signals", Test_Signals'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
   Mount_Mock.Destroy (Mock);
end Main;
