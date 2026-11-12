--  Glib.Main tests for contexts, event sources, callbacks, and source cleanup.
--  No directly matching GLib C test exists in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with System;

with Glib;      use Glib;
with Glib.Main; use Glib.Main;
with Glib.Poll;
with Glib.Test; use Glib.Test;
with Gtkada.Bindings;

procedure Main is

   use type Glib.Main.G_Main_Context;
   use type Glib.Main.G_Source;
   use type Glib.Main.G_Source_Id;
   use type Glib.Main.G_Priority;
   use type System.Address;

   --  From_Object_Free consumes a wrapper allocated with GLib's allocator.
   type Poll_FD_Access is access all Glib.Poll.GPoll_FD;
   function To_Poll_FD_Access is new Ada.Unchecked_Conversion
     (System.Address, Poll_FD_Access);
   function G_Malloc (Size : Interfaces.C.size_t) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   type Source_Data is record
      Value : Natural;
   end record;

   package Data_Sources is new Glib.Main.Generic_Sources (Source_Data);

   Low_Level_Called : Boolean := False;
   Low_Level_Raise  : Boolean := False;
   Data_Raise       : Boolean := False;
   Data_Called      : Boolean := False;
   Data_Notified    : Boolean := False;
   Data_Value       : Natural := 0;
   Exception_Caught : Boolean := False;
   Dispatch_Called  : Boolean := False;
   Finalized        : Boolean := False;

   --  Both callbacks return False so GLib removes their source after dispatch.
   function Low_Level_Callback return Boolean;
   function Low_Level_Callback return Boolean is
   begin
      Low_Level_Called := True;
      if Low_Level_Raise then
         raise Program_Error with "exercise the C callback exception boundary";
      end if;
      return False;
   end Low_Level_Callback;

   function Data_Callback (Data : Source_Data) return Boolean;
   function Data_Callback (Data : Source_Data) return Boolean is
   begin
      Data_Called := True;
      Data_Value := Data.Value;
      if Data_Raise then
         raise Program_Error with "exercise the generic callback exception boundary";
      end if;
      return False;
   end Data_Callback;

   procedure Data_Notify (Data : in out Source_Data);
   procedure Data_Notify (Data : in out Source_Data) is
   begin
      Data_Notified := True;
      Data_Value := Data.Value;
   end Data_Notify;

   --  Replace the diagnostic handler temporarily so an expected callback
   --  exception is recorded without cluttering the unattended test output.
   procedure Record_Exception (Occurrence : Ada.Exceptions.Exception_Occurrence);
   procedure Record_Exception (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Exception_Caught := True;
   end Record_Exception;

   --  This callback verifies that Default_Dispatch forwards its data callback.
   function Dispatch_Callback (Data : System.Address) return Gboolean
   with Convention => C;

   function Dispatch_Callback (Data : System.Address) return Gboolean is
      pragma Unreferenced (Data);
   begin
      Dispatch_Called := True;
      return 1;
   end Dispatch_Callback;

   function Prepare
     (Source : G_Source; Timeout : access Gint) return Gboolean
   with Convention => C;

   function Prepare
     (Source : G_Source; Timeout : access Gint) return Gboolean is
      pragma Unreferenced (Source);
   begin
      Timeout.all := 0;
      return 0;
   end Prepare;

   function Check (Source : G_Source) return Gboolean
   with Convention => C;

   function Check (Source : G_Source) return Gboolean is
      pragma Unreferenced (Source);
   begin
      return 0;
   end Check;

   procedure Finalize_Source (Source : G_Source)
   with Convention => C;

   procedure Finalize_Source (Source : G_Source) is
      pragma Unreferenced (Source);
   begin
      Finalized := True;
   end Finalize_Source;

   procedure Test_Contexts with Convention => C;
   procedure Test_Basic_Sources with Convention => C;
   procedure Test_Generic_Sources with Convention => C;
   procedure Test_Custom_Source with Convention => C;
   procedure Test_Dispatch with Convention => C;
   procedure Test_Poll_Record_Free with Convention => C;

   --  Check recursive context ownership and the empty, nonblocking iteration.
   procedure Test_Contexts is
      --  A private context makes ownership checks independent of GTK events.
      Context : constant G_Main_Context := Main_Context_New;
   begin
      Assert_True (Context /= null);
      Assert_True (Main_Context_Default /= null);
      --  Balance the explicit ref before dropping the original new-context ref.
      Main_Context_Ref (Context);
      Main_Context_Unref (Context);
      Wakeup (Context);

      Assert_True (Acquire (Context));
      Assert_True (Is_Owner (Context));
      Assert_True (Acquire (Context));
      Release (Context);
      Assert_True (Is_Owner (Context));
      Release (Context);
      Assert_True (not Is_Owner (Context));

      Assert_True (not Main_Context_Iteration (Context, May_Block => False));
      Assert_True (Depth = 0);
      Main_Context_Unref (Context);
   end Test_Contexts;

   --  Exercise the plain callbacks and the accessors on built-in sources.
   procedure Test_Basic_Sources is
      Context : constant G_Main_Context := Main_Context_New;
      Source  : G_Source;
      Id      : G_Source_Id;
   begin
      Low_Level_Called := False;
      Low_Level_Raise := False;
      --  Iterating the default context dispatches this ready idle callback.
      Id := Idle_Add (Low_Level_Callback'Unrestricted_Access);
      Assert_True (Id /= No_Source_Id);
      Assert_True (Find_Source_By_Id (Id) /= null);
      Assert_True (Main_Context_Iteration (May_Block => False));
      Assert_True (Low_Level_Called);

      --  A null Ada callback is carried as null user data; the wrapper returns
      --  False and GLib removes the idle source without calling Ada code.
      Id := Idle_Add (null);
      Assert_True (Id /= No_Source_Id);
      Assert_True (Main_Context_Iteration (May_Block => False));

      --  A zero interval is immediately ready, making the timeout deterministic.
      Low_Level_Called := False;
      Id := Timeout_Add (0, Low_Level_Callback'Unrestricted_Access);
      Assert_True (Id /= No_Source_Id);
      Assert_True (Main_Context_Iteration (May_Block => True));
      Assert_True (Low_Level_Called);

      --  A callback exception is caught by the low-level C-callable wrapper.
      --  Install a quiet handler so the test can assert that containment ran.
      Low_Level_Called := False;
      Low_Level_Raise := True;
      Exception_Caught := False;
      Gtkada.Bindings.Set_On_Exception (Record_Exception'Unrestricted_Access);
      Id := Idle_Add (Low_Level_Callback'Unrestricted_Access);
      Assert_True (Main_Context_Iteration (May_Block => False));
      Assert_True (Low_Level_Called);
      Assert_True (Exception_Caught);
      Low_Level_Raise := False;

      Source := Idle_Source_New;
      Assert_True (Get_Context (Source) = null);
      Set_Priority (Source, Priority_High);
      Assert_True (Get_Priority (Source) = Priority_High);
      Assert_True (not Get_Can_Recurse (Source));
      Set_Can_Recurse (Source, True);
      Assert_True (Get_Can_Recurse (Source));
      Set_Can_Recurse (Source, False);

      --  Poll registration can be tested with stdin without waiting on it.
      --  The source is never iterated while this descriptor is registered.
      declare
         Fd : Glib.Poll.GPoll_FD := (Fd => 0, Events => 1, Revents => 0);
      begin
         Add_Poll (Source, Fd);
         Remove_Poll (Source, Fd);
      end;

      Id := Attach (Source, Context);
      --  Attach adds a context reference while this test keeps its own source ref.
      Assert_True (Id /= No_Source_Id);
      Assert_True (Get_Id (Source) = Id);
      Assert_True (Get_Context (Source) = Context);
      Assert_True (Find_Source_By_Id (Id, Context) = Source);
      Source_Ref (Source);
      Source_Unref (Source);
      --  Remove operates only on the default context, so destroy this custom
      --  context source directly before checking its id lookup.
      Source_Destroy (Source);
      Assert_True (Find_Source_By_Id (Id, Context) = null);
      Source_Unref (Source);

      --  The Boolean overload removes a live source from the default context.
      Id := Idle_Add (Low_Level_Callback'Unrestricted_Access);
      Assert_True (Remove (Id));
      Assert_True (Find_Source_By_Id (Id) = null);

      --  Cover the procedure overload, which intentionally discards Remove's
      --  result, using a fresh idle source on the default context.
      Id := Idle_Add (Low_Level_Callback'Unrestricted_Access);
      Assert_True (Id /= No_Source_Id);
      Remove (Id);
      Assert_True (Find_Source_By_Id (Id) = null);
      Main_Context_Unref (Context);
   end Test_Basic_Sources;

   --  Verify copied callback data and that destroy notifiers run on removal.
   procedure Test_Generic_Sources is
      Context : constant G_Main_Context := Main_Context_New;
      Source  : G_Source;
      Id      : G_Source_Id;
   begin
      Data_Called := False;
      Data_Notified := False;
      Data_Value := 0;
      --  The generic wrapper stores a copy of Data until GLib destroys the source.
      Id := Data_Sources.Idle_Add
        (Data_Callback'Unrestricted_Access,
         (Value => 41),
         Notify => Data_Notify'Unrestricted_Access);
      Assert_True (Id /= No_Source_Id);
      Assert_True (Main_Context_Iteration (May_Block => False));
      Assert_True (Data_Called);
      Assert_True (Data_Notified);
      Assert_True (Data_Value = 41);

      Data_Called := False;
      Data_Notified := False;
      Id := Data_Sources.Timeout_Add
        (0, Data_Callback'Unrestricted_Access, (Value => 42),
         Notify => Data_Notify'Unrestricted_Access);
      Assert_True (Id /= No_Source_Id);
      Assert_True (Main_Context_Iteration (May_Block => True));
      Assert_True (Data_Called);
      Assert_True (Data_Notified);
      Assert_True (Data_Value = 42);

      --  Set_Callback uses the same generic trampoline for a manually created
      --  source; explicit destruction checks the notifier's removal path.
      Data_Notified := False;
      Source := Idle_Source_New;
      --  Set_Callback takes ownership of the callback-data copy until destruction.
      Data_Sources.Set_Callback
        (Source, Data_Callback'Unrestricted_Access, (Value => 43),
         Data_Notify'Unrestricted_Access);
      Id := Attach (Source, Context);
      Assert_True (Main_Context_Iteration (Context, May_Block => False));
      Assert_True (Data_Value = 43);
      Assert_True (Data_Notified);
      --  Drop the caller's reference after dispatch has destroyed the source.
      Source_Unref (Source);

      --  An Ada exception is contained in General_Cb and treated as False.
      Data_Called := False;
      Data_Notified := False;
      Data_Raise := True;
      Id := Data_Sources.Idle_Add
        (Data_Callback'Unrestricted_Access, (Value => 44),
         Notify => Data_Notify'Unrestricted_Access);
      Assert_True (Id /= No_Source_Id);
      Exception_Caught := False;
      Assert_True (Main_Context_Iteration (May_Block => False));
      Assert_True (Data_Called and Data_Notified);
      Assert_True (Data_Value = 44);
      Assert_True (Exception_Caught);
      Data_Raise := False;
      Main_Context_Unref (Context);
   end Test_Generic_Sources;

   --  Create and attach an Ada-defined source and verify its data and finalizer.
   procedure Test_Custom_Source is
      Context : constant G_Main_Context := Main_Context_New;
      Marker  : aliased Integer := 99;
      Kind    : constant G_Source_Type := G_Source_Type_New
        (Prepare  => Prepare'Unrestricted_Access,
         Check    => Check'Unrestricted_Access,
         Finalize => Finalize_Source'Unrestricted_Access);
      Source  : G_Source;
      Id      : G_Source_Id;
   begin
      Finalized := False;
      --  Marker is aliased because the custom source retains its address.
      Source := Source_New (Kind, Marker'Address);
      Assert_True (Get_User_Data (Source) = Marker'Address);
      Id := Attach (Source, Context);
      Assert_True (Id /= No_Source_Id);
      Assert_True (not Main_Context_Iteration (Context, May_Block => False));
      Source_Destroy (Source);
      Source_Unref (Source);
      Assert_True (Finalized);
      Main_Context_Unref (Context);
   end Test_Custom_Source;

   --  Check that the helper copies every GPoll_FD field before freeing the
   --  temporary GLib allocation that held the record.
   procedure Test_Poll_Record_Free is
      Bytes : constant Interfaces.C.size_t := Interfaces.C.size_t
        (Glib.Poll.GPoll_FD'Size / System.Storage_Unit);
      Wrapper : constant Poll_FD_Access :=
        To_Poll_FD_Access (G_Malloc (Bytes));
      Result : Glib.Poll.GPoll_FD;
   begin
      Wrapper.all := (Fd => 17, Events => 3, Revents => 5);
      Result := Glib.Poll.From_Object_Free (Wrapper);
      Assert_True (Result.Fd = 17);
      Assert_True (Result.Events = 3);
      Assert_True (Result.Revents = 5);
   end Test_Poll_Record_Free;

   --  Default_Dispatch forwards non-null callbacks and disables a source when
   --  there is no callback to invoke.
   procedure Test_Dispatch is
   begin
      Dispatch_Called := False;
      Assert_True
        (Default_Dispatch
           (null, Dispatch_Callback'Unrestricted_Access,
            System.Null_Address) = 1);
      Assert_True (Dispatch_Called);
      Assert_True
        (Default_Dispatch
           (null, null, System.Null_Address) = 0);
      Activate_Application;
   end Test_Dispatch;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/glib-main/contexts", Test_Contexts'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-main/basic-sources", Test_Basic_Sources'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-main/generic-sources", Test_Generic_Sources'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-main/custom-source", Test_Custom_Source'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-main/poll-record-free",
      Test_Poll_Record_Free'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib-main/dispatch", Test_Dispatch'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
