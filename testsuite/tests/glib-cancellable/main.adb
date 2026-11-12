--  Glib.Cancellable tests for constructors, cancellation state, current
--  cancellable stack, errors, file descriptors, sources, and signal callbacks.
--  There is no matching focused C test to port.

with Ada.Command_Line;
with System;

with Glib;
with Glib.Cancellable; use Glib.Cancellable;
with Glib.Error;
with Glib.Main;
with Glib.Object;
with Glib.Test; use Glib.Test;
with Callbacks;
with Gtkada.Bindings;

procedure Main is

   use type Glib.Error.GError;
   use type Glib.Cancellable.Gcancellable;
   use type Glib.GType;
   use type Glib.Guint;
   use type Glib.Gint;
   use type Glib.Gulong;
   use type Glib.Main.G_Source;
   use type Glib.Object.GObject;
   use type System.Address;

   --  Instantiate the binding for an ordinary Ada payload; its callback is
   --  library-level in Callbacks so the stored address remains valid.
   package Integer_Connection is new Connect_User_Data (Integer);
   use type Integer_Connection.Gcallback;

   ----------------------
   -- Test_Lifecycle --
   ----------------------

   --  Exercise the straightforward state operations and release every
   --  reference acquired by constructors and GSource_New.
   procedure Test_Lifecycle with Convention => C;
   procedure Test_Lifecycle is
      First  : Gcancellable;
      Second : Gcancellable := Gcancellable_New;
      Error  : Glib.Error.GError;
      Fd     : Glib.Gint;
      Source : Glib.Main.G_Source;
   begin
      --  Both constructor forms initialize the underlying GObject.
      G_New (First);
      Initialize (First);
      Assert_True (Glib.Object.Is_Created (First.all));
      Assert_True (Glib.Object.Is_Created (Second.all));
      Assert_True (Get_Type /= 0);

      --  An uncancelled object has no error; the result is checked both ways.
      Assert_False (Is_Cancelled (First));
      Assert_False (Set_Error_If_Cancelled (First, Error));
      Assert_True (Error = null);

      --  Push and pop nest in last-in, first-out order and restore an empty
      --  stack after both objects have been removed.
      Assert_True (Get_Current = null);
      Push_Current (First);
      Assert_True (Get_Current = First);
      Push_Current (Second);
      Assert_True (Get_Current = Second);
      Pop_Current (Second);
      Assert_True (Get_Current = First);
      Pop_Current (First);
      Assert_True (Get_Current = null);

      --  Get_Fd may be unsupported on some platforms; release it only when
      --  GLib successfully allocated the descriptor.
      Fd := Get_Fd (First);
      Assert_True (Fd >= -1);
      if Fd >= 0 then
         Release_Fd (First);
      end if;

      --  A cancellable source is owned by the caller until it is unreferenced.
      Source := Source_New (First);
      Assert_True (Source /= null);
      Glib.Main.Source_Destroy (Source);
      Glib.Main.Source_Unref (Source);

      --  Cancellation sets an error and Reset restores the reusable state.
      Cancel (First);
      Assert_True (Is_Cancelled (First));
      Assert_True (Set_Error_If_Cancelled (First, Error));
      Assert_True (Error /= null);
      Glib.Error.Error_Free (Error);
      Reset (First);
      Assert_False (Is_Cancelled (First));

      --  GLib documents Cancel(null) as a no-op, and query helpers accept null.
      Cancel (null);
      Assert_False (Is_Cancelled (null));

      Glib.Object.Unref (Glib.Object.GObject (First));
      Glib.Object.Unref (Glib.Object.GObject (Second));
   end Test_Lifecycle;

   ----------------------
   -- Test_Callbacks --
   ----------------------

   --  Check direct connection management and both generated signal
   --  marshallers by cancelling one object synchronously.
   procedure Test_Callbacks with Convention => C;
   procedure Test_Callbacks is
      Cancellable : Gcancellable := Gcancellable_New;
      Slot        : Gcancellable := Gcancellable_New;
      Direct_Handler : Glib.Gulong;
      Removed     : Glib.Gulong;
      User_Data_Callback : Integer_Connection.Gcallback := null;
   begin
      --  The generic access type defaults to null as an Ada callback value.
      Assert_True (User_Data_Callback = null);
      Callbacks.Expected_Cancellable := Cancellable;
      Callbacks.Expected_Slot := Slot;

      --  The direct connection returns an ID for its no-argument callback.
      Direct_Handler := Connect
        (Cancellable, Callbacks.On_Basic_Callback'Access, null);
      Assert_True (Direct_Handler /= 0);

      --  A disconnected handler must not observe the later cancellation.
      Removed := Connect
        (Cancellable, Callbacks.On_Basic_Callback'Access, null);
      Assert_True (Removed /= 0);
      Disconnect (Cancellable, Removed);
      Disconnect (Cancellable, 0);

      --  Disconnect the direct callback after checking its returned ID. Its
      --  invocation is tested separately as a binding issue in the notes.
      Disconnect (Cancellable, Direct_Handler);

      --  The two overloads cover the typed emitter and an explicit slot object.
      On_Cancelled
        (Cancellable, Callbacks.On_Typed_Cancelled'Access);
      On_Cancelled
        (Cancellable, Callbacks.On_Slot_Cancelled'Access, Slot.all'Access,
         After => True);

      --  Deliberately raising callbacks verify that both C marshallers route
      --  exceptions to GtkAda's configured exception handler.
      On_Cancelled (Cancellable, Callbacks.On_Typed_Raises'Access);
      On_Cancelled
        (Cancellable, Callbacks.On_Slot_Raises'Access, Slot.all'Access);
      Gtkada.Bindings.Set_On_Exception
        (Callbacks.Capture_Signal_Exception'Access);

      Cancel (Cancellable);
      Assert_True (Is_Cancelled (Cancellable));
      Assert_Cmpuint_Eq (Glib.Guint (Callbacks.Basic_Callback_Count), 0);
      Assert_Cmpuint_Eq (Glib.Guint (Callbacks.Typed_Signal_Count), 1);
      Assert_Cmpuint_Eq (Glib.Guint (Callbacks.Slot_Signal_Count), 1);
      Assert_Cmpuint_Eq (Glib.Guint (Callbacks.Exception_Count), 2);
      Glib.Object.Unref (Glib.Object.GObject (Slot));
      Glib.Object.Unref (Glib.Object.GObject (Cancellable));
   end Test_Callbacks;

   --  Regression for glib-cancellable.callback-abi: the C cancelled signal
   --  passes the emitting object before the closure data. Connect's adapter
   --  must use the latter to find the no-argument Ada callback. Cancellation
   --  is synchronous, so the callback must have run when Cancel returns.
   procedure Test_Connect_Callback with Convention => C;
   procedure Test_Connect_Callback is
      Cancellable : Gcancellable := Gcancellable_New;
      Handler     : Glib.Gulong;
   begin
      Callbacks.Basic_Callback_Count := 0;
      Handler := Connect
        (Cancellable, Callbacks.On_Basic_Callback'Access, null);
      Assert_True (Handler /= 0);

      --  The defective adapter raises STORAGE_ERROR before the callback runs.
      Cancel (Cancellable);
      Assert_True
        (Callbacks.Basic_Callback_Count = 1,
         "Connect must invoke the registered callback on cancellation");
      Cancel (Cancellable);
      Assert_True
        (Callbacks.Basic_Callback_Count = 1,
         "Connect callback must run at most once");
      Disconnect (Cancellable, Handler);
      Glib.Object.Unref (Glib.Object.GObject (Cancellable));
   end Test_Connect_Callback;

   --  The generic adapter has the same ABI defect. It must dispatch the
   --  callback with the original Ada payload, rather than interpreting the
   --  emitting GCancellable as its internal closure. A null destroy notify
   --  keeps this case independent of the separate closure cleanup defect.
   procedure Test_Connect_User_Data_Callback with Convention => C;
   procedure Test_Connect_User_Data_Callback is
      Cancellable : Gcancellable := Gcancellable_New;
      Handler     : Glib.Gulong;
   begin
      Callbacks.Data_Callback_Count := 0;
      Callbacks.Last_Callback_Data := 0;
      Handler := Integer_Connection.Connect
        (Cancellable, Callbacks.On_Data_Callback'Access, 12345, null);
      Assert_True (Handler /= 0);

      Cancel (Cancellable);
      Assert_True
        (Callbacks.Data_Callback_Count = 1,
         "Connect_User_Data must invoke the registered callback");
      Assert_True
        (Callbacks.Last_Callback_Data = 12345,
         "Connect_User_Data must deliver the original Ada payload");
      Cancel (Cancellable);
      Assert_True
        (Callbacks.Data_Callback_Count = 1,
         "Connect_User_Data callback must run at most once");
      Disconnect (Cancellable, Handler);
      Glib.Object.Unref (Glib.Object.GObject (Cancellable));
   end Test_Connect_User_Data_Callback;

   --  Connecting an already-cancelled object invokes the callback directly
   --  from g_cancellable_connect rather than through signal emission. Both
   --  dispatch paths must use the same two-argument C callback profile.
   procedure Test_Connect_Already_Cancelled with Convention => C;
   procedure Test_Connect_Already_Cancelled is
      Cancellable : Gcancellable := Gcancellable_New;
      Handler     : Glib.Gulong;
   begin
      Callbacks.Basic_Callback_Count := 0;
      Callbacks.Data_Callback_Count := 0;
      Callbacks.Last_Callback_Data := 0;
      Cancel (Cancellable);

      Handler := Connect
        (Cancellable, Callbacks.On_Basic_Callback'Access, null);
      Assert_True (Handler = 0);
      Assert_True
        (Callbacks.Basic_Callback_Count = 1,
         "Connect must invoke the callback before returning");

      Handler := Integer_Connection.Connect
        (Cancellable, Callbacks.On_Data_Callback'Access, -6789, null);
      Assert_True (Handler = 0);
      Assert_True
        (Callbacks.Data_Callback_Count = 1,
         "Connect_User_Data must invoke the callback before returning");
      Assert_True
        (Callbacks.Last_Callback_Data = -6789,
         "Immediate cancellation must deliver the original Ada payload");

      Cancel (Cancellable);
      Assert_True (Callbacks.Basic_Callback_Count = 1);
      Assert_True (Callbacks.Data_Callback_Count = 1);
      Glib.Object.Unref (Glib.Object.GObject (Cancellable));
   end Test_Connect_Already_Cancelled;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/glib-cancellable/lifecycle", Test_Lifecycle'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-cancellable/callbacks", Test_Callbacks'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-cancellable/connect-callback",
      Test_Connect_Callback'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-cancellable/connect-user-data-callback",
      Test_Connect_User_Data_Callback'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-cancellable/connect-already-cancelled",
      Test_Connect_Already_Cancelled'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
