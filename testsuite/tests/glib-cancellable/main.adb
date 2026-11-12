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

   --  The instance exposes the generic callback type independently of the
   --  callback execution path, which is documented in the coverage notes.
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

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/glib-cancellable/lifecycle", Test_Lifecycle'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib-cancellable/callbacks", Test_Callbacks'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
