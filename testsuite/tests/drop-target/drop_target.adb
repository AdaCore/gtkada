--  Exercises Gtk.Drop_Target constructors, properties, getters, and signals.
--  GTK has no matching C test for GtkDropTarget, so these cases are new Ada
--  tests; signals are emitted directly to check the generated marshallers.

with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C.Strings;
with System;

with Gdk.Content_Formats; use Gdk.Content_Formats;
with Gdk.Drag;            use Gdk.Drag;
with Gdk.Drop;            use Gdk.Drop;
with Glib;                use Glib;
with Glib.Object;         use Glib.Object;
with Glib.Test;           use Glib.Test;
with Glib.Values;         use Glib.Values;
with Gtk.Drop_Target;     use Gtk.Drop_Target;
with Gtk.Main;
with Gtkada.Bindings;

use type System.Address;

procedure Drop_Target is
   use Interfaces.C.Strings;

   --  The fixed coordinates make the callback argument checks exact.
   Pointer_X : constant Gdouble := 3.5;
   Pointer_Y : constant Gdouble := 7.25;

   --  Callback assertions compare the active emitter and slot against these.
   Current_Target : Gtk_Drop_Target := null;
   Expected_Slot  : System.Address := System.Null_Address;
   Caught_Exceptions : Natural := 0;

   --  Counts ensure each connected handler was actually invoked once.
   Typed_Drops : Natural := 0;
   Slot_Drops  : Natural := 0;
   Typed_Enters : Natural := 0;
   Slot_Enters  : Natural := 0;
   Typed_Leaves : Natural := 0;
   Slot_Leaves  : Natural := 0;
   Typed_Motions : Natural := 0;
   Slot_Motions  : Natural := 0;

   --  Fixed C profiles let the tests emit signals without a real drag device.
   procedure Emit_Drop_Raw
     (Object : System.Address;
      Name   : chars_ptr;
      Value  : System.Address;
      X, Y   : Gdouble;
      Result : access Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Action_Raw
     (Object : System.Address;
      Name   : chars_ptr;
      X, Y   : Gdouble;
      Result : access Drag_Action)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   --  Void signals have no return-value pointer in their variadic arguments.
   procedure Emit_Void (Object : System.Address; Name : chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Test_Constructors_And_Properties with Convention => C;
   procedure Test_Signal_Handlers with Convention => C;
   procedure Test_Callback_Exceptions with Convention => C;

   --  Keep each C string alive through its synchronous signal emission only.
   procedure Emit_Drop
     (Target : Gtk_Drop_Target;
      Value  : GValue;
      Result : access Gboolean) is
      Name : chars_ptr := New_String ("drop");
   begin
      Emit_Drop_Raw
        (Get_Object (Target), Name, Value'Address,
         Pointer_X, Pointer_Y, Result);
      Free (Name);
   end Emit_Drop;

   procedure Emit_Action_For
     (Target : Gtk_Drop_Target;
      Signal : String;
      Result : access Drag_Action) is
      Name : chars_ptr := New_String (Signal);
   begin
      --  Motion and enter return a GdkDragAction through the final C argument.
      Emit_Action_Raw
        (Get_Object (Target), Name, Pointer_X, Pointer_Y, Result);
      Free (Name);
   end Emit_Action_For;

   procedure Emit_Void_For (Target : Gtk_Drop_Target; Signal : String) is
      Name : chars_ptr := New_String (Signal);
   begin
      --  Leave has no parameters after the signal name.
      Emit_Void (Get_Object (Target), Name);
      Free (Name);
   end Emit_Void_For;

   --  Typed callbacks receive the signal emitter; compare handles to verify it.
   procedure Check_Target
     (Self : access Gtk_Drop_Target_Record'Class) is
   begin
      Assert_True (Self /= null);
      Assert_True (Get_Object (Self) = Get_Object (Current_Target));
   end Check_Target;

   --  The GObject overloads receive the slot object passed to On_*.
   procedure Check_Slot (Self : access GObject_Record'Class) is
   begin
      --  This catches accidental use of the emitter instead of closure data.
      Assert_True (Self /= null);
      Assert_True (Get_Object (Self) = Expected_Slot);
   end Check_Slot;

   --  Accept needs a GdkDrop created by a live drag, so test both registrations
   --  below without emitting this signal or manufacturing a backend-owned drop.
   function On_Accept_Typed
     (Self : access Gtk_Drop_Target_Record'Class;
      Drop : not null access Gdk_Drop_Record'Class) return Boolean;
   function On_Accept_Slot
     (Self : access GObject_Record'Class;
      Drop : not null access Gdk_Drop_Record'Class) return Boolean;

   function On_Accept_Typed
     (Self : access Gtk_Drop_Target_Record'Class;
      Drop : not null access Gdk_Drop_Record'Class) return Boolean is
      pragma Unreferenced (Self, Drop);
   begin
      return True;
   end On_Accept_Typed;

   function On_Accept_Slot
     (Self : access GObject_Record'Class;
      Drop : not null access Gdk_Drop_Record'Class) return Boolean is
      pragma Unreferenced (Self, Drop);
   begin
      return True;
   end On_Accept_Slot;

   --  The drop handlers check that the GValue and both coordinates arrive intact.
   function On_Drop_Typed
     (Self : access Gtk_Drop_Target_Record'Class;
      Value : GValue;
      X, Y : Gdouble) return Boolean;
   function On_Drop_Slot
     (Self : access GObject_Record'Class;
      Value : GValue;
      X, Y : Gdouble) return Boolean;

   function On_Drop_Typed
     (Self : access Gtk_Drop_Target_Record'Class;
      Value : GValue;
      X, Y : Gdouble) return Boolean is
   begin
      Check_Target (Self);
      Assert_Cmpint_Eq (Get_Int (Value), 42);
      Assert_Cmpfloat_Eq (X, Pointer_X);
      Assert_Cmpfloat_Eq (Y, Pointer_Y);
      Typed_Drops := Typed_Drops + 1;
      return True;
   end On_Drop_Typed;

   function On_Drop_Slot
     (Self : access GObject_Record'Class;
      Value : GValue;
      X, Y : Gdouble) return Boolean is
   begin
      Check_Slot (Self);
      Assert_Cmpint_Eq (Get_Int (Value), 42);
      Assert_Cmpfloat_Eq (X, Pointer_X);
      Assert_Cmpfloat_Eq (Y, Pointer_Y);
      Slot_Drops := Slot_Drops + 1;
      return True;
   end On_Drop_Slot;

   --  Enter and motion return actions and both carry the pointer coordinates.
   function On_Enter_Typed
     (Self : access Gtk_Drop_Target_Record'Class;
      X, Y : Gdouble) return Drag_Action;
   function On_Enter_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble) return Drag_Action;
   function On_Motion_Typed
     (Self : access Gtk_Drop_Target_Record'Class;
      X, Y : Gdouble) return Drag_Action;
   function On_Motion_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble) return Drag_Action;

   function On_Enter_Typed
     (Self : access Gtk_Drop_Target_Record'Class;
      X, Y : Gdouble) return Drag_Action is
   begin
      Check_Target (Self);
      Assert_Cmpfloat_Eq (X, Pointer_X);
      Assert_Cmpfloat_Eq (Y, Pointer_Y);
      Typed_Enters := Typed_Enters + 1;
      return Gdk_Action_Copy;
   end On_Enter_Typed;

   function On_Enter_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble) return Drag_Action is
   begin
      Check_Slot (Self);
      Assert_Cmpfloat_Eq (X, Pointer_X);
      Assert_Cmpfloat_Eq (Y, Pointer_Y);
      Slot_Enters := Slot_Enters + 1;
      return Gdk_Action_Link;
   end On_Enter_Slot;

   function On_Motion_Typed
     (Self : access Gtk_Drop_Target_Record'Class;
      X, Y : Gdouble) return Drag_Action is
   begin
      Check_Target (Self);
      Assert_Cmpfloat_Eq (X, Pointer_X);
      Assert_Cmpfloat_Eq (Y, Pointer_Y);
      Typed_Motions := Typed_Motions + 1;
      return Gdk_Action_Move;
   end On_Motion_Typed;

   function On_Motion_Slot
     (Self : access GObject_Record'Class;
      X, Y : Gdouble) return Drag_Action is
   begin
      Check_Slot (Self);
      Assert_Cmpfloat_Eq (X, Pointer_X);
      Assert_Cmpfloat_Eq (Y, Pointer_Y);
      Slot_Motions := Slot_Motions + 1;
      return Gdk_Action_Copy;
   end On_Motion_Slot;

   --  Leave has no signal arguments beyond the emitting object or slot.
   procedure On_Leave_Typed (Self : access Gtk_Drop_Target_Record'Class);
   procedure On_Leave_Slot (Self : access GObject_Record'Class);

   procedure On_Leave_Typed (Self : access Gtk_Drop_Target_Record'Class) is
   begin
      Check_Target (Self);
      Typed_Leaves := Typed_Leaves + 1;
   end On_Leave_Typed;

   procedure On_Leave_Slot (Self : access GObject_Record'Class) is
   begin
      Check_Slot (Self);
      Slot_Leaves := Slot_Leaves + 1;
   end On_Leave_Slot;

   --  GtkAda reports callback exceptions through this hook instead of letting
   --  an Ada exception unwind through GLib's C signal stack.
   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Caught_Exceptions := Caught_Exceptions + 1;
   end Record_Exception;

   --  The void callback has no return value, so its exception path can be
   --  checked without leaving an undefined signal result for GTK.
   procedure Raise_Void_Typed (Self : access Gtk_Drop_Target_Record'Class);

   procedure Raise_Void_Typed (Self : access Gtk_Drop_Target_Record'Class) is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "expected typed leave callback exception";
   end Raise_Void_Typed;

   --  Check both constructors and the no-drop state, then exercise the mutable
   --  action and preload properties through their public getters and setters.
   procedure Test_Constructors_And_Properties is
      By_Function : constant Gtk_Drop_Target :=
        Gtk_Drop_Target_New (GType_Int, Gdk_Action_Copy or Gdk_Action_Move);
      By_Procedure : Gtk_Drop_Target;
      Formats : Gdk_Content_Formats;
   begin
      --  Exercise the out-parameter constructor separately from the function.
      Gtk_New (By_Procedure, GType_Int, Gdk_Action_Link);
      Assert_True (By_Function.Get_Actions =
        (Gdk_Action_Copy or Gdk_Action_Move));
      Assert_True (By_Procedure.Get_Actions = Gdk_Action_Link);

      --  The constructor's GType becomes the target's accepted content format.
      Formats := By_Function.Get_Formats;
      --  The accepted formats remain available before any drop starts.
      Assert_True (Contain_Gtype (Formats, GType_Int));
      Assert_True (By_Function.Get_Current_Drop = null);
      Assert_True (By_Function.Get_Drop = null);
      Assert_True (By_Function.Get_Value = null);

      By_Function.Reject;
      --  Reject is documented as a no-op when there is no active drop.
      Assert_True (By_Function.Get_Current_Drop = null);
      Assert_False (By_Function.Get_Preload);
      By_Function.Set_Actions (Gdk_Action_Link);
      Assert_True (By_Function.Get_Actions = Gdk_Action_Link);
      By_Function.Set_Preload (True);
      Assert_True (By_Function.Get_Preload);

      Unref (By_Function);
      Unref (By_Procedure);
   end Test_Constructors_And_Properties;

   --  Emit safe signals with typed callbacks and GObject slots. Enter is
   --  registered both ways, but its GTK default handler requires a live drop.
   procedure Test_Signal_Handlers is
      Typed_Target : constant Gtk_Drop_Target :=
        Gtk_Drop_Target_New (GType_Int, Gdk_Action_Copy);
      Slot_Target : constant Gtk_Drop_Target :=
        Gtk_Drop_Target_New (GType_Int, Gdk_Action_Copy);
      Slot : constant Gtk_Drop_Target :=
        Gtk_Drop_Target_New (GType_Int, Gdk_Action_Copy);
      Value : GValue;
      Result : aliased Gboolean := 0;
      Motion_Result : aliased Drag_Action := Gdk_Action_None;
   begin
      --  Reset counters so cases do not depend on their execution order.
      Typed_Drops := 0;
      Typed_Enters := 0;
      Typed_Leaves := 0;
      Typed_Motions := 0;
      Slot_Drops := 0;
      Slot_Enters := 0;
      Slot_Leaves := 0;
      Slot_Motions := 0;
      Init_Set_Int (Value, 42);

      --  These handlers check the Gtk-specific callback signatures and values.
      Current_Target := Typed_Target;
      --  Both accept overloads are connected; a real GdkDrop is needed to emit.
      Typed_Target.On_Accept (On_Accept_Typed'Unrestricted_Access);
      Typed_Target.On_Accept
        (On_Accept_Slot'Unrestricted_Access, Slot, After => True);
      Typed_Target.On_Drop (On_Drop_Typed'Unrestricted_Access);
      Typed_Target.On_Enter (On_Enter_Typed'Unrestricted_Access);
      Typed_Target.On_Leave (On_Leave_Typed'Unrestricted_Access);
      Typed_Target.On_Motion (On_Motion_Typed'Unrestricted_Access);

      --  The handlers validate the GValue payload and action results.
      Emit_Drop (Typed_Target, Value, Result'Access);
      Emit_Void_For (Typed_Target, "leave");
      Emit_Action_For (Typed_Target, "motion", Motion_Result'Access);

      Assert_Cmpuint_Eq (Guint (Typed_Drops), 1);
      Assert_Cmpuint_Eq (Guint (Typed_Leaves), 1);
      Assert_Cmpuint_Eq (Guint (Typed_Motions), 1);
      Assert_True (Result /= 0);
      Assert_True (Motion_Result = Gdk_Action_Move);

      --  Slot overloads receive the supplied object as their first argument.
      Current_Target := Slot_Target;
      Expected_Slot := Get_Object (Slot);
      --  Passing a third target as Slot distinguishes closure data from emitter.
      Slot_Target.On_Drop (On_Drop_Slot'Unrestricted_Access, Slot, After => True);
      Slot_Target.On_Enter (On_Enter_Slot'Unrestricted_Access, Slot);
      Slot_Target.On_Leave (On_Leave_Slot'Unrestricted_Access, Slot);
      Slot_Target.On_Motion (On_Motion_Slot'Unrestricted_Access, Slot);

      Result := 0;
      Emit_Drop (Slot_Target, Value, Result'Access);
      Emit_Void_For (Slot_Target, "leave");
      Motion_Result := Gdk_Action_None;
      Emit_Action_For (Slot_Target, "motion", Motion_Result'Access);

      --  Actions returned from coordinate signals survive the GValue marshaller.
      Assert_Cmpuint_Eq (Guint (Slot_Drops), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Leaves), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Motions), 1);
      Assert_True (Result /= 0);
      Assert_True (Motion_Result = Gdk_Action_Copy);

      Unset (Value);
      Unref (Typed_Target);
      Unref (Slot_Target);
      Unref (Slot);
      Current_Target := null;
      Expected_Slot := System.Null_Address;
   end Test_Signal_Handlers;

   --  Exercise the typed void marshaller's exception handler. Exceptions from
   --  callbacks with a signal return value are left to a live drag context.
   procedure Test_Callback_Exceptions is
      Typed_Target : constant Gtk_Drop_Target :=
        Gtk_Drop_Target_New (GType_Int, Gdk_Action_Copy);
   begin
      Caught_Exceptions := 0;
      --  The marshaller must report this callback exception to the installed hook.
      Typed_Target.On_Leave (Raise_Void_Typed'Unrestricted_Access);
      Emit_Void_For (Typed_Target, "leave");
      Assert_Cmpuint_Eq (Guint (Caught_Exceptions), 1);
      Unref (Typed_Target);
   end Test_Callback_Exceptions;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Gtkada.Bindings.Set_On_Exception (Record_Exception'Unrestricted_Access);

   Add_Func
     ("/droptarget/constructors-properties",
      Test_Constructors_And_Properties'Unrestricted_Access);
   Add_Func
     ("/droptarget/signals",
      Test_Signal_Handlers'Unrestricted_Access);
   Add_Func
     ("/droptarget/callback-exceptions",
      Test_Callback_Exceptions'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Drop_Target;
