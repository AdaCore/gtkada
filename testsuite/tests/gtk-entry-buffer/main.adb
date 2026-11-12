--  Tests Gtk.Entry_Buffer construction, text operations, and signal adapters.
--  No matching gtk_entry_buffer test exists in testsuite/c_tests to port.

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Test;             use Glib.Test;
with Gtk.Entry_Buffer;      use Gtk.Entry_Buffer;
with Gtk.Main;
with Gtkada.Bindings;
with System;

procedure Main is

   use type System.Address;

   --  Keep callback observations so assertions run in the test body.
   type Observation is record
      Calls    : Natural := 0;
      Position : Guint := 0;
      Count    : Guint := 0;
      Text     : Unbounded_String := Null_Unbounded_String;
      Object   : System.Address := System.Null_Address;
   end record;

   Deleted_Typed, Deleted_Slot : Observation;
   Inserted_Typed, Inserted_Slot : Observation;
   Callback_Exception_Count : Natural := 0;

   --  These separate callback types instantiate each signal marshaller path.
   --  Typed callbacks receive the emitter; slot callbacks receive their slot.
   procedure On_Deleted_Typed
     (Self : access Gtk_Entry_Buffer_Record'Class;
      Position, N_Chars : Guint);
   procedure On_Deleted_Slot
     (Self : access GObject_Record'Class;
      Position, N_Chars : Guint);
   procedure On_Inserted_Typed
     (Self : access Gtk_Entry_Buffer_Record'Class;
      Position : Guint; Chars : UTF8_String; N_Chars : Guint);
   procedure On_Inserted_Slot
     (Self : access GObject_Record'Class;
      Position : Guint; Chars : UTF8_String; N_Chars : Guint);
   procedure Raise_Deleted_Typed
     (Self : access Gtk_Entry_Buffer_Record'Class;
      Position, N_Chars : Guint);
   procedure Raise_Deleted_Slot
     (Self : access GObject_Record'Class;
      Position, N_Chars : Guint);
   procedure Raise_Inserted_Typed
     (Self : access Gtk_Entry_Buffer_Record'Class;
      Position : Guint; Chars : UTF8_String; N_Chars : Guint);
   procedure Raise_Inserted_Slot
     (Self : access GObject_Record'Class;
      Position : Guint; Chars : UTF8_String; N_Chars : Guint);

   procedure Test_Constructors with Convention => C;
   procedure Test_Text_Operations with Convention => C;
   procedure Test_Signals with Convention => C;
   procedure Test_Callback_Exception_Containment with Convention => C;

   --  Count exceptions reported by the signal marshallers without propagating
   --  them through GLib's C signal emitter.
   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Record_Callback_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Occurrence);
   begin
      Callback_Exception_Count := Callback_Exception_Count + 1;
   end Record_Callback_Exception;

   --  Record callback parameters, including the underlying GObject pointer.
   procedure On_Deleted_Typed
     (Self : access Gtk_Entry_Buffer_Record'Class;
      Position, N_Chars : Guint) is
   begin
      --  Save the sender to distinguish typed callbacks from slot callbacks.
      Deleted_Typed.Calls := Deleted_Typed.Calls + 1;
      Deleted_Typed.Position := Position;
      Deleted_Typed.Count := N_Chars;
      Deleted_Typed.Object := Get_Object (Self);
   end On_Deleted_Typed;

   procedure On_Deleted_Slot
     (Self : access GObject_Record'Class;
      Position, N_Chars : Guint) is
   begin
      --  The callback's object is the one passed as Slot at connection time.
      Deleted_Slot.Calls := Deleted_Slot.Calls + 1;
      Deleted_Slot.Position := Position;
      Deleted_Slot.Count := N_Chars;
      Deleted_Slot.Object := Get_Object (Self);
   end On_Deleted_Slot;

   --  The inserted signal carries UTF-8 text as well as character positions.
   procedure On_Inserted_Typed
     (Self : access Gtk_Entry_Buffer_Record'Class;
      Position : Guint; Chars : UTF8_String; N_Chars : Guint) is
   begin
      --  Preserve the payload so the test can check GTK's UTF-8 conversion.
      Inserted_Typed.Calls := Inserted_Typed.Calls + 1;
      Inserted_Typed.Position := Position;
      Inserted_Typed.Count := N_Chars;
      Inserted_Typed.Text := To_Unbounded_String (Chars);
      Inserted_Typed.Object := Get_Object (Self);
   end On_Inserted_Typed;

   procedure On_Inserted_Slot
     (Self : access GObject_Record'Class;
      Position : Guint; Chars : UTF8_String; N_Chars : Guint) is
   begin
      --  This generic profile covers the second inserted-text marshaller.
      Inserted_Slot.Calls := Inserted_Slot.Calls + 1;
      Inserted_Slot.Position := Position;
      Inserted_Slot.Count := N_Chars;
      Inserted_Slot.Text := To_Unbounded_String (Chars);
      Inserted_Slot.Object := Get_Object (Self);
   end On_Inserted_Slot;

   --  Deliberately fail each callback profile to exercise marshaller guards.
   procedure Raise_Deleted_Typed
     (Self : access Gtk_Entry_Buffer_Record'Class;
      Position, N_Chars : Guint) is
      pragma Unreferenced (Self, Position, N_Chars);
   begin
      raise Program_Error with "expected deleted-text callback exception";
   end Raise_Deleted_Typed;

   procedure Raise_Deleted_Slot
     (Self : access GObject_Record'Class;
      Position, N_Chars : Guint) is
      pragma Unreferenced (Self, Position, N_Chars);
   begin
      raise Program_Error with "expected deleted-text slot exception";
   end Raise_Deleted_Slot;

   procedure Raise_Inserted_Typed
     (Self : access Gtk_Entry_Buffer_Record'Class;
      Position : Guint; Chars : UTF8_String; N_Chars : Guint) is
      pragma Unreferenced (Self, Position, Chars, N_Chars);
   begin
      raise Program_Error with "expected inserted-text callback exception";
   end Raise_Inserted_Typed;

   procedure Raise_Inserted_Slot
     (Self : access GObject_Record'Class;
      Position : Guint; Chars : UTF8_String; N_Chars : Guint) is
      pragma Unreferenced (Self, Position, Chars, N_Chars);
   begin
      raise Program_Error with "expected inserted-text slot exception";
   end Raise_Inserted_Slot;

   --  Both constructor forms create a buffer, and repeated initialization is
   --  documented to leave an already-created object unchanged.
   procedure Test_Constructors is
      Via_Function : constant Gtk_Entry_Buffer :=
        Gtk_Entry_Buffer_New ("first", -1);
      Via_Procedure : Gtk_Entry_Buffer;
   begin
      --  Verify both public constructor entry points with explicit text.
      Gtk_New (Via_Procedure, "second", -1);
      Assert_Cmpstr_Eq (Via_Function.Get_Text, "first");
      Assert_Cmpstr_Eq (Via_Procedure.Get_Text, "second");
      Gtk.Entry_Buffer.Initialize (Via_Procedure, "replacement", -1);
      Assert_Cmpstr_Eq (Via_Procedure.Get_Text, "second");
   end Test_Constructors;

   --  UTF-8 byte sizes differ from character counts; the API documents both.
   procedure Test_Text_Operations is
      Buffer : constant Gtk_Entry_Buffer :=
        Gtk_Entry_Buffer_New ("AéC", -1);
   begin
      --  ASCII contributes one byte per character; é contributes two.
      Assert_Cmpstr_Eq (Buffer.Get_Text, "AéC");
      Assert_Cmpuint_Eq (Buffer.Get_Length, 3);
      Assert_Cmpuint_Eq (Guint (Buffer.Get_Bytes), 4);
      Assert_Cmpint_Eq (Buffer.Get_Max_Length, 0);

      Assert_Cmpuint_Eq (Buffer.Insert_Text (1, "xy", 2), 2);
      Assert_Cmpstr_Eq (Buffer.Get_Text, "AxyéC");
      Assert_Cmpuint_Eq (Buffer.Delete_Text (1, 2), 2);
      Assert_Cmpstr_Eq (Buffer.Get_Text, "AéC");

      --  Negative N_Chars asks insertion to consume through its terminator.
      --  One four-byte character occupies one GTK character position.
      Assert_Cmpuint_Eq (Buffer.Insert_Text (1, "🙂", -1), 1);
      Assert_Cmpuint_Eq (Buffer.Get_Length, 4);
      Assert_Cmpuint_Eq (Guint (Buffer.Get_Bytes), 8);
      Assert_Cmpuint_Eq (Buffer.Delete_Text (1, 1), 1);
      Assert_Cmpuint_Eq (Buffer.Delete_Text (1, -1), 2);
      Assert_Cmpstr_Eq (Buffer.Get_Text, "A");

      --  A smaller limit truncates current contents; zero removes the limit.
      Buffer.Set_Text ("abcdef", -1);
      Buffer.Set_Max_Length (2);
      Assert_Cmpint_Eq (Buffer.Get_Max_Length, 2);
      Assert_Cmpstr_Eq (Buffer.Get_Text, "ab");
      Buffer.Set_Max_Length (0);
      Buffer.Set_Text ("λ🦄", -1);
      --  This combines a two-byte Greek letter and a four-byte symbol.
      Assert_Cmpstr_Eq (Buffer.Get_Text, "λ🦄");
      Assert_Cmpuint_Eq (Buffer.Get_Length, 2);
      Assert_Cmpuint_Eq (Guint (Buffer.Get_Bytes), 6);
   end Test_Text_Operations;

   --  Real edits and the explicit emit helpers exercise all four marshallers.
   procedure Test_Signals is
      Buffer : constant Gtk_Entry_Buffer :=
        Gtk_Entry_Buffer_New ("abc", -1);
      Slot : constant Gtk_Entry_Buffer := Gtk_Entry_Buffer_New ("slot", -1);
   begin
      --  Typed handlers use the emitter; generic handlers use their slot.
      On_Deleted_Text (Buffer, On_Deleted_Typed'Unrestricted_Access);
      On_Deleted_Text
        (Buffer, On_Deleted_Slot'Unrestricted_Access, Slot => Slot,
         After => True);
      On_Inserted_Text
        (Buffer, On_Inserted_Typed'Unrestricted_Access, After => True);
      On_Inserted_Text
        (Buffer, On_Inserted_Slot'Unrestricted_Access, Slot => Slot);

      Assert_Cmpuint_Eq (Buffer.Insert_Text (1, "X", -1), 1);
      Assert_Cmpuint_Eq (Buffer.Delete_Text (1, 1), 1);
      Buffer.Emit_Inserted_Text (2, "yz", 2);
      Buffer.Emit_Deleted_Text (2, 2);

      --  Each helper and each real edit should have delivered a signal.
      Assert_Cmpuint_Eq (Guint (Deleted_Typed.Calls), 2);
      Assert_Cmpuint_Eq (Guint (Deleted_Slot.Calls), 2);
      Assert_Cmpuint_Eq (Deleted_Typed.Position, 2);
      Assert_Cmpuint_Eq (Deleted_Typed.Count, 2);
      Assert_Cmpuint_Eq (Deleted_Slot.Position, 2);
      Assert_Cmpuint_Eq (Deleted_Slot.Count, 2);
      Assert_True (Deleted_Typed.Object = Get_Object (Buffer));
      Assert_True (Deleted_Slot.Object = Get_Object (Slot));

      --  The explicit inserted emission is the final callback payload.
      Assert_Cmpuint_Eq (Guint (Inserted_Typed.Calls), 2);
      Assert_Cmpuint_Eq (Guint (Inserted_Slot.Calls), 2);
      Assert_Cmpuint_Eq (Inserted_Typed.Position, 2);
      Assert_Cmpuint_Eq (Inserted_Typed.Count, 2);
      Assert_Cmpstr_Eq (To_String (Inserted_Typed.Text), "yz");
      Assert_Cmpuint_Eq (Inserted_Slot.Position, 2);
      Assert_Cmpuint_Eq (Inserted_Slot.Count, 2);
      Assert_Cmpstr_Eq (To_String (Inserted_Slot.Text), "yz");
      Assert_True (Inserted_Typed.Object = Get_Object (Buffer));
      Assert_True (Inserted_Slot.Object = Get_Object (Slot));
   end Test_Signals;

   --  Each generated marshaller catches a callback exception and calls the
   --  installed GtkAda handler once.
   procedure Test_Callback_Exception_Containment is
      Buffer : constant Gtk_Entry_Buffer := Gtk_Entry_Buffer_New ("abc", -1);
      Slot   : constant Gtk_Entry_Buffer := Gtk_Entry_Buffer_New ("slot", -1);
      Inserted, Deleted : Guint;
   begin
      Callback_Exception_Count := 0;
      On_Deleted_Text (Buffer, Raise_Deleted_Typed'Unrestricted_Access);
      On_Deleted_Text
        (Buffer, Raise_Deleted_Slot'Unrestricted_Access, Slot => Slot);
      On_Inserted_Text (Buffer, Raise_Inserted_Typed'Unrestricted_Access);
      On_Inserted_Text
        (Buffer, Raise_Inserted_Slot'Unrestricted_Access, Slot => Slot);

      Inserted := Buffer.Insert_Text (1, "x", -1);
      Deleted := Buffer.Delete_Text (1, 1);
      Assert_Cmpuint_Eq (Inserted, 1);
      Assert_Cmpuint_Eq (Deleted, 1);
      Assert_Cmpuint_Eq (Guint (Callback_Exception_Count), 4);
   end Test_Callback_Exception_Containment;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Gtkada.Bindings.Set_On_Exception
     (Record_Callback_Exception'Unrestricted_Access);
   Add_Func ("/entry-buffer/constructors", Test_Constructors'Unrestricted_Access);
   Add_Func ("/entry-buffer/text-operations", Test_Text_Operations'Unrestricted_Access);
   Add_Func ("/entry-buffer/signals", Test_Signals'Unrestricted_Access);
   Add_Func
     ("/entry-buffer/callback-exceptions",
      Test_Callback_Exception_Containment'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
