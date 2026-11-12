--  Ada port of GTK's testsuite/gtk/adjustment.c (GTK 4.22.2).
--
--  Gtk.Adjustment is a pure data model, so the whole suite runs without a
--  realized window: it exercises the constructor and every getter/setter
--  (basic), the changed/value-changed emission counts (signals), value
--  clamping to the adjustment range (clamp) and Clamp_Page (clamp_page).
--  All four C test cases are ported.

with Ada.Exceptions;
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Glib.Test;        use Glib.Test;
with Ada.Command_Line;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Main;
with Gtkada.Bindings;
with System;

use type System.Address;

procedure Adjustment is

   Changed_Count       : Gint := 0;
   Value_Changed_Count : Gint := 0;
   --  Callback-visible state records which slot the generic marshaller passes.
   Changed_Slot_Count  : Gint := 0;
   Value_Slot_Count    : Gint := 0;
   Expected_Slot       : System.Address := System.Null_Address;
   Raise_Callbacks     : Boolean := False;
   Exception_Count     : Natural := 0;

   procedure Test_Basic
   with Convention => C;

   procedure Test_Signals
   with Convention => C;

   procedure Test_Signal_Slots
   with Convention => C;

   procedure Test_Clamp
   with Convention => C;

   procedure Test_Clamp_Page
   with Convention => C;

   procedure Changed_Cb (Self : access Gtk_Adjustment_Record'Class);
   procedure Value_Changed_Cb (Self : access Gtk_Adjustment_Record'Class);
   --  These handlers use the generic GObject callback signature.
   procedure Changed_Slot_Cb (Self : access GObject_Record'Class);
   procedure Value_Slot_Cb (Self : access GObject_Record'Class);
   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Changed_Cb (Self : access Gtk_Adjustment_Record'Class) is
      pragma Unreferenced (Self);
   begin
      if Raise_Callbacks then
         raise Program_Error with "expected changed callback exception";
      end if;
      Changed_Count := Changed_Count + 1;
   end Changed_Cb;

   procedure Value_Changed_Cb (Self : access Gtk_Adjustment_Record'Class) is
      pragma Unreferenced (Self);
   begin
      if Raise_Callbacks then
         raise Program_Error with "expected value-changed callback exception";
      end if;
      Value_Changed_Count := Value_Changed_Count + 1;
   end Value_Changed_Cb;

   --  The generic signal overload passes its separate slot object to the
   --  callback. Check that it is the connected object, not the emitter.
   procedure Changed_Slot_Cb (Self : access GObject_Record'Class) is
   begin
      Assert_True (Get_Object (Self) = Expected_Slot);
      if Raise_Callbacks then
         raise Program_Error with "expected changed slot exception";
      end if;
      Changed_Slot_Count := Changed_Slot_Count + 1;
   end Changed_Slot_Cb;

   procedure Value_Slot_Cb (Self : access GObject_Record'Class) is
   begin
      Assert_True (Get_Object (Self) = Expected_Slot);
      if Raise_Callbacks then
         raise Program_Error with "expected value-changed slot exception";
      end if;
      Value_Slot_Count := Value_Slot_Count + 1;
   end Value_Slot_Cb;

   --  Count exceptions caught by the signal marshallers instead of letting
   --  them unwind through GTK's C signal machinery.
   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (Occurrence);
   begin
      Exception_Count := Exception_Count + 1;
   end Record_Exception;

   procedure Test_Basic is
      A : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (2.0, 0.0, 100.0, 1.0, 5.0, 10.0);
   begin
      Assert_Cmpfloat_Eq (A.Get_Value, 2.0);
      Assert_Cmpfloat_Eq (A.Get_Lower, 0.0);
      Assert_Cmpfloat_Eq (A.Get_Upper, 100.0);
      Assert_Cmpfloat_Eq (A.Get_Step_Increment, 1.0);
      Assert_Cmpfloat_Eq (A.Get_Page_Increment, 5.0);
      Assert_Cmpfloat_Eq (A.Get_Page_Size, 10.0);
      Assert_Cmpfloat_Eq (A.Get_Minimum_Increment, 1.0);

      A.Set_Value (50.0);
      A.Set_Lower (20.0);
      A.Set_Upper (75.5);
      A.Set_Step_Increment (2.2);
      A.Set_Page_Increment (1.5);
      A.Set_Page_Size (10.0);

      Assert_Cmpfloat_Eq (A.Get_Value, 50.0);
      Assert_Cmpfloat_Eq (A.Get_Lower, 20.0);
      Assert_Cmpfloat_Eq (A.Get_Upper, 75.5);
      Assert_Cmpfloat_Eq (A.Get_Step_Increment, 2.2);
      Assert_Cmpfloat_Eq (A.Get_Page_Increment, 1.5);
      Assert_Cmpfloat_Eq (A.Get_Page_Size, 10.0);
      Assert_Cmpfloat_Eq (A.Get_Minimum_Increment, 1.5);
   end Test_Basic;

   procedure Test_Signals is
      A : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
   begin
      A.On_Changed (Changed_Cb'Unrestricted_Access);
      A.On_Value_Changed (Value_Changed_Cb'Unrestricted_Access);

      Changed_Count := 0;
      Value_Changed_Count := 0;
      A.Configure (0.0, 0.0, 100.0, 1.0, 5.0, 0.0);
      Assert_Cmpint_Eq (Changed_Count, 1);
      Assert_Cmpint_Eq (Value_Changed_Count, 0);

      Changed_Count := 0;
      Value_Changed_Count := 0;
      A.Set_Value (50.0);
      A.Set_Lower (20.0);
      A.Set_Upper (75.5);
      A.Set_Step_Increment (2.2);
      A.Set_Page_Increment (1.5);
      A.Set_Page_Size (10.0);
      Assert_Cmpint_Eq (Changed_Count, 5);
      Assert_Cmpint_Eq (Value_Changed_Count, 1);
   end Test_Signals;

   --  Exercise both generated signal overloads that adapt callbacks to a
   --  generic GObject slot, while keeping each emitted signal observable.
   procedure Test_Signal_Slots is
      Emitter : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
      Slot : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (0.0, 0.0, 1.0, 1.0, 1.0, 0.0);
   begin
      Expected_Slot := Get_Object (Slot);
      Changed_Slot_Count := 0;
      Value_Slot_Count := 0;
      Changed_Count := 0;
      Value_Changed_Count := 0;

      --  The same slot object is attached to both signal connections.
      Emitter.On_Changed (Changed_Slot_Cb'Unrestricted_Access, Slot);
      Emitter.On_Value_Changed (Value_Slot_Cb'Unrestricted_Access, Slot);
      Emitter.On_Changed (Changed_Cb'Unrestricted_Access);
      Emitter.On_Value_Changed (Value_Changed_Cb'Unrestricted_Access);

      --  Configure changes only adjustment parameters; Set_Value changes only
      --  the value, so each signal must reach just its matching slot handler.
      Emitter.Configure (0.0, 0.0, 100.0, 1.0, 5.0, 0.0);
      Assert_Cmpint_Eq (Changed_Slot_Count, 1);
      Assert_Cmpint_Eq (Value_Slot_Count, 0);
      Assert_Cmpint_Eq (Changed_Count, 1);
      Assert_Cmpint_Eq (Value_Changed_Count, 0);

      Emitter.Set_Value (10.0);
      Assert_Cmpint_Eq (Changed_Slot_Count, 1);
      Assert_Cmpint_Eq (Value_Slot_Count, 1);
      Assert_Cmpint_Eq (Changed_Count, 1);
      Assert_Cmpint_Eq (Value_Changed_Count, 1);

      --  Exceptions from each callback form must be caught and reported by
      --  its generated marshaller rather than escaping back through GTK.
      Gtkada.Bindings.Set_On_Exception
        (Record_Exception'Unrestricted_Access);
      Exception_Count := 0;
      Raise_Callbacks := True;
      Emitter.Configure (10.0, 0.0, 90.0, 1.0, 5.0, 0.0);
      Emitter.Set_Value (11.0);
      Raise_Callbacks := False;
      Assert_Cmpuint_Eq (Guint (Exception_Count), 4);
      Assert_Cmpfloat_Eq (Emitter.Get_Upper, 90.0);
      Assert_Cmpfloat_Eq (Emitter.Get_Value, 11.0);
   end Test_Signal_Slots;

   procedure Test_Clamp is
      A : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (2.0, 0.0, 100.0, 1.0, 5.0, 10.0);
   begin
      A.Set_Value (-10.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 0.0);

      A.Set_Value (200.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 90.0);

      A.Set_Value (99.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 90.0);

      A.Configure (0.0, 0.0, 10.0, 1.0, 5.0, 10.0);

      A.Set_Value (5.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 0.0);
   end Test_Clamp;

   procedure Test_Clamp_Page is
      A : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (20.0, 0.0, 100.0, 1.0, 5.0, 10.0);
   begin
      A.Clamp_Page (50.0, 55.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 45.0);

      A.Clamp_Page (52.0, 58.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 48.0);

      A.Clamp_Page (48.0, 50.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 48.0);

      A.Clamp_Page (30.0, 50.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 30.0);
   end Test_Clamp_Page;

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/adjustment/basic", Test_Basic'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/adjustment/signals", Test_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/adjustment/signal-slots", Test_Signal_Slots'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/adjustment/clamp", Test_Clamp'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/adjustment/clamp_page", Test_Clamp_Page'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Adjustment;
