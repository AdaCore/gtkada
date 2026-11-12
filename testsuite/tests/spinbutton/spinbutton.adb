--  Ada port of testsuite/c_tests/spinbutton.c:
--  test_value_changed, test_adjustment_changed, and test_adjustment_null.
--  The remaining cases cover Gtk.Spin_Button's range, editable, signal,
--  property, and interface methods through their documented behavior.

with Ada.Command_Line;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Test;             use Glib.Test;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Accessible_Range;
with Gtk.Adjustment;        use Gtk.Adjustment;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Cell_Editable;
with Gtk.Constraint_Target;
with Gtk.Editable;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Main;
with Gtk.Orientable;
with Gtk.Spin_Button;       use Gtk.Spin_Button;
with Gtk.Widget;
with Gtkada.Types;          use Gtkada.Types;
with Interfaces.C;
with System;

use type System.Address;
use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Accessible_Range.Gtk_Accessible_Range;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Editable.Gtk_Editable;

procedure Spinbutton is

   --  These imports use GtkAda's signal-emission helpers to exercise signal
   --  adapters whose signals are normally emitted by keyboard input. They
   --  keep the tests independent of physical input devices.
   procedure Emit_Signal
     (Object : System.Address;
      Name   : Chars_Ptr);
   pragma Import (C, Emit_Signal, "ada_g_signal_emit_by_name");

   procedure Emit_Int_Signal
     (Object : System.Address;
      Name   : Chars_Ptr;
      Value  : Gint;
      Extra  : System.Address);
   pragma Import (C, Emit_Int_Signal, "ada_g_signal_emit_by_name_int_ptr");

   --  The standard variadic signal API expects a result pointer after its
   --  signal parameters when emitting a non-void signal.
   procedure Emit_Boolean_Signal
     (Object      : System.Address;
      Name        : Chars_Ptr;
      Return_Value : access Gboolean);
   pragma Import (C, Emit_Boolean_Signal, "g_signal_emit_by_name");

   --  GObject's property notification signature is shared across all
   --  generated objects, so use a small C-convention callback for the port.
   Adjustment_Count : Natural := 0;
   procedure Adjustment_Changed
     (Object : System.Address;
      Param  : System.Address;
      Data   : System.Address);
   pragma Convention (C, Adjustment_Changed);

   function Signal_Connect_Data
     (Object          : System.Address;
      Detailed_Signal : Chars_Ptr;
      Handler         : System.Address;
      Data            : System.Address;
      Notify          : System.Address;
      Flags           : Guint) return Gulong;
   pragma Import (C, Signal_Connect_Data, "g_signal_connect_data");

   procedure Adjustment_Changed
     (Object : System.Address;
      Param  : System.Address;
      Data   : System.Address)
   is
      pragma Unreferenced (Param, Data);
   begin
      Assert_True (Object /= System.Null_Address);
      Adjustment_Count := Adjustment_Count + 1;
   end Adjustment_Changed;

   Changed_Typed : Natural := 0;
   Changed_Slot  : Natural := 0;
   --  Separate counts confirm each connection form receives the same signal.
   Activated_Count : Natural := 0;
   Wrapped_Count   : Natural := 0;
   Input_Typed   : Natural := 0;
   Input_Slot    : Natural := 0;
   Output_Typed  : Natural := 0;
   Output_Slot   : Natural := 0;
   --  The last value lets the keybinding signal test verify enum marshalling.
   Change_Typed  : Natural := 0;
   Change_Slot   : Natural := 0;
   Seen_Scroll   : Gtk_Scroll_Type := Scroll_None;

   --  Typed callbacks exercise the class-specific signal marshallers.
   --  Assertions inside callbacks catch incorrect self-object conversions.
   --  Counters let each test distinguish notifications from no-op updates.
   procedure Value_Changed (Self : access Gtk_Spin_Button_Record'Class) is
   begin
      Changed_Typed := Changed_Typed + 1;
      Assert_True (Self /= null);
   end Value_Changed;

   procedure On_Activated (Self : access Gtk_Spin_Button_Record'Class) is
   begin
      Activated_Count := Activated_Count + 1;
      Assert_True (Self /= null);
   end On_Activated;

   procedure On_Wrapped (Self : access Gtk_Spin_Button_Record'Class) is
   begin
      Wrapped_Count := Wrapped_Count + 1;
      Assert_True (Self /= null);
   end On_Wrapped;

   procedure Change_Value
     (Self   : access Gtk_Spin_Button_Record'Class;
      Scroll : Gtk_Scroll_Type)
   is
   begin
      Change_Typed := Change_Typed + 1;
      Seen_Scroll := Scroll;
      Assert_True (Self /= null);
   end Change_Value;

   function Input_Value
     (Self      : access Gtk_Spin_Button_Record'Class;
      New_Value : out Gdouble) return Gint
   is
   begin
      --  Return an in-range value so GtkSpinButton accepts the parsed input.
      Input_Typed := Input_Typed + 1;
      Assert_True (Self /= null);
      New_Value := 7.5;
      return 1;
   end Input_Value;

   function Format_Output
     (Self : access Gtk_Spin_Button_Record'Class) return Boolean
   is
   begin
      --  TRUE marks the output as handled and gives the emission a result.
      Output_Typed := Output_Typed + 1;
      Assert_True (Self /= null);
      return True;
   end Format_Output;

   --  Slot callbacks cover the generic GObject forms of the generated
   --  marshallers. GtkAda keeps the supplied slot alive for the connection.
   --  Their counters remain independent of the typed callback counters.
   procedure Value_Changed_Slot
     (Self : access GObject_Record'Class)
   is
   begin
      Changed_Slot := Changed_Slot + 1;
      Assert_True (Self /= null);
   end Value_Changed_Slot;

   procedure Activated_Slot (Self : access GObject_Record'Class) is
   begin
      Activated_Count := Activated_Count + 1;
      Assert_True (Self /= null);
   end Activated_Slot;

   procedure Wrapped_Slot (Self : access GObject_Record'Class) is
   begin
      Wrapped_Count := Wrapped_Count + 1;
      Assert_True (Self /= null);
   end Wrapped_Slot;

   procedure Change_Value_Slot
     (Self   : access GObject_Record'Class;
      Scroll : Gtk_Scroll_Type)
   is
   begin
      Change_Slot := Change_Slot + 1;
      Seen_Scroll := Scroll;
      Assert_True (Self /= null);
   end Change_Value_Slot;

   function Input_Value_Slot
     (Self      : access GObject_Record'Class;
      New_Value : out Gdouble) return Gint
   is
   begin
      Input_Slot := Input_Slot + 1;
      Assert_True (Self /= null);
      New_Value := 8.5;
      return 1;
   end Input_Value_Slot;

   function Format_Output_Slot
     (Self : access GObject_Record'Class) return Boolean
   is
   begin
      Output_Slot := Output_Slot + 1;
      Assert_True (Self /= null);
      return True;
   end Format_Output_Slot;

   procedure Test_Value_Changed
   with Convention => C;
   procedure Test_Adjustment_Changed
   with Convention => C;
   procedure Test_Adjustment_Null
   with Convention => C;
   procedure Test_Constructors_And_Range
   with Convention => C;
   procedure Test_Properties_And_Spin
   with Convention => C;
   procedure Test_Editable
   with Convention => C;
   procedure Test_Accessibility_And_Interfaces
   with Convention => C;
   procedure Test_Signals
   with Convention => C;

   --  Port of the C value-changed test, including its no-op assignment.
   procedure Test_Value_Changed is
      Spin : constant Gtk_Spin_Button :=
        Gtk_Spin_Button_New_With_Range (0.0, 10.0, 1.0);
      Adj  : Gtk_Adjustment;
   begin
      Ref_Sink (Spin);
      --  The repeated value verifies that an unchanged assignment is silent.
      Changed_Typed := 0;
      Spin.On_Value_Changed (Value_Changed'Unrestricted_Access);
      Spin.Set_Value (1.0);
      Assert_Cmpuint_Eq (Guint (Changed_Typed), 1);
      Spin.Set_Value (2.0);
      Assert_Cmpuint_Eq (Guint (Changed_Typed), 2);
      Spin.Set_Value (2.0);
      Assert_Cmpuint_Eq (Guint (Changed_Typed), 2);
      Spin.Spin (Spin_Step_Forward, 0.5);
      Assert_Cmpuint_Eq (Guint (Changed_Typed), 3);
      Spin.Configure (null, 1.0, 0);
      --  Changing the adjustment's value must notify the spin button too.
      Assert_Cmpuint_Eq (Guint (Changed_Typed), 4);
      Adj := Spin.Get_Adjustment;
      Adj.Set_Value (0.0);
      Assert_Cmpuint_Eq (Guint (Changed_Typed), 5);
      Unref (Spin);
   end Test_Value_Changed;

   --  Port of the C adjustment notification test and its distinct cases.
   procedure Test_Adjustment_Changed is
      Spin : constant Gtk_Spin_Button :=
        Gtk_Spin_Button_New_With_Range (0.0, 10.0, 1.0);
      Adj  : Gtk_Adjustment;
      Signal_Name : Chars_Ptr;
      Handler_Id : Gulong;
   begin
      Ref_Sink (Spin);
      --  The unit has no generated typed connector for GObject's notify signal.
      Adjustment_Count := 0;
      Signal_Name := New_String ("notify::adjustment");
      Handler_Id := Signal_Connect_Data
        (Spin.Get_Object, Signal_Name,
         Adjustment_Changed'Address, System.Null_Address,
         System.Null_Address, 0);
      Free (Signal_Name);
      Assert_True (Handler_Id /= 0);
      Adj := Gtk_Adjustment_New (50.0, 0.0, 100.0, 1.0, 1.0);
      Spin.Configure (Adj, 1.0, 0);
      Assert_Cmpuint_Eq (Guint (Adjustment_Count), 1);
      Adj := Gtk_Adjustment_New (51.0, 1.0, 101.0, 1.0, 1.0);
      Spin.Set_Adjustment (Adj);
      Assert_Cmpuint_Eq (Guint (Adjustment_Count), 2);
      Spin.Set_Range (2.0, 102.0);
      Assert_Cmpuint_Eq (Guint (Adjustment_Count), 2);
      Unref (Spin);
   end Test_Adjustment_Changed;

   --  Port the nullable Configure check from the C test. The Set_Adjustment
   --  NULL check is omitted because GtkAda declares that argument not null.
   procedure Test_Adjustment_Null is
      Spin : constant Gtk_Spin_Button :=
        Gtk_Spin_Button_New_With_Range (0.0, 10.0, 1.0);
      Before : Gtk_Adjustment;
   begin
      Ref_Sink (Spin);
      Before := Spin.Get_Adjustment;
      Spin.Configure (null, 1.0, 0);
      Assert_True (Before.Get_Object = Spin.Get_Adjustment.Get_Object);
      Unref (Spin);
   end Test_Adjustment_Null;

   --  Exercise all four constructor forms and both initialize guards.
   --  The checks also confirm that supplied adjustments retain their value.
   procedure Test_Constructors_And_Range is
      Adj : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (3.0, 0.0, 12.0, 0.5, 2.0);
      By_Procedure : Gtk_Spin_Button;
      By_Range_Procedure : Gtk_Spin_Button;
      By_Function : Gtk_Spin_Button;
      By_Range_Function : Gtk_Spin_Button;
      Initialized : constant Gtk_Spin_Button :=
        new Gtk_Spin_Button_Record;
   begin
      Gtk_New (By_Procedure, Adj, 2.0, 1);
      Gtk_New_With_Range (By_Range_Procedure, -2.0, 8.0, 0.25);
      By_Function := Gtk_Spin_Button_New (Adj, 1.5, 2);
      By_Range_Function := Gtk_Spin_Button_New_With_Range (1.0, 9.0, 0.5);
      Initialized.Initialize (Adj, 0.5, 1);
      --  Reinitialization must preserve the object created by the first call.
      Initialized.Initialize (null, 9.0, 9);
      Initialized.Initialize_With_Range (-99.0, 99.0, 1.0);
      --  Both Initialize variants ignore requests after object creation.
      Initialized.Initialize_With_Range (0.0, 1.0, 1.0);
      Initialized.Initialize_With_Range (0.0, 1.0, 1.0);

      Ref_Sink (By_Procedure);
      Ref_Sink (By_Range_Procedure);
      Ref_Sink (By_Function);
      Ref_Sink (By_Range_Function);
      Ref_Sink (Initialized);
      Assert_Cmpfloat_Eq (By_Procedure.Get_Value, 3.0);
      Assert_Cmpfloat_Eq (By_Range_Procedure.Get_Value, -2.0);
      Assert_Cmpfloat_Eq (By_Function.Get_Value, 3.0);
      Assert_Cmpfloat_Eq (By_Range_Function.Get_Value, 1.0);
      Assert_Cmpuint_Eq (By_Procedure.Get_Digits, 1);
      Assert_Cmpfloat_Eq (By_Procedure.Get_Climb_Rate, 2.0);
      Unref (Initialized);
      Unref (By_Range_Function);
      Unref (By_Function);
      Unref (By_Range_Procedure);
      Unref (By_Procedure);
   end Test_Constructors_And_Range;

   --  Check every spin-specific getter and setter, plus the seven spin modes.
   --  Property tests use non-default settings and verify each observable value.
   procedure Test_Properties_And_Spin is
      Spin : constant Gtk_Spin_Button :=
        Gtk_Spin_Button_New_With_Range (0.0, 20.0, 1.0);
      Step, Page : Gdouble;
      Min, Max : Gdouble;
   begin
      Ref_Sink (Spin);
      Assert_False (Spin.Get_Activates_Default);
      Spin.Set_Activates_Default (True);
      Assert_True (Spin.Get_Activates_Default);
      Spin.Set_Climb_Rate (3.5);
      Assert_Cmpfloat_Eq (Spin.Get_Climb_Rate, 3.5);
      Spin.Set_Digits (2);
      Assert_Cmpuint_Eq (Spin.Get_Digits, 2);
      Spin.Set_Increments (0.25, 4.0);
      --  The step and page increments differ to make each spin mode visible.
      Spin.Get_Increments (Step, Page);
      Assert_Cmpfloat_Eq (Step, 0.25);
      Assert_Cmpfloat_Eq (Page, 4.0);
      Assert_True (Spin.Get_Numeric);
      Spin.Set_Numeric (False);
      Assert_False (Spin.Get_Numeric);
      Spin.Set_Numeric (True);
      Spin.Set_Range (-3.0, 30.0);
      Spin.Get_Range (Min, Max);
      Assert_Cmpfloat_Eq (Min, -3.0);
      Assert_Cmpfloat_Eq (Max, 30.0);
      Spin.Set_Snap_To_Ticks (True);
      Assert_True (Spin.Get_Snap_To_Ticks);
      Spin.Set_Snap_To_Ticks (False);
      Assert_False (Spin.Get_Snap_To_Ticks);
      --  Exercise both policies because they govern invalid typed input.
      Spin.Set_Update_Policy (Update_If_Valid);
      Assert_True (Spin.Get_Update_Policy = Update_If_Valid);
      Spin.Set_Update_Policy (Update_Always);
      Assert_True (Spin.Get_Update_Policy = Update_Always);
      Spin.Set_Wrap (True);
      Assert_True (Spin.Get_Wrap);
      Spin.Set_Wrap (False);
      Assert_False (Spin.Get_Wrap);

      Spin.Set_Value (4.0);
      Assert_Cmpfloat_Eq (Spin.Get_Value, 4.0);
      Assert_Cmpint_Eq (Spin.Get_Value_As_Int, 4);
      Spin.Spin (Spin_Step_Forward, 0.0);
      Assert_Cmpfloat_Eq (Spin.Get_Value, 4.25);
      Spin.Spin (Spin_Step_Backward, 0.0);
      Assert_Cmpfloat_Eq (Spin.Get_Value, 4.0);
      Spin.Spin (Spin_Page_Forward, 0.0);
      Assert_Cmpfloat_Eq (Spin.Get_Value, 8.0);
      Spin.Spin (Spin_Page_Backward, 0.0);
      Assert_Cmpfloat_Eq (Spin.Get_Value, 4.0);
      --  Home and End use the current adjustment limits, not the step size.
      Spin.Spin (Spin_Home, 0.0);
      Assert_Cmpfloat_Eq (Spin.Get_Value, -3.0);
      Spin.Spin (Spin_End, 0.0);
      Assert_Cmpfloat_Eq (Spin.Get_Value, 30.0);
      Spin.Set_Value (0.0);
      Spin.Spin (Spin_User_Defined, 2.5);
      Assert_Cmpfloat_Eq (Spin.Get_Value, 2.5);

      --  The enum-backed properties are declared by this package as well.
      Set_Property (Spin, Gtk.Spin_Button.Activates_Default_Property, False);
      Assert_False (Get_Property (Spin, Gtk.Spin_Button.Activates_Default_Property));
      Set_Property (Spin, Gtk.Spin_Button.Climb_Rate_Property, 4.0);
      Assert_Cmpfloat_Eq (Get_Property (Spin, Gtk.Spin_Button.Climb_Rate_Property), 4.0);
      Set_Property (Spin, Gtk.Spin_Button.Numeric_Property, True);
      Assert_True (Get_Property (Spin, Gtk.Spin_Button.Numeric_Property));
      Set_Property (Spin, Gtk.Spin_Button.Snap_To_Ticks_Property, True);
      Assert_True (Get_Property (Spin, Gtk.Spin_Button.Snap_To_Ticks_Property));
      Set_Property (Spin, Gtk.Spin_Button.The_Digits_Property, 3);
      Assert_Cmpuint_Eq (Get_Property (Spin, Gtk.Spin_Button.The_Digits_Property), 3);
      Set_Property (Spin, Gtk.Spin_Button.Update_Policy_Property, Update_If_Valid);
      Assert_True (Get_Property (Spin, Gtk.Spin_Button.Update_Policy_Property) = Update_If_Valid);
      Set_Property (Spin, Gtk.Spin_Button.Value_Property, 6.0);
      Assert_Cmpfloat_Eq (Get_Property (Spin, Gtk.Spin_Button.Value_Property), 6.0);
      Set_Property (Spin, Gtk.Spin_Button.Wrap_Property, True);
      Assert_True (Get_Property (Spin, Gtk.Spin_Button.Wrap_Property));
      Unref (Spin);
   end Test_Properties_And_Spin;

   --  GtkSpinButton implements GtkEditable directly; exercise text editing
   --  through its delegate methods and check the resulting string and cursor.
   procedure Test_Editable is
      Spin : constant Gtk_Spin_Button :=
        Gtk_Spin_Button_New_With_Range (0.0, 10000.0, 1.0);
      Position : Gint := 1;
      Start_Pos, End_Pos : Gint;
      Has_Selection : Boolean;
      Delegate : Gtk.Editable.Gtk_Editable;
   begin
      Ref_Sink (Spin);
      Delegate := Spin.Get_Delegate;
      Assert_True (Delegate /= Gtk.Editable.Null_Gtk_Editable);
      Assert_True (Spin.Get_Editable);
      Spin.Set_Editable (False);
      Assert_False (Spin.Get_Editable);
      Spin.Set_Editable (True);
      Assert_True (Spin.Get_Editable);
      Spin.Set_Enable_Undo (False);
      Assert_False (Spin.Get_Enable_Undo);
      Spin.Set_Enable_Undo (True);
      Assert_True (Spin.Get_Enable_Undo);
      Spin.Set_Alignment (Interfaces.C.C_float (0.25));
      Assert_Cmpfloat_Eq (Gdouble (Spin.Get_Alignment), 0.25);
      Spin.Set_Max_Width_Chars (8);
      Assert_Cmpint_Eq (Spin.Get_Max_Width_Chars, 8);
      Spin.Set_Width_Chars (12);
      Assert_Cmpint_Eq (Spin.Get_Width_Chars, 12);

      Spin.Set_Text ("1234");
      --  Numeric text stays valid while the selection and insertion edit it.
      Assert_Cmpstr_Eq (Spin.Get_Text, "1234");
      Assert_Cmpstr_Eq (Spin.Get_Chars (1, 3), "23");
      Spin.Set_Position (2);
      Assert_Cmpint_Eq (Spin.Get_Position, 2);
      Spin.Select_Region (1, 3);
      --  Selection positions are character offsets and the end is exclusive.
      Spin.Get_Selection_Bounds (Start_Pos, End_Pos, Has_Selection);
      Assert_True (Has_Selection);
      Assert_Cmpint_Eq (Start_Pos, 1);
      Assert_Cmpint_Eq (End_Pos, 3);
      Spin.Delete_Selection;
      --  Removing the selected digits still leaves a value within the range.
      Assert_Cmpstr_Eq (Spin.Get_Text, "14");
      Spin.Set_Text ("1234");
      Spin.Insert_Text ("2", 1, Position);
      Assert_Cmpstr_Eq (Spin.Get_Text, "12234");
      Assert_Cmpint_Eq (Position, 2);
      Spin.Delete_Text (1, 2);
      Assert_Cmpstr_Eq (Spin.Get_Text, "1234");

      Spin.Set_Orientation (Orientation_Vertical);
      --  GtkOrientable exposes a regular property-like getter/setter pair.
      Assert_True (Spin.Get_Orientation = Orientation_Vertical);
      Spin.Set_Orientation (Orientation_Horizontal);
      Assert_True (Spin.Get_Orientation = Orientation_Horizontal);
      Unref (Spin);
   end Test_Editable;

   --  Accessible operations do not require a real display server. Bounds are
   --  absent for an unparented widget, while the role is always spin-button.
   --  Interface conversions ensure every interface declared by the binding works.
   procedure Test_Accessibility_And_Interfaces is
      Spin : constant Gtk_Spin_Button :=
        Gtk_Spin_Button_New_With_Range (0.0, 10.0, 1.0);
      Other : constant Gtk_Spin_Button :=
        Gtk_Spin_Button_New_With_Range (0.0, 10.0, 1.0);
      Parent : Gtk.Accessible.Gtk_Accessible;
      Next   : Gtk.Accessible.Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
      Has_Bounds : Boolean;
   begin
      Ref_Sink (Spin);
      Ref_Sink (Other);
      --  An unrooted widget has no AT context or screen-coordinate bounds.
      Assert_True (Spin.Get_Accessible_Role = Accessible_Role_Spin_Button);
      Assert_Cmpstr_Eq (Spin.Get_Accessible_Id, "");
      Context := Spin.Get_At_Context;
      Assert_True (Context = null);
      Has_Bounds := Spin.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Parent := Spin.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Next := Spin.Get_Next_Accessible_Sibling;
      Assert_True (Next = Null_Gtk_Accessible);
      --  GTK exposes the internal editable delegate as the accessible child.
      Assert_True (Spin.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      Assert_False
        (Spin.Get_Platform_State (Accessible_Platform_State_Focused));
      Assert_False
        (Spin.Delegate_Get_Accessible_Platform_State
           (Accessible_Platform_State_Focused));
      Spin.Announce ("spin button test", Accessible_Announcement_Priority_Low);
      Spin.Reset_Property (Accessible_Property_Label);
      Spin.Reset_Relation (Accessible_Relation_Labelled_By);
      Spin.Reset_State (Accessible_State_Busy);
      Spin.Update_Platform_State (Accessible_Platform_State_Focused);
      --  Detached tree updates need not appear until a parent is established.
      Spin.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Spin.Get_Accessible_Parent = Null_Gtk_Accessible);
      Spin.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Spin.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Each conversion pair in Gtk.Spin_Button's interface declarations is
      --  checked by converting to the interface and back to the same object.
      --  The resulting access values must still identify the original widget.
      declare
         A : constant Gtk.Accessible.Gtk_Accessible := +Spin;
         S : constant Gtk_Spin_Button := -A;
      begin
         Assert_True (S.Get_Object = Spin.Get_Object);
      end;
      declare
         A : constant Gtk.Accessible_Range.Gtk_Accessible_Range := +Spin;
         S : constant Gtk_Spin_Button := -A;
      begin
         Assert_True (S.Get_Object = Spin.Get_Object);
         --  The interface's own unary plus is an identity conversion.
         Assert_True (Gtk.Accessible_Range."+" (A) = A);
      end;
      declare
         A : constant Gtk.Buildable.Gtk_Buildable := +Spin;
         S : constant Gtk_Spin_Button := -A;
      begin
         Assert_True (S.Get_Object = Spin.Get_Object);
      end;
      declare
         A : constant Gtk.Cell_Editable.Gtk_Cell_Editable := +Spin;
         S : constant Gtk_Spin_Button := -A;
      begin
         Assert_True (S.Get_Object = Spin.Get_Object);
      end;
      declare
         A : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +Spin;
         S : constant Gtk_Spin_Button := -A;
      begin
         Assert_True (S.Get_Object = Spin.Get_Object);
      end;
      declare
         A : constant Gtk.Editable.Gtk_Editable := +Spin;
         S : constant Gtk_Spin_Button := -A;
      begin
         Assert_True (S.Get_Object = Spin.Get_Object);
      end;
      declare
         A : constant Gtk.Orientable.Gtk_Orientable := +Spin;
         S : constant Gtk_Spin_Button := -A;
      begin
         Assert_True (S.Get_Object = Spin.Get_Object);
         --  Gtk.Orientable's own unary plus preserves an interface value.
         declare
            Same_Orientable : constant Gtk.Orientable.Gtk_Orientable :=
              Gtk.Orientable."+" (A);
         begin
            Assert_True
              (Gtk.Orientable.Get_Orientation (Same_Orientable) =
                 Gtk.Orientable.Get_Orientation (A));
         end;
      end;

      Unref (Other);
      Unref (Spin);
   end Test_Accessibility_And_Interfaces;

   --  Exercise typed and generic signal connections and each generated
   --  marshaller. Explicit emission makes keyboard-only signals deterministic.
   --  The callbacks assert their arguments and record how often each ran.
   procedure Test_Signals is
      Spin : constant Gtk_Spin_Button :=
        Gtk_Spin_Button_New_With_Range (0.0, 2.0, 1.0);
      Slot : constant Gtk_Spin_Button :=
        Gtk_Spin_Button_New_With_Range (0.0, 2.0, 1.0);
      --  Separate output widget avoids its first handler stopping emission.
      Generic_Output : constant Gtk_Spin_Button :=
        Gtk_Spin_Button_New_With_Range (0.0, 2.0, 1.0);
      Signal_Name : Chars_Ptr;
   begin
      Ref_Sink (Spin);
      Ref_Sink (Slot);
      Ref_Sink (Generic_Output);
      Changed_Typed := 0;
      Changed_Slot := 0;
      Activated_Count := 0;
      Input_Typed := 0;
      Input_Slot := 0;
      Output_Typed := 0;
      Output_Slot := 0;
      Change_Typed := 0;
      Change_Slot := 0;
      Wrapped_Count := 0;

      Spin.On_Activate (On_Activated'Unrestricted_Access);
      Spin.On_Activate (Activated_Slot'Unrestricted_Access, Slot);
      Spin.On_Input (Input_Value'Unrestricted_Access);
      Spin.On_Input (Input_Value_Slot'Unrestricted_Access, Slot);
      Spin.On_Change_Value (Change_Value'Unrestricted_Access);
      Spin.On_Change_Value (Change_Value_Slot'Unrestricted_Access, Slot);
      Spin.On_Output (Format_Output'Unrestricted_Access);
      Generic_Output.On_Output (Format_Output_Slot'Unrestricted_Access, Slot);
      Spin.On_Value_Changed (Value_Changed'Unrestricted_Access);
      Spin.On_Value_Changed (Value_Changed_Slot'Unrestricted_Access, Slot);
      Spin.On_Wrapped (On_Wrapped'Unrestricted_Access);
      Spin.On_Wrapped (Wrapped_Slot'Unrestricted_Access, Slot);

      --  Activate has no return value, so its simple emission helper suffices.
      Signal_Name := New_String ("activate");
      Emit_Signal (Spin.Get_Object, Signal_Name);
      Free (Signal_Name);
      Assert_Cmpuint_Eq (Guint (Activated_Count), 2);

      --  Supply the return storage required by this boolean-valued signal.
      declare
         Return_Value : aliased Gboolean := 0;
      begin
         Signal_Name := New_String ("output");
         Emit_Boolean_Signal
           (Spin.Get_Object, Signal_Name, Return_Value'Access);
         Free (Signal_Name);
         Assert_True (Return_Value /= 0);
      end;
      Assert_Cmpuint_Eq (Guint (Output_Typed), 1);
      declare
         Return_Value : aliased Gboolean := 0;
      begin
         Signal_Name := New_String ("output");
         Emit_Boolean_Signal
           (Generic_Output.Get_Object, Signal_Name, Return_Value'Access);
         Free (Signal_Name);
         Assert_True (Return_Value /= 0);
      end;
      Assert_Cmpuint_Eq (Guint (Output_Slot), 1);

      --  The keybinding signal carries a GtkScrollType argument. Emitting it
      --  directly avoids synthesizing a keyboard event while testing both
      --  typed and slot-based marshallers.
      Signal_Name := New_String ("change-value");
      Emit_Int_Signal
        (Spin.Get_Object, Signal_Name,
         Gint (Gtk_Scroll_Type'Pos (Scroll_Step_Forward)), System.Null_Address);
      Free (Signal_Name);
      Assert_Cmpuint_Gt (Guint (Change_Typed), 0);
      Assert_Cmpuint_Gt (Guint (Change_Slot), 0);
      Assert_True (Seen_Scroll = Scroll_Step_Forward);

      Spin.Set_Value (1.0);
      Assert_Cmpuint_Gt (Guint (Changed_Typed), 0);
      Assert_Cmpuint_Gt (Guint (Changed_Slot), 0);
      Spin.Set_Text ("1");
      --  Update emits input and commits the converted value to the adjustment.
      Spin.Update;
      Assert_Cmpuint_Gt (Guint (Input_Typed), 0);
      Assert_Cmpuint_Gt (Guint (Input_Slot), 0);
      Assert_Cmpfloat_Eq (Spin.Get_Value, 2.0);

      --  Crossing the upper endpoint verifies the wrapped signal and its
      --  class-specific and GObject callback variants.
      Spin.Set_Wrap (True);
      Spin.Set_Value (2.0);
      --  A forward step at the upper endpoint wraps to the lower endpoint.
      Spin.Spin (Spin_Step_Forward, 0.0);
      Assert_Cmpuint_Gt (Guint (Wrapped_Count), 0);
      Assert_Cmpuint_Gt (Guint (Changed_Typed), 0);
      Assert_Cmpuint_Gt (Guint (Changed_Slot), 0);
      Unref (Generic_Output);
      Unref (Slot);
      Unref (Spin);
   end Test_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/spinbutton/value-changed", Test_Value_Changed'Unrestricted_Access);
   Add_Func ("/spinbutton/adjustment-changed", Test_Adjustment_Changed'Unrestricted_Access);
   Add_Func ("/spinbutton/adjustment-null", Test_Adjustment_Null'Unrestricted_Access);
   Add_Func ("/spinbutton/constructors-range", Test_Constructors_And_Range'Unrestricted_Access);
   Add_Func ("/spinbutton/properties-spin", Test_Properties_And_Spin'Unrestricted_Access);
   Add_Func ("/spinbutton/editable", Test_Editable'Unrestricted_Access);
   Add_Func ("/spinbutton/accessibility-interfaces", Test_Accessibility_And_Interfaces'Unrestricted_Access);
   Add_Func ("/spinbutton/signals", Test_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Spinbutton;
