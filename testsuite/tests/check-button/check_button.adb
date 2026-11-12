--  Exercise Gtk.Check_Button's constructors, state, child, grouping,
--  accessibility, actionable methods, and both signal callback overloads.
--  GTK's C testsuite has no standalone check-button test to port; the related
--  active-state assertions in testsuite/c_tests/builder.c are covered here.

with Ada.Command_Line;
with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;
with Glib.Test;               use Glib.Test;
with Glib.Variant;            use Glib.Variant;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Check_Button;        use Gtk.Check_Button;
with Gtk.Label;               use Gtk.Label;
with Gtk.Main;
with Gtk.Widget;
with Gtkada.Types;
with Interfaces.C.Strings;
with System;

use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Widget.Gtk_Widget;
use type Glib.Variant.Gvariant;
use type System.Address;

procedure Check_Button is
   Typed_Toggled_Count : Natural := 0;
   Slot_Toggled_Count  : Natural := 0;
   Typed_Activate_Count : Natural := 0;
   Slot_Activate_Count  : Natural := 0;

   --  Activate has no parameters, so this C varargs declaration emits it
   --  directly without relying on keyboard input or a mapped window.
   procedure Emit_Activate
     (Object : System.Address; Name : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure On_Toggled_Typed
     (Self : access Gtk_Check_Button_Record'Class);
   procedure On_Toggled_Slot
     (Self : access GObject_Record'Class);
   procedure On_Activate_Typed
     (Self : access Gtk_Check_Button_Record'Class);
   procedure On_Activate_Slot
     (Self : access GObject_Record'Class);

   procedure Test_Constructors_And_State with Convention => C;
   procedure Test_Children_And_Groups with Convention => C;
   procedure Test_Accessibility with Convention => C;
   procedure Test_Actionable with Convention => C;
   procedure Test_Signals with Convention => C;

   --  Each callback confirms the object reaches the expected typed or
   --  GObject-compatible adapter before recording the signal delivery.
   procedure On_Toggled_Typed
     (Self : access Gtk_Check_Button_Record'Class) is
   begin
      Assert_True (Self /= null);
      Typed_Toggled_Count := Typed_Toggled_Count + 1;
   end On_Toggled_Typed;

   procedure On_Toggled_Slot (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Slot_Toggled_Count := Slot_Toggled_Count + 1;
   end On_Toggled_Slot;

   procedure On_Activate_Typed
     (Self : access Gtk_Check_Button_Record'Class) is
   begin
      Assert_True (Self /= null);
      Typed_Activate_Count := Typed_Activate_Count + 1;
   end On_Activate_Typed;

   procedure On_Activate_Slot (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Slot_Activate_Count := Slot_Activate_Count + 1;
   end On_Activate_Slot;

   --  Both function and procedure constructors preserve their label and
   --  initial inactive state; mnemonic construction accepts underscore text.
   procedure Test_Constructors_And_State is
      Plain_Function : constant Gtk_Check_Button := Gtk_Check_Button_New;
      Label_Function : constant Gtk_Check_Button :=
        Gtk_Check_Button_New_With_Label ("function label");
      Mnemonic_Function : constant Gtk_Check_Button :=
        Gtk_Check_Button_New_With_Mnemonic ("function _mnemonic");
      Plain_Procedure : Gtk_Check_Button;
      Label_Procedure : Gtk_Check_Button;
      Mnemonic_Procedure : Gtk_Check_Button;
   begin
      Gtk_New (Plain_Procedure);
      Gtk_New_With_Label (Label_Procedure, "procedure label");
      Gtk_New_With_Mnemonic (Mnemonic_Procedure, "procedure _mnemonic");
      Ref_Sink (Plain_Function);
      Ref_Sink (Label_Function);
      Ref_Sink (Mnemonic_Function);
      Ref_Sink (Plain_Procedure);
      Ref_Sink (Label_Procedure);
      Ref_Sink (Mnemonic_Procedure);

      Assert_True (Plain_Function.Get_Type = Gtk.Check_Button.Get_Type);
      Assert_Cmpstr_Eq (Plain_Function.Get_Label, "");
      Assert_Cmpstr_Eq (Label_Function.Get_Label, "function label");
      Assert_Cmpstr_Eq
        (Mnemonic_Function.Get_Label, "function _mnemonic");
      Assert_Cmpstr_Eq (Plain_Procedure.Get_Label, "");
      Assert_Cmpstr_Eq (Label_Procedure.Get_Label, "procedure label");
      Assert_Cmpstr_Eq
        (Mnemonic_Procedure.Get_Label, "procedure _mnemonic");
      Assert_False (Plain_Function.Get_Active);
      Assert_False (Plain_Function.Get_Use_Underline);

      --  Active and inconsistent are independent states and both round-trip.
      Plain_Function.Set_Active (True);
      Assert_True (Plain_Function.Get_Active);
      Plain_Function.Set_Inconsistent (True);
      Assert_True (Plain_Function.Get_Inconsistent);
      Plain_Function.Set_Inconsistent (False);
      Assert_False (Plain_Function.Get_Inconsistent);
      Plain_Function.Set_Use_Underline (True);
      Assert_True (Plain_Function.Get_Use_Underline);
      Plain_Function.Set_Use_Underline (False);
      Assert_False (Plain_Function.Get_Use_Underline);

      --  Initialize is documented as a no-op after the native object exists.
      Gtk.Check_Button.Initialize (Plain_Procedure);
      Assert_Cmpstr_Eq (Plain_Procedure.Get_Label, "");

      Unref (Plain_Function);
      Unref (Label_Function);
      Unref (Mnemonic_Function);
      Unref (Plain_Procedure);
      Unref (Label_Procedure);
      Unref (Mnemonic_Procedure);
   end Test_Constructors_And_State;

   --  Child widgets can replace the text label, and grouped check buttons
   --  enforce the documented one-active-button rule.
   procedure Test_Children_And_Groups is
      Button : constant Gtk_Check_Button :=
        Gtk_Check_Button_New_With_Label ("initial label");
      Child : constant Gtk_Label := Gtk_Label_New ("custom child");
      First : constant Gtk_Check_Button := Gtk_Check_Button_New;
      Second : constant Gtk_Check_Button := Gtk_Check_Button_New;
   begin
      Ref_Sink (Button);
      Ref_Sink (First);
      Ref_Sink (Second);
      Assert_True (Button.Get_Child = null);
      Assert_Cmpstr_Eq (Button.Get_Label, "initial label");

      --  The custom child replaces the label; the getter returns that same
      --  widget without transferring its reference to this test.
      Button.Set_Child (Child);
      Assert_True (Button.Get_Child = Gtk.Widget.Gtk_Widget (Child));
      Assert_Cmpstr_Eq (Button.Get_Label, "");
      Button.Set_Child (null);
      Assert_True (Button.Get_Child = null);

      --  Passing an empty string clears the text and exercises the nullable
      --  UTF-8 pointer path in the generated setter.
      Button.Set_Label ("replacement");
      Assert_Cmpstr_Eq (Button.Get_Label, "replacement");
      Button.Set_Label;
      Assert_Cmpstr_Eq (Button.Get_Label, "");
      Assert_True (Button.Get_Child = null);

      --  Selecting the second member clears the first member's active state.
      Second.Set_Group (First);
      First.Set_Active (True);
      Assert_True (First.Get_Active);
      Second.Set_Active (True);
      Assert_False (First.Get_Active);
      Assert_True (Second.Get_Active);

      Unref (Button);
      Unref (First);
      Unref (Second);
   end Test_Children_And_Groups;

   --  Detached widgets have no AT context or screen bounds. A custom child
   --  still appears in the accessible tree, independently of a real display.
   procedure Test_Accessibility is
      Button : constant Gtk_Check_Button := Gtk_Check_Button_New;
      Child : constant Gtk_Label := Gtk_Label_New ("accessible child");
      Parent : Gtk_Accessible;
      Next : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (Button);
      Button.Set_Child (Child);

      Assert_True (Button.Get_Accessible_Role = Accessible_Role_Checkbox);
      Assert_Cmpstr_Eq (Button.Get_Accessible_Id, "");
      Assert_True (Button.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      --  Detached widgets have no parent or following sibling in the
      --  accessible tree, even though the custom child is present.
      Parent := Button.Get_Accessible_Parent;
      Next := Button.Get_Next_Accessible_Sibling;
      Assert_True (Parent = Null_Gtk_Accessible);
      Assert_True (Next = Null_Gtk_Accessible);
      Context := Button.Get_At_Context;
      Assert_True (Context = null);
      Assert_False (Button.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (Button.Get_Platform_State (Accessible_Platform_State_Focused));

      --  The announcement accepts a message and priority without requiring
      --  an available screen reader or an interactive user session.
      --  These valid notifications and resets do not need a screen reader.
      Button.Announce ("check button test", Accessible_Announcement_Priority_Low);
      Button.Reset_Property (Accessible_Property_Label);
      Button.Reset_Relation (Accessible_Relation_Labelled_By);
      Button.Reset_State (Accessible_State_Busy);
      Button.Update_Platform_State (Accessible_Platform_State_Focusable);
      Button.Set_Accessible_Parent
        (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Button.Get_Accessible_Parent = Null_Gtk_Accessible);
      Button.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Button.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Assert_True (Button.Get_Accessible_Role = Accessible_Role_Checkbox);

      Unref (Button);
   end Test_Accessibility;

   --  Action names and GVariant targets use GtkActionable's regular
   --  get/set contract; the detailed form also clears an earlier target.
   procedure Test_Actionable is
      Button : constant Gtk_Check_Button := Gtk_Check_Button_New;
      Target : constant Gvariant := Gvariant_New_String ("checked");
      Read_Back : Gvariant;
   begin
      Ref_Sink (Button);
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "");
      --  An unset actionable target is represented by a null GVariant.
      Read_Back := Button.Get_Action_Target_Value;
      Assert_True (Read_Back = Null_Gvariant);

      Button.Set_Action_Name ("app.check");
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "app.check");
      --  The target's string value survives the transfer through GtkActionable.
      Button.Set_Action_Target_Value (Target);
      Read_Back := Button.Get_Action_Target_Value;
      Assert_True (Read_Back /= Null_Gvariant);
      Assert_Cmpstr_Eq (Get_String (Read_Back), "checked");

      Button.Set_Detailed_Action_Name ("app.ready");
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "app.ready");
      Button.Set_Action_Name;
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "");
      Unref (Button);
   end Test_Actionable;

   --  Toggled is generated by changing active state; activate is emitted
   --  directly to cover both typed and slot marshallers without user input.
   procedure Test_Signals is
      Typed : constant Gtk_Check_Button := Gtk_Check_Button_New;
      Slot : constant Gtk_Check_Button := Gtk_Check_Button_New;
      Name : Interfaces.C.Strings.chars_ptr;
   begin
      Ref_Sink (Typed);
      Ref_Sink (Slot);
      --  Programmatic active changes emit toggled synchronously on both
      --  objects and reach the corresponding callback signatures.
      Typed.On_Toggled (On_Toggled_Typed'Unrestricted_Access);
      Slot.On_Toggled (On_Toggled_Slot'Unrestricted_Access, Slot);
      Typed.Set_Active (True);
      Slot.Set_Active (True);
      Assert_True (Typed_Toggled_Count = 1);
      Assert_True (Slot_Toggled_Count = 1);

      Typed.On_Activate (On_Activate_Typed'Unrestricted_Access);
      Slot.On_Activate (On_Activate_Slot'Unrestricted_Access, Slot);
      Name := Interfaces.C.Strings.New_String ("activate");
      Emit_Activate (Get_Object (Typed), Name);
      Emit_Activate (Get_Object (Slot), Name);
      Interfaces.C.Strings.Free (Name);
      Assert_True (Typed_Activate_Count = 1);
      Assert_True (Slot_Activate_Count = 1);

      Unref (Typed);
      Unref (Slot);
   end Test_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func
     ("/check-button/constructors-state",
      Test_Constructors_And_State'Unrestricted_Access);
   Add_Func
     ("/check-button/children-groups",
      Test_Children_And_Groups'Unrestricted_Access);
   Add_Func
     ("/check-button/accessibility", Test_Accessibility'Unrestricted_Access);
   Add_Func ("/check-button/actionable", Test_Actionable'Unrestricted_Access);
   Add_Func ("/check-button/signals", Test_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Check_Button;
