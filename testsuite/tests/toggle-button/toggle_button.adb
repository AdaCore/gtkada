--  Exercise Gtk.Toggle_Button's constructors, grouping, accessibility,
--  actionable interface, and typed and slot signal callbacks.
--  GTK's C testsuite has no standalone toggle-button tests to port.

with Ada.Command_Line;
with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;
with Glib.Test;               use Glib.Test;
with Glib.Variant;            use Glib.Variant;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Main;
with Gtk.Toggle_Button;       use Gtk.Toggle_Button;
with Gtk.Widget;
with Gtkada.Types;
with System;

use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Widget.Gtk_Widget;
use type Glib.Variant.Gvariant;

procedure Toggle_Button is
   Typed_Toggled_Count : Natural := 0;
   Slot_Toggled_Count  : Natural := 0;

   procedure On_Toggled_Typed
     (Self : access Gtk_Toggle_Button_Record'Class);
   procedure On_Toggled_Slot
     (Self : access GObject_Record'Class);

   procedure Test_Constructors_And_State with Convention => C;
   procedure Test_Groups with Convention => C;
   procedure Test_Accessibility with Convention => C;
   procedure Test_Actionable with Convention => C;
   procedure Test_Signals with Convention => C;

   --  Both callback adapters receive a live widget before incrementing their
   --  separate counters, so the assertions also verify callback conversion.
   procedure On_Toggled_Typed
     (Self : access Gtk_Toggle_Button_Record'Class) is
   begin
      Assert_True (Self /= null);
      Typed_Toggled_Count := Typed_Toggled_Count + 1;
   end On_Toggled_Typed;

   procedure On_Toggled_Slot (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Slot_Toggled_Count := Slot_Toggled_Count + 1;
   end On_Toggled_Slot;

   --  Function and procedure constructors preserve labels and start inactive;
   --  repeated initialization is documented as a no-op on created objects.
   --  The mnemonic constructor also accepts underscore-marked label text.
   --  Active state is toggled both on and off and checked after each change.
   procedure Test_Constructors_And_State is
      Plain_Function : constant Gtk_Toggle_Button := Gtk_Toggle_Button_New;
      Label_Function : constant Gtk_Toggle_Button :=
        Gtk_Toggle_Button_New_With_Label ("function label");
      Mnemonic_Function : constant Gtk_Toggle_Button :=
        Gtk_Toggle_Button_New_With_Mnemonic ("function _mnemonic");
      Plain_Procedure : Gtk_Toggle_Button;
      Label_Procedure : Gtk_Toggle_Button;
      Mnemonic_Procedure : Gtk_Toggle_Button;
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

      Assert_True (Plain_Function.Get_Type = Gtk.Toggle_Button.Get_Type);
      Assert_Cmpstr_Eq (Plain_Function.Get_Label, "");
      Assert_Cmpstr_Eq (Label_Function.Get_Label, "function label");
      Assert_Cmpstr_Eq
        (Mnemonic_Function.Get_Label, "function _mnemonic");
      Assert_Cmpstr_Eq (Plain_Procedure.Get_Label, "");
      Assert_Cmpstr_Eq (Label_Procedure.Get_Label, "procedure label");
      Assert_Cmpstr_Eq
        (Mnemonic_Procedure.Get_Label, "procedure _mnemonic");
      Assert_False (Plain_Function.Get_Active);

      --  Reinitializing does not replace the native widget or its label.
      Gtk.Toggle_Button.Initialize (Plain_Procedure);
      Gtk.Toggle_Button.Initialize_With_Label
        (Label_Procedure, "ignored replacement");
      Gtk.Toggle_Button.Initialize_With_Mnemonic
        (Mnemonic_Procedure, "ignored replacement");
      Assert_Cmpstr_Eq (Label_Procedure.Get_Label, "procedure label");
      Assert_Cmpstr_Eq
        (Mnemonic_Procedure.Get_Label, "procedure _mnemonic");

      --  Active state changes round-trip through the toggle button API.
      Plain_Function.Set_Active (True);
      Assert_True (Plain_Function.Get_Active);
      Plain_Function.Set_Active (False);
      Assert_False (Plain_Function.Get_Active);

      Unref (Plain_Function);
      Unref (Label_Function);
      Unref (Mnemonic_Function);
      Unref (Plain_Procedure);
      Unref (Label_Procedure);
      Unref (Mnemonic_Procedure);
   end Test_Constructors_And_State;

   --  Grouped toggle buttons allow only one active member at a time.
   --  Activating the second member must clear the first member immediately.
   --  Turning off the active member leaves the group with no active button.
   procedure Test_Groups is
      First : constant Gtk_Toggle_Button := Gtk_Toggle_Button_New;
      Second : constant Gtk_Toggle_Button := Gtk_Toggle_Button_New;
   begin
      Ref_Sink (First);
      Ref_Sink (Second);
      Second.Set_Group (First);
      First.Set_Active (True);
      Assert_True (First.Get_Active);
      Second.Set_Active (True);
      Assert_False (First.Get_Active);
      Assert_True (Second.Get_Active);
      Second.Set_Active (False);
      Assert_False (Second.Get_Active);
      Unref (First);
      Unref (Second);
   end Test_Groups;

   --  Detached toggle buttons have no parent, context, or screen bounds.
   --  Their accessibility notifications can still be queried and reset.
   --  The test checks the role, empty child tree, and unset platform focus.
   procedure Test_Accessibility is
      Button : constant Gtk_Toggle_Button := Gtk_Toggle_Button_New;
      Parent : Gtk_Accessible;
      Next : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (Button);
      Assert_True (Button.Get_Accessible_Role = Accessible_Role_Toggle_Button);
      Assert_Cmpstr_Eq (Button.Get_Accessible_Id, "");
      Assert_True (Button.Get_First_Accessible_Child = Null_Gtk_Accessible);
      Parent := Button.Get_Accessible_Parent;
      Next := Button.Get_Next_Accessible_Sibling;
      Assert_True (Parent = Null_Gtk_Accessible);
      Assert_True (Next = Null_Gtk_Accessible);
      Context := Button.Get_At_Context;
      Assert_True (Context = null);
      Assert_False (Button.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (Button.Get_Platform_State (Accessible_Platform_State_Focused));

      --  These notifications are valid for an unattached widget and do not
      --  require a running screen reader or a mapped display window.
      Button.Announce
        ("toggle button test", Accessible_Announcement_Priority_Low);
      Button.Reset_Property (Accessible_Property_Label);
      Button.Reset_Relation (Accessible_Relation_Labelled_By);
      Button.Reset_State (Accessible_State_Busy);
      Button.Update_Platform_State (Accessible_Platform_State_Focusable);
      Button.Set_Accessible_Parent
        (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Button.Get_Accessible_Parent = Null_Gtk_Accessible);
      Button.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Button.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Assert_True (Button.Get_Accessible_Role = Accessible_Role_Toggle_Button);
      Unref (Button);
   end Test_Accessibility;

   --  Action names and GVariant targets round-trip through GtkActionable;
   --  the detailed name and empty name cover both string pointer paths.
   --  It also checks nullable target values before and after setting a target.
   --  The read-back string confirms the target content, not just non-nullness.
   procedure Test_Actionable is
      Button : constant Gtk_Toggle_Button := Gtk_Toggle_Button_New;
      Target : constant Gvariant := Gvariant_New_String ("selected");
      Read_Back : Gvariant;
   begin
      Ref_Sink (Button);
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "");
      Read_Back := Button.Get_Action_Target_Value;
      Assert_True (Read_Back = Null_Gvariant);

      Button.Set_Action_Name ("app.toggle");
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "app.toggle");
      Button.Set_Action_Target_Value (Target);
      Read_Back := Button.Get_Action_Target_Value;
      Assert_True (Read_Back /= Null_Gvariant);
      Assert_Cmpstr_Eq (Get_String (Read_Back), "selected");

      Button.Set_Detailed_Action_Name ("app.chosen");
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "app.chosen");
      Button.Set_Action_Name;
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "");
      Button.Set_Action_Target_Value (Null_Gvariant);
      Assert_True (Button.Get_Action_Target_Value = Null_Gvariant);
      Unref (Button);
   end Test_Actionable;

   --  State changes emit toggled synchronously. The deprecated Toggled
   --  method is also exercised to verify it emits the same signal.
   --  Separate typed and GObject slot callbacks check both marshaller forms.
   procedure Test_Signals is
      Typed : constant Gtk_Toggle_Button := Gtk_Toggle_Button_New;
      Slot : constant Gtk_Toggle_Button := Gtk_Toggle_Button_New;
   begin
      Ref_Sink (Typed);
      Ref_Sink (Slot);
      Typed.On_Toggled (On_Toggled_Typed'Unrestricted_Access);
      Slot.On_Toggled (On_Toggled_Slot'Unrestricted_Access, Slot);
      Typed.Set_Active (True);
      Slot.Set_Active (True);
      Assert_True (Typed_Toggled_Count = 1);
      Assert_True (Slot_Toggled_Count = 1);

      Typed.Toggled;
      Assert_True (Typed_Toggled_Count = 2);
      Unref (Typed);
      Unref (Slot);
   end Test_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func
     ("/toggle-button/constructors-state",
      Test_Constructors_And_State'Unrestricted_Access);
   Add_Func ("/toggle-button/groups", Test_Groups'Unrestricted_Access);
   Add_Func
     ("/toggle-button/accessibility", Test_Accessibility'Unrestricted_Access);
   Add_Func ("/toggle-button/actionable", Test_Actionable'Unrestricted_Access);
   Add_Func ("/toggle-button/signals", Test_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Toggle_Button;
