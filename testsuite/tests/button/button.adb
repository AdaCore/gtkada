--  Exercise Gtk.Button constructors, content properties, accessibility,
--  GtkActionable methods, and both signal callback overloads.
--  GTK's C testsuite has no standalone GtkButton API test to port; its
--  action.c cases use buttons as action-group targets without checking the
--  Gtk.Button methods exercised here.

with Ada.Command_Line;
with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;
with Glib.Test;               use Glib.Test;
with Glib.Variant;            use Glib.Variant;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Button;               use Gtk.Button;
with Gtk.Label;                use Gtk.Label;
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

procedure Button is
   Typed_Clicked_Count  : Natural := 0;
   Slot_Clicked_Count   : Natural := 0;
   Typed_Activate_Count : Natural := 0;
   Slot_Activate_Count  : Natural := 0;

   --  Emit button signals directly so no pointer, keyboard, or mapped-window
   --  interaction is needed. This C varargs declaration has two fixed args.
   procedure Emit_Signal
     (Object : System.Address; Name : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure On_Clicked_Typed (Self : access Gtk_Button_Record'Class);
   procedure On_Clicked_Slot (Self : access GObject_Record'Class);
   procedure On_Activate_Typed (Self : access Gtk_Button_Record'Class);
   procedure On_Activate_Slot (Self : access GObject_Record'Class);

   procedure Test_Constructors with Convention => C;
   procedure Test_Button_Methods with Convention => C;
   procedure Test_Accessibility with Convention => C;
   procedure Test_Actionable with Convention => C;
   procedure Test_Signals with Convention => C;

   --  Every callback validates that its adapter supplies the button object.
   procedure On_Clicked_Typed (Self : access Gtk_Button_Record'Class) is
   begin
      Assert_True (Self /= null);
      Typed_Clicked_Count := Typed_Clicked_Count + 1;
   end On_Clicked_Typed;

   procedure On_Clicked_Slot (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Slot_Clicked_Count := Slot_Clicked_Count + 1;
   end On_Clicked_Slot;

   procedure On_Activate_Typed (Self : access Gtk_Button_Record'Class) is
   begin
      Assert_True (Self /= null);
      Typed_Activate_Count := Typed_Activate_Count + 1;
   end On_Activate_Typed;

   procedure On_Activate_Slot (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Slot_Activate_Count := Slot_Activate_Count + 1;
   end On_Activate_Slot;

   --  Both constructor forms preserve their text; mnemonic labels preserve
   --  the underscore in Gtk.Button.Get_Label, which describes the input.
   procedure Test_Constructors is
      Label_Function : constant Gtk_Button :=
        Gtk_Button_New_With_Label ("function label");
      Empty_Function : constant Gtk_Button := Gtk_Button_New_With_Label;
      Icon_Function : constant Gtk_Button :=
        Gtk_Button_New_From_Icon_Name ("document-open-symbolic");
      Mnemonic_Function : constant Gtk_Button :=
        Gtk_Button_New_With_Mnemonic ("function _mnemonic");
      Label_Procedure : Gtk_Button;
      Empty_Procedure : Gtk_Button;
      Icon_Procedure : Gtk_Button;
      Mnemonic_Procedure : Gtk_Button;
   begin
      --  Sink each floating top-level widget so this test can release it.
      Ref_Sink (Label_Function);
      Ref_Sink (Empty_Function);
      Ref_Sink (Icon_Function);
      Ref_Sink (Mnemonic_Function);

      Gtk_New (Label_Procedure, "procedure label");
      Gtk_New (Empty_Procedure);
      Gtk_New_From_Icon_Name
        (Icon_Procedure, "document-open-symbolic");
      Gtk_New_With_Mnemonic
        (Mnemonic_Procedure, "procedure _mnemonic");
      Ref_Sink (Label_Procedure);
      Ref_Sink (Empty_Procedure);
      Ref_Sink (Icon_Procedure);
      Ref_Sink (Mnemonic_Procedure);

      --  Empty labels normalize to the empty Ada string; icon and mnemonic
      --  getters return their configured source values.
      Assert_Cmpstr_Eq (Label_Function.Get_Label, "function label");
      Assert_Cmpstr_Eq (Empty_Function.Get_Label, "");
      Assert_Cmpstr_Eq (Icon_Function.Get_Icon_Name, "document-open-symbolic");
      Assert_Cmpstr_Eq (Mnemonic_Function.Get_Label, "function _mnemonic");
      Assert_Cmpstr_Eq (Label_Procedure.Get_Label, "procedure label");
      Assert_Cmpstr_Eq (Empty_Procedure.Get_Label, "");
      Assert_Cmpstr_Eq (Icon_Procedure.Get_Icon_Name, "document-open-symbolic");
      Assert_Cmpstr_Eq (Mnemonic_Procedure.Get_Label, "procedure _mnemonic");

      Unref (Label_Function);
      Unref (Empty_Function);
      Unref (Icon_Function);
      Unref (Mnemonic_Function);
      Unref (Label_Procedure);
      Unref (Empty_Procedure);
      Unref (Icon_Procedure);
      Unref (Mnemonic_Procedure);
   end Test_Constructors;

   --  Check the getter/setter pairs and confirm child replacement updates the
   --  same widget relationship reported by Get_Child.
   procedure Test_Button_Methods is
      Button : constant Gtk_Button := Gtk_Button_New_With_Label ("initial");
      Child : constant Gtk_Label := Gtk_Label_New ("custom child");
   begin
      Ref_Sink (Button);
      --  Initializers are documented no-ops after construction; none may
      --  replace the original label or child.
      Button.Initialize ("ignored label");
      Button.Initialize_From_Icon_Name ("document-open-symbolic");
      Button.Initialize_With_Mnemonic ("ignored _mnemonic");
      Assert_Cmpstr_Eq (Button.Get_Label, "initial");

      Assert_False (Button.Get_Can_Shrink);
      Button.Set_Can_Shrink (True);
      Assert_True (Button.Get_Can_Shrink);
      Button.Set_Can_Shrink (False);
      Assert_False (Button.Get_Can_Shrink);

      --  The constructor supplies a label child before a custom child replaces it.
      Assert_True (Button.Get_Child /= null);
      Button.Set_Child (Child);
      Assert_True (Button.Get_Child = Gtk.Widget.Gtk_Widget (Child));
      Assert_Cmpstr_Eq (Child.Get_Label, "custom child");
      --  Passing null removes the custom child from the button.
      Button.Set_Child (null);
      Assert_True (Button.Get_Child = null);

      --  The default frame is on; both setter values are reflected by the getter.
      Assert_True (Button.Get_Has_Frame);
      Button.Set_Has_Frame (False);
      Assert_False (Button.Get_Has_Frame);
      Button.Set_Has_Frame (True);
      Assert_True (Button.Get_Has_Frame);

      --  Setting text replaces the generated icon child and clears its name.
      Button.Set_Icon_Name ("document-open-symbolic");
      Assert_Cmpstr_Eq (Button.Get_Icon_Name, "document-open-symbolic");
      Button.Set_Label ("replacement label");
      Assert_Cmpstr_Eq (Button.Get_Label, "replacement label");
      Assert_Cmpstr_Eq (Button.Get_Icon_Name, "");
      Assert_False (Button.Get_Use_Underline);
      Button.Set_Use_Underline (True);
      Assert_True (Button.Get_Use_Underline);
      Button.Set_Use_Underline (False);
      Assert_False (Button.Get_Use_Underline);

      Unref (Button);
   end Test_Button_Methods;

   --  A detached button has no AT context, bounds, parent, or sibling. The
   --  custom child verifies that accessible children remain enumerable.
   procedure Test_Accessibility is
      Button : constant Gtk_Button := Gtk_Button_New_With_Label ("accessible");
      Child : constant Gtk_Label := Gtk_Label_New ("child");
      Parent : Gtk_Accessible;
      Next : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (Button);
      Button.Set_Child (Child);

      --  GtkButton has button semantics even when its child is customized.
      Assert_True (Button.Get_Accessible_Role = Accessible_Role_Button);
      Assert_Cmpstr_Eq (Button.Get_Accessible_Id, "");
      Assert_True (Button.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      Parent := Button.Get_Accessible_Parent;
      Next := Button.Get_Next_Accessible_Sibling;
      Assert_True (Parent = Null_Gtk_Accessible);
      Assert_True (Next = Null_Gtk_Accessible);
      Context := Button.Get_At_Context;
      Assert_True (Context = null);
      --  An unmapped button has no screen coordinates or focused platform state.
      Assert_False (Button.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (Button.Get_Platform_State (Accessible_Platform_State_Focused));

      --  These accessibility notifications are valid without a screen reader.
      Button.Announce ("button test", Accessible_Announcement_Priority_Low);
      Button.Reset_Property (Accessible_Property_Label);
      Button.Reset_Relation (Accessible_Relation_Labelled_By);
      Button.Reset_State (Accessible_State_Busy);
      Button.Update_Platform_State (Accessible_Platform_State_Focusable);
      Button.Set_Accessible_Parent
        (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Button.Get_Accessible_Parent = Null_Gtk_Accessible);
      Button.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Button.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Assert_True (Button.Get_Accessible_Role = Accessible_Role_Button);

      Unref (Button);
   end Test_Accessibility;

   --  GtkActionable stores action names and optional GVariant targets; a
   --  detailed action name replaces both values, and an empty name clears it.
   procedure Test_Actionable is
      Button : constant Gtk_Button := Gtk_Button_New_With_Label ("actionable");
      Target : constant Gvariant := Gvariant_New_String ("ready");
      Read_Back : Gvariant;
   begin
      Ref_Sink (Button);
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "");
      Read_Back := Button.Get_Action_Target_Value;
      Assert_True (Read_Back = Null_Gvariant);

      Button.Set_Action_Name ("app.check");
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "app.check");
      --  Compare the target's value, not pointer identity, after round-tripping.
      Button.Set_Action_Target_Value (Target);
      Read_Back := Button.Get_Action_Target_Value;
      Assert_True (Read_Back /= Null_Gvariant);
      Assert_Cmpstr_Eq (Get_String (Read_Back), "ready");

      Button.Set_Detailed_Action_Name ("app.ready");
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "app.ready");
      Button.Set_Action_Name;
      Assert_Cmpstr_Eq (Button.Get_Action_Name, "");
      Unref (Button);
   end Test_Actionable;

   --  The typed and GObject-slot callback overloads both receive clicked and
   --  activate when those signals are emitted synchronously.
   procedure Test_Signals is
      Typed : constant Gtk_Button := Gtk_Button_New_With_Label ("typed");
      Slot : constant Gtk_Button := Gtk_Button_New_With_Label ("slot");
      Name : Interfaces.C.Strings.chars_ptr;
   begin
      Ref_Sink (Typed);
      Ref_Sink (Slot);
      --  Register one typed handler and one GObject slot handler per signal.
      Typed.On_Clicked (On_Clicked_Typed'Unrestricted_Access);
      Slot.On_Clicked (On_Clicked_Slot'Unrestricted_Access, Slot);
      Typed.On_Activate (On_Activate_Typed'Unrestricted_Access);
      Slot.On_Activate (On_Activate_Slot'Unrestricted_Access, Slot);

      Name := Interfaces.C.Strings.New_String ("clicked");
      Emit_Signal (Get_Object (Typed), Name);
      Emit_Signal (Get_Object (Slot), Name);
      Interfaces.C.Strings.Free (Name);
      Name := Interfaces.C.Strings.New_String ("activate");
      Emit_Signal (Get_Object (Typed), Name);
      Emit_Signal (Get_Object (Slot), Name);
      Interfaces.C.Strings.Free (Name);

      Assert_True (Typed_Clicked_Count = 1);
      Assert_True (Slot_Clicked_Count = 1);
      Assert_True (Typed_Activate_Count = 1);
      Assert_True (Slot_Activate_Count = 1);
      Unref (Typed);
      Unref (Slot);
   end Test_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/button/constructors", Test_Constructors'Unrestricted_Access);
   Add_Func ("/button/methods", Test_Button_Methods'Unrestricted_Access);
   Add_Func ("/button/accessibility", Test_Accessibility'Unrestricted_Access);
   Add_Func ("/button/actionable", Test_Actionable'Unrestricted_Access);
   Add_Func ("/button/signals", Test_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Button;
