--  Exercise Gtk.Switch constructors, state, accessibility and actionable APIs,
--  interface conversions, and signal callbacks. GTK 4.22.2 has no switch test
--  in testsuite/gtk/switch.c, so these cases cover the Ada binding directly.

with Ada.Command_Line;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Test;             use Glib.Test;
with Glib.Variant;          use Glib.Variant;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Actionable;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Main;
with Gtk.Switch;            use Gtk.Switch;
with Gtk.Widget;
with Interfaces.C.Strings;
with System;

use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Actionable.Gtk_Actionable;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Buildable.Gtk_Buildable;
use type Gtk.Constraint_Target.Gtk_Constraint_Target;
use type Gtk.Switch.Gtk_Switch;
use type Glib.Variant.Gvariant;
use type System.Address;

procedure Main is
   Activate_Typed_Count : Natural := 0;
   Activate_Slot_Count  : Natural := 0;
   State_Typed_Count    : Natural := 0;
   State_Slot_Count     : Natural := 0;
   Seen_Typed_State     : Boolean := False;
   Seen_Slot_State      : Boolean := False;

   --  Gtk.Switch's activate signal has no parameters or return value.
   procedure Emit (Object : System.Address; Name : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   --  For state-set, the boolean parameter is followed by the signal's return
   --  value pointer, matching g_signal_emit_by_name's C varargs convention.
   procedure Emit_State_Set
     (Object       : System.Address;
      Name         : Interfaces.C.Strings.chars_ptr;
      State        : Gboolean;
      Return_Value : access Gboolean)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure On_Activate_Typed (Self : access Gtk_Switch_Record'Class);
   procedure On_Activate_Slot (Self : access GObject_Record'Class);
   function On_State_Set_Typed
     (Self : access Gtk_Switch_Record'Class; State : Boolean) return Boolean;
   function On_State_Set_Slot
     (Self : access GObject_Record'Class; State : Boolean) return Boolean;

   procedure Test_State_And_Properties with Convention => C;
   procedure Test_Accessibility_And_Interfaces with Convention => C;
   procedure Test_Actionable with Convention => C;
   procedure Test_Signals with Convention => C;

   --  The typed callback checks the concrete Gtk.Switch signal overload.
   procedure On_Activate_Typed (Self : access Gtk_Switch_Record'Class) is
   begin
      Assert_True (Self /= null);
      Activate_Typed_Count := Activate_Typed_Count + 1;
   end On_Activate_Typed;

   --  The slot callback checks the GObject-compatible overload of On_Activate.
   procedure On_Activate_Slot (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Activate_Slot_Count := Activate_Slot_Count + 1;
   end On_Activate_Slot;

   --  Returning True prevents the default state-set handler, as documented.
   function On_State_Set_Typed
     (Self : access Gtk_Switch_Record'Class; State : Boolean) return Boolean is
   begin
      Assert_True (Self /= null);
      State_Typed_Count := State_Typed_Count + 1;
      Seen_Typed_State := State;
      return True;
   end On_State_Set_Typed;

   --  This callback covers the GObject slot form and records its boolean input.
   function On_State_Set_Slot
     (Self : access GObject_Record'Class; State : Boolean) return Boolean is
   begin
      Assert_True (Self /= null);
      State_Slot_Count := State_Slot_Count + 1;
      Seen_Slot_State := State;
      return True;
   end On_State_Set_Slot;

   --  Both constructors and both properties are checked against the public
   --  getters so the Ada wrappers' state conversions are observable.
   procedure Test_State_And_Properties is
      By_Procedure : Gtk_Switch;
      By_Function  : constant Gtk_Switch := Gtk_Switch_New;
   begin
      Gtk_New (By_Procedure);
      Ref_Sink (By_Procedure);
      Ref_Sink (By_Function);

      Assert_False (By_Procedure.Get_Active);
      Assert_False (By_Procedure.Get_State);
      By_Procedure.Set_Active (True);
      Assert_True (By_Procedure.Get_Active);
      Assert_True (Get_Property (By_Procedure, Active_Property));
      By_Procedure.Set_Active (False);
      Assert_False (Get_Property (By_Procedure, Active_Property));

      By_Function.Set_State (True);
      Assert_True (By_Function.Get_State);
      Assert_True (Get_Property (By_Function, State_Property));
      By_Function.Set_State (False);
      Assert_False (By_Function.Get_State);

      --  Property writes use the same API contract as the corresponding
      --  methods, and confirm both directions of the generated property API.
      Set_Property (By_Procedure, Active_Property, True);
      Assert_True (By_Procedure.Get_Active);
      Set_Property (By_Function, State_Property, True);
      Assert_True (By_Function.Get_State);

      Unref (By_Procedure);
      Unref (By_Function);
   end Test_State_And_Properties;

   --  This case checks the inherited GtkAccessible methods and each interface
   --  conversion pair on an unrooted switch, where bounds and AT context are
   --  expected to be absent and do not depend on a physical display.
   procedure Test_Accessibility_And_Interfaces is
      Switch  : constant Gtk_Switch := Gtk_Switch_New;
      Parent  : Gtk_Accessible;
      Next    : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
      Has_Bounds : Boolean;
      Accessible : constant Gtk_Accessible := +Switch;
      Actionable : constant Gtk.Actionable.Gtk_Actionable := +Switch;
      Buildable : constant Gtk.Buildable.Gtk_Buildable := +Switch;
      Constraint_Target : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +Switch;
   begin
      Ref_Sink (Switch);
      Assert_True (Switch.Get_Accessible_Role = Accessible_Role_Switch);
      Assert_Cmpstr_Eq (Switch.Get_Accessible_Id, "");
      Assert_True (Switch.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True (Switch.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Assert_True (Switch.Get_First_Accessible_Child /= Null_Gtk_Accessible);

      Context := Switch.Get_At_Context;
      Assert_True (Context = null);
      Has_Bounds := Switch.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Assert_False (Switch.Get_Platform_State (Accessible_Platform_State_Focused));

      --  Resets and accessibility updates accept valid enum values without a
      --  platform backend; setting a null parent leaves this widget unparented.
      Switch.Announce ("switch test", Accessible_Announcement_Priority_Low);
      Switch.Reset_Property (Accessible_Property_Label);
      Switch.Reset_Relation (Accessible_Relation_Labelled_By);
      Switch.Reset_State (Accessible_State_Busy);
      Switch.Update_Platform_State (Accessible_Platform_State_Focusable);
      Switch.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Parent := Switch.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Switch.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Next := Switch.Get_Next_Accessible_Sibling;
      Assert_True (Next = Null_Gtk_Accessible);

      --  Convert through every interface listed by Gtk.Switch and back to the
      --  object to verify that the binding's interface hooks preserve identity.
      Assert_True (-Accessible = Switch);
      Assert_True (-Actionable = Switch);
      --  Gtk.Actionable's own unary plus is an identity conversion on an
      --  interface value; check it separately from Gtk.Switch's conversion.
      Assert_True (Gtk.Actionable."+" (Actionable) = Actionable);
      Assert_True (-Buildable = Switch);
      Assert_True (-Constraint_Target = Switch);
      Unref (Switch);
   end Test_Accessibility_And_Interfaces;

   --  Gtk.Switch implements GtkActionable, so action names and target variants
   --  can be round-tripped without installing an action group or activating it.
   procedure Test_Actionable is
      Switch : constant Gtk_Switch := Gtk_Switch_New;
      Actionable : constant Gtk.Actionable.Gtk_Actionable := +Switch;
      Target : constant Gvariant := Gvariant_New_String ("enabled");
      Read_Back : Gvariant;
   begin
      Ref_Sink (Switch);
      Assert_Cmpstr_Eq (Gtk.Actionable.Get_Action_Name (Actionable), "");
      Gtk.Actionable.Set_Action_Name (Actionable, "app.switch");
      Assert_Cmpstr_Eq
        (Gtk.Actionable.Get_Action_Name (Actionable), "app.switch");

      Gtk.Actionable.Set_Action_Target_Value (Actionable, Target);
      Read_Back := Gtk.Actionable.Get_Action_Target_Value (Actionable);
      Assert_True (Read_Back /= Null_Gvariant);
      Assert_Cmpstr_Eq (Get_String (Read_Back), "enabled");

      --  A detailed action name without an explicit target replaces the name
      --  and clears the previous target, while the empty name clears the link.
      Gtk.Actionable.Set_Detailed_Action_Name (Actionable, "app.ready");
      Assert_Cmpstr_Eq
        (Gtk.Actionable.Get_Action_Name (Actionable), "app.ready");
      Gtk.Actionable.Set_Action_Name (Actionable);
      Assert_Cmpstr_Eq (Gtk.Actionable.Get_Action_Name (Actionable), "");
      Unref (Switch);
   end Test_Actionable;

   --  Emit both switch signals directly so the tests do not depend on mouse or
   --  keyboard input; each Ada callback overload must receive its own values.
   procedure Test_Signals is
      Typed : constant Gtk_Switch := Gtk_Switch_New;
      Slot  : constant Gtk_Switch := Gtk_Switch_New;
      Name  : Interfaces.C.Strings.chars_ptr;
      Returned : aliased Gboolean := 0;
   begin
      Ref_Sink (Typed);
      Ref_Sink (Slot);
      Typed.On_Activate (On_Activate_Typed'Unrestricted_Access);
      Slot.On_Activate (On_Activate_Slot'Unrestricted_Access, Slot);
      Name := Interfaces.C.Strings.New_String ("activate");
      Emit (Get_Object (Typed), Name);
      Emit (Get_Object (Slot), Name);
      Interfaces.C.Strings.Free (Name);
      Assert_True (Activate_Typed_Count = 1);
      Assert_True (Activate_Slot_Count = 1);

      Typed.On_State_Set (On_State_Set_Typed'Unrestricted_Access);
      Slot.On_State_Set (On_State_Set_Slot'Unrestricted_Access, Slot);
      Name := Interfaces.C.Strings.New_String ("state-set");
      Emit_State_Set (Get_Object (Typed), Name, 1, Returned'Access);
      Assert_True (Returned /= 0);
      Emit_State_Set (Get_Object (Slot), Name, 0, Returned'Access);
      Interfaces.C.Strings.Free (Name);
      Assert_True (State_Typed_Count = 1);
      Assert_True (Seen_Typed_State);
      Assert_True (State_Slot_Count = 1);
      Assert_False (Seen_Slot_State);
      Assert_False (Slot.Get_State);

      Unref (Typed);
      Unref (Slot);
   end Test_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/gtk-switch/state-and-properties", Test_State_And_Properties'Unrestricted_Access);
   Add_Func ("/gtk-switch/accessibility-and-interfaces", Test_Accessibility_And_Interfaces'Unrestricted_Access);
   Add_Func ("/gtk-switch/actionable", Test_Actionable'Unrestricted_Access);
   Add_Func ("/gtk-switch/signals", Test_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
