--  Exercise Glib.Simple_Action construction, state, enablement, and signals.
--  The C action tests cover widget action-group inheritance, so this focused
--  test checks the direct GSimpleAction API exposed by the Ada binding.

with Ada.Command_Line;

with Glib.Action;
with Glib.Object;
with Glib.Simple_Action;
with Glib.Test; use Glib.Test;
with Glib.Variant;

procedure Main is
   use type Glib.Variant.Gvariant_Type;

   Typed_Activate_Calls : Natural := 0;
   Slot_Activate_Calls  : Natural := 0;
   Typed_State_Calls    : Natural := 0;
   Slot_State_Calls     : Natural := 0;
   Interface_Activate_Calls : Natural := 0;

   --  The typed activate overload passes the action itself to the callback.
   procedure Typed_Activate
     (Self      : access Glib.Simple_Action.Gsimple_Action_Record'Class;
      Parameter : Glib.Variant.Gvariant);

   procedure Typed_Activate
     (Self      : access Glib.Simple_Action.Gsimple_Action_Record'Class;
      Parameter : Glib.Variant.Gvariant) is
   begin
      Assert_True (Self /= null);
      Assert_Cmpstr_Eq (Glib.Variant.Get_String (Parameter), "launch");
      Typed_Activate_Calls := Typed_Activate_Calls + 1;
   end Typed_Activate;

   --  A slot callback receives the object supplied to On_Activate as Self.
   procedure Slot_Activate
     (Self      : access Glib.Object.GObject_Record'Class;
      Parameter : Glib.Variant.Gvariant);

   procedure Slot_Activate
     (Self      : access Glib.Object.GObject_Record'Class;
      Parameter : Glib.Variant.Gvariant) is
   begin
      Assert_True (Self /= null);
      Assert_Cmpstr_Eq (Glib.Variant.Get_String (Parameter), "slot-launch");
      Slot_Activate_Calls := Slot_Activate_Calls + 1;
   end Slot_Activate;

   --  A typed change-state callback observes the requested state value.
   procedure Typed_Change_State
     (Self      : access Glib.Simple_Action.Gsimple_Action_Record'Class;
      Parameter : Glib.Variant.Gvariant);

   procedure Typed_Change_State
     (Self      : access Glib.Simple_Action.Gsimple_Action_Record'Class;
      Parameter : Glib.Variant.Gvariant) is
   begin
      Assert_True (Self /= null);
      Assert_Cmpstr_Eq (Glib.Variant.Get_String (Parameter), "typed-state");
      Typed_State_Calls := Typed_State_Calls + 1;
   end Typed_Change_State;

   --  The slot form uses a separate action as its closure object, keeping the
   --  slot lifetime explicit and checking the object passed to the callback.
   procedure Slot_Change_State
     (Self      : access Glib.Object.GObject_Record'Class;
      Parameter : Glib.Variant.Gvariant);

   procedure Slot_Change_State
     (Self      : access Glib.Object.GObject_Record'Class;
      Parameter : Glib.Variant.Gvariant) is
   begin
      Assert_True (Self /= null);
      Assert_Cmpstr_Eq (Glib.Variant.Get_String (Parameter), "slot-state");
      Slot_State_Calls := Slot_State_Calls + 1;
   end Slot_Change_State;

   procedure Test_Stateless_Constructor with Convention => C;
   procedure Test_State with Convention => C;
   procedure Test_Activate_Signals with Convention => C;
   procedure Test_Change_State_Signals with Convention => C;
   procedure Test_Action_Interface with Convention => C;

   --  This callback proves that Activate through Gaction still emits the
   --  implementation's signal with its parameter intact.
   procedure Interface_Activate
     (Self      : access Glib.Simple_Action.Gsimple_Action_Record'Class;
      Parameter : Glib.Variant.Gvariant);

   procedure Interface_Activate
     (Self      : access Glib.Simple_Action.Gsimple_Action_Record'Class;
      Parameter : Glib.Variant.Gvariant) is
   begin
      Assert_True (Self /= null);
      Assert_Cmpstr_Eq (Glib.Variant.Get_String (Parameter), "from-interface");
      Interface_Activate_Calls := Interface_Activate_Calls + 1;
   end Interface_Activate;

   --  Cover the out-parameter constructors and verify stateless metadata.
   procedure Test_Stateless_Constructor is
      Action : Glib.Simple_Action.Gsimple_Action;
   begin
      Glib.Simple_Action.G_New
        (Action, "stateless", Glib.Variant.Gvariant_Type_String);
      Assert_Cmpstr_Eq (Action.Get_Name, "stateless");
      Assert_True (Action.Get_Enabled);
      Assert_True
        (Glib.Variant.Peek_String (Action.Get_Parameter_Type) = "s");

      --  Initialize is idempotent once the object has a GObject instance.
      Action.Initialize ("ignored", Glib.Variant.Gvariant_Type_Boolean);
      Assert_Cmpstr_Eq (Action.Get_Name, "stateless");
      Assert_True
        (Glib.Variant.Peek_String (Action.Get_Parameter_Type) = "s");
      Glib.Object.Unref (Glib.Object.GObject (Action));
   end Test_Stateless_Constructor;

   --  Cover state accessors and prove that unhandled change-state requests
   --  take GLib's documented default action of replacing the state.
   procedure Test_State is
      Initial : constant Glib.Variant.Gvariant :=
        Glib.Variant.Gvariant_New_String ("ready");
      Action : Glib.Simple_Action.Gsimple_Action;
      Value  : Glib.Variant.Gvariant;
   begin
      Glib.Simple_Action.G_New_Stateful
        (Action, "mode", Glib.Variant.Gvariant_Type_String, Initial);
      Assert_Cmpstr_Eq (Action.Get_Name, "mode");
      Assert_True
        (Glib.Variant.Peek_String (Action.Get_State_Type) = "s");

      Value := Action.Get_State;
      Assert_Cmpstr_Eq (Glib.Variant.Get_String (Value), "ready");
      Glib.Variant.Unref (Value);

      Action.Set_State (Glib.Variant.Gvariant_New_String ("direct"));
      Value := Action.Get_State;
      Assert_Cmpstr_Eq (Glib.Variant.Get_String (Value), "direct");
      --  Get_State returns an owned variant, so release each read after use.
      Glib.Variant.Unref (Value);

      Action.Set_State_Hint (Glib.Variant.Gvariant_New_String ("ready/busy"));
      Value := Action.Get_State_Hint;
      Assert_Cmpstr_Eq (Glib.Variant.Get_String (Value), "ready/busy");
      Glib.Variant.Unref (Value);

      Action.Change_State (Glib.Variant.Gvariant_New_String ("requested"));
      Value := Action.Get_State;
      Assert_Cmpstr_Eq (Glib.Variant.Get_String (Value), "requested");
      Glib.Variant.Unref (Value);
      Glib.Object.Unref (Glib.Object.GObject (Action));
   end Test_State;

   --  Exercise both activate signal overloads and ensure disabled actions do
   --  not emit their signal before they are enabled again.
   procedure Test_Activate_Signals is
      Typed : constant Glib.Simple_Action.Gsimple_Action :=
        Glib.Simple_Action.Gsimple_Action_New
          ("typed-activate", Glib.Variant.Gvariant_Type_String);
      Slotted : constant Glib.Simple_Action.Gsimple_Action :=
        Glib.Simple_Action.Gsimple_Action_New
          ("slot-activate", Glib.Variant.Gvariant_Type_String);
      Slot : constant Glib.Simple_Action.Gsimple_Action :=
        Glib.Simple_Action.Gsimple_Action_New
          ("activate-slot", Glib.Variant.Gvariant_Type_String);
   begin
      Typed.On_Activate (Typed_Activate'Unrestricted_Access, After => True);
      --  The slotted connection keeps the separate slot object alive through
      --  the action's signal closure until the action is unreferenced below.
      Slotted.On_Activate (Slot_Activate'Unrestricted_Access, Slot);

      Typed.Set_Enabled (False);
      Assert_False (Typed.Get_Enabled);
      Typed.Activate (Glib.Variant.Gvariant_New_String ("launch"));
      Assert_True (Typed_Activate_Calls = 0);

      Typed.Set_Enabled (True);
      Assert_True (Typed.Get_Enabled);
      Typed.Activate (Glib.Variant.Gvariant_New_String ("launch"));
      Slotted.Activate (Glib.Variant.Gvariant_New_String ("slot-launch"));
      Assert_True (Typed_Activate_Calls = 1);
      Assert_True (Slot_Activate_Calls = 1);

      Glib.Object.Unref (Glib.Object.GObject (Typed));
      Glib.Object.Unref (Glib.Object.GObject (Slotted));
      Glib.Object.Unref (Glib.Object.GObject (Slot));
   end Test_Activate_Signals;

   --  Check both change-state handler signatures. Connected handlers suppress
   --  GLib's default update, so the callback receives the request unchanged.
   procedure Test_Change_State_Signals is
      Typed : constant Glib.Simple_Action.Gsimple_Action :=
        Glib.Simple_Action.Gsimple_Action_New_Stateful
          ("typed-state", Glib.Variant.Gvariant_Type_String,
           Glib.Variant.Gvariant_New_String ("initial"));
      Slotted : constant Glib.Simple_Action.Gsimple_Action :=
        Glib.Simple_Action.Gsimple_Action_New_Stateful
          ("slot-state", Glib.Variant.Gvariant_Type_String,
           Glib.Variant.Gvariant_New_String ("initial"));
      Slot : constant Glib.Simple_Action.Gsimple_Action :=
        Glib.Simple_Action.Gsimple_Action_New
          ("state-slot", Glib.Variant.Gvariant_Type_String);
      Value : Glib.Variant.Gvariant;
   begin
      Typed.On_Change_State (Typed_Change_State'Unrestricted_Access);
      Slotted.On_Change_State (Slot_Change_State'Unrestricted_Access, Slot);

      --  These requests exercise the marshallers synchronously on this thread.
      Typed.Change_State (Glib.Variant.Gvariant_New_String ("typed-state"));
      Slotted.Change_State (Glib.Variant.Gvariant_New_String ("slot-state"));
      Assert_True (Typed_State_Calls = 1);
      Assert_True (Slot_State_Calls = 1);

      Value := Typed.Get_State;
      Assert_Cmpstr_Eq (Glib.Variant.Get_String (Value), "initial");
      Glib.Variant.Unref (Value);
      Value := Slotted.Get_State;
      Assert_Cmpstr_Eq (Glib.Variant.Get_String (Value), "initial");
      Glib.Variant.Unref (Value);

      Glib.Object.Unref (Glib.Object.GObject (Typed));
      Glib.Object.Unref (Glib.Object.GObject (Slotted));
      Glib.Object.Unref (Glib.Object.GObject (Slot));
   end Test_Change_State_Signals;

   --  Exercise Glib.Action's generated interface wrappers rather than only
   --  Gsimple_Action's concrete forwarding methods.
   procedure Test_Action_Interface is
      Action : constant Glib.Simple_Action.Gsimple_Action :=
        Glib.Simple_Action.Gsimple_Action_New_Stateful
          ("interface-action", Glib.Variant.Gvariant_Type_String,
           Glib.Variant.Gvariant_New_String ("initial"));
      --  The conversion creates a typed interface view of the same GObject.
      Action_View : constant Glib.Action.Gaction :=
        Glib.Simple_Action."+" (Action);
      Value : Glib.Variant.Gvariant;
   begin
      Action.Set_State_Hint (Glib.Variant.Gvariant_New_String ("range"));
      Action.On_Activate (Interface_Activate'Unrestricted_Access);

      Assert_Cmpstr_Eq
        (Glib.Action.Get_Name (Action_View), "interface-action");
      Assert_True (Glib.Action.Get_Enabled (Action_View));
      Assert_True
        (Glib.Variant.Peek_String
           (Glib.Action.Get_Parameter_Type (Action_View)) = "s");
      Assert_True
        (Glib.Variant.Peek_String
           (Glib.Action.Get_State_Type (Action_View)) = "s");

      --  Get_State transfers an owned variant to the caller.
      Value := Glib.Action.Get_State (Action_View);
      Assert_Cmpstr_Eq (Glib.Variant.Get_String (Value), "initial");
      Glib.Variant.Unref (Value);
      Value := Glib.Action.Get_State_Hint (Action_View);
      Assert_Cmpstr_Eq (Glib.Variant.Get_String (Value), "range");
      Glib.Variant.Unref (Value);

      --  Interface calls dispatch to GSimpleAction and retain normal behavior.
      Glib.Action.Change_State
        (Action_View, Glib.Variant.Gvariant_New_String ("changed"));
      Value := Glib.Action.Get_State (Glib.Action."+" (Action_View));
      Assert_Cmpstr_Eq (Glib.Variant.Get_String (Value), "changed");
      Glib.Variant.Unref (Value);
      Glib.Action.Activate
        (Action_View, Glib.Variant.Gvariant_New_String ("from-interface"));
      Assert_True (Interface_Activate_Calls = 1);

      Glib.Object.Unref (Glib.Object.GObject (Action));
   end Test_Action_Interface;

   --  Check the GLib action-name rules and detailed-name formatting for both
   --  actions without targets and actions with string targets.
   procedure Test_Action_Names with Convention => C;
   procedure Test_Action_Names is
   begin
      --  The invalid candidate is only sent to Name_Is_Valid, which accepts it.
      Assert_True (Glib.Action.Name_Is_Valid ("app.save-2"));
      Assert_False (Glib.Action.Name_Is_Valid ("app.bad name"));
      --  A missing target omits the separator; a string target follows it.
      Assert_Cmpstr_Eq
        (Glib.Action.Print_Detailed_Name
           ("app.quit", Glib.Variant.Null_Gvariant),
         "app.quit");
      Assert_Cmpstr_Eq
        (Glib.Action.Print_Detailed_Name
           ("app.open", Glib.Variant.Gvariant_New_String ("notes.txt")),
         "app.open::notes.txt");
   end Test_Action_Names;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/glib/simple-action/stateless", Test_Stateless_Constructor'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib/simple-action/state", Test_State'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib/simple-action/activate-signals", Test_Activate_Signals'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib/simple-action/change-state-signals", Test_Change_State_Signals'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib/simple-action/action-interface", Test_Action_Interface'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib/simple-action/action-names", Test_Action_Names'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
