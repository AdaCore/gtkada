--  Exercises Glib.Action_Group and Glib.Action_Map wrappers on a
--  Glib.Simple_Action_Group, including query outputs, entry construction,
--  action operations and both callback forms for group signals. Query_Action
--  has four optional outputs: C accepts NULL for each, and State and
--  State_Hint are transfer-full. Interface conversion matters because the
--  concrete group has separate wrappers for several of the same operations.
--
--  No widget is created, so this test needs no display.

with Ada.Unchecked_Conversion;
with Glib;                     use Glib;
with Glib.Action;              use type Glib.Action.Gaction;
with Glib.Action_Group;        use Glib.Action_Group;
with Glib.Action_Map;          use Glib.Action_Map;
with Glib.Simple_Action;       use Glib.Simple_Action;
with Glib.Simple_Action_Group; use Glib.Simple_Action_Group;
with Glib.Test;                use Glib.Test;
with Glib.Variant;             use Glib.Variant;
with Glib.Object;
with GNAT.Strings;
with Ada.Command_Line;
with System;

procedure Main is

   procedure Test_Query_Action
   with Convention => C;

   procedure Test_Query_Action_Declined
   with Convention => C;

   procedure Test_Query_Unknown_Action
   with Convention => C;

   procedure Test_Interface_Wrappers
   with Convention => C;

   procedure Test_Concrete_Wrappers
   with Convention => C;

   procedure Test_Entry_From_Object_Free
   with Convention => C;

   --  The conversion helper consumes storage from GLib's C allocator. Keep
   --  this access type private to the test so the allocated block is freed by
   --  the same allocator as the production helper expects.
   type GAction_Entry_Access is access all GAction_Entry;
   function To_GAction_Entry_Access is new Ada.Unchecked_Conversion
     (System.Address, GAction_Entry_Access);

   function G_Malloc0 (N_Bytes : Glib.Gsize) return System.Address;
   pragma Import (C, G_Malloc0, "g_malloc0");

   procedure Activated
     (Self      : access Gsimple_Action_Record'Class;
      Parameter : Gvariant);

   procedure Group_Action_Added
     (Self : Gaction_Group; Action_Name : UTF8_String);

   procedure Group_Action_Enabled
     (Self : Gaction_Group; Action_Name : UTF8_String; Enabled : Boolean);

   procedure Group_Action_Removed
     (Self : Gaction_Group; Action_Name : UTF8_String);

   procedure Group_Action_State
     (Self : Gaction_Group; Action_Name : UTF8_String; State : Gvariant);

   procedure Slot_Action_Added
     (Self : access Glib.Object.GObject_Record'Class;
      Action_Name : UTF8_String);

   procedure Slot_Action_Enabled
     (Self : access Glib.Object.GObject_Record'Class;
      Action_Name : UTF8_String; Enabled : Boolean);

   procedure Slot_Action_Removed
     (Self : access Glib.Object.GObject_Record'Class;
      Action_Name : UTF8_String);

   procedure Slot_Action_State
     (Self : access Glib.Object.GObject_Record'Class;
      Action_Name : UTF8_String; State : Gvariant);

   Activation_Calls : Natural := 0;
   Added_Calls      : Natural := 0;
   Enabled_Calls    : Natural := 0;
   Removed_Calls    : Natural := 0;
   State_Calls      : Natural := 0;
   Slot_Added_Calls   : Natural := 0;
   Slot_Enabled_Calls : Natural := 0;
   Slot_Removed_Calls : Natural := 0;
   Slot_State_Calls   : Natural := 0;

   function New_Group return Gsimple_Action_Group;
   --  An action group holding one stateful boolean action, "toggle".

   ---------------
   -- New_Group --
   ---------------

   function New_Group return Gsimple_Action_Group is
      Group  : Gsimple_Action_Group;
      Action : constant Gsimple_Action :=
        Gsimple_Action_New_Stateful
          ("toggle", null, Gvariant_New_Boolean (True));
      Hint : constant Gvariant :=
        Ref_Sink (Gvariant_New_Boolean (True));
   begin
      G_New (Group);
      --  A boolean hint gives the interface getters a non-null variant.
      Action.Set_State_Hint (Hint);
      Unref (Hint);
      Group.Add_Action (+Action);
      return Group;
   end New_Group;

   ---------------
   -- Activated --
   ---------------

   --  The parameter action proves that Activate_Action forwards its variant.
   procedure Activated
     (Self      : access Gsimple_Action_Record'Class;
      Parameter : Gvariant) is
   begin
      Assert_True (Self /= null);
      Assert_Cmpstr_Eq (Parameter.Get_String, "payload");
      Activation_Calls := Activation_Calls + 1;
   end Activated;

   ----------------------
   -- Group_Action_Added --
   ----------------------

   procedure Group_Action_Added
     (Self : Gaction_Group; Action_Name : UTF8_String) is
      pragma Unreferenced (Self);
   begin
      Assert_Cmpstr_Eq (Action_Name, "toggle");
      Added_Calls := Added_Calls + 1;
   end Group_Action_Added;

   -------------------------
   -- Group_Action_Enabled --
   -------------------------

   procedure Group_Action_Enabled
     (Self : Gaction_Group; Action_Name : UTF8_String; Enabled : Boolean) is
      pragma Unreferenced (Self);
   begin
      Assert_Cmpstr_Eq (Action_Name, "toggle");
      Assert_True (Enabled);
      Enabled_Calls := Enabled_Calls + 1;
   end Group_Action_Enabled;

   -----------------------
   -- Group_Action_Removed --
   -----------------------

   procedure Group_Action_Removed
     (Self : Gaction_Group; Action_Name : UTF8_String) is
      pragma Unreferenced (Self);
   begin
      Assert_Cmpstr_Eq (Action_Name, "toggle");
      Removed_Calls := Removed_Calls + 1;
   end Group_Action_Removed;

   ---------------------
   -- Group_Action_State --
   ---------------------

   procedure Group_Action_State
     (Self : Gaction_Group; Action_Name : UTF8_String; State : Gvariant) is
      pragma Unreferenced (Self);
   begin
      Assert_Cmpstr_Eq (Action_Name, "toggle");
      Assert_False (State.Get_Boolean);
      State_Calls := State_Calls + 1;
   end Group_Action_State;

   ----------------------
   -- Slot_Action_Added --
   ----------------------

   --  Slot callbacks receive their explicit GObject context as the first
   --  argument, instead of the action group interface.
   procedure Slot_Action_Added
     (Self : access Glib.Object.GObject_Record'Class;
      Action_Name : UTF8_String) is
   begin
      Assert_True (Self /= null);
      Assert_Cmpstr_Eq (Action_Name, "toggle");
      Slot_Added_Calls := Slot_Added_Calls + 1;
   end Slot_Action_Added;

   -------------------------
   -- Slot_Action_Enabled --
   -------------------------

   procedure Slot_Action_Enabled
     (Self : access Glib.Object.GObject_Record'Class;
      Action_Name : UTF8_String; Enabled : Boolean) is
   begin
      Assert_True (Self /= null);
      Assert_Cmpstr_Eq (Action_Name, "toggle");
      Assert_True (Enabled);
      Slot_Enabled_Calls := Slot_Enabled_Calls + 1;
   end Slot_Action_Enabled;

   ------------------------
   -- Slot_Action_Removed --
   ------------------------

   procedure Slot_Action_Removed
     (Self : access Glib.Object.GObject_Record'Class;
      Action_Name : UTF8_String) is
   begin
      Assert_True (Self /= null);
      Assert_Cmpstr_Eq (Action_Name, "toggle");
      Slot_Removed_Calls := Slot_Removed_Calls + 1;
   end Slot_Action_Removed;

   ----------------------
   -- Slot_Action_State --
   ----------------------

   procedure Slot_Action_State
     (Self : access Glib.Object.GObject_Record'Class;
      Action_Name : UTF8_String; State : Gvariant) is
   begin
      Assert_True (Self /= null);
      Assert_Cmpstr_Eq (Action_Name, "toggle");
      Assert_False (State.Get_Boolean);
      Slot_State_Calls := Slot_State_Calls + 1;
   end Slot_Action_State;

   -----------------------
   -- Test_Query_Action --
   -----------------------

   procedure Test_Query_Action is
      Group      : constant Gsimple_Action_Group := New_Group;
      Enabled    : Boolean;
      State_Type : aliased Gvariant_Type;
      State      : aliased Gvariant;
   begin
      Assert_True
        (Group.Query_Action
           ("toggle",
            Enabled,
            State_Type => State_Type'Access,
            State      => State'Access));
      Assert_True (Enabled);
      Assert_True (State_Type /= null);
      Assert_False (Is_Null (State));
      Assert_True (Get_Boolean (State));

      --  State is transfer-full: the caller owns the reference

      Unref (State);
      Unref (Group);
   end Test_Query_Action;

   --------------------------------
   -- Test_Query_Action_Declined --
   --------------------------------

   procedure Test_Query_Action_Declined is
      Group   : constant Gsimple_Action_Group := New_Group;
      Enabled : Boolean;
   begin
      --  A successful call that wants none of the optional outputs, both
      --  by omission and by an explicit null.

      Assert_True (Group.Query_Action ("toggle", Enabled));
      Assert_True (Enabled);

      Assert_True
        (Group.Query_Action
           ("toggle",
            Enabled,
            Parameter_Type => null,
            State_Type     => null,
            State_Hint     => null,
            State          => null));
      Assert_True (Enabled);

      Unref (Group);
   end Test_Query_Action_Declined;

   -------------------------------
   -- Test_Query_Unknown_Action --
   -------------------------------

   procedure Test_Query_Unknown_Action is
      Group      : constant Gsimple_Action_Group := New_Group;
      Enabled    : Boolean;
      State_Type : aliased Gvariant_Type := null;
      State      : aliased Gvariant;
   begin
      --  C writes nothing when it returns False, so the outputs must be
      --  left alone rather than filled from an untouched temporary.

      Assert_False
        (Group.Query_Action
           ("no-such-action",
            Enabled,
            State_Type => State_Type'Access,
            State      => State'Access));
      Assert_True (State_Type = null);
      Assert_True (Is_Null (State));

      Unref (Group);
   end Test_Query_Unknown_Action;

   -----------------------------
   -- Test_Interface_Wrappers --
   -----------------------------

   --  Invoke through Gaction_Group so the interface package's wrappers run.
   procedure Test_Interface_Wrappers is
      Concrete : constant Gsimple_Action_Group := New_Group;
      Group    : constant Gaction_Group := Glib.Simple_Action_Group."+" (Concrete);
      Toggle   : constant Gsimple_Action :=
        Gsimple_Action_New ("echo", Gvariant_Type_String);
      New_State  : constant Gvariant :=
        Ref_Sink (Gvariant_New_Boolean (False));
      Parameter  : constant Gvariant :=
        Ref_Sink (Gvariant_New_String ("payload"));
      Enabled    : Boolean;
      Param_Type : aliased Gvariant_Type;
      State_Type : aliased Gvariant_Type;
      State_Hint : aliased Gvariant;
      State      : aliased Gvariant;
      Saw_Toggle : Boolean := False;
      Saw_Echo   : Boolean := False;
   begin
      --  The interface's identity conversion preserves the same object.
      Assert_True (Glib.Action_Group."+" (Group) = Group);

      --  Install a string action to check parameter metadata and activation.
      Toggle.On_Activate (Activated'Unrestricted_Access);
      Concrete.Add_Action (+Toggle);

      Assert_True (Has_Action (Group, "toggle"));
      Assert_True (Has_Action (Group, "echo"));
      Assert_False (Has_Action (Group, "missing"));
      Assert_True (Get_Action_Enabled (Group, "toggle"));
      Assert_True (Get_Action_Enabled (Group, "echo"));
      Assert_Cmpstr_Eq
        (Peek_String (Get_Action_Parameter_Type (Group, "echo")), "s");
      Assert_True (Get_Action_Parameter_Type (Group, "toggle") = null);
      Assert_True (Get_Action_State_Type (Group, "toggle") /= null);
      Assert_True (Get_Action_State_Type (Group, "echo") = null);

      --  The returned state and hint are owned variants and are released here.
      State := Get_Action_State (Group, "toggle");
      Assert_True (State.Get_Boolean);
      Unref (State);
      State_Hint := Get_Action_State_Hint (Group, "toggle");
      Assert_True (State_Hint.Get_Boolean);
      Unref (State_Hint);
      Assert_True (Is_Null (Get_Action_State (Group, "echo")));
      Assert_True (Is_Null (Get_Action_State_Hint (Group, "echo")));

      --  List_Actions returns allocated strings; check names, then free them.
      declare
         Actions : GNAT.Strings.String_List := List_Actions (Group);
      begin
         Assert_True (Actions'Length = 2);
         for Name of Actions loop
            Saw_Toggle := Saw_Toggle or else Name.all = "toggle";
            Saw_Echo := Saw_Echo or else Name.all = "echo";
            GNAT.Strings.Free (Name);
         end loop;
      end;
      Assert_True (Saw_Toggle and Saw_Echo);

      --  Query_Action handles all optional outputs and the no-output case.
      Assert_True
        (Query_Action
           (Group, "toggle", Enabled, Param_Type'Access, State_Type'Access,
            State_Hint'Access, State'Access));
      Assert_True (Enabled);
      Assert_True (Param_Type = null);
      Assert_True (State_Type /= null);
      Assert_True (State_Hint.Get_Boolean);
      Assert_True (State.Get_Boolean);
      Unref (State_Hint);
      Unref (State);
      Assert_True (Query_Action (Group, "toggle", Enabled));
      Assert_True (Enabled);
      Assert_False
        (Query_Action
           (Group, "not-present", Enabled, State_Hint => State_Hint'Access,
            State => State'Access));
      Assert_True (Is_Null (State_Hint));
      Assert_True (Is_Null (State));

      --  Change_Action_State changes the state; activation toggles it back.
      Change_Action_State (Group, "toggle", New_State);
      State := Get_Action_State (Group, "toggle");
      Assert_False (State.Get_Boolean);
      Unref (State);
      Activate_Action (Group, "toggle", Null_Gvariant);
      State := Get_Action_State (Group, "toggle");
      Assert_True (State.Get_Boolean);
      Unref (State);

      --  The parameter action callback checks the value forwarded by C.
      Activate_Action (Group, "echo", Parameter);
      Assert_True (Activation_Calls = 1);

      --  Connect both interface and slot callback overloads to the four
      --  signals. Passing the group as Slot exercises its GObject conversion.
      Glib.Action_Group.On_Action_Added
        (Group, Group_Action_Added'Unrestricted_Access);
      Glib.Action_Group.On_Action_Enabled_Changed
        (Group, Group_Action_Enabled'Unrestricted_Access);
      Glib.Action_Group.On_Action_Removed
        (Group, Group_Action_Removed'Unrestricted_Access);
      Glib.Action_Group.On_Action_State_Changed
        (Group, Group_Action_State'Unrestricted_Access);
      Glib.Action_Group.On_Action_Added
        (Group, Slot_Action_Added'Unrestricted_Access, Concrete);
      Glib.Action_Group.On_Action_Enabled_Changed
        (Group, Slot_Action_Enabled'Unrestricted_Access, Concrete);
      Glib.Action_Group.On_Action_Removed
        (Group, Slot_Action_Removed'Unrestricted_Access, Concrete);
      Glib.Action_Group.On_Action_State_Changed
        (Group, Slot_Action_State'Unrestricted_Access, Concrete);

      --  Emitters notify listeners without changing group membership/state.
      Glib.Action_Group.Action_Added (Group, "toggle");
      Glib.Action_Group.Action_Enabled_Changed (Group, "toggle", True);
      Glib.Action_Group.Action_State_Changed (Group, "toggle", New_State);
      Glib.Action_Group.Action_Removed (Group, "toggle");
      Assert_True (Has_Action (Group, "toggle"));
      Assert_True (Added_Calls = 1 and Slot_Added_Calls = 1);
      Assert_True (Enabled_Calls = 1 and Slot_Enabled_Calls = 1);
      Assert_True (State_Calls = 1 and Slot_State_Calls = 1);
      Assert_True (Removed_Calls = 1 and Slot_Removed_Calls = 1);

      Unref (New_State);
      Unref (Parameter);
      Glib.Object.Unref (Glib.Object.GObject (Toggle));
      Unref (Concrete);
   end Test_Interface_Wrappers;

   --------------------------------
   -- Test_Concrete_Wrappers --
   --------------------------------

   --  Call the duplicate methods on Gsimple_Action_Group itself. The
   --  interface test above intentionally calls Gaction_Group wrappers.
   procedure Test_Concrete_Wrappers is
      Group : constant Gsimple_Action_Group := Gsimple_Action_Group_New;
      Toggle : constant Gsimple_Action :=
        Gsimple_Action_New_Stateful
          ("toggle", null, Gvariant_New_Boolean (True));
      Hint : constant Gvariant :=
        Ref_Sink (Gvariant_New_Boolean (True));
      Echo : constant Gsimple_Action :=
        Gsimple_Action_New ("echo", Gvariant_Type_String);
      Inserted : constant Gsimple_Action := Gsimple_Action_New ("inserted", null);
      Entries : constant GAction_Entry_Array :=
        (0 => Build ("entry-a", Parameter_Type => "s"),
         1 => Build ("entry-b", State => "true"));
      Parameter : constant Gvariant :=
        Ref_Sink (Gvariant_New_String ("payload"));
      Next_State : constant Gvariant :=
        Ref_Sink (Gvariant_New_Boolean (False));
      Group_Interface : constant Gaction_Group :=
        Glib.Simple_Action_Group."+" (Group);
      Map : constant Gaction_Map := Glib.Simple_Action_Group."+" (Group);
      Map_Roundtrip : constant Gaction_Map := Glib.Action_Map."+" (Map);
      Enabled : Boolean;
      Parameter_Type : aliased Gvariant_Type;
      State_Type : aliased Gvariant_Type;
      State_Hint : aliased Gvariant;
      State : aliased Gvariant;
      Saw_Toggle : Boolean := False;
      Saw_Echo : Boolean := False;
      Added_Before : constant Natural := Added_Calls;
      Enabled_Before : constant Natural := Enabled_Calls;
      Removed_Before : constant Natural := Removed_Calls;
      State_Before : constant Natural := State_Calls;
      Activations_Before : constant Natural := Activation_Calls;
   begin
      --  The function constructor initializes the object; a second Initialize
      --  call is harmless and covers the already-created path.
      Glib.Simple_Action_Group.Initialize (Group);
      Toggle.Set_State_Hint (Hint);
      Unref (Hint);
      Echo.On_Activate (Activated'Unrestricted_Access);
      Group.Add_Action (+Toggle);
      Group.Add_Action (+Echo);

      --  Exercise the concrete action-group metadata accessors and check the
      --  values they return for stateful and parameterized actions.
      Assert_True (Group.Has_Action ("toggle"));
      Assert_True (Group.Has_Action ("echo"));
      Assert_False (Group.Has_Action ("missing"));
      Assert_True (Group.Get_Action_Enabled ("toggle"));
      Assert_Cmpstr_Eq
        (Peek_String (Group.Get_Action_Parameter_Type ("echo")), "s");
      Assert_True (Group.Get_Action_Parameter_Type ("toggle") = null);
      Assert_True (Group.Get_Action_State_Type ("toggle") /= null);
      Assert_True (Group.Get_Action_State_Type ("echo") = null);

      --  Returned variants are transfer-full, so release each after checking.
      State := Group.Get_Action_State ("toggle");
      Assert_True (State.Get_Boolean);
      Unref (State);
      State_Hint := Group.Get_Action_State_Hint ("toggle");
      Assert_True (State_Hint.Get_Boolean);
      Unref (State_Hint);
      Assert_True (Is_Null (Group.Get_Action_State ("echo")));
      Assert_True (Is_Null (Group.Get_Action_State_Hint ("echo")));

      --  The concrete list wrapper returns allocated action-name strings.
      declare
         Actions : GNAT.Strings.String_List := Group.List_Actions;
      begin
         Assert_True (Actions'Length = 2);
         for Name of Actions loop
            Saw_Toggle := Saw_Toggle or else Name.all = "toggle";
            Saw_Echo := Saw_Echo or else Name.all = "echo";
            GNAT.Strings.Free (Name);
         end loop;
      end;
      Assert_True (Saw_Toggle and Saw_Echo);

      --  Query all optional outputs, including the transfer-full state and
      --  hint variants, and verify both action-map lookup entry points.
      Assert_True
        (Group.Query_Action
           ("toggle", Enabled, Parameter_Type'Access, State_Type'Access,
            State_Hint'Access, State'Access));
      Assert_True (Enabled);
      Assert_True (Parameter_Type = null);
      Assert_True (State_Type /= null);
      Assert_True (State_Hint.Get_Boolean and State.Get_Boolean);
      Unref (State_Hint);
      Unref (State);
      Assert_True (Group.Lookup ("toggle") = +Toggle);
      Assert_True (Group.Lookup_Action ("echo") = +Echo);
      Assert_True
        (Group.Lookup_Action ("missing") = Glib.Action.Null_Gaction);

      --  State changes and activation are forwarded to the underlying group.
      Group.Change_Action_State ("toggle", Next_State);
      State := Group.Get_Action_State ("toggle");
      Assert_False (State.Get_Boolean);
      Unref (State);
      Group.Activate_Action ("echo", Parameter);
      Assert_True (Activation_Calls = Activations_Before + 1);

      --  The legacy group entry API adds actions which can then be removed
      --  individually through both concrete removal wrappers.
      Group.Add_Entries (Entries);
      Assert_True (Group.Has_Action ("entry-a"));
      Assert_True (Group.Has_Action ("entry-b"));
      Group.Remove ("entry-a");
      Group.Remove_Action ("entry-b");
      Assert_False (Group.Has_Action ("entry-a"));
      Assert_False (Group.Has_Action ("entry-b"));

      --  Insert and the deprecated simple-group lookup/remove methods share
      --  the same action map, so check their membership effects explicitly.
      Group.Insert (+Inserted);
      Assert_True (Group.Lookup ("inserted") = +Inserted);
      Group.Remove ("inserted");
      Assert_False (Group.Has_Action ("inserted"));

      --  Exercise the interface-level GActionMap batch wrapper; the entries
      --  create one parameterized action and one stateful action whose
      --  metadata can be checked through the group.
      Glib.Action_Map.Add_Action_Entries
        (Map_Roundtrip, Entries);
      Assert_True (Group.Has_Action ("entry-a"));
      Assert_True (Group.Has_Action ("entry-b"));
      Assert_True (Glib.Action_Map.Lookup_Action (Map, "entry-a") /=
                   Glib.Action.Null_Gaction);
      Assert_True (Glib.Action_Map.Lookup_Action (Map, "entry-b") /=
                   Glib.Action.Null_Gaction);
      Assert_True (Glib.Action_Map.Lookup_Action (Map, "missing") =
                   Glib.Action.Null_Gaction);
      Assert_Cmpstr_Eq
        (Peek_String (Group.Get_Action_Parameter_Type ("entry-a")), "s");
      State := Group.Get_Action_State ("entry-b");
      Assert_True (State.Get_Boolean);
      Unref (State);

      --  Exercise the standalone GActionMap remove wrapper, including its
      --  harmless missing-action lookup after removal.
      Glib.Action_Map.Remove_Action (Map, "entry-a");
      Assert_True (Glib.Action_Map.Lookup_Action (Map, "entry-a") =
                   Glib.Action.Null_Gaction);
      Assert_False (Group.Has_Action ("entry-a"));

      --  Remove_Action_Entries is the reverse of batch addition; removal also
      --  tolerates the already-removed first entry.
      Group.Remove_Action_Entries (Entries, Gint (Entries'Length));
      Assert_False (Group.Has_Action ("entry-b"));

      --  Connect signal checks through the interface, then call the concrete
      --  emitters to verify their UTF-8, Boolean and variant arguments.
      Glib.Action_Group.On_Action_Added
        (Group_Interface, Group_Action_Added'Unrestricted_Access);
      Glib.Action_Group.On_Action_Enabled_Changed
        (Group_Interface, Group_Action_Enabled'Unrestricted_Access);
      Glib.Action_Group.On_Action_Removed
        (Group_Interface, Group_Action_Removed'Unrestricted_Access);
      Glib.Action_Group.On_Action_State_Changed
        (Group_Interface, Group_Action_State'Unrestricted_Access);
      Group.Action_Added ("toggle");
      Group.Action_Enabled_Changed ("toggle", True);
      Group.Action_State_Changed ("toggle", Next_State);
      Group.Action_Removed ("toggle");
      Assert_True (Added_Calls = Added_Before + 1);
      Assert_True (Enabled_Calls = Enabled_Before + 1);
      Assert_True (State_Calls = State_Before + 1);
      Assert_True (Removed_Calls = Removed_Before + 1);

      Unref (Next_State);
      Unref (Parameter);
      Glib.Object.Unref (Glib.Object.GObject (Toggle));
      Glib.Object.Unref (Glib.Object.GObject (Echo));
      Glib.Object.Unref (Glib.Object.GObject (Inserted));
      Unref (Group);
   end Test_Concrete_Wrappers;

   --------------------------------------
   -- Test_Entry_From_Object_Free --
   --------------------------------------

   --  Exercise the internal converter with a correctly sized, zeroed
   --  C-owned entry. Comparing the shallow copy verifies the returned value
   --  before the helper releases its C-allocated storage.
   procedure Test_Entry_From_Object_Free is
      Bytes : constant Glib.Gsize := Glib.Gsize
        ((GAction_Entry'Size + System.Storage_Unit - 1) / System.Storage_Unit);
      Storage : constant System.Address := G_Malloc0 (Bytes);
      Entry_Ptr : constant GAction_Entry_Access :=
        To_GAction_Entry_Access (Storage);
      Expected : constant GAction_Entry := Entry_Ptr.all;
      Actual : constant GAction_Entry :=
        Glib.Action_Map.From_Object_Free (Entry_Ptr.all'Access);
   begin
      Assert_True (Actual = Expected);
   end Test_Entry_From_Object_Free;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func
     ("/action-group/query-action", Test_Query_Action'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/action-group/query-action-declined",
      Test_Query_Action_Declined'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/action-group/query-unknown-action",
      Test_Query_Unknown_Action'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/action-group/interface-wrappers",
      Test_Interface_Wrappers'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/action-group/concrete-wrappers",
      Test_Concrete_Wrappers'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/action-group/entry-from-object-free",
      Test_Entry_From_Object_Free'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
