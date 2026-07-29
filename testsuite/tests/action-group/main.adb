--  Exercises Glib.Action_Group.Query_Action, whose four last outputs are
--  optional: C accepts a NULL for each of them, and State is documented as
--  transfer-full. The point of interest is that declining an output really
--  does hand C a null pointer, so that a *successful* call computes no
--  reference the Ada side would then drop on the floor.
--
--  No widget is created, so this test needs no display.

with Glib;                     use Glib;
with Glib.Simple_Action;       use Glib.Simple_Action;
with Glib.Simple_Action_Group; use Glib.Simple_Action_Group;
with Glib.Test;                use Glib.Test;
with Glib.Variant;             use Glib.Variant;
with Ada.Command_Line;

procedure Main is

   procedure Test_Query_Action
   with Convention => C;

   procedure Test_Query_Action_Declined
   with Convention => C;

   procedure Test_Query_Unknown_Action
   with Convention => C;

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
   begin
      G_New (Group);
      Group.Add_Action (+Action);
      return Group;
   end New_Group;

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

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
