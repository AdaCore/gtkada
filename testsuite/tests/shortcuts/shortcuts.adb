--  Test GTK shortcut actions and triggers, including parsing and printing.
--  Ported checks come from shortcuts.c:test_action_parse,
--  shortcuts.c:test_trigger_equal, shortcuts.c:test_trigger_basic,
--  and notify.c:test_type.

with Ada.Command_Line;

with Gdk;
with Gdk.Display;
with Gtkada.Types;
with Glib;
with Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.String;
with Glib.Test; use Glib.Test;
with Glib.Variant; use Glib.Variant;
with Gtk.Main;
with Gtk.Activate_Action;
with Gtk.Label;
with Gtk.Mnemonic_Action;
with Gtk.Never_Trigger;
with Gtk.Named_Action; use Gtk.Named_Action;
with Gtk.Nothing_Action;
with Gtk.Shortcut; use Gtk.Shortcut;
with Gtk.Shortcut_Action;
with Gtk.Shortcut_Trigger;
with Gtk.Signal_Action; use Gtk.Signal_Action;

use type Gtk.Shortcut_Action.Gtk_Shortcut_Action;
use type Gtk.Mnemonic_Action.Gtk_Mnemonic_Action;
use type Gtk.Never_Trigger.Gtk_Never_Trigger;
use type Gtk.Nothing_Action.Gtk_Nothing_Action;
use type Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger;
use type Glib.Variant.Gvariant;
use type Glib.GType;
use type Gdk.Gdk_Display;

procedure Shortcuts is

   procedure Test_Named_Action
   with Convention => C;
   procedure Test_Signal_Action
   with Convention => C;
   procedure Test_Shortcut_Action_Parse
   with Convention => C;
   procedure Test_Shortcut_Object
   with Convention => C;
   procedure Test_Trigger_Primitives
   with Convention => C;
   procedure Test_Activate_Action
   with Convention => C;
   procedure Test_Nothing_Action
   with Convention => C;
   procedure Test_Mnemonic_Action
   with Convention => C;
   procedure Test_Never_Trigger
   with Convention => C;

   --  Check both Ada constructors and the documented idempotence of
   --  Initialize when called on an object that already has a GObject.
   procedure Test_Named_Action is
      Action : Gtk_Named_Action;
   begin
      --  This constructor is the one exercised by the original C test.
      Action := Gtk_Named_Action_New ("text.undo");
      Assert_Cmpstr_Eq (Action.Get_Action_Name, "text.undo");

      --  Reinitializing an existing object must leave its original name.
      Gtk.Named_Action.Initialize (Action, "text.redo");
      Assert_Cmpstr_Eq (Action.Get_Action_Name, "text.undo");

      --  Release the reference returned by the constructor after checking it.
      Glib.Object.Unref (Glib.Object.GObject (Action));

      --  Gtk_New reaches the out-parameter constructor and initializes a
      --  freshly allocated action through the same public Initialize routine.
      Gtk_New (Action, "text.copy");
      Assert_Cmpstr_Eq (Action.Get_Action_Name, "text.copy");
      Glib.Object.Unref (Glib.Object.GObject (Action));
   end Test_Named_Action;

   --  Port the signal-action check from GTK and exercise both Ada constructors.
   procedure Test_Signal_Action is
      Action : Gtk_Signal_Action;
   begin
      --  Check the constructor, the direct getter, and the public property.
      Action := Gtk_Signal_Action_New ("activate");
      Assert_Cmpstr_Eq (Action.Get_Signal_Name, "activate");
      Assert_Cmpstr_Eq
        (Get_Property (Action, Signal_Name_Property), "activate");

      --  Initialize is a no-op once the object already owns a GObject.
      Gtk.Signal_Action.Initialize (Action, "notify");
      Assert_Cmpstr_Eq (Action.Get_Signal_Name, "activate");
      Glib.Object.Unref (Glib.Object.GObject (Action));

      --  Gtk_New must initialize a fresh object with the requested signal.
      Gtk_New (Action, "clicked");
      Assert_Cmpstr_Eq (Action.Get_Signal_Name, "clicked");
      Assert_Cmpstr_Eq
        (Get_Property (Action, Signal_Name_Property), "clicked");
      Glib.Object.Unref (Glib.Object.GObject (Action));
   end Test_Signal_Action;

   --  Port the parser's five successful cases from
   --  testsuite/c_tests/shortcuts.c:test_action_parse. Also check the
   --  debugging printer and its string-returning convenience wrapper.
   procedure Test_Shortcut_Action_Parse is
      procedure Check_Parse
        (Text, Expected_Type : Glib.UTF8_String)
      is
         Action : constant Gtk.Shortcut_Action.Gtk_Shortcut_Action :=
           Gtk.Shortcut_Action.Gtk_Shortcut_Action_Parse_String (Text);
      begin
         --  Parsing returns a base action handle whose GObject keeps its
         --  concrete GTK type, matching the C test's GTK_IS_* assertions.
         Assert_True (Action /= null);
         Assert_Cmpstr_Eq
           (Glib.Type_Name
              (Glib.Object.Get_Type (Glib.Object.GObject (Action))),
            Expected_Type);
         Glib.Object.Unref (Glib.Object.GObject (Action));
      end Check_Parse;

      Action : constant Gtk.Shortcut_Action.Gtk_Shortcut_Action :=
        Gtk.Shortcut_Action.Gtk_Shortcut_Action_Parse_String
          ("action(win.dark)");
      Printed : constant Glib.String.Gstring := Glib.String.Gstring_New;
   begin
      Check_Parse ("nothing", "GtkNothingAction");
      Check_Parse ("activate", "GtkActivateAction");
      Check_Parse ("mnemonic-activate", "GtkMnemonicAction");
      Check_Parse ("action(win.dark)", "GtkNamedAction");
      Check_Parse ("signal(frob)", "GtkSignalAction");

      --  GTK documents this representation as debug output, so compare the
      --  two public APIs instead of pinning a format that may change.
      Gtk.Shortcut_Action.Print (Action, Printed);
      Assert_Cmpstr_Ne (Gtkada.Types.Value (Printed.Str), "");
      Assert_Cmpstr_Eq
        (Gtkada.Types.Value (Printed.Str),
         Gtk.Shortcut_Action.To_String (Action));
      Assert_Cmpstr_Eq (Glib.String.Free (Printed, True), "");
      Glib.Object.Unref (Glib.Object.GObject (Action));
   end Test_Shortcut_Action_Parse;

   --  Exercise GtkShortcut's constructors, object getters and setters.
   --  GTK's C shortcuts test has no direct GtkShortcut object case to port.
   procedure Test_Shortcut_Object is
      Trigger_1 : Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger;
      Trigger_2 : Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger;
      Action_1  : Gtk.Shortcut_Action.Gtk_Shortcut_Action;
      Action_2  : Gtk.Shortcut_Action.Gtk_Shortcut_Action;
      By_Function  : Gtk_Shortcut;
      By_Procedure : Gtk_Shortcut;
      Args      : Gvariant;
      Read_Back : Gvariant;
   begin
      --  Gtk_New produces the trigger and action consumed by GtkShortcut_New.
      Gtk.Shortcut_Trigger.Gtk_New (Trigger_1, "<Control>x");
      Gtk.Shortcut_Trigger.Gtk_New (Trigger_2, "<Control>y");
      Gtk.Shortcut_Action.Gtk_New (Action_1, "nothing");
      Gtk.Shortcut_Action.Gtk_New (Action_2, "activate");

      --  Keep local references because the constructor takes ownership; the
      --  original trigger and action are reused for the procedure constructor.
      Glib.Object.Ref (Glib.Object.GObject (Trigger_1));
      Glib.Object.Ref (Glib.Object.GObject (Action_1));

      --  The getters return the same Ada objects supplied to the constructor.
      By_Function := Gtk_Shortcut_New (Trigger_1, Action_1);
      Assert_True (By_Function.Get_Trigger = Trigger_1);
      Assert_True (By_Function.Get_Action = Action_1);

      --  Arguments are a borrowed getter result; hold our own reference until
      --  the setter has retained its copy, then release that local reference.
      Args := Ref_Sink (Gvariant_New_String ("open-document"));
      By_Function.Set_Arguments (Args);
      Read_Back := By_Function.Get_Arguments;
      Assert_Cmpstr_Eq (Get_String (Read_Back), "open-document");
      Unref (Args);

      --  Setters replace the current action and trigger with their arguments.
      By_Function.Set_Action (Action_2);
      By_Function.Set_Trigger (Trigger_2);
      Assert_True (By_Function.Get_Action = Action_2);
      Assert_True (By_Function.Get_Trigger = Trigger_2);

      --  The out-parameter constructor initializes an independent shortcut.
      Gtk.Shortcut.Gtk_New (By_Procedure, Trigger_1, Action_1);
      Assert_True (By_Procedure.Get_Trigger = Trigger_1);
      Assert_True (By_Procedure.Get_Action = Action_1);

      --  Each shortcut owns its trigger/action references after construction.
      Glib.Object.Unref (Glib.Object.GObject (By_Function));
      Glib.Object.Unref (Glib.Object.GObject (By_Procedure));
   end Test_Shortcut_Object;

   --  Port the comparison and representation checks from
   --  testsuite/c_tests/shortcuts.c:test_trigger_equal, and cover the
   --  GtkShortcutTrigger parse, print, and display-label methods.
   procedure Test_Trigger_Primitives is
      Trigger      : constant Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger :=
        Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Parse_String
          ("<Control>x");
      Same_Trigger : constant Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger :=
        Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Parse_String
          ("<Control>x");
      Other        : constant Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger :=
        Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Parse_String
          ("<Control>y");
      Display      : constant Gdk.Display.Gdk_Display :=
        Gdk.Display.Get_Default;
      Printed      : constant Glib.String.Gstring :=
        Glib.String.Gstring_New;
      Label        : constant Glib.String.Gstring :=
        Glib.String.Gstring_New;
      Text_Label   : constant Glib.UTF8_String :=
        Gtk.Shortcut_Trigger.To_Label (Trigger, Display);
   begin
      --  Parsing the same trigger twice gives semantically equal objects.
      Assert_True (Trigger /= null);
      Assert_True (Same_Trigger /= null);
      Assert_True (Other /= null);
      Assert_True (Trigger.Equal (Same_Trigger));
      Assert_False (Trigger.Equal (Other));
      Assert_Cmpint_Eq (Trigger.Compare (Same_Trigger), 0);
      Assert_Cmpint_Ne (Trigger.Compare (Other), 0);
      Assert_Cmpuint_Eq (Trigger.Hash, Same_Trigger.Hash);

      --  The debug printer and string-returning helper describe the same
      --  trigger; avoid asserting the exact spelling of a debug format.
      Gtk.Shortcut_Trigger.Print (Trigger, Printed);
      Assert_Cmpstr_Ne (Gtkada.Types.Value (Printed.Str), "");
      Assert_Cmpstr_Eq
        (Gtkada.Types.Value (Printed.Str),
         Gtk.Shortcut_Trigger.To_String (Trigger));

      --  Label output is display-specific, so check that both public forms
      --  produce usable text rather than fixing a localized spelling.
      Assert_True (Display /= null);
      Assert_True (Gtk.Shortcut_Trigger.Print_Label (Trigger, Display, Label));
      Assert_Cmpstr_Ne (Gtkada.Types.Value (Label.Str), "");
      Assert_Cmpstr_Ne (Text_Label, "");

      --  GString buffers are allocated by GLib and must be explicitly freed.
      Assert_Cmpstr_Eq (Glib.String.Free (Printed, True), "");
      Assert_Cmpstr_Eq (Glib.String.Free (Label, True), "");
      Glib.Object.Unref (Glib.Object.GObject (Trigger));
      Glib.Object.Unref (Glib.Object.GObject (Same_Trigger));
      Glib.Object.Unref (Glib.Object.GObject (Other));
   end Test_Trigger_Primitives;

   --  Port the activate-action singleton case from
   --  testsuite/c_tests/notify.c:test_type.  GTK returns a borrowed singleton.
   procedure Test_Activate_Action is
      First  : constant Gtk.Shortcut_Action.Gtk_Shortcut_Action :=
        Gtk.Activate_Action.Get;
      Second : constant Gtk.Shortcut_Action.Gtk_Shortcut_Action :=
        Gtk.Activate_Action.Get;
   begin
      --  The action is registered under its documented GTK type and the
      --  conversion hook must preserve that dynamic type in GtkAda.
      Assert_Cmpstr_Eq
        (Glib.Type_Name (Gtk.Activate_Action.Get_Type), "GtkActivateAction");
      Assert_True (First /= null);
      Assert_True
        (Glib.Object.Get_Type (Glib.Object.GObject (First)) =
         Gtk.Activate_Action.Get_Type);

      --  Both borrowed results refer to GTK's one immutable singleton; they
      --  are intentionally not unreferenced by this test.
      Assert_True (First = Second);
   end Test_Activate_Action;

   --  Port the nothing-action activation check from shortcuts.c:test_action_activate
   --  and its singleton-type check from notify.c:test_type.
   procedure Test_Nothing_Action is
      First  : constant Gtk.Shortcut_Action.Gtk_Shortcut_Action :=
        Gtk.Nothing_Action.Get;
      Second : constant Gtk.Shortcut_Action.Gtk_Shortcut_Action :=
        Gtk.Nothing_Action.Get;
      Specific : constant Gtk.Nothing_Action.Gtk_Nothing_Action :=
        Gtk.Nothing_Action.Gtk_Nothing_Action (First);
      Widget : Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New ("");
   begin
      --  GTK returns its borrowed singleton, whose registered GType must be
      --  preserved by GtkAda's interface conversion hook.
      Assert_True (First /= null);
      Assert_True (Specific /= null);
      Assert_Cmpstr_Eq
        (Glib.Type_Name (Gtk.Nothing_Action.Get_Type), "GtkNothingAction");
      Assert_True
        (Glib.Object.Get_Type (Glib.Object.GObject (First)) =
         Gtk.Nothing_Action.Get_Type);
      Assert_True (First = Second);

      --  The action's defining behavior is to fail activation on a valid
      --  widget. Sink its initial floating reference before releasing it.
      Widget.Ref_Sink;
      Assert_False (First.Activate (0, Widget, Null_Gvariant));
      Glib.Object.Unref (Glib.Object.GObject (Widget));
   end Test_Nothing_Action;

   --  Port the mnemonic-action singleton check from
   --  testsuite/c_tests/notify.c:test_type.
   procedure Test_Mnemonic_Action is
      First  : constant Gtk.Shortcut_Action.Gtk_Shortcut_Action :=
        Gtk.Mnemonic_Action.Get;
      Second : constant Gtk.Shortcut_Action.Gtk_Shortcut_Action :=
        Gtk.Mnemonic_Action.Get;
      Specific : constant Gtk.Mnemonic_Action.Gtk_Mnemonic_Action :=
        Gtk.Mnemonic_Action.Gtk_Mnemonic_Action (First);
   begin
      --  Check GTK's registered type and GtkAda's conversion to its derived
      --  Ada record when the borrowed singleton is wrapped.
      Assert_Cmpstr_Eq
        (Glib.Type_Name (Gtk.Mnemonic_Action.Get_Type), "GtkMnemonicAction");
      Assert_True (First /= null);
      Assert_True (Specific /= null);
      Assert_True
        (Glib.Object.Get_Type (Glib.Object.GObject (First)) =
         Gtk.Mnemonic_Action.Get_Type);

      --  The getter returns GTK's borrowed immutable singleton each time.
      Assert_True (First = Second);
   end Test_Mnemonic_Action;

   --  Port the never-trigger type check from shortcuts.c:test_trigger_basic.
   procedure Test_Never_Trigger is
      First  : constant Gtk.Never_Trigger.Gtk_Never_Trigger :=
        Gtk.Never_Trigger.Get;
      Second : constant Gtk.Never_Trigger.Gtk_Never_Trigger :=
        Gtk.Never_Trigger.Get;
   begin
      --  The borrowed singleton must be wrapped as its specific GtkAda type.
      Assert_True (First /= null);
      Assert_True (Second /= null);
      Assert_Cmpstr_Eq
        (Glib.Type_Name (Gtk.Never_Trigger.Get_Type), "GtkNeverTrigger");
      Assert_True
        (Glib.Object.Get_Type (Glib.Object.GObject (First)) =
         Gtk.Never_Trigger.Get_Type);

      --  Get returns the same borrowed singleton on each call; do not unref it.
      Assert_True (First = Second);
   end Test_Never_Trigger;

begin
   Glib.Test.Init;
   --  The C test uses gtk_test_init, so initialize GTK before creating its
   --  GtkShortcutAction object as well.
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/shortcuts/named-action", Test_Named_Action'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/shortcuts/signal-action", Test_Signal_Action'Unrestricted_Access);
   --  Retrieve the singleton before the generic parser wraps it as a base
   --  GtkShortcutAction; GTK's nothing action is a process-wide object.
   Glib.Test.Add_Func
     ("/shortcuts/nothing-action",
      Test_Nothing_Action'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/shortcuts/shortcut-object", Test_Shortcut_Object'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/shortcuts/trigger-primitives",
      Test_Trigger_Primitives'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/shortcuts/activate-action",
      Test_Activate_Action'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/shortcuts/mnemonic-action",
      Test_Mnemonic_Action'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/shortcuts/never-trigger",
      Test_Never_Trigger'Unrestricted_Access);
   --  Parse "nothing" last because GTK may return its singleton; parsing it
   --  first would attach a base-action Ada wrapper before the typed getter.
   Glib.Test.Add_Func
     ("/shortcuts/action-parse",
      Test_Shortcut_Action_Parse'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Shortcuts;
