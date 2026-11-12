with Ada.Command_Line;
with Glib.Object;           use Glib.Object;
with Glib.Test;             use Glib.Test;
with Gtk.Alternative_Trigger;
with Gtk.Named_Action;
with Gtk.Never_Trigger;
with Gtk.Shortcut_Action;   use Gtk.Shortcut_Action;
with Gtk.Shortcut_Trigger;  use Gtk.Shortcut_Trigger;

procedure Main is
   procedure Test_Procedures with Convention => C;
   procedure Test_Null_Results with Convention => C;
   procedure Test_Cached_Wrapper with Convention => C;

   --  The function-form regression is in alternative-trigger. The out
   --  parameter constructors must preserve the returned subtype as well.
   procedure Test_Procedures is
      Trigger : Gtk_Shortcut_Trigger;
      Action  : Gtk_Shortcut_Action;
   begin
      Gtk.Shortcut_Trigger.Gtk_New (Trigger, "_U|<Shift>u");
      Assert_True
        (Trigger.all in
           Gtk.Alternative_Trigger.Gtk_Alternative_Trigger_Record'Class);
      Gtk.Shortcut_Action.Gtk_New (Action, "action(win.dark)");
      Assert_True
        (Action.all in Gtk.Named_Action.Gtk_Named_Action_Record'Class);
      Unref (Trigger);
      Unref (Action);
   end Test_Procedures;

   --  A failed parse returns null, without allocating an Ada base wrapper
   --  whose C pointer is null. Check both public constructor forms.
   procedure Test_Null_Results is
      Trigger : Gtk_Shortcut_Trigger :=
        Gtk_Shortcut_Trigger_Parse_String ("not a trigger");
      Action : Gtk_Shortcut_Action :=
        Gtk_Shortcut_Action_Parse_String ("not an action");
   begin
      Assert_True (Trigger = null);
      Assert_True (Action = null);
      Gtk.Shortcut_Trigger.Gtk_New (Trigger, "not a trigger");
      Gtk.Shortcut_Action.Gtk_New (Action, "not an action");
      Assert_True (Trigger = null);
      Assert_True (Action = null);
   end Test_Null_Results;

   --  Parsing "never" returns a reference to GTK's singleton. Reuse its
   --  existing Ada wrapper so other handles to that object stay valid.
   procedure Test_Cached_Wrapper is
      Cached : constant Gtk_Shortcut_Trigger :=
        Gtk_Shortcut_Trigger (Gtk.Never_Trigger.Get);
      Parsed : Gtk_Shortcut_Trigger :=
        Gtk_Shortcut_Trigger_Parse_String ("never");
   begin
      Assert_True (Parsed = Cached);
      Unref (Parsed);
      Gtk.Shortcut_Trigger.Gtk_New (Parsed, "never");
      Assert_True (Parsed = Cached);
      Unref (Parsed);
      --  Cached is borrowed from Get; only the parser references are ours.
   end Test_Cached_Wrapper;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/polymorphic-constructors/procedures",
      Test_Procedures'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/polymorphic-constructors/null-results",
      Test_Null_Results'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/polymorphic-constructors/cached-wrapper",
      Test_Cached_Wrapper'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
