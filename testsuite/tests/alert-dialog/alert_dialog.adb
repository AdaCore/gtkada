--  Gtk.Alert_Dialog tests for constructors, configurable properties, and
--  asynchronous cancellation. There is no matching C test to port.

with Ada.Command_Line;
with GNAT.Strings;

with Glib;
with Glib.Cancellable;
with Glib.Error;
with Glib.Main;
with Glib.Object;
with Glib.Test; use Glib.Test;
with Gtk.Alert_Dialog; use Gtk.Alert_Dialog;
with Gtk.Main;
with Gtk.Widget;
with Gtk.Window;

procedure Alert_Dialog is
   use type Glib.Error.GError;
   use type Glib.Gint;
   use type Glib.Guint;
   use type Gtk.Widget.Widget_List.Glist;

   Choice_Done          : Boolean := False;
   Expect_Cancellation  : Boolean := False;

   procedure On_Choice_Ready
     (Source : access Glib.Object.GObject_Record'Class;
      Result : Glib.G_Async_Result);
   procedure Test_Construction with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Cancelled_Choose with Convention => C;
   procedure Test_Choose_Default with Convention => C;
   procedure Test_Null_Callback with Convention => C;
   procedure Test_Show with Convention => C;
   function Toplevel_Count return Glib.Guint;
   procedure Activate_Defaults;

   --  The callback completes the asynchronous operation through its matching
   --  Finish routine and checks that cancellation is reported as an error.
   procedure On_Choice_Ready
     (Source : access Glib.Object.GObject_Record'Class;
      Result : Glib.G_Async_Result)
   is
      Dialog : constant Gtk_Alert_Dialog := Gtk_Alert_Dialog (Source);
      Error  : Glib.Error.GError;
      Index  : Glib.Gint;
   begin
      Index := Dialog.Choose_Finish (Result, Error);
      if Expect_Cancellation then
         Assert_True (Error /= null);
         Glib.Error.Error_Free (Error);
      else
         Assert_True (Error = null);
         Assert_Cmpint_Eq (Index, 0);
      end if;
      Choice_Done := True;
   end On_Choice_Ready;

   --  List_Toplevels returns GTK-owned nodes containing unreferenced widgets;
   --  the test only activates each current window's default action.
   function Toplevel_Count return Glib.Guint is
   begin
      return Gtk.Widget.Widget_List.Length (Gtk.Window.List_Toplevels);
   end Toplevel_Count;

   procedure Activate_Defaults is
      Windows : Gtk.Widget.Widget_List.Glist := Gtk.Window.List_Toplevels;
      Current : Gtk.Widget.Widget_List.Glist :=
        Gtk.Widget.Widget_List.First (Windows);
   begin
      while Current /= Gtk.Widget.Widget_List.Null_List loop
         Gtk.Widget.Activate_Default
           (Gtk.Widget.Widget_List.Get_Data (Current));
         Current := Gtk.Widget.Widget_List.Next (Current);
      end loop;
   end Activate_Defaults;

   --  Gtk_New initializes a fresh record; Initialize on that object is a
   --  no-op, while the function constructor independently creates an object.
   procedure Test_Construction is
      By_Procedure : Gtk_Alert_Dialog;
      By_Function  : Gtk_Alert_Dialog;
      Empty        : Gtk_Alert_Dialog;
   begin
      Gtk_New (By_Procedure, "Procedure constructor");
      Assert_True (By_Procedure /= null);
      Assert_Cmpstr_Eq (By_Procedure.Get_Message, "Procedure constructor");

      Gtk.Alert_Dialog.Initialize (By_Procedure, "Ignored second message");
      Assert_Cmpstr_Eq (By_Procedure.Get_Message, "Procedure constructor");

      By_Function := Gtk_Alert_Dialog_New ("Function constructor");
      Assert_True (By_Function /= null);
      Assert_Cmpstr_Eq (By_Function.Get_Message, "Function constructor");

      --  An empty constructor message leaves the GTK default empty string.
      Gtk_New (Empty);
      Assert_Cmpstr_Eq (Empty.Get_Message, "");

      By_Procedure.Unref;
      By_Function.Unref;
      Empty.Unref;
   end Test_Construction;

   --  Round-trip every property-like getter and setter without showing UI.
   procedure Test_Properties is
      Dialog : constant Gtk_Alert_Dialog := Gtk_Alert_Dialog_New;
      Labels : GNAT.Strings.String_List :=
        (new String'("Continue"), new String'("Cancel"));
   begin
      --  Set_Buttons copies the strings, so the input array can be freed.
      Dialog.Set_Buttons (Labels);
      for Label of Labels loop
         GNAT.Strings.Free (Label);
      end loop;

      --  Get_Buttons returns caller-owned copies of the configured labels.
      declare
         Actual : GNAT.Strings.String_List := Dialog.Get_Buttons;
      begin
         Assert_Cmpuint_Eq (Glib.Guint (Actual'Length), 2);
         Assert_Cmpstr_Eq (Actual (Actual'First).all, "Continue");
         Assert_Cmpstr_Eq (Actual (Actual'Last).all, "Cancel");
         for Label of Actual loop
            GNAT.Strings.Free (Label);
         end loop;
      end;

      --  Button indices are values, including GTK's unset index of -1.
      Dialog.Set_Cancel_Button (1);
      Assert_Cmpint_Eq (Dialog.Get_Cancel_Button, 1);
      Dialog.Set_Cancel_Button (-1);
      Assert_Cmpint_Eq (Dialog.Get_Cancel_Button, -1);
      Dialog.Set_Default_Button (0);
      Assert_Cmpint_Eq (Dialog.Get_Default_Button, 0);
      Dialog.Set_Default_Button (-1);
      Assert_Cmpint_Eq (Dialog.Get_Default_Button, -1);

      --  UTF-8 text is preserved by both string marshalling accessors.
      Dialog.Set_Detail ("More information: café");
      Assert_Cmpstr_Eq (Dialog.Get_Detail, "More information: café");
      Dialog.Set_Message ("A warning message");
      Assert_Cmpstr_Eq (Dialog.Get_Message, "A warning message");

      --  Verify both modal states so the Boolean to GBoolean conversion runs.
      Dialog.Set_Modal (False);
      Assert_False (Dialog.Get_Modal);
      Dialog.Set_Modal (True);
      Assert_True (Dialog.Get_Modal);

      Dialog.Unref;
   end Test_Properties;

   --  Cancellation dispatches the ready callback without waiting for a
   --  button press. The callback exercises the Ada callback conversion and
   --  Choose_Finish's GError output.
   procedure Test_Cancelled_Choose is
      Dialog      : constant Gtk_Alert_Dialog :=
        Gtk_Alert_Dialog_New ("This alert is cancelled by the test");
      Cancellable : Glib.Cancellable.Gcancellable :=
        Glib.Cancellable.Gcancellable_New;
      Dispatched  : Boolean;
   begin
      Choice_Done := False;
      Expect_Cancellation := True;
      Dialog.Choose (null, Cancellable, On_Choice_Ready'Unrestricted_Access);
      Cancellable.Cancel;

      --  Dispatch GTK's deferred completion as required by the async API.
      for Attempt in 1 .. 5_000 loop
         exit when Choice_Done;
         Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
         delay 0.001;
      end loop;
      Assert_True (Choice_Done);

      Cancellable.Unref;
      Dialog.Unref;
   end Test_Cancelled_Choose;

   --  Activating the GTK window's default action selects the configured
   --  default button, so Choose_Finish's successful result is testable under
   --  Xvfb without a human interaction.
   procedure Test_Choose_Default is
      Dialog : constant Gtk_Alert_Dialog :=
        Gtk_Alert_Dialog_New ("Select the default button");
      Labels : GNAT.Strings.String_List := (1 => new String'("Continue"));
      Baseline : constant Glib.Guint := Toplevel_Count;
      Dispatched : Boolean;
   begin
      Dialog.Set_Buttons (Labels);
      GNAT.Strings.Free (Labels (Labels'First));
      Dialog.Set_Default_Button (0);
      Choice_Done := False;
      Expect_Cancellation := False;
      Dialog.Choose (null, null, On_Choice_Ready'Unrestricted_Access);

      --  Pump creation and selection until GTK dispatches the ready callback.
      for Attempt in 1 .. 5_000 loop
         exit when Choice_Done;
         Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
         if Toplevel_Count > Baseline then
            Activate_Defaults;
         end if;
         delay 0.001;
      end loop;
      Assert_True (Choice_Done);
      Dialog.Unref;
   end Test_Choose_Default;

   --  Choose explicitly permits a null callback for a one-button alert. Its
   --  default action dismisses the alert, and the observed toplevel count
   --  checks both presentation and completion without callback state.
   procedure Test_Null_Callback is
      Dialog : constant Gtk_Alert_Dialog :=
        Gtk_Alert_Dialog_New ("Dismiss without a callback");
      Labels : GNAT.Strings.String_List := (1 => new String'("Close"));
      Baseline : constant Glib.Guint := Toplevel_Count;
      Seen     : Boolean := False;
      Dispatched : Boolean;
   begin
      Dialog.Set_Buttons (Labels);
      GNAT.Strings.Free (Labels (Labels'First));
      Dialog.Set_Default_Button (0);
      Dialog.Choose (null, null, null);

      --  The dialog is dismissed once its toplevel has appeared.
      for Attempt in 1 .. 5_000 loop
         Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
         if Toplevel_Count > Baseline then
            Seen := True;
            Activate_Defaults;
         elsif Seen then
            exit;
         end if;
         delay 0.001;
      end loop;
      Assert_True (Seen);
      Assert_Cmpuint_Eq (Toplevel_Count, Baseline);
      Dialog.Unref;
   end Test_Null_Callback;

   --  Show is the one-button synchronous presentation API; GTK's default
   --  action closes the window after the test observes it.
   procedure Test_Show is
      Dialog : constant Gtk_Alert_Dialog :=
        Gtk_Alert_Dialog_New ("Shown and dismissed by the test");
      Labels : GNAT.Strings.String_List := (1 => new String'("Close"));
      Baseline : constant Glib.Guint := Toplevel_Count;
      Seen     : Boolean := False;
      Dispatched : Boolean;
   begin
      Dialog.Set_Buttons (Labels);
      GNAT.Strings.Free (Labels (Labels'First));
      Dialog.Set_Default_Button (0);
      Dialog.Show (null);

      --  Iterate the main context to let GTK present, then activate Close.
      for Attempt in 1 .. 5_000 loop
         Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
         if Toplevel_Count > Baseline then
            Seen := True;
            Activate_Defaults;
         elsif Seen then
            exit;
         end if;
         delay 0.001;
      end loop;
      Assert_True (Seen);
      Assert_Cmpuint_Eq (Toplevel_Count, Baseline);
      Dialog.Unref;
   end Test_Show;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/alert-dialog/constructors", Test_Construction'Unrestricted_Access);
   Add_Func ("/alert-dialog/properties", Test_Properties'Unrestricted_Access);
   Add_Func
     ("/alert-dialog/cancelled-choose",
      Test_Cancelled_Choose'Unrestricted_Access);
   Add_Func
     ("/alert-dialog/choose-default",
      Test_Choose_Default'Unrestricted_Access);
   Add_Func
     ("/alert-dialog/null-callback",
      Test_Null_Callback'Unrestricted_Access);
   Add_Func ("/alert-dialog/show", Test_Show'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Alert_Dialog;
