--  Port of testsuite/c_tests/revealer-size.c (keep_size and the six
--  slide/crossfade/none test pairs), plus Gtk.Revealer constructor,
--  accessibility, and Gtk.Settings checks for the wrappers declared here.

with Ada.Command_Line;
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Glib.Properties;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Button;       use Gtk.Button;
with Gtk.Enums;
with Glib.Test;        use Glib.Test;
with Gdk.Display;
with Gtk.Label;        use Gtk.Label;
with Gtk.Main;
with Gtk.Revealer;     use Gtk.Revealer;
with Gtk.Settings;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Widget;       use Gtk.Widget;

procedure Revealer is

   procedure Test_Defaults with Convention => C;
   procedure Test_Constructor with Convention => C;
   procedure Test_Child with Convention => C;
   procedure Test_Transition with Convention => C;
   procedure Test_Reveal with Convention => C;
   procedure Test_Bind with Convention => C;
   procedure Test_Accessibility with Convention => C;
   procedure Test_Sizing with Convention => C;
   procedure Test_Settings with Convention => C;

   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gdk.Gdk_Display;
   use type Gtk.Settings.Gtk_Settings;
   use type Gtk.Widget.Gtk_Widget;

   -------------------
   -- Test_Defaults --
   -------------------

   procedure Test_Defaults is
      R : constant Gtk_Revealer := Gtk_Revealer_New;
   begin
      Ref_Sink (R);

      Assert_False (R.Get_Reveal_Child);
      Assert_False (R.Get_Child_Revealed);
      Assert_True (R.Get_Child = null);
      Assert_True
        (R.Get_Transition_Type = Slide_Down);
      Assert_Cmpuint_Eq (R.Get_Transition_Duration, 250);

      Unref (R);
   end Test_Defaults;

   ----------------------
   -- Test_Constructor --
   ----------------------

   procedure Test_Constructor is
      R : Gtk_Revealer;
   begin
      --  Exercise the output-parameter constructor as well as the function
      --  constructor covered by Test_Defaults.
      Gtk_New (R);
      Ref_Sink (R);
      Assert_True (R.Get_Child = null);
      Assert_True (R.Get_Reveal_Child = False);
      Unref (R);
   end Test_Constructor;

   ----------------
   -- Test_Child --
   ----------------

   procedure Test_Child is
      R     : constant Gtk_Revealer := Gtk_Revealer_New;
      Label : constant Gtk_Label := Gtk_Label_New ("child");
   begin
      Ref_Sink (R);

      R.Set_Child (Label);
      Assert_True (R.Get_Child = Gtk_Widget (Label));

      R.Set_Child (null);
      Assert_True (R.Get_Child = null);

      Unref (R);
   end Test_Child;

   ---------------------
   -- Test_Transition --
   ---------------------

   procedure Test_Transition is
      R : constant Gtk_Revealer := Gtk_Revealer_New;
   begin
      Ref_Sink (R);

      --  Every value must survive the trip through the C enum.
      for T in Gtk_Revealer_Transition_Type loop
         R.Set_Transition_Type (T);
         Assert_True (R.Get_Transition_Type = T);
      end loop;

      R.Set_Transition_Duration (2_000);
      Assert_Cmpuint_Eq (R.Get_Transition_Duration, 2_000);

      Unref (R);
   end Test_Transition;

   -----------------
   -- Test_Reveal --
   -----------------

   procedure Test_Reveal is
      R : constant Gtk_Revealer := Gtk_Revealer_New;
   begin
      Ref_Sink (R);
      R.Set_Child (Gtk_Label_New ("child"));

      --  With no transition, or with the revealer unmapped, GTK applies the
      --  change at once instead of animating it, so child-revealed follows
      --  reveal-child without a main loop.
      R.Set_Transition_Type (None);

      R.Set_Reveal_Child (True);
      Assert_True (R.Get_Reveal_Child);
      Assert_True (R.Get_Child_Revealed);

      R.Set_Reveal_Child (False);
      Assert_False (R.Get_Reveal_Child);
      Assert_False (R.Get_Child_Revealed);

      Unref (R);
   end Test_Reveal;

   ---------------
   -- Test_Bind --
   ---------------

   procedure Test_Bind is
      --  How the demo drives a revealer: a toggle button's "active" bound to
      --  the revealer's "reveal-child".
      R      : constant Gtk_Revealer := Gtk_Revealer_New;
      Button : constant Gtk_Toggle_Button := Gtk_Toggle_Button_New;
   begin
      Ref_Sink (R);
      Ref_Sink (Button);

      Button.Bind_Property ("active", R, "reveal-child");

      Button.Set_Active (True);
      Assert_True (R.Get_Reveal_Child);
      Button.Set_Active (False);
      Assert_False (R.Get_Reveal_Child);

      Unref (Button);
      Unref (R);
   end Test_Bind;

   -------------------------
   -- Test_Accessibility --
   -------------------------

   procedure Test_Accessibility is
      R : constant Gtk_Revealer := Gtk_Revealer_New;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (R);

      --  A detached revealer has the documented group role and no assigned
      --  identity, parent, accessible children, sibling, context, or bounds.
      Assert_True
        (R.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_Group);
      Assert_Cmpstr_Eq (R.Get_Accessible_Id, "");
      Assert_True
        (R.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (R.Get_First_Accessible_Child = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (R.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (R.Get_At_Context = null);
      Assert_False (R.Get_Bounds (X, Y, Width, Height));

      --  Accessibility notifications and resets are valid while detached.
      R.Announce
        ("revealer accessibility test",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      R.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      R.Reset_Relation (Gtk.Accessible.Accessible_Relation_Labelled_By);
      R.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      Assert_False
        (R.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));

      --  GtkWidget's focusable property supplies this state on its own; the
      --  update notification is still exercised as part of the interface.
      R.Set_Focusable (True);
      R.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);
      Assert_True
        (R.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focusable));

      --  Null tree links leave this widget detached after exercising both
      --  setters for accessible parent and next sibling.
      R.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (R.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      R.Update_Next_Accessible_Sibling
        (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (R.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);

      Unref (R);
   end Test_Accessibility;

   ------------------
   -- Test_Sizing --
   ------------------

   procedure Test_Sizing is
      Settings : constant Gtk.Settings.Gtk_Settings :=
        Gtk.Settings.Get_Default;
      Animations_Before : constant Boolean :=
        Glib.Properties.Get_Property
          (Settings, Gtk.Settings.Gtk_Enable_Animations_Property);

      procedure Check_Size
        (Transition : Gtk_Revealer_Transition_Type;
         Keep_Width : Boolean;
         Keep_Height : Boolean;
         Animations : Boolean);

      procedure Check_Size
        (Transition : Gtk_Revealer_Transition_Type;
         Keep_Width : Boolean;
         Keep_Height : Boolean;
         Animations : Boolean)
      is
         R : constant Gtk_Revealer := Gtk_Revealer_New;
         Child : constant Gtk_Button := Gtk_Button_New_With_Label ("Some Text!");
         Min_Child_Width, Min_Child_Height : Gint;
         Min_Width, Min_Height : Gint;
         Natural_Size, Min_Baseline, Natural_Baseline : Gint;
      begin
         --  The C test toggles the display setting for every case. The
         --  revealer is deliberately unrooted, so no animation needs a loop.
         Glib.Properties.Set_Property
           (Settings, Gtk.Settings.Gtk_Enable_Animations_Property, Animations);
         Ref_Sink (R);
         R.Set_Child (Child);
         R.Set_Transition_Type (Transition);
         R.Set_Reveal_Child (True);

         Child.Measure
           (Gtk.Enums.Orientation_Horizontal, -1, Min_Child_Width,
            Natural_Size, Min_Baseline, Natural_Baseline);
         Child.Measure
           (Gtk.Enums.Orientation_Vertical, -1, Min_Child_Height,
            Natural_Size, Min_Baseline, Natural_Baseline);
         R.Measure
           (Gtk.Enums.Orientation_Horizontal, -1, Min_Width,
            Natural_Size, Min_Baseline, Natural_Baseline);
         R.Measure
           (Gtk.Enums.Orientation_Vertical, -1, Min_Height,
            Natural_Size, Min_Baseline, Natural_Baseline);

         --  When revealed, every transition type requests the child's full
         --  minimum size, matching the assertions in the C helper.
         Assert_Cmpint_Eq (Min_Width, Min_Child_Width);
         Assert_Cmpint_Eq (Min_Height, Min_Child_Height);

         R.Set_Reveal_Child (False);
         R.Measure
           (Gtk.Enums.Orientation_Horizontal, -1, Min_Width,
            Natural_Size, Min_Baseline, Natural_Baseline);
         R.Measure
           (Gtk.Enums.Orientation_Vertical, -1, Min_Height,
            Natural_Size, Min_Baseline, Natural_Baseline);

         --  Slide transitions retain the dimension perpendicular to motion;
         --  none and crossfade retain both dimensions.
         if Keep_Width then
            Assert_Cmpint_Eq (Min_Width, Min_Child_Width);
         else
            Assert_Cmpint_Eq (Min_Width, 0);
         end if;
         if Keep_Height then
            Assert_Cmpint_Eq (Min_Height, Min_Child_Height);
         else
            Assert_Cmpint_Eq (Min_Height, 0);
         end if;

         Unref (R);
      end Check_Size;

   begin
      --  Preserve all six native transition cases under both animation
      --  settings, as registered by revealer-size.c.
      for Animations in Boolean loop
         Check_Size (Slide_Right, False, True, Animations);
         Check_Size (Slide_Left, False, True, Animations);
         Check_Size (None, True, True, Animations);
         Check_Size (Crossfade, True, True, Animations);
         Check_Size (Slide_Down, True, False, Animations);
         Check_Size (Slide_Up, True, False, Animations);
      end loop;

      --  Restore the process-wide setting so later tests see their baseline.
      Glib.Properties.Set_Property
        (Settings, Gtk.Settings.Gtk_Enable_Animations_Property,
         Animations_Before);
   end Test_Sizing;

   -------------------
   -- Test_Settings --
   -------------------

   procedure Test_Settings is
      Display : constant Gdk.Gdk_Display := Gdk.Display.Get_Default;
      Settings : constant Gtk.Settings.Gtk_Settings :=
        Gtk.Settings.Get_Default;
      Display_Settings : Gtk.Settings.Gtk_Settings;
      Animations_Before : constant Boolean :=
        Glib.Properties.Get_Property
          (Settings, Gtk.Settings.Gtk_Enable_Animations_Property);
   begin
      --  GTK was initialized before the test cases, so both lookup functions
      --  must return the settings object associated with the Xvfb display.
      Assert_True (Display /= null);
      Display_Settings := Gtk.Settings.Get_For_Display (Display);
      Assert_True (Display_Settings /= null);
      Assert_True (Display_Settings = Settings);

      --  Reset_Property takes GTK's string property name.  Resetting after
      --  an application override must restore the startup setting value.
      Glib.Properties.Set_Property
        (Settings, Gtk.Settings.Gtk_Enable_Animations_Property,
         not Animations_Before);
      Assert_True
        (Glib.Properties.Get_Property
           (Settings, Gtk.Settings.Gtk_Enable_Animations_Property) =
         not Animations_Before);
      Settings.Reset_Property ("gtk-enable-animations");
      Assert_True
        (Glib.Properties.Get_Property
           (Settings, Gtk.Settings.Gtk_Enable_Animations_Property) =
         Animations_Before);

      --  Keep later cases independent even if the session baseline differs
      --  from the value that reset restored.
      Glib.Properties.Set_Property
        (Settings, Gtk.Settings.Gtk_Enable_Animations_Property,
         Animations_Before);
   end Test_Settings;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/revealer/defaults", Test_Defaults'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/revealer/constructor", Test_Constructor'Unrestricted_Access);
   Glib.Test.Add_Func ("/revealer/child", Test_Child'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/revealer/transition", Test_Transition'Unrestricted_Access);
   Glib.Test.Add_Func ("/revealer/reveal", Test_Reveal'Unrestricted_Access);
   Glib.Test.Add_Func ("/revealer/bind", Test_Bind'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/revealer/accessibility", Test_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/revealer/sizing", Test_Sizing'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/revealer/settings", Test_Settings'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Revealer;
