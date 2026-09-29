--  GTK has no testsuite/gtk/revealer.c to port, so these cases are ours:
--  they cover the surface the Gtk.Revealer binding adds, and no more.

with Ada.Command_Line;
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Glib.Test;        use Glib.Test;
with Gtk.Label;        use Gtk.Label;
with Gtk.Main;
with Gtk.Revealer;     use Gtk.Revealer;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Widget;       use Gtk.Widget;

procedure Revealer is

   procedure Test_Defaults with Convention => C;
   procedure Test_Child with Convention => C;
   procedure Test_Transition with Convention => C;
   procedure Test_Reveal with Convention => C;
   procedure Test_Bind with Convention => C;

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

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/revealer/defaults", Test_Defaults'Unrestricted_Access);
   Glib.Test.Add_Func ("/revealer/child", Test_Child'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/revealer/transition", Test_Transition'Unrestricted_Access);
   Glib.Test.Add_Func ("/revealer/reveal", Test_Reveal'Unrestricted_Access);
   Glib.Test.Add_Func ("/revealer/bind", Test_Bind'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Revealer;
