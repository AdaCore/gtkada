--  GTK has no testsuite/gtk/linkbutton.c to port, so these cases are ours:
--  they cover the surface the Gtk.Link_Button binding adds, and no more.

with Ada.Command_Line;
with Glib.Test;       use Glib.Test;
with Gtk.Button;      use Gtk.Button;
with Gtk.Link_Button; use Gtk.Link_Button;
with Gtk.Main;

procedure Link_Button is

   Home : constant String := "http://www.example.com/";
   Away : constant String := "http://www.adacore.com/";

   procedure Test_Uri
   with Convention => C;

   procedure Test_Visited
   with Convention => C;

   procedure Test_Label
   with Convention => C;

   --  There is deliberately no activate-link case. Nothing in the gtk4
   --  binding emits the signal synchronously: gtk_button_clicked is gone,
   --  and Gtk.Widget.Activate only starts GtkButton's press animation, which
   --  emits "clicked" from a 250ms timeout. Driving it would mean spinning
   --  the main loop on a timer, which buys less than it risks in flakiness.

   --------------
   -- Test_Uri --
   --------------

   procedure Test_Uri is
      B : constant Gtk_Link_Button := Gtk_Link_Button_New (Home);
   begin
      Assert_Cmpstr_Eq (B.Get_Uri, Home);

      B.Set_Uri (Away);
      Assert_Cmpstr_Eq (B.Get_Uri, Away);
   end Test_Uri;

   ------------------
   -- Test_Visited --
   ------------------

   procedure Test_Visited is
      B : constant Gtk_Link_Button := Gtk_Link_Button_New (Home);
   begin
      Assert_False (B.Get_Visited);

      B.Set_Visited (True);
      Assert_True (B.Get_Visited);

      --  Re-pointing the button is documented to unset "visited".
      B.Set_Uri (Away);
      Assert_False (B.Get_Visited);
   end Test_Visited;

   ----------------
   -- Test_Label --
   ----------------

   procedure Test_Label is
      Labelled : constant Gtk_Link_Button :=
        Gtk_Link_Button_New_With_Label (Home, "Click me");

      --  The label is nullable, and the binding maps the default "" onto
      --  NULL, which is what makes GTK fall back on the URI.
      Bare     : constant Gtk_Link_Button :=
        Gtk_Link_Button_New_With_Label (Home);
   begin
      Assert_Cmpstr_Eq (Labelled.Get_Uri, Home);
      Assert_Cmpstr_Eq (Labelled.Get_Label, "Click me");

      Assert_Cmpstr_Eq (Bare.Get_Uri, Home);
      Assert_Cmpstr_Eq (Bare.Get_Label, Home);
   end Test_Label;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/link-button/uri", Test_Uri'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/link-button/visited", Test_Visited'Unrestricted_Access);
   Glib.Test.Add_Func ("/link-button/label", Test_Label'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Link_Button;
