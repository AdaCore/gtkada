--  Exercise the Gtk.Progress_Bar API through the generated Ada binding.

with Ada.Command_Line;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Test;       use Glib.Test;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Main;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Pango.Layout;     use Pango.Layout;

procedure Progress_Bar is
   procedure Test_Defaults with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Text with Convention => C;
   procedure Test_Pulse with Convention => C;

   procedure Test_Defaults is
      Bar : Gtk_Progress_Bar;
   begin
      Gtk_New (Bar);
      Ref_Sink (Bar);
      Assert_Cmpfloat_Eq (Bar.Get_Fraction, 0.0);
      Assert_Cmpfloat_Eq (Bar.Get_Pulse_Step, 0.1);
      Assert_False (Bar.Get_Inverted);
      Assert_False (Bar.Get_Show_Text);
      Assert_Cmpstr_Eq (Bar.Get_Text, "");
      Assert_True (Bar.Get_Ellipsize = Ellipsize_None);
      Assert_True (Bar.Get_Orientation = Orientation_Horizontal);
      Unref (Bar);
   end Test_Defaults;

   procedure Test_Properties is
      Bar : constant Gtk_Progress_Bar := Gtk_Progress_Bar_New;
   begin
      Ref_Sink (Bar);
      for I in 0 .. 4 loop
         Bar.Set_Fraction (Gdouble (I) / 4.0);
         Assert_Cmpfloat_Eq (Bar.Get_Fraction, Gdouble (I) / 4.0);
      end loop;
      Assert_Cmpfloat_Eq (Get_Property (Bar, Fraction_Property), 1.0);
      Set_Property (Bar, Fraction_Property, 0.5);
      Assert_Cmpfloat_Eq (Bar.Get_Fraction, 0.5);
      Bar.Set_Inverted (True);
      Assert_True (Bar.Get_Inverted);
      Bar.Set_Inverted (False);
      Assert_False (Bar.Get_Inverted);
      Bar.Set_Orientation (Orientation_Vertical);
      Assert_True (Bar.Get_Orientation = Orientation_Vertical);
      for Mode in Pango_Ellipsize_Mode loop
         Bar.Set_Ellipsize (Mode);
         Assert_True (Bar.Get_Ellipsize = Mode);
      end loop;
      Unref (Bar);
   end Test_Properties;

   procedure Test_Text is
      Bar : constant Gtk_Progress_Bar := Gtk_Progress_Bar_New;
   begin
      Ref_Sink (Bar);
      Bar.Set_Show_Text (True);
      Assert_True (Bar.Get_Show_Text);
      Bar.Set_Text ("Loading — café");
      Assert_Cmpstr_Eq (Bar.Get_Text, "Loading — café");
      Bar.Set_Text;
      Assert_Cmpstr_Eq (Bar.Get_Text, "");
      Bar.Set_Show_Text (False);
      Assert_False (Bar.Get_Show_Text);
      Unref (Bar);
   end Test_Text;

   procedure Test_Pulse is
      Bar : constant Gtk_Progress_Bar := Gtk_Progress_Bar_New;
   begin
      Ref_Sink (Bar);
      Bar.Set_Fraction (0.5);
      Bar.Set_Pulse_Step (0.2);
      Assert_Cmpfloat_Eq (Bar.Get_Pulse_Step, 0.2);
      for I in 1 .. 10 loop
         Bar.Pulse;
      end loop;
      --  Activity mode leaves the stored fraction intact. Setting a new
      --  fraction returns the widget to determinate progress.
      Assert_Cmpfloat_Eq (Bar.Get_Fraction, 0.5);
      Bar.Set_Fraction (0.75);
      Assert_Cmpfloat_Eq (Bar.Get_Fraction, 0.75);
      Unref (Bar);
   end Test_Pulse;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/progress-bar/defaults", Test_Defaults'Unrestricted_Access);
   Add_Func ("/progress-bar/properties", Test_Properties'Unrestricted_Access);
   Add_Func ("/progress-bar/text", Test_Text'Unrestricted_Access);
   Add_Func ("/progress-bar/pulse", Test_Pulse'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Progress_Bar;
