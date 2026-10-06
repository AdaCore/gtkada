with Ada.Command_Line;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Glib.Test;         use Glib.Test;
with Gtk.Main;
with Gtk.Spinner;       use Gtk.Spinner;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;

procedure Spinner is

   procedure Test_Start_Stop with Convention => C;
   procedure Test_Property with Convention => C;
   procedure Test_Bind with Convention => C;

   procedure Test_Start_Stop is
      S : constant Gtk_Spinner := Gtk_Spinner_New;
   begin
      Ref_Sink (S);
      Assert_False (S.Get_Spinning);
      S.Start;
      Assert_True (S.Get_Spinning);
      S.Start;
      Assert_True (S.Get_Spinning);
      S.Stop;
      Assert_False (S.Get_Spinning);
      S.Stop;
      Assert_False (S.Get_Spinning);
      Unref (S);
   end Test_Start_Stop;

   procedure Test_Property is
      S : Gtk_Spinner;
   begin
      Gtk_New (S);
      Ref_Sink (S);
      Assert_False (Get_Property (S, Spinning_Property));
      S.Set_Spinning (True);
      Assert_True (Get_Property (S, Spinning_Property));
      S.Set_Spinning (False);
      Assert_False (Get_Property (S, Spinning_Property));
      Set_Property (S, Spinning_Property, True);
      Assert_True (S.Get_Spinning);
      Set_Property (S, Spinning_Property, False);
      Assert_False (S.Get_Spinning);
      S.Start;
      Assert_True (Get_Property (S, Spinning_Property));
      S.Stop;
      Assert_False (Get_Property (S, Spinning_Property));
      Unref (S);
   end Test_Property;

   procedure Test_Bind is
      S : constant Gtk_Spinner := Gtk_Spinner_New;
      B : constant Gtk_Toggle_Button := Gtk_Toggle_Button_New;
   begin
      Ref_Sink (S);
      Ref_Sink (B);
      B.Bind_Property ("active", S, "spinning");
      B.Set_Active (True);
      Assert_True (S.Get_Spinning);
      B.Set_Active (False);
      Assert_False (S.Get_Spinning);
      Unref (B);
      Unref (S);
   end Test_Bind;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/spinner/start-stop", Test_Start_Stop'Unrestricted_Access);
   Add_Func ("/spinner/property", Test_Property'Unrestricted_Access);
   Add_Func ("/spinner/bind", Test_Bind'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Spinner;
