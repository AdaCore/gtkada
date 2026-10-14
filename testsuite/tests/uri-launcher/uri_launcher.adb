with Ada.Command_Line;
with Glib;             use Glib;
with Glib.Error;
with Glib.Main;
with Glib.Object;      use Glib.Object;
with Glib.Test;        use Glib.Test;
with Gtk.Main;
with Gtk.Uri_Launcher; use Gtk.Uri_Launcher;

procedure Uri_Launcher is
   use type Glib.Error.GError;
   Done : Boolean := False;
   procedure Test_Uri with Convention => C;
   procedure Test_Error with Convention => C;
   procedure On_Done (Source : access GObject_Record'Class; Res : G_Async_Result);

   procedure On_Done (Source : access GObject_Record'Class; Res : G_Async_Result)
   is
      Error : Glib.Error.GError;
      Success : constant Boolean :=
        Gtk_Uri_Launcher (Source).Launch_Finish (Res, Error);
   begin
      Assert_False (Success);
      Assert_True (Error /= null);
      Glib.Error.Error_Free (Error);
      Done := True;
   end On_Done;

   procedure Test_Uri is
      Launcher : Gtk_Uri_Launcher;
   begin
      Gtk_New (Launcher, "https://www.gtk.org/");
      Assert_Cmpstr_Eq (Launcher.Get_Uri, "https://www.gtk.org/");
      Launcher.Set_Uri ("file:///tmp/gtkada%20test.txt");
      Assert_Cmpstr_Eq (Launcher.Get_Uri, "file:///tmp/gtkada%20test.txt");
      Launcher.Set_Uri ("");
      Assert_Cmpstr_Eq (Launcher.Get_Uri, "");
      Launcher.Unref;
   end Test_Uri;

   procedure Test_Error is
      Launcher : Gtk_Uri_Launcher;
      Dispatched : Boolean;
   begin
      --  Invalid URI exercises finish without launching a desktop app.
      Gtk_New (Launcher, "this is not a URI");
      Launcher.Launch (null, null, On_Done'Unrestricted_Access);
      for I in 1 .. 5_000 loop
         exit when Done;
         Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
         delay 0.001;
      end loop;
      Assert_True (Done);
      Launcher.Unref;
   end Test_Error;
begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/uri-launcher/uri", Test_Uri'Unrestricted_Access);
   Add_Func ("/uri-launcher/error", Test_Error'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Uri_Launcher;
