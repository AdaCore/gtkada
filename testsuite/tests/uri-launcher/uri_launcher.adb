--  Test Gtk.Uri_Launcher URI access, constructors, and error paths that do
--  not require a registered desktop application or network access.

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
   procedure Test_Function_Constructor with Convention => C;
   procedure Test_Unsupported_Uri with Convention => C;
   procedure Test_Optional_Callback with Convention => C;
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

   --  The function constructor returns a live launcher initialized with its
   --  argument, just like Gtk_New.
   procedure Test_Function_Constructor is
      Launcher : constant Gtk_Uri_Launcher :=
        Gtk_Uri_Launcher_New ("https://example.invalid/");
   begin
      Assert_Cmpstr_Eq (Launcher.Get_Uri, "https://example.invalid/");
      Launcher.Unref;
   end Test_Function_Constructor;

   --  A malformed URI has no usable scheme, so GTK knows it cannot launch it.
   procedure Test_Unsupported_Uri is
      Launcher : Gtk_Uri_Launcher;
   begin
      Gtk_New (Launcher, "this is not a URI");
      Assert_False (Launcher.Can_Launch (null));
      Launcher.Unref;
   end Test_Unsupported_Uri;

   --  GTK accepts a null callback. With a malformed URI this starts an
   --  asynchronous error path without opening a chooser or external program.
   procedure Test_Optional_Callback is
      Launcher : Gtk_Uri_Launcher;
   begin
      Gtk_New (Launcher, "this is not a URI");
      Launcher.Launch (null, null, null);
      Assert_Cmpstr_Eq (Launcher.Get_Uri, "this is not a URI");
      Launcher.Unref;
   end Test_Optional_Callback;

   --  The callback form of an invalid URI completes with a recoverable error.
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
   Add_Func
     ("/uri-launcher/function-constructor",
      Test_Function_Constructor'Unrestricted_Access);
   Add_Func
     ("/uri-launcher/unsupported-uri",
      Test_Unsupported_Uri'Unrestricted_Access);
   Add_Func
     ("/uri-launcher/optional-callback",
      Test_Optional_Callback'Unrestricted_Access);
   Add_Func ("/uri-launcher/error", Test_Error'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Uri_Launcher;
