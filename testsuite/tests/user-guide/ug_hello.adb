--  Code sample quoted by the "Starting an application" chapter of the
--  User's Guide (docs/gtkada_ug/applications.rst). The text between the
--  START and END markers is included verbatim in the guide.
--
--  This procedure is only compiled by the test, not run: it would pass the
--  test driver's own switches to the application. User_Guide runs the same
--  handlers with a command line of its choosing instead.

--  START main
with Ada.Command_Line;
with Glib;             use Glib;
with Glib.Application; use Glib.Application;
with Gtk.Application;  use Gtk.Application;
with UG_Application;

procedure UG_Hello is
   App    : Gtk_Application;
   Status : Gint;
begin
   Gtk_New (App, "com.example.hello", G_Application_Flags_None);
   App.On_Activate (UG_Application.On_Activate'Access);

   --  Run the application until its last window is closed
   Status := App.Run;
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status (Status));
end UG_Hello;
--  END main
