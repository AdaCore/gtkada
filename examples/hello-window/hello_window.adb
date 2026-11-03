with Ada.Command_Line;
with Glib;                   use Glib;
with Glib.Application;       use Glib.Application;
with Gtk.Application;        use Gtk.Application;
with Gtk.Application_Window; use Gtk.Application_Window;
with Gtk.Window;             use Gtk.Window;

procedure Hello_Window is
   App    : Gtk_Application;
   Status : Gint;

   procedure Activate (Self : access Gapplication_Record'Class) is
      Application : constant Gtk_Application := Gtk_Application (Self);
      Window      : Gtk_Application_Window;
   begin
      --  Activation can happen again: present the existing window.
      if Application.Get_Active_Window /= null then
         Application.Get_Active_Window.Present;
         return;
      end if;

      --  Run has initialized GTK before calling this handler.
      Gtk_New (Window, Application);
      Window.Set_Title ("Hello, world!");
      Window.Set_Default_Size (320, 200);
      Window.Present;
   end Activate;

begin
   Gtk_New (App, "com.example.hello_window", G_Application_Flags_None);
   App.On_Activate (Activate'Unrestricted_Access);
   --  Closing the last window ends the application and returns from Run.
   Status := App.Run;
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status (Status));
end Hello_Window;
