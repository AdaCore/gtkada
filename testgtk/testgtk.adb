
with Gtk.Main;
with Gtk.Rc;
with Gtk.Widget;
with Gtk.Window;
with Main_Windows;

procedure Testgtk is
   Win : Main_Windows.Main_Window;
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk.Rc.Parse ("testgtkrc");
   Main_Windows.Gtk_New (Win);
   Main_Windows.Show_All (Win);
   Gtk.Main.Main;
end Testgtk;
