with Gtk.Main;   use Gtk.Main;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Window; use Gtk.Window;
with Calendar_Combo; use Calendar_Combo;

procedure Main is
   Win : Gtk_Window;
   Cal : Gtk_Calendar_Combo;
begin
   Gtk.Main.Init;

   Gtk_New (Win, Window_Toplevel);

   Gtk_New (Cal);
   Add (Win, Cal);

   Show_All (Win);

   Gtk.Main.Main;
end Main;
