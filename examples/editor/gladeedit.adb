with Gtk; use Gtk;
with Gtk.Main;
with Main_Window_Pkg; use Main_Window_Pkg;

procedure Gladeedit is
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Main_Window);
   Show_All (Main_Window);
   Gtk.Main.Main;
end Gladeedit;
