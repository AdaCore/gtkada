with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Main_Window_Pkg; use Main_Window_Pkg;
with Open_File_Selection_Pkg; use Open_File_Selection_Pkg;
with About_Dialog_Pkg; use About_Dialog_Pkg;
with Save_File_Selection_Pkg; use Save_File_Selection_Pkg;

procedure Gladeedit is
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Main_Window);
   Show_All (Main_Window);
   Gtk.Main.Main;
end Gladeedit;
