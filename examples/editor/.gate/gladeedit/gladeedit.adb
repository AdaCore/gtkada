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
   Gtk_New (Open_File_Selection);
   Show_All (Open_File_Selection);
   Gtk_New (About_Dialog);
   Show_All (About_Dialog);
   Gtk_New (Save_File_Selection);
   Show_All (Save_File_Selection);
   Gtk.Main.Main;
end Gladeedit;
