with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Create_Main_Window;
with Gtk.File_Selection; use Gtk.File_Selection;
with Create_Open_File_Selection;
with Gtk.Dialog; use Gtk.Dialog;
with Create_About_Dialog;
with Gtk.File_Selection; use Gtk.File_Selection;
with Create_Save_File_Selection;
with Gtk.Util; use Gtk.Util;

procedure Gladeedit is
   Main_Window : Gtk_Window;
   Open_File_Selection : Gtk_File_Selection;
   About_Dialog : Gtk_Dialog;
   Save_File_Selection : Gtk_File_Selection;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Main_Window := Create_Main_Window;
   Widget.Show_All (Gtk_Widget (Main_Window));
   Gtk.Main.Main;
end Gladeedit;
