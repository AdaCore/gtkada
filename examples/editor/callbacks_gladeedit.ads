with Gtk.Signal;
with Gtk.Window; use Gtk.Window;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Button; use Gtk.Button;
with Gtk.Text; use Gtk.Text;
with Gtk.File_Selection; use Gtk.File_Selection;

package Callbacks_Gladeedit is

   package Window_Callback is new
     Gtk.Signal.Void_Callback (Gtk_Window_Record);

   package Menu_Item_Callback is new
     Gtk.Signal.Void_Callback (Gtk_Menu_Item_Record);

   package Button_Callback is new
     Gtk.Signal.Void_Callback (Gtk_Button_Record);

   package Text_Callback is new
     Gtk.Signal.Void_Callback (Gtk_Text_Record);

   package File_Selection_Callback is new
     Gtk.Signal.Void_Callback (Gtk_File_Selection_Record);

end Callbacks_Gladeedit;
