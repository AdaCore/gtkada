with Gtk.Window;

package Gtk.Dialog is

   type Gtk_Dialog is new Window.Gtk_Window with private;

   procedure Gtk_New (Dialog : out Gtk_Dialog);
   --  mapping: Gtk_New gtkdialog.h gtk_dialog_new

private

   type Gtk_Dialog is new Window.Gtk_Window with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkdialog.h gtk_dialog_get_type

end Gtk.Dialog;
