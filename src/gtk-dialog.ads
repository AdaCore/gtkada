
with Gtk.Box;
with Gtk.Window;

package Gtk.Dialog is

   type Gtk_Dialog is new Gtk.Window.Gtk_Window with private;

   function Get_Action_Area (Widget : in Gtk_Dialog'Class)
                             return      Gtk.Box.Gtk_Box;
   function Get_Vbox (Widget : in Gtk_Dialog'Class)
                      return      Gtk.Box.Gtk_Box;
   procedure Gtk_New (Widget : out Gtk_Dialog);

private
   type Gtk_Dialog is new Gtk.Window.Gtk_Window with null record;

   --  mapping: Get_Action_Area gtkdialog.h GtkDialog->action_area
   --  mapping: NOT_IMPLEMENTED gtkdialog.h gtk_dialog_get_type
   --  mapping: Get_Vbox gtkdialog.h GtkDialog->vbox
   --  mapping: Gtk_New gtkdialog.h gtk_dialog_new
end Gtk.Dialog;
