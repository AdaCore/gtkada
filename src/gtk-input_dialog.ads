with Gtk.Dialog;


package Gtk.Input_Dialog is

   type Gtk_Input_Dialog is new Dialog.Gtk_Dialog with private;

   procedure Gtk_New (Input_Dialog : out Gtk_Input_Dialog);
   --  mapping: NOT_IMPLEMENTED gtkinputdialog.h gtk_input_dialog_new

private

   type Gtk_Input_Dialog is new Dialog.Gtk_Dialog with null record;

   --  mapping: NOT_IMPLEMENTED gtkinputdialog.h gtk_input_dialog_get_type

end Gtk.Input_Dialog;
