
with Gtk.Bin;

package Gtk.Handle_Box is

   type Gtk_Handle_Box is new Gtk.Bin.Gtk_Bin with private;

   procedure Gtk_New (Widget : out Gtk_Handle_Box);

private
   type Gtk_Handle_Box is new Gtk.Bin.Gtk_Bin with null record;

   --  mapping: NOT_IMPLEMENTED gtkhandlebox.h gtk_handle_box_get_type
   --  mapping: Gtk_New gtkhandlebox.h gtk_handle_box_new
end Gtk.Handle_Box;
