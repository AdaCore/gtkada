with Gtk.Container;

package Gtk.Bin is

   type Gtk_Bin is new Container.Gtk_Container with private;

private

   type Gtk_Bin is new Container.Gtk_Container with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkbin.h gtk_bin_get_type

end Gtk.Bin;
