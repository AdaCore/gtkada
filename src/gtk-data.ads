with Gtk.Object;

package Gtk.Data is

   type Gtk_Data is new Object.Gtk_Object with private;

private

   type Gtk_Data is new Object.Gtk_Object with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkdata.h gtk_data_get_type

end Gtk.Data;
