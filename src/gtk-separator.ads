
with Gtk.Widget;

package Gtk.Separator is

   type Gtk_Separator is new Gtk.Widget.Gtk_Widget with private;


private
   type Gtk_Separator is new Gtk.Widget.Gtk_Widget with null record;

   --  mapping: NOT_IMPLEMENTED gtkseparator.h gtk_separator_get_type
end Gtk.Separator;
