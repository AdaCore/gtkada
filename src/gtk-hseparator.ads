
with Gtk.Separator;

package Gtk.HSeparator is

   type Gtk_HSeparator is new Gtk.Separator.Gtk_Separator with private;

   procedure Gtk_New (Widget : out Gtk_HSeparator);

private
   type Gtk_HSeparator is new Gtk.Separator.Gtk_Separator with null record;

   --  mapping: NOT_IMPLEMENTED gtkhseparator.h gtk_hseparator_get_type
   --  mapping: Gtk_New gtkhseparator.h gtk_hseparator_new
end Gtk.HSeparator;
