
with Gtk.Separator;

package Gtk.VSeparator is

   type Gtk_VSeparator is new Gtk.Separator.Gtk_Separator with private;

   procedure Gtk_New (Widget : out Gtk_VSeparator);

private
   type Gtk_VSeparator is new Gtk.Separator.Gtk_Separator with null record;

   --  mapping: NOT_IMPLEMENTED gtkvseparator.h gtk_vseparator_get_type
   --  mapping: Gtk_New gtkvseparator.h gtk_vseparator_new
end Gtk.VSeparator;
