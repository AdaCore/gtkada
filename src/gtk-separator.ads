
with Gtk.Widget;

package Gtk.Separator is

   type Gtk_Separator is new Gtk.Widget.Gtk_Widget with private;

   procedure Gtk_New_Hseparator (Widget : out Gtk_Separator);
   procedure Gtk_New_Vseparator (Widget : out Gtk_Separator);

private
   type Gtk_Separator is new Gtk.Widget.Gtk_Widget with null record;

   --  mapping: NOT_IMPLEMENTED gtkseparator.h gtk_separator_get_type
   --  mapping: Gtk_New_Hseparator gtkhseparator.h gtk_hseparator_new
   --  mapping: Gtk_New_Vseparator gtkvseparator.h gtk_vseparator_new
end Gtk.Separator;
