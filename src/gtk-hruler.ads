
with Gtk.Ruler;

package Gtk.HRuler is

   type Gtk_HRuler is new Gtk.Ruler.Gtk_Ruler with private;

   procedure Gtk_New (Widget : out Gtk_HRuler);

private
   type Gtk_HRuler is new Gtk.Ruler.Gtk_Ruler with null record;

   --  mapping: NOT_IMPLEMENTED gtkhruler.h gtk_hruler_get_type
   --  mapping: Gtk_New gtkhruler.h gtk_hruler_new
end Gtk.HRuler;
