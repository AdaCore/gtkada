
with Gtk.Ruler;

package Gtk.VRuler is

   type Gtk_VRuler is new Gtk.Ruler.Gtk_Ruler with private;

   procedure Gtk_New (Widget : out Gtk_VRuler);

private
   type Gtk_VRuler is new Gtk.Ruler.Gtk_Ruler with null record;

   --  mapping: NOT_IMPLEMENTED gtkvruler.h gtk_vruler_get_type
   --  mapping: Gtk_New gtkvruler.h gtk_vruler_new
end Gtk.VRuler;
