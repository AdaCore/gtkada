
with Gtk.Adjustment;
with Gtk.Scale;

package Gtk.HScale is

   type Gtk_HScale is new Gtk.Scale.Gtk_Scale with private;

   procedure Gtk_New (Widget     : out Gtk_HScale;
                      Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);

private
   type Gtk_HScale is new Gtk.Scale.Gtk_Scale with null record;

   --  mapping: NOT_IMPLEMENTED gtkhscale.h gtk_hscale_get_type
   --  mapping: Gtk_New gtkhscale.h gtk_hscale_new
end Gtk.HScale;
