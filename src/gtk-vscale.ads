
with Gtk.Adjustment;
with Gtk.Scale;

package Gtk.VScale is

   type Gtk_VScale is new Gtk.Scale.Gtk_Scale with private;

   procedure Gtk_New (Widget     : out Gtk_VScale;
                      Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);

private
   type Gtk_VScale is new Gtk.Scale.Gtk_Scale with null record;

   --  mapping: NOT_IMPLEMENTED gtkvscale.h gtk_vscale_get_type
   --  mapping: Gtk_New gtkvscale.h gtk_vscale_new
end Gtk.VScale;
