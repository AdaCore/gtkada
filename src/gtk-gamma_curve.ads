with Gtk.Vbox;
with Gtk.Curve;

package Gtk.Gamma_Curve is

   type Gtk_Gamma_Curve is new Gtk.Vbox.Gtk_Vbox with private;

   procedure Gtk_New (Widget : out Gtk_Gamma_Curve);

   function Get_Curve (Widget : in Gtk_Gamma_Curve'Class)
                       return Gtk.Curve.Gtk_Curve;

   function Get_Gamma (Widget : in Gtk_Gamma_Curve'Class)
                       return Gfloat;

private

   type Gtk_Gamma_Curve is new Gtk.Vbox.Gtk_Vbox with null record;

   --  mapping: Gtk_New gtkgamma.h gtk_gamma_curve_new

   --  mapping: NOT_IMPLEMENTED gtkgamma.h gtk_gamma_curve_get_type

end Gtk.Gamma_Curve;
