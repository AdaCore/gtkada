
with Gtk.Box;

package Gtk.Hbox is

   type Gtk_Hbox is new Gtk.Box.Gtk_Box with private;

   procedure Gtk_New (Widget      : out Gtk_Hbox;
                      Homogeneous : in Boolean;
                      Spacing     : in GInt);
   --  mapping: Gtk_New gtkhbox.h gtk_hbox_new


private

   type Gtk_Hbox is new Gtk.Box.Gtk_Box with null record;

end Gtk.Hbox;
