
with Gtk.Box;

package Gtk.Hbox is

   type Gtk_Hbox is new Gtk.Box.Gtk_Box with private;

   procedure Gtk_New (Widget      : out Gtk_Hbox;
                      Homogeneous : in Boolean;
                      Spacing     : in Gint);
   --  mapping: New gtkhbox.h gtk_hbox_new


   --  mapping: NOT_IMPLEMENTED gtkhbox.h gtk_hbox_get_type

private

   type Gtk_Hbox is new Gtk.Box.Gtk_Box with null record;

end Gtk.Hbox;
