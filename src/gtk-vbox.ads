
with Gtk.Box;

package Gtk.Vbox is

   type Gtk_Vbox is new Gtk.Box.Gtk_Box with private;

   procedure Gtk_New (Widget      : out Gtk_Vbox;
                      Homogeneous : in Boolean;
                      Spacing     : in Gint);
   --  mapping: New gtkvbox.h gtk_hbox_new


   --  mapping: NOT_IMPLEMENTED gtkvbox.h gtk_hbox_get_type

private

   type Gtk_Vbox is new Gtk.Box.Gtk_Box with null record;

end Gtk.Vbox;
