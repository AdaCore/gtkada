
with Gtk.Container;

package Gtk.Button is

   type Button is new Gtk.Container.Container with private;

   procedure Gtk_New (Widget : out Button);
   procedure Gtk_New (Widget : out Button'Class;
                      Label  : in String);

   --  mapping: New gtkbutton.h gtk_button_new
   --  mapping: New gtkbutton.h gtk_button_new_with_label

private

   type Button is new Gtk.Container.Container with null record;

end Gtk.Button;
