
with Gtk.Container;

package Gtk.Button is

   type Gtk_Button is new Gtk.Container.Gtk_Container with private;

   procedure Gtk_New (Widget : out Gtk_Button);
   procedure Gtk_New (Widget : out Gtk_Button'Class;
                      Label  : in String);

   --  mapping: Gtk_New gtkbutton.h gtk_button_new
   --  mapping: Gtk_New gtkbutton.h gtk_button_new_with_label

private

   type Gtk_Button is new Gtk.Container.Gtk_Container with null record;

end Gtk.Button;
