
with Gtk.Container;
with Gtk.Widget;

package Gtk.Box is

   type Gtk_Box is new Gtk.Container.Gtk_Container with private;

   procedure Gtk_Pack_Start (Box     : in Gtk_Box'Class;
                             Widget  : in Gtk.Widget.Gtk_Widget'Class;
                             Expand  : in Boolean;
                             Fill    : in Boolean;
                             Padding : in GInt);

   --  mapping: Gtk_Pack_Start gtkbox.h gtk_box_pack_start

private

   type Gtk_Box is new Gtk.Container.Gtk_Container with null record;

end Gtk.Box;
