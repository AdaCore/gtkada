
with Gtk.Container;
with Gtk.Widget;

package Gtk.Box is

   type Box is new Gtk.Container.Container with private;

   procedure Pack_Start (In_Box  : in Box'Class;
                         Widget  : in Gtk.Widget.Widget'Class;
                         Expand  : in Boolean;
                         Fill    : in Boolean;
                         Padding : in GInt);

   --  mapping: Pack_Start gtkbox.h gtk_box_pack_start

private

   type Box is new Gtk.Container.Container with null record;

end Gtk.Box;
