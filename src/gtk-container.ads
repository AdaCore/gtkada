with Gtk.Widget;

package Gtk.Container is

   type Gtk_Container is new Gtk.Widget.Gtk_Widget with private;

   procedure Add (In_Container : in Gtk_Container'Class;
                  Widget       : in Gtk.Widget.Gtk_Widget'Class);

   procedure Border_Width (Of_Container : in Gtk_Container'Class;
                           Border_Width : in GInt);

   --  mapping: Add  gtkcontainer.h gtk_container_add
   --  mapping: Border_Width gtkcontainer.h gtk_container_border_width

private

   type Gtk_Container is new Gtk.Widget.Gtk_Widget with null record;

end Gtk.Container;
