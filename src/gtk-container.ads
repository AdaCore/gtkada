
with Gtk.Widget;

package Gtk.Container is

   type Container is new Gtk.Widget.Widget with private;

   procedure Add (In_Container : in Container'Class;
                  Widget       : in Gtk.Widget.Widget'Class);

   procedure Border_Width (Of_Container : in Container'Class;
                           Border_Width : in GInt);

   --  mapping: Add  gtkcontainer.h gtk_container_add
   --  mapping: Border_Width gtkcontainer.h gtk_container_border_width

private

   type Container is new Gtk.Widget.Widget with null record;

end Gtk.Container;
