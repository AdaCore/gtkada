
with Gtk.Widget;

package Gtk.Container is

   type Gtk_Container is new Gtk.Widget.Gtk_Widget with private;

   procedure Gtk_Add (Container : in Gtk_Container'Class;
                      Widget    : in Gtk.Widget.Gtk_Widget'Class);

   procedure Gtk_Border_Width (Container    : in Gtk_Container'Class;
                               Border_Width : in GInt);

   --  mapping: Gtk_Add  gtkcontainer.h gtk_container_add
   --  mapping: Gtk_Border_Width gtkcontainer.h gtk_container_border_width

private

   type Gtk_Container is new Gtk.Widget.Gtk_Widget with null record;

end Gtk.Container;
