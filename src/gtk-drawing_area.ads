
with Gtk.Widget;

package Gtk.Drawing_Area is

   type Gtk_Drawing_Area is new Gtk.Widget.Gtk_Widget with private;

   procedure Gtk_New (Widget : out Gtk_Drawing_Area);
   procedure Size
     (Darea  : in Gtk_Drawing_Area'Class;
      Width  : in Gint;
      Height : in Gint);

private

   type Gtk_Drawing_Area is new Gtk.Widget.Gtk_Widget with null record;

   --  mapping: NOT_IMPLEMENTED gtkdrawingarea.h gtk_drawing_area_get_type
   --  mapping: Gtk_New gtkdrawingarea.h gtk_drawing_area_new
   --  mapping: Size gtkdrawingarea.h gtk_drawing_area_size

end Gtk.Drawing_Area;
