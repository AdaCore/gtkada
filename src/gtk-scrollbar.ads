
with Gtk.Gtk_Range;

package Gtk.Scrollbar is

   type Gtk_Scrollbar is new Gtk.Gtk_Range.Gtk_Range with private;


private
   type Gtk_Scrollbar is new Gtk.Gtk_Range.Gtk_Range with null record;

   --  mapping: NOT_IMPLEMENTED gtkscrollbar.h gtk_scrollbar_get_type
end Gtk.Scrollbar;
