
with Gtk.GRange;

package Gtk.Scrollbar is

   type Gtk_Scrollbar is new Gtk.GRange.Gtk_Range with private;


private
   type Gtk_Scrollbar is new Gtk.GRange.Gtk_Range with null record;

   --  mapping: NOT_IMPLEMENTED gtkscrollbar.h gtk_scrollbar_get_type
end Gtk.Scrollbar;
