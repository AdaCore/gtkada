
with Gtk.Adjustment;
with Gtk.Scrollbar;

package Gtk.HScrollbar is

   type Gtk_HScrollbar is new Gtk.Scrollbar.Gtk_Scrollbar with private;

   procedure Gtk_New (Widget     : out Gtk_HScrollbar;
                      Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);

private
   type Gtk_HScrollbar is new Gtk.Scrollbar.Gtk_Scrollbar with null record;

   --  mapping: NOT_IMPLEMENTED gtkhscrollbar.h gtk_hscrollbar_get_type
   --  mapping: Gtk_New gtkhscrollbar.h gtk_hscrollbar_new
end Gtk.HScrollbar;
