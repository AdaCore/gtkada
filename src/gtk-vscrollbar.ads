
with Gtk.Adjustment;
with Gtk.Scrollbar;

package Gtk.VScrollbar is

   type Gtk_VScrollbar is new Gtk.Scrollbar.Gtk_Scrollbar with private;

   procedure Gtk_New (Widget     : out Gtk_VScrollbar;
                      Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);

private
   type Gtk_VScrollbar is new Gtk.Scrollbar.Gtk_Scrollbar with null record;

   --  mapping: NOT_IMPLEMENTED gtkvscrollbar.h gtk_vscrollbar_get_type
   --  mapping: Gtk_New gtkvscrollbar.h gtk_vscrollbar_new
end Gtk.VScrollbar;
