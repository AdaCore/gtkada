
with Gtk.GRange;
with Gtk.Adjustment;

package Gtk.Scrollbar is

   type Gtk_Scrollbar is new Gtk.GRange.Gtk_Range with private;

   procedure Gtk_New_Hscrollbar
     (Widget     : out Gtk_Scrollbar;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);
   procedure Gtk_New_Vscrollbar
     (Widget     : out Gtk_Scrollbar;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class);

private
   type Gtk_Scrollbar is new Gtk.GRange.Gtk_Range with null record;

   --  mapping: NOT_IMPLEMENTED gtkscrollbar.h gtk_scrollbar_get_type
   --  mapping: Gtk_New_Hscrollbar gtkhscrollbar.h gtk_hscrollbar_new
   --  mapping: Gtk_New_Vscrollbar gtkvscrollbar.h gtk_vscrollbar_new
end Gtk.Scrollbar;
