
with Gtk.Widget;

package Gtk.Progress_Bar is

   type Gtk_Progress_Bar is new Gtk.Widget.Gtk_Widget with private;

   procedure Gtk_New (Widget : out Gtk_Progress_Bar);
   procedure Update
      (Pbar       : in Gtk_Progress_Bar'Class;
       Percentage : in Gfloat);

private
   type Gtk_Progress_Bar is new Gtk.Widget.Gtk_Widget with null record;

   --  mapping: NOT_IMPLEMENTED gtkprogressbar.h gtk_progress_bar_get_type
   --  mapping: Gtk_New gtkprogressbar.h gtk_progress_bar_new
   --  mapping: Update gtkprogressbar.h gtk_progress_bar_update
end Gtk.Progress_Bar;
