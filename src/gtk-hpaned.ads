
with Gtk.Paned;

package Gtk.HPaned is

   type Gtk_HPaned is new Gtk.Paned.Gtk_Paned with private;

   procedure Gtk_New (Widget : out Gtk_HPaned);

private
   type Gtk_HPaned is new Gtk.Paned.Gtk_Paned with null record;

   --  mapping: NOT_IMPLEMENTED gtkhpaned.h gtk_hpaned_get_type
   --  mapping: Gtk_New gtkhpaned.h gtk_hpaned_new
end Gtk.HPaned;
