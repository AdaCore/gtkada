
with Gtk.Paned;

package Gtk.VPaned is

   type Gtk_VPaned is new Gtk.Paned.Gtk_Paned with private;

   procedure Gtk_New (Widget : out Gtk_VPaned);

private
   type Gtk_VPaned is new Gtk.Paned.Gtk_Paned with null record;

   --  mapping: NOT_IMPLEMENTED gtkvpaned.h gtk_vpaned_get_type
   --  mapping: Gtk_New gtkvpaned.h gtk_vpaned_new
end Gtk.VPaned;
