
with Gtk.Bin;

package Gtk.Event_Box is

   type Gtk_Event_Box is new Gtk.Bin.Gtk_Bin with private;

   procedure Gtk_New (Widget : out Gtk_Event_Box);

private
   type Gtk_Event_Box is new Gtk.Bin.Gtk_Bin with null record;

   --  mapping: NOT_IMPLEMENTED gtkeventbox.h gtk_event_box_get_type
   --  mapping: Gtk_New gtkeventbox.h gtk_event_box_new
end Gtk.Event_Box;
