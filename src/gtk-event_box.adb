

package body Gtk.Event_Box is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Event_Box)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_event_box_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

end Gtk.Event_Box;
