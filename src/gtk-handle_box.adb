

package body Gtk.Handle_Box is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Handle_Box)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_handle_box_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

end Gtk.Handle_Box;
