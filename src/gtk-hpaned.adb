

package body Gtk.HPaned is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_HPaned)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hpaned_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

end Gtk.HPaned;
