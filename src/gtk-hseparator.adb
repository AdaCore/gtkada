

package body Gtk.HSeparator is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_HSeparator)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hseparator_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

end Gtk.HSeparator;
