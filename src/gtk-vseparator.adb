

package body Gtk.VSeparator is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_VSeparator)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vseparator_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

end Gtk.VSeparator;
