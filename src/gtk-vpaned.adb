

package body Gtk.VPaned is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_VPaned)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vpaned_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

end Gtk.VPaned;
