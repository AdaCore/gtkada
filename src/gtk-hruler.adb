

package body Gtk.HRuler is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_HRuler)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hruler_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

end Gtk.HRuler;
