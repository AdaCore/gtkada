

package body Gtk.VRuler is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_VRuler)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vruler_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

end Gtk.VRuler;
