

package body Gtk.VScale is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget     : out Gtk_VScale;
                      Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class)
   is
      function Internal (Adjustment : in System.Address)
                         return          System.Address;
      pragma Import (C, Internal, "gtk_vscale_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Adjustment)));
   end Gtk_New;

end Gtk.VScale;
