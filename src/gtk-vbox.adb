
package body Gtk.Vbox is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget      : out Gtk_Vbox;
                      Homogeneous : in Boolean;
                      Spacing     : in Gint)
   is
      function Internal (Homogeneous : Gint;
                         Spacing     : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_vbox_new");
   begin
      Set_Object (Widget, Internal (Boolean'Pos (Homogeneous),
                                    Spacing));
   end Gtk_New;

end Gtk.Vbox;
