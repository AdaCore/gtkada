
package body Gtk.Hbox is

   procedure Gtk_New (Widget      : out Hbox;
                      Homogeneous : in Boolean;
                      Spacing     : in GInt)
   is
      function Internal (Homogeneous : GInt;
                         Spacing     : GInt) return System.Address;
      pragma Import (C, Internal, "gtk_hbox_new");
   begin
      Set_Object (Widget, Internal (Boolean'Pos (Homogeneous),
                                    Spacing));
   end Gtk_New;
end Gtk.Hbox;
