

package body Gtk.VScrollbar is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget     : out Gtk_VScrollbar;
                      Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class)
   is
      function Internal (Adjustment : in System.Address)
                         return          System.Address;
      pragma Import (C, Internal, "gtk_vscrollbar_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Adjustment)));
   end Gtk_New;

end Gtk.VScrollbar;
