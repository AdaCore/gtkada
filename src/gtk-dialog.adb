package body Gtk.Dialog is


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Dialog : out Gtk_Dialog) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_dialog_new");
   begin
      Set_Object (Dialog, Internal);
   end Gtk_New;

end Gtk.Dialog;
