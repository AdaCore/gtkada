package body Gtk.Input_Dialog is


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Input_Dialog : out Gtk_Input_Dialog) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_input_dialog_new");
   begin
      Set_Object (Input_Dialog, Internal);
   end Gtk_New;

end Gtk.Input_Dialog;
