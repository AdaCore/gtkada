package body Gtk.Check_Button is


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Widget : out Gtk_Check_Button) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_check_button_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Widget : out Gtk_Check_Button;
                         With_Label : in String) is
      function Internal (Label : in String) return System.Address;
      pragma Import (C, Internal, "gtk_check_button_new_with_label");
   begin
      Set_Object (Widget, Internal (With_Label & ASCII.NUL));
   end Gtk_New;

end Gtk.Check_Button;
