
package body Gtk.Button is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Button) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_button_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   --------------
   --  New --
   --------------

   procedure Gtk_New (Widget : out Button'Class;
                      Label  : in String) is
      function New_Internal (S : String) return System.Address;
      pragma Import (C, New_Internal, "gtk_button_new_with_label");
   begin
      Set_Object (Widget, New_Internal (Label & Ascii.NUL));
   end Gtk_New;

end Gtk.Button;







