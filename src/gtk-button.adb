
package body Gtk.Button is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Button) is
      function Gtk_New_Internal return System.Address;
      pragma Import (C, Gtk_New_Internal, "gtk_button_new");
   begin
      Set_Object (Widget, Gtk_New_Internal);
   end Gtk_New;

   --------------
   --  Gtk_New --
   --------------

   procedure Gtk_New (Widget : out Gtk_Button'Class;
                      Label  : in String) is
      function Gtk_New_Internal (S : String) return System.Address;
      pragma Import (C, Gtk_New_Internal, "gtk_button_new_with_label");
   begin
      Set_Object (Widget, Gtk_New_Internal (Label & Ascii.NUL));
   end Gtk_New;

end Gtk.Button;







