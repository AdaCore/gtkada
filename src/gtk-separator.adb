
package body Gtk.Separator is

   ------------------------
   -- Gtk_New_Hseparator --
   ------------------------

   procedure Gtk_New_Hseparator (Widget : out Gtk_Separator)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hseparator_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New_Hseparator;

   ------------------------
   -- Gtk_New_Vseparator --
   ------------------------

   procedure Gtk_New_Vseparator (Widget : out Gtk_Separator)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vseparator_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New_Vseparator;

end Gtk.Separator;
