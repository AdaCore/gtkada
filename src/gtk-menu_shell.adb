
package body Gtk.Menu_Shell is

   ------------
   -- Append --
   ------------

   procedure Append
     (Menu_Shell : in Gtk_Menu_Shell'Class;
      Child      : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal
        (Menu_Shell : System.Address;
         Child      : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_append");
   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Child));
   end Append;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate (Menu_Shell : in Gtk_Menu_Shell'Class)
   is
      procedure Internal (Menu_Shell : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_deactivate");
   begin
      Internal (Get_Object (Menu_Shell));
   end Deactivate;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Menu_Shell : in Gtk_Menu_Shell'Class;
      Child      : in Gtk.Widget.Gtk_Widget'Class;
      Position   : in Gint)
   is
      procedure Internal (Menu_Shell : System.Address;
                          Child      : System.Address;
                          Position   : Gint);
      pragma Import (C, Internal, "gtk_menu_shell_insert");
   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Child), Position);
   end Insert;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Menu_Shell : in Gtk_Menu_Shell'Class;
      Child      : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal (Menu_Shell : System.Address;
                          Child      : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_prepend");
   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Child));
   end Prepend;
end Gtk.Menu_Shell;
