
package body Gtk.Menu_Bar is

   ------------
   -- Append --
   ------------

   procedure Append
     (Menu_Bar : in Gtk_Menu_Bar'Class;
      Child    : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal
        (Menu_Bar : System.Address;
         Child    : System.Address);
      pragma Import (C, Internal, "gtk_menu_bar_append");
   begin
      Internal (Get_Object (Menu_Bar), Get_Object (Child));
   end Append;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Menu_Bar : in Gtk_Menu_Bar'Class;
      Child    : in Gtk.Widget.Gtk_Widget'Class;
      Position : in Gint)
   is
      procedure Internal (Menu_Bar : System.Address;
                          Child    : System.Address;
                          Position : Gint);
      pragma Import (C, Internal, "gtk_menu_bar_insert");
   begin
      Internal (Get_Object (Menu_Bar), Get_Object (Child), Position);
   end Insert;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Menu_Bar) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_menu_bar_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Menu_Bar : in Gtk_Menu_Bar'Class;
      Child    : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal (Menu_Bar : System.Address;
                          Child    : System.Address);
      pragma Import (C, Internal, "gtk_menu_bar_prepend");
   begin
      Internal (Get_Object (Menu_Bar), Get_Object (Child));
   end Prepend;

end Gtk.Menu_Bar;
