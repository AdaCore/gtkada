package body Gtk.Radio_Menu_Item is


   -----------------------
   --  New_From_Widget  --
   -----------------------

   procedure New_From_Widget (Radio_Menu_Item : out Gtk_Radio_Menu_Item) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_radio_menu_item_new_from_widget");
   begin
      Set_Object (Radio_Menu_Item, Internal);
   end New_From_Widget;


   -----------------------
   --  New_From_Widget  --
   -----------------------

   procedure New_From_Widget (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
                              Label           : in  String) is
      function Internal (Label : in String) return System.Address;
      pragma Import (C, Internal,
                     "gtk_radio_menu_item_new_with_label_from_widget");
   begin
      Set_Object (Radio_Menu_Item, Internal (Label & ASCII.NUL));
   end New_From_Widget;

end Gtk.Radio_Menu_Item;
