package body Gtk.Item is

   -----------------
   -- Item_Select --
   -----------------

   procedure Item_Select (Item : in Gtk_Item'Class) is
      procedure Internal (Item : in System.Address);
      pragma Import (C, Internal, "gtk_item_select");
   begin
      Internal (Get_Object (Item));
   end Item_Select;

   -------------------
   -- Item_Deselect --
   -------------------

   procedure Item_Deselect (Item : in Gtk_Item'Class) is
      procedure Internal (Item : in System.Address);
      pragma Import (C, Internal, "gtk_item_deselect");
   begin
      Internal (Get_Object (Item));
   end Item_Deselect;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (Item : in Gtk_Item'Class) is
      procedure Internal (Item : in System.Address);
      pragma Import (C, Internal, "gtk_item_toggle");
   begin
      Internal (Get_Object (Item));
   end Toggle;

end Gtk.Item;

