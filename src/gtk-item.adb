package body Gtk.Item is

   -----------------
   -- Item_Select --
   -----------------

   procedure Item_Select (The_Item : in Gtk_Item'Class) is
      procedure Internal (The_Item : in System.Address);
      pragma Import (C, Internal, "gtk_item_select");
   begin
      Internal (Get_Object (The_Item));
   end Item_Select;

   -------------------
   -- Item_Deselect --
   -------------------

   procedure Item_Deselect (The_Item : in Gtk_Item'Class) is
      procedure Internal (The_Item : in System.Address);
      pragma Import (C, Internal, "gtk_item_deselect");
   begin
      Internal (Get_Object (The_Item));
   end Item_Deselect;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (The_Item : in Gtk_Item'Class) is
      procedure Internal (The_Item : in System.Address);
      pragma Import (C, Internal, "gtk_item_toggle");
   begin
      Internal (Get_Object (The_Item));
   end Toggle;

end Gtk.Item;

