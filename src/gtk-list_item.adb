

package body Gtk.List_Item is

   --------------
   -- Deselect --
   --------------

   procedure Deselect (List_Item : in Gtk_List_Item'Class)
   is
      procedure Internal (List_Item : in System.Address);
      pragma Import (C, Internal, "gtk_list_item_deselect");
   begin
      Internal (Get_Object (List_Item));
   end Deselect;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_List_Item;
                      Label  : in String)
   is
      function Internal (Label  : in String)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_list_item_new_with_label");
   begin
      Set_Object (Widget, Internal (Label & Ascii.NUL));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_List_Item)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_list_item_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ----------------
   -- Gtk_Select --
   ----------------

   procedure Gtk_Select (List_Item : in Gtk_List_Item'Class)
   is
      procedure Internal (List_Item : in System.Address);
      pragma Import (C, Internal, "gtk_list_item_select");
   begin
      Internal (Get_Object (List_Item));
   end Gtk_Select;

end Gtk.List_Item;
