with Gtk.Item;

package Gtk.List_Item is

   type Gtk_List_Item is new Item.Gtk_Item with private;

   procedure Gtk_New (List_Item : out Gtk_List_Item);
   --  mapping: Gtk_New gtklistitem.h gtk_list_item_new

   procedure Gtk_New (List_Item : out Gtk_List_Item;
                      Label     : in String);
   --  mapping: Gtk_New gtklistitem.h gtk_list_item_new_with_label

   procedure Gtk_Select (List_Item : in out Gtk_List_Item'Class);
   --  mapping: Gtk_Select gtklistitem.h gtk_list_item_select

   procedure Deselect (List_Item : in out Gtk_List_Item'Class);
   --  mapping: Deselect gtklistitem.h gtk_list_item_deselect

private

   type Gtk_List_Item is new Item.Gtk_Item with null record;

   --  mapping: USE_OBJECT_ORIENTED gtklistitem.h gtk_list_item_get_type

end Gtk.List_Item;
