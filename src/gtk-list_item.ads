
with Gtk.Item;

package Gtk.List_Item is

   type Gtk_List_Item is new Gtk.Item.Gtk_Item with private;

   procedure Deselect (List_Item : in Gtk_List_Item'Class);
   procedure Gtk_New (Widget : out Gtk_List_Item;
                      Label  : in String);
   procedure Gtk_New (Widget : out Gtk_List_Item);
   procedure Gtk_Select (List_Item : in Gtk_List_Item'Class);

private
   type Gtk_List_Item is new Gtk.Item.Gtk_Item with null record;

   --  mapping: Deselect gtklistitem.h gtk_list_item_deselect
   --  mapping: NOT_IMPLEMENTED gtklistitem.h gtk_list_item_get_type
   --  mapping: Gtk_New gtklistitem.h gtk_list_item_new_with_label
   --  mapping: Gtk_New gtklistitem.h gtk_list_item_new
   --  mapping: Gtk_Select gtklistitem.h gtk_list_item_select
end Gtk.List_Item;
