
with Gtk.Bin;

package Gtk.Item is

   type Gtk_Item is new Bin.Gtk_Bin with private;

   procedure Item_Select (Item : in Gtk_Item'Class);
   --  mapping: Item_Select gtkitem.h gtk_item_select

   procedure Item_Deselect (Item : in Gtk_Item'Class);
   --  mapping: Item_Deselect gtkitem.h gtk_item_deselect

   procedure Toggle (Item : in Gtk_Item'Class);
   --  mapping: Toggle gtkitem.h gtk_item_toggle

private

   type Gtk_Item is new Gtk.Bin.Gtk_Bin with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkitem.h gtk_item_get_type

end Gtk.Item;
