
with Gtk.Bin;

package Gtk.Item is

   type Gtk_Item is new Gtk.Bin.Gtk_Bin with private;

   procedure Item_Select (The_Item : in Gtk_Item'Class);
   --  mapping: Item_Select gtkitem.h gtk_item_select

   procedure Item_Deselect (The_Item : in Gtk_Item'Class);
   --  mapping: Item_Deselect gtkitem.h gtk_item_deselect

   procedure Toggle (The_Item : in Gtk_Item'Class);
   --  mapping: Toggle gtkitem.h gtk_item_toggle

   --  mapping: NOT_IMPLEMENTED gtkitem.h gtk_item_get_type

private

   type Gtk_Item is new Gtk.Bin.Gtk_Bin with null record;


end Gtk.Item;
