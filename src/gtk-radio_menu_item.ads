with Gtk.Check_Menu_Item;

package Gtk.Radio_Menu_Item is

   type Gtk_Radio_Menu_Item is new Check_Menu_Item.Gtk_Check_Menu_Item
     with private;

   procedure New_From_Widget (Radio_Menu_Item : out Gtk_Radio_Menu_Item);

   procedure New_From_Widget (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
                              Label           : in  String);

private

   type Gtk_Radio_Menu_Item is new Check_Menu_Item.Gtk_Check_Menu_Item
     with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkradiomenuitem.h \
   --  mapping:                     gtk_radio_menu_item_get_type
   --  mapping: NOT_IMPLEMENTED gtkradiomenuitem.h gtk_radio_menu_item_new
   --  mapping: NOT_IMPLEMENTED gtkradiomenuitem.h \
   --  mapping:                 gtk_radio_menu_item_new_with_label
   --  mapping: NOT_IMPLEMENTED gtkradiomenuitem.h gtk_radio_menu_item_group
   --  mapping: NOT_IMPLEMENTED gtkradiomenuitem.h \
   --  mapping:                 gtk_radio_menu_item_set_group

end Gtk.Radio_Menu_Item;
