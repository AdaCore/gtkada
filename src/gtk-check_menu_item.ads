
with Gtk.Menu_Item;

package Gtk.Check_Menu_Item is

   type Gtk_Check_Menu_Item is new Gtk.Menu_Item.Gtk_Menu_Item with private;

   procedure Gtk_New (Widget : out Gtk_Check_Menu_Item;
                      Label  : in String);
   procedure Gtk_New (Widget : out Gtk_Check_Menu_Item);
   procedure Set_Show_Toggle
      (Menu_Item : in Gtk_Check_Menu_Item'Class;
       Always    : in Boolean);
   procedure Set_State
      (Check_Menu_Item : in Gtk_Check_Menu_Item'Class;
       State           : in Gint);
   procedure Toggled (Check_Menu_Item : in Gtk_Check_Menu_Item'Class);

private
   type Gtk_Check_Menu_Item is new Gtk.Menu_Item.Gtk_Menu_Item
     with null record;

   --  mapping: NOT_IMPLEMENTED gtkcheckmenuitem.h gtk_check_menu_item_get_type
   --  mapping: Gtk_New gtkcheckmenuitem.h gtk_check_menu_item_new_with_label
   --  mapping: Gtk_New gtkcheckmenuitem.h gtk_check_menu_item_new
   --  mapping: Set_Show_Toggle gtkcheckmenuitem.h \
   --  mapping:      gtk_check_menu_item_set_show_toggle
   --  mapping: Set_State gtkcheckmenuitem.h gtk_check_menu_item_set_state
   --  mapping: Toggled gtkcheckmenuitem.h gtk_check_menu_item_toggled
end Gtk.Check_Menu_Item;
