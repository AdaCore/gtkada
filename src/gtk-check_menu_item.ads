with Gtk.Menu_Item;

package Gtk.Check_Menu_Item is

   type Gtk_Check_Menu_Item is new Menu_Item.Gtk_Menu_Item with private;


   procedure Gtk_New (Check_Menu_Item : out Gtk_Check_Menu_Item);
   --  mapping: Gtk_New gtkcheckmenuitem.h gtk_check_menu_item_new

   procedure Gtk_New (Check_Menu_Item : out Gtk_Check_Menu_Item;
                      Label            : in  String);
   --  mapping: Gtk_New gtkcheckmenuitem.h gtk_check_menu_item_new_with_label

   procedure Set_State (Check_Menu_Item : in out Gtk_Check_Menu_Item'Class;
                        Activated : in Boolean);
   --  mapping: Set_State gtkcheckmenuitem.h gtk_check_menu_item_set_state

   procedure Toggled (Check_Menu_Item : in out Gtk_Check_Menu_Item'Class);
   --  mapping: Toggled gtkcheckmenuitem.h gtk_check_menu_item_toggled

private

   type Gtk_Check_Menu_Item is new Menu_Item.Gtk_Menu_Item with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkcheckmenuitem.h \
   --  mapping:                     gtk_check_menu_item_get_type

end Gtk.Check_Menu_Item;
