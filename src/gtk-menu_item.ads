with Gtk.Enums;
with Gtk.Item;
with Gtk.Widget;

package Gtk.Menu_Item is

   type Gtk_Menu_Item is new Item.Gtk_Item with private;

   procedure Gtk_New (Menu_Item : out Gtk_Menu_Item);
   --  mapping: Gtk_New gtkmenuitem.h gtk_menu_item_new

   procedure Gtk_New (Menu_Item : out Gtk_Menu_Item;
                      Label     : in  String);
   --  mapping: Gtk_New gtkmenuitem.h gtk_menu_item_new_with_label

   procedure Set_Submenu (Menu_Item : in out Gtk_Menu_Item'Class;
                          Submenu   : in     Widget.Gtk_Widget'Class);
   --  mapping: Set_Submenu gtkmenuitem.h gtk_menu_item_set_submenu

   procedure Remove_Submenu (Menu_Item : in out Gtk_Menu_Item'Class);
   --  mapping: Remove_Submenu gtkmenuitem.h gtk_menu_item_remove_submenu

   procedure Set_Placement (Menu_Item : in out Gtk_Menu_Item'Class;
                            Placement : in     Enums.Submenu_Placement);
   --  mapping: Set_Placement gtkmenuitem.h gtk_menu_item_set_placement

   procedure Accelerator_Size (Menu_Item : in out Gtk_Menu_Item'Class);
   --  mapping: Accelerator_Size gtkmenuitem.h gtk_menu_item_accelerator_size

   function Accelerator_Text (Menu_Item : in Gtk_Menu_Item'Class)
                              return String;
   --  mapping: Accelerator_Text gtkmenuitem.h gtk_menu_item_accelerator_text
   --  FIXME  This service should be tested to see if it works...

   procedure Configure (Menu_Item              : in out Gtk_Menu_Item'Class;
                        Show_Toggle_Indicator  : in     Boolean;
                        Show_Submenu_Indicator : in     Boolean);
   --  mapping: Configure gtkmenuitem.h gtk_menu_item_configure

   procedure Gtk_Select (Menu_Item : in out Gtk_Menu_Item'Class);
   --  mapping: Gtk_Select gtkmenuitem.h gtk_menu_item_select

   procedure Deselect (Menu_Item : in out Gtk_Menu_Item'Class);
   --  mapping: Deselect gtkmenuitem.h gtk_menu_item_deselect

   procedure Activate (Menu_Item : in out Gtk_Menu_Item'Class);
   --  mapping: Activate gtkmenuitem.h gtk_menu_item_activate

   procedure Right_Justify (Menu_Item : in out Gtk_Menu_Item'Class);
   --  mapping: Right_Justify gtkmenuitem.h gtk_menu_item_right_justify

private

   type Gtk_Menu_Item is new Item.Gtk_Item with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkmenuitem.h gtk_menu_item_get_type

end Gtk.Menu_Item;
