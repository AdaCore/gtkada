with Gtk.Check_Menu_Item;
with Gtk.Enums; use Gtk.Enums;

package Gtk.Radio_Menu_Item is

   type Gtk_Radio_Menu_Item is new Gtk.Check_Menu_Item.Gtk_Check_Menu_Item
     with private;

   function Group (Radio_Menu_Item : in Gtk_Radio_Menu_Item'Class)
                   return               Widget_SList.GSlist;
   procedure Gtk_New
      (Widget : out Gtk_Radio_Menu_Item;
       Group  : in Widget_SList.GSlist;
       Label  : in String);
   procedure Gtk_New (Widget : out Gtk_Radio_Menu_Item;
                      Group  : in Widget_SList.GSlist);
   procedure Set_Group
      (Radio_Menu_Item : in Gtk_Radio_Menu_Item'Class;
       Group           : in Widget_SList.GSlist);

private
   type Gtk_Radio_Menu_Item is new Gtk.Check_Menu_Item.Gtk_Check_Menu_Item
     with null record;

   --  mapping: NOT_IMPLEMENTED gtkradiomenuitem.h gtk_radio_menu_item_get_type
   --  mapping: Group gtkradiomenuitem.h gtk_radio_menu_item_group
   --  mapping: Gtk_New gtkradiomenuitem.h gtk_radio_menu_item_new_with_label
   --  mapping: Gtk_New gtkradiomenuitem.h gtk_radio_menu_item_new
   --  mapping: Set_Group gtkradiomenuitem.h gtk_radio_menu_item_set_group
end Gtk.Radio_Menu_Item;
