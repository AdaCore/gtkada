with Gtk.Check_Menu_Item;

package Gtk.Radio_Menu_Item is

   type Gtk_Radio_Menu_Item is new Gtk.Check_Menu_Item.Gtk_Check_Menu_Item
     with private;
   type Group_List is private;

   function Group (Radio_Menu_Item : in Gtk_Radio_Menu_Item'Class)
                   return               Group_List;
   procedure Gtk_New
      (Widget : out Gtk_Radio_Menu_Item;
       Group  : in Group_List;
       Label  : in String);
   procedure Gtk_New (Widget : out Gtk_Radio_Menu_Item;
                      Group  : in Group_List);
   procedure Set_Group
      (Radio_Menu_Item : in Gtk_Radio_Menu_Item'Class;
       Group           : in Group_List);

private
   type Gtk_Radio_Menu_Item is new Gtk.Check_Menu_Item.Gtk_Check_Menu_Item
     with null record;
   type Group_List is record
      Ptr : System.Address := System.Null_Address;
   end record;

   function Get_Object (Group : in Group_List) return System.Address;
   pragma Inline (Get_Object);
   procedure Set_Object (Group : out Group_List;
                         Value : in System.Address);
   pragma Inline (Set_Object);

   --  mapping: NOT_IMPLEMENTED gtkradiomenuitem.h gtk_radio_menu_item_get_type
   --  mapping: Group gtkradiomenuitem.h gtk_radio_menu_item_group
   --  mapping: Gtk_New gtkradiomenuitem.h gtk_radio_menu_item_new_with_label
   --  mapping: Gtk_New gtkradiomenuitem.h gtk_radio_menu_item_new
   --  mapping: Set_Group gtkradiomenuitem.h gtk_radio_menu_item_set_group
end Gtk.Radio_Menu_Item;
