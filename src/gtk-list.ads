
with Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.List is

   type Gtk_List is new Gtk.Container.Gtk_Container with private;

   procedure Append_Items
      (List  : in Gtk_List'Class;
       Items : in Widget_List.Glist);
   function Child_Position
      (List   : in Gtk_List'Class;
       Child  : in Gtk.Widget.Gtk_Widget'Class)
       return      Gint;
   procedure Clear_Items
      (List    : in Gtk_List'Class;
       Start   : in Gint;
       The_End : in Gint);
   function Get_Children (Widget : in Gtk.List.Gtk_List'Class)
                          return      Widget_List.Glist;
   function Get_Selection (Widget : in Gtk.List.Gtk_List'Class)
                           return      Widget_List.Glist;
   procedure Gtk_New (Widget : out Gtk_List);
   procedure Insert_Items
      (List     : in Gtk_List'Class;
       Items    : in Widget_List.Glist;
       Position : in Gint);
   procedure Prepend_Items
      (List  : in Gtk_List'Class;
       Items : in Widget_List.Glist);
   procedure Remove_Items
      (List  : in Gtk_List'Class;
       Items : in Widget_List.Glist);
   procedure Remove_Items_No_Unref
      (List  : in Gtk_List'Class;
       Items : in Widget_List.Glist);
   procedure Select_Child
      (List  : in Gtk_List'Class;
       Child : in Gtk.Widget.Gtk_Widget'Class);
   procedure Select_Item
      (List : in Gtk_List'Class;
       Item : in Gint);
   procedure Set_Selection_Mode
      (List : in Gtk_List'Class;
       Mode : in Gtk_Selection_Mode);
   procedure Unselect_Child
      (List  : in Gtk_List'Class;
       Child : in Gtk.Widget.Gtk_Widget'Class);
   procedure Unselect_Item
      (List : in Gtk_List'Class;
       Item : in Gint);

private
   type Gtk_List is new Gtk.Container.Gtk_Container with null record;

   --  mapping: Append_Items gtklist.h gtk_list_append_items
   --  mapping: Child_Position gtklist.h gtk_list_child_position
   --  mapping: Clear_Items gtklist.h gtk_list_clear_items
   --  mapping: Get_Children gtklist.h GtkList->children
   --  mapping: Get_Selection gtklist.h GtkList->selection
   --  mapping: NOT_IMPLEMENTED gtklist.h gtk_list_get_type
   --  mapping: Gtk_New gtklist.h gtk_list_new
   --  mapping: Insert_Items gtklist.h gtk_list_insert_items
   --  mapping: Prepend_Items gtklist.h gtk_list_prepend_items
   --  mapping: Remove_Items gtklist.h gtk_list_remove_items
   --  mapping: Remove_Items_No_Unref gtklist.h gtk_list_remove_items_no_unref
   --  mapping: Select_Child gtklist.h gtk_list_select_child
   --  mapping: Select_Item gtklist.h gtk_list_select_item
   --  mapping: Set_Selection_Mode gtklist.h gtk_list_set_selection_mode
   --  mapping: Unselect_Child gtklist.h gtk_list_unselect_child
   --  mapping: Unselect_Item gtklist.h gtk_list_unselect_item
end Gtk.List;
