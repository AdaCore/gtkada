
with Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.Tree is

   type Gtk_Tree is new Gtk.Container.Gtk_Container with private;

   procedure Append
      (Tree      : in Gtk_Tree'Class;
       Tree_Item : in Gtk.Widget.Gtk_Widget'Class);
   function Child_Position
      (Tree   : in Gtk_Tree'Class;
       Child  : in Gtk.Widget.Gtk_Widget'Class)
       return      Gint;
   procedure Clear_Items
      (Tree    : in Gtk_Tree'Class;
       Start   : in Gint;
       The_End : in Gint);
   function Get_Children (Widget : in Gtk.Tree.Gtk_Tree'Class)
                          return      Widget_List.Glist;
   function Get_Selection (Widget : in Gtk.Tree.Gtk_Tree'Class)
                           return      Widget_List.Glist;
   procedure Gtk_New (Widget : out Gtk_Tree);
   procedure Insert
      (Tree      : in Gtk_Tree'Class;
       Tree_Item : in Gtk.Widget.Gtk_Widget'Class;
       Position  : in Gint);
   procedure Prepend
      (Tree      : in Gtk_Tree'Class;
       Tree_Item : in Gtk.Widget.Gtk_Widget'Class);
   procedure Remove_Items
      (Tree  : in Gtk_Tree'Class;
       Items : in Widget_List.Glist);
   procedure Select_Child
      (Tree      : in Gtk_Tree'Class;
       Tree_Item : in Gtk.Widget.Gtk_Widget'Class);
   procedure Select_Item
      (Tree : in Gtk_Tree'Class;
       Item : in Gint);
   procedure Set_Selection_Mode
      (Tree : in Gtk_Tree'Class;
       Mode : in Gtk_Selection_Mode);
   procedure Set_View_Lines
      (Tree : in Gtk_Tree'Class;
       Flag : in Boolean);
   procedure Set_View_Mode
      (Tree : in Gtk_Tree'Class;
       Mode : in Gtk_Tree_View_Mode);
   procedure Unselect_Child
      (Tree      : in Gtk_Tree'Class;
       Tree_Item : in Gtk.Widget.Gtk_Widget'Class);
   procedure Unselect_Item
      (Tree : in Gtk_Tree'Class;
       Item : in Gint);

private
   type Gtk_Tree is new Gtk.Container.Gtk_Container with null record;

   --  mapping: Append gtktree.h gtk_tree_append
   --  mapping: Child_Position gtktree.h gtk_tree_child_position
   --  mapping: Clear_Items gtktree.h gtk_tree_clear_items
   --  mapping: Get_Children gtktree.h GtkTree->children
   --  mapping: Get_Selection gtktree.h GtkTree->selection
   --  mapping: NOT_IMPLEMENTED gtktree.h gtk_tree_get_type
   --  mapping: Gtk_New gtktree.h gtk_tree_new
   --  mapping: Insert gtktree.h gtk_tree_insert
   --  mapping: Prepend gtktree.h gtk_tree_prepend
   --  mapping: DEPRECATED gtktree.h gtk_tree_remove_item
   --  mapping: Remove_Items gtktree.h gtk_tree_remove_items
   --  mapping: Select_Child gtktree.h gtk_tree_select_child
   --  mapping: Select_Item gtktree.h gtk_tree_select_item
   --  mapping: Set_Selection_Mode gtktree.h gtk_tree_set_selection_mode
   --  mapping: Set_View_Lines gtktree.h gtk_tree_set_view_lines
   --  mapping: Set_View_Mode gtktree.h gtk_tree_set_view_mode
   --  mapping: Unselect_Child gtktree.h gtk_tree_unselect_child
   --  mapping: Unselect_Item gtktree.h gtk_tree_unselect_item
end Gtk.Tree;
