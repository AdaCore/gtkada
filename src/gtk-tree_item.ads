
with Gtk.Item;
with Gtk.Tree;
with Gtk.Widget;

package Gtk.Tree_Item is

   type Gtk_Tree_Item is new Gtk.Item.Gtk_Item with private;

   procedure Collapse (Tree_Item : in Gtk_Tree_Item'Class);
   procedure Deselect (Tree_Item : in Gtk_Tree_Item'Class);
   procedure Expand (Tree_Item : in Gtk_Tree_Item'Class);
   function From_Tree (Tree : in Gtk.Tree.Gtk_Tree'Class) return Gtk_Tree_Item;
   function Get_Subtree (Tree_Item : in Gtk_Tree_Item'Class)
                         return Gtk.Tree.Gtk_Tree;
   procedure Gtk_New (Widget : out Gtk_Tree_Item;
                      Label  : in String);
   procedure Gtk_New (Widget : out Gtk_Tree_Item);
   procedure Gtk_Select (Tree_Item : in Gtk_Tree_Item'Class);
   procedure Remove_Subtree (Tree_Item : in Gtk_Tree_Item'Class);
   procedure Set_Subtree
      (Tree_Item : in Gtk_Tree_Item'Class;
       Subtree   : in Gtk.Widget.Gtk_Widget'Class);
   function To_Tree (Item : in Gtk_Tree_Item'Class) return Gtk.Tree.Gtk_Tree;

private
   type Gtk_Tree_Item is new Gtk.Item.Gtk_Item with null record;

   --  mapping: Collapse gtktreeitem.h gtk_tree_item_collapse
   --  mapping: Deselect gtktreeitem.h gtk_tree_item_deselect
   --  mapping: Expand gtktreeitem.h gtk_tree_item_expand
   --  mapping: NOT_IMPLEMENTED gtktreeitem.h gtk_tree_item_get_type
   --  mapping: Gtk_New gtktreeitem.h gtk_tree_item_new_with_label
   --  mapping: Gtk_New gtktreeitem.h gtk_tree_item_new
   --  mapping: Gtk_Select gtktreeitem.h gtk_tree_item_select
   --  mapping: Remove_Subtree gtktreeitem.h gtk_tree_item_remove_subtree
   --  mapping: Set_Subtree gtktreeitem.h gtk_tree_item_set_subtree
end Gtk.Tree_Item;
