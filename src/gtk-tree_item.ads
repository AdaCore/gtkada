-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------


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
