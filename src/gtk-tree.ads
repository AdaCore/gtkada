-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
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

--  <description>
--
--  Note that this widget is obsolete and broken.
--  Consider using Gtk.Tree_View instead.
--
--  This widget displays a tree with expandable nodes. A tree is used to
--  display hierarchically-organized data. It is a vertical container for
--  arbitrary widgets of type Gtk_Tree_Item. The difference with a Gtk_Clist
--  is that Gtk_Tree widgets can be nested within other Gtk_Tree widgets.
--
--  See also Gtk_Ctree for a columned tree which is also more efficient.
--
--  </description>
--  <c_version>1.2.7</c_version>

with Gtk.Container;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Tree is

   type Gtk_Tree_Record is new Gtk.Container.Gtk_Container_Record with private;
   type Gtk_Tree is access all Gtk_Tree_Record'Class;

   -----------------------------------
   -- Creation, insertion, deletion --
   -----------------------------------
   --  Elements inside a Gtk_Tree are not ordered from the top to the bottom
   --  as is the case for Gtk_List. Instead, they are put in the tree by
   --  indicating where in the tree they should be placed. The position of an
   --  element (called a node) is defined by a parent node and a sibling node.
   --  The node will be attached in the parent subtree, on top of the sibling
   --  node.
   --  Like the Gtk_List widget, a Gtk_Tree will simply keep growing as
   --  more items are added to it, as well as when subtrees are expanded.
   --  For this reason, they are almost always packed into a
   --  Gtk_Scrolled_Window. You might want to use Gtk.Widget.Set_Usize on
   --  the scrolled window to ensure that it is big enough to see the tree's
   --  items, as the default size for Gtk_Scrolled_Window is quite small.

   procedure Gtk_New (Widget : out Gtk_Tree);
   --  Create an empty tree.

   procedure Initialize (Widget : access Gtk_Tree_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Tree.

   procedure Append
     (Tree      : access Gtk_Tree_Record;
      Tree_Item : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Append a tree item to a Tree.
   --  See package Gtk.Tree_Item for creating tree items and setting them as
   --  subtree.
   --  Remember to do Gtk.Widget.Show on the tree item after you append or
   --  insert items (can be done any time), otherwise they will not be shown
   --  if the user expands them for example.

   function Child_Position
     (Tree   : access Gtk_Tree_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint;
   --  Return the position in the tree of Child.
   --  If Child is not in the tree, return -1.

   procedure Clear_Items
     (Tree    : access Gtk_Tree_Record;
      Start   : in Gint;
      The_End : in Gint);
   --  Remove the items from position Start to position The_End from the tree.
   --  The warning about dereferencing at Remove_Items applies here too, as
   --  Clear_Items simply constructs a list and passes it to Remove_Items.

   function Get_Children
     (Widget : access Gtk.Tree.Gtk_Tree_Record) return Widget_List.Glist;
   --  Get the child items of the tree node.

   function Get_Selection
     (Widget : access Gtk.Tree.Gtk_Tree_Record) return Widget_List.Glist;
   --  Get the current selection of items in the tree.

   procedure Insert
     (Tree      : access Gtk_Tree_Record;
      Tree_Item : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position  : in Gint);
   --  Insert a tree item into a tree at the given position.

   procedure Prepend
     (Tree      : access Gtk_Tree_Record;
      Tree_Item : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Prepend a tree item to a tree.

   procedure Remove_Items
     (Tree  : access Gtk_Tree_Record;
      Items : in Widget_List.Glist);
   --  Remove a list of items (in the form of a Glist) from the tree.
   --  Note that removing an item from a tree dereferences (and thus usually)
   --  destroys it and its subtree, if it has one, and all subtrees in that
   --  subtree. If you want to remove only one item, you can use
   --  Gtk.Container.Remove.

   procedure Select_Child
     (Tree      : access Gtk_Tree_Record;
      Tree_Item : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Emit the select_item signal for the child Tree_Item, thus selecting it.

   procedure Select_Item
     (Tree : access Gtk_Tree_Record;
      Item : in Gint);
   --  Emit the select_item signal for the child at position Item.
   --  As a result, the child be is selected (unless you unselect it in a
   --  signal handler).

   procedure Set_Selection_Mode
     (Tree : access Gtk_Tree_Record;
      Mode : in Gtk_Selection_Mode);
   --  Set the selection mode.
   --  The mode can be one of Selection_Single (the default),
   --  Selection_Browse, Selection_Multiple, or Selection_Extended.
   --  This is only defined for root trees, which makes sense, since the
   --  root tree "owns" the selection. Setting it for subtrees has no effect
   --  at all; the value is simply ignored.

   procedure Set_View_Lines
     (Tree : access Gtk_Tree_Record;
      Flag : in Boolean);
   --  If Flag is True then connecting lines between tree items are drawn,
   --  otherwise not.

   procedure Set_View_Mode
     (Tree : access Gtk_Tree_Record;
      Mode : in Gtk_Tree_View_Mode);
   --  Set the "view mode".
   --  The mode can be either Tree_View_Line (the default) or Tree_View_Item.
   --  The view mode propagates from a tree to its subtrees, and can't be set
   --  exclusively to a subtree.
   --  The term "view mode" is rather ambiguous - basically, it controls the
   --  way the highlight is drawn when one of a tree's children is selected.
   --  If it's Tree_View_Line, the entire Tree_Item widget is highlighted,
   --  while for Tree_View_Item only the child widget (i.e., usually the
   --  label) is highlighted.

   procedure Unselect_Child
     (Tree      : access Gtk_Tree_Record;
      Tree_Item : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Emit the unselect_item signal for the child Tree_Item.
   --  As a result, Tree_Item is unselected.

   procedure Unselect_Item
     (Tree : access Gtk_Tree_Record;
      Item : in Gint);
   --  Emit the "unselect_item" signal for the child at position Item.
   --  As a result, the child is unselected.

private
   type Gtk_Tree_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_tree_get_type");
end Gtk.Tree;
