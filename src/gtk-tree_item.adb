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
--         General Public License for more details.                  --
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

with System;
with Gdk; use Gdk;
with Gtk.Widget;

package body Gtk.Tree_Item is

   --------------
   -- Collapse --
   --------------

   procedure Collapse (Tree_Item : in Gtk_Tree_Item'Class)
   is
      procedure Internal (Tree_Item : in System.Address);
      pragma Import (C, Internal, "gtk_tree_item_collapse");
   begin
      Internal (Get_Object (Tree_Item));
   end Collapse;

   --------------
   -- Deselect --
   --------------

   procedure Deselect (Tree_Item : in Gtk_Tree_Item'Class)
   is
      procedure Internal (Tree_Item : in System.Address);
      pragma Import (C, Internal, "gtk_tree_item_deselect");
   begin
      Internal (Get_Object (Tree_Item));
   end Deselect;

   ------------
   -- Expand --
   ------------

   procedure Expand (Tree_Item : in Gtk_Tree_Item'Class)
   is
      procedure Internal (Tree_Item : in System.Address);
      pragma Import (C, Internal, "gtk_tree_item_expand");
   begin
      Internal (Get_Object (Tree_Item));
   end Expand;

   ---------------
   -- From_Tree --
   ---------------

   function From_Tree (Tree : in Gtk.Tree.Gtk_Tree'Class)
                       return Gtk_Tree_Item is
      Item : Gtk_Tree_Item;
   begin
      Set_Object (Item, Get_Object (Tree));
      return Item;
   end From_Tree;

   -----------------
   -- Get_Subtree --
   -----------------

   function Get_Subtree (Tree_Item : in Gtk_Tree_Item'Class)
                         return Gtk.Tree.Gtk_Tree
   is
      function Internal (Tree_Item : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "ada_tree_item_get_subtree");
      Tree : Gtk.Tree.Gtk_Tree;
   begin
      Set_Object (Tree, Internal (Get_Object (Tree_Item)));
      return Tree;
   end Get_Subtree;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Tree_Item;
                      Label  : in String)
   is
      function Internal (Label  : in String)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_tree_item_new_with_label");
   begin
      Set_Object (Widget, Internal (Label & Ascii.NUL));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Tree_Item)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tree_item_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ----------------
   -- Gtk_Select --
   ----------------

   procedure Gtk_Select (Tree_Item : in Gtk_Tree_Item'Class)
   is
      procedure Internal (Tree_Item : in System.Address);
      pragma Import (C, Internal, "gtk_tree_item_select");
   begin
      Internal (Get_Object (Tree_Item));
   end Gtk_Select;

   --------------------
   -- Remove_Subtree --
   --------------------

   procedure Remove_Subtree (Tree_Item : in Gtk_Tree_Item'Class)
   is
      procedure Internal (Tree_Item : in System.Address);
      pragma Import (C, Internal, "gtk_tree_item_remove_subtree");
   begin
      Internal (Get_Object (Tree_Item));
   end Remove_Subtree;

   -----------------
   -- Set_Subtree --
   -----------------

   procedure Set_Subtree
      (Tree_Item : in Gtk_Tree_Item'Class;
       Subtree   : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal
         (Tree_Item : in System.Address;
          Subtree   : in System.Address);
      pragma Import (C, Internal, "gtk_tree_item_set_subtree");
   begin
      Internal (Get_Object (Tree_Item),
                Get_Object (Subtree));
   end Set_Subtree;

   -------------
   -- To_Tree --
   -------------

   function To_Tree (Item : in Gtk_Tree_Item'Class) return Gtk.Tree.Gtk_Tree is
      Tree : Gtk.Tree.Gtk_Tree;
   begin
      Set_Object (Tree, Get_Object (Item));
      return Tree;
   end To_Tree;

end Gtk.Tree_Item;
