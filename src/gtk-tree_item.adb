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

with System;
with Gdk; use Gdk;
with Gtk.Widget;
with Gtk.Util; use Gtk.Util;

package body Gtk.Tree_Item is

   --------------
   -- Collapse --
   --------------

   procedure Collapse (Tree_Item : access Gtk_Tree_Item_Record)
   is
      procedure Internal (Tree_Item : in System.Address);
      pragma Import (C, Internal, "gtk_tree_item_collapse");
   begin
      Internal (Get_Object (Tree_Item));
   end Collapse;

   --------------
   -- Deselect --
   --------------

   procedure Deselect (Tree_Item : access Gtk_Tree_Item_Record)
   is
      procedure Internal (Tree_Item : in System.Address);
      pragma Import (C, Internal, "gtk_tree_item_deselect");
   begin
      Internal (Get_Object (Tree_Item));
   end Deselect;

   ------------
   -- Expand --
   ------------

   procedure Expand (Tree_Item : access Gtk_Tree_Item_Record)
   is
      procedure Internal (Tree_Item : in System.Address);
      pragma Import (C, Internal, "gtk_tree_item_expand");
   begin
      Internal (Get_Object (Tree_Item));
   end Expand;

   -----------------
   -- Get_Subtree --
   -----------------

   function Get_Subtree (Tree_Item : access Gtk_Tree_Item_Record)
     return Gtk.Tree.Gtk_Tree
   is
      function Internal (Tree_Item : in System.Address)
        return System.Address;
      pragma Import (C, Internal, "ada_tree_item_get_subtree");
      Stub : Gtk.Tree.Gtk_Tree_Record;

   begin
      return Gtk.Tree.Gtk_Tree
        (Get_User_Data (Internal (Get_Object (Tree_Item)), Stub));
   end Get_Subtree;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Tree_Item : out Gtk_Tree_Item;
                      Label     : in String := "")
   is
   begin
      Tree_Item := new Gtk_Tree_Item_Record;
      Initialize (Tree_Item, Label);
   end Gtk_New;

   ----------------
   -- Gtk_Select --
   ----------------

   procedure Gtk_Select (Tree_Item : access Gtk_Tree_Item_Record)
   is
      procedure Internal (Tree_Item : in System.Address);
      pragma Import (C, Internal, "gtk_tree_item_select");
   begin
      Internal (Get_Object (Tree_Item));
   end Gtk_Select;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Tree_Item : access Gtk_Tree_Item_Record'Class;
                         Label     : in String := "")
   is
      function Internal (Label  : in String)
        return System.Address;
      pragma Import (C, Internal, "gtk_tree_item_new_with_label");
   begin
      Set_Object (Tree_Item, Internal (Label & ASCII.Nul));
      Initialize_User_Data (Tree_Item);
   end Initialize;

   --------------------
   -- Remove_Subtree --
   --------------------

   procedure Remove_Subtree (Tree_Item : access Gtk_Tree_Item_Record)
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
      (Tree_Item : access Gtk_Tree_Item_Record;
       Subtree   : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Tree_Item : in System.Address;
          Subtree   : in System.Address);
      pragma Import (C, Internal, "gtk_tree_item_set_subtree");
   begin
      Internal (Get_Object (Tree_Item),
                Get_Object (Subtree));
   end Set_Subtree;

   --------------
   -- Generate --
   --------------

   procedure Generate (N         : in Node_Ptr;
                       File      : in File_Type) is
   begin
      Gen_New (N, "Tree_Item", Get_Field (N, "label").all,
        File => File, Delim => '"');
      Item.Generate (N, File);
   end Generate;

   procedure Generate (Tree_Item : in out Gtk_Object;
                       N         : in Node_Ptr) is
   begin
      if not N.Specific_Data.Created then
         Gtk_New (Gtk_Tree_Item (Tree_Item), Get_Field (N, "label").all);
         Set_Object (Get_Field (N, "name"), Tree_Item);
         N.Specific_Data.Created := True;
      end if;

      Item.Generate (Tree_Item, N);
   end Generate;

end Gtk.Tree_Item;
