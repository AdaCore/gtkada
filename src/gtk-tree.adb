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
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;
with Gtk.Util; use Gtk.Util;

package body Gtk.Tree is

   use Container;
   use Widget_List;

   ------------
   -- Append --
   ------------

   procedure Append
     (Tree      : access Gtk_Tree_Record;
      Tree_Item : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Tree      : in System.Address;
         Tree_Item : in System.Address);
      pragma Import (C, Internal, "gtk_tree_append");
   begin
      Internal (Get_Object (Tree),
                Get_Object (Tree_Item));
   end Append;

   --------------------
   -- Child_Position --
   --------------------

   function Child_Position
     (Tree   : access Gtk_Tree_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gint
   is
      function Internal
        (Tree   : in System.Address;
         Child  : in System.Address)
         return      Gint;
      pragma Import (C, Internal, "gtk_tree_child_position");
   begin
      return Internal (Get_Object (Tree),
                       Get_Object (Child));
   end Child_Position;

   -----------------
   -- Clear_Items --
   -----------------

   procedure Clear_Items
     (Tree    : access Gtk_Tree_Record;
      Start   : in Gint;
      The_End : in Gint)
   is
      procedure Internal
        (Tree    : in System.Address;
         Start   : in Gint;
         The_End : in Gint);
      pragma Import (C, Internal, "gtk_tree_clear_items");
   begin
      Internal (Get_Object (Tree), Start, The_End);
   end Clear_Items;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children (Widget : access Gtk.Tree.Gtk_Tree_Record)
     return Widget_List.Glist
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_tree_get_children");
      List : Widget_List.Glist;
   begin
      Set_Object (List, Internal (Get_Object (Widget)));
      return List;
   end Get_Children;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection (Widget : access Gtk.Tree.Gtk_Tree_Record)
      return Widget_List.Glist
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_tree_get_selection");
      List : Widget_List.Glist;
   begin
      Set_Object (List, Internal (Get_Object (Widget)));
      return List;
   end Get_Selection;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Tree) is
   begin
      Widget := new Gtk_Tree_Record;
      Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Tree_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tree_new");
   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Tree      : access Gtk_Tree_Record;
      Tree_Item : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position  : in Gint)
   is
      procedure Internal
        (Tree      : in System.Address;
         Tree_Item : in System.Address;
         Position  : in Gint);
      pragma Import (C, Internal, "gtk_tree_insert");
   begin
      Internal (Get_Object (Tree), Get_Object (Tree_Item), Position);
   end Insert;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Tree      : access Gtk_Tree_Record;
      Tree_Item : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Tree      : in System.Address;
         Tree_Item : in System.Address);
      pragma Import (C, Internal, "gtk_tree_prepend");
   begin
      Internal (Get_Object (Tree), Get_Object (Tree_Item));
   end Prepend;

   ------------------
   -- Remove_Items --
   ------------------

   procedure Remove_Items
     (Tree  : access Gtk_Tree_Record;
      Items : in Widget_List.Glist)
   is
      procedure Internal
        (Tree  : in System.Address;
         Items : in System.Address);
      pragma Import (C, Internal, "gtk_tree_remove_items");
   begin
      Internal (Get_Object (Tree), Get_Object (Items));
   end Remove_Items;

   ------------------
   -- Select_Child --
   ------------------

   procedure Select_Child
     (Tree      : access Gtk_Tree_Record;
      Tree_Item : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Tree      : in System.Address;
         Tree_Item : in System.Address);
      pragma Import (C, Internal, "gtk_tree_select_child");
   begin
      Internal (Get_Object (Tree), Get_Object (Tree_Item));
   end Select_Child;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
     (Tree : access Gtk_Tree_Record;
      Item : in Gint)
   is
      procedure Internal
        (Tree : in System.Address;
         Item : in Gint);
      pragma Import (C, Internal, "gtk_tree_select_item");
   begin
      Internal (Get_Object (Tree), Item);
   end Select_Item;

   ------------------------
   -- Set_Selection_Mode --
   ------------------------

   procedure Set_Selection_Mode
     (Tree : access Gtk_Tree_Record;
      Mode : in Gtk_Selection_Mode)
   is
      procedure Internal
        (Tree : in System.Address;
         Mode : in Gint);
      pragma Import (C, Internal, "gtk_tree_set_selection_mode");
   begin
      Internal (Get_Object (Tree), Gtk_Selection_Mode'Pos (Mode));
   end Set_Selection_Mode;

   --------------------
   -- Set_View_Lines --
   --------------------

   procedure Set_View_Lines
     (Tree : access Gtk_Tree_Record;
      Flag : in Boolean)
   is
      procedure Internal
        (Tree : in System.Address;
         Flag : in Guint);
      pragma Import (C, Internal, "gtk_tree_set_view_lines");
   begin
      Internal (Get_Object (Tree), Boolean'Pos (Flag));
   end Set_View_Lines;

   -------------------
   -- Set_View_Mode --
   -------------------

   procedure Set_View_Mode
     (Tree : access Gtk_Tree_Record;
      Mode : in Gtk_Tree_View_Mode)
   is
      procedure Internal
        (Tree : in System.Address;
         Mode : in Gint);
      pragma Import (C, Internal, "gtk_tree_set_view_mode");
   begin
      Internal (Get_Object (Tree), Gtk_Tree_View_Mode'Pos (Mode));
   end Set_View_Mode;

   --------------------
   -- Unselect_Child --
   --------------------

   procedure Unselect_Child
     (Tree      : access Gtk_Tree_Record;
      Tree_Item : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Tree      : in System.Address;
         Tree_Item : in System.Address);
      pragma Import (C, Internal, "gtk_tree_unselect_child");
   begin
      Internal (Get_Object (Tree), Get_Object (Tree_Item));
   end Unselect_Child;

   -------------------
   -- Unselect_Item --
   -------------------

   procedure Unselect_Item
     (Tree : access Gtk_Tree_Record;
      Item : in Gint)
   is
      procedure Internal
        (Tree : in System.Address;
         Item : in Gint);
      pragma Import (C, Internal, "gtk_tree_unselect_item");
   begin
      Internal (Get_Object (Tree), Item);
   end Unselect_Item;

   --------------
   -- Generate --
   --------------

   procedure Generate (N    : in Node_Ptr;
                       File : in File_Type) is
   begin
      Gen_New (N, "Tree", File => File);
      Container.Generate (N, File);
      Gen_Set (N, "Tree", "selection_mode", File);
      Gen_Set (N, "Tree", "view_lines", File);
      Gen_Set (N, "Tree", "view_mode", File);

      if not N.Specific_Data.Has_Container then
         Gen_Call_Child (N, null, "Container", "Add", File => File);
         N.Specific_Data.Has_Container := True;
      end if;
   end Generate;

   procedure Generate (Tree : in out Gtk_Object;
                       N    : in Node_Ptr) is
      S : String_Ptr;
   begin
      if not N.Specific_Data.Created then
         Gtk_New (Gtk_Tree (Tree));
         Set_Object (Get_Field (N, "name"), Tree);
         N.Specific_Data.Created := True;
      end if;

      Container.Generate (Tree, N);

      S := Get_Field (N, "selection_mode");

      if S /= null then
         Set_Selection_Mode
           (Gtk_Tree (Tree),
            Gtk_Selection_Mode'Value (S (S'First + 4 .. S'Last)));
      end if;

      S := Get_Field (N, "view_lines");

      if S /= null then
         Set_View_Lines (Gtk_Tree (Tree), Boolean'Value (S.all));
      end if;

      S := Get_Field (N, "view_mode");

      if S /= null then
         Set_View_Mode
           (Gtk_Tree (Tree),
            Gtk_Tree_View_Mode'Value (S (S'First + 4 .. S'Last)));
      end if;

      if not N.Specific_Data.Has_Container then
         Container.Add
           (Gtk_Container (Get_Object (Get_Field (N.Parent, "name"))),
            Gtk_Widget (Tree));
         N.Specific_Data.Has_Container := True;
      end if;
   end Generate;

end Gtk.Tree;
