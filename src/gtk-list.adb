-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;

package body Gtk.List is

   use Widget_List;

   ------------------
   -- Append_Items --
   ------------------

   procedure Append_Items
      (List  : access Gtk_List_Record;
       Items : in Widget_List.Glist)
   is
      procedure Internal
         (List  : in System.Address;
          Items : in System.Address);
      pragma Import (C, Internal, "gtk_list_append_items");
   begin
      Internal (Get_Object (List),
                Get_Object (Items));
   end Append_Items;

   --------------------
   -- Child_Position --
   --------------------

   function Child_Position
      (List   : access Gtk_List_Record;
       Child  : in Gtk.Widget.Gtk_Widget)
       return      Gint
   is
      function Internal
         (List   : in System.Address;
          Child  : in System.Address)
          return      Gint;
      pragma Import (C, Internal, "gtk_list_child_position");
   begin
      return Internal (Get_Object (List),
                       Get_Object (Child));
   end Child_Position;

   -----------------
   -- Clear_Items --
   -----------------

   procedure Clear_Items
      (List    : access Gtk_List_Record;
       Start   : in Gint;
       The_End : in Gint)
   is
      procedure Internal
         (List    : in System.Address;
          Start   : in Gint;
          The_End : in Gint);
      pragma Import (C, Internal, "gtk_list_clear_items");
   begin
      Internal (Get_Object (List),
                Start,
                The_End);
   end Clear_Items;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children (Widget : access Gtk.List.Gtk_List_Record)
                          return      Widget_List.Glist
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_list_get_children");
      List : Gtk.Widget.Widget_List.Glist;
   begin
      Set_Object (List, Internal (Get_Object (Widget)));
      return List;
   end Get_Children;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection (Widget : access Gtk.List.Gtk_List_Record)
                           return      Widget_List.Glist
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_list_get_selection");
      List : Gtk.Widget.Widget_List.Glist;
   begin
      Set_Object (List, Internal (Get_Object (Widget)));
      return List;
   end Get_Selection;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_List)
   is
   begin
      Widget := new Gtk_List_Record;
      Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_List_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_list_new");
   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
   end Initialize;

   ------------------
   -- Insert_Items --
   ------------------

   procedure Insert_Items
      (List     : access Gtk_List_Record;
       Items    : in Widget_List.Glist;
       Position : in Gint)
   is
      procedure Internal
         (List     : in System.Address;
          Items    : in System.Address;
          Position : in Gint);
      pragma Import (C, Internal, "gtk_list_insert_items");
   begin
      Internal (Get_Object (List),
                Get_Object (Items),
                Position);
   end Insert_Items;

   -------------------
   -- Prepend_Items --
   -------------------

   procedure Prepend_Items
      (List  : access Gtk_List_Record;
       Items : in Widget_List.Glist)
   is
      procedure Internal
         (List  : in System.Address;
          Items : in System.Address);
      pragma Import (C, Internal, "gtk_list_prepend_items");
   begin
      Internal (Get_Object (List),
                Get_Object (Items));
   end Prepend_Items;

   ------------------
   -- Remove_Items --
   ------------------

   procedure Remove_Items
      (List  : access Gtk_List_Record;
       Items : in Widget_List.Glist)
   is
      procedure Internal
         (List  : in System.Address;
          Items : in System.Address);
      pragma Import (C, Internal, "gtk_list_remove_items");
   begin
      Internal (Get_Object (List),
                Get_Object (Items));
   end Remove_Items;

   ---------------------------
   -- Remove_Items_No_Unref --
   ---------------------------

   procedure Remove_Items_No_Unref
      (List  : access Gtk_List_Record;
       Items : in Widget_List.Glist)
   is
      procedure Internal
         (List  : in System.Address;
          Items : in System.Address);
      pragma Import (C, Internal, "gtk_list_remove_items_no_unref");
   begin
      Internal (Get_Object (List),
                Get_Object (Items));
   end Remove_Items_No_Unref;

   ------------------
   -- Select_Child --
   ------------------

   procedure Select_Child
      (List  : access Gtk_List_Record;
       Child : in Gtk.Widget.Gtk_Widget)
   is
      procedure Internal
         (List  : in System.Address;
          Child : in System.Address);
      pragma Import (C, Internal, "gtk_list_select_child");
   begin
      Internal (Get_Object (List),
                Get_Object (Child));
   end Select_Child;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
      (List : access Gtk_List_Record;
       Item : in Gint)
   is
      procedure Internal
         (List : in System.Address;
          Item : in Gint);
      pragma Import (C, Internal, "gtk_list_select_item");
   begin
      Internal (Get_Object (List),
                Item);
   end Select_Item;

   ------------------------
   -- Set_Selection_Mode --
   ------------------------

   procedure Set_Selection_Mode
      (List : access Gtk_List_Record;
       Mode : in Gtk_Selection_Mode)
   is
      procedure Internal
         (List : in System.Address;
          Mode : in Gint);
      pragma Import (C, Internal, "gtk_list_set_selection_mode");
   begin
      Internal (Get_Object (List),
                Gtk_Selection_Mode'Pos (Mode));
   end Set_Selection_Mode;

   --------------------
   -- Unselect_Child --
   --------------------

   procedure Unselect_Child
      (List  : access Gtk_List_Record;
       Child : in Gtk.Widget.Gtk_Widget)
   is
      procedure Internal
         (List  : in System.Address;
          Child : in System.Address);
      pragma Import (C, Internal, "gtk_list_unselect_child");
   begin
      Internal (Get_Object (List),
                Get_Object (Child));
   end Unselect_Child;

   -------------------
   -- Unselect_Item --
   -------------------

   procedure Unselect_Item
      (List : access Gtk_List_Record;
       Item : in Gint)
   is
      procedure Internal
         (List : in System.Address;
          Item : in Gint);
      pragma Import (C, Internal, "gtk_list_unselect_item");
   begin
      Internal (Get_Object (List),
                Item);
   end Unselect_Item;

end Gtk.List;
