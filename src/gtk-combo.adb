-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------

with System;
with Gdk; use Gdk;

package body Gtk.Combo is

   ----------------------
   -- Disable_Activate --
   ----------------------

   procedure Disable_Activate (Combo_Box : in Gtk_Combo'Class) is
      procedure Internal (Combo_Box  : in System.Address);
      pragma Import (C, Internal, "gtk_combo_disable_activate");
   begin
      Internal (Get_Object (Combo_Box));
   end Disable_Activate;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry (Combo_Box : in Gtk_Combo'Class)
                       return         Gtk.GEntry.Gtk_Entry
   is
      function Internal (Combo_Box : in System.Address)
                         return         System.Address;
      pragma Import (C, Internal, "ada_combo_get_entry");
      Tmp : Gtk.GEntry.Gtk_Entry;
   begin
      Set_Object (Tmp, Internal (Get_Object (Combo_Box)));
      return Tmp;
   end Get_Entry;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Combo) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ------------------------
   -- Set_Case_Sensitive --
   ------------------------

   procedure Set_Case_Sensitive
     (Combo_Box : in Gtk_Combo'Class;
      Val       : in Boolean)
   is
      procedure Internal (Combo_Box : in System.Address;
                          Val       : in Gint);
      pragma Import (C, Internal, "gtk_combo_set_case_sensitive");
   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Val));
   end Set_Case_Sensitive;

   ---------------------
   -- Set_Item_String --
   ---------------------

   procedure Set_Item_String
     (Combo_Box  : in Gtk_Combo'Class;
      Item       : in Gtk.Item.Gtk_Item'Class;
      Item_Value : in String)
   is
      procedure Internal (Combo_Box  : in System.Address;
                          Item       : in System.Address;
                          Item_Value : in String);
      pragma Import (C, Internal, "gtk_combo_set_item_string");
   begin
      Internal (Get_Object (Combo_Box), Get_Object (Item),
                Item_Value & Ascii.NUL);
   end Set_Item_String;

   -------------------------
   -- Set_Popdown_Strings --
   -------------------------

   procedure Set_Popdown_Strings
     (Combo_Box : in Gtk_Combo'Class;
      Strings   : in String_List.Glist)
   is
      procedure Internal (Combo_Box : in System.Address;
                          Strings   : in System.Address);
      pragma Import (C, Internal, "gtk_combo_set_popdown_strings");
   begin
      Internal (Get_Object (Combo_Box),
                String_List.Get_Object (Strings));
   end Set_Popdown_Strings;

   --------------------
   -- Set_Use_Arrows --
   --------------------

   procedure Set_Use_Arrows
     (Combo_Box : in Gtk_Combo'Class;
      Val       : in Boolean)
   is
      procedure Internal (Combo_Box : in System.Address;
                          Val       : in Gint);
      pragma Import (C, Internal, "gtk_combo_set_use_arrows");
   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Val));
   end Set_Use_Arrows;

   ---------------------------
   -- Set_Use_Arrows_Always --
   ---------------------------

   procedure Set_Use_Arrows_Always
     (Combo_Box : in Gtk_Combo'Class;
      Val       : in Boolean)
   is
      procedure Internal (Combo_Box : in System.Address;
                          Val       : in Gint);
      pragma Import (C, Internal, "gtk_combo_set_use_arrows_always");
   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Val));
   end Set_Use_Arrows_Always;

   -----------------------
   -- Set_Value_In_List --
   -----------------------

   procedure Set_Value_In_List
     (Combo_Box   : in Gtk_Combo'Class;
      Val         : in Gint;
      Ok_If_Empty : in Boolean)
   is
      procedure Internal (Combo_Box   : in System.Address;
                          Val         : in Gint;
                          Ok_If_Empty : in Gint);
      pragma Import (C, Internal, "gtk_combo_set_value_in_list");
   begin
      Internal (Get_Object (Combo_Box), Val, Boolean'Pos (Ok_If_Empty));
   end Set_Value_In_List;

end Gtk.Combo;

