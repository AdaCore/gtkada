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


with Gtk.GEntry;
with Gtk.Box;
with Gtk.Item;
with Gtk.Enums; use Gtk.Enums;

package Gtk.Combo is

   type Gtk_Combo is new Gtk.Box.Gtk_Box with private;

   procedure Disable_Activate (Combo_Box : in Gtk_Combo'Class);
   --  mapping: Disable_Activate gtkcombo.h gtk_combo_disable_activate

   function Get_Entry (Combo_Box : in Gtk_Combo'Class)
                       return         Gtk.GEntry.Gtk_Entry;
   --  mapping: Get_Entry gtkcombo.h GtkCombo->entry

   procedure Gtk_New (Widget      : out Gtk_Combo);
   --  mapping: Gtk_New gtkcombo.h gtk_combo_new

   procedure Set_Case_Sensitive (Combo_Box : in Gtk_Combo'Class;
                                 Val       : in Boolean);
   --  mapping: Set_Case_Sensitive gtkcombo.h gtk_combo_set_case_sensitive

   procedure Set_Item_String (Combo_Box  : in Gtk_Combo'Class;
                              Item       : in Gtk.Item.Gtk_Item'Class;
                              Item_Value : in String);
   --  mapping: Set_Item_String gtkcombo.h gtk_combo_set_item_string

   procedure Set_Popdown_Strings (Combo_Box : in Gtk_Combo'Class;
                                  Strings   : in String_List.Glist);
   --  mapping: Set_Popdown_Strings gtkcombo.h gtk_combo_set_popdown_strings

   procedure Set_Use_Arrows (Combo_Box : in Gtk_Combo'Class;
                             Val       : in Boolean);
   --  mapping: Set_Use_Arrows gtkcombo.h gtk_combo_set_use_arrows

   procedure Set_Use_Arrows_Always (Combo_Box : in Gtk_Combo'Class;
                                    Val       : in Boolean);
   --  mapping: Set_Use_Arrows_Always gtkcombo.h \
   --  mapping: gtk_combo_set_use_arrows_always

   procedure Set_Value_In_List (Combo_Box   : in Gtk_Combo'Class;
                                Val         : in Gint;
                                Ok_If_Empty : in Boolean);
   --  mapping: Set_Value_In_List gtkcombo.h gtk_combo_set_value_in_list

   --  mapping: NOT_IMPLEMENTED gtkcombo.h gtk_combo_get_type

private

   type Gtk_Combo is new Gtk.Box.Gtk_Box with null record;

end Gtk.Combo;
