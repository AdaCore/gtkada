-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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

package body Gtk.Combo is

   ----------------------
   -- Disable_Activate --
   ----------------------

   procedure Disable_Activate (Combo_Box : access Gtk_Combo_Record) is
      procedure Internal (Combo_Box : System.Address);
      pragma Import (C, Internal, "gtk_combo_disable_activate");

   begin
      Internal (Get_Object (Combo_Box));
   end Disable_Activate;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry
     (Combo_Box : access Gtk_Combo_Record) return Gtk.GEntry.Gtk_Entry
   is
      function Internal (Combo_Box : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_combo_get_entry");

      Stub : Gtk.GEntry.Gtk_Entry_Record;

   begin
      return Gtk.GEntry.Gtk_Entry
        (Get_User_Data (Internal (Get_Object (Combo_Box)), Stub));
   end Get_Entry;

   --------------
   -- Get_List --
   --------------

   pragma Warnings (Off); --  Gtk_List is obsolescent
   function Get_List
     (Combo_Box : access Gtk_Combo_Record) return Gtk.List.Gtk_List
   is
      function Internal (Combo_Box : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_combo_get_list");

      Stub : Gtk.List.Gtk_List_Record;

   begin
      return Gtk.List.Gtk_List
        (Get_User_Data (Internal (Get_Object (Combo_Box)), Stub));
   end Get_List;
   pragma Warnings (On);

   ----------------------
   -- Get_Popup_Window --
   ----------------------

   function Get_Popup_Window
     (Combo_Box : access Gtk_Combo_Record) return Gtk.Window.Gtk_Window
   is
      function Internal (Combo_Box : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_combo_get_popup_window");

      Stub : Gtk.Window.Gtk_Window_Record;
   begin
      return Gtk.Window.Gtk_Window
        (Get_User_Data (Internal (Get_Object (Combo_Box)), Stub));
   end Get_Popup_Window;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Combo_Box : out Gtk_Combo) is
   begin
      Combo_Box := new Gtk_Combo_Record;
      Gtk.Combo.Initialize (Combo_Box);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Combo_Box : access Gtk_Combo_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_new");

   begin
      Set_Object (Combo_Box, Internal);
   end Initialize;

   ------------------------
   -- Set_Case_Sensitive --
   ------------------------

   procedure Set_Case_Sensitive
     (Combo_Box : access Gtk_Combo_Record; Val : Boolean := True)
   is
      procedure Internal
        (Combo_Box : System.Address; Val : Gint);
      pragma Import (C, Internal, "gtk_combo_set_case_sensitive");

   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Val));
   end Set_Case_Sensitive;

   ---------------
   -- Set_Entry --
   ---------------

   procedure Set_Entry
     (Combo_Box : access Gtk_Combo_Record;
      GEntry    : Gtk.GEntry.Gtk_Entry)
   is
      procedure Internal
        (Combo_Box : System.Address;
         GEntry    : System.Address);
      pragma Import (C, Internal, "ada_combo_set_entry");

   begin
      Internal (Get_Object (Combo_Box), Get_Object (GEntry));
   end Set_Entry;

   ---------------------
   -- Set_Item_String --
   ---------------------

   procedure Set_Item_String
     (Combo_Box  : access Gtk_Combo_Record;
      Item       : Gtk.Item.Gtk_Item;
      Item_Value : UTF8_String)
   is
      procedure Internal
        (Combo_Box  : System.Address;
         Item       : System.Address;
         Item_Value : UTF8_String);
      pragma Import (C, Internal, "gtk_combo_set_item_string");

   begin
      Internal (Get_Object (Combo_Box), Get_Object (Item),
                Item_Value & ASCII.NUL);
   end Set_Item_String;

   -------------------------
   -- Set_Popdown_Strings --
   -------------------------

   procedure Set_Popdown_Strings
     (Combo_Box : access Gtk_Combo_Record;
      Strings   : String_List.Glist)
   is
      procedure Internal
        (Combo_Box : System.Address;
         Strings   : System.Address);
      pragma Import (C, Internal, "gtk_combo_set_popdown_strings");

   begin
      Internal (Get_Object (Combo_Box), String_List.Get_Object (Strings));
   end Set_Popdown_Strings;

   --------------------
   -- Set_Use_Arrows --
   --------------------

   procedure Set_Use_Arrows
     (Combo_Box : access Gtk_Combo_Record; Val : Boolean := True)
   is
      procedure Internal (Combo_Box : System.Address; Val : Gint);
      pragma Import (C, Internal, "gtk_combo_set_use_arrows");

   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Val));
   end Set_Use_Arrows;

   ---------------------------
   -- Set_Use_Arrows_Always --
   ---------------------------

   procedure Set_Use_Arrows_Always
     (Combo_Box : access Gtk_Combo_Record; Val : Boolean := True)
   is
      procedure Internal (Combo_Box : System.Address; Val : Gint);
      pragma Import (C, Internal, "gtk_combo_set_use_arrows_always");

   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Val));
   end Set_Use_Arrows_Always;

   -----------------------
   -- Set_Value_In_List --
   -----------------------

   procedure Set_Value_In_List
     (Combo_Box   : access Gtk_Combo_Record;
      Val         : Boolean := True;
      Ok_If_Empty : Boolean := False)
   is
      procedure Internal
        (Combo_Box   : System.Address;
         Val         : Gboolean;
         Ok_If_Empty : Gboolean);
      pragma Import (C, Internal, "gtk_combo_set_value_in_list");

   begin
      Internal (Get_Object (Combo_Box),
                Boolean'Pos (Val), Boolean'Pos (Ok_If_Empty));
   end Set_Value_In_List;

end Gtk.Combo;
