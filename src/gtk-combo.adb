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
with Gtk.Util; use Gtk.Util;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Gtk.Combo is

   ----------------------
   -- Disable_Activate --
   ----------------------

   procedure Disable_Activate (Combo_Box : access Gtk_Combo_Record) is
      procedure Internal (Combo_Box  : in System.Address);
      pragma Import (C, Internal, "gtk_combo_disable_activate");

   begin
      Internal (Get_Object (Combo_Box));
   end Disable_Activate;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry (Combo_Box : access Gtk_Combo_Record)
     return Gtk.GEntry.Gtk_Entry
   is
      function Internal (Combo_Box : in System.Address)
        return System.Address;
      pragma Import (C, Internal, "ada_combo_get_entry");

      Stub : Gtk.GEntry.Gtk_Entry_Record;

   begin
      return Gtk.GEntry.Gtk_Entry
        (Get_User_Data (Internal (Get_Object (Combo_Box)), Stub));
   end Get_Entry;

   --------------
   -- Get_List --
   --------------

   function Get_List (Combo_Box : access Gtk_Combo_Record)
     return Gtk.List.Gtk_List
   is
      function Internal (Combo_Box : in System.Address)
        return System.Address;
      pragma Import (C, Internal, "ada_combo_get_list");

      Stub : Gtk.List.Gtk_List_Record;

   begin
      return Gtk.List.Gtk_List
        (Get_User_Data (Internal (Get_Object (Combo_Box)), Stub));
   end Get_List;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Combo_Box : out Gtk_Combo) is
   begin
      Combo_Box := new Gtk_Combo_Record;
      Initialize (Combo_Box);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Combo_Box : access Gtk_Combo_Record) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_new");

   begin
      Set_Object (Combo_Box, Internal);
      Initialize_User_Data (Combo_Box);
   end Initialize;

   ------------------------
   -- Set_Case_Sensitive --
   ------------------------

   procedure Set_Case_Sensitive (Combo_Box : access Gtk_Combo_Record;
                                 Val : in Boolean)
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
     (Combo_Box  : access Gtk_Combo_Record;
      Item       : in Gtk.Item.Gtk_Item;
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
     (Combo_Box : access Gtk_Combo_Record;
      Strings   : in String_List.Glist)
   is
      procedure Internal (Combo_Box : in System.Address;
                          Strings   : in System.Address);
      pragma Import (C, Internal, "gtk_combo_set_popdown_strings");

   begin
      Internal (Get_Object (Combo_Box), String_List.Get_Object (Strings));
   end Set_Popdown_Strings;

   --------------------
   -- Set_Use_Arrows --
   --------------------

   procedure Set_Use_Arrows (Combo_Box : access Gtk_Combo_Record;
                             Val : in Boolean)
   is
      procedure Internal (Combo_Box : in System.Address; Val : in Gint);
      pragma Import (C, Internal, "gtk_combo_set_use_arrows");

   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Val));
   end Set_Use_Arrows;

   ---------------------------
   -- Set_Use_Arrows_Always --
   ---------------------------

   procedure Set_Use_Arrows_Always (Combo_Box : access Gtk_Combo_Record;
                                    Val : in Boolean)
   is
      procedure Internal (Combo_Box : in System.Address; Val : in Gint);
      pragma Import (C, Internal, "gtk_combo_set_use_arrows_always");

   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Val));
   end Set_Use_Arrows_Always;

   -----------------------
   -- Set_Value_In_List --
   -----------------------

   procedure Set_Value_In_List
     (Combo_Box   : access Gtk_Combo_Record;
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

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      S : String_Ptr;
      First, Last : Natural;

   begin
      Gen_New (N, "Combo", File => File);
      Box.Generate (N, File);
      Gen_Set (N, "Combo", "case_sensitive", File);
      Gen_Set (N, "Combo", "use_arrows", File);
      Gen_Set (N, "Combo", "use_arrows_always", File);

      S := Get_Field (N, "items");

      if S /= null then
         First := S'First;

         loop
            Last := Index (S (First .. S'Last), ASCII.LF & "");

            if Last = 0 then
               Last := S'Last + 1;
            end if;

            Put_Line (File, "   String_List.Append (" &
              To_Ada (Get_Field (N, "name").all) & "_Items, """ &
              S (First .. Last - 1) & """);");

            exit when Last >= S'Last;

            First := Last + 1;
         end loop;

         Put_Line (File, "   Combo.Set_Popdown_Strings (Gtk_Combo (" &
           To_Ada (Get_Field (N, "name").all) & "), " &
           To_Ada (Get_Field (N, "name").all) & "_Items);");
         Put_Line (File, "   String_List.Free (" &
           To_Ada (Get_Field (N, "name").all) & "_Items);");
      end if;
   end Generate;

   procedure Generate
     (Combo_Box : in out Object.Gtk_Object; N : in Node_Ptr)
   is
      S     : String_Ptr;
      Items : String_List.Glist;
      First, Last : Natural;

   begin
      if not N.Specific_Data.Created then
         Gtk_New (Gtk_Combo (Combo_Box));
         Set_Object (Get_Field (N, "name"), Combo_Box);
         N.Specific_Data.Created := True;
      end if;

      Box.Generate (Combo_Box, N);

      S := Get_Field (N, "case_sensitive");

      if S /= null then
         Set_Case_Sensitive (Gtk_Combo (Combo_Box), Boolean'Value (S.all));
      end if;

      S := Get_Field (N, "use_arrows");

      if S /= null then
         Set_Use_Arrows (Gtk_Combo (Combo_Box), Boolean'Value (S.all));
      end if;

      S := Get_Field (N, "use_arrows_always");

      if S /= null then
         Set_Use_Arrows_Always (Gtk_Combo (Combo_Box), Boolean'Value (S.all));
      end if;

      S := Get_Field (N, "items");

      if S /= null then
         First := S'First;

         loop
            Last := Index (S (First .. S'Last), ASCII.LF & "");

            if Last = 0 then
               Last := S'Last + 1;
            end if;

            String_List.Append (Items, S (First .. Last - 1));

            exit when Last >= S'Last;

            First := Last + 1;
         end loop;

         Set_Popdown_Strings (Gtk_Combo (Combo_Box), Items);
         String_List.Free (Items);
      end if;
   end Generate;

end Gtk.Combo;
