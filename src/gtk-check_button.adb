-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

package body Gtk.Check_Button is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Check_Button : out Gtk_Check_Button;
      Label        : String := "") is
   begin
      Check_Button := new Gtk_Check_Button_Record;
      Initialize (Check_Button, Label);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
     (Check_Button : out Gtk_Check_Button;
      Label        : String) is
   begin
      Check_Button := new Gtk_Check_Button_Record;
      Initialize_With_Mnemonic (Check_Button, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Check_Button : access Gtk_Check_Button_Record'Class;
      Label        : String := "")
   is
      function Internal (Label : String) return System.Address;
      pragma Import (C, Internal, "gtk_check_button_new_with_label");

      function Internal2 return System.Address;
      pragma Import (C, Internal2, "gtk_check_button_new");

   begin
      if Label = "" then
         Set_Object (Check_Button, Internal2);
      else
         Set_Object (Check_Button, Internal (Label & ASCII.NUL));
      end if;
   end Initialize;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
     (Check_Button : access Gtk_Check_Button_Record'Class;
      Label        : String)
   is
      function Internal (Label : String) return System.Address;
      pragma Import (C, Internal, "gtk_check_button_new_with_mnemonic");

   begin
      Set_Object (Check_Button, Internal (Label & ASCII.NUL));
   end Initialize_With_Mnemonic;

end Gtk.Check_Button;
