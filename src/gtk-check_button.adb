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

package body Gtk.Check_Button is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Check_Button : out Gtk_Check_Button;
                      With_Label   : in String := "") is
   begin
      Check_Button := new Gtk_Check_Button_Record;
      Initialize (Check_Button, With_Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Check_Button : access Gtk_Check_Button_Record'Class;
                         With_Label   : in String := "") is
      function Internal (Label : in String) return System.Address;
      pragma Import (C, Internal, "gtk_check_button_new_with_label");

   begin
      Set_Object (Check_Button, Internal (With_Label & ASCII.NUL));
      Initialize_User_Data (Check_Button);
   end Initialize;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      Label : String_Ptr := Get_Field (N, "label");

   begin
      if not N.Specific_Data.Created then
         if Label = null then
            Gen_New (N, "Check_Button", File => File);
         else
            Gen_New (N, "Check_Button", Label.all, File => File, Delim => '"');
         end if;
      end if;

      Toggle_Button.Generate (N, File);
   end Generate;

   procedure Generate
     (Check_Button : in out Object.Gtk_Object; N : in Node_Ptr)
   is
      S : String_Ptr := Get_Field (N, "label");

   begin
      if not N.Specific_Data.Created then
         if S = null then
            Gtk_New (Gtk_Check_Button (Check_Button));
         else
            Gtk_New (Gtk_Check_Button (Check_Button), S.all);
         end if;

         Set_Object (Get_Field (N, "name"), Check_Button);
         N.Specific_Data.Created := True;
      end if;

      Toggle_Button.Generate (Check_Button, N);
   end Generate;

end Gtk.Check_Button;
