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

package body Gtk.Toggle_Button is

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active (Toggle_Button : access Gtk_Toggle_Button_Record)
                       return Boolean
   is
      function Internal (Widget : in System.Address) return Integer;
      pragma Import (C, Internal, "gtk_toggle_button_get_active");
   begin
      return Boolean'Val (Internal (Get_Object (Toggle_Button)));
   end Get_Active;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Toggle_Button : out Gtk_Toggle_Button;
                      Label         : in String := "") is
   begin
      Toggle_Button := new Gtk_Toggle_Button_Record;
      Initialize (Toggle_Button, Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Toggle_Button : access Gtk_Toggle_Button_Record'Class;
                         Label         : in String := "")
   is
      function Internal (Label  : in String) return System.Address;
      pragma Import (C, Internal, "gtk_toggle_button_new_with_label");
      function Internal2 return System.Address;
      pragma Import (C, Internal2, "gtk_toggle_button_new");
   begin
      if Label = "" then
         Set_Object (Toggle_Button, Internal2);
      else
         Set_Object (Toggle_Button, Internal (Label & Ascii.NUL));
      end if;
      Initialize_User_Data (Toggle_Button);
   end Initialize;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
     (Toggle_Button : access Gtk_Toggle_Button_Record;
      Is_Active     : in Boolean)
   is
      procedure Internal
        (Toggle_Button : in System.Address;
         Is_Active     : in Gint);
      pragma Import (C, Internal, "gtk_toggle_button_set_active");

   begin
      Internal (Get_Object (Toggle_Button), Boolean'Pos (Is_Active));
   end Set_Active;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (Toggle_Button  : access Gtk_Toggle_Button_Record;
      Draw_Indicator : in Boolean)
   is
      procedure Internal
        (Toggle_Button  : in System.Address;
         Draw_Indicator : in Gint);
      pragma Import (C, Internal, "gtk_toggle_button_set_mode");

   begin
      Internal (Get_Object (Toggle_Button), Boolean'Pos (Draw_Indicator));
   end Set_Mode;

   -------------
   -- Toggled --
   -------------

   procedure Toggled (Toggle_Button : access Gtk_Toggle_Button_Record) is
      procedure Internal (Toggle_Button : in System.Address);
      pragma Import (C, Internal, "gtk_toggle_button_toggled");
   begin
      Internal (Get_Object (Toggle_Button));
   end Toggled;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      Label : String_Ptr := Get_Field (N, "label");

   begin
      if not N.Specific_Data.Created then
         if Label = null then
            Gen_New (N, "Toggle_Button", File => File);
         else
            Gen_New (N, "Toggle_Button", Label.all, File => File,
              Delim => '"');
         end if;
      end if;

      Button.Generate (N, File);
      Gen_Set (N, "Toggle_Button", "mode", File);
      Gen_Set (N, "Toggle_Button", "active", File);
   end Generate;

   procedure Generate
     (Toggle_Button : in out Object.Gtk_Object; N : in Node_Ptr)
   is
      S : String_Ptr := Get_Field (N, "label");

   begin
      if not N.Specific_Data.Created then
         if S = null then
            Gtk_New (Gtk_Toggle_Button (Toggle_Button));
         else
            Gtk_New (Gtk_Toggle_Button (Toggle_Button), S.all);
         end if;

         Set_Object (Get_Field (N, "name"), Toggle_Button);
         N.Specific_Data.Created := True;
      end if;

      Button.Generate (Toggle_Button, N);

      S := Get_Field (N, "mode");

      if S /= null then
         Set_Mode (Gtk_Toggle_Button (Toggle_Button), Boolean'Value (S.all));
      end if;

      S := Get_Field (N, "active");

      if S /= null then
         Set_Active (Gtk_Toggle_Button (Toggle_Button), Boolean'Value (S.all));
      end if;
   end Generate;

end Gtk.Toggle_Button;
