-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
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

package body Gtk.Toggle_Button is

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active (Widget : in Gtk_Toggle_Button)
                        return      Boolean
   is
      function Internal (Widget : in System.Address)
                         return      Integer;
      pragma Import (C, Internal, "ada_toggle_button_get_active");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Is_Active;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Toggle_Button)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_toggle_button_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Toggle_Button;
                      Label  : in String)
   is
      function Internal (Label  : in String)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_toggle_button_new_with_label");
   begin
      Set_Object (Widget, Internal (Label & Ascii.NUL));
   end Gtk_New;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
      (Toggle_Button  : in Gtk_Toggle_Button;
       Draw_Indicator : in Boolean)
   is
      procedure Internal
         (Toggle_Button  : in System.Address;
          Draw_Indicator : in Gint);
      pragma Import (C, Internal, "gtk_toggle_button_set_mode");
   begin
      Internal (Get_Object (Toggle_Button),
                Boolean'Pos (Draw_Indicator));
   end Set_Mode;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State
      (Toggle_Button : in Gtk_Toggle_Button;
       Active        : in Boolean)
   is
      procedure Internal
         (Toggle_Button : in System.Address;
          State         : in Gint);
      pragma Import (C, Internal, "gtk_toggle_button_set_state");
   begin
      Internal (Get_Object (Toggle_Button),
                Boolean'Pos (Active));
   end Set_State;

   -------------
   -- Toggled --
   -------------

   procedure Toggled (Toggle_Button : in Gtk_Toggle_Button)
   is
      procedure Internal (Toggle_Button : in System.Address);
      pragma Import (C, Internal, "gtk_toggle_button_toggled");
   begin
      Internal (Get_Object (Toggle_Button));
   end Toggled;

end Gtk.Toggle_Button;
