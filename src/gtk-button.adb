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

package body Gtk.Button is

   -------------
   -- Clicked --
   -------------

   procedure Clicked (Widget : in Gtk_Button) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_clicked");
   begin
      Internal (Get_Object (Widget));
   end Clicked;

   -----------
   -- Enter --
   -----------

   procedure Enter (Widget : in Gtk_Button) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_enter");
   begin
      Internal (Get_Object (Widget));
   end Enter;

   ----------------
   -- Get_Relief --
   ----------------

   function Get_Relief (Widget : in Gtk_Button)
                        return Gtk.Enums.Gtk_Relief_Style is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_button_get_relief");
   begin
      return Gtk.Enums.Gtk_Relief_Style'Val (Internal (Get_Object (Widget)));
   end Get_Relief;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Button) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_button_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Button; Label  : in String) is
      function Internal (S : String) return System.Address;
      pragma Import (C, Internal, "gtk_button_new_with_label");
   begin
      Set_Object (Widget, Internal (Label & Ascii.NUL));
   end Gtk_New;

   -----------
   -- Leave --
   -----------

   procedure Leave (Widget : in Gtk_Button) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_enter");
   begin
      Internal (Get_Object (Widget));
   end Leave;

   -------------
   -- Pressed --
   -------------

   procedure Pressed (Widget : in Gtk_Button) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_pressed");
   begin
      Internal (Get_Object (Widget));
   end Pressed;

   --------------
   -- Released --
   --------------

   procedure Released (Widget : in Gtk_Button) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_released");
   begin
      Internal (Get_Object (Widget));
   end Released;

   ----------------
   -- Set_Relief --
   ----------------

   procedure Set_Relief (Widget   : in out Gtk_Button;
                         NewStyle : in Gtk.Enums.Gtk_Relief_Style) is
      procedure Internal (Widget : in System.Address;
                          NewStyle : in Gint);
      pragma Import (C, Internal, "gtk_button_set_relief");
   begin
      Internal (Get_Object (Widget),
                Gtk.Enums.Gtk_Relief_Style'Pos (NewStyle));
   end Set_Relief;

end Gtk.Button;
