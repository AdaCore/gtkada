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
-- Library General Public License for more details.                  --
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

package body Gtk.Tooltips is

   ------------
   -- Enable --
   ------------

   procedure Enable (Tooltips : in Gtk_Tooltips'Class) is
      procedure Internal (Tooltips : System.Address);
      pragma Import (C, Internal, "gtk_tooltips_enable");
   begin
      Internal (Get_Object (Tooltips));
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (Tooltips : in Gtk_Tooltips'Class) is
      procedure Internal (Tooltips : System.Address);
      pragma Import (C, Internal, "gtk_tooltips_disable");
   begin
      Internal (Get_Object (Tooltips));
   end Disable;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Tooltips) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tooltips_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ---------------
   -- Set_Delay --
   ---------------

   procedure Set_Delay
     (Tooltips : in Gtk_Tooltips'Class;
      duration : in Guint)
   is
      procedure Internal (Tooltips : System.Address;
                          Duration : Guint);
      pragma Import (C, Internal, "gtk_tooltips_set_delay");
   begin
      Internal (Get_Object (Tooltips), Duration);
   end Set_Delay;

   -------------
   -- Set_Tip --
   -------------

   procedure Set_Tip
     (Tooltips    : in Gtk_Tooltips'Class;
      Widget      : in Gtk.Widget.Gtk_Widget'Class;
      Tip_Text    : in String;
      Tip_Private : in String)
   is
      procedure Internal (Tooltips : System.Address;
                          Widget   : System.Address;
                          Tip_Text : String;
                          Tip_Private : String);
      pragma Import (C, Internal, "gtk_tooltips_set_tip");
   begin
      Internal (Get_Object (Tooltips), Get_Object (Widget),
                Tip_Text & Ascii.NUL,
                Tip_Private & Ascii.NUL);
   end Set_Tip;

end Gtk.Tooltips;
