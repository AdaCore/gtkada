-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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
with Interfaces.C.Strings;

package body Gtk.Tooltips is

   ------------
   -- Enable --
   ------------

   procedure Enable (Tooltips : access Gtk_Tooltips_Record) is
      procedure Internal (Tooltips : System.Address);
      pragma Import (C, Internal, "gtk_tooltips_enable");
   begin
      Internal (Get_Object (Tooltips));
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (Tooltips : access Gtk_Tooltips_Record) is
      procedure Internal (Tooltips : System.Address);
      pragma Import (C, Internal, "gtk_tooltips_disable");
   begin
      Internal (Get_Object (Tooltips));
   end Disable;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Tooltips) is
   begin
      Widget := new Gtk_Tooltips_Record;
      Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Tooltips_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tooltips_new");
   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
   end Initialize;

   ---------------
   -- Set_Delay --
   ---------------

   procedure Set_Delay
     (Tooltips : access Gtk_Tooltips_Record;
      duration : in Guint := 500)
   is
      procedure Internal (Tooltips : System.Address;
                          Duration : Guint);
      pragma Import (C, Internal, "gtk_tooltips_set_delay");
   begin
      Internal (Get_Object (Tooltips), Duration);
   end Set_Delay;

   ----------------
   -- Set_Colors --
   ----------------

   procedure Set_Colors (Tooltips   : access Gtk_Tooltips_Record;
                         Foreground : Gdk.Color.Gdk_Color;
                         Background : Gdk.Color.Gdk_Color)
   is
      procedure Internal (Tooltips   : System.Address;
                          Foreground : System.Address;
                          Background : System.Address);
      pragma Import (C, Internal, "gtk_tooltips_set_colors");
      Fore : aliased Gdk.Color.Gdk_Color := Foreground;
      Back : aliased Gdk.Color.Gdk_Color := Background;
   begin
      Internal (Get_Object (Tooltips), Fore'Address, Back'Address);
   end Set_Colors;

   -------------
   -- Set_Tip --
   -------------

   procedure Set_Tip
     (Tooltips    : access Gtk_Tooltips_Record;
      Widget      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tip_Text    : in String;
      Tip_Private : in String := "")
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

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
                     return Tooltips_Data
   is
      type C_Data is record
         Tooltips : System.Address;
         Widget   : System.Address;
         Text     : Interfaces.C.Strings.chars_ptr;
         Privat   : Interfaces.C.Strings.chars_ptr;
      end record;
      --  keep in sync with the C structure.
      type C_Data_Access is access C_Data;

      function Internal (Widget : System.Address) return C_Data_Access;
      pragma Import (C, Internal, "gtk_tooltips_data_get");

      Data : C_Data_Access := Internal (Get_Object (Widget));
      T    : String := Interfaces.C.Strings.Value (Data.Text);
      P    : String := Interfaces.C.Strings.Value (Data.Privat);
      Data2 : Tooltips_Data (Text_Length    => T'Length,
                             Private_Length => P'Length);
      Stub : Gtk_Tooltips_Record;
   begin
      Data2.Tooltips     := Gtk_Tooltips (Get_User_Data (Data.Tooltips, Stub));
      Data2.Widget       := Gtk.Widget.Gtk_Widget (Widget);
      Data2.Text         := T;
      Data2.Text_Private := P;
      return Data2;
   end Get_Data;

end Gtk.Tooltips;
