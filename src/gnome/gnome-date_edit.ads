------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Gtk.Box;

package Gnome.Date_Edit is

   type Gnome_Date_Edit_Flags is mod 2 ** 32;

   Show_Time             : constant Gnome_Date_Edit_Flags := 1;
   Twentyfour_Hour       : constant Gnome_Date_Edit_Flags := 2;
   Week_Starts_On_Monday : constant Gnome_Date_Edit_Flags := 4;

   type Time_T is new Long_Integer;

   type Gnome_Date_Edit_Record is new Gtk.Box.Gtk_Hbox_Record
     with private;
   type Gnome_Date_Edit is access all Gnome_Date_Edit_Record'Class;

   procedure Gnome_New
     (Date_Edit       : out Gnome_Date_Edit;
      The_Time        : Time_T;
      Show_Time       : Boolean;
      Use_24hr_Format : Boolean);

   procedure Gnome_New
     (Date_Edit : out Gnome_Date_Edit;
      The_Time  : Time_T;
      Flags     : Gnome_Date_Edit_Flags);

   procedure Initialize
     (Date_Edit : access Gnome_Date_Edit_Record'Class;
      The_Time  : Time_T;
      Flags     : Gnome_Date_Edit_Flags);

   function Get_Type return Gtk.Gtk_Type;

   procedure Set_Time
     (Date_Edit : Gnome_Date_Edit;
      The_Time  : Time_T);

   function Get_Time
     (Date_Edit : Gnome_Date_Edit) return Time_T;

   procedure Set_Popup_Range
     (Date_Edit : Gnome_Date_Edit;
      Low_Hour  : Integer;
      Up_Hour   : Integer);

   procedure Set_Flags
     (Date_Edit : Gnome_Date_Edit;
      Flags     : Gnome_Date_Edit_Flags);

   function Get_Flags
     (Date_Edit : Gnome_Date_Edit) return Gnome_Date_Edit_Flags;

private
   type Gnome_Date_Edit_Record is new Gtk.Box.Gtk_Hbox_Record
     with null record;

   pragma Import (C, Get_Type, "gnome_date_edit_get_type");
end Gnome.Date_Edit;
