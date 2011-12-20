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

with System;
with Gtk; use Gtk;

package body Gnome.Date_Edit is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Date_Edit       : out Gnome_Date_Edit;
      The_Time        : Time_T;
      Show_Time       : Boolean;
      Use_24hr_Format : Boolean)
   is
      Flags : Gnome_Date_Edit_Flags := 0;
   begin
      Date_Edit := new Gnome_Date_Edit_Record;

      if Show_Time then
         Flags := Flags or Gnome.Date_Edit.Show_Time;
      end if;

      if Use_24hr_Format then
         Flags := Flags or Twentyfour_Hour;
      end if;

      Initialize (Date_Edit, The_Time, Flags);
   end Gnome_New;

   procedure Gnome_New
     (Date_Edit : out Gnome_Date_Edit;
      The_Time  : Time_T;
      Flags     : Gnome_Date_Edit_Flags) is
   begin
      Date_Edit := new Gnome_Date_Edit_Record;
      Initialize (Date_Edit, The_Time, Flags);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Date_Edit : access Gnome_Date_Edit_Record'Class;
      The_Time  : Time_T;
      Flags     : Gnome_Date_Edit_Flags)
   is
      function Internal
        (The_Time : Time_T;
         Flags    : Gnome_Date_Edit_Flags) return System.Address;
      pragma Import (C, Internal, "gnome_date_edit_new_flags");

   begin
      Set_Object (Date_Edit, Internal (The_Time, Flags));
   end Initialize;

   --------------
   -- Get_Time --
   --------------

   function Get_Time (Date_Edit : Gnome_Date_Edit) return Time_T is
      function Internal (Date_Edit : System.Address) return Time_T;
      pragma Import (C, Internal, "gnome_date_edit_get_time");

   begin
      return Internal (Get_Object (Date_Edit));
   end Get_Time;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
     (Date_Edit : Gnome_Date_Edit) return Gnome_Date_Edit_Flags
   is
      function Internal
        (Date_Edit : System.Address) return Gnome_Date_Edit_Flags;
      pragma Import (C, Internal, "gnome_date_edit_get_flags");

   begin
      return Internal (Get_Object (Date_Edit));
   end Get_Flags;

   ---------------
   -- Set_Flags --
   ---------------

   procedure Set_Flags
     (Date_Edit : Gnome_Date_Edit;
      Flags     : Gnome_Date_Edit_Flags)
   is
      procedure Internal
        (Date_Edit : System.Address;
         Flags     : Gnome_Date_Edit_Flags);
      pragma Import (C, Internal, "gnome_date_edit_set_flags");

   begin
      Internal (Get_Object (Date_Edit), Flags);
   end Set_Flags;

   ---------------------
   -- Set_Popup_Range --
   ---------------------

   procedure Set_Popup_Range
     (Date_Edit : Gnome_Date_Edit;
      Low_Hour  : Integer;
      Up_Hour   : Integer)
   is
      procedure Internal
        (Date_Edit : System.Address;
         Low_Hour  : Integer;
         Up_Hour   : Integer);
      pragma Import (C, Internal, "gnome_date_edit_set_popup_range");

   begin
      Internal (Get_Object (Date_Edit), Low_Hour, Up_Hour);
   end Set_Popup_Range;

   --------------
   -- Set_Time --
   --------------

   procedure Set_Time
     (Date_Edit : Gnome_Date_Edit;
      The_Time  : Time_T)
   is
      procedure Internal (Date_Edit : System.Address; The_Time : Time_T);
      pragma Import (C, Internal, "gnome_date_edit_set_time");

   begin
      Internal (Get_Object (Date_Edit), The_Time);
   end Set_Time;

end Gnome.Date_Edit;
