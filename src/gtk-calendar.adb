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

with Gtk.Widget; use Gtk.Widget;
with System;

package body Gtk.Calendar is

   ----------
   -- Left --
   ----------

   function "and"
     (Left, Right : Gtk_Calendar_Display_Options)
      return Gtk_Calendar_Display_Options is
   begin
      return Left + Right;
   end "and";

   -----------------
   -- Clear_Marks --
   -----------------

   procedure Clear_Marks (Calendar : access Gtk_Calendar_Record) is
      procedure Internal (Calendar : in System.Address);
      pragma Import (C, Internal, "gtk_calendar_clear_marks");
   begin
      Internal (Get_Object (Calendar));
   end Clear_Marks;

   ---------------------
   -- Display_Options --
   ---------------------

   procedure Display_Options
     (Calendar : access Gtk_Calendar_Record;
      Flags    : in Gtk_Calendar_Display_Options)
   is
      procedure Internal
        (Calendar : in System.Address;
         Flags    : in Gint);
      pragma Import (C, Internal, "gtk_calendar_display_options");
   begin
      Internal (Get_Object (Calendar),
                Gtk_Calendar_Display_Options'Pos (Flags));
   end Display_Options;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Calendar : access Gtk_Calendar_Record) is
      procedure Internal (Calendar : in System.Address);
      pragma Import (C, Internal, "gtk_calendar_freeze");
   begin
      Internal (Get_Object (Calendar));
   end Freeze;

   --------------
   -- Get_Date --
   --------------

   procedure Get_Date
     (Calendar : access Gtk_Calendar_Record;
      Year     : out Guint;
      Month    : out Guint;
      Day      : out Guint)
   is
      procedure Internal
        (Calendar : System.Address;
         Year     : out Guint;
         Month    : out Guint;
         Day      : out Guint);
      pragma Import (C, Internal, "gtk_calendar_get_date");

   begin
      Internal (Get_Object (Calendar), Year, Month, Day);
   end Get_Date;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Calendar) is
   begin
      Widget := new Gtk_Calendar_Record;
      Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Calendar_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_calendar_new");
   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
   end Initialize;

   --------------
   -- Mark_Day --
   --------------

   function Mark_Day
     (Calendar : access Gtk_Calendar_Record;
      Day      : in Guint) return Boolean
   is
      function Internal
        (Calendar : in System.Address;
         Day      : in Guint) return Gint;
      pragma Import (C, Internal, "gtk_calendar_mark_day");

   begin
      return Boolean'Val (Internal (Get_Object (Calendar), Day));
   end Mark_Day;

   ----------------
   -- Select_Day --
   ----------------

   procedure Select_Day
     (Calendar : access Gtk_Calendar_Record;
      Day      : in Guint)
   is
      procedure Internal
        (Calendar : in System.Address;
         Day      : in Guint);
      pragma Import (C, Internal, "gtk_calendar_select_day");

   begin
      Internal (Get_Object (Calendar), Day);
   end Select_Day;

   ------------------
   -- Select_Month --
   ------------------

   function Select_Month
     (Calendar : access Gtk_Calendar_Record;
      Month    : in Guint;
      Year     : in Guint) return Boolean
   is
      function Internal
        (Calendar : in System.Address;
         Month    : in Guint;
         Year     : in Guint) return Gint;
      pragma Import (C, Internal, "gtk_calendar_select_month");

   begin
      return Boolean'Val (Internal (Get_Object (Calendar), Month, Year));
   end Select_Month;

   ----------
   -- Thaw --
   ----------

   procedure Thaw (Calendar : access Gtk_Calendar_Record) is
      procedure Internal (Calendar : in System.Address);
      pragma Import (C, Internal, "gtk_calendar_thaw");
   begin
      Internal (Get_Object (Calendar));
   end Thaw;

   ----------------
   -- Unmark_Day --
   ----------------

   function Unmark_Day
     (Calendar : access Gtk_Calendar_Record;
      Day      : in Guint) return Boolean
   is
      function Internal
        (Calendar : in System.Address;
         Day      : in Guint) return Gint;
      pragma Import (C, Internal, "gtk_calendar_unmark_day");

   begin
      return Boolean'Val (Internal (Get_Object (Calendar), Day));
   end Unmark_Day;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      Id : constant Gtk_Type := Get_Type;
      pragma Warnings (Off, Id);

   begin
      Gen_New (N, "Calendar", File => File);
      Widget.Generate (N, File);
   end Generate;

end Gtk.Calendar;
