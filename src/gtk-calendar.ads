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

--  <description>
--  Gtk_Calendar is a widget that displays a calendar, one month at a time.
--  It can be created with Gtk_New.
--
--  The month and year currently displayed can be altered with Select_Month.
--  The exact day can be selected from the displayed month using Select_Day.
--
--  The way in which the calendar itself is displayed can be altered using
--  Display_Options.
--
--  The selected date can be retrieved from a Gtk_Calendar using Get_Date.
--
--  If performing many 'mark' operations, the calendar can be frozen to prevent
--  flicker, using Freeze, and 'thawed' again using Thaw.
--  </description>
--  <c_version>1.2.8</c_version>

with Gtk.Widget;

package Gtk.Calendar is

   type Gtk_Calendar_Display_Options is private;
   Show_Heading : constant Gtk_Calendar_Display_Options;
   --  Specify that the month and year should be displayed.

   Show_Day_Names : constant Gtk_Calendar_Display_Options;
   --  Specify that three letter day descriptions should be present.

   No_Month_Change : constant Gtk_Calendar_Display_Options;
   --  Prevent the user from switching months with the calendar.

   Show_Week_Number : constant Gtk_Calendar_Display_Options;
   --  Display each week numbers of the current year, down the left side of
   --  the calendar.

   Week_Start_Monday : constant Gtk_Calendar_Display_Options;
   --  Start the calendar week on Monday, instead of the default Sunday.

   function "and" (Left, Right : Gtk_Calendar_Display_Options)
                   return Gtk_Calendar_Display_Options;

   type Gtk_Calendar_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Calendar is access all Gtk_Calendar_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Calendar);
   --  Create a new Calendar that points to the current date.

   procedure Initialize (Widget : access Gtk_Calendar_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Calendar.

   function Select_Month
     (Calendar : access Gtk_Calendar_Record;
      Month    : in Guint;
      Year     : in Guint) return Boolean;
   --  Shift the calendar to a different month/year.
   --  Return True if sucessful.

   procedure Select_Day
     (Calendar : access Gtk_Calendar_Record;
      Day      : in Guint);
   --  Select a day from the current month.
   --  Only one day can be selected at a time.

   function Mark_Day
     (Calendar : access Gtk_Calendar_Record;
      Day      : in Guint) return Boolean;
   --  Set a specified Day as marked in the Calendar.
   --  This is shown visually as a painted box around the Day.
   --  Note that several days can be marked.
   --  Return True if successful.

   function Unmark_Day
     (Calendar : access Gtk_Calendar_Record;
      Day      : in Guint) return Boolean;
   --  Undo the marking of Day.
   --  Return True if sucessful.

   procedure Clear_Marks (Calendar : access Gtk_Calendar_Record);
   --  Clear all the marks set by Mark_Day.

   procedure Display_Options
     (Calendar : access Gtk_Calendar_Record;
      Flags    : in Gtk_Calendar_Display_Options);
   --  Change the display options.
   --  See individual Display_Option flags for more details.

   procedure Get_Date
     (Calendar : access Gtk_Calendar_Record;
      Year     : out Guint;
      Month    : out Guint;
      Day      : out Guint);
   --  Return the date currently selected.

   procedure Freeze (Calendar : access Gtk_Calendar_Record);
   --  Lock the display of the calendar until it is thawed.

   procedure Thaw (Calendar : access Gtk_Calendar_Record);
   --  Defrost a calendar.
   --  All the changes made since the last Freeze are displayed.

   ----------------------
   -- Support for Gate --
   ----------------------

   procedure Generate (N : in Node_Ptr; File : in File_Type);
   --  Gate internal function

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  - "month_changed"
   --    procedure Handler (Calendar : access Gtk_Calendar_Record'Class);
   --
   --  Emitted when the user clicks a button to change the selected month on a
   --  calendar.
   --  </signals>

private
   type Gtk_Calendar_Record is new Gtk.Widget.Gtk_Widget_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_calendar_get_type");

   type Gtk_Calendar_Display_Options is new Gint;
   Show_Heading : constant Gtk_Calendar_Display_Options := 1;
   Show_Day_Names : constant Gtk_Calendar_Display_Options := 2;
   No_Month_Change : constant Gtk_Calendar_Display_Options := 4;
   Show_Week_Number : constant Gtk_Calendar_Display_Options := 8;
   Week_Start_Monday : constant Gtk_Calendar_Display_Options := 16;

end Gtk.Calendar;
