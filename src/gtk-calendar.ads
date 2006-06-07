-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  <c_version>2.8.17</c_version>
--  <group>Selectors</group>
--  <testgtk>create_calendar.adb</testgtk>
--  <screenshot>gtk-calendar</screenshot>

with Glib.Properties;
with Gtk.Widget;

package Gtk.Calendar is

   type Gtk_Calendar_Display_Options is mod 2 ** 8;

   Show_Heading : constant Gtk_Calendar_Display_Options;
   --  Specify that the month and year should be displayed.

   Show_Day_Names : constant Gtk_Calendar_Display_Options;
   --  Specify that three letter day descriptions should be present.

   No_Month_Change : constant Gtk_Calendar_Display_Options;
   --  Prevent the user from switching months with the calendar.

   Show_Week_Numbers : constant Gtk_Calendar_Display_Options;
   --  Display each week numbers of the current year, down the left side of
   --  the calendar.

   Week_Start_Monday : constant Gtk_Calendar_Display_Options;
   --  Start the calendar week on Monday, instead of the default Sunday.

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
      Month    : Guint;
      Year     : Guint) return Boolean;
   --  Shift the calendar to a different month/year.
   --  Return True if sucessful.

   procedure Select_Day
     (Calendar : access Gtk_Calendar_Record;
      Day      : Guint);
   --  Select a day from the current month.
   --  Only one day can be selected at a time.

   function Mark_Day
     (Calendar : access Gtk_Calendar_Record;
      Day      : Guint) return Boolean;
   --  Set a specified Day as marked in the Calendar.
   --  This is shown visually as a painted box around the Day.
   --  Note that several days can be marked.
   --  Return True if successful.

   function Unmark_Day
     (Calendar : access Gtk_Calendar_Record;
      Day      : Guint) return Boolean;
   --  Undo the marking of Day.
   --  Return True if sucessful.

   procedure Clear_Marks (Calendar : access Gtk_Calendar_Record);
   --  Clear all the marks set by Mark_Day.

   procedure Get_Date
     (Calendar : access Gtk_Calendar_Record;
      Year     : out Guint;
      Month    : out Guint;
      Day      : out Guint);
   --  Return the date currently selected.

   procedure Set_Display_Options
     (Calendar : access Gtk_Calendar_Record;
      Flags    : Gtk_Calendar_Display_Options);
   function Get_Display_Options
     (Calendar : access Gtk_Calendar_Record)
      return Gtk_Calendar_Display_Options;
   --  Sets display options (whether to display the heading and the month
   --  headings).

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   procedure Display_Options
     (Calendar : access Gtk_Calendar_Record;
      Flags    : Gtk_Calendar_Display_Options);
   pragma Obsolescent;  --  Display_Options
   --  Change the display options.
   --  See individual Display_Option flags for more details.

   procedure Freeze (Calendar : access Gtk_Calendar_Record);
   pragma Obsolescent;  --  Freeze
   --  Lock the display of the calendar until it is thawed.

   procedure Thaw (Calendar : access Gtk_Calendar_Record);
   pragma Obsolescent;  --  Thaw
   --  Defrost a calendar.
   --  All the changes made since the last Freeze are displayed.

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Day_Property
   --  Type:  Int
   --  Descr: The selected day (as a number between 1 and 31, or 0 to unselect
   --         the currently selected day)
   --
   --  Name:  Month_Property
   --  Type:  Int
   --  Descr: The selected month (as a number between 0 and 11)
   --
   --  Name:  No_Month_Change_Property
   --  Type:  Boolean
   --  Descr: If TRUE, the selected month cannot be changed
   --
   --  Name:  Show_Day_Names_Property
   --  Type:  Boolean
   --  Descr: If TRUE, day names are displayed
   --
   --  Name:  Show_Heading_Property
   --  Type:  Boolean
   --  Descr: If TRUE, a heading is displayed
   --
   --  Name:  Show_Week_Numbers_Property
   --  Type:  Boolean
   --  Descr: If TRUE, week numbers are displayed
   --
   --  Name:  Year_Property
   --  Type:  Int
   --  Descr: The selected year
   --
   --  </properties>

   Day_Property               : constant Glib.Properties.Property_Int;
   Month_Property             : constant Glib.Properties.Property_Int;
   No_Month_Change_Property   : constant Glib.Properties.Property_Boolean;
   Show_Day_Names_Property    : constant Glib.Properties.Property_Boolean;
   Show_Heading_Property      : constant Glib.Properties.Property_Boolean;
   Show_Week_Numbers_Property : constant Glib.Properties.Property_Boolean;
   Year_Property              : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  - "month_changed"
   --    procedure Handler (Calendar : access Gtk_Calendar_Record'Class);
   --    Emitted when the user clicks a button to change the selected month on
   --    calendar.
   --
   --  - "day_selected"
   --    procedure Handler (Calendar : access Gtk_Calendar_Record'Class);
   --    Emitted when the user selects a day on a calendar.
   --
   --  - "day_selected_double_click"
   --    procedure Handler (Calendar : access Gtk_Calendar_Record'Class);
   --    Emitted when the user double clicks a day on a calendar.
   --
   --  - "prev_month"
   --    procedure Handler (Calendar : access Gtk_Calendar_Record'Class);
   --    Emitted when the user selects the previous month on a calendar.
   --
   --  - "next_month"
   --    procedure Handler (Calendar : access Gtk_Calendar_Record'Class);
   --    Emitted when the user selects the next month on a calendar.
   --
   --  - "prev_year"
   --    procedure Handler (Calendar : access Gtk_Calendar_Record'Class);
   --    Emitted when the user selects the previous year on a calendar.
   --
   --  - "next_year"
   --    procedure Handler (Calendar : access Gtk_Calendar_Record'Class);
   --    Emitted when the user selects the next year on a calendar.
   --
   --  </signals>

   Signal_Day_Selected              : constant String := "day_selected";
   Signal_Day_Selected_Double_Click : constant String :=
     "day_selected_double_click";
   Signal_Month_Changed             : constant String := "month_changed";
   Signal_Next_Month                : constant String := "next_month";
   Signal_Next_Year                 : constant String := "next_year";
   Signal_Prev_Month                : constant String := "prev_month";
   Signal_Prev_Year                 : constant String := "prev_year";

private
   type Gtk_Calendar_Record is new Gtk.Widget.Gtk_Widget_Record
      with null record;

   Day_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("day");
   Month_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("month");
   No_Month_Change_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("no-month-change");
   Show_Day_Names_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-day-names");
   Show_Heading_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-heading");
   Show_Week_Numbers_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-week-numbers");
   Year_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("year");

   pragma Import (C, Get_Type, "gtk_calendar_get_type");

   Show_Heading      : constant Gtk_Calendar_Display_Options := 2 ** 0;
   Show_Day_Names    : constant Gtk_Calendar_Display_Options := 2 ** 1;
   No_Month_Change   : constant Gtk_Calendar_Display_Options := 2 ** 2;
   Show_Week_Numbers : constant Gtk_Calendar_Display_Options := 2 ** 3;
   Week_Start_Monday : constant Gtk_Calendar_Display_Options := 2 ** 4;

end Gtk.Calendar;
