------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  <description>
--  Gtk.Calendar.Gtk_Calendar is a widget that displays a Gregorian calendar,
--  one month at a time. It can be created with Gtk.Calendar.Gtk_New.
--
--  The month and year currently displayed can be altered with
--  Gtk.Calendar.Select_Month. The exact day can be selected from the displayed
--  month using Gtk.Calendar.Select_Day.
--
--  To place a visual marker on a particular day, use Gtk.Calendar.Mark_Day
--  and to remove the marker, Gtk.Calendar.Unmark_Day. Alternative, all marks
--  can be cleared with Gtk.Calendar.Clear_Marks.
--
--  The way in which the calendar itself is displayed can be altered using
--  Gtk.Calendar.Set_Display_Options.
--
--  The selected date can be retrieved from a Gtk.Calendar.Gtk_Calendar using
--  Gtk.Calendar.Get_Date.
--
--  Users should be aware that, although the Gregorian calendar is the legal
--  calendar in most countries, it was adopted progressively between 1582 and
--  1929. Display before these dates is likely to be historically incorrect.
--
--  </description>
--  <screenshot>gtk-calendar</screenshot>
--  <group>Selectors</group>
--  <testgtk>create_calendar.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Calendar is

   type Gtk_Calendar_Record is new Gtk_Widget_Record with null record;
   type Gtk_Calendar is access all Gtk_Calendar_Record'Class;

   type Gtk_Calendar_Display_Options is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Calendar_Display_Options);
   --  These options can be used to influence the display and behaviour of a
   --  Gtk.Calendar.Gtk_Calendar.

   Show_Heading : constant Gtk_Calendar_Display_Options := 1;
   Show_Day_Names : constant Gtk_Calendar_Display_Options := 2;
   No_Month_Change : constant Gtk_Calendar_Display_Options := 4;
   Show_Week_Numbers : constant Gtk_Calendar_Display_Options := 8;
   Show_Details : constant Gtk_Calendar_Display_Options := 32;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Calendar_Detail_Func is access function
     (Calendar : not null access Gtk_Calendar_Record'Class;
      Year     : Guint;
      Month    : Guint;
      Day      : Guint) return UTF8_String;
   --  This kind of functions provide Pango markup with detail information for
   --  the specified day. Examples for such details are holidays or
   --  appointments. The function returns null when no information is
   --  available.
   --  Since: gtk+ 2.14
   --  "calendar": a Gtk.Calendar.Gtk_Calendar.
   --  "year": the year for which details are needed.
   --  "month": the month for which details are needed.
   --  "day": the day of Month for which details are needed.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Calendar_Display_Options_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Calendar_Display_Options);
   type Property_Gtk_Calendar_Display_Options is new Gtk_Calendar_Display_Options_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Calendar : out Gtk_Calendar);
   procedure Initialize
      (Calendar : not null access Gtk_Calendar_Record'Class);
   --  Creates a new calendar, with the current date being selected.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Calendar_New return Gtk_Calendar;
   --  Creates a new calendar, with the current date being selected.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_calendar_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clear_Marks (Calendar : not null access Gtk_Calendar_Record);
   --  Remove all visual markers.

   procedure Get_Date
      (Calendar : not null access Gtk_Calendar_Record;
       Year     : out Guint;
       Month    : out Guint;
       Day      : out Guint);
   --  Obtains the selected date from a Gtk.Calendar.Gtk_Calendar.
   --  "year": location to store the year as a decimal number (e.g. 2011), or
   --  null
   --  "month": location to store the month number (between 0 and 11), or null
   --  "day": location to store the day number (between 1 and 31), or null

   function Get_Day_Is_Marked
      (Calendar : not null access Gtk_Calendar_Record;
       Day      : Guint) return Boolean;
   --  Returns if the Day of the Calendar is already marked.
   --  Since: gtk+ 3.0
   --  "day": the day number between 1 and 31.

   function Get_Detail_Height_Rows
      (Calendar : not null access Gtk_Calendar_Record) return Glib.Gint;
   --  Queries the height of detail cells, in rows. See
   --  Gtk.Calendar.Gtk_Calendar:detail-width-chars.
   --  Since: gtk+ 2.14

   procedure Set_Detail_Height_Rows
      (Calendar : not null access Gtk_Calendar_Record;
       Rows     : Glib.Gint);
   --  Updates the height of detail cells. See
   --  Gtk.Calendar.Gtk_Calendar:detail-height-rows.
   --  Since: gtk+ 2.14
   --  "rows": detail height in rows.

   function Get_Detail_Width_Chars
      (Calendar : not null access Gtk_Calendar_Record) return Glib.Gint;
   --  Queries the width of detail cells, in characters. See
   --  Gtk.Calendar.Gtk_Calendar:detail-width-chars.
   --  Since: gtk+ 2.14

   procedure Set_Detail_Width_Chars
      (Calendar : not null access Gtk_Calendar_Record;
       Chars    : Glib.Gint);
   --  Updates the width of detail cells. See
   --  Gtk.Calendar.Gtk_Calendar:detail-width-chars.
   --  Since: gtk+ 2.14
   --  "chars": detail width in characters.

   function Get_Display_Options
      (Calendar : not null access Gtk_Calendar_Record)
       return Gtk_Calendar_Display_Options;
   --  Returns the current display options of Calendar.
   --  Since: gtk+ 2.4

   procedure Set_Display_Options
      (Calendar : not null access Gtk_Calendar_Record;
       Flags    : Gtk_Calendar_Display_Options);
   --  Sets display options (whether to display the heading and the month
   --  headings).
   --  Since: gtk+ 2.4
   --  "flags": the display options to set

   procedure Mark_Day
      (Calendar : not null access Gtk_Calendar_Record;
       Day      : Guint);
   --  Places a visual marker on a particular day.
   --  "day": the day number to mark between 1 and 31.

   procedure Select_Day
      (Calendar : not null access Gtk_Calendar_Record;
       Day      : Guint);
   --  Selects a day from the current month.
   --  "day": the day number between 1 and 31, or 0 to unselect the currently
   --  selected day.

   procedure Select_Month
      (Calendar : not null access Gtk_Calendar_Record;
       Month    : Guint;
       Year     : Guint);
   --  Shifts the calendar to a different month.
   --  "month": a month number between 0 and 11.
   --  "year": the year the month is in.

   procedure Set_Detail_Func
      (Calendar : not null access Gtk_Calendar_Record;
       Func     : Gtk_Calendar_Detail_Func);
   --  Installs a function which provides Pango markup with detail information
   --  for each day. Examples for such details are holidays or appointments.
   --  That information is shown below each day when
   --  Gtk.Calendar.Gtk_Calendar:show-details is set. A tooltip containing with
   --  full detail information is provided, if the entire text should not fit
   --  into the details area, or if Gtk.Calendar.Gtk_Calendar:show-details is
   --  not set.
   --  The size of the details area can be restricted by setting the
   --  Gtk.Calendar.Gtk_Calendar:detail-width-chars and
   --  Gtk.Calendar.Gtk_Calendar:detail-height-rows properties.
   --  Since: gtk+ 2.14
   --  "func": a function providing details for each day.

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Detail_Func_User_Data is

      type Gtk_Calendar_Detail_Func is access function
        (Calendar  : not null access Gtk.Calendar.Gtk_Calendar_Record'Class;
         Year      : Guint;
         Month     : Guint;
         Day       : Guint;
         User_Data : User_Data_Type) return UTF8_String;
      --  This kind of functions provide Pango markup with detail information for
      --  the specified day. Examples for such details are holidays or
      --  appointments. The function returns null when no information is
      --  available.
      --  Since: gtk+ 2.14
      --  "calendar": a Gtk.Calendar.Gtk_Calendar.
      --  "year": the year for which details are needed.
      --  "month": the month for which details are needed.
      --  "day": the day of Month for which details are needed.
      --  "user_data": the data passed with Gtk.Calendar.Set_Detail_Func.

      procedure Set_Detail_Func
         (Calendar : not null access Gtk.Calendar.Gtk_Calendar_Record'Class;
          Func     : Gtk_Calendar_Detail_Func;
          Data     : User_Data_Type);
      --  Installs a function which provides Pango markup with detail
      --  information for each day. Examples for such details are holidays or
      --  appointments. That information is shown below each day when
      --  Gtk.Calendar.Gtk_Calendar:show-details is set. A tooltip containing
      --  with full detail information is provided, if the entire text should
      --  not fit into the details area, or if
      --  Gtk.Calendar.Gtk_Calendar:show-details is not set.
      --  The size of the details area can be restricted by setting the
      --  Gtk.Calendar.Gtk_Calendar:detail-width-chars and
      --  Gtk.Calendar.Gtk_Calendar:detail-height-rows properties.
      --  Since: gtk+ 2.14
      --  "func": a function providing details for each day.
      --  "data": data to pass to Func invokations.

   end Set_Detail_Func_User_Data;

   procedure Unmark_Day
      (Calendar : not null access Gtk_Calendar_Record;
       Day      : Guint);
   --  Removes the visual marker from a particular day.
   --  "day": the day number to unmark between 1 and 31.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Day_Property : constant Glib.Properties.Property_Int;
   --  The selected day (as a number between 1 and 31, or 0 to unselect the
   --  currently selected day). This property gets initially set to the current
   --  day.

   Detail_Height_Rows_Property : constant Glib.Properties.Property_Int;
   --  Height of a detail cell, in rows. A value of 0 allows any width. See
   --  Gtk.Calendar.Set_Detail_Func.

   Detail_Width_Chars_Property : constant Glib.Properties.Property_Int;
   --  Width of a detail cell, in characters. A value of 0 allows any width.
   --  See Gtk.Calendar.Set_Detail_Func.

   Month_Property : constant Glib.Properties.Property_Int;
   --  The selected month (as a number between 0 and 11). This property gets
   --  initially set to the current month.

   No_Month_Change_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether the selected month can be changed.

   Show_Day_Names_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether day names are displayed.

   Show_Details_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether details are shown directly in the widget, or if they
   --  are available only as tooltip. When this property is set days with
   --  details are marked.

   Show_Heading_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether a heading is displayed.

   Show_Week_Numbers_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether week numbers are displayed.

   Year_Property : constant Glib.Properties.Property_Int;
   --  The selected year. This property gets initially set to the current
   --  year.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Calendar_Void is not null access procedure (Self : access Gtk_Calendar_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Day_Selected : constant Glib.Signal_Name := "day-selected";
   procedure On_Day_Selected
      (Self  : not null access Gtk_Calendar_Record;
       Call  : Cb_Gtk_Calendar_Void;
       After : Boolean := False);
   procedure On_Day_Selected
      (Self  : not null access Gtk_Calendar_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user selects a day.

   Signal_Day_Selected_Double_Click : constant Glib.Signal_Name := "day-selected-double-click";
   procedure On_Day_Selected_Double_Click
      (Self  : not null access Gtk_Calendar_Record;
       Call  : Cb_Gtk_Calendar_Void;
       After : Boolean := False);
   procedure On_Day_Selected_Double_Click
      (Self  : not null access Gtk_Calendar_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user double-clicks a day.

   Signal_Month_Changed : constant Glib.Signal_Name := "month-changed";
   procedure On_Month_Changed
      (Self  : not null access Gtk_Calendar_Record;
       Call  : Cb_Gtk_Calendar_Void;
       After : Boolean := False);
   procedure On_Month_Changed
      (Self  : not null access Gtk_Calendar_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user clicks a button to change the selected month on a
   --  calendar.

   Signal_Next_Month : constant Glib.Signal_Name := "next-month";
   procedure On_Next_Month
      (Self  : not null access Gtk_Calendar_Record;
       Call  : Cb_Gtk_Calendar_Void;
       After : Boolean := False);
   procedure On_Next_Month
      (Self  : not null access Gtk_Calendar_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user switched to the next month.

   Signal_Next_Year : constant Glib.Signal_Name := "next-year";
   procedure On_Next_Year
      (Self  : not null access Gtk_Calendar_Record;
       Call  : Cb_Gtk_Calendar_Void;
       After : Boolean := False);
   procedure On_Next_Year
      (Self  : not null access Gtk_Calendar_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when user switched to the next year.

   Signal_Prev_Month : constant Glib.Signal_Name := "prev-month";
   procedure On_Prev_Month
      (Self  : not null access Gtk_Calendar_Record;
       Call  : Cb_Gtk_Calendar_Void;
       After : Boolean := False);
   procedure On_Prev_Month
      (Self  : not null access Gtk_Calendar_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the user switched to the previous month.

   Signal_Prev_Year : constant Glib.Signal_Name := "prev-year";
   procedure On_Prev_Year
      (Self  : not null access Gtk_Calendar_Record;
       Call  : Cb_Gtk_Calendar_Void;
       After : Boolean := False);
   procedure On_Prev_Year
      (Self  : not null access Gtk_Calendar_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when user switched to the previous year.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Calendar_Record, Gtk_Calendar);
   function "+"
     (Widget : access Gtk_Calendar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Calendar
   renames Implements_Gtk_Buildable.To_Object;

private
   Year_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("year");
   Show_Week_Numbers_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-week-numbers");
   Show_Heading_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-heading");
   Show_Details_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-details");
   Show_Day_Names_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-day-names");
   No_Month_Change_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("no-month-change");
   Month_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("month");
   Detail_Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("detail-width-chars");
   Detail_Height_Rows_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("detail-height-rows");
   Day_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("day");
end Gtk.Calendar;
