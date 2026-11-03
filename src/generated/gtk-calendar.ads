------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Displays a Gregorian calendar, one month at a time.
--
--  <picture> <source srcset="calendar-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkCalendar" src="calendar.png"> </picture>
--  A `GtkCalendar` can be created with [ctorGtk.Calendar.new].
--
--  The selected date can be retrieved from a `GtkCalendar` using
--  [methodGtk.Calendar.get_date]. It can be altered with
--  [methodGtk.Calendar.set_date].
--
--  To place a visual marker on a particular day, use
--  [methodGtk.Calendar.mark_day] and to remove the marker,
--  [methodGtk.Calendar.unmark_day]. Alternative, all marks can be cleared with
--  [methodGtk.Calendar.clear_marks].
--
--  Users should be aware that, although the Gregorian calendar is the legal
--  calendar in most countries, it was adopted progressively between 1582 and
--  1929. Display before these dates is likely to be historically incorrect.
--
--  # Shortcuts and Gestures
--
--  `GtkCalendar` supports the following gestures:
--
--  - Scrolling up or down will switch to the previous or next month. - Date
--  strings can be dropped for setting the current day.
--
--  # CSS nodes
--
--  ``` calendar.view ├── header │ ├── button │ ├── stack.month │ ├── button │
--  ├── button │ ├── label.year │ ╰── button ╰── grid ╰──
--  label[.day-name][.week-number][.day-number][.other-month][.today] ```
--
--  `GtkCalendar` has a main node with name calendar. It contains a subnode
--  called header containing the widgets for switching between years and
--  months.
--
--  The grid subnode contains all day labels, including week numbers on the
--  left (marked with the .week-number css class) and day names on top (marked
--  with the .day-name css class).
--
--  Day labels that belong to the previous or next month get the .other-month
--  style class. The label of the current day get the .today style class.
--
--  Marked day labels get the :selected state assigned.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Date_Time;        use Glib.Date_Time;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Calendar is

   type Gtk_Calendar_Record is new Gtk_Widget_Record with null record;
   type Gtk_Calendar is access all Gtk_Calendar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Calendar);
   procedure Initialize (Self : not null access Gtk_Calendar_Record'Class);
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

   procedure Clear_Marks (Self : not null access Gtk_Calendar_Record);
   --  Remove all visual markers.

   function Get_Date
      (Self : not null access Gtk_Calendar_Record)
       return Glib.Date_Time.Gdate_Time;
   --  Returns a `GDateTime` representing the shown year, month and the
   --  selected day.
   --  The returned date is in the local time zone.
   --  @return the `GDateTime` representing the selected date. Has
   --  transfer-ownership='full'.

   procedure Set_Date
      (Self : not null access Gtk_Calendar_Record;
       Date : Glib.Date_Time.Gdate_Time);
   --  Switches to Date's year and month and selects its day.
   --  Since: gtk+ 4.20
   --  @param Date a `GDateTime` representing the day to select

   function Get_Day
      (Self : not null access Gtk_Calendar_Record) return Glib.Gint;
   --  Gets the day of the selected date.
   --  Since: gtk+ 4.14
   --  @return the day of the selected date.

   procedure Set_Day
      (Self : not null access Gtk_Calendar_Record;
       Day  : Glib.Gint);
   --  Sets the day for the selected date.
   --  The new date must be valid. For example, setting the day to 31 when the
   --  month is February will fail.
   --  Since: gtk+ 4.14
   --  @param Day The desired day for the selected date (as a number between 1
   --  and 31).

   function Get_Day_Is_Marked
      (Self : not null access Gtk_Calendar_Record;
       Day  : Guint) return Boolean;
   --  Returns if the Day of the Calendar is already marked.
   --  @param Day the day number between 1 and 31.
   --  @return whether the day is marked.

   function Get_Month
      (Self : not null access Gtk_Calendar_Record) return Glib.Gint;
   --  Gets the month of the selected date.
   --  Since: gtk+ 4.14
   --  @return The month of the selected date (as a number between 0 and 11).

   procedure Set_Month
      (Self  : not null access Gtk_Calendar_Record;
       Month : Glib.Gint);
   --  Sets the month for the selected date.
   --  The new date must be valid. For example, setting the month to 1
   --  (February) when the day is 31 will fail.
   --  Since: gtk+ 4.14
   --  @param Month The desired month for the selected date (as a number
   --  between 0 and 11).

   function Get_Show_Day_Names
      (Self : not null access Gtk_Calendar_Record) return Boolean;
   --  Returns whether Self is currently showing the names of the week days.
   --  This is the value of the [propertyGtk.Calendar:show-day-names]
   --  property.
   --  @return Whether the calendar shows day names.

   procedure Set_Show_Day_Names
      (Self  : not null access Gtk_Calendar_Record;
       Value : Boolean);
   --  Sets whether the calendar shows day names.
   --  @param Value Whether to show day names above the day numbers

   function Get_Show_Heading
      (Self : not null access Gtk_Calendar_Record) return Boolean;
   --  Returns whether Self is currently showing the heading.
   --  This is the value of the [propertyGtk.Calendar:show-heading] property.
   --  @return Whether the calendar is showing a heading.

   procedure Set_Show_Heading
      (Self  : not null access Gtk_Calendar_Record;
       Value : Boolean);
   --  Sets whether the calendar should show a heading.
   --  The heading contains the current year and month as well as buttons for
   --  changing both.
   --  @param Value Whether to show the heading in the calendar

   function Get_Show_Week_Numbers
      (Self : not null access Gtk_Calendar_Record) return Boolean;
   --  Returns whether Self is showing week numbers right now.
   --  This is the value of the [propertyGtk.Calendar:show-week-numbers]
   --  property.
   --  @return Whether the calendar is showing week numbers.

   procedure Set_Show_Week_Numbers
      (Self  : not null access Gtk_Calendar_Record;
       Value : Boolean);
   --  Sets whether week numbers are shown in the calendar.
   --  @param Value whether to show week numbers alongside the days

   function Get_Year
      (Self : not null access Gtk_Calendar_Record) return Glib.Gint;
   --  Gets the year of the selected date.
   --  Since: gtk+ 4.14
   --  @return the year of the selected date.

   procedure Set_Year
      (Self : not null access Gtk_Calendar_Record;
       Year : Glib.Gint);
   --  Sets the year for the selected date.
   --  The new date must be valid. For example, setting the year to 2023 when
   --  the date is February 29 will fail.
   --  Since: gtk+ 4.14
   --  @param Year The desired year for the selected date (within
   --  [structGlib.DateTime] limits, i.e. from 0001 to 9999).

   procedure Mark_Day
      (Self : not null access Gtk_Calendar_Record;
       Day  : Guint);
   --  Places a visual marker on a particular day of the current month.
   --  @param Day the day number to mark between 1 and 31.

   procedure Unmark_Day
      (Self : not null access Gtk_Calendar_Record;
       Day  : Guint);
   --  Removes the visual marker from a particular day.
   --  @param Day the day number to unmark between 1 and 31.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Calendar_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Calendar_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Calendar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Calendar_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Calendar_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Calendar_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Calendar_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Calendar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Calendar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Calendar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Calendar_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Calendar_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Calendar_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Calendar_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Calendar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Date_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GLib.Date_Time
   --  The selected date.
   --
   --  This property gets initially set to the current date.

   Day_Property : constant Glib.Properties.Property_Int;
   --  The selected day (as a number between 1 and 31).

   Month_Property : constant Glib.Properties.Property_Int;
   --  The selected month (as a number between 0 and 11).
   --
   --  This property gets initially set to the current month.

   Show_Day_Names_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether day names are displayed.

   Show_Heading_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether a heading is displayed.

   Show_Week_Numbers_Property : constant Glib.Properties.Property_Boolean;
   --  Determines whether week numbers are displayed.

   Year_Property : constant Glib.Properties.Property_Int;
   --  The selected year.
   --
   --  This property gets initially set to the current year.

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
   --  Emitted when the user switches to the next month.

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
   --  Emitted when user switches to the next year.

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
   --  Emitted when the user switches to the previous month.

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
   --  Emitted when user switches to the previous year.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Calendar_Record, Gtk_Calendar);
   function "+"
     (Widget : access Gtk_Calendar_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Calendar
   renames Implements_Gtk_Accessible.To_Object;

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

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Calendar_Record, Gtk_Calendar);
   function "+"
     (Widget : access Gtk_Calendar_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Calendar
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Year_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("year");
   Show_Week_Numbers_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-week-numbers");
   Show_Heading_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-heading");
   Show_Day_Names_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-day-names");
   Month_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("month");
   Day_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("day");
   Date_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("date");
end Gtk.Calendar;
