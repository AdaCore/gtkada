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

with Gtk.Widget;

package Gtk.Calendar is

   type Gtk_Calendar_Display_Options is private;
   Show_Heading : constant Gtk_Calendar_Display_Options;
   Show_Day_Names : constant Gtk_Calendar_Display_Options;
   No_Month_Change : constant Gtk_Calendar_Display_Options;
   Show_Week_Number : constant Gtk_Calendar_Display_Options;
   Week_Start_Monday : constant Gtk_Calendar_Display_Options;

   function "and" (Left, Right : Gtk_Calendar_Display_Options)
                   return Gtk_Calendar_Display_Options;

   type Gtk_Calendar_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Calendar is access all Gtk_Calendar_Record'Class;

   procedure Clear_Marks (Calendar : access Gtk_Calendar_Record);
   procedure Display_Options
      (Calendar : access Gtk_Calendar_Record;
       Flags    : in Gtk_Calendar_Display_Options);
   procedure Freeze (Calendar : access Gtk_Calendar_Record);
   procedure Get_Date
      (Calendar : access Gtk_Calendar_Record;
       Year     : in out Guint;
       Month    : in out Guint;
       Day      : in out Guint);
   procedure Gtk_New (Widget : out Gtk_Calendar);
   procedure Initialize (Widget : access Gtk_Calendar_Record);
   function Mark_Day
      (Calendar : access Gtk_Calendar_Record;
       Day      : in Guint)
       return        Gint;
   procedure Select_Day
      (Calendar : access Gtk_Calendar_Record;
       Day      : in Guint);
   function Select_Month
      (Calendar : access Gtk_Calendar_Record;
       Month    : in Guint;
       Year     : in Guint)
       return        Gint;
   procedure Thaw (Calendar : access Gtk_Calendar_Record);
   function Unmark_Day
      (Calendar : access Gtk_Calendar_Record;
       Day      : in Guint)
       return        Gint;

   procedure Generate (Calendar : access Gtk_Calendar_Record;
                       N      : in Node_Ptr;
                       File   : in File_Type);

   procedure Generate (Calendar : access Gtk_Calendar_Record;
                       N        : in Node_Ptr);

private
   type Gtk_Calendar_Record is new Gtk.Widget.Gtk_Widget_Record
     with null record;

   type Gtk_Calendar_Display_Options is new Gint;
   Show_Heading : constant Gtk_Calendar_Display_Options := 1;
   Show_Day_Names : constant Gtk_Calendar_Display_Options := 2;
   No_Month_Change : constant Gtk_Calendar_Display_Options := 4;
   Show_Week_Number : constant Gtk_Calendar_Display_Options := 8;
   Week_Start_Monday : constant Gtk_Calendar_Display_Options := 16;

end Gtk.Calendar;
