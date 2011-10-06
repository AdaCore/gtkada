-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2011, AdaCore                   --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Object;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Calendar is

   function To_Gtk_Calendar_Detail_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Calendar_Detail_Func);

   procedure C_Gtk_Calendar_Set_Detail_Func
      (Calendar : System.Address;
       Func     : System.Address;
       Data     : System.Address;
       Destroy  : System.Address);
   pragma Import (C, C_Gtk_Calendar_Set_Detail_Func, "gtk_calendar_set_detail_func");
   --  Installs a function which provides Pango markup with detail information
   --  for each day. Examples for such details are holidays or appointments.
   --  That information is shown below each day when
   --  Gtk.Calendar.Gtk_Calendar:show-details is set. A tooltip containing with
   --  full detail information is provided, if the entire text should not fit
   --  into the details area, or if Gtk.Calendar.Gtk_Calendar:show-details is
   --  not set. The size of the details area can be restricted by setting the
   --  Gtk.Calendar.Gtk_Calendar:detail-width-chars and
   --  Gtk.Calendar.Gtk_Calendar:detail-height-rows properties.
   --  Since: gtk+ 2.14
   --  "func": a function providing details for each day.
   --  "data": data to pass to Func invokations.
   --  "destroy": a function for releasing Data.

   function Internal_Gtk_Calendar_Detail_Func
      (Calendar  : System.Address;
       Year      : Guint;
       Month     : Guint;
       Day       : Guint;
       User_Data : System.Address) return Interfaces.C.Strings.chars_ptr;
   pragma Convention (C, Internal_Gtk_Calendar_Detail_Func);
   --  "calendar": a Gtk.Calendar.Gtk_Calendar.
   --  "year": the year for which details are needed.
   --  "month": the month for which details are needed.
   --  "day": the day of Month for which details are needed.
   --  "user_data": the data passed with Gtk.Calendar.Set_Detail_Func.

   ---------------------------------------
   -- Internal_Gtk_Calendar_Detail_Func --
   ---------------------------------------

   function Internal_Gtk_Calendar_Detail_Func
      (Calendar  : System.Address;
       Year      : Guint;
       Month     : Guint;
       Day       : Guint;
       User_Data : System.Address) return Interfaces.C.Strings.chars_ptr
   is
      Func              : constant Gtk_Calendar_Detail_Func := To_Gtk_Calendar_Detail_Func (User_Data);
      Stub_Gtk_Calendar : Gtk_Calendar_Record;
   begin
      return New_String (Func (Gtk.Calendar.Gtk_Calendar (Get_User_Data (Calendar, Stub_Gtk_Calendar)), Year, Month, Day));
   end Internal_Gtk_Calendar_Detail_Func;

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Calendar_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Calendar : out Gtk_Calendar) is
   begin
      Calendar := new Gtk_Calendar_Record;
      Gtk.Calendar.Initialize (Calendar);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Calendar : access Gtk_Calendar_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_calendar_new");
   begin
      Set_Object (Calendar, Internal);
   end Initialize;

   -----------------
   -- Clear_Marks --
   -----------------

   procedure Clear_Marks (Calendar : access Gtk_Calendar_Record) is
      procedure Internal (Calendar : System.Address);
      pragma Import (C, Internal, "gtk_calendar_clear_marks");
   begin
      Internal (Get_Object (Calendar));
   end Clear_Marks;

   ---------------------
   -- Display_Options --
   ---------------------

   procedure Display_Options
      (Calendar : access Gtk_Calendar_Record;
       Flags    : Gtk_Calendar_Display_Options)
   is
      procedure Internal
         (Calendar : System.Address;
          Flags    : Gtk_Calendar_Display_Options);
      pragma Import (C, Internal, "gtk_calendar_display_options");
   begin
      Internal (Get_Object (Calendar), Flags);
   end Display_Options;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Calendar : access Gtk_Calendar_Record) is
      procedure Internal (Calendar : System.Address);
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

   ----------------------------
   -- Get_Detail_Height_Rows --
   ----------------------------

   function Get_Detail_Height_Rows
      (Calendar : access Gtk_Calendar_Record) return Gint
   is
      function Internal (Calendar : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_calendar_get_detail_height_rows");
   begin
      return Internal (Get_Object (Calendar));
   end Get_Detail_Height_Rows;

   ----------------------------
   -- Get_Detail_Width_Chars --
   ----------------------------

   function Get_Detail_Width_Chars
      (Calendar : access Gtk_Calendar_Record) return Gint
   is
      function Internal (Calendar : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_calendar_get_detail_width_chars");
   begin
      return Internal (Get_Object (Calendar));
   end Get_Detail_Width_Chars;

   -------------------------
   -- Get_Display_Options --
   -------------------------

   function Get_Display_Options
      (Calendar : access Gtk_Calendar_Record)
       return Gtk_Calendar_Display_Options
   is
      function Internal
         (Calendar : System.Address) return Gtk_Calendar_Display_Options;
      pragma Import (C, Internal, "gtk_calendar_get_display_options");
   begin
      return Internal (Get_Object (Calendar));
   end Get_Display_Options;

   --------------
   -- Mark_Day --
   --------------

   function Mark_Day
      (Calendar : access Gtk_Calendar_Record;
       Day      : Guint) return Boolean
   is
      function Internal
         (Calendar : System.Address;
          Day      : Guint) return Integer;
      pragma Import (C, Internal, "gtk_calendar_mark_day");
   begin
      return Boolean'Val (Internal (Get_Object (Calendar), Day));
   end Mark_Day;

   ----------------
   -- Select_Day --
   ----------------

   procedure Select_Day (Calendar : access Gtk_Calendar_Record; Day : Guint) is
      procedure Internal (Calendar : System.Address; Day : Guint);
      pragma Import (C, Internal, "gtk_calendar_select_day");
   begin
      Internal (Get_Object (Calendar), Day);
   end Select_Day;

   ------------------
   -- Select_Month --
   ------------------

   function Select_Month
      (Calendar : access Gtk_Calendar_Record;
       Month    : Guint;
       Year     : Guint) return Boolean
   is
      function Internal
         (Calendar : System.Address;
          Month    : Guint;
          Year     : Guint) return Integer;
      pragma Import (C, Internal, "gtk_calendar_select_month");
   begin
      return Boolean'Val (Internal (Get_Object (Calendar), Month, Year));
   end Select_Month;

   ---------------------
   -- Set_Detail_Func --
   ---------------------

   procedure Set_Detail_Func
      (Calendar : access Gtk_Calendar_Record;
       Func     : Gtk_Calendar_Detail_Func)
   is
   begin
      C_Gtk_Calendar_Set_Detail_Func (Get_Object (Calendar), Internal_Gtk_Calendar_Detail_Func'Address, Func'Address, System.Null_Address);
   end Set_Detail_Func;

   package body Set_Detail_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);
      function To_Gtk_Calendar_Detail_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Calendar_Detail_Func);

      function Internal_Cb
         (Calendar  : access Gtk.Calendar.Gtk_Calendar_Record'Class;
          Year      : Guint;
          Month     : Guint;
          Day       : Guint;
          User_Data : System.Address) return UTF8_String;
      --  This kind of functions provide Pango markup with detail information
      --  for the specified day. Examples for such details are holidays or
      --  appointments. The function returns null when no information is
      --  available. for the specified day, or null.
      --  Since: gtk+ 2.14
      --  "calendar": a Gtk.Calendar.Gtk_Calendar.
      --  "year": the year for which details are needed.
      --  "month": the month for which details are needed.
      --  "day": the day of Month for which details are needed.
      --  "user_data": the data passed with Gtk.Calendar.Set_Detail_Func.

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Calendar  : access Gtk.Calendar.Gtk_Calendar_Record'Class;
          Year      : Guint;
          Month     : Guint;
          Day       : Guint;
          User_Data : System.Address) return UTF8_String
      is
         D : constant Users.Internal_Data_Access := Users.Convert (User_Data);
      begin
         return To_Gtk_Calendar_Detail_Func (D.Func) (Calendar, Year, Month, Day, D.Data.all);
      end Internal_Cb;

      ---------------------
      -- Set_Detail_Func --
      ---------------------

      procedure Set_Detail_Func
         (Calendar : access Gtk.Calendar.Gtk_Calendar_Record'Class;
          Func     : Gtk_Calendar_Detail_Func;
          Data     : User_Data_Type)
      is
      begin
         C_Gtk_Calendar_Set_Detail_Func (Get_Object (Calendar), Internal_Cb'Address, Users.Build (Func'Address, Data), Users.Free_Data'Address);
      end Set_Detail_Func;

   end Set_Detail_Func_User_Data;

   ----------------------------
   -- Set_Detail_Height_Rows --
   ----------------------------

   procedure Set_Detail_Height_Rows
      (Calendar : access Gtk_Calendar_Record;
       Rows     : Gint)
   is
      procedure Internal (Calendar : System.Address; Rows : Gint);
      pragma Import (C, Internal, "gtk_calendar_set_detail_height_rows");
   begin
      Internal (Get_Object (Calendar), Rows);
   end Set_Detail_Height_Rows;

   ----------------------------
   -- Set_Detail_Width_Chars --
   ----------------------------

   procedure Set_Detail_Width_Chars
      (Calendar : access Gtk_Calendar_Record;
       Chars    : Gint)
   is
      procedure Internal (Calendar : System.Address; Chars : Gint);
      pragma Import (C, Internal, "gtk_calendar_set_detail_width_chars");
   begin
      Internal (Get_Object (Calendar), Chars);
   end Set_Detail_Width_Chars;

   -------------------------
   -- Set_Display_Options --
   -------------------------

   procedure Set_Display_Options
      (Calendar : access Gtk_Calendar_Record;
       Flags    : Gtk_Calendar_Display_Options)
   is
      procedure Internal
         (Calendar : System.Address;
          Flags    : Gtk_Calendar_Display_Options);
      pragma Import (C, Internal, "gtk_calendar_set_display_options");
   begin
      Internal (Get_Object (Calendar), Flags);
   end Set_Display_Options;

   ----------
   -- Thaw --
   ----------

   procedure Thaw (Calendar : access Gtk_Calendar_Record) is
      procedure Internal (Calendar : System.Address);
      pragma Import (C, Internal, "gtk_calendar_thaw");
   begin
      Internal (Get_Object (Calendar));
   end Thaw;

   ----------------
   -- Unmark_Day --
   ----------------

   function Unmark_Day
      (Calendar : access Gtk_Calendar_Record;
       Day      : Guint) return Boolean
   is
      function Internal
         (Calendar : System.Address;
          Day      : Guint) return Integer;
      pragma Import (C, Internal, "gtk_calendar_unmark_day");
   begin
      return Boolean'Val (Internal (Get_Object (Calendar), Day));
   end Unmark_Day;

end Gtk.Calendar;
