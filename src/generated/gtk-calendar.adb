------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
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

pragma Ada_05;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Object;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Calendar is

   function To_Gtk_Calendar_Detail_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Calendar_Detail_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Calendar_Detail_Func, System.Address);

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
   --  not set.
   --  The size of the details area can be restricted by setting the
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

   package Type_Conversion_Gtk_Calendar is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Calendar_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Calendar);

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

   procedure Initialize
      (Calendar : not null access Gtk_Calendar_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_calendar_new");
   begin
      Set_Object (Calendar, Internal);
   end Initialize;

   -----------------
   -- Clear_Marks --
   -----------------

   procedure Clear_Marks (Calendar : not null access Gtk_Calendar_Record) is
      procedure Internal (Calendar : System.Address);
      pragma Import (C, Internal, "gtk_calendar_clear_marks");
   begin
      Internal (Get_Object (Calendar));
   end Clear_Marks;

   --------------
   -- Get_Date --
   --------------

   procedure Get_Date
      (Calendar : not null access Gtk_Calendar_Record;
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

   -----------------------
   -- Get_Day_Is_Marked --
   -----------------------

   function Get_Day_Is_Marked
      (Calendar : not null access Gtk_Calendar_Record;
       Day      : Guint) return Boolean
   is
      function Internal
         (Calendar : System.Address;
          Day      : Guint) return Integer;
      pragma Import (C, Internal, "gtk_calendar_get_day_is_marked");
   begin
      return Boolean'Val (Internal (Get_Object (Calendar), Day));
   end Get_Day_Is_Marked;

   ----------------------------
   -- Get_Detail_Height_Rows --
   ----------------------------

   function Get_Detail_Height_Rows
      (Calendar : not null access Gtk_Calendar_Record) return Gint
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
      (Calendar : not null access Gtk_Calendar_Record) return Gint
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
      (Calendar : not null access Gtk_Calendar_Record)
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

   procedure Mark_Day
      (Calendar : not null access Gtk_Calendar_Record;
       Day      : Guint)
   is
      procedure Internal (Calendar : System.Address; Day : Guint);
      pragma Import (C, Internal, "gtk_calendar_mark_day");
   begin
      Internal (Get_Object (Calendar), Day);
   end Mark_Day;

   ----------------
   -- Select_Day --
   ----------------

   procedure Select_Day
      (Calendar : not null access Gtk_Calendar_Record;
       Day      : Guint)
   is
      procedure Internal (Calendar : System.Address; Day : Guint);
      pragma Import (C, Internal, "gtk_calendar_select_day");
   begin
      Internal (Get_Object (Calendar), Day);
   end Select_Day;

   ------------------
   -- Select_Month --
   ------------------

   procedure Select_Month
      (Calendar : not null access Gtk_Calendar_Record;
       Month    : Guint;
       Year     : Guint)
   is
      procedure Internal
         (Calendar : System.Address;
          Month    : Guint;
          Year     : Guint);
      pragma Import (C, Internal, "gtk_calendar_select_month");
   begin
      Internal (Get_Object (Calendar), Month, Year);
   end Select_Month;

   ---------------------
   -- Set_Detail_Func --
   ---------------------

   procedure Set_Detail_Func
      (Calendar : not null access Gtk_Calendar_Record;
       Func     : Gtk_Calendar_Detail_Func)
   is
   begin
      if Func = null then
         C_Gtk_Calendar_Set_Detail_Func (Get_Object (Calendar), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Calendar_Set_Detail_Func (Get_Object (Calendar), Internal_Gtk_Calendar_Detail_Func'Address, To_Address (Func), System.Null_Address);
      end if;
   end Set_Detail_Func;

   package body Set_Detail_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Calendar_Detail_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Calendar_Detail_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Calendar_Detail_Func, System.Address);

      function Internal_Cb
         (Calendar  : System.Address;
          Year      : Guint;
          Month     : Guint;
          Day       : Guint;
          User_Data : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Convention (C, Internal_Cb);
      --  This kind of functions provide Pango markup with detail information
      --  for the specified day. Examples for such details are holidays or
      --  appointments. The function returns null when no information is
      --  available.
      --  for the specified day, or null.
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
         (Calendar  : System.Address;
          Year      : Guint;
          Month     : Guint;
          Day       : Guint;
          User_Data : System.Address) return Interfaces.C.Strings.chars_ptr
      is
         D                 : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_Calendar : Gtk.Calendar.Gtk_Calendar_Record;
      begin
         return New_String (To_Gtk_Calendar_Detail_Func (D.Func) (Gtk.Calendar.Gtk_Calendar (Get_User_Data (Calendar, Stub_Gtk_Calendar)), Year, Month, Day, D.Data.all));
      end Internal_Cb;

      ---------------------
      -- Set_Detail_Func --
      ---------------------

      procedure Set_Detail_Func
         (Calendar : not null access Gtk.Calendar.Gtk_Calendar_Record'Class;
          Func     : Gtk_Calendar_Detail_Func;
          Data     : User_Data_Type)
      is
      begin
         if Func = null then
            C_Gtk_Calendar_Set_Detail_Func (Get_Object (Calendar), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            C_Gtk_Calendar_Set_Detail_Func (Get_Object (Calendar), Internal_Cb'Address, Users.Build (To_Address (Func), Data), Users.Free_Data'Address);
         end if;
      end Set_Detail_Func;

   end Set_Detail_Func_User_Data;

   ----------------------------
   -- Set_Detail_Height_Rows --
   ----------------------------

   procedure Set_Detail_Height_Rows
      (Calendar : not null access Gtk_Calendar_Record;
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
      (Calendar : not null access Gtk_Calendar_Record;
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
      (Calendar : not null access Gtk_Calendar_Record;
       Flags    : Gtk_Calendar_Display_Options)
   is
      procedure Internal
         (Calendar : System.Address;
          Flags    : Gtk_Calendar_Display_Options);
      pragma Import (C, Internal, "gtk_calendar_set_display_options");
   begin
      Internal (Get_Object (Calendar), Flags);
   end Set_Display_Options;

   ----------------
   -- Unmark_Day --
   ----------------

   procedure Unmark_Day
      (Calendar : not null access Gtk_Calendar_Record;
       Day      : Guint)
   is
      procedure Internal (Calendar : System.Address; Day : Guint);
      pragma Import (C, Internal, "gtk_calendar_unmark_day");
   begin
      Internal (Get_Object (Calendar), Day);
   end Unmark_Day;

end Gtk.Calendar;
