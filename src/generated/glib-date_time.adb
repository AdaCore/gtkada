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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

package body Glib.Date_Time is

   function From_Object_Free
     (B : access Gdate_Time'Class) return Gdate_Time
   is
      Result : constant Gdate_Time := Gdate_Time (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gdate_Time is
      S : Gdate_Time;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -----------
   -- G_New --
   -----------

   procedure G_New
      (Self    : out Gdate_Time;
       Tz      : Glib.Time_Zone.Gtime_Zone;
       Year    : Glib.Gint;
       Month   : Glib.Gint;
       Day     : Glib.Gint;
       Hour    : Glib.Gint;
       Minute  : Glib.Gint;
       Seconds : Gdouble)
   is
      function Internal
         (Tz      : System.Address;
          Year    : Glib.Gint;
          Month   : Glib.Gint;
          Day     : Glib.Gint;
          Hour    : Glib.Gint;
          Minute  : Glib.Gint;
          Seconds : Gdouble) return System.Address;
      pragma Import (C, Internal, "g_date_time_new");
   begin
      Self.Set_Object (Internal (Get_Object (Tz), Year, Month, Day, Hour, Minute, Seconds));
   end G_New;

   ------------------------
   -- G_New_From_Iso8601 --
   ------------------------

   procedure G_New_From_Iso8601
      (Self       : out Gdate_Time;
       Text       : UTF8_String;
       Default_Tz : Glib.Time_Zone.Gtime_Zone)
   is
      function Internal
         (Text       : Gtkada.Types.Chars_Ptr;
          Default_Tz : System.Address) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_from_iso8601");
      Tmp_Text   : Gtkada.Types.Chars_Ptr := New_String (Text);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Text, Get_Object (Default_Tz));
      Self.Set_Object (Tmp_Return);
      Free (Tmp_Text);
   end G_New_From_Iso8601;

   ---------------------------
   -- G_New_From_Unix_Local --
   ---------------------------

   procedure G_New_From_Unix_Local (Self : out Gdate_Time; T : Gint64) is
      function Internal (T : Gint64) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_from_unix_local");
   begin
      Self.Set_Object (Internal (T));
   end G_New_From_Unix_Local;

   --------------------------------
   -- G_New_From_Unix_Local_Usec --
   --------------------------------

   procedure G_New_From_Unix_Local_Usec
      (Self  : out Gdate_Time;
       Usecs : Gint64)
   is
      function Internal (Usecs : Gint64) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_from_unix_local_usec");
   begin
      Self.Set_Object (Internal (Usecs));
   end G_New_From_Unix_Local_Usec;

   -------------------------
   -- G_New_From_Unix_Utc --
   -------------------------

   procedure G_New_From_Unix_Utc (Self : out Gdate_Time; T : Gint64) is
      function Internal (T : Gint64) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_from_unix_utc");
   begin
      Self.Set_Object (Internal (T));
   end G_New_From_Unix_Utc;

   ------------------------------
   -- G_New_From_Unix_Utc_Usec --
   ------------------------------

   procedure G_New_From_Unix_Utc_Usec
      (Self  : out Gdate_Time;
       Usecs : Gint64)
   is
      function Internal (Usecs : Gint64) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_from_unix_utc_usec");
   begin
      Self.Set_Object (Internal (Usecs));
   end G_New_From_Unix_Utc_Usec;

   -----------------
   -- G_New_Local --
   -----------------

   procedure G_New_Local
      (Self    : out Gdate_Time;
       Year    : Glib.Gint;
       Month   : Glib.Gint;
       Day     : Glib.Gint;
       Hour    : Glib.Gint;
       Minute  : Glib.Gint;
       Seconds : Gdouble)
   is
      function Internal
         (Year    : Glib.Gint;
          Month   : Glib.Gint;
          Day     : Glib.Gint;
          Hour    : Glib.Gint;
          Minute  : Glib.Gint;
          Seconds : Gdouble) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_local");
   begin
      Self.Set_Object (Internal (Year, Month, Day, Hour, Minute, Seconds));
   end G_New_Local;

   ---------------
   -- G_New_Now --
   ---------------

   procedure G_New_Now
      (Self : out Gdate_Time;
       Tz   : Glib.Time_Zone.Gtime_Zone)
   is
      function Internal (Tz : System.Address) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_now");
   begin
      Self.Set_Object (Internal (Get_Object (Tz)));
   end G_New_Now;

   ---------------------
   -- G_New_Now_Local --
   ---------------------

   procedure G_New_Now_Local (Self : out Gdate_Time) is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_date_time_new_now_local");
   begin
      Self.Set_Object (Internal);
   end G_New_Now_Local;

   -------------------
   -- G_New_Now_Utc --
   -------------------

   procedure G_New_Now_Utc (Self : out Gdate_Time) is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_date_time_new_now_utc");
   begin
      Self.Set_Object (Internal);
   end G_New_Now_Utc;

   ---------------
   -- G_New_Utc --
   ---------------

   procedure G_New_Utc
      (Self    : out Gdate_Time;
       Year    : Glib.Gint;
       Month   : Glib.Gint;
       Day     : Glib.Gint;
       Hour    : Glib.Gint;
       Minute  : Glib.Gint;
       Seconds : Gdouble)
   is
      function Internal
         (Year    : Glib.Gint;
          Month   : Glib.Gint;
          Day     : Glib.Gint;
          Hour    : Glib.Gint;
          Minute  : Glib.Gint;
          Seconds : Gdouble) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_utc");
   begin
      Self.Set_Object (Internal (Year, Month, Day, Hour, Minute, Seconds));
   end G_New_Utc;

   --------------------
   -- Gdate_Time_New --
   --------------------

   function Gdate_Time_New
      (Tz      : Glib.Time_Zone.Gtime_Zone;
       Year    : Glib.Gint;
       Month   : Glib.Gint;
       Day     : Glib.Gint;
       Hour    : Glib.Gint;
       Minute  : Glib.Gint;
       Seconds : Gdouble) return Gdate_Time
   is
      function Internal
         (Tz      : System.Address;
          Year    : Glib.Gint;
          Month   : Glib.Gint;
          Day     : Glib.Gint;
          Hour    : Glib.Gint;
          Minute  : Glib.Gint;
          Seconds : Gdouble) return System.Address;
      pragma Import (C, Internal, "g_date_time_new");
      Self : Gdate_Time;
   begin
      Self.Set_Object (Internal (Get_Object (Tz), Year, Month, Day, Hour, Minute, Seconds));
      return Self;
   end Gdate_Time_New;

   ---------------------------------
   -- Gdate_Time_New_From_Iso8601 --
   ---------------------------------

   function Gdate_Time_New_From_Iso8601
      (Text       : UTF8_String;
       Default_Tz : Glib.Time_Zone.Gtime_Zone) return Gdate_Time
   is
      function Internal
         (Text       : Gtkada.Types.Chars_Ptr;
          Default_Tz : System.Address) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_from_iso8601");
      Self       : Gdate_Time;
      Tmp_Text   : Gtkada.Types.Chars_Ptr := New_String (Text);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Text, Get_Object (Default_Tz));
      Self.Set_Object (Tmp_Return);
      Free (Tmp_Text);
      return Self;
   end Gdate_Time_New_From_Iso8601;

   ------------------------------------
   -- Gdate_Time_New_From_Unix_Local --
   ------------------------------------

   function Gdate_Time_New_From_Unix_Local (T : Gint64) return Gdate_Time is
      function Internal (T : Gint64) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_from_unix_local");
      Self : Gdate_Time;
   begin
      Self.Set_Object (Internal (T));
      return Self;
   end Gdate_Time_New_From_Unix_Local;

   -----------------------------------------
   -- Gdate_Time_New_From_Unix_Local_Usec --
   -----------------------------------------

   function Gdate_Time_New_From_Unix_Local_Usec
      (Usecs : Gint64) return Gdate_Time
   is
      function Internal (Usecs : Gint64) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_from_unix_local_usec");
      Self : Gdate_Time;
   begin
      Self.Set_Object (Internal (Usecs));
      return Self;
   end Gdate_Time_New_From_Unix_Local_Usec;

   ----------------------------------
   -- Gdate_Time_New_From_Unix_Utc --
   ----------------------------------

   function Gdate_Time_New_From_Unix_Utc (T : Gint64) return Gdate_Time is
      function Internal (T : Gint64) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_from_unix_utc");
      Self : Gdate_Time;
   begin
      Self.Set_Object (Internal (T));
      return Self;
   end Gdate_Time_New_From_Unix_Utc;

   ---------------------------------------
   -- Gdate_Time_New_From_Unix_Utc_Usec --
   ---------------------------------------

   function Gdate_Time_New_From_Unix_Utc_Usec
      (Usecs : Gint64) return Gdate_Time
   is
      function Internal (Usecs : Gint64) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_from_unix_utc_usec");
      Self : Gdate_Time;
   begin
      Self.Set_Object (Internal (Usecs));
      return Self;
   end Gdate_Time_New_From_Unix_Utc_Usec;

   --------------------------
   -- Gdate_Time_New_Local --
   --------------------------

   function Gdate_Time_New_Local
      (Year    : Glib.Gint;
       Month   : Glib.Gint;
       Day     : Glib.Gint;
       Hour    : Glib.Gint;
       Minute  : Glib.Gint;
       Seconds : Gdouble) return Gdate_Time
   is
      function Internal
         (Year    : Glib.Gint;
          Month   : Glib.Gint;
          Day     : Glib.Gint;
          Hour    : Glib.Gint;
          Minute  : Glib.Gint;
          Seconds : Gdouble) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_local");
      Self : Gdate_Time;
   begin
      Self.Set_Object (Internal (Year, Month, Day, Hour, Minute, Seconds));
      return Self;
   end Gdate_Time_New_Local;

   ------------------------
   -- Gdate_Time_New_Now --
   ------------------------

   function Gdate_Time_New_Now
      (Tz : Glib.Time_Zone.Gtime_Zone) return Gdate_Time
   is
      function Internal (Tz : System.Address) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_now");
      Self : Gdate_Time;
   begin
      Self.Set_Object (Internal (Get_Object (Tz)));
      return Self;
   end Gdate_Time_New_Now;

   ------------------------------
   -- Gdate_Time_New_Now_Local --
   ------------------------------

   function Gdate_Time_New_Now_Local return Gdate_Time is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_date_time_new_now_local");
      Self : Gdate_Time;
   begin
      Self.Set_Object (Internal);
      return Self;
   end Gdate_Time_New_Now_Local;

   ----------------------------
   -- Gdate_Time_New_Now_Utc --
   ----------------------------

   function Gdate_Time_New_Now_Utc return Gdate_Time is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_date_time_new_now_utc");
      Self : Gdate_Time;
   begin
      Self.Set_Object (Internal);
      return Self;
   end Gdate_Time_New_Now_Utc;

   ------------------------
   -- Gdate_Time_New_Utc --
   ------------------------

   function Gdate_Time_New_Utc
      (Year    : Glib.Gint;
       Month   : Glib.Gint;
       Day     : Glib.Gint;
       Hour    : Glib.Gint;
       Minute  : Glib.Gint;
       Seconds : Gdouble) return Gdate_Time
   is
      function Internal
         (Year    : Glib.Gint;
          Month   : Glib.Gint;
          Day     : Glib.Gint;
          Hour    : Glib.Gint;
          Minute  : Glib.Gint;
          Seconds : Gdouble) return System.Address;
      pragma Import (C, Internal, "g_date_time_new_utc");
      Self : Gdate_Time;
   begin
      Self.Set_Object (Internal (Year, Month, Day, Hour, Minute, Seconds));
      return Self;
   end Gdate_Time_New_Utc;

   ---------
   -- Add --
   ---------

   function Add
      (Self     : Gdate_Time;
       Timespan : Glib.Gint64) return Gdate_Time
   is
      function Internal
         (Self     : System.Address;
          Timespan : Glib.Gint64) return System.Address;
      pragma Import (C, Internal, "g_date_time_add");
   begin
      return From_Object (Internal (Get_Object (Self), Timespan));
   end Add;

   --------------
   -- Add_Days --
   --------------

   function Add_Days (Self : Gdate_Time; Days : Glib.Gint) return Gdate_Time is
      function Internal
         (Self : System.Address;
          Days : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "g_date_time_add_days");
   begin
      return From_Object (Internal (Get_Object (Self), Days));
   end Add_Days;

   --------------
   -- Add_Full --
   --------------

   function Add_Full
      (Self    : Gdate_Time;
       Years   : Glib.Gint;
       Months  : Glib.Gint;
       Days    : Glib.Gint;
       Hours   : Glib.Gint;
       Minutes : Glib.Gint;
       Seconds : Gdouble) return Gdate_Time
   is
      function Internal
         (Self    : System.Address;
          Years   : Glib.Gint;
          Months  : Glib.Gint;
          Days    : Glib.Gint;
          Hours   : Glib.Gint;
          Minutes : Glib.Gint;
          Seconds : Gdouble) return System.Address;
      pragma Import (C, Internal, "g_date_time_add_full");
   begin
      return From_Object (Internal (Get_Object (Self), Years, Months, Days, Hours, Minutes, Seconds));
   end Add_Full;

   ---------------
   -- Add_Hours --
   ---------------

   function Add_Hours
      (Self  : Gdate_Time;
       Hours : Glib.Gint) return Gdate_Time
   is
      function Internal
         (Self  : System.Address;
          Hours : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "g_date_time_add_hours");
   begin
      return From_Object (Internal (Get_Object (Self), Hours));
   end Add_Hours;

   -----------------
   -- Add_Minutes --
   -----------------

   function Add_Minutes
      (Self    : Gdate_Time;
       Minutes : Glib.Gint) return Gdate_Time
   is
      function Internal
         (Self    : System.Address;
          Minutes : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "g_date_time_add_minutes");
   begin
      return From_Object (Internal (Get_Object (Self), Minutes));
   end Add_Minutes;

   ----------------
   -- Add_Months --
   ----------------

   function Add_Months
      (Self   : Gdate_Time;
       Months : Glib.Gint) return Gdate_Time
   is
      function Internal
         (Self   : System.Address;
          Months : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "g_date_time_add_months");
   begin
      return From_Object (Internal (Get_Object (Self), Months));
   end Add_Months;

   -----------------
   -- Add_Seconds --
   -----------------

   function Add_Seconds
      (Self    : Gdate_Time;
       Seconds : Gdouble) return Gdate_Time
   is
      function Internal
         (Self    : System.Address;
          Seconds : Gdouble) return System.Address;
      pragma Import (C, Internal, "g_date_time_add_seconds");
   begin
      return From_Object (Internal (Get_Object (Self), Seconds));
   end Add_Seconds;

   ---------------
   -- Add_Weeks --
   ---------------

   function Add_Weeks
      (Self  : Gdate_Time;
       Weeks : Glib.Gint) return Gdate_Time
   is
      function Internal
         (Self  : System.Address;
          Weeks : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "g_date_time_add_weeks");
   begin
      return From_Object (Internal (Get_Object (Self), Weeks));
   end Add_Weeks;

   ---------------
   -- Add_Years --
   ---------------

   function Add_Years
      (Self  : Gdate_Time;
       Years : Glib.Gint) return Gdate_Time
   is
      function Internal
         (Self  : System.Address;
          Years : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "g_date_time_add_years");
   begin
      return From_Object (Internal (Get_Object (Self), Years));
   end Add_Years;

   -------------
   -- Compare --
   -------------

   function Compare (Self : Gdate_Time; Dt2 : Gdate_Time) return Glib.Gint is
      function Internal
         (Self : System.Address;
          Dt2  : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_date_time_compare");
   begin
      return Internal (Get_Object (Self), Get_Object (Dt2));
   end Compare;

   ----------------
   -- Difference --
   ----------------

   function Difference
      (Self  : Gdate_Time;
       Start : Gdate_Time) return Glib.Gint64
   is
      function Internal
         (Self  : System.Address;
          Start : System.Address) return Glib.Gint64;
      pragma Import (C, Internal, "g_date_time_difference");
   begin
      return Internal (Get_Object (Self), Get_Object (Start));
   end Difference;

   -----------
   -- Equal --
   -----------

   function Equal (Self : Gdate_Time; Dt2 : Gdate_Time) return Boolean is
      function Internal
         (Self : System.Address;
          Dt2  : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_date_time_equal");
   begin
      return Internal (Get_Object (Self), Get_Object (Dt2)) /= 0;
   end Equal;

   ------------
   -- Format --
   ------------

   function Format
      (Self   : Gdate_Time;
       Format : UTF8_String) return UTF8_String
   is
      function Internal
         (Self   : System.Address;
          Format : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_date_time_format");
      Tmp_Format : Gtkada.Types.Chars_Ptr := New_String (Format);
      Tmp_Return : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Format);
      Free (Tmp_Format);
      return Gtkada.Bindings.Value_And_Free (Tmp_Return);
   end Format;

   --------------------
   -- Format_Iso8601 --
   --------------------

   function Format_Iso8601 (Self : Gdate_Time) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_date_time_format_iso8601");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Format_Iso8601;

   ----------------------
   -- Get_Day_Of_Month --
   ----------------------

   function Get_Day_Of_Month (Self : Gdate_Time) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_date_time_get_day_of_month");
   begin
      return Internal (Get_Object (Self));
   end Get_Day_Of_Month;

   ---------------------
   -- Get_Day_Of_Week --
   ---------------------

   function Get_Day_Of_Week (Self : Gdate_Time) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_date_time_get_day_of_week");
   begin
      return Internal (Get_Object (Self));
   end Get_Day_Of_Week;

   ---------------------
   -- Get_Day_Of_Year --
   ---------------------

   function Get_Day_Of_Year (Self : Gdate_Time) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_date_time_get_day_of_year");
   begin
      return Internal (Get_Object (Self));
   end Get_Day_Of_Year;

   --------------
   -- Get_Hour --
   --------------

   function Get_Hour (Self : Gdate_Time) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_date_time_get_hour");
   begin
      return Internal (Get_Object (Self));
   end Get_Hour;

   ---------------------
   -- Get_Microsecond --
   ---------------------

   function Get_Microsecond (Self : Gdate_Time) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_date_time_get_microsecond");
   begin
      return Internal (Get_Object (Self));
   end Get_Microsecond;

   ----------------
   -- Get_Minute --
   ----------------

   function Get_Minute (Self : Gdate_Time) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_date_time_get_minute");
   begin
      return Internal (Get_Object (Self));
   end Get_Minute;

   ---------------
   -- Get_Month --
   ---------------

   function Get_Month (Self : Gdate_Time) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_date_time_get_month");
   begin
      return Internal (Get_Object (Self));
   end Get_Month;

   ----------------
   -- Get_Second --
   ----------------

   function Get_Second (Self : Gdate_Time) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_date_time_get_second");
   begin
      return Internal (Get_Object (Self));
   end Get_Second;

   -----------------
   -- Get_Seconds --
   -----------------

   function Get_Seconds (Self : Gdate_Time) return Gdouble is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "g_date_time_get_seconds");
   begin
      return Internal (Get_Object (Self));
   end Get_Seconds;

   ------------------
   -- Get_Timezone --
   ------------------

   function Get_Timezone
      (Self : Gdate_Time) return Glib.Time_Zone.Gtime_Zone
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_date_time_get_timezone");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Timezone;

   -------------------------------
   -- Get_Timezone_Abbreviation --
   -------------------------------

   function Get_Timezone_Abbreviation (Self : Gdate_Time) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_date_time_get_timezone_abbreviation");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Timezone_Abbreviation;

   --------------------
   -- Get_Utc_Offset --
   --------------------

   function Get_Utc_Offset (Self : Gdate_Time) return Glib.Gint64 is
      function Internal (Self : System.Address) return Glib.Gint64;
      pragma Import (C, Internal, "g_date_time_get_utc_offset");
   begin
      return Internal (Get_Object (Self));
   end Get_Utc_Offset;

   -----------------------------
   -- Get_Week_Numbering_Year --
   -----------------------------

   function Get_Week_Numbering_Year (Self : Gdate_Time) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_date_time_get_week_numbering_year");
   begin
      return Internal (Get_Object (Self));
   end Get_Week_Numbering_Year;

   ----------------------
   -- Get_Week_Of_Year --
   ----------------------

   function Get_Week_Of_Year (Self : Gdate_Time) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_date_time_get_week_of_year");
   begin
      return Internal (Get_Object (Self));
   end Get_Week_Of_Year;

   --------------
   -- Get_Year --
   --------------

   function Get_Year (Self : Gdate_Time) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "g_date_time_get_year");
   begin
      return Internal (Get_Object (Self));
   end Get_Year;

   -------------
   -- Get_Ymd --
   -------------

   procedure Get_Ymd
      (Self  : Gdate_Time;
       Year  : out Glib.Gint;
       Month : out Glib.Gint;
       Day   : out Glib.Gint)
   is
      procedure Internal
         (Self  : System.Address;
          Year  : out Glib.Gint;
          Month : out Glib.Gint;
          Day   : out Glib.Gint);
      pragma Import (C, Internal, "g_date_time_get_ymd");
   begin
      Internal (Get_Object (Self), Year, Month, Day);
   end Get_Ymd;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Gdate_Time) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "g_date_time_hash");
   begin
      return Internal (Get_Object (Self));
   end Hash;

   -------------------------
   -- Is_Daylight_Savings --
   -------------------------

   function Is_Daylight_Savings (Self : Gdate_Time) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_date_time_is_daylight_savings");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Daylight_Savings;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gdate_Time) return Gdate_Time is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_date_time_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   --------------
   -- To_Local --
   --------------

   function To_Local (Self : Gdate_Time) return Gdate_Time is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_date_time_to_local");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end To_Local;

   -----------------
   -- To_Timezone --
   -----------------

   function To_Timezone
      (Self : Gdate_Time;
       Tz   : Glib.Time_Zone.Gtime_Zone) return Gdate_Time
   is
      function Internal
         (Self : System.Address;
          Tz   : System.Address) return System.Address;
      pragma Import (C, Internal, "g_date_time_to_timezone");
   begin
      return From_Object (Internal (Get_Object (Self), Get_Object (Tz)));
   end To_Timezone;

   -------------
   -- To_Unix --
   -------------

   function To_Unix (Self : Gdate_Time) return Gint64 is
      function Internal (Self : System.Address) return Gint64;
      pragma Import (C, Internal, "g_date_time_to_unix");
   begin
      return Internal (Get_Object (Self));
   end To_Unix;

   ------------------
   -- To_Unix_Usec --
   ------------------

   function To_Unix_Usec (Self : Gdate_Time) return Gint64 is
      function Internal (Self : System.Address) return Gint64;
      pragma Import (C, Internal, "g_date_time_to_unix_usec");
   begin
      return Internal (Get_Object (Self));
   end To_Unix_Usec;

   ------------
   -- To_Utc --
   ------------

   function To_Utc (Self : Gdate_Time) return Gdate_Time is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_date_time_to_utc");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end To_Utc;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gdate_Time) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_date_time_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Glib.Date_Time;
