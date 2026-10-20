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

--  `GDateTime` is a structure that combines a Gregorian date and time into a
--  single structure.
--
--  `GDateTime` provides many conversion and methods to manipulate dates and
--  times. Time precision is provided down to microseconds and the time can
--  range (proleptically) from 0001-01-01 00:00:00 to 9999-12-31
--  23:59:59.999999. `GDateTime` follows POSIX time in the sense that it is
--  oblivious to leap seconds.
--
--  `GDateTime` is an immutable object; once it has been created it cannot be
--  modified further. All modifiers will create a new `GDateTime`. Nearly all
--  such functions can fail due to the date or time going out of range, in
--  which case null will be returned.
--
--  `GDateTime` is reference counted: the reference count is increased by
--  calling [methodGlib.DateTime.ref] and decreased by calling
--  [methodGlib.DateTime.unref]. When the reference count drops to 0, the
--  resources allocated by the `GDateTime` structure are released.
--
--  Many parts of the API may produce non-obvious results. As an example,
--  adding two months to January 31st will yield March 31st whereas adding one
--  month and then one month again will yield either March 28th or March 29th.
--  Also note that adding 24 hours is not always the same as adding one day
--  (since days containing daylight savings time transitions are either 23 or
--  25 hours in length).
--
--  <group>GLib</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib.Time_Zone; use Glib.Time_Zone;

package Glib.Date_Time is

   type Gdate_Time is new Glib.C_Boxed with null record;
   Null_Gdate_Time : constant Gdate_Time;

   function From_Object (Object : System.Address) return Gdate_Time;
   function From_Object_Free (B : access Gdate_Time'Class) return Gdate_Time;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure G_New
      (Self    : out Gdate_Time;
       Tz      : Glib.Time_Zone.Gtime_Zone;
       Year    : Glib.Gint;
       Month   : Glib.Gint;
       Day     : Glib.Gint;
       Hour    : Glib.Gint;
       Minute  : Glib.Gint;
       Seconds : Gdouble);
   --  Creates a new Glib.Date_Time.Gdate_Time corresponding to the given date
   --  and time in the time zone Tz.
   --  The Year must be between 1 and 9999, Month between 1 and 12 and Day
   --  between 1 and 28, 29, 30 or 31 depending on the month and the year.
   --  Hour must be between 0 and 23 and Minute must be between 0 and 59.
   --  Seconds must be at least 0.0 and must be strictly less than 60.0. It
   --  will be rounded down to the nearest microsecond.
   --  If the given time is not representable in the given time zone (for
   --  example, 02:30 on March 14th 2010 in Toronto, due to daylight savings
   --  time) then the time will be rounded up to the nearest existing time (in
   --  this case, 03:00). If this matters to you then you should verify the
   --  return value for containing the same as the numbers you gave.
   --  In the case that the given time is ambiguous in the given time zone
   --  (for example, 01:30 on November 7th 2010 in Toronto, due to daylight
   --  savings time) then the time falling within standard (ie: non-daylight)
   --  time is taken.
   --  It not considered a programmer error for the values to this function to
   --  be out of range, but in the case that they are, the function will return
   --  null.
   --  You should release the return value by calling Glib.Date_Time.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.26
   --  @param Tz a Glib.Time_Zone.Gtime_Zone
   --  @param Year the year component of the date
   --  @param Month the month component of the date
   --  @param Day the day component of the date
   --  @param Hour the hour component of the date
   --  @param Minute the minute component of the date
   --  @param Seconds the number of seconds past the minute

   function Gdate_Time_New
      (Tz      : Glib.Time_Zone.Gtime_Zone;
       Year    : Glib.Gint;
       Month   : Glib.Gint;
       Day     : Glib.Gint;
       Hour    : Glib.Gint;
       Minute  : Glib.Gint;
       Seconds : Gdouble) return Gdate_Time;
   --  Creates a new Glib.Date_Time.Gdate_Time corresponding to the given date
   --  and time in the time zone Tz.
   --  The Year must be between 1 and 9999, Month between 1 and 12 and Day
   --  between 1 and 28, 29, 30 or 31 depending on the month and the year.
   --  Hour must be between 0 and 23 and Minute must be between 0 and 59.
   --  Seconds must be at least 0.0 and must be strictly less than 60.0. It
   --  will be rounded down to the nearest microsecond.
   --  If the given time is not representable in the given time zone (for
   --  example, 02:30 on March 14th 2010 in Toronto, due to daylight savings
   --  time) then the time will be rounded up to the nearest existing time (in
   --  this case, 03:00). If this matters to you then you should verify the
   --  return value for containing the same as the numbers you gave.
   --  In the case that the given time is ambiguous in the given time zone
   --  (for example, 01:30 on November 7th 2010 in Toronto, due to daylight
   --  savings time) then the time falling within standard (ie: non-daylight)
   --  time is taken.
   --  It not considered a programmer error for the values to this function to
   --  be out of range, but in the case that they are, the function will return
   --  null.
   --  You should release the return value by calling Glib.Date_Time.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.26
   --  @param Tz a Glib.Time_Zone.Gtime_Zone
   --  @param Year the year component of the date
   --  @param Month the month component of the date
   --  @param Day the day component of the date
   --  @param Hour the hour component of the date
   --  @param Minute the minute component of the date
   --  @param Seconds the number of seconds past the minute

   procedure G_New_From_Iso8601
      (Self       : out Gdate_Time;
       Text       : UTF8_String;
       Default_Tz : Glib.Time_Zone.Gtime_Zone);
   --  Creates a Glib.Date_Time.Gdate_Time corresponding to the given [ISO
   --  8601 formatted string](https://en.wikipedia.org/wiki/ISO_8601) Text. ISO
   --  8601 strings of the form <date><sep><time><tz> are supported, with some
   --  extensions from [RFC 3339](https://tools.ietf.org/html/rfc3339) as
   --  mentioned below.
   --  Note that as Glib.Date_Time.Gdate_Time "is oblivious to leap seconds",
   --  leap seconds information in an ISO-8601 string will be ignored, so a
   --  `23:59:60` time would be parsed as `23:59:59`.
   --  <sep> is the separator and can be either 'T', 't' or ' '. The latter
   --  two separators are an extension from [RFC
   --  3339](https://tools.ietf.org/html/rfc3339section-5.6).
   --  <date> is in the form:
   --  - `YYYY-MM-DD` - Year/month/day, e.g. 2016-08-24. - `YYYYMMDD` - Same
   --  as above without dividers. - `YYYY-DDD` - Ordinal day where DDD is from
   --  001 to 366, e.g. 2016-237. - `YYYYDDD` - Same as above without dividers.
   --  - `YYYY-Www-D` - Week day where ww is from 01 to 52 and D from 1-7, e.g.
   --  2016-W34-3. - `YYYYWwwD` - Same as above without dividers.
   --  <time> is in the form:
   --  - `hh:mm:ss(.sss)` - Hours, minutes, seconds (subseconds), e.g.
   --  22:10:42.123. - `hhmmss(.sss)` - Same as above without dividers.
   --  <tz> is an optional timezone suffix of the form:
   --  - `Z` - UTC. - `+hh:mm` or `-hh:mm` - Offset from UTC in hours and
   --  minutes, e.g. +12:00. - `+hh` or `-hh` - Offset from UTC in hours, e.g.
   --  +12.
   --  If the timezone is not provided in Text it must be provided in
   --  Default_Tz (this field is otherwise ignored).
   --  This call can fail (returning null) if Text is not a valid ISO 8601
   --  formatted string.
   --  You should release the return value by calling Glib.Date_Time.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.56
   --  @param Text an ISO 8601 formatted time string.
   --  @param Default_Tz a Glib.Time_Zone.Gtime_Zone to use if the text
   --  doesn't contain a timezone, or null.

   function Gdate_Time_New_From_Iso8601
      (Text       : UTF8_String;
       Default_Tz : Glib.Time_Zone.Gtime_Zone) return Gdate_Time;
   --  Creates a Glib.Date_Time.Gdate_Time corresponding to the given [ISO
   --  8601 formatted string](https://en.wikipedia.org/wiki/ISO_8601) Text. ISO
   --  8601 strings of the form <date><sep><time><tz> are supported, with some
   --  extensions from [RFC 3339](https://tools.ietf.org/html/rfc3339) as
   --  mentioned below.
   --  Note that as Glib.Date_Time.Gdate_Time "is oblivious to leap seconds",
   --  leap seconds information in an ISO-8601 string will be ignored, so a
   --  `23:59:60` time would be parsed as `23:59:59`.
   --  <sep> is the separator and can be either 'T', 't' or ' '. The latter
   --  two separators are an extension from [RFC
   --  3339](https://tools.ietf.org/html/rfc3339section-5.6).
   --  <date> is in the form:
   --  - `YYYY-MM-DD` - Year/month/day, e.g. 2016-08-24. - `YYYYMMDD` - Same
   --  as above without dividers. - `YYYY-DDD` - Ordinal day where DDD is from
   --  001 to 366, e.g. 2016-237. - `YYYYDDD` - Same as above without dividers.
   --  - `YYYY-Www-D` - Week day where ww is from 01 to 52 and D from 1-7, e.g.
   --  2016-W34-3. - `YYYYWwwD` - Same as above without dividers.
   --  <time> is in the form:
   --  - `hh:mm:ss(.sss)` - Hours, minutes, seconds (subseconds), e.g.
   --  22:10:42.123. - `hhmmss(.sss)` - Same as above without dividers.
   --  <tz> is an optional timezone suffix of the form:
   --  - `Z` - UTC. - `+hh:mm` or `-hh:mm` - Offset from UTC in hours and
   --  minutes, e.g. +12:00. - `+hh` or `-hh` - Offset from UTC in hours, e.g.
   --  +12.
   --  If the timezone is not provided in Text it must be provided in
   --  Default_Tz (this field is otherwise ignored).
   --  This call can fail (returning null) if Text is not a valid ISO 8601
   --  formatted string.
   --  You should release the return value by calling Glib.Date_Time.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.56
   --  @param Text an ISO 8601 formatted time string.
   --  @param Default_Tz a Glib.Time_Zone.Gtime_Zone to use if the text
   --  doesn't contain a timezone, or null.

   procedure G_New_From_Unix_Local (Self : out Gdate_Time; T : Gint64);
   --  Creates a Glib.Date_Time.Gdate_Time corresponding to the given Unix
   --  time T in the local time zone.
   --  Unix time is the number of seconds that have elapsed since 1970-01-01
   --  00:00:00 UTC, regardless of the local time offset.
   --  This call can fail (returning null) if T represents a time outside of
   --  the supported range of Glib.Date_Time.Gdate_Time.
   --  You should release the return value by calling Glib.Date_Time.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.26
   --  @param T the Unix time

   function Gdate_Time_New_From_Unix_Local (T : Gint64) return Gdate_Time;
   --  Creates a Glib.Date_Time.Gdate_Time corresponding to the given Unix
   --  time T in the local time zone.
   --  Unix time is the number of seconds that have elapsed since 1970-01-01
   --  00:00:00 UTC, regardless of the local time offset.
   --  This call can fail (returning null) if T represents a time outside of
   --  the supported range of Glib.Date_Time.Gdate_Time.
   --  You should release the return value by calling Glib.Date_Time.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.26
   --  @param T the Unix time

   procedure G_New_From_Unix_Local_Usec
      (Self  : out Gdate_Time;
       Usecs : Gint64);

   function Gdate_Time_New_From_Unix_Local_Usec
      (Usecs : Gint64) return Gdate_Time;

   procedure G_New_From_Unix_Utc (Self : out Gdate_Time; T : Gint64);
   --  Creates a Glib.Date_Time.Gdate_Time corresponding to the given Unix
   --  time T in UTC.
   --  Unix time is the number of seconds that have elapsed since 1970-01-01
   --  00:00:00 UTC.
   --  This call can fail (returning null) if T represents a time outside of
   --  the supported range of Glib.Date_Time.Gdate_Time.
   --  You should release the return value by calling Glib.Date_Time.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.26
   --  @param T the Unix time

   function Gdate_Time_New_From_Unix_Utc (T : Gint64) return Gdate_Time;
   --  Creates a Glib.Date_Time.Gdate_Time corresponding to the given Unix
   --  time T in UTC.
   --  Unix time is the number of seconds that have elapsed since 1970-01-01
   --  00:00:00 UTC.
   --  This call can fail (returning null) if T represents a time outside of
   --  the supported range of Glib.Date_Time.Gdate_Time.
   --  You should release the return value by calling Glib.Date_Time.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.26
   --  @param T the Unix time

   procedure G_New_From_Unix_Utc_Usec
      (Self  : out Gdate_Time;
       Usecs : Gint64);

   function Gdate_Time_New_From_Unix_Utc_Usec
      (Usecs : Gint64) return Gdate_Time;

   procedure G_New_Local
      (Self    : out Gdate_Time;
       Year    : Glib.Gint;
       Month   : Glib.Gint;
       Day     : Glib.Gint;
       Hour    : Glib.Gint;
       Minute  : Glib.Gint;
       Seconds : Gdouble);
   --  Creates a new Glib.Date_Time.Gdate_Time corresponding to the given date
   --  and time in the local time zone.
   --  This call is equivalent to calling g_date_time_new with the time zone
   --  returned by g_time_zone_new_local.
   --  Since: gtk+ 2.26
   --  @param Year the year component of the date
   --  @param Month the month component of the date
   --  @param Day the day component of the date
   --  @param Hour the hour component of the date
   --  @param Minute the minute component of the date
   --  @param Seconds the number of seconds past the minute

   function Gdate_Time_New_Local
      (Year    : Glib.Gint;
       Month   : Glib.Gint;
       Day     : Glib.Gint;
       Hour    : Glib.Gint;
       Minute  : Glib.Gint;
       Seconds : Gdouble) return Gdate_Time;
   --  Creates a new Glib.Date_Time.Gdate_Time corresponding to the given date
   --  and time in the local time zone.
   --  This call is equivalent to calling g_date_time_new with the time zone
   --  returned by g_time_zone_new_local.
   --  Since: gtk+ 2.26
   --  @param Year the year component of the date
   --  @param Month the month component of the date
   --  @param Day the day component of the date
   --  @param Hour the hour component of the date
   --  @param Minute the minute component of the date
   --  @param Seconds the number of seconds past the minute

   procedure G_New_Now
      (Self : out Gdate_Time;
       Tz   : Glib.Time_Zone.Gtime_Zone);
   --  Creates a Glib.Date_Time.Gdate_Time corresponding to this exact instant
   --  in the given time zone Tz. The time is as accurate as the system allows,
   --  to a maximum accuracy of 1 microsecond.
   --  This function will always succeed unless GLib is still being used after
   --  the year 9999.
   --  You should release the return value by calling Glib.Date_Time.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.26
   --  @param Tz a Glib.Time_Zone.Gtime_Zone

   function Gdate_Time_New_Now
      (Tz : Glib.Time_Zone.Gtime_Zone) return Gdate_Time;
   --  Creates a Glib.Date_Time.Gdate_Time corresponding to this exact instant
   --  in the given time zone Tz. The time is as accurate as the system allows,
   --  to a maximum accuracy of 1 microsecond.
   --  This function will always succeed unless GLib is still being used after
   --  the year 9999.
   --  You should release the return value by calling Glib.Date_Time.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.26
   --  @param Tz a Glib.Time_Zone.Gtime_Zone

   procedure G_New_Now_Local (Self : out Gdate_Time);
   --  Creates a Glib.Date_Time.Gdate_Time corresponding to this exact instant
   --  in the local time zone.
   --  This is equivalent to calling g_date_time_new_now with the time zone
   --  returned by g_time_zone_new_local.
   --  Since: gtk+ 2.26

   function Gdate_Time_New_Now_Local return Gdate_Time;
   --  Creates a Glib.Date_Time.Gdate_Time corresponding to this exact instant
   --  in the local time zone.
   --  This is equivalent to calling g_date_time_new_now with the time zone
   --  returned by g_time_zone_new_local.
   --  Since: gtk+ 2.26

   procedure G_New_Now_Utc (Self : out Gdate_Time);
   --  Creates a Glib.Date_Time.Gdate_Time corresponding to this exact instant
   --  in UTC.
   --  This is equivalent to calling g_date_time_new_now with the time zone
   --  returned by g_time_zone_new_utc.
   --  Since: gtk+ 2.26

   function Gdate_Time_New_Now_Utc return Gdate_Time;
   --  Creates a Glib.Date_Time.Gdate_Time corresponding to this exact instant
   --  in UTC.
   --  This is equivalent to calling g_date_time_new_now with the time zone
   --  returned by g_time_zone_new_utc.
   --  Since: gtk+ 2.26

   procedure G_New_Utc
      (Self    : out Gdate_Time;
       Year    : Glib.Gint;
       Month   : Glib.Gint;
       Day     : Glib.Gint;
       Hour    : Glib.Gint;
       Minute  : Glib.Gint;
       Seconds : Gdouble);
   --  Creates a new Glib.Date_Time.Gdate_Time corresponding to the given date
   --  and time in UTC.
   --  This call is equivalent to calling g_date_time_new with the time zone
   --  returned by g_time_zone_new_utc.
   --  Since: gtk+ 2.26
   --  @param Year the year component of the date
   --  @param Month the month component of the date
   --  @param Day the day component of the date
   --  @param Hour the hour component of the date
   --  @param Minute the minute component of the date
   --  @param Seconds the number of seconds past the minute

   function Gdate_Time_New_Utc
      (Year    : Glib.Gint;
       Month   : Glib.Gint;
       Day     : Glib.Gint;
       Hour    : Glib.Gint;
       Minute  : Glib.Gint;
       Seconds : Gdouble) return Gdate_Time;
   --  Creates a new Glib.Date_Time.Gdate_Time corresponding to the given date
   --  and time in UTC.
   --  This call is equivalent to calling g_date_time_new with the time zone
   --  returned by g_time_zone_new_utc.
   --  Since: gtk+ 2.26
   --  @param Year the year component of the date
   --  @param Month the month component of the date
   --  @param Day the day component of the date
   --  @param Hour the hour component of the date
   --  @param Minute the minute component of the date
   --  @param Seconds the number of seconds past the minute

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_date_time_get_type");

   -------------
   -- Methods --
   -------------

   function Add
      (Self     : Gdate_Time;
       Timespan : Glib.Gint64) return Gdate_Time;
   --  Creates a copy of Datetime and adds the specified timespan to the copy.
   --  Since: gtk+ 2.26
   --  @param Timespan a Glib.Gint64
   --  @return the newly created Glib.Date_Time.Gdate_Time which should be
   --  freed with Glib.Date_Time.Unref, or null. Has transfer-ownership='full'.

   function Add_Days (Self : Gdate_Time; Days : Glib.Gint) return Gdate_Time;
   --  Creates a copy of Datetime and adds the specified number of days to the
   --  copy. Add negative values to subtract days.
   --  Since: gtk+ 2.26
   --  @param Days the number of days
   --  @return the newly created Glib.Date_Time.Gdate_Time which should be
   --  freed with Glib.Date_Time.Unref, or null. Has transfer-ownership='full'.

   function Add_Full
      (Self    : Gdate_Time;
       Years   : Glib.Gint;
       Months  : Glib.Gint;
       Days    : Glib.Gint;
       Hours   : Glib.Gint;
       Minutes : Glib.Gint;
       Seconds : Gdouble) return Gdate_Time;
   --  Creates a new Glib.Date_Time.Gdate_Time adding the specified values to
   --  the current date and time in Datetime. Add negative values to subtract.
   --  Since: gtk+ 2.26
   --  @param Years the number of years to add
   --  @param Months the number of months to add
   --  @param Days the number of days to add
   --  @param Hours the number of hours to add
   --  @param Minutes the number of minutes to add
   --  @param Seconds the number of seconds to add
   --  @return the newly created Glib.Date_Time.Gdate_Time which should be
   --  freed with Glib.Date_Time.Unref, or null. Has transfer-ownership='full'.

   function Add_Hours
      (Self  : Gdate_Time;
       Hours : Glib.Gint) return Gdate_Time;
   --  Creates a copy of Datetime and adds the specified number of hours. Add
   --  negative values to subtract hours.
   --  Since: gtk+ 2.26
   --  @param Hours the number of hours to add
   --  @return the newly created Glib.Date_Time.Gdate_Time which should be
   --  freed with Glib.Date_Time.Unref, or null. Has transfer-ownership='full'.

   function Add_Minutes
      (Self    : Gdate_Time;
       Minutes : Glib.Gint) return Gdate_Time;
   --  Creates a copy of Datetime adding the specified number of minutes. Add
   --  negative values to subtract minutes.
   --  Since: gtk+ 2.26
   --  @param Minutes the number of minutes to add
   --  @return the newly created Glib.Date_Time.Gdate_Time which should be
   --  freed with Glib.Date_Time.Unref, or null. Has transfer-ownership='full'.

   function Add_Months
      (Self   : Gdate_Time;
       Months : Glib.Gint) return Gdate_Time;
   --  Creates a copy of Datetime and adds the specified number of months to
   --  the copy. Add negative values to subtract months.
   --  The day of the month of the resulting Glib.Date_Time.Gdate_Time is
   --  clamped to the number of days in the updated calendar month. For
   --  example, if adding 1 month to 31st January 2018, the result would be
   --  28th February 2018. In 2020 (a leap year), the result would be 29th
   --  February.
   --  Since: gtk+ 2.26
   --  @param Months the number of months
   --  @return the newly created Glib.Date_Time.Gdate_Time which should be
   --  freed with Glib.Date_Time.Unref, or null. Has transfer-ownership='full'.

   function Add_Seconds
      (Self    : Gdate_Time;
       Seconds : Gdouble) return Gdate_Time;
   --  Creates a copy of Datetime and adds the specified number of seconds.
   --  Add negative values to subtract seconds.
   --  Since: gtk+ 2.26
   --  @param Seconds the number of seconds to add
   --  @return the newly created Glib.Date_Time.Gdate_Time which should be
   --  freed with Glib.Date_Time.Unref, or null. Has transfer-ownership='full'.

   function Add_Weeks
      (Self  : Gdate_Time;
       Weeks : Glib.Gint) return Gdate_Time;
   --  Creates a copy of Datetime and adds the specified number of weeks to
   --  the copy. Add negative values to subtract weeks.
   --  Since: gtk+ 2.26
   --  @param Weeks the number of weeks
   --  @return the newly created Glib.Date_Time.Gdate_Time which should be
   --  freed with Glib.Date_Time.Unref, or null. Has transfer-ownership='full'.

   function Add_Years
      (Self  : Gdate_Time;
       Years : Glib.Gint) return Gdate_Time;
   --  Creates a copy of Datetime and adds the specified number of years to
   --  the copy. Add negative values to subtract years.
   --  As with Glib.Date_Time.Add_Months, if the resulting date would be 29th
   --  February on a non-leap year, the day will be clamped to 28th February.
   --  Since: gtk+ 2.26
   --  @param Years the number of years
   --  @return the newly created Glib.Date_Time.Gdate_Time which should be
   --  freed with Glib.Date_Time.Unref, or null. Has transfer-ownership='full'.

   function Compare (Self : Gdate_Time; Dt2 : Gdate_Time) return Glib.Gint;
   --  A comparison function for GDate_Times that is suitable as a
   --  Gcompare_Func. Both GDate_Times must be non-null.
   --  Since: gtk+ 2.26
   --  @param Dt2 second Glib.Date_Time.Gdate_Time to compare
   --  @return -1, 0 or 1 if Dt1 is less than, equal to or greater than Dt2.

   function Difference
      (Self  : Gdate_Time;
       Start : Gdate_Time) return Glib.Gint64;
   --  Calculates the difference in time between End and Begin. The
   --  Glib.Gint64 that is returned is effectively End - Begin (ie: positive if
   --  the first parameter is larger).
   --  Since: gtk+ 2.26
   --  @param Start a Glib.Date_Time.Gdate_Time
   --  @return the difference between the two Glib.Date_Time.Gdate_Time, as a
   --  time span expressed in microseconds.

   function Equal (Self : Gdate_Time; Dt2 : Gdate_Time) return Boolean;
   --  Checks to see if Dt1 and Dt2 are equal.
   --  Equal here means that they represent the same moment after converting
   --  them to the same time zone.
   --  Since: gtk+ 2.26
   --  @param Dt2 a Glib.Date_Time.Gdate_Time
   --  @return True if Dt1 and Dt2 are equal

   function Format
      (Self   : Gdate_Time;
       Format : UTF8_String) return UTF8_String;
   --  Creates a newly allocated string representing the requested Format.
   --  The format strings understood by this function are a subset of the
   --  `strftime` format language as specified by C99. The `%D`, `%U` and `%W`
   --  conversions are not supported, nor is the `E` modifier. The GNU
   --  extensions `%k`, `%l`, `%s` and `%P` are supported, however, as are the
   --  `0`, `_` and `-` modifiers. The Python extension `%f` is also supported.
   --  In contrast to `strftime`, this function always produces a UTF-8
   --  string, regardless of the current locale. Note that the rendering of
   --  many formats is locale-dependent and may not match the `strftime` output
   --  exactly.
   --  The following format specifiers are supported:
   --  - `%a`: the abbreviated weekday name according to the current locale -
   --  `%A`: the full weekday name according to the current locale - `%b`: the
   --  abbreviated month name according to the current locale - `%B`: the full
   --  month name according to the current locale - `%c`: the preferred date
   --  and time representation for the current locale - `%C`: the century
   --  number (year/100) as a 2-digit integer (00-99) - `%d`: the day of the
   --  month as a decimal number (range 01 to 31) - `%e`: the day of the month
   --  as a decimal number (range 1 to 31); single digits are preceded by a
   --  figure space (U+2007) - `%F`: equivalent to `%Y-%m-%d` (the ISO 8601
   --  date format) - `%g`: the last two digits of the ISO 8601 week-based year
   --  as a decimal number (00-99). This works well with `%V` and `%u`. - `%G`:
   --  the ISO 8601 week-based year as a decimal number. This works well with
   --  `%V` and `%u`. - `%h`: equivalent to `%b` - `%H`: the hour as a decimal
   --  number using a 24-hour clock (range 00 to 23) - `%I`: the hour as a
   --  decimal number using a 12-hour clock (range 01 to 12) - `%j`: the day of
   --  the year as a decimal number (range 001 to 366) - `%k`: the hour
   --  (24-hour clock) as a decimal number (range 0 to 23); single digits are
   --  preceded by a figure space (U+2007) - `%l`: the hour (12-hour clock) as
   --  a decimal number (range 1 to 12); single digits are preceded by a figure
   --  space (U+2007) - `%m`: the month as a decimal number (range 01 to 12) -
   --  `%M`: the minute as a decimal number (range 00 to 59) - `%f`: the
   --  microsecond as a decimal number (range 000000 to 999999) - `%p`: either
   --  ‘AM' or ‘PM' according to the given time value, or the corresponding
   --  strings for the current locale. Noon is treated as ‘PM' and midnight as
   --  ‘AM'. Use of this format specifier is discouraged, as many locales have
   --  no concept of AM/PM formatting. Use `%c` or `%X` instead. - `%P`: like
   --  `%p` but lowercase: ‘am' or ‘pm' or a corresponding string for the
   --  current locale. Use of this format specifier is discouraged, as many
   --  locales have no concept of AM/PM formatting. Use `%c` or `%X` instead. -
   --  `%r`: the time in a.m. or p.m. notation. Use of this format specifier is
   --  discouraged, as many locales have no concept of AM/PM formatting. Use
   --  `%c` or `%X` instead. - `%R`: the time in 24-hour notation (`%H:%M`) -
   --  `%s`: the number of seconds since the Epoch, that is, since 1970-01-01
   --  00:00:00 UTC - `%S`: the second as a decimal number (range 00 to 60) -
   --  `%t`: a tab character - `%T`: the time in 24-hour notation with seconds
   --  (`%H:%M:%S`) - `%u`: the ISO 8601 standard day of the week as a decimal,
   --  range 1 to 7, Monday being 1. This works well with `%G` and `%V`. -
   --  `%V`: the ISO 8601 standard week number of the current year as a decimal
   --  number, range 01 to 53, where week 1 is the first week that has at least
   --  4 days in the new year. See Glib.Date_Time.Get_Week_Of_Year. This works
   --  well with `%G` and `%u`. - `%w`: the day of the week as a decimal, range
   --  0 to 6, Sunday being 0. This is not the ISO 8601 standard format — use
   --  `%u` instead. - `%x`: the preferred date representation for the current
   --  locale without the time - `%X`: the preferred time representation for
   --  the current locale without the date - `%y`: the year as a decimal number
   --  without the century - `%Y`: the year as a decimal number including the
   --  century - `%z`: the time zone as an offset from UTC (`+hhmm`) - `%:z`:
   --  the time zone as an offset from UTC (`+hh:mm`). This is a gnulib
   --  `strftime` extension. Since: 2.38 - `%::z`: the time zone as an offset
   --  from UTC (`+hh:mm:ss`). This is a gnulib `strftime` extension. Since:
   --  2.38 - `%:::z`: the time zone as an offset from UTC, with `:` to
   --  necessary precision (e.g., `-04`, `+05:30`). This is a gnulib `strftime`
   --  extension. Since: 2.38 - `%Z`: the time zone or name or abbreviation -
   --  `%%`: a literal `%` character
   --  Some conversion specifications can be modified by preceding the
   --  conversion specifier by one or more modifier characters.
   --  The following modifiers are supported for many of the numeric
   --  conversions:
   --  - `O`: Use alternative numeric symbols, if the current locale supports
   --  those. - `_`: Pad a numeric result with spaces. This overrides the
   --  default padding for the specifier. - `-`: Do not pad a numeric result.
   --  This overrides the default padding for the specifier. - `0`: Pad a
   --  numeric result with zeros. This overrides the default padding for the
   --  specifier.
   --  The following modifiers are supported for many of the alphabetic
   --  conversions:
   --  - `^`: Use upper case if possible. This is a gnulib `strftime`
   --  extension. Since: 2.80 - `#`: Use opposite case if possible. This is a
   --  gnulib `strftime` extension. Since: 2.80
   --  Additionally, when `O` is used with `B`, `b`, or `h`, it produces the
   --  alternative form of a month name. The alternative form should be used
   --  when the month name is used without a day number (e.g., standalone). It
   --  is required in some languages (Baltic, Slavic, Greek, and more) due to
   --  their grammatical rules. For other languages there is no difference.
   --  `OB` is a GNU and BSD `strftime` extension expected to be added to the
   --  future POSIX specification, `Ob` and `Oh` are GNU `strftime` extensions.
   --  Since: 2.56
   --  Since: gtk+ 2.26
   --  @param Format a valid UTF-8 string, containing the format for the
   --  Glib.Date_Time.Gdate_Time
   --  @return a newly allocated string formatted to the requested format or
   --  null in the case that there was an error (such as a format specifier not
   --  being supported in the current locale). The string should be freed with
   --  g_free.

   function Format_Iso8601 (Self : Gdate_Time) return UTF8_String;
   --  Format Datetime in [ISO 8601
   --  format](https://en.wikipedia.org/wiki/ISO_8601), including the date,
   --  time and time zone, and return that as a UTF-8 encoded string.
   --  Since GLib 2.66, this will output to sub-second precision if needed.
   --  Since: gtk+ 2.62
   --  @return a newly allocated string formatted in ISO 8601 format or null
   --  in the case that there was an error. The string should be freed with
   --  g_free.

   function Get_Day_Of_Month (Self : Gdate_Time) return Glib.Gint;
   --  Retrieves the day of the month represented by Datetime in the gregorian
   --  calendar.
   --  Since: gtk+ 2.26
   --  @return the day of the month

   function Get_Day_Of_Week (Self : Gdate_Time) return Glib.Gint;
   --  Retrieves the ISO 8601 day of the week on which Datetime falls (1 is
   --  Monday, 2 is Tuesday... 7 is Sunday).
   --  Since: gtk+ 2.26
   --  @return the day of the week

   function Get_Day_Of_Year (Self : Gdate_Time) return Glib.Gint;
   --  Retrieves the day of the year represented by Datetime in the Gregorian
   --  calendar.
   --  Since: gtk+ 2.26
   --  @return the day of the year

   function Get_Hour (Self : Gdate_Time) return Glib.Gint;
   --  Retrieves the hour of the day represented by Datetime
   --  Since: gtk+ 2.26
   --  @return the hour of the day

   function Get_Microsecond (Self : Gdate_Time) return Glib.Gint;
   --  Retrieves the microsecond of the date represented by Datetime
   --  Since: gtk+ 2.26
   --  @return the microsecond of the second

   function Get_Minute (Self : Gdate_Time) return Glib.Gint;
   --  Retrieves the minute of the hour represented by Datetime
   --  Since: gtk+ 2.26
   --  @return the minute of the hour

   function Get_Month (Self : Gdate_Time) return Glib.Gint;
   --  Retrieves the month of the year represented by Datetime in the
   --  Gregorian calendar.
   --  Since: gtk+ 2.26
   --  @return the month represented by Datetime

   function Get_Second (Self : Gdate_Time) return Glib.Gint;
   --  Retrieves the second of the minute represented by Datetime
   --  Since: gtk+ 2.26
   --  @return the second represented by Datetime

   function Get_Seconds (Self : Gdate_Time) return Gdouble;
   --  Retrieves the number of seconds since the start of the last minute,
   --  including the fractional part.
   --  Since: gtk+ 2.26
   --  @return the number of seconds

   function Get_Timezone
      (Self : Gdate_Time) return Glib.Time_Zone.Gtime_Zone;
   --  Get the time zone for this Datetime.
   --  Since: gtk+ 2.58
   --  @return the time zone. Has transfer-ownership='none'.

   function Get_Timezone_Abbreviation (Self : Gdate_Time) return UTF8_String;
   --  Determines the time zone abbreviation to be used at the time and in the
   --  time zone of Datetime.
   --  For example, in Toronto this is currently "EST" during the winter
   --  months and "EDT" during the summer months when daylight savings time is
   --  in effect.
   --  Since: gtk+ 2.26
   --  @return the time zone abbreviation. The returned string is owned by the
   --  Glib.Date_Time.Gdate_Time and it should not be modified or freed

   function Get_Utc_Offset (Self : Gdate_Time) return Glib.Gint64;
   --  Determines the offset to UTC in effect at the time and in the time zone
   --  of Datetime.
   --  The offset is the number of microseconds that you add to UTC time to
   --  arrive at local time for the time zone (ie: negative numbers for time
   --  zones west of GMT, positive numbers for east).
   --  If Datetime represents UTC time, then the offset is always zero.
   --  Since: gtk+ 2.26
   --  @return the number of microseconds that should be added to UTC to get
   --  the local time

   function Get_Week_Numbering_Year (Self : Gdate_Time) return Glib.Gint;
   --  Returns the ISO 8601 week-numbering year in which the week containing
   --  Datetime falls.
   --  This function, taken together with Glib.Date_Time.Get_Week_Of_Year and
   --  Glib.Date_Time.Get_Day_Of_Week can be used to determine the full ISO
   --  week date on which Datetime falls.
   --  This is usually equal to the normal Gregorian year (as returned by
   --  Glib.Date_Time.Get_Year), except as detailed below:
   --  For Thursday, the week-numbering year is always equal to the usual
   --  calendar year. For other days, the number is such that every day within
   --  a complete week (Monday to Sunday) is contained within the same
   --  week-numbering year.
   --  For Monday, Tuesday and Wednesday occurring near the end of the year,
   --  this may mean that the week-numbering year is one greater than the
   --  calendar year (so that these days have the same week-numbering year as
   --  the Thursday occurring early in the next year).
   --  For Friday, Saturday and Sunday occurring near the start of the year,
   --  this may mean that the week-numbering year is one less than the calendar
   --  year (so that these days have the same week-numbering year as the
   --  Thursday occurring late in the previous year).
   --  An equivalent description is that the week-numbering year is equal to
   --  the calendar year containing the majority of the days in the current
   --  week (Monday to Sunday).
   --  Note that January 1 0001 in the proleptic Gregorian calendar is a
   --  Monday, so this function never returns 0.
   --  Since: gtk+ 2.26
   --  @return the ISO 8601 week-numbering year for Datetime

   function Get_Week_Of_Year (Self : Gdate_Time) return Glib.Gint;
   --  Returns the ISO 8601 week number for the week containing Datetime. The
   --  ISO 8601 week number is the same for every day of the week (from Moday
   --  through Sunday). That can produce some unusual results (described
   --  below).
   --  The first week of the year is week 1. This is the week that contains
   --  the first Thursday of the year. Equivalently, this is the first week
   --  that has more than 4 of its days falling within the calendar year.
   --  The value 0 is never returned by this function. Days contained within a
   --  year but occurring before the first ISO 8601 week of that year are
   --  considered as being contained in the last week of the previous year.
   --  Similarly, the final days of a calendar year may be considered as being
   --  part of the first ISO 8601 week of the next year if 4 or more days of
   --  that week are contained within the new year.
   --  Since: gtk+ 2.26
   --  @return the ISO 8601 week number for Datetime.

   function Get_Year (Self : Gdate_Time) return Glib.Gint;
   --  Retrieves the year represented by Datetime in the Gregorian calendar.
   --  Since: gtk+ 2.26
   --  @return the year represented by Datetime

   procedure Get_Ymd
      (Self  : Gdate_Time;
       Year  : out Glib.Gint;
       Month : out Glib.Gint;
       Day   : out Glib.Gint);
   --  Retrieves the Gregorian day, month, and year of a given
   --  Glib.Date_Time.Gdate_Time.
   --  Since: gtk+ 2.26
   --  @param Year the return location for the gregorian year, or null.
   --  @param Month the return location for the month of the year, or null.
   --  @param Day the return location for the day of the month, or null.

   function Hash (Self : Gdate_Time) return Guint;
   --  Hashes Datetime into a Guint, suitable for use within GHash_Table.
   --  Since: gtk+ 2.26
   --  @return a Guint containing the hash

   function Is_Daylight_Savings (Self : Gdate_Time) return Boolean;
   --  Determines if daylight savings time is in effect at the time and in the
   --  time zone of Datetime.
   --  Since: gtk+ 2.26
   --  @return True if daylight savings time is in effect

   function Ref (Self : Gdate_Time) return Gdate_Time;
   --  Atomically increments the reference count of Datetime by one.
   --  Since: gtk+ 2.26
   --  @return the Glib.Date_Time.Gdate_Time with the reference count
   --  increased. Has transfer-ownership='full'.

   function To_Local (Self : Gdate_Time) return Gdate_Time;
   --  Creates a new Glib.Date_Time.Gdate_Time corresponding to the same
   --  instant in time as Datetime, but in the local time zone.
   --  This call is equivalent to calling Glib.Date_Time.To_Timezone with the
   --  time zone returned by g_time_zone_new_local.
   --  Since: gtk+ 2.26
   --  @return the newly created Glib.Date_Time.Gdate_Time which should be
   --  freed with Glib.Date_Time.Unref, or null. Has transfer-ownership='full'.

   function To_Timezone
      (Self : Gdate_Time;
       Tz   : Glib.Time_Zone.Gtime_Zone) return Gdate_Time;
   --  Create a new Glib.Date_Time.Gdate_Time corresponding to the same
   --  instant in time as Datetime, but in the time zone Tz.
   --  This call can fail in the case that the time goes out of bounds. For
   --  example, converting 0001-01-01 00:00:00 UTC to a time zone west of
   --  Greenwich will fail (due to the year 0 being out of range).
   --  Since: gtk+ 2.26
   --  @param Tz the new Glib.Time_Zone.Gtime_Zone
   --  @return the newly created Glib.Date_Time.Gdate_Time which should be
   --  freed with Glib.Date_Time.Unref, or null. Has transfer-ownership='full'.

   function To_Unix (Self : Gdate_Time) return Gint64;
   --  Gives the Unix time corresponding to Datetime, rounding down to the
   --  nearest second.
   --  Unix time is the number of seconds that have elapsed since 1970-01-01
   --  00:00:00 UTC, regardless of the time zone associated with Datetime.
   --  Since: gtk+ 2.26
   --  @return the Unix time corresponding to Datetime

   function To_Unix_Usec (Self : Gdate_Time) return Gint64;

   function To_Utc (Self : Gdate_Time) return Gdate_Time;
   --  Creates a new Glib.Date_Time.Gdate_Time corresponding to the same
   --  instant in time as Datetime, but in UTC.
   --  This call is equivalent to calling Glib.Date_Time.To_Timezone with the
   --  time zone returned by g_time_zone_new_utc.
   --  Since: gtk+ 2.26
   --  @return the newly created Glib.Date_Time.Gdate_Time which should be
   --  freed with Glib.Date_Time.Unref, or null. Has transfer-ownership='full'.

   procedure Unref (Self : Gdate_Time);
   --  Atomically decrements the reference count of Datetime by one.
   --  When the reference count reaches zero, the resources allocated by
   --  Datetime are freed
   --  Since: gtk+ 2.26

private
   Null_Gdate_Time : constant Gdate_Time :=
      (Glib.C_Boxed with null record);

end Glib.Date_Time;
