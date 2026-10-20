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

--  A `GTimeZone` represents a time zone, at no particular point in time.
--
--  The `GTimeZone` struct is refcounted and immutable.
--
--  Each time zone has an identifier (for example, ‘Europe/London') which is
--  platform dependent. See [methodGlib.TimeZone.new] for information on the
--  identifier formats. The identifier of a time zone can be retrieved using
--  [methodGlib.TimeZone.get_identifier].
--
--  A time zone contains a number of intervals. Each interval has an
--  abbreviation to describe it (for example, ‘PDT'), an offset to UTC and a
--  flag indicating if the daylight savings time is in effect during that
--  interval. A time zone always has at least one interval — interval 0. Note
--  that interval abbreviations are not the same as time zone identifiers
--  (apart from ‘UTC'), and cannot be passed to [methodGlib.TimeZone.new].
--
--  Every UTC time is contained within exactly one interval, but a given local
--  time may be contained within zero, one or two intervals (due to
--  incontinuities associated with daylight savings time).
--
--  An interval may refer to a specific period of time (eg: the duration of
--  daylight savings time during 2010) or it may refer to many periods of time
--  that share the same properties (eg: all periods of daylight savings time).
--  It is also possible (usually for political reasons) that some properties
--  (like the abbreviation) change between intervals without other properties
--  changing.
--
--  <group>GLib</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Glib.Time_Zone is

   type Gtime_Zone is new Glib.C_Boxed with null record;
   Null_Gtime_Zone : constant Gtime_Zone;

   function From_Object (Object : System.Address) return Gtime_Zone;
   function From_Object_Free (B : access Gtime_Zone'Class) return Gtime_Zone;
   pragma Inline (From_Object_Free, From_Object);

   type GTime_Type is (
      G_Time_Type_Standard,
      G_Time_Type_Daylight,
      G_Time_Type_Universal);
   pragma Convention (C, GTime_Type);
   --  Disambiguates a given time in two ways.
   --
   --  First, specifies if the given time is in universal or local time.
   --
   --  Second, if the time is in local time, specifies if it is local standard
   --  time or local daylight time. This is important for the case where the
   --  same local time occurs twice (during daylight savings time transitions,
   --  for example).

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package GTime_Type_Properties is
      new Generic_Internal_Discrete_Property (GTime_Type);
   type Property_GTime_Type is new GTime_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure G_New (Self : out Gtime_Zone; Identifier : UTF8_String := "");
   --  A version of g_time_zone_new_identifier which returns the UTC time zone
   --  if Identifier could not be parsed or loaded.
   --  If you need to check whether Identifier was loaded successfully, use
   --  g_time_zone_new_identifier.
   --  Since: gtk+ 2.26
   --  @param Identifier a timezone identifier

   function Gtime_Zone_New
      (Identifier : UTF8_String := "") return Gtime_Zone;
   --  A version of g_time_zone_new_identifier which returns the UTC time zone
   --  if Identifier could not be parsed or loaded.
   --  If you need to check whether Identifier was loaded successfully, use
   --  g_time_zone_new_identifier.
   --  Since: gtk+ 2.26
   --  @param Identifier a timezone identifier

   procedure G_New_Identifier
      (Self       : out Gtime_Zone;
       Identifier : UTF8_String := "");
   --  Creates a Glib.Time_Zone.Gtime_Zone corresponding to Identifier. If
   --  Identifier cannot be parsed or loaded, null is returned.
   --  Identifier can either be an RFC3339/ISO 8601 time offset or something
   --  that would pass as a valid value for the `TZ` environment variable
   --  (including null).
   --  In Windows, Identifier can also be the unlocalized name of a time zone
   --  for standard time, for example "Pacific Standard Time".
   --  Valid RFC3339 time offsets are `"Z"` (for UTC) or `"±hh:mm"`. ISO 8601
   --  additionally specifies `"±hhmm"` and `"±hh"`. Offsets are time values to
   --  be added to Coordinated Universal Time (UTC) to get the local time.
   --  In UNIX, the `TZ` environment variable typically corresponds to the
   --  name of a file in the zoneinfo database, an absolute path to a file
   --  somewhere else, or a string in "std offset [dst
   --  [offset],start[/time],end[/time]]" (POSIX) format. There are no spaces
   --  in the specification. The name of standard and daylight savings time
   --  zone must be three or more alphabetic characters. Offsets are time
   --  values to be added to local time to get Coordinated Universal Time (UTC)
   --  and should be `"[±]hh[[:]mm[:ss]]"`. Dates are either `"Jn"` (Julian day
   --  with n between 1 and 365, leap years not counted), `"n"` (zero-based
   --  Julian day with n between 0 and 365) or `"Mm.w.d"` (day d (0 <= d <= 6)
   --  of week w (1 <= w <= 5) of month m (1 <= m <= 12), day 0 is a Sunday).
   --  Times are in local wall clock time, the default is 02:00:00.
   --  In Windows, the "tzn[+|–]hh[:mm[:ss]][dzn]" format is used, but also
   --  accepts POSIX format. The Windows format uses US rules for all time
   --  zones; daylight savings time is 60 minutes behind the standard time with
   --  date and time of change taken from Pacific Standard Time. Offsets are
   --  time values to be added to the local time to get Coordinated Universal
   --  Time (UTC).
   --  g_time_zone_new_local calls this function with the value of the `TZ`
   --  environment variable. This function itself is independent of the value
   --  of `TZ`, but if Identifier is null then `/etc/localtime` will be
   --  consulted to discover the correct time zone on UNIX and the registry
   --  will be consulted or GetTimeZoneInformation will be used to get the
   --  local time zone on Windows.
   --  If intervals are not available, only time zone rules from `TZ`
   --  environment variable or other means, then they will be computed from
   --  year 1900 to 2037. If the maximum year for the rules is available and it
   --  is greater than 2037, then it will followed instead.
   --  See [RFC3339 §5.6](http://tools.ietf.org/html/rfc3339section-5.6) for a
   --  precise definition of valid RFC3339 time offsets (the `time-offset`
   --  expansion) and ISO 8601 for the full list of valid time offsets. See
   --  [The GNU C Library
   --  manual](http://www.gnu.org/s/libc/manual/html_node/TZ-Variable.html) for
   --  an explanation of the possible values of the `TZ` environment variable.
   --  See [Microsoft Time Zone Index
   --  Values](http://msdn.microsoft.com/en-us/library/ms912391%28v=winembedded.11%29.aspx)
   --  for the list of time zones on Windows.
   --  You should release the return value by calling Glib.Time_Zone.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.68
   --  @param Identifier a timezone identifier

   function Gtime_Zone_New_Identifier
      (Identifier : UTF8_String := "") return Gtime_Zone;
   --  Creates a Glib.Time_Zone.Gtime_Zone corresponding to Identifier. If
   --  Identifier cannot be parsed or loaded, null is returned.
   --  Identifier can either be an RFC3339/ISO 8601 time offset or something
   --  that would pass as a valid value for the `TZ` environment variable
   --  (including null).
   --  In Windows, Identifier can also be the unlocalized name of a time zone
   --  for standard time, for example "Pacific Standard Time".
   --  Valid RFC3339 time offsets are `"Z"` (for UTC) or `"±hh:mm"`. ISO 8601
   --  additionally specifies `"±hhmm"` and `"±hh"`. Offsets are time values to
   --  be added to Coordinated Universal Time (UTC) to get the local time.
   --  In UNIX, the `TZ` environment variable typically corresponds to the
   --  name of a file in the zoneinfo database, an absolute path to a file
   --  somewhere else, or a string in "std offset [dst
   --  [offset],start[/time],end[/time]]" (POSIX) format. There are no spaces
   --  in the specification. The name of standard and daylight savings time
   --  zone must be three or more alphabetic characters. Offsets are time
   --  values to be added to local time to get Coordinated Universal Time (UTC)
   --  and should be `"[±]hh[[:]mm[:ss]]"`. Dates are either `"Jn"` (Julian day
   --  with n between 1 and 365, leap years not counted), `"n"` (zero-based
   --  Julian day with n between 0 and 365) or `"Mm.w.d"` (day d (0 <= d <= 6)
   --  of week w (1 <= w <= 5) of month m (1 <= m <= 12), day 0 is a Sunday).
   --  Times are in local wall clock time, the default is 02:00:00.
   --  In Windows, the "tzn[+|–]hh[:mm[:ss]][dzn]" format is used, but also
   --  accepts POSIX format. The Windows format uses US rules for all time
   --  zones; daylight savings time is 60 minutes behind the standard time with
   --  date and time of change taken from Pacific Standard Time. Offsets are
   --  time values to be added to the local time to get Coordinated Universal
   --  Time (UTC).
   --  g_time_zone_new_local calls this function with the value of the `TZ`
   --  environment variable. This function itself is independent of the value
   --  of `TZ`, but if Identifier is null then `/etc/localtime` will be
   --  consulted to discover the correct time zone on UNIX and the registry
   --  will be consulted or GetTimeZoneInformation will be used to get the
   --  local time zone on Windows.
   --  If intervals are not available, only time zone rules from `TZ`
   --  environment variable or other means, then they will be computed from
   --  year 1900 to 2037. If the maximum year for the rules is available and it
   --  is greater than 2037, then it will followed instead.
   --  See [RFC3339 §5.6](http://tools.ietf.org/html/rfc3339section-5.6) for a
   --  precise definition of valid RFC3339 time offsets (the `time-offset`
   --  expansion) and ISO 8601 for the full list of valid time offsets. See
   --  [The GNU C Library
   --  manual](http://www.gnu.org/s/libc/manual/html_node/TZ-Variable.html) for
   --  an explanation of the possible values of the `TZ` environment variable.
   --  See [Microsoft Time Zone Index
   --  Values](http://msdn.microsoft.com/en-us/library/ms912391%28v=winembedded.11%29.aspx)
   --  for the list of time zones on Windows.
   --  You should release the return value by calling Glib.Time_Zone.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.68
   --  @param Identifier a timezone identifier

   procedure G_New_Local (Self : out Gtime_Zone);
   --  Creates a Glib.Time_Zone.Gtime_Zone corresponding to local time. The
   --  local time zone may change between invocations to this function; for
   --  example, if the system administrator changes it.
   --  This is equivalent to calling g_time_zone_new with the value of the
   --  `TZ` environment variable (including the possibility of null).
   --  You should release the return value by calling Glib.Time_Zone.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.26

   function Gtime_Zone_New_Local return Gtime_Zone;
   --  Creates a Glib.Time_Zone.Gtime_Zone corresponding to local time. The
   --  local time zone may change between invocations to this function; for
   --  example, if the system administrator changes it.
   --  This is equivalent to calling g_time_zone_new with the value of the
   --  `TZ` environment variable (including the possibility of null).
   --  You should release the return value by calling Glib.Time_Zone.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.26

   procedure G_New_Offset (Self : out Gtime_Zone; Seconds : Gint32);
   --  Creates a Glib.Time_Zone.Gtime_Zone corresponding to the given constant
   --  offset from UTC, in seconds.
   --  This is equivalent to calling g_time_zone_new with a string in the form
   --  `[+|-]hh[:mm[:ss]]`.
   --  It is possible for this function to fail if Seconds is too big (greater
   --  than 24 hours), in which case this function will return the UTC timezone
   --  for backwards compatibility. To detect failures like this, use
   --  g_time_zone_new_identifier directly.
   --  Since: gtk+ 2.58
   --  @param Seconds offset to UTC, in seconds

   function Gtime_Zone_New_Offset (Seconds : Gint32) return Gtime_Zone;
   --  Creates a Glib.Time_Zone.Gtime_Zone corresponding to the given constant
   --  offset from UTC, in seconds.
   --  This is equivalent to calling g_time_zone_new with a string in the form
   --  `[+|-]hh[:mm[:ss]]`.
   --  It is possible for this function to fail if Seconds is too big (greater
   --  than 24 hours), in which case this function will return the UTC timezone
   --  for backwards compatibility. To detect failures like this, use
   --  g_time_zone_new_identifier directly.
   --  Since: gtk+ 2.58
   --  @param Seconds offset to UTC, in seconds

   procedure G_New_Utc (Self : out Gtime_Zone);
   --  Creates a Glib.Time_Zone.Gtime_Zone corresponding to UTC.
   --  This is equivalent to calling g_time_zone_new with a value like "Z",
   --  "UTC", "+00", etc.
   --  You should release the return value by calling Glib.Time_Zone.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.26

   function Gtime_Zone_New_Utc return Gtime_Zone;
   --  Creates a Glib.Time_Zone.Gtime_Zone corresponding to UTC.
   --  This is equivalent to calling g_time_zone_new with a value like "Z",
   --  "UTC", "+00", etc.
   --  You should release the return value by calling Glib.Time_Zone.Unref
   --  when you are done with it.
   --  Since: gtk+ 2.26

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_time_zone_get_type");

   -------------
   -- Methods --
   -------------

   function Adjust_Time
      (Self     : Gtime_Zone;
       The_Type : GTime_Type;
       Time     : in out Gint64) return Glib.Gint;
   --  Finds an interval within Tz that corresponds to the given Time_,
   --  possibly adjusting Time_ if required to fit into an interval. The
   --  meaning of Time_ depends on Type.
   --  This function is similar to Glib.Time_Zone.Find_Interval, with the
   --  difference that it always succeeds (by making the adjustments described
   --  below).
   --  In any of the cases where Glib.Time_Zone.Find_Interval succeeds then
   --  this function returns the same value, without modifying Time_.
   --  This function may, however, modify Time_ in order to deal with
   --  non-existent times. If the non-existent local Time_ of 02:30 were
   --  requested on March 14th 2010 in Toronto then this function would adjust
   --  Time_ to be 03:00 and return the interval containing the adjusted time.
   --  Since: gtk+ 2.26
   --  @param The_Type the Glib.Time_Zone.GTime_Type of Time_
   --  @param Time a pointer to a number of seconds since January 1, 1970
   --  @return the interval containing Time_, never -1

   function Find_Interval
      (Self     : Gtime_Zone;
       The_Type : GTime_Type;
       Time     : Gint64) return Glib.Gint;
   --  Finds an interval within Tz that corresponds to the given Time_. The
   --  meaning of Time_ depends on Type.
   --  If Type is Glib.Time_Zone.G_Time_Type_Universal then this function will
   --  always succeed (since universal time is monotonic and continuous).
   --  Otherwise Time_ is treated as local time. The distinction between
   --  Glib.Time_Zone.G_Time_Type_Standard and
   --  Glib.Time_Zone.G_Time_Type_Daylight is ignored except in the case that
   --  the given Time_ is ambiguous. In Toronto, for example, 01:30 on November
   --  7th 2010 occurred twice (once inside of daylight savings time and the
   --  next, an hour later, outside of daylight savings time). In this case,
   --  the different value of Type would result in a different interval being
   --  returned.
   --  It is still possible for this function to fail. In Toronto, for
   --  example, 02:00 on March 14th 2010 does not exist (due to the leap
   --  forward to begin daylight savings time). -1 is returned in that case.
   --  Since: gtk+ 2.26
   --  @param The_Type the Glib.Time_Zone.GTime_Type of Time_
   --  @param Time a number of seconds since January 1, 1970
   --  @return the interval containing Time_, or -1 in case of failure

   function Get_Abbreviation
      (Self     : Gtime_Zone;
       Interval : Glib.Gint) return UTF8_String;
   --  Determines the time zone abbreviation to be used during a particular
   --  Interval of time in the time zone Tz.
   --  For example, in Toronto this is currently "EST" during the winter
   --  months and "EDT" during the summer months when daylight savings time is
   --  in effect.
   --  Since: gtk+ 2.26
   --  @param Interval an interval within the timezone
   --  @return the time zone abbreviation, which belongs to Tz

   function Get_Identifier (Self : Gtime_Zone) return UTF8_String;
   --  Get the identifier of this Glib.Time_Zone.Gtime_Zone, as passed to
   --  g_time_zone_new. If the identifier passed at construction time was not
   --  recognised, `UTC` will be returned. If it was null, the identifier of
   --  the local timezone at construction time will be returned.
   --  The identifier will be returned in the same format as provided at
   --  construction time: if provided as a time offset, that will be returned
   --  by this function.
   --  Since: gtk+ 2.58
   --  @return identifier for this timezone

   function Get_Offset
      (Self     : Gtime_Zone;
       Interval : Glib.Gint) return Gint32;
   --  Determines the offset to UTC in effect during a particular Interval of
   --  time in the time zone Tz.
   --  The offset is the number of seconds that you add to UTC time to arrive
   --  at local time for Tz (ie: negative numbers for time zones west of GMT,
   --  positive numbers for east).
   --  Since: gtk+ 2.26
   --  @param Interval an interval within the timezone
   --  @return the number of seconds that should be added to UTC to get the
   --  local time in Tz

   function Is_Dst (Self : Gtime_Zone; Interval : Glib.Gint) return Boolean;
   --  Determines if daylight savings time is in effect during a particular
   --  Interval of time in the time zone Tz.
   --  Since: gtk+ 2.26
   --  @param Interval an interval within the timezone
   --  @return True if daylight savings time is in effect

   function Ref (Self : Gtime_Zone) return Gtime_Zone;
   --  Increases the reference count on Tz.
   --  Since: gtk+ 2.26
   --  @return a new reference to Tz. Has transfer-ownership='full'.

   procedure Unref (Self : Gtime_Zone);
   --  Decreases the reference count on Tz.
   --  Since: gtk+ 2.26

private
   Null_Gtime_Zone : constant Gtime_Zone :=
      (Glib.C_Boxed with null record);

end Glib.Time_Zone;
