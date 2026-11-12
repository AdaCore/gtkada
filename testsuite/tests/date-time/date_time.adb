--  Exercise the generated boxed date/time binding, without requiring GTK.
with Ada.Command_Line;
with Glib;           use Glib;
with Glib.Date_Time; use Glib.Date_Time;
with Glib.Time_Zone; use Glib.Time_Zone;
with Glib.Test;      use Glib.Test;

procedure Date_Time is
   procedure Test_Constructors with Convention => C;
   procedure Test_Arithmetic with Convention => C;
   procedure Test_Timezones with Convention => C;
   procedure Test_Time_Zone_Constructors with Convention => C;
   procedure Test_Lifetime with Convention => C;
   procedure Test_Zone_Intervals with Convention => C;

   procedure Test_Constructors is
      Epoch : Gdate_Time := Gdate_Time_New_From_Unix_Utc (0);
      Parsed : Gdate_Time := Gdate_Time_New_From_Iso8601
        ("2024-02-29T12:34:56.123456Z", Null_Gtime_Zone);
      Year, Month, Day : Gint;
      Fractional : Gdate_Time :=
        Gdate_Time_New_From_Unix_Utc_Usec (1_500_000);
      Invalid : Gdate_Time := Gdate_Time_New_From_Iso8601
        ("not-a-date", Null_Gtime_Zone);
   begin
      Assert_True (Epoch /= Null_Gdate_Time);
      Assert_Cmpint_Eq (Epoch.Get_Year, 1970);
      Assert_Cmpstr_Eq (Epoch.Format ("%Y-%m-%d %H:%M:%S"),
                        "1970-01-01 00:00:00");
      Assert_True (Parsed /= Null_Gdate_Time);
      Assert_Cmpint_Eq (Parsed.Get_Microsecond, 123456);
      Assert_Cmpstr_Eq (Parsed.Format_Iso8601,
                        "2024-02-29T12:34:56.123456Z");
      Parsed.Get_Ymd (Year, Month, Day);
      Assert_Cmpint_Eq (Year, 2024);
      Assert_Cmpint_Eq (Month, 2);
      Assert_Cmpint_Eq (Day, 29);
      Assert_True (Fractional.To_Unix_Usec = 1_500_000);
      Assert_True (Fractional.To_Unix = 1);
      Assert_True (Invalid = Null_Gdate_Time);
      Unref (Fractional);
      Unref (Parsed);
      Unref (Epoch);
   end Test_Constructors;

   procedure Test_Constructor_Overloads with Convention => C;
   procedure Test_Now_Constructors with Convention => C;
   procedure Test_Accessors_And_Additions with Convention => C;

   procedure Test_Constructor_Overloads is
      --  A fixed offset makes the timezone-aware constructor deterministic.
      Zone : Gtime_Zone := Gtime_Zone_New_Offset (7_200);
      By_Zone : Gdate_Time;
      By_Zone_Function : Gdate_Time := Gdate_Time_New
        (Zone, 2024, 3, 4, 5, 6, 7.25);
      Iso_Procedure : Gdate_Time;
      Local_Unix_Procedure : Gdate_Time;
      Local_Procedure : Gdate_Time;
      Local_Usec_Procedure : Gdate_Time;
      Utc_Unix_Procedure : Gdate_Time;
      Utc_Procedure : Gdate_Time;
      Utc_Usec_Procedure : Gdate_Time;
      Local_Function : Gdate_Time := Gdate_Time_New_Local
        (2024, 3, 4, 5, 6, 7.25);
      Local_Unix : Gdate_Time := Gdate_Time_New_From_Unix_Local (0);
      Local_Unix_Usec : Gdate_Time := Gdate_Time_New_From_Unix_Local_Usec
        (1_500_000);
      Utc_Unix_Usec : Gdate_Time := Gdate_Time_New_From_Unix_Utc_Usec
        (1_500_000);
      Utc_Function : Gdate_Time := Gdate_Time_New_Utc
        (2024, 3, 4, 5, 6, 7.25);
   begin
      --  Exercise the procedure constructor and its timezone argument.
      G_New (By_Zone, Zone, 2024, 3, 4, 5, 6, 7.25);
      Assert_True (Equal (By_Zone, By_Zone_Function));
      Assert_Cmpint_Eq (By_Zone.Get_Hour, 5);
      Assert_True (By_Zone.Get_Utc_Offset = 7_200_000_000);

      --  Procedure forms cover the same C entry points as their function
      --  overloads and should represent the same Unix instants.
      G_New_From_Iso8601
        (Iso_Procedure, "2024-03-04T05:06:07.25Z", Null_Gtime_Zone);
      G_New_From_Unix_Local (Local_Unix_Procedure, 0);
      G_New_From_Unix_Local_Usec (Local_Usec_Procedure, 1_500_000);
      G_New_From_Unix_Utc (Utc_Unix_Procedure, 0);
      G_New_From_Unix_Utc_Usec (Utc_Usec_Procedure, 1_500_000);
      --  The local calendar constructor is compared with its function form
      --  below, without assuming which timezone the test host uses.
      G_New_Local (Local_Procedure, 2024, 3, 4, 5, 6, 7.25);
      G_New_Utc (Utc_Procedure, 2024, 3, 4, 5, 6, 7.25);
      Assert_Cmpstr_Eq (Iso_Procedure.Format_Iso8601,
                        "2024-03-04T05:06:07.250000Z");
      Assert_True (Local_Unix.To_Unix = 0);
      Assert_True (Local_Unix_Procedure.To_Unix = 0);
      --  Both whole-second and microsecond Unix constructors preserve epoch.
      Assert_True (Local_Usec_Procedure.To_Unix_Usec = 1_500_000);
      Assert_True (Local_Unix_Usec.To_Unix_Usec = 1_500_000);
      Assert_True (Utc_Unix_Procedure.To_Unix = 0);
      Assert_Cmpint_Eq (Utc_Procedure.Get_Hour, 5);
      Assert_True (Utc_Usec_Procedure.To_Unix_Usec = 1_500_000);
      Assert_True (Utc_Unix_Usec.To_Unix_Usec = 1_500_000);
      Assert_True (Utc_Function.To_Unix = Utc_Procedure.To_Unix);
      --  Local time depends on the host timezone, so compare the two APIs
      --  only after both are converted to Unix time.
      Assert_True (Local_Function.To_Unix = Local_Procedure.To_Unix);

      Unref (Utc_Unix_Usec);
      Unref (Local_Unix_Usec);
      Unref (Local_Unix);
      Unref (Utc_Function);
      Unref (Local_Function);
      Unref (Utc_Usec_Procedure);
      Unref (Utc_Procedure);
      Unref (Utc_Unix_Procedure);
      Unref (Local_Usec_Procedure);
      Unref (Local_Procedure);
      Unref (Local_Unix_Procedure);
      Unref (Iso_Procedure);
      Unref (By_Zone_Function);
      Unref (By_Zone);
      Unref (Zone);
   end Test_Constructor_Overloads;

   procedure Test_Now_Constructors is
      --  Bracket every clock-based constructor with UTC Unix timestamps.
      Zone : Gtime_Zone := Gtime_Zone_New_Offset (3_600);
      Before_Date : Gdate_Time := Gdate_Time_New_Now_Utc;
      Before : constant Gint64 := Before_Date.To_Unix;
      By_Zone : Gdate_Time;
      By_Zone_Function : Gdate_Time := Gdate_Time_New_Now (Zone);
      Local_Procedure : Gdate_Time;
      Local_Function : Gdate_Time := Gdate_Time_New_Now_Local;
      Utc_Procedure : Gdate_Time;
      Utc_Function : Gdate_Time := Gdate_Time_New_Now_Utc;
      After_Date : Gdate_Time;
      After : Gint64;
   begin
      --  The system clock can advance during this test; bound each result by
      --  timestamps sampled around the calls rather than expecting one second.
      G_New_Now (By_Zone, Zone);
      G_New_Now_Local (Local_Procedure);
      G_New_Now_Utc (Utc_Procedure);
      After_Date := Gdate_Time_New_Now_Utc;
      After := After_Date.To_Unix;
      Assert_True (Before <= By_Zone.To_Unix and By_Zone.To_Unix <= After);
      Assert_True
        (Before <= By_Zone_Function.To_Unix
         and By_Zone_Function.To_Unix <= After);
      Assert_True
        (Before <= Local_Procedure.To_Unix
         and Local_Procedure.To_Unix <= After);
      Assert_True
        (Before <= Local_Function.To_Unix
         and Local_Function.To_Unix <= After);
      Assert_True (Before <= Utc_Procedure.To_Unix
                   and Utc_Procedure.To_Unix <= After);
      Assert_True (Before <= Utc_Function.To_Unix
                   and Utc_Function.To_Unix <= After);
      --  The supplied fixed-offset zone must be retained by the result.
      Assert_True (By_Zone.Get_Utc_Offset = 3_600_000_000);

      Unref (Utc_Function);
      Unref (Utc_Procedure);
      Unref (Local_Function);
      Unref (Local_Procedure);
      Unref (By_Zone_Function);
      Unref (By_Zone);
      Unref (After_Date);
      Unref (Before_Date);
      Unref (Zone);
   end Test_Now_Constructors;

   procedure Test_Accessors_And_Additions is
      Date : Gdate_Time := Gdate_Time_New_Utc
        (2021, 1, 1, 23, 45, 12.25);
      Y2024 : Gdate_Time := Gdate_Time_New_Utc
        (2024, 1, 10, 12, 30, 40.25);
      Zone : Gtime_Zone := Gtime_Zone_New_Offset (7_200);
      With_Zone : Gdate_Time := Date.To_Timezone (Zone);
      Local : Gdate_Time := Date.To_Local;
      One_Day : Gdate_Time := Y2024.Add_Days (1);
      Full : Gdate_Time := Y2024.Add_Full (0, 0, 0, 1, 1, 1.5);
      Hours : Gdate_Time := Y2024.Add_Hours (1);
      Minutes : Gdate_Time := Y2024.Add_Minutes (1);
      Months : Gdate_Time := Y2024.Add_Months (1);
      Seconds : Gdate_Time := Y2024.Add_Seconds (1.5);
      Weeks : Gdate_Time := Y2024.Add_Weeks (1);
      Years : Gdate_Time := Y2024.Add_Years (1);
      Year, Month, Day : Gint;
      Borrowed_Zone : Gtime_Zone := Date.Get_Timezone;
   begin
      --  Check the calendar and clock components, including ISO week-year
      --  behavior where January 1 belongs to the previous week-year.
      Assert_Cmpint_Eq (Date.Get_Day_Of_Month, 1);
      Assert_Cmpint_Eq (Date.Get_Day_Of_Week, 5);
      Assert_Cmpint_Eq (Date.Get_Day_Of_Year, 1);
      Assert_Cmpint_Eq (Date.Get_Hour, 23);
      Assert_Cmpint_Eq (Date.Get_Microsecond, 250_000);
      Assert_Cmpint_Eq (Date.Get_Minute, 45);
      Assert_Cmpint_Eq (Date.Get_Month, 1);
      Assert_Cmpint_Eq (Date.Get_Second, 12);
      Assert_True (Date.Get_Seconds >= 12.24 and Date.Get_Seconds <= 12.26);
      Assert_Cmpint_Eq (Date.Get_Week_Numbering_Year, 2020);
      Assert_Cmpint_Eq (Date.Get_Week_Of_Year, 53);
      Assert_Cmpint_Eq (Date.Get_Year, 2021);
      --  The multi-output call returns the same Gregorian date components.
      Date.Get_Ymd (Year, Month, Day);
      Assert_True (Year = 2021 and Month = 1 and Day = 1);
      Assert_Cmpstr_Eq (Date.Get_Timezone_Abbreviation, "UTC");
      Assert_False (Date.Is_Daylight_Savings);
      --  Get_Timezone is borrowed from Date and remains valid for this check.
      Assert_True (Borrowed_Zone.Get_Identifier = "UTC");

      --  Each modifier returns a new immutable date/time. Verify its changed
      --  component or elapsed time so the wrapper call itself is exercised.
      Assert_Cmpint_Eq (One_Day.Get_Day_Of_Month, 11);
      Assert_True (Full.Difference (Y2024) = 3_661_500_000);
      Assert_Cmpint_Eq (Hours.Get_Hour, 13);
      Assert_Cmpint_Eq (Minutes.Get_Minute, 31);
      Assert_Cmpint_Eq (Months.Get_Month, 2);
      Assert_True (Seconds.Difference (Y2024) = 1_500_000);
      Assert_Cmpint_Eq (Weeks.Get_Day_Of_Month, 17);
      Assert_Cmpint_Eq (Years.Get_Year, 2025);
      --  Zone conversions preserve the instant while changing local fields.
      Assert_True (Local.To_Unix = Date.To_Unix);
      Assert_Cmpint_Eq (With_Zone.Get_Hour, 1);
      Assert_True (With_Zone.Get_Utc_Offset = 7_200_000_000);

      Unref (Years);
      Unref (Weeks);
      Unref (Seconds);
      Unref (Months);
      Unref (Minutes);
      Unref (Hours);
      Unref (Full);
      Unref (One_Day);
      Unref (Local);
      Unref (With_Zone);
      Unref (Zone);
      Unref (Y2024);
      Unref (Date);
   end Test_Accessors_And_Additions;

   procedure Test_Arithmetic is
      Start : Gdate_Time := Gdate_Time_New_Utc (2024, 2, 28, 0, 0, 0.0);
      Next : Gdate_Time := Start.Add_Days (1);
      Later : Gdate_Time := Start.Add (1_500_000);
      Limit : Gdate_Time := Gdate_Time_New_Utc (9999, 12, 31, 0, 0, 0.0);
      Overflow : Gdate_Time := Limit.Add_Days (1);
   begin
      Assert_Cmpint_Eq (Start.Get_Day_Of_Month, 28);
      Assert_Cmpint_Eq (Next.Get_Day_Of_Month, 29);
      Assert_True (Later.Difference (Start) = 1_500_000);
      Assert_True (Start.Difference (Next) = -86_400_000_000);
      Assert_Cmpint_Eq (Later.Get_Microsecond, 500000);
      Assert_True (Compare (Start, Next) < 0);
      Assert_True (Compare (Next, Start) > 0);
      Assert_True (Equal (Start, Start));
      Assert_False (Equal (Start, Next));
      Assert_True (Overflow = Null_Gdate_Time);
      Unref (Start);
      Unref (Next);
      Unref (Later);
      Unref (Limit);
   end Test_Arithmetic;

   procedure Test_Timezones is
      Zone : Gtime_Zone := Gtime_Zone_New_Offset (7200);
      Utc : Gdate_Time := Gdate_Time_New_From_Unix_Utc (0);
      Local : Gdate_Time := Utc.To_Timezone (Zone);
      Borrowed : Gtime_Zone := Local.Get_Timezone;
      Roundtrip : Gdate_Time := Local.To_Utc;
   begin
      --  The datetime retains its timezone after the caller releases it.
      Unref (Zone);
      Assert_Cmpint_Eq (Local.Get_Hour, 2);
      Assert_True (Local.Get_Utc_Offset = 7_200_000_000);
      Assert_Cmpstr_Eq (Local.Format_Iso8601, "1970-01-01T02:00:00+02");
      Assert_True (Equal (Utc, Local));
      Assert_True (Equal (Utc, Roundtrip));
      Assert_Cmpuint_Eq (Utc.Hash, Local.Hash);
      Assert_Cmpstr_Eq (Borrowed.Get_Identifier, "+02:00:00");
      --  Get_Timezone borrows: do not unref Borrowed.
      Unref (Roundtrip);
      Unref (Local);
      Unref (Utc);
   end Test_Timezones;

   --  Exercise each GTimeZone constructor overload. Fixed offsets keep all
   --  expectations independent of the host's configured local timezone.
   procedure Test_Time_Zone_Constructors is
      By_New : Gtime_Zone;
      By_New_Empty : Gtime_Zone;
      By_New_Identifier : Gtime_Zone;
      By_New_Identifier_Empty : Gtime_Zone;
      By_Function : Gtime_Zone := Gtime_Zone_New ("+01:30");
      By_Function_Empty : Gtime_Zone := Gtime_Zone_New ("");
      By_Identifier_Function : Gtime_Zone :=
        Gtime_Zone_New_Identifier ("+01:30");
      By_Identifier_Function_Empty : Gtime_Zone :=
        Gtime_Zone_New_Identifier ("");
      Local_Procedure : Gtime_Zone;
      Local_Function : Gtime_Zone := Gtime_Zone_New_Local;
      Offset_Procedure : Gtime_Zone;
      Offset_Function : Gtime_Zone := Gtime_Zone_New_Offset (5_400);
      Utc_Procedure : Gtime_Zone;
      Utc_Function : Gtime_Zone := Gtime_Zone_New_Utc;
   begin
      --  Both identifier-based APIs accept a nonempty offset and the empty
      --  string (which requests the process-local zone).
      G_New (By_New, "+01:30");
      G_New (By_New_Empty, "");
      G_New_Identifier (By_New_Identifier, "+01:30");
      G_New_Identifier (By_New_Identifier_Empty, "");
      Assert_Cmpint_Eq (Gint (By_New.Get_Offset (0)), 5_400);
      Assert_Cmpint_Eq (Gint (By_Function.Get_Offset (0)), 5_400);
      Assert_Cmpint_Eq
        (Gint (By_New_Identifier.Get_Offset (0)), 5_400);
      Assert_Cmpint_Eq
        (Gint (By_Identifier_Function.Get_Offset (0)), 5_400);
      Assert_True (By_New_Empty /= Null_Gtime_Zone);
      Assert_True (By_Function_Empty /= Null_Gtime_Zone);
      Assert_True (By_New_Identifier_Empty /= Null_Gtime_Zone);
      Assert_True (By_Identifier_Function_Empty /= Null_Gtime_Zone);

      --  Procedure and function forms of the remaining constructors agree
      --  on stable identifiers or offsets.
      G_New_Local (Local_Procedure);
      G_New_Offset (Offset_Procedure, 5_400);
      G_New_Utc (Utc_Procedure);
      Assert_Cmpstr_Eq
        (Local_Procedure.Get_Identifier, Local_Function.Get_Identifier);
      Assert_Cmpint_Eq (Gint (Offset_Procedure.Get_Offset (0)), 5_400);
      Assert_Cmpint_Eq (Gint (Offset_Function.Get_Offset (0)), 5_400);
      Assert_Cmpstr_Eq (Utc_Procedure.Get_Identifier, "UTC");
      Assert_Cmpstr_Eq (Utc_Function.Get_Identifier, "UTC");

      --  Release every transfer-full result after checking it.
      Unref (Utc_Function);
      Unref (Utc_Procedure);
      Unref (Offset_Function);
      Unref (Offset_Procedure);
      Unref (Local_Function);
      Unref (Local_Procedure);
      Unref (By_Identifier_Function_Empty);
      Unref (By_Identifier_Function);
      Unref (By_Function_Empty);
      Unref (By_Function);
      Unref (By_New_Identifier_Empty);
      Unref (By_New_Identifier);
      Unref (By_New_Empty);
      Unref (By_New);
   end Test_Time_Zone_Constructors;

   --  Check interval lookup and all accessors on a deterministic UTC offset.
   procedure Test_Zone_Intervals is
      Zone : Gtime_Zone := Gtime_Zone_New_Offset (7200);
      Copy : Gtime_Zone := Zone.Ref;
      Invalid : Gtime_Zone :=
        Gtime_Zone_New_Identifier ("Invalid/Not_A_Timezone");
      Time : Gint64 := 1234567890;
      Interval : Gint;
      Local_Interval : Gint;
   begin
      Assert_True (Invalid = Null_Gtime_Zone);
      Interval := Zone.Adjust_Time (G_Time_Type_Universal, Time);
      --  A fixed offset needs no adjustment; the in-out value must survive.
      Assert_True (Time = 1234567890);
      Assert_Cmpint_Eq
        (Interval, Zone.Find_Interval (G_Time_Type_Universal, Time));
      Local_Interval := Zone.Find_Interval (G_Time_Type_Standard, Time);
      Assert_Cmpint_Eq (Local_Interval, Interval);
      Assert_Cmpint_Eq
        (Zone.Find_Interval (G_Time_Type_Daylight, Time), Interval);
      Assert_Cmpint_Eq (Gint (Zone.Get_Offset (Interval)), 7200);
      Assert_True (Zone.Get_Abbreviation (Interval) /= "");
      Assert_False (Zone.Is_Dst (Interval));
      Unref (Zone);
      Assert_Cmpstr_Eq (Copy.Get_Identifier, "+02:00:00");
      Unref (Copy);
   end Test_Zone_Intervals;

   procedure Test_Lifetime is
      Original : Gdate_Time := Gdate_Time_New_From_Unix_Utc (0);
      Retained : Gdate_Time := Original.Ref;
   begin
      Unref (Original);
      Assert_Cmpstr_Eq (Retained.Format ("%Y"), "1970");
      Unref (Retained);
   end Test_Lifetime;
begin
   Glib.Test.Init;
   Add_Func ("/date-time/constructors", Test_Constructors'Unrestricted_Access);
   Add_Func ("/date-time/constructor-overloads",
             Test_Constructor_Overloads'Unrestricted_Access);
   Add_Func ("/date-time/now-constructors",
             Test_Now_Constructors'Unrestricted_Access);
   Add_Func ("/date-time/accessors-and-additions",
             Test_Accessors_And_Additions'Unrestricted_Access);
   Add_Func ("/date-time/arithmetic", Test_Arithmetic'Unrestricted_Access);
   Add_Func ("/date-time/timezones", Test_Timezones'Unrestricted_Access);
   Add_Func ("/date-time/time-zone-constructors",
             Test_Time_Zone_Constructors'Unrestricted_Access);
   Add_Func ("/date-time/zone-intervals",
             Test_Zone_Intervals'Unrestricted_Access);
   Add_Func ("/date-time/lifetime", Test_Lifetime'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Date_Time;
