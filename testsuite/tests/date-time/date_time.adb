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

   procedure Test_Zone_Intervals is
      Zone : Gtime_Zone := Gtime_Zone_New_Offset (7200);
      Copy : Gtime_Zone := Zone.Ref;
      Invalid : Gtime_Zone :=
        Gtime_Zone_New_Identifier ("Invalid/Not_A_Timezone");
      Time : Gint64 := 1234567890;
      Interval : Gint;
   begin
      Assert_True (Invalid = Null_Gtime_Zone);
      Interval := Zone.Adjust_Time (G_Time_Type_Universal, Time);
      --  A fixed offset needs no adjustment; the in-out value must survive.
      Assert_True (Time = 1234567890);
      Assert_Cmpint_Eq
        (Interval, Zone.Find_Interval (G_Time_Type_Universal, Time));
      Assert_Cmpint_Eq (Gint (Zone.Get_Offset (Interval)), 7200);
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
   Add_Func ("/date-time/arithmetic", Test_Arithmetic'Unrestricted_Access);
   Add_Func ("/date-time/timezones", Test_Timezones'Unrestricted_Access);
   Add_Func ("/date-time/zone-intervals",
             Test_Zone_Intervals'Unrestricted_Access);
   Add_Func ("/date-time/lifetime", Test_Lifetime'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Date_Time;
