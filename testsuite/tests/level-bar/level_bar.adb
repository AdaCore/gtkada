--  Tests for the Gtk.Level_Bar binding: constructors, properties and
--  named offsets, including the offset-changed signal delivered from C.

with Ada.Command_Line;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Test;       use Glib.Test;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Level_Bar;   use Gtk.Level_Bar;
with Gtk.Main;

procedure Level_Bar is
   Changes : Gint := 0;

   procedure Offset_Changed
     (Self : access Gtk_Level_Bar_Record'Class; Name : UTF8_String) is
      Value : Gdouble;
   begin
      Assert_Cmpstr_Eq (Name, "custom");
      Assert_True (Self.Get_Offset_Value (Name, Value));
      Assert_Cmpfloat_Eq (Value, 0.5);
      Changes := Changes + 1;
   end Offset_Changed;

   procedure Test_Constructors with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Offsets with Convention => C;

   procedure Test_Constructors is
      Bar : Gtk_Level_Bar;
      Interval : constant Gtk_Level_Bar :=
        Gtk_Level_Bar_New_For_Interval (2.0, 8.0);
   begin
      Gtk_New (Bar);
      Ref_Sink (Bar);
      Ref_Sink (Interval);
      Assert_Cmpfloat_Eq (Bar.Get_Min_Value, 0.0);
      Assert_Cmpfloat_Eq (Bar.Get_Max_Value, 1.0);
      Assert_Cmpfloat_Eq (Bar.Get_Value, 0.0);
      Assert_True (Bar.Get_Mode = Level_Bar_Mode_Continuous);
      Assert_True (Bar.Get_Orientation = Orientation_Horizontal);
      Assert_False (Bar.Get_Inverted);
      Assert_Cmpfloat_Eq (Interval.Get_Min_Value, 2.0);
      Assert_Cmpfloat_Eq (Interval.Get_Max_Value, 8.0);
      Unref (Bar);
      Gtk_New_For_Interval (Bar, 1.0, 5.0);
      Ref_Sink (Bar);
      Assert_Cmpfloat_Eq (Bar.Get_Min_Value, 1.0);
      Assert_Cmpfloat_Eq (Bar.Get_Max_Value, 5.0);
      Unref (Bar);
      Unref (Interval);
   end Test_Constructors;

   procedure Test_Properties is
      Bar : constant Gtk_Level_Bar := Gtk_Level_Bar_New;
   begin
      Ref_Sink (Bar);
      Bar.Set_Max_Value (10.0);
      Bar.Set_Min_Value (2.0);
      Bar.Set_Value (7.5);
      Assert_Cmpfloat_Eq (Bar.Get_Min_Value, 2.0);
      Assert_Cmpfloat_Eq (Bar.Get_Max_Value, 10.0);
      Assert_Cmpfloat_Eq (Bar.Get_Value, 7.5);
      Assert_Cmpfloat_Eq (Get_Property (Bar, Value_Property), 7.5);
      Set_Property (Bar, Value_Property, 4.0);
      Assert_Cmpfloat_Eq (Bar.Get_Value, 4.0);
      for Mode in Gtk_Level_Bar_Mode loop
         Bar.Set_Mode (Mode);
         Assert_True (Bar.Get_Mode = Mode);
      end loop;
      Bar.Set_Orientation (Orientation_Vertical);
      Assert_True (Bar.Get_Orientation = Orientation_Vertical);
      Bar.Set_Inverted (True);
      Assert_True (Bar.Get_Inverted);
      Bar.Set_Inverted (False);
      Assert_False (Bar.Get_Inverted);
      Unref (Bar);
   end Test_Properties;

   procedure Test_Offsets is
      Bar : constant Gtk_Level_Bar := Gtk_Level_Bar_New;
      Value : Gdouble;
   begin
      Ref_Sink (Bar);
      Assert_True (Bar.Get_Offset_Value ("low", Value));
      Assert_Cmpfloat_Eq (Value, 0.25);
      Assert_True (Bar.Get_Offset_Value ("high", Value));
      Assert_Cmpfloat_Eq (Value, 0.75);
      Assert_True (Bar.Get_Offset_Value ("full", Value));
      Assert_Cmpfloat_Eq (Value, 1.0);
      Assert_False (Bar.Get_Offset_Value ("missing", Value));
      Bar.Add_Offset_Value ("custom", 0.4);
      Assert_True (Bar.Get_Offset_Value ("custom", Value));
      Assert_Cmpfloat_Eq (Value, 0.4);
      Changes := 0;
      Bar.On_Offset_Changed (Offset_Changed'Unrestricted_Access);
      Bar.Add_Offset_Value ("custom", 0.5);
      Assert_Cmpint_Eq (Changes, 1);
      Bar.Add_Offset_Value ("custom", 0.5);
      Assert_Cmpint_Eq (Changes, 1);
      Bar.Remove_Offset_Value ("custom");
      Assert_False (Bar.Get_Offset_Value ("custom", Value));
      Unref (Bar);
   end Test_Offsets;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/level-bar/constructors", Test_Constructors'Unrestricted_Access);
   Add_Func ("/level-bar/properties", Test_Properties'Unrestricted_Access);
   Add_Func ("/level-bar/offsets", Test_Offsets'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Level_Bar;
