--  Ada port of GTK's testsuite/gtk/adjustment.c (GTK 4.22.2).
--
--  Gtk.Adjustment is a pure data model, so the whole suite runs without a
--  realized window: it exercises the constructor and every getter/setter
--  (basic), the changed/value-changed emission counts (signals), value
--  clamping to the adjustment range (clamp) and Clamp_Page (clamp_page).
--  All four C test cases are ported.

with Glib;             use Glib;
with Glib.Test;        use Glib.Test;
with Ada.Command_Line;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Main;

procedure Adjustment is

   Changed_Count       : Gint := 0;
   Value_Changed_Count : Gint := 0;

   procedure Test_Basic
   with Convention => C;

   procedure Test_Signals
   with Convention => C;

   procedure Test_Clamp
   with Convention => C;

   procedure Test_Clamp_Page
   with Convention => C;

   procedure Changed_Cb (Self : access Gtk_Adjustment_Record'Class);
   procedure Value_Changed_Cb (Self : access Gtk_Adjustment_Record'Class);

   procedure Changed_Cb (Self : access Gtk_Adjustment_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Changed_Count := Changed_Count + 1;
   end Changed_Cb;

   procedure Value_Changed_Cb (Self : access Gtk_Adjustment_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Value_Changed_Count := Value_Changed_Count + 1;
   end Value_Changed_Cb;

   procedure Test_Basic is
      A : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (2.0, 0.0, 100.0, 1.0, 5.0, 10.0);
   begin
      Assert_Cmpfloat_Eq (A.Get_Value, 2.0);
      Assert_Cmpfloat_Eq (A.Get_Lower, 0.0);
      Assert_Cmpfloat_Eq (A.Get_Upper, 100.0);
      Assert_Cmpfloat_Eq (A.Get_Step_Increment, 1.0);
      Assert_Cmpfloat_Eq (A.Get_Page_Increment, 5.0);
      Assert_Cmpfloat_Eq (A.Get_Page_Size, 10.0);
      Assert_Cmpfloat_Eq (A.Get_Minimum_Increment, 1.0);

      A.Set_Value (50.0);
      A.Set_Lower (20.0);
      A.Set_Upper (75.5);
      A.Set_Step_Increment (2.2);
      A.Set_Page_Increment (1.5);
      A.Set_Page_Size (10.0);

      Assert_Cmpfloat_Eq (A.Get_Value, 50.0);
      Assert_Cmpfloat_Eq (A.Get_Lower, 20.0);
      Assert_Cmpfloat_Eq (A.Get_Upper, 75.5);
      Assert_Cmpfloat_Eq (A.Get_Step_Increment, 2.2);
      Assert_Cmpfloat_Eq (A.Get_Page_Increment, 1.5);
      Assert_Cmpfloat_Eq (A.Get_Page_Size, 10.0);
      Assert_Cmpfloat_Eq (A.Get_Minimum_Increment, 1.5);
   end Test_Basic;

   procedure Test_Signals is
      A : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
   begin
      A.On_Changed (Changed_Cb'Unrestricted_Access);
      A.On_Value_Changed (Value_Changed_Cb'Unrestricted_Access);

      Changed_Count := 0;
      Value_Changed_Count := 0;
      A.Configure (0.0, 0.0, 100.0, 1.0, 5.0, 0.0);
      Assert_Cmpint_Eq (Changed_Count, 1);
      Assert_Cmpint_Eq (Value_Changed_Count, 0);

      Changed_Count := 0;
      Value_Changed_Count := 0;
      A.Set_Value (50.0);
      A.Set_Lower (20.0);
      A.Set_Upper (75.5);
      A.Set_Step_Increment (2.2);
      A.Set_Page_Increment (1.5);
      A.Set_Page_Size (10.0);
      Assert_Cmpint_Eq (Changed_Count, 5);
      Assert_Cmpint_Eq (Value_Changed_Count, 1);
   end Test_Signals;

   procedure Test_Clamp is
      A : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (2.0, 0.0, 100.0, 1.0, 5.0, 10.0);
   begin
      A.Set_Value (-10.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 0.0);

      A.Set_Value (200.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 90.0);

      A.Set_Value (99.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 90.0);

      A.Configure (0.0, 0.0, 10.0, 1.0, 5.0, 10.0);

      A.Set_Value (5.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 0.0);
   end Test_Clamp;

   procedure Test_Clamp_Page is
      A : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (20.0, 0.0, 100.0, 1.0, 5.0, 10.0);
   begin
      A.Clamp_Page (50.0, 55.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 45.0);

      A.Clamp_Page (52.0, 58.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 48.0);

      A.Clamp_Page (48.0, 50.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 48.0);

      A.Clamp_Page (30.0, 50.0);
      Assert_Cmpfloat_Eq (A.Get_Value, 30.0);
   end Test_Clamp_Page;

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/adjustment/basic", Test_Basic'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/adjustment/signals", Test_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/adjustment/clamp", Test_Clamp'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/adjustment/clamp_page", Test_Clamp_Page'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Adjustment;
