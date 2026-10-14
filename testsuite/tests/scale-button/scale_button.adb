--  Exercise string-array marshalling, shared adjustments, inherited volume
--  button methods and the double argument of value-changed.
with Ada.Command_Line;
with GNAT.Strings;
with Gtk.Widget;        use Gtk.Widget;
with Glib;              use Glib;
with Glib.Properties;   use Glib.Properties;
with Glib.Test;         use Glib.Test;
with Gtk.Adjustment;    use Gtk.Adjustment;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Main;
with Gtk.Scale;         use Gtk.Scale;
with Gtk.Scale_Button;  use Gtk.Scale_Button;
with Gtk.Scrollbar;     use Gtk.Scrollbar;
with Gtk.Volume_Button; use Gtk.Volume_Button;

procedure Scale_Button is
   Calls : Gint := 0;
   Seen  : Gdouble := 0.0;

   procedure Changed
     (Self : access Gtk_Scale_Button_Record'Class; Value : Gdouble) is
   begin
      Assert_Cmpfloat_Eq (Self.Get_Value, Value);
      Calls := Calls + 1;
      Seen := Value;
   end Changed;

   procedure Test_Shared_Adjustment with Convention => C;
   procedure Test_Volume with Convention => C;

   procedure Test_Shared_Adjustment is
      B : Gtk_Scale_Button;
      S : Gtk_Scale;
      R : Gtk_Scrollbar;
      A : Gtk_Adjustment;
      Icons : GNAT.Strings.String_List :=
        (new String'("audio-volume-muted-symbolic"),
         new String'("audio-volume-high-symbolic"),
         new String'("audio-volume-low-symbolic"));
   begin
      Gtk_New (B, 0.0, 100.0, 2.0, Icons);
      B.Ref_Sink;
      A := B.Get_Adjustment;
      Assert_Cmpfloat_Eq (A.Get_Upper, 100.0);
      Assert_Cmpfloat_Eq (A.Get_Step_Increment, 2.0);
      B.Set_Icons (Icons (2 .. 2));
      for Icon of Icons loop
         GNAT.Strings.Free (Icon);
      end loop;
      B.Set_Icons ((1 .. 0 => null));
      Gtk_New (A, 20.0, 0.0, 100.0, 1.0, 10.0, 0.0);
      B.Set_Adjustment (A);
      Assert_True (B.Get_Adjustment = A);
      Gtk_New (S, Orientation_Horizontal, A);
      S.Ref_Sink;
      Gtk_New (R, Orientation_Horizontal, A);
      R.Ref_Sink;
      Assert_True (R.Get_Adjustment = A);
      B.On_Value_Changed (Changed'Unrestricted_Access);
      Calls := 0;
      S.Set_Value (40.0);
      Assert_Cmpfloat_Eq (B.Get_Value, 40.0);
      Assert_Cmpfloat_Eq (Seen, 40.0);
      Assert_Cmpint_Eq (Calls, 1);
      B.Set_Value (40.0);
      Assert_Cmpint_Eq (Calls, 1);
      B.Set_Value (200.0);
      Assert_Cmpfloat_Eq (Seen, 100.0);
      Assert_Cmpfloat_Eq (R.Get_Adjustment.Get_Value, 100.0);
      Assert_Cmpfloat_Eq (S.Get_Value, 100.0);
      Assert_Cmpint_Eq (Calls, 2);
      B.Set_Orientation (Orientation_Horizontal);
      Assert_True (B.Get_Orientation = Orientation_Horizontal);
      Assert_True (B.Get_Plus_Button /= null);
      Assert_True (B.Get_Minus_Button /= null);
      Assert_True (B.Get_Popup /= null);
      B.Unref;
      S.Unref;
      R.Unref;
   end Test_Shared_Adjustment;

   procedure Test_Volume is
      V : Gtk_Volume_Button;
      B : Gtk_Scale_Button;
   begin
      Gtk_New (V);
      V.Ref_Sink;
      Assert_Cmpfloat_Eq (V.Get_Adjustment.Get_Lower, 0.0);
      Assert_Cmpfloat_Eq (V.Get_Adjustment.Get_Upper, 1.0);
      Assert_Cmpfloat_Eq (V.Get_Adjustment.Get_Step_Increment, 0.02);
      V.Set_Value (0.5);
      Assert_Cmpfloat_Eq (V.Get_Value, 0.5);
      Set_Property (V, Use_Symbolic_Property, False);
      Assert_False (Get_Property (V, Use_Symbolic_Property));
      V.Unref;
      --  Nullable icons are also accepted by the scale button constructor.
      Gtk_New (B, 0.0, 1.0, 0.1, (1 .. 0 => null));
      B.Ref_Sink;
      B.Set_Value (0.75);
      Assert_Cmpfloat_Eq (B.Get_Value, 0.75);
      B.Unref;
   end Test_Volume;

begin
   Gtk.Main.Init;
   Glib.Test.Init;
   Add_Func ("/scale-button/shared-adjustment",
             Test_Shared_Adjustment'Unrestricted_Access);
   Add_Func ("/scale-button/volume", Test_Volume'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Scale_Button;
