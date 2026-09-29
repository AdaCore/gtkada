--  Tests for Gtk.Scale and its parent Gtk.GRange (GtkRange).
--
--  GTK has no C test of its own for GtkScale, so this is written directly
--  against the Ada API. Only the formatting tests use a window: the rest
--  runs on scales that are never shown. It covers the constructors (with
--  a range, with a shared adjustment), the property
--  accessors of both classes, the adjustment being shared between the range
--  and its owner, the value-changed signal, marks and Set_Format_Value_Func
--  (both flavours: GTK calls the formatter and shows what it returns, on a
--  scale in a window as well, and the user data of the second one is handed
--  back and destroyed).

with Glib;             use Glib;
with Glib.Main;        use Glib.Main;
with Glib.Properties;  use Glib.Properties;
with Glib.Test;        use Glib.Test;
with Ada.Command_Line;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.GRange;       use Gtk.GRange;
with Gtk.Main;
with Gtk.Scale;        use Gtk.Scale;
with Gtk.Window;       use Gtk.Window;

procedure Scale is

   Value_Changed_Count : Gint := 0;

   Format_Calls   : Natural := 0;
   --  How many times GTK called a formatter

   Destroyed      : Natural := 0;
   Last_Destroyed : Unbounded_String;
   --  How many times, and for which data, the user data of the formatter
   --  was destroyed

   procedure Test_New_With_Range
   with Convention => C;

   procedure Test_New_With_Adjustment
   with Convention => C;

   procedure Test_Scale_Accessors
   with Convention => C;

   procedure Test_Range_Accessors
   with Convention => C;

   procedure Test_Set_Range
   with Convention => C;

   procedure Test_Value_Changed
   with Convention => C;

   procedure Test_Marks
   with Convention => C;

   procedure Test_Format_Value_Func
   with Convention => C;

   procedure Test_Format_Drawn
   with Convention => C;

   procedure Test_Format_User_Data
   with Convention => C;

   procedure Value_Changed_Cb (Self : access Gtk_Range_Record'Class);

   function Format (Scale : not null access Gtk_Scale_Record'Class;
                    Value : Gdouble) return UTF8_String;

   function Format_With_Data
     (Scale : not null access Gtk_Scale_Record'Class;
      Value : Gdouble;
      Data  : String) return UTF8_String;

   procedure Note_Destroyed (Data : in out String);

   package Formatter is new Set_Format_Value_Func_User_Data
     (String, Note_Destroyed);

   function Image (Value : Gdouble) return String
   is (Trim (Gint'Image (Gint (Value)), Left));

   Timed_Out : Boolean := False;
   pragma Volatile (Timed_Out);
   --  Set by Stop, from a timeout dispatched by Main_Context_Iteration

   function Stop return Boolean;

   procedure Pump;
   --  Run the main loop for a moment, to let GTK realize, lay out and draw
   --  what it has to: the frame clock works from its own timer.

   function Shown_Text (S : not null access Gtk_Scale_Record'Class)
     return String;
   --  The text GTK draws for the value of S

   procedure Value_Changed_Cb (Self : access Gtk_Range_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Value_Changed_Count := Value_Changed_Count + 1;
   end Value_Changed_Cb;

   function Format (Scale : not null access Gtk_Scale_Record'Class;
                    Value : Gdouble) return UTF8_String
   is
      pragma Unreferenced (Scale);
   begin
      Format_Calls := Format_Calls + 1;
      return "<" & Image (Value) & ">";
   end Format;

   function Format_With_Data
     (Scale : not null access Gtk_Scale_Record'Class;
      Value : Gdouble;
      Data  : String) return UTF8_String
   is
      pragma Unreferenced (Scale);
   begin
      Format_Calls := Format_Calls + 1;
      return Data & ":" & Image (Value);
   end Format_With_Data;

   procedure Note_Destroyed (Data : in out String) is
   begin
      Destroyed := Destroyed + 1;
      Last_Destroyed := To_Unbounded_String (Data);
   end Note_Destroyed;

   function Stop return Boolean is
   begin
      Timed_Out := True;
      Wakeup (null);
      return False;
   end Stop;

   procedure Pump is
      Id : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      Timed_Out := False;
      Id := Timeout_Add (300, Stop'Unrestricted_Access);
      while not Timed_Out loop
         if Main_Context_Iteration (null, May_Block => True) then
            null;
         end if;
      end loop;
   end Pump;

   function Shown_Text (S : not null access Gtk_Scale_Record'Class)
     return String is
   begin
      return S.Get_Layout.Get_Text;
   end Shown_Text;

   procedure Test_New_With_Range is
      S : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);

      Assert_True (S /= null);
      Assert_Cmpfloat_Eq (S.Get_Adjustment.Get_Lower, 0.0);
      Assert_Cmpfloat_Eq (S.Get_Adjustment.Get_Upper, 100.0);
      Assert_Cmpfloat_Eq (S.Get_Adjustment.Get_Step_Increment, 1.0);
      Assert_True (S.Get_Orientation = Orientation_Horizontal);
      Assert_Cmpfloat_Eq (S.Get_Value, 0.0);

      --  The number of digits is derived from the step: 1.0 needs none,
      --  0.1 needs one.
      Assert_Cmpint_Eq (S.Get_Digits, 0);
      Gtk_New_With_Range (S, Orientation_Vertical, 0.0, 1.0, 0.1);
      Assert_Cmpint_Eq (S.Get_Digits, 1);
      Assert_True (S.Get_Orientation = Orientation_Vertical);
   end Test_New_With_Range;

   procedure Test_New_With_Adjustment is
      Adj : constant Gtk_Adjustment :=
        Gtk_Adjustment_New (5.0, 0.0, 20.0, 1.0, 5.0, 0.0);
      S   : Gtk_Scale;
   begin
      Gtk_New (S, Orientation_Horizontal, Adj);
      Assert_True (S.Get_Adjustment = Adj);
      Assert_Cmpfloat_Eq (S.Get_Value, 5.0);

      --  The adjustment is shared, whichever side changes the value.
      S.Set_Value (7.0);
      Assert_Cmpfloat_Eq (Adj.Get_Value, 7.0);
      Adj.Set_Value (9.0);
      Assert_Cmpfloat_Eq (S.Get_Value, 9.0);

      --  A null adjustment makes the scale create its own.
      Gtk_New (S, Orientation_Vertical, null);
      Assert_True (S.Get_Adjustment /= null);
      Assert_True (S.Get_Adjustment /= Adj);

      S.Set_Adjustment (Adj);
      Assert_True (S.Get_Adjustment = Adj);
   end Test_New_With_Adjustment;

   procedure Test_Scale_Accessors is
      S : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 10.0, 1.0);

      --  Defaults of the GtkScale properties.
      Assert_False (S.Get_Draw_Value);
      Assert_True (S.Get_Has_Origin);
      Assert_True (S.Get_Value_Pos = Pos_Top);

      S.Set_Digits (3);
      Assert_Cmpint_Eq (S.Get_Digits, 3);
      Assert_Cmpint_Eq (Get_Property (S, The_Digits_Property), 3);

      S.Set_Draw_Value (True);
      Assert_True (S.Get_Draw_Value);
      S.Set_Draw_Value (False);
      Assert_False (S.Get_Draw_Value);

      S.Set_Has_Origin (False);
      Assert_False (S.Get_Has_Origin);
      S.Set_Has_Origin (True);
      Assert_True (S.Get_Has_Origin);

      for Pos in Gtk_Position_Type loop
         S.Set_Value_Pos (Pos);
         Assert_True (S.Get_Value_Pos = Pos);
      end loop;
   end Test_Scale_Accessors;

   procedure Test_Range_Accessors is
      S : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);

      S.Set_Inverted (True);
      Assert_True (S.Get_Inverted);
      S.Set_Inverted (False);
      Assert_False (S.Get_Inverted);

      S.Set_Flippable (False);
      Assert_False (S.Get_Flippable);
      S.Set_Flippable (True);
      Assert_True (S.Get_Flippable);

      S.Set_Round_Digits (2);
      Assert_Cmpint_Eq (S.Get_Round_Digits, 2);

      S.Set_Fill_Level (40.0);
      Assert_Cmpfloat_Eq (S.Get_Fill_Level, 40.0);

      S.Set_Show_Fill_Level (True);
      Assert_True (S.Get_Show_Fill_Level);
      S.Set_Show_Fill_Level (False);
      Assert_False (S.Get_Show_Fill_Level);

      S.Set_Restrict_To_Fill_Level (False);
      Assert_False (S.Get_Restrict_To_Fill_Level);

      S.Set_Slider_Size_Fixed (True);
      Assert_True (S.Get_Slider_Size_Fixed);
      S.Set_Slider_Size_Fixed (False);
      Assert_False (S.Get_Slider_Size_Fixed);

      S.Set_Increments (2.0, 25.0);
      Assert_Cmpfloat_Eq (S.Get_Adjustment.Get_Step_Increment, 2.0);
      Assert_Cmpfloat_Eq (S.Get_Adjustment.Get_Page_Increment, 25.0);
   end Test_Range_Accessors;

   procedure Test_Set_Range is
      S : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);
      S.Set_Value (50.0);

      --  Narrowing the range clamps the value into it.
      S.Set_Range (60.0, 80.0);
      Assert_Cmpfloat_Eq (S.Get_Adjustment.Get_Lower, 60.0);
      Assert_Cmpfloat_Eq (S.Get_Adjustment.Get_Upper, 80.0);
      Assert_Cmpfloat_Eq (S.Get_Value, 60.0);

      S.Set_Value (1000.0);
      Assert_Cmpfloat_Eq (S.Get_Value, 80.0);
      S.Set_Value (-1000.0);
      Assert_Cmpfloat_Eq (S.Get_Value, 60.0);
   end Test_Set_Range;

   procedure Test_Value_Changed is
      S : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);
      On_Value_Changed (S, Value_Changed_Cb'Unrestricted_Access);

      Value_Changed_Count := 0;
      S.Set_Value (10.0);
      Assert_Cmpint_Eq (Value_Changed_Count, 1);

      --  No change of value, no signal.
      S.Set_Value (10.0);
      Assert_Cmpint_Eq (Value_Changed_Count, 1);

      --  The adjustment is the source of the change, the range relays it.
      S.Get_Adjustment.Set_Value (20.0);
      Assert_Cmpint_Eq (Value_Changed_Count, 2);
   end Test_Value_Changed;

   procedure Test_Marks is
      S : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);

      S.Add_Mark (0.0, Pos_Bottom);
      S.Add_Mark (50.0, Pos_Bottom, "<b>half</b>");
      S.Add_Mark (100.0, Pos_Top, "full");
      S.Clear_Marks;
      S.Add_Mark (25.0, Pos_Top);
      S.Clear_Marks;
   end Test_Marks;

   procedure Test_Format_Value_Func is
      S : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);
      S.Set_Draw_Value (True);
      S.Set_Value (42.0);

      --  Without a function the value is rounded to the digits of the scale.
      Assert_Cmpstr_Eq (Shown_Text (S), "42");

      Format_Calls := 0;
      S.Set_Format_Value_Func (Format'Unrestricted_Access);
      Assert_Cmpstr_Eq (Shown_Text (S), "<42>");
      Assert_True (Format_Calls > 0);

      --  The value reaches the function, and the returned string is taken
      --  over by GTK, which frees it each time the text is replaced.
      for V in 0 .. 20 loop
         S.Set_Value (Gdouble (V));
         Assert_Cmpstr_Eq (Shown_Text (S), "<" & Image (Gdouble (V)) & ">");
      end loop;

      --  Clearing the function goes back to the default.
      S.Set_Format_Value_Func (null);
      Assert_Cmpstr_Eq (Shown_Text (S), "20");
   end Test_Format_Value_Func;

   procedure Test_Format_Drawn is
      Window : constant Gtk_Window := Gtk_Window_New;
      S      : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);
      S.Set_Draw_Value (True);
      S.Set_Value (63.0);
      S.Set_Format_Value_Func (Format'Unrestricted_Access);

      Window.Set_Child (S);
      Window.Present;
      Pump;
      Assert_Cmpstr_Eq (Shown_Text (S), "<63>");

      --  GTK4 draws the value with an internal label, which is given the
      --  text the function returns each time the value changes.
      Format_Calls := 0;
      S.Set_Value (80.0);
      Pump;
      Assert_True (Format_Calls > 0);
      Assert_Cmpstr_Eq (Shown_Text (S), "<80>");

      Window.Destroy;
   end Test_Format_Drawn;

   procedure Test_Format_User_Data is
      Window : constant Gtk_Window := Gtk_Window_New;
      S      : Gtk_Scale;
   begin
      Gtk_New_With_Range (S, Orientation_Horizontal, 0.0, 100.0, 1.0);
      S.Set_Draw_Value (True);
      S.Set_Value (42.0);
      Window.Set_Child (S);

      Destroyed := 0;
      Last_Destroyed := Null_Unbounded_String;

      Formatter.Set_Format_Value_Func
        (S, Format_With_Data'Unrestricted_Access, "first");
      Assert_Cmpstr_Eq (Shown_Text (S), "first:42");
      Assert_Cmpint_Eq (Gint (Destroyed), 0);

      --  Replacing the function destroys the data of the old one, once.
      Formatter.Set_Format_Value_Func
        (S, Format_With_Data'Unrestricted_Access, "second");
      Assert_Cmpint_Eq (Gint (Destroyed), 1);
      Assert_Cmpstr_Eq (To_String (Last_Destroyed), "first");
      Assert_Cmpstr_Eq (Shown_Text (S), "second:42");

      --  So does clearing it.
      S.Set_Format_Value_Func (null);
      Assert_Cmpint_Eq (Gint (Destroyed), 2);
      Assert_Cmpstr_Eq (To_String (Last_Destroyed), "second");
      Assert_Cmpstr_Eq (Shown_Text (S), "42");

      --  And so does destroying the scale.
      Formatter.Set_Format_Value_Func
        (S, Format_With_Data'Unrestricted_Access, "third");
      Window.Present;
      Pump;
      Assert_Cmpint_Eq (Gint (Destroyed), 2);

      Window.Destroy;
      Pump;
      Assert_Cmpint_Eq (Gint (Destroyed), 3);
      Assert_Cmpstr_Eq (To_String (Last_Destroyed), "third");
   end Test_Format_User_Data;

begin
   Glib.Test.Init;

   --  The C tests use gtk_test_init, which also initializes GTK.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/scale/new_with_range", Test_New_With_Range'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/new_with_adjustment",
      Test_New_With_Adjustment'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/scale_accessors", Test_Scale_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/range_accessors", Test_Range_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/set_range", Test_Set_Range'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/value_changed", Test_Value_Changed'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/marks", Test_Marks'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/format_value_func",
      Test_Format_Value_Func'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/format_drawn", Test_Format_Drawn'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/scale/format_user_data", Test_Format_User_Data'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Scale;
