--  Unit test for Gtk.Search_Entry.
--
--  GTK's own testsuite has no searchentry.c to port, so this exercises the
--  widget's own surface (the accessors, and the delayed "search-changed"
--  signal that distinguishes a search entry from a plain entry) plus a
--  sample of what it inherits from Gtk.Editable.

with Ada.Command_Line;
with Glib;             use Glib;
with Glib.Main;        use Glib.Main;
with Glib.Test;        use Glib.Test;
with Gtk.Button;       use Gtk.Button;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Main;
with Gtk.Search_Entry; use Gtk.Search_Entry;
with Gtk.Widget;       use Gtk.Widget;

procedure Search_Entry is

   Changed : Natural := 0;
   pragma Volatile (Changed);
   --  Incremented by the "search-changed" handler, which is dispatched from
   --  Main_Context_Iteration below.

   Done : Boolean := False;
   pragma Volatile (Done);
   --  Set from a timeout callback dispatched by Main_Context_Iteration

   procedure On_Changed (Self : access Gtk_Search_Entry_Record'Class);
   function Stop return Boolean;

   procedure Test_Accessors with Convention => C;
   procedure Test_Key_Capture_Widget with Convention => C;
   procedure Test_Editable with Convention => C;
   procedure Test_Search_Changed with Convention => C;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed (Self : access Gtk_Search_Entry_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Changed := Changed + 1;
   end On_Changed;

   ----------
   -- Stop --
   ----------

   function Stop return Boolean is
   begin
      Done := True;
      Wakeup (null);
      return False;
   end Stop;

   --------------------
   -- Test_Accessors --
   --------------------

   procedure Test_Accessors is
      E : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
   begin
      --  placeholder-text is nullable on both sides: the unset value comes
      --  back as the empty string rather than raising.
      Assert_Cmpstr_Eq (E.Get_Placeholder_Text, "");
      E.Set_Placeholder_Text ("Search");
      Assert_Cmpstr_Eq (E.Get_Placeholder_Text, "Search");
      E.Set_Placeholder_Text ("");
      Assert_Cmpstr_Eq (E.Get_Placeholder_Text, "");

      E.Set_Search_Delay (300);
      Assert_Cmpuint_Eq (E.Get_Search_Delay, 300);

      E.Set_Input_Purpose (Input_Purpose_Alpha);
      Assert_True (E.Get_Input_Purpose = Input_Purpose_Alpha);

      E.Set_Input_Hints (Input_Hint_No_Spellcheck);
      Assert_True (E.Get_Input_Hints = Input_Hint_No_Spellcheck);
   end Test_Accessors;

   ------------------------------
   -- Test_Key_Capture_Widget --
   ------------------------------

   procedure Test_Key_Capture_Widget is
      E      : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
      Button : constant Gtk_Button := Gtk_Button_New_With_Label;
   begin
      Assert_True (E.Get_Key_Capture_Widget = null);

      E.Set_Key_Capture_Widget (Button);
      Assert_True (E.Get_Key_Capture_Widget = Gtk_Widget (Button));

      --  The property is nullable, so the capture widget can be unset again.
      E.Set_Key_Capture_Widget (null);
      Assert_True (E.Get_Key_Capture_Widget = null);
   end Test_Key_Capture_Widget;

   -------------------
   -- Test_Editable --
   -------------------

   procedure Test_Editable is
      E             : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
      Start_Pos     : Gint;
      End_Pos       : Gint;
      Has_Selection : Boolean;
   begin
      E.Set_Text ("hello world");
      Assert_Cmpstr_Eq (E.Get_Text, "hello world");
      Assert_Cmpstr_Eq (E.Get_Chars (0, 5), "hello");
      Assert_Cmpstr_Eq (E.Get_Chars (6), "world");

      E.Select_Region (0, 5);
      E.Get_Selection_Bounds (Start_Pos, End_Pos, Has_Selection);
      Assert_True (Has_Selection);
      Assert_Cmpint_Eq (Glib.Gint'Pos (Start_Pos), 0);
      Assert_Cmpint_Eq (Glib.Gint'Pos (End_Pos), 5);

      E.Set_Editable (False);
      Assert_False (E.Get_Editable);
      E.Set_Editable (True);
      Assert_True (E.Get_Editable);

      E.Set_Width_Chars (12);
      Assert_Cmpint_Eq (Glib.Gint'Pos (E.Get_Width_Chars), 12);
   end Test_Editable;

   -------------------------
   -- Test_Search_Changed --
   -------------------------

   procedure Test_Search_Changed is
      E  : constant Gtk_Search_Entry := Gtk_Search_Entry_New;
      Id : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      --  "search-changed" is emitted only after search-delay milliseconds
      --  have elapsed without further input, so the main loop has to be
      --  spun for it to arrive at all.
      E.Set_Search_Delay (50);
      E.On_Search_Changed (On_Changed'Unrestricted_Access);

      Changed := 0;
      E.Set_Text ("needle");

      Done := False;
      Id := Timeout_Add (1000, Stop'Unrestricted_Access);

      while not Done and then Changed = 0 loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;

      Assert_Cmpuint_Eq (Guint (Changed), 1);
   end Test_Search_Changed;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/searchentry/accessors", Test_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchentry/key-capture-widget",
      Test_Key_Capture_Widget'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchentry/editable", Test_Editable'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/searchentry/search-changed", Test_Search_Changed'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Search_Entry;
