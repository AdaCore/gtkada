------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                    Copyright (C) 1998-2026, AdaCore                      --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with Glib;                    use Glib;
with Glib.Main;               use Glib.Main;
with Glib.Object;             use Glib.Object;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Check_Button;        use Gtk.Check_Button;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.GEntry;              use Gtk.GEntry;
with Gtk.Label;               use Gtk.Label;
with Gtk.Search_Bar;          use Gtk.Search_Bar;
with Gtk.Search_Entry;        use Gtk.Search_Entry;
with Gtk.Separator;           use Gtk.Separator;
with Gtk.Spin_Button;         use Gtk.Spin_Button;
with Gtk.Toggle_Button;       use Gtk.Toggle_Button;
with Gtk.Widget;              use Gtk.Widget;

package body Create_Entry is

   --  The widgets a callback acts upon are held at library level: the Gtk4
   --  signal handlers below receive only the widget that emitted the signal,
   --  so a check button cannot otherwise reach the entry it governs.

   The_Entry    : Gtk_Entry;
   Pulsed       : Gtk_Entry;
   Fractional   : Gtk_Entry;
   Search       : Gtk_Search_Entry;
   Matches      : Gtk_Label;
   Status       : Gtk_Label;
   Bar          : Gtk_Search_Bar;

   Timer1, Timer2 : G_Source_Id := 0;
   --  The sources driving the two progress entries. Stored here so that
   --  Stop_Timers can cancel them when the demo is swapped out.

   Candidates : constant array (Positive range <>) of Unbounded_String :=
     (To_Unbounded_String ("Apricot"),
      To_Unbounded_String ("Blackcurrant"),
      To_Unbounded_String ("Cherry"),
      To_Unbounded_String ("Damson"),
      To_Unbounded_String ("Elderberry"),
      To_Unbounded_String ("Gooseberry"),
      To_Unbounded_String ("Greengage"),
      To_Unbounded_String ("Quince"),
      To_Unbounded_String ("Redcurrant"),
      To_Unbounded_String ("Rhubarb"));
   --  The little corpus the search entry searches through, to make the
   --  delayed "search-changed" signal visible.

   Current_Pattern : Unbounded_String;
   --  The lower-cased text of the last "search-changed", i.e. what the
   --  candidates are matched against.

   Current_Match : Natural := 0;
   --  Index in Candidates of the highlighted match, or 0 when the pattern
   --  matches nothing (in particular when it is empty).

   procedure Toggle_Editable (Self : access Gtk_Check_Button_Record'Class);
   procedure Toggle_Overwrite (Self : access Gtk_Check_Button_Record'Class);
   procedure Toggle_Sensitive (Self : access Gtk_Check_Button_Record'Class);
   procedure Toggle_Visibility (Self : access Gtk_Check_Button_Record'Class);
   procedure Toggle_Search_Mode (Self : access Gtk_Toggle_Button_Record'Class);

   function Find_Match (From : Natural; Forward : Boolean) return Natural;
   procedure Show_Candidates;
   procedure Report_Match (Signal : String);

   procedure Search_Changed (Self : access Gtk_Search_Entry_Record'Class);
   procedure Search_Started (Self : access Gtk_Search_Entry_Record'Class);
   procedure Next_Match (Self : access Gtk_Search_Entry_Record'Class);
   procedure Previous_Match (Self : access Gtk_Search_Entry_Record'Class);
   procedure Stop_Search (Self : access Gtk_Search_Entry_Record'Class);

   procedure Delay_Changed (Self : access Gtk_Spin_Button_Record'Class);
   procedure Stop_Timers (Object : access GObject_Record'Class);

   function Pulse_Timeout return Boolean;
   function Fractional_Timeout return Boolean;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo shows the three text-entry widgets of Gtk4."
        & ASCII.LF
        & "The first two @bGtk_Entry@B widgets carry an embedded progress"
        & " indicator, pulsed and fractional respectively. The third is a"
        & " plain @bGtk_Entry@B whose @bEditable@B, @bOverwrite@B,"
        & " @bVisible@B and @bSensitive@B settings can be toggled with the"
        & " check buttons below it."
        & ASCII.LF
        & "A @bGtk_Search_Entry@B looks like an entry but emits"
        & " @bsearch-changed@B only once the user has stopped typing for"
        & " @bSearch_Delay@B milliseconds, which is what makes it suitable"
        & " for driving a live search. Raise the delay with the spin button"
        & " to feel the difference. Here it searches a small list of fruits,"
        & " shown in full below it with the current match in bold."
        & ASCII.LF
        & "It also offers the keybinding signals @bnext-match@B (Ctrl+G),"
        & " @bprevious-match@B (Ctrl+Shift+G) and @bstop-search@B (Escape),"
        & " reported below the list. The first two move the bold highlight"
        & " to the next and previous match, wrapping around at either end."
        & ASCII.LF
        & "A @bGtk_Search_Bar@B is the revealer that usually holds such an"
        & " entry. It is hidden until its search mode is switched on, either"
        & " with the toggle button or simply by typing: the bar is given this"
        & " demo's frame as its @bkey capture widget@B, so any keystroke"
        & " landing outside another entry reveals it.";
   end Help;

   ---------------------
   -- Toggle_Editable --
   ---------------------

   procedure Toggle_Editable (Self : access Gtk_Check_Button_Record'Class) is
   begin
      The_Entry.Set_Editable (Self.Get_Active);
   end Toggle_Editable;

   ----------------------
   -- Toggle_Overwrite --
   ----------------------

   procedure Toggle_Overwrite (Self : access Gtk_Check_Button_Record'Class) is
   begin
      The_Entry.Set_Overwrite_Mode (Self.Get_Active);
   end Toggle_Overwrite;

   ----------------------
   -- Toggle_Sensitive --
   ----------------------

   procedure Toggle_Sensitive (Self : access Gtk_Check_Button_Record'Class) is
   begin
      The_Entry.Set_Sensitive (Self.Get_Active);
   end Toggle_Sensitive;

   -----------------------
   -- Toggle_Visibility --
   -----------------------

   procedure Toggle_Visibility (Self : access Gtk_Check_Button_Record'Class) is
   begin
      The_Entry.Set_Visibility (Self.Get_Active);
   end Toggle_Visibility;

   ------------------------
   -- Toggle_Search_Mode --
   ------------------------

   procedure Toggle_Search_Mode
     (Self : access Gtk_Toggle_Button_Record'Class) is
   begin
      Bar.Set_Search_Mode (Self.Get_Active);
   end Toggle_Search_Mode;

   ----------------
   -- Find_Match --
   ----------------

   function Find_Match (From : Natural; Forward : Boolean) return Natural is
      Count : constant Integer := Candidates'Length;
      Start : constant Integer :=
        (if From = 0 and then not Forward then Candidates'First else From);
      --  Searching backwards from "no match" wraps to the last candidate;
      --  searching forwards from it naturally starts at the first one.

      Index : Integer;
   begin
      if Current_Pattern = Null_Unbounded_String then
         return 0;
      end if;

      --  Walk the whole corpus once, cyclically, so that the search wraps
      --  around at either end.

      for Offset in 1 .. Count loop
         Index :=
           (if Forward then Start - 1 + Offset else Start - 1 - Offset)
             mod Count + Candidates'First;

         if Ada.Strings.Fixed.Index
              (To_Lower (To_String (Candidates (Index))),
               To_String (Current_Pattern)) /= 0
         then
            return Index;
         end if;
      end loop;

      return 0;
   end Find_Match;

   ---------------------
   -- Show_Candidates --
   ---------------------

   procedure Show_Candidates is
      Markup : Unbounded_String;
   begin
      --  All the candidates stay listed: only the current match is shown in
      --  bold, through Pango markup. The corpus is plain ASCII, so it needs
      --  no markup escaping.

      for Index in Candidates'Range loop
         if Index /= Candidates'First then
            Append (Markup, ", ");
         end if;

         if Index = Current_Match then
            Append (Markup, "<b>" & Candidates (Index) & "</b>");
         else
            Append (Markup, Candidates (Index));
         end if;
      end loop;

      Matches.Set_Markup (To_String (Markup));
   end Show_Candidates;

   ------------------
   -- Report_Match --
   ------------------

   procedure Report_Match (Signal : String) is
   begin
      if Current_Match = 0 then
         Status.Set_Text (Signal & ": no match");
      else
         Status.Set_Text
           (Signal & ": " & To_String (Candidates (Current_Match)));
      end if;
   end Report_Match;

   --------------------
   -- Search_Changed --
   --------------------

   procedure Search_Changed (Self : access Gtk_Search_Entry_Record'Class) is
   begin
      Current_Pattern := To_Unbounded_String (To_Lower (Self.Get_Text));

      --  A new pattern restarts the navigation on its first match

      Current_Match := Find_Match (From => 0, Forward => True);
      Show_Candidates;
      Report_Match ("search-changed");
   end Search_Changed;

   --------------------
   -- Search_Started --
   --------------------

   procedure Search_Started (Self : access Gtk_Search_Entry_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Status.Set_Text ("search-started");
   end Search_Started;

   ----------------
   -- Next_Match --
   ----------------

   procedure Next_Match (Self : access Gtk_Search_Entry_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Current_Match := Find_Match (From => Current_Match, Forward => True);
      Show_Candidates;
      Report_Match ("next-match (Ctrl+G)");
   end Next_Match;

   --------------------
   -- Previous_Match --
   --------------------

   procedure Previous_Match (Self : access Gtk_Search_Entry_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Current_Match := Find_Match (From => Current_Match, Forward => False);
      Show_Candidates;
      Report_Match ("previous-match (Ctrl+Shift+G)");
   end Previous_Match;

   -----------------
   -- Stop_Search --
   -----------------

   procedure Stop_Search (Self : access Gtk_Search_Entry_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Status.Set_Text ("stop-search (Escape)");
   end Stop_Search;

   -------------------
   -- Delay_Changed --
   -------------------

   procedure Delay_Changed (Self : access Gtk_Spin_Button_Record'Class) is
   begin
      Search.Set_Search_Delay (Guint (Self.Get_Value_As_Int));
   end Delay_Changed;

   -------------------
   -- Pulse_Timeout --
   -------------------

   function Pulse_Timeout return Boolean is
   begin
      Pulsed.Progress_Pulse;
      return True;
   end Pulse_Timeout;

   ------------------------
   -- Fractional_Timeout --
   ------------------------

   function Fractional_Timeout return Boolean is
      Progress : Gdouble := Fractional.Get_Progress_Fraction + 0.005;
   begin
      if Progress > 1.0 then
         Progress := 0.0;
      end if;

      Fractional.Set_Progress_Fraction (Progress);
      return True;
   end Fractional_Timeout;

   -----------------
   -- Stop_Timers --
   -----------------

   procedure Stop_Timers (Object : access GObject_Record'Class) is
      pragma Unreferenced (Object);
   begin
      if Timer1 /= 0 then
         Remove (Timer1);
         Timer1 := 0;
      end if;

      if Timer2 /= 0 then
         Remove (Timer2);
         Timer2 := 0;
      end if;
   end Stop_Timers;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box       : Gtk_Box;
      Row       : Gtk_Box;
      Separator : Gtk_Separator;
      Check     : Gtk_Check_Button;
      Toggle    : Gtk_Toggle_Button;
      Spin      : Gtk_Spin_Button;
      Label     : Gtk_Label;
      Bar_Entry : Gtk_Search_Entry;

      procedure Add_Check
        (Caption : String;
         Handler : Cb_Gtk_Check_Button_Void;
         Active  : Boolean);
      --  Append a check button governing The_Entry, initially in state
      --  Active. The handler is connected after the initial state is set, so
      --  that setting it does not itself fire the callback.

      ---------------
      -- Add_Check --
      ---------------

      procedure Add_Check
        (Caption : String;
         Handler : Cb_Gtk_Check_Button_Void;
         Active  : Boolean) is
      begin
         Check := Gtk_Check_Button_New_With_Label (Caption);
         Check.Set_Active (Active);
         Check.On_Toggled (Handler);
         Box.Append (Check);
      end Add_Check;

   begin
      Frame.Set_Label ("Entry");

      Gtk_New (Box, Orientation_Vertical, Spacing => 10);
      Box.Set_Homogeneous (False);
      Box.Set_Margin_Start (10);
      Box.Set_Margin_End (10);
      Box.Set_Margin_Top (10);
      Box.Set_Margin_Bottom (10);
      Frame.Set_Child (Box);

      --  A plain entry, with an embedded progress indicator

      Pulsed := Gtk_Entry_New;
      Pulsed.Set_Text ("Pulsed progress");
      Pulsed.Set_Editable (False);
      Timer1 := Timeout_Add (100, Pulse_Timeout'Access);
      Box.Append (Pulsed);

      Fractional := Gtk_Entry_New;
      Fractional.Set_Text ("Fractional progress");
      Fractional.Set_Editable (False);
      Timer2 := Timeout_Add (20, Fractional_Timeout'Access);
      Box.Append (Fractional);

      --  A plain entry, with its settings exposed as check buttons

      The_Entry := Gtk_Entry_New;
      The_Entry.Set_Text ("Hello world");
      Box.Append (The_Entry);

      Add_Check ("Editable", Toggle_Editable'Access, True);
      Add_Check ("Overwrite", Toggle_Overwrite'Access, False);
      Add_Check ("Visible", Toggle_Visibility'Access, True);
      Add_Check ("Sensitive", Toggle_Sensitive'Access, True);

      Gtk_New (Separator, Orientation_Horizontal);
      Box.Append (Separator);

      --  A search entry, filtering Candidates

      Search := Gtk_Search_Entry_New;
      Search.Set_Placeholder_Text ("Search a fruit");
      Search.Set_Search_Delay (200);
      Search.On_Search_Changed (Search_Changed'Access);
      Search.On_Search_Started (Search_Started'Access);
      Search.On_Next_Match (Next_Match'Access);
      Search.On_Previous_Match (Previous_Match'Access);
      Search.On_Stop_Search (Stop_Search'Access);
      Box.Append (Search);

      Gtk_New (Row, Orientation_Horizontal, Spacing => 10);
      Row.Set_Homogeneous (False);
      Gtk_New (Label, "Search delay (ms):");
      Row.Append (Label);
      Spin := Gtk_Spin_Button_New_With_Range (0.0, 2000.0, 100.0);
      Spin.Set_Value (200.0);
      Spin.On_Value_Changed (Delay_Changed'Access);
      Row.Append (Spin);
      Box.Append (Row);

      Matches := Gtk_Label_New ("");
      Matches.Set_Xalign (0.0);
      Matches.Set_Wrap (True);
      Box.Append (Matches);

      Status := Gtk_Label_New ("");
      Status.Set_Xalign (0.0);
      Box.Append (Status);

      --  Show the full list, with nothing highlighted, to start with
      Show_Candidates;

      Gtk_New (Separator, Orientation_Horizontal);
      Box.Append (Separator);

      --  A search bar, revealing a search entry of its own

      Bar := Gtk_Search_Bar_New;
      Bar.Set_Show_Close_Button (True);

      Bar_Entry := Gtk_Search_Entry_New;
      Bar_Entry.Set_Placeholder_Text ("Type to search");
      Bar.Set_Child (Bar_Entry);
      Bar.Connect_Entry (+Bar_Entry);

      --  Typing anywhere in the demo reveals the bar and starts the search
      Bar.Set_Key_Capture_Widget (Frame);
      Box.Append (Bar);

      Toggle := Gtk_Toggle_Button_New_With_Label ("Show the search bar");
      Toggle.On_Toggled (Toggle_Search_Mode'Access);
      Box.Append (Toggle);

      --  Stop the timers when the demo is swapped out, so that the periodic
      --  callbacks never reference a destroyed entry.
      Box.On_Destroy (Stop_Timers'Access, Slot => Box);
   end Run;

end Create_Entry;
