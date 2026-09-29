------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2010-2026, AdaCore                     --
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

with Glib;              use Glib;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Frame;         use Gtk.Frame;
with Gtk.GEntry;        use Gtk.GEntry;
with Gtk.Grid;          use Gtk.Grid;
with Gtk.Label;         use Gtk.Label;
with Gtk.Revealer;      use Gtk.Revealer;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Widget;        use Gtk.Widget;

package body Create_Revealer is

   Duration : constant Guint := 2000;
   --  Slow enough to see what each transition does

   procedure Add_Revealer
     (Grid        : not null access Gtk_Grid_Record'Class;
      Title       : String;
      Text        : String;
      Transition  : Gtk_Revealer_Transition_Type;
      Button_Col  : Gint;
      Button_Row  : Gint;
      Col         : Gint;
      Row         : Gint;
      Halign      : Gtk_Align := Align_Fill;
      Valign      : Gtk_Align := Align_Fill;
      Hexpand     : Boolean := False;
      Vexpand     : Boolean := False);
   --  Put a toggle button labelled Title at (Button_Col, Button_Row), and at
   --  (Col, Row) a revealer showing an entry holding Text, which the button
   --  reveals with the given transition.

   ------------------
   -- Add_Revealer --
   ------------------

   procedure Add_Revealer
     (Grid        : not null access Gtk_Grid_Record'Class;
      Title       : String;
      Text        : String;
      Transition  : Gtk_Revealer_Transition_Type;
      Button_Col  : Gint;
      Button_Row  : Gint;
      Col         : Gint;
      Row         : Gint;
      Halign      : Gtk_Align := Align_Fill;
      Valign      : Gtk_Align := Align_Fill;
      Hexpand     : Boolean := False;
      Vexpand     : Boolean := False)
   is
      Button   : constant Gtk_Toggle_Button :=
        Gtk_Toggle_Button_New_With_Label (Title);
      Revealer : constant Gtk_Revealer := Gtk_Revealer_New;
      Ent      : constant Gtk_Entry := Gtk_Entry_New;
   begin
      Grid.Attach (Button, Button_Col, Button_Row);

      Ent.Set_Text (Text);
      Revealer.Set_Child (Ent);
      Revealer.Set_Halign (Halign);
      Revealer.Set_Valign (Valign);
      Revealer.Set_Hexpand (Hexpand);
      Revealer.Set_Vexpand (Vexpand);
      Revealer.Set_Transition_Type (Transition);
      Revealer.Set_Transition_Duration (Duration);
      Grid.Attach (Revealer, Col, Row);

      Button.Bind_Property ("active", Revealer, "reveal-child");
   end Add_Revealer;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Revealer@B animates showing or hiding its single"
        & " child."
        & ASCII.LF
        & "Toggle each button below to reveal its entry, using a different"
        & " @bGtk_Revealer_Transition_Type@B (none, crossfade, or slide"
        & " from each side). The transitions here are slowed to 2 seconds"
        & " so you can see them clearly."
        & ASCII.LF
        & "Each button is tied to its revealer by binding the button's"
        & " @bactive@B property to the revealer's @breveal-child@B, so no"
        & " signal handler is needed.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Grid : constant Gtk_Grid := Gtk_Grid_New;

      procedure Add_Note (Col, Row : Gint);
      procedure Add_Note (Col, Row : Gint) is
         Note : constant Gtk_Label :=
           Gtk_Label_New
             ("The animations in this demo" & ASCII.LF & "were made very slow");
      begin
         Note.Set_Margin_Top (10);
         Note.Set_Margin_Bottom (10);
         Note.Set_Margin_Start (10);
         Note.Set_Margin_End (10);
         Grid.Attach (Note, Col, Row);
      end Add_Note;
   begin
      Frame.Set_Label ("Revealer");
      Frame.Set_Child (Grid);

      Add_Note (1, 1);
      Add_Note (3, 3);

      Add_Revealer
        (Grid, "None", "00000", None,
         Button_Col => 0, Button_Row => 0, Col => 1, Row => 0,
         Halign => Align_Start, Valign => Align_Start);
      Add_Revealer
        (Grid, "Fade", "00000", Crossfade,
         Button_Col => 4, Button_Row => 4, Col => 3, Row => 4,
         Halign => Align_End, Valign => Align_End);
      Add_Revealer
        (Grid, "Right", "12345", Slide_Right,
         Button_Col => 0, Button_Row => 2, Col => 1, Row => 2,
         Halign => Align_Start, Hexpand => True);
      Add_Revealer
        (Grid, "Down", "23456", Slide_Down,
         Button_Col => 2, Button_Row => 0, Col => 2, Row => 1,
         Valign => Align_Start, Vexpand => True);
      Add_Revealer
        (Grid, "Left", "34567", Slide_Left,
         Button_Col => 4, Button_Row => 2, Col => 3, Row => 2,
         Halign => Align_End, Hexpand => True);
      Add_Revealer
        (Grid, "Up", "45678", Slide_Up,
         Button_Col => 2, Button_Row => 4, Col => 2, Row => 3,
         Valign => Align_End, Vexpand => True);
   end Run;

end Create_Revealer;
