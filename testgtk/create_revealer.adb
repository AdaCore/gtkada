------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
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

with Gtk.Grid;          use Gtk.Grid;
with Gtk.Frame;         use Gtk.Frame;
with Gtk.Label;         use Gtk.Label;
with Gtk.GEntry;        use Gtk.GEntry;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Revealer;      use Gtk.Revealer;
with Gtk.Widget;        use Gtk.Widget;

package body Create_Revealer is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box      : Gtk_Grid;
      Label    : Gtk_Label;
      Button   : Gtk_Toggle_Button;
      Revealer : Gtk_Revealer;
      Ent      : Gtk_Entry;
   begin
      Frame.Set_Label ("Revealer");

      Gtk_New (Box);
      Frame.Add (Box);

      Gtk_New (Label,
               "The animations in this demo"
               & ASCII.LF & "were made very slow");
      Label.Set_Margin_Top (10);
      Label.Set_Margin_Bottom (10);
      Label.Set_Margin_Start (10);
      Label.Set_Margin_End (10);
      Box.Attach (Label, 1, 1, 1, 1);

      Gtk_New (Label,
               "The animations in this demo"
               & ASCII.LF & "were made very slow");
      Label.Set_Margin_Top (10);
      Label.Set_Margin_Bottom (10);
      Label.Set_Margin_Start (10);
      Label.Set_Margin_End (10);
      Box.Attach (Label, 3, 3, 1, 1);

      Gtk_New (Button, "None");
      Box.Attach (Button, 0, 0, 1, 1);
      Gtk_New (Revealer);
      Revealer.Set_Halign (Align_Start);
      Revealer.Set_Valign (Align_Start);
      Gtk_New (Ent);
      Ent.Set_Text ("00000");
      Revealer.Add (Ent);
      Button.Bind_Property ("active", Revealer, "reveal-child");
      Revealer.Set_Transition_Type (Revealer_Transition_Type_None);
      Revealer.Set_Transition_Duration (2000);
      Box.Attach (Revealer, 1, 0, 1, 1);

      Gtk_New (Button, "Fade");
      Box.Attach (Button, 4, 4, 1, 1);
      Gtk_New (Revealer);
      Revealer.Set_Halign (Align_End);
      Revealer.Set_Valign (Align_End);
      Gtk_New (Ent);
      Ent.Set_Text ("00000");
      Revealer.Add (Ent);
      Button.Bind_Property ("active", Revealer, "reveal-child");
      Revealer.Set_Transition_Type (Revealer_Transition_Type_Crossfade);
      Revealer.Set_Transition_Duration (2000);
      Box.Attach (Revealer, 3, 4, 1, 1);

      Gtk_New (Button, "Right");
      Box.Attach (Button, 0, 2, 1, 1);
      Gtk_New (Revealer);
      Revealer.Set_Hexpand (True);
      Revealer.Set_Halign (Align_Start);
      Gtk_New (Ent);
      Ent.Set_Text ("12345");
      Revealer.Add (Ent);
      Button.Bind_Property ("active", Revealer, "reveal-child");
      Revealer.Set_Transition_Type (Revealer_Transition_Type_Slide_Right);
      Revealer.Set_Transition_Duration (2000);
      Box.Attach (Revealer, 1, 2, 1, 1);

      Gtk_New (Button, "Down");
      Box.Attach (Button, 2, 0, 1, 1);
      Gtk_New (Revealer);
      Revealer.Set_Vexpand (True);
      Revealer.Set_Valign (Align_Start);
      Gtk_New (Ent);
      Ent.Set_Text ("23456");
      Revealer.Add (Ent);
      Button.Bind_Property ("active", Revealer, "reveal-child");
      Revealer.Set_Transition_Type (Revealer_Transition_Type_Slide_Down);
      Revealer.Set_Transition_Duration (2000);
      Box.Attach (Revealer, 2, 1, 1, 1);

      Gtk_New (Button, "Left");
      Box.Attach (Button, 4, 2, 1, 1);
      Gtk_New (Revealer);
      Revealer.Set_Hexpand (True);
      Revealer.Set_Halign (Align_End);
      Gtk_New (Ent);
      Ent.Set_Text ("34567");
      Revealer.Add (Ent);
      Button.Bind_Property ("active", Revealer, "reveal-child");
      Revealer.Set_Transition_Type (Revealer_Transition_Type_Slide_Left);
      Revealer.Set_Transition_Duration (2000);
      Box.Attach (Revealer, 3, 2, 1, 1);

      Gtk_New (Button, "Up");
      Box.Attach (Button, 2, 4, 1, 1);
      Gtk_New (Revealer);
      Revealer.Set_Vexpand (True);
      Revealer.Set_Valign (Align_End);
      Gtk_New (Ent);
      Ent.Set_Text ("45678");
      Revealer.Add (Ent);
      Button.Bind_Property ("active", Revealer, "reveal-child");
      Revealer.Set_Transition_Type (Revealer_Transition_Type_Slide_Up);
      Revealer.Set_Transition_Duration (2000);
      Box.Attach (Revealer, 2, 3, 1, 1);

      Frame.Show_All;
   end Run;

end Create_Revealer;
