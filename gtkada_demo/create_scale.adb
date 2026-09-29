------------------------------------------------------------------------------
--               GtkAda - Ada12 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2026, AdaCore                     --
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

with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Glib;              use Glib;

with Gtk.Adjustment;    use Gtk.Adjustment;
with Gtk.Box;           use Gtk.Box;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Grid;          use Gtk.Grid;
with Gtk.GRange;        use Gtk.GRange;
with Gtk.Label;         use Gtk.Label;
with Gtk.Scale;         use Gtk.Scale;
with Gtk.Widget;        use Gtk.Widget;

package body Create_Scale is

   Value_Label : Gtk_Label;
   --  Shows the value of the "Synchronised" scales.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Scale@B lets the user select a value in an interval"
        & " by dragging a slider. It is a @bGtk_Range@B, so the value,"
        & " the interval and the increments are those of its"
        & " @bGtk_Adjustment@B."
        & ASCII.LF
        & "The scales in the left column show the options: whether and"
        & " where to draw the value (@bSet_Draw_Value@B,"
        & " @bSet_Value_Pos@B, @bSet_Digits@B), how to format it"
        & " (@bSet_Format_Value_Func@B), marks along the trough"
        & " (@bAdd_Mark@B) and an inverted range (@bSet_Inverted@B)."
        & ASCII.LF
        & "The two scales below share one adjustment, so moving one moves"
        & " the other with no callback, and the label follows through the"
        & " @bvalue-changed@B signal.";
   end Help;

   -------------
   -- Percent --
   -------------

   function Percent
     (Scale : not null access Gtk_Scale_Record'Class;
      Value : Gdouble) return UTF8_String
   is
      pragma Unreferenced (Scale);
   begin
      return Trim (Gint'Image (Gint (Value)), Left) & " %";
   end Percent;

   -------------------
   -- Value_Changed --
   -------------------

   procedure Value_Changed (Self : access Gtk_Range_Record'Class) is
   begin
      Value_Label.Set_Text
        ("Value: " & Trim (Gint'Image (Gint (Self.Get_Value)), Left));
   end Value_Changed;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Main_Box : Gtk_Box;
      Grid     : Gtk_Grid;
      Label    : Gtk_Label;
      Scale    : Gtk_Scale;
      Vertical : Gtk_Scale;
      Adj      : Gtk_Adjustment;

      procedure Add_Row
        (Row   : Gint;
         Title : String;
         Scale : not null access Gtk_Scale_Record'Class);
      --  Put a title and a scale on Row of Grid.

      procedure Add_Row
        (Row   : Gint;
         Title : String;
         Scale : not null access Gtk_Scale_Record'Class) is
      begin
         Gtk_New (Label, Title);
         Label.Set_Halign (Align_Start);
         Grid.Attach (Label, 0, Row);

         Scale.Set_Hexpand (True);
         Scale.Set_Size_Request (200, -1);
         Grid.Attach (Scale, 1, Row);
      end Add_Row;

   begin
      Frame.Set_Label ("Scales");

      Gtk_New (Main_Box, Orientation_Horizontal, 20);
      Main_Box.Set_Margin_Start (10);
      Main_Box.Set_Margin_End (10);
      Main_Box.Set_Margin_Top (10);
      Main_Box.Set_Margin_Bottom (10);
      Frame.Set_Child (Main_Box);

      Gtk_New (Grid);
      Grid.Set_Row_Spacing (10);
      Grid.Set_Column_Spacing (10);
      Grid.Set_Hexpand (True);
      Main_Box.Append (Grid);

      --  No value shown: the slider only
      Gtk_New_With_Range (Scale, Orientation_Horizontal, 0.0, 100.0, 1.0);
      Add_Row (0, "Plain", Scale);

      --  The value above the slider, with the precision the step implies
      Gtk_New_With_Range (Scale, Orientation_Horizontal, 0.0, 1.0, 0.1);
      Scale.Set_Draw_Value (True);
      Scale.Set_Value (0.5);
      Add_Row (1, "Value on top", Scale);

      --  The value on the right, formatted by the application
      Gtk_New_With_Range (Scale, Orientation_Horizontal, 0.0, 100.0, 1.0);
      Scale.Set_Draw_Value (True);
      Scale.Set_Value_Pos (Pos_Right);
      Scale.Set_Format_Value_Func (Percent'Access);
      Scale.Set_Value (25.0);
      Add_Row (2, "Formatted", Scale);

      --  Marks above and below the trough, some with a (markup) text
      Gtk_New_With_Range (Scale, Orientation_Horizontal, 0.0, 100.0, 10.0);
      Scale.Add_Mark (0.0, Pos_Bottom, "<small>0</small>");
      Scale.Add_Mark (50.0, Pos_Bottom, "<small>half</small>");
      Scale.Add_Mark (100.0, Pos_Bottom, "<small>100</small>");
      Scale.Add_Mark (25.0, Pos_Top);
      Scale.Add_Mark (75.0, Pos_Top);
      Add_Row (3, "Marks", Scale);

      --  Higher values on the left
      Gtk_New_With_Range (Scale, Orientation_Horizontal, 0.0, 100.0, 1.0);
      Scale.Set_Inverted (True);
      Scale.Set_Has_Origin (False);
      Add_Row (4, "Inverted", Scale);

      --  Two scales on the same adjustment
      Gtk_New (Adj, 50.0, 0.0, 100.0, 1.0, 10.0, 0.0);
      Gtk_New (Scale, Orientation_Horizontal, Adj);
      Scale.Set_Draw_Value (True);
      Scale.Set_Digits (0);
      Scale.On_Value_Changed (Value_Changed'Access);
      Add_Row (5, "Synchronised", Scale);

      Gtk_New (Vertical, Orientation_Vertical, Adj);
      Vertical.Set_Draw_Value (True);
      Vertical.Set_Digits (0);
      Vertical.Set_Inverted (True);
      Vertical.Set_Vexpand (True);
      Vertical.Add_Mark (0.0, Pos_Right, "<small>0</small>");
      Vertical.Add_Mark (100.0, Pos_Right, "<small>100</small>");
      Main_Box.Append (Vertical);

      Gtk_New (Value_Label, "Value: 50");
      Value_Label.Set_Halign (Align_Start);
      Grid.Attach (Value_Label, 1, 6);
   end Run;

end Create_Scale;
