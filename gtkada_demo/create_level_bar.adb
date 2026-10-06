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

with Gtk.Box;           use Gtk.Box;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Label;         use Gtk.Label;
with Gtk.Level_Bar;     use Gtk.Level_Bar;
with Gtk.Scale;         use Gtk.Scale;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;

package body Create_Level_Bar is
   function Help return String is
   begin
      return "A @bGtk_Level_Bar@B displays a value within an interval."
        & " Move the scale to compare continuous and discrete modes."
        & " The named @blow@B, @bhigh@B and @bfull@B offsets select the"
        & " fill's style as the value crosses each threshold."
        & ASCII.LF
        & "The bars share the scale's value through @bBind_Property@B."
        & " The toggle reverses their fill direction with @bSet_Inverted@B.";
   end Help;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box      : Gtk_Box;
      Scale    : Gtk_Scale;
      Bar      : Gtk_Level_Bar;
      Inverted : Gtk_Toggle_Button;
   begin
      Frame.Set_Label ("Level Bars");
      Gtk_New (Box, Orientation_Vertical, 12);
      Box.Set_Margin_Start (12);
      Box.Set_Margin_End (12);
      Box.Set_Margin_Top (12);
      Box.Set_Margin_Bottom (12);
      Frame.Set_Child (Box);

      Gtk_New_With_Range (Scale, Orientation_Horizontal, 0.0, 10.0, 0.1);
      Scale.Set_Draw_Value (True);
      Box.Append (Scale);
      Gtk_New_With_Label (Inverted, "Inverted");

      for Mode in Gtk_Level_Bar_Mode loop
         if Mode = Level_Bar_Mode_Continuous then
            Box.Append (Gtk_Label_New ("Continuous"));
         else
            Box.Append (Gtk_Label_New ("Discrete"));
         end if;
         Gtk_New_For_Interval (Bar, 0.0, 10.0);
         Bar.Set_Mode (Mode);
         Bar.Add_Offset_Value ("low", 3.0);
         Bar.Add_Offset_Value ("high", 8.0);
         Bar.Add_Offset_Value ("full", 10.0);
         Box.Append (Bar);
         Scale.Get_Adjustment.Bind_Property ("value", Bar, "value");
         Inverted.Bind_Property ("active", Bar, "inverted");
      end loop;
      Box.Append (Inverted);
      Scale.Set_Value (5.0);
   end Run;
end Create_Level_Bar;
