------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
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
------------------------------------------------------------------------------

with GNAT.Strings;

with Glib;              use Glib;
with Gtk.Adjustment;    use Gtk.Adjustment;
with Gtk.Box;           use Gtk.Box;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Label;         use Gtk.Label;
with Gtk.Scale;         use Gtk.Scale;
with Gtk.Scale_Button;  use Gtk.Scale_Button;
with Gtk.Scrollbar;     use Gtk.Scrollbar;
with Gtk.Volume_Button; use Gtk.Volume_Button;

package body Create_Range is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "The scale, scrollbar and scale button share one"
        & " @bGtk_Adjustment@B. Changing any of them updates the other"
        & " two immediately, without an explicit callback."
        & ASCII.LF
        & "A @bGtk_Scale_Button@B opens a popup scale and selects its"
        & " icon from the list passed to @bGtk_New@B or @bSet_Icons@B."
        & ASCII.LF
        & "The separate @bGtk_Volume_Button@B supplies volume icons"
        & " and a range from 0 to 1. It is deprecated since GTK 4.10;"
        & " new applications can use a scale button with volume icons.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Main_Box     : Gtk_Box;
      Adjustment   : Gtk_Adjustment;
      Scale        : Gtk_Scale;
      Scrollbar    : Gtk_Scrollbar;

      Buttons      : Gtk_Box;
      Label        : Gtk_Label;
      Scale_Button : Gtk_Scale_Button;
      Vol_Button   : Gtk_Volume_Button;
      Icons        : GNAT.Strings.String_List :=
        (new String'("audio-volume-muted-symbolic"),
         new String'("audio-volume-high-symbolic"),
         new String'("audio-volume-low-symbolic"),
         new String'("audio-volume-medium-symbolic"));

   begin
      Set_Label (Frame, "Range");

      Gtk_New (Main_Box, Orientation_Vertical, 10);
      Main_Box.Set_Margin_Start (10);
      Main_Box.Set_Margin_End (10);
      Main_Box.Set_Margin_Top (10);
      Main_Box.Set_Margin_Bottom (10);
      Frame.Set_Child (Main_Box);

      Gtk_New (Adjustment, 50.0, 0.0, 100.0, 0.1, 1.0, 0.0);
      Gtk_New (Scale, Orientation_Horizontal, Adjustment);
      Scale.Set_Size_Request (150, 80);
      Set_Digits (Scale, 1);
      Set_Draw_Value (Scale, True);
      Set_Value_Pos (Scale, Pos_Bottom);
      Main_Box.Append (Scale);

      for I in 0 .. 10 loop
         Add_Mark (Scale, Gdouble (I) * 10.0, Pos_Top, I'Img);
      end loop;

      Gtk_New (Scrollbar, Orientation_Horizontal, Adjustment);
      Main_Box.Append (Scrollbar);

      Gtk_New (Buttons, Orientation_Horizontal, 10);
      Main_Box.Append (Buttons);

      Gtk_New (Label, "Scale button:");
      Buttons.Append (Label);

      Gtk_New (Scale_Button, 0.0, 100.0, 2.0, Icons);
      for Icon of Icons loop
         GNAT.Strings.Free (Icon);
      end loop;
      Scale_Button.Set_Adjustment (Adjustment);
      Buttons.Append (Scale_Button);

      Gtk_New (Label, "Volume button:");
      Buttons.Append (Label);

      Gtk_New (Vol_Button);
      Vol_Button.Set_Value (0.5);
      Buttons.Append (Vol_Button);
   end Run;

end Create_Range;
