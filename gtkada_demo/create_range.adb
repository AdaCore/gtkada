------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2018, AdaCore                     --
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
with Gtk;               use Gtk;
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
      return "A @bGtk_Range@B is a simple way to select a value in a"
        & " specific interval. Although less precise than a "
        & "@bGtk_Spin_Button@B, it is much faster to use."
        & ASCII.LF
        & "A @bGtk_Scrollbar@B (seen below the range) can also be used as a"
        & " @bGtk_Range@B, although it does not display its value and relies"
        & " on another widget to do so."
        & ASCII.LF
        & "Note that this demo does not require any explicit callback to be"
        & " set to connect the two widgets. In fact, they both use the same"
        & " @bGtk_Adjustment@B for their value. Thus, when you modify one of"
        & " the widget, it modifies its value in the adjustment, which is then"
        & " reflected in the other widget."
        & ASCII.LF
        & "As you can see, the @bGtk_Scrollbar@B update its value as soon as"
        & " it is moved, whereas the @bGtk_Range@B only update its value when"
        & " the user releases the mouse button.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1         : Gtk_Box;
      Box2         : Gtk_Box;
      Adjustment   : Gtk_Adjustment;
      Scale        : Gtk_Scale;
      Scrollbar    : Gtk_Scrollbar;

      Box3         : Gtk_Box;
      Label        : Gtk_Label;
      Scale_Button : Gtk_Scale_Button;
      Vol_Button   : Gtk_Volume_Button;

   begin
      Set_Label (Frame, "Range");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (Adjustment, 0.0, 0.0, 101.0, 0.1, 1.0, 1.0);
      Gtk_New_Hscale (Scale, Adjustment);
      Scale.Set_Size_Request (150, 80);
      Set_Digits (Scale, 1);
      Set_Draw_Value (Scale, True);
      Set_Value_Pos (Scale, Pos_Bottom);
      Pack_Start (Box2, Scale, True, True, 0);

      for I in 0 .. 10 loop
         Add_Mark (Scale, Gdouble (I) * 10.0, Pos_Top, I'Img);
      end loop;

      Gtk_New_Hscrollbar (Scrollbar, Adjustment);
      Pack_Start (Box2, Scrollbar, True, True, 0);

      Gtk_New_Hbox (Box3, False, 10);
      Pack_Start (Box2, Box3, True, True, 0);

      Gtk_New (Label, "Scale button:");
      Pack_Start (Box3, Label, False, False, 0);

      Gtk_New (Scale_Button, Icon_Size_Button, 0.0, 100.0, 2.0,
               Icons => (1 .. 0 => null));
      Pack_Start (Box3, Scale_Button, False, False, 0);

      Gtk_New (Label, "Volume button:");
      Pack_Start (Box3, Label, False, False, 0);

      Gtk_New (Vol_Button);
      Pack_Start (Box3, Vol_Button, False, False, 0);

      Show_All (Frame);
   end Run;

end Create_Range;
