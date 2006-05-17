-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Box; use Gtk.Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Scale; use Gtk.Scale;
with Gtk.Scrollbar; use Gtk.Scrollbar;
with Gtk; use Gtk;

package body Create_Range is

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

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1       : Gtk_Box;
      Box2       : Gtk_Box;
      Adjustment : Gtk_Adjustment;
      Scale      : Gtk_Scale;
      Scrollbar  : Gtk_Scrollbar;

   begin
      Set_Label (Frame, "Range");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (Adjustment, 0.0, 0.0, 101.0, 0.1, 1.0, 1.0);
      Gtk_New_Hscale (Scale, Adjustment);
      Set_USize (Scale, 150, 30);
      Set_Update_Policy (Scale, Update_Delayed);
      Set_Digits (Scale, 1);
      Set_Draw_Value (Scale, True);
      Pack_Start (Box2, Scale, True, True, 0);

      Gtk_New_Hscrollbar (Scrollbar, Adjustment);
      Set_Update_Policy (Scrollbar, Update_Continuous);
      Pack_Start (Box2, Scrollbar, True, True, 0);

      Show_All (Frame);
   end Run;

end Create_Range;

