------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2011-2026, AdaCore                     --
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

with Gtk.Frame;         use Gtk.Frame;
with Gtk.Grid;          use Gtk.Grid;
with Gtk.Label;         use Gtk.Label;
with Gtk.Spinner;       use Gtk.Spinner;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;

package body Create_Spinners is

   function Help return String is
   begin
      return "A @bGtk_Spinner@B shows activity when progress cannot be"
        & " measured. Use @bStart@B and @bStop@B, or set its"
        & " @bspinning@B property."
        & ASCII.LF
        & "The first spinner runs continuously, the second follows the"
        & " toggle button, and the third stays stopped.";
   end Help;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Grid     : constant Gtk_Grid := Gtk_Grid_New;
      Active   : constant Gtk_Spinner := Gtk_Spinner_New;
      On_Off   : constant Gtk_Spinner := Gtk_Spinner_New;
      Inactive : Gtk_Spinner;
      Toggle   : constant Gtk_Toggle_Button :=
        Gtk_Toggle_Button_New_With_Label ("Start / stop");
   begin
      Frame.Set_Label ("Spinners");
      Frame.Set_Child (Grid);
      Grid.Set_Row_Spacing (12);
      Grid.Set_Column_Spacing (12);
      Grid.Set_Margin_Top (12);
      Grid.Set_Margin_Bottom (12);
      Grid.Set_Margin_Start (12);
      Grid.Set_Margin_End (12);

      Gtk_New (Inactive);
      Active.Set_Size_Request (32, 32);
      On_Off.Set_Size_Request (32, 32);
      Inactive.Set_Size_Request (32, 32);
      Grid.Attach (Gtk_Label_New ("Active spinner:"), 0, 0);
      Grid.Attach (Active, 1, 0);
      Grid.Attach (Gtk_Label_New ("On/off spinner:"), 0, 1);
      Grid.Attach (On_Off, 1, 1);
      Grid.Attach (Toggle, 2, 1);
      Grid.Attach (Gtk_Label_New ("Inactive spinner:"), 0, 2);
      Grid.Attach (Inactive, 1, 2);

      Active.Start;
      Toggle.Bind_Property ("active", On_Off, "spinning");
   end Run;

end Create_Spinners;
