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

with Gtk.Box; use Gtk.Box;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;

package body Create_Radio_Button is

   function Help return String is
   begin
      return "A @bGtk_Radio_Button@B is part of a group. Only one button"
        & " can be selected in this group. If you select a new button,"
        & " the button currently selected is first deselected."
        & ASCII.LF
        & "This group is actually a simple @bWidget_SList.GSList@B. You"
        & " put the first button into the @bNull_List@B, and then add"
        & " the next buttons to the @bGroup@B created.";
   end Help;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1, Box2 : Gtk_Box;
      Button     : Gtk_Radio_Button;

   begin
      Set_Label (Frame, "Radio Buttons");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (Button, Widget_SList.Null_List, "button1");
      Pack_Start (Box2, Button, True, True, 0);

      Gtk_New (Button, Get_Group (Button), "button2");
      Set_Active (Button, True);
      Pack_Start (Box2, Button, True, True, 0);

      Gtk_New (Button, Get_Group (Button), "button3");
      Pack_Start (Box2, Button, True, True, 0);

      Show_All (Frame);
   end Run;

end Create_Radio_Button;
