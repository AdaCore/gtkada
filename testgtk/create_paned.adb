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

with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Frame;        use Gtk.Frame;
with Gtk.Paned;        use Gtk.Paned;
with Gtk.Widget;       use Gtk.Widget;
with Gtk;              use Gtk;

package body Create_Paned is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Paned@B splits a container in two parts, that can be"
        & " resized by the user." & ASCII.LF
        & "They have two children, one for each side."
        & ASCII.LF
        & "If @bShrink@B is set to True for one of the children, then the user"
        & " can resize it to any size. If it is set to False, then the"
        & " minimum size set for the child by a call to @bSet_Usize@B is"
        & " enforced, and the child can never be shrinked more than that."
        & ASCII.LF
        & "If @bResize@B is set to True for one of the children only, then"
        & " that child gets the exact size it requested, the other gets the"
        & " remaining space. In the example, the two buttons have requested"
        & " the same size, but since the second sets Resize to True, it gets"
        & " more space."
        & ASCII.LF
        & "If both children have the same value for resize, their allocated"
        & " size is a ratio between their respective sizes."
        & ASCII.LF
        & "Note that moving the handle to manually resize the widgets"
        & " cancels the effect of the resize buttons, so you should click on"
        & " them before playing with the handles...";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      VPaned : Gtk_Paned;
      HPaned : Gtk_Paned;
      Frame2  : Gtk_Frame;
      Button : Gtk_Button;
      Vbox   : Gtk_Box;

   begin
      Set_Label (Frame, "Panes");

      Gtk_New_Vbox (Vbox, False, 0);
      Add (Frame, Vbox);

      Gtk_New_Vpaned (VPaned);
      Pack_Start (Vbox, VPaned, True, True, 0);
      Set_Border_Width (VPaned, 5);

      Gtk_New_Hpaned (HPaned);
      Pack1 (VPaned, HPaned, Resize => False, Shrink => True);

      Gtk_New (Frame2);
      Gtk_New (Button, "not Resize, not Shrink, minWidth=60");
      Add (Frame2, Button);
      Set_Shadow_Type (Frame2, Shadow_In);
      Set_Size_Request (Frame2, 60, 60);
      Pack1 (HPaned, Frame2, False, False);

      Gtk_New (Frame2);
      Gtk_New (Button, "Resize, Shrink");
      Add (Frame2, Button);
      Set_Shadow_Type (Frame2, Shadow_In);
      Set_Size_Request (Frame2, 80, 60);
      Pack2 (HPaned, Frame2, Resize => True, Shrink => True);

      Gtk_New (Frame2);
      Gtk_New (Button, "not Resize, not Shrink, minHeight=280");
      Add (Frame2, Button);
      Set_Shadow_Type (Frame2, Shadow_In);
      Set_Size_Request (Frame2, 60, 280);
      Pack2 (VPaned, Frame2, Resize => False, Shrink => False);

      Show_All (Frame);
   end Run;

end Create_Paned;
