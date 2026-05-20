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

with Glib; use Glib;
with Gtk.Arrow; use Gtk.Arrow;
with Gtk.Box; use Gtk.Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label; use Gtk.Label;
with Gtk.Grid;  use Gtk.Grid;
with Gtk; use Gtk;

package body Create_Arrow is

   function Help return String is
   begin
      return "This demo shows the four possible types of @bGtk_Shadow_Type@B"
        & " that can be used within @bGtkAda@B."
        & ASCII.LF
        & "It also demonstrates how items can be organized in a @bGtk_Table@B"
        & " to position them as you want. Each of the groups @bGtk_Arrow@B +"
        & " @bGtk_Label@B is first put in a @bGtk_Box@B, that is then put in"
        & " a table element.";
   end Help;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1      : Gtk_Box;
      Box2      : Gtk_Box;
      Table     : Gtk_Grid;
      Arrow     : Gtk_Arrow;
      Label     : Gtk_Label;
   begin
      Frame.Set_Label ("Arrows");

      Gtk_New_Vbox (Box1, Homogeneous => False, Spacing => 0);
      Frame.Add (Box1);

      Gtk_New (Table);
      Table.Set_Border_Width (Border_Width => 10);
      Box1.Pack_Start (Table, Expand => False, Fill => False, Padding => 0);

      Gtk_New_Vbox (Box2, Homogeneous => False, Spacing => 0);
      Gtk_New (Arrow,
               Arrow_Type  => Arrow_Up,
               Shadow_Type => Shadow_In);
      Pack_Start (Box2, Arrow, Expand => True, Fill => True, Padding => 0);
      Gtk_New (Label, "Shadow_In");
      Pack_Start (Box2, Label, Expand => True, Fill => True, Padding => 0);
      Table.Attach (Box2, 1, 0);

      Gtk_New_Vbox (Box2, Homogeneous => False, Spacing => 0);
      Gtk_New (Arrow,
               Arrow_Type  => Arrow_Left,
               Shadow_Type => Shadow_Out);
      Pack_Start (Box2, Arrow, Expand => True, Fill => True, Padding => 0);
      Gtk_New (Label, "Shadow_Out");
      Pack_Start (Box2, Label, Expand => True, Fill => True, Padding => 0);
      Table.Attach (Box2, 0, 1);

      Gtk_New_Vbox (Box2, Homogeneous => False, Spacing => 0);
      Gtk_New (Arrow,
               Arrow_Type  => Arrow_Right,
               Shadow_Type => Shadow_Etched_In);
      Pack_Start (Box2, Arrow, Expand => True, Fill => True, Padding => 0);
      Gtk_New (Label, "Shadow_Etched_In");
      Pack_Start (Box2, Label, Expand => True, Fill => True, Padding => 0);
      Table.Attach (Box2, 2, 1);

      Gtk_New_Vbox (Box2, Homogeneous => False, Spacing => 0);
      Gtk_New (Arrow,
               Arrow_Type  => Arrow_Down,
               Shadow_Type => Shadow_Etched_Out);
      Pack_Start (Box2, Arrow, Expand => True, Fill => True, Padding => 0);
      Gtk_New (Label, "Shadow_Etched_Out");
      Pack_Start (Box2, Label, Expand => True, Fill => True, Padding => 0);
      Table.Attach (Box2, 1, 2);

      Show_All (Box1);
   end Run;

end Create_Arrow;
