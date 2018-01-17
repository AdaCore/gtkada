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

with Glib;         use Glib;
with Gtk.Grid;     use Gtk.Grid;
with Gtk.Enums;    use Gtk.Enums;
with Gtk.Frame;    use Gtk.Frame;
with Gtk.Label;    use Gtk.Label;

package body Create_Frame is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo shows the different kinds of shadows that are"
        & " available for frames, as well as the positions that the label"
        & " of the frame can have."
        & ASCII.LF
        & "Note that the label must be displayed at the top of the frame"
        & " and can not be displayed elsewhere."
        & ASCII.LF
        & "If you want to constrained the child to a specific aspect ratio"
        & " even when the frame is resized, you should look at the"
        & " @bGtk_Aspect_Frame@B widget instead.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Table  : Gtk_Grid;
      Label  : Gtk_Label;
      Frame2 : Gtk_Frame;
   begin
      Gtk.Frame.Set_Label (Frame, "Frames");

      Gtk_New (Table);
      Table.Set_Border_Width (Border_Width => 10);
      Frame.Add (Table);

      --  First frame
      Gtk_New (Frame2, "");
      Frame2.Set_Shadow_Type (Gtk.Enums.Shadow_In);
      Table.Attach (Frame2, 0, 0);
      Gtk_New (Label, "Shadow_In");
      Frame2.Add (Label);

      --  Second Frame
      Gtk_New (Frame2, "");
      Frame2.Set_Shadow_Type (Gtk.Enums.Shadow_Out);
      Table.Attach (Frame2, 1, 0);
      Gtk_New (Label, "Shadow_Out");
      Frame2.Add (Label);

      --  Third Frame
      Gtk_New (Frame2, "");
      Frame2.Set_Shadow_Type (Gtk.Enums.Shadow_Etched_In);
      Table.Attach (Frame2, 0, 1);
      Gtk_New (Label, "Shadow_Etched_In");
      Frame2.Add (Label);

      --  Fourth Frame
      Gtk_New (Frame2, "");
      Frame2.Set_Shadow_Type (Gtk.Enums.Shadow_Etched_Out);
      Table.Attach (Frame2, 1, 1);
      Gtk_New (Label, "Shadow_Etched_Out");
      Frame2.Add (Label);

      --  Fifth Frame
      Gtk_New (Frame2, "Title");
      Frame2.Set_Label_Align (Xalign => 0.2, Yalign => 0.0);
      Table.Attach (Frame2, 0, 2);
      Gtk_New (Label, "Label_Align: Xalign = 0.2");
      Frame2.Add (Label);

      --  Sixth Frame
      Gtk_New (Frame2, "Title");
      Frame2.Set_Label_Align (Xalign => 0.8, Yalign => 0.0);
      Table.Attach (Frame2, 1, 2);
      Gtk_New (Label, "Label_Align: Xalign = 0.8");
      Frame2.Add (Label);

      Frame.Show_All;
   end Run;

end Create_Frame;
