------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2012, AdaCore                     --
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
with Gtk.Table;    use Gtk.Table;
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
      Table  : Gtk_Table;
      Label  : Gtk_Label;
      Frame2 : Gtk_Frame;
   begin
      Gtk.Frame.Set_Label (Frame, "Frames");

      Gtk_New (Table, Rows => 3, Columns => 2, Homogeneous => True);
      Set_Row_Spacings (Table, Spacing => 5);
      Set_Col_Spacings (Table, Spacing => 5);
      Set_Border_Width (Table, Border_Width => 10);
      Add (Frame, Table);

      --  First frame
      Gtk_New (Frame2, "");
      Set_Shadow_Type (Frame2, Gtk.Enums.Shadow_In);
      Attach_Defaults (Table, Frame2, 0, 1, 0, 1);
      Gtk_New (Label, "Shadow_In");
      Add (Frame2, Label);

      --  Second Frame
      Gtk_New (Frame2, "");
      Set_Shadow_Type (Frame2, Gtk.Enums.Shadow_Out);
      Attach_Defaults (Table, Frame2, 1, 2, 0, 1);
      Gtk_New (Label, "Shadow_Out");
      Add (Frame2, Label);

      --  Third Frame
      Gtk_New (Frame2, "");
      Set_Shadow_Type (Frame2, Gtk.Enums.Shadow_Etched_In);
      Attach_Defaults (Table, Frame2, 0, 1, 1, 2);
      Gtk_New (Label, "Shadow_Etched_In");
      Add (Frame2, Label);

      --  Fourth Frame
      Gtk_New (Frame2, "");
      Set_Shadow_Type (Frame2, Gtk.Enums.Shadow_Etched_Out);
      Attach_Defaults (Table, Frame2, 1, 2, 1, 2);
      Gtk_New (Label, "Shadow_Etched_Out");
      Add (Frame2, Label);

      --  Fifth Frame
      Gtk_New (Frame2, "Title");
      Set_Label_Align (Frame2, Xalign => 0.2, Yalign => 0.0);
      Attach_Defaults (Table, Frame2, 0, 1, 2, 3);
      Gtk_New (Label, "Label_Align: Xalign = 0.2");
      Add (Frame2, Label);

      --  Sixth Frame
      Gtk_New (Frame2, "Title");
      Set_Label_Align (Frame2, Xalign => 0.8, Yalign => 0.0);
      Attach_Defaults (Table, Frame2, 1, 2, 2, 3);
      Gtk_New (Label, "Label_Align: Xalign = 0.8");
      Add (Frame2, Label);

      Show_All (Frame);
   end Run;

end Create_Frame;
