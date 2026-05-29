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

with Interfaces.C; use Interfaces.C;

with Glib;         use Glib;
with Gtk.Frame;    use Gtk.Frame;
with Gtk.Grid;     use Gtk.Grid;
with Gtk.Label;    use Gtk.Label;
with Gtk.Widget;   use Gtk.Widget;

package body Create_Frame is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo shows what a @bGtk_Frame@B offers in Gtk4."
        & ASCII.LF
        & "A frame draws a border around its single child and may carry an"
        & " optional text label embedded in its top edge."
        & ASCII.LF
        & "The shadow types of Gtk3 (@bShadow_In@B, @bShadow_Out@B, ...) no"
        & " longer exist in Gtk4, so the frames below instead illustrate the"
        & " presence or absence of a label and the horizontal placement of"
        & " that label, set with @bSet_Label_Align@B. An @bXalign@B of 0.0 is"
        & " left-aligned and 1.0 is right-aligned; the vertical placement"
        & " argument is gone, as labels always sit on the top edge."
        & ASCII.LF
        & "If you want to constrain the child to a specific aspect ratio even"
        & " when the frame is resized, you should look at the"
        & " @bGtk_Aspect_Frame@B widget instead.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Table : Gtk_Grid;

      procedure Add_Sub_Frame
        (Column, Row : Gint;
         Title       : String;
         Xalign      : C_float;
         Caption     : String);
      --  Attach a labelled (when Title is not empty) sub-frame at the given
      --  grid position, aligning its label at Xalign and showing Caption as
      --  the frame's child.

      -------------------
      -- Add_Sub_Frame --
      -------------------

      procedure Add_Sub_Frame
        (Column, Row : Gint;
         Title       : String;
         Xalign      : C_float;
         Caption     : String)
      is
         Sub_Frame : Gtk_Frame;
         Label     : Gtk_Label;
      begin
         Gtk_New (Sub_Frame, Title);
         if Title /= "" then
            Sub_Frame.Set_Label_Align (Xalign);
         end if;
         Gtk_New (Label, Caption);
         Sub_Frame.Set_Child (Label);
         Table.Attach (Sub_Frame, Column, Row);
      end Add_Sub_Frame;

   begin
      Gtk.Frame.Set_Label (Frame, "Frames");

      Gtk_New (Table);
      Table.Set_Margin_Start (10);
      Table.Set_Margin_End (10);
      Table.Set_Margin_Top (10);
      Table.Set_Margin_Bottom (10);
      Table.Set_Row_Spacing (10);
      Table.Set_Column_Spacing (10);
      Frame.Set_Child (Table);

      Add_Sub_Frame (0, 0, "",      0.0, "No label");
      Add_Sub_Frame (1, 0, "Title", 0.0, "Xalign = 0.0");
      Add_Sub_Frame (0, 1, "Title", 0.2, "Xalign = 0.2");
      Add_Sub_Frame (1, 1, "Title", 0.5, "Xalign = 0.5");
      Add_Sub_Frame (0, 2, "Title", 0.8, "Xalign = 0.8");
      Add_Sub_Frame (1, 2, "Title", 1.0, "Xalign = 1.0");
   end Run;

end Create_Frame;
