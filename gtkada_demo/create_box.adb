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

with Gtk.Box;    use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Frame;  use Gtk.Frame;
with Gtk.Label;  use Gtk.Label;
with Gtk.Widget; use Gtk.Widget;

package body Create_Box is

   procedure Add_Buttons
     (Vbox        : Gtk_Box;
      Message     : String;
      Homogeneous : Boolean;
      Expand      : Boolean := False;
      Fill        : Boolean := False);
   --  Add a labelled row of three buttons within Vbox, packing them into a
   --  homogeneous (or not) horizontal box and applying the per-child hexpand
   --  and halign properties derived from Expand and Fill.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "This demo shows how you can use the @bGtk_Box@B container in"
        & " Gtk4."
        & ASCII.LF
        & ASCII.LF
        & "Each row below packs three buttons into a horizontal box with"
        & " @bAppend@B, and illustrates how the @bHomogeneous@B box property"
        & " combines with the per-child @bhexpand@B and @bhalign@B properties."
        & " Gtk4 has replaced the old @bExpand@B and @bFill@B arguments of"
        & " @bPack_Start@B with these child properties."
        & ASCII.LF
        & " - @bHomogeneous@B: if True, every child of the box is given the"
        & " same size as the largest child. The per-child expand setting is"
        & " then irrelevant."
        & ASCII.LF
        & " - @bhexpand@B (labelled @bExpand@B below): if True, the child"
        & " claims any extra space available along the box's orientation."
        & ASCII.LF
        & " - @bhalign@B (labelled @bFill@B below): @bAlign_Fill@B stretches"
        & " the child so it occupies all the space it was given, whereas"
        & " @bAlign_Center@B leaves the child at its minimum requested size,"
        & " surrounded by empty space.";
   end Help;

   -----------------
   -- Add_Buttons --
   -----------------

   procedure Add_Buttons
     (Vbox        : Gtk_Box;
      Message     : String;
      Homogeneous : Boolean;
      Expand      : Boolean := False;
      Fill        : Boolean := False)
   is
      Box   : Gtk_Box;
      Label : Gtk_Label;

      procedure Add_One (Caption : String);
      --  Append a single button carrying Caption, with the expand/fill
      --  child properties common to this row.

      -------------
      -- Add_One --
      -------------

      procedure Add_One (Caption : String) is
         Button : Gtk_Button;
      begin
         Gtk_New (Button, Caption);
         Button.Set_Hexpand (Expand);
         Button.Set_Halign (if Fill then Align_Fill else Align_Center);
         Box.Append (Button);
      end Add_One;

   begin
      Gtk_New (Label, Message);
      Label.Set_Xalign (0.0);
      Vbox.Append (Label);

      Gtk_New (Box, Orientation_Vertical, Spacing => 0);
      Box.Set_Homogeneous (Homogeneous);
      Vbox.Append (Box);

      --  Use a function from one of the implemented interfaces.
      Box.Set_Orientation (Gtk.Enums.Orientation_Horizontal);

      Add_One ("Small");
      Add_One ("A bit longer");
      Add_One ("The longest button");
   end Add_Buttons;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Vbox : Gtk_Box;
      Box  : Gtk_Box;

   begin
      Gtk.Frame.Set_Label (Frame, "Boxes");

      Gtk_New (Vbox, Orientation_Vertical, Spacing => 25);
      Vbox.Set_Homogeneous (False);
      Vbox.Set_Margin_Start (10);
      Vbox.Set_Margin_End (10);
      Vbox.Set_Margin_Top (10);
      Vbox.Set_Margin_Bottom (10);
      Frame.Set_Child (Vbox);

      Gtk_New (Box, Orientation_Vertical, Spacing => 0);
      Box.Set_Homogeneous (False);
      Vbox.Append (Box);

      Add_Buttons
        (Box,
         "Homogeneous => False, Expand => False",
         Homogeneous => False,
         Expand      => False,
         Fill        => False);
      Add_Buttons
        (Box,
         "Homogeneous => False, Expand => True, Fill => False",
         Homogeneous => False,
         Expand      => True,
         Fill        => False);
      Add_Buttons
        (Box,
         "Homogeneous => False, Expand => True, Fill => True",
         Homogeneous => False,
         Expand      => True,
         Fill        => True);
      Add_Buttons
        (Box,
         "Homogeneous => True, Fill => False",
         Homogeneous => True,
         Fill        => False);
      Add_Buttons
        (Box,
         "Homogeneous => True, Fill => True",
         Homogeneous => True,
         Fill        => True);
   end Run;

end Create_Box;
