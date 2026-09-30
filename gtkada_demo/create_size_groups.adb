------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                    Copyright (C) 1998-2026, AdaCore                      --
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

with GNAT.Strings;

with Glib;             use Glib;
with Gtk;              use Gtk;
with Gtk.Box;          use Gtk.Box;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Drop_Down;    use Gtk.Drop_Down;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Frame;        use Gtk.Frame;
with Gtk.Grid;         use Gtk.Grid;
with Gtk.Label;        use Gtk.Label;
with Gtk.Size_Group;   use Gtk.Size_Group;
with Gtk.Widget;       use Gtk.Widget;

package body Create_Size_Groups is

   Group : Gtk_Size_Group;
   --  The group shared by every drop-down of the demo

   procedure Add_Row
     (Grid    : not null access Gtk_Grid_Record'Class;
      Row     : Gint;
      Text    : String;
      Options : GNAT.Strings.String_List);
   --  Add a new row in Grid, with a label Text and a drop-down offering
   --  Options. The drop-down is added to Group.

   procedure Toggle_Grouping
     (Check_Button : access Gtk_Check_Button_Record'Class);
   --  Toggle whether the size group is active

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGtk_Size_Group@B makes a set of widgets request the same"
        & " size, which is useful to line up controls that do not share a"
        & " common container. Here the drop-downs of two independent"
        & " grids are put in the same horizontal size group, so that"
        & " they all have the same width."
        & ASCII.LF
        & "Clear the ""Enable grouping"" check button to see them fall"
        & " out of alignment."
        & ASCII.LF
        & "Note that a size group only affects the size @brequested@B by"
        & " the widgets, not the size they are finally allocated.";
   end Help;

   -------------
   -- Add_Row --
   -------------

   procedure Add_Row
     (Grid    : not null access Gtk_Grid_Record'Class;
      Row     : Gint;
      Text    : String;
      Options : GNAT.Strings.String_List)
   is
      Label    : Gtk_Label;
      Dropdown : Gtk_Drop_Down;
   begin
      Gtk_New_With_Mnemonic (Label, Text);
      Label.Set_Halign (Align_Start);
      Label.Set_Hexpand (True);
      Grid.Attach (Label, 0, Row);

      Gtk_New_From_Strings (Dropdown, Options);
      Label.Set_Mnemonic_Widget (Dropdown);
      Group.Add_Widget (Dropdown);
      Grid.Attach (Dropdown, 1, Row);
   end Add_Row;

   ---------------------
   -- Toggle_Grouping --
   ---------------------

   procedure Toggle_Grouping
     (Check_Button : access Gtk_Check_Button_Record'Class) is
   begin
      --  Setting the property and calling Set_Mode are equivalent: both are
      --  shown only to demonstrate the two techniques.

      if Check_Button.Get_Active then
         Set_Property (Group, Mode_Property, Horizontal);
      else
         Group.Set_Mode (None);
      end if;
   end Toggle_Grouping;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Color_Options : GNAT.Strings.String_List :=
        (new String'("Red"), new String'("Green"), new String'("Blue"));
      Dash_Options  : GNAT.Strings.String_List :=
        (new String'("Solid"), new String'("Dashed"), new String'("Dotted"));
      End_Options   : GNAT.Strings.String_List :=
        (new String'("Square"), new String'("Round"),
         new String'("Double Arrow"));

      Vbox   : Gtk_Box;
      Toggle : Gtk_Check_Button;
      Grid   : Gtk_Grid;

      function New_Options_Frame (Title : String) return Gtk_Grid;
      --  Append to Vbox a frame titled Title, and return the grid it holds

      procedure Free (List : in out GNAT.Strings.String_List);
      --  Free every string in List

      ----------
      -- Free --
      ----------

      procedure Free (List : in out GNAT.Strings.String_List) is
      begin
         for S of List loop
            GNAT.Strings.Free (S);
         end loop;
      end Free;

      -----------------------
      -- New_Options_Frame --
      -----------------------

      function New_Options_Frame (Title : String) return Gtk_Grid is
         F    : Gtk_Frame;
         Grid : Gtk_Grid;
      begin
         Gtk_New (F, Title);
         Vbox.Append (F);

         Gtk_New (Grid);
         Grid.Set_Margin_Start (5);
         Grid.Set_Margin_End (5);
         Grid.Set_Margin_Top (5);
         Grid.Set_Margin_Bottom (5);
         Grid.Set_Row_Spacing (5);
         Grid.Set_Column_Spacing (10);
         F.Set_Child (Grid);
         return Grid;
      end New_Options_Frame;

   begin
      Set_Label (Frame, "Size Groups");

      Gtk_New (Vbox, Orientation_Vertical, 5);
      Vbox.Set_Margin_Start (5);
      Vbox.Set_Margin_End (5);
      Vbox.Set_Margin_Top (5);
      Vbox.Set_Margin_Bottom (5);
      Frame.Set_Child (Vbox);

      Gtk_New (Group, Horizontal);

      Grid := New_Options_Frame ("Color Options");
      Add_Row (Grid, 0, "_Foreground", Color_Options);
      Add_Row (Grid, 1, "_Background", Color_Options);

      Grid := New_Options_Frame ("Line Options");
      Add_Row (Grid, 0, "_Dashing", Dash_Options);
      Add_Row (Grid, 1, "_Line ends", End_Options);

      Gtk_New_With_Mnemonic (Toggle, "_Enable grouping");
      Toggle.Set_Active (True);
      Toggle.On_Toggled (Toggle_Grouping'Access);
      Vbox.Append (Toggle);

      Free (Color_Options);
      Free (Dash_Options);
      Free (End_Options);
   end Run;

end Create_Size_Groups;
