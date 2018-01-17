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

with Glib;              use Glib;
with Gtk.Box;           use Gtk.Box;
with Gtk.Button;        use Gtk.Button;
with Gtk.Check_Button;  use Gtk.Check_Button;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Frame;         use Gtk.Frame;
with Gtk.Handlers;      use Gtk.Handlers;
with Gtk.Label;         use Gtk.Label;
with Gtk.Size_Group;    use Gtk.Size_Group;
with Gtk.Grid;          use Gtk.Grid;

package body Create_Size_Groups is

   procedure Add_Row
     (Table : access Gtk_Grid_Record'Class;
      Row   : Gint;
      Group : Gtk_Size_Group;
      Text  : String);
   --  Add a new row in Table, with a label Text .
   --  The option menu in that row is added to the group Group.

   procedure Toggle_Grouping
     (Check_Button : access Gtk_Check_Button_Record'Class;
      Group        : Gtk_Size_Group);
   --  Toggle whether the size group is active.

   package Toggle_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Check_Button_Record, Gtk_Size_Group);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "@bGtk_Size_Group@B provides a mechanism for grouping a number of"
        & " widgets together so they all request the same amount of space."
        & " This is typically useful when you want a column of widgets to have"
        & " the same size, but you can't use a @bGtk_Table@B widget."
        & ASCII.LF
        & "Note that size groups only affect the amount of space requested,"
        & " not the size that the widgets finally receive. If you want the"
        & " widgets in a @bGtk_Size_Group@B to actually be the same size,"
        & " you need to pack them in such a way that they get the size they"
        & " request and not more. For example, if you are packing your"
        & " widgets into a table, you would not include the FILL flag.";
   end Help;

   -------------
   -- Add_Row --
   -------------

   procedure Add_Row
     (Table : access Gtk_Grid_Record'Class;
      Row   : Gint;
      Group : Gtk_Size_Group;
      Text  : String)
   is
      Label  : Gtk_Label;
      Button : Gtk_Button;
   begin
      Gtk_New (Label, Text);
      Set_Alignment (Label, 0.0, 1.0);
      Table.Attach (Label, 0, Row);

      Gtk_New (Button, Text);
      Gtk.Size_Group.Add_Widget (Group, Button);
      Table.Attach (Button, 1, Row);
   end Add_Row;

   ---------------------
   -- Toggle_Grouping --
   ---------------------

   procedure Toggle_Grouping
     (Check_Button : access Gtk_Check_Button_Record'Class;
      Group        : Gtk_Size_Group) is
   begin
      --  Note: we use both the properties and the directy function call only
      --  to demonstrate the two technics. No other technical reason.

      if Get_Active (Check_Button) then
         Set_Property (Group, Mode_Property, Horizontal);
      else
         Gtk.Size_Group.Set_Mode (Group, None);
      end if;
   end Toggle_Grouping;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Vbox   : Gtk_Box;
      Group  : Gtk_Size_Group;
      Table  : Gtk_Grid;
      F      : Gtk_Frame;
      Toggle : Gtk_Check_Button;

   begin
      Gtk_New_Vbox (Vbox, Homogeneous => False);
      Add (Frame, Vbox);
      Set_Label (Frame, "Size group");

      Gtk_New (Group, Horizontal);

      Gtk_New (F, "Options1");
      Pack_Start (Vbox, F, Expand => False, Fill => False);

      Gtk_New (Table);
      F.Add (Table);
      Table.Set_Border_Width (5);

      Add_Row (Table, 0, Group, "foofoofoofoofoofoofoofoofoo");
      Add_Row (Table, 1, Group, "foofoofoo");

      Gtk_New (F, "Options2");
      Vbox.Pack_Start (F, Expand => False, Fill => False);

      Gtk_New (Table);
      F.Add (Table);
      Table.Set_Border_Width (5);

      Add_Row (Table, 0, Group, "foo");
      Add_Row (Table, 1, Group, "foofoofoofoofoofoo");

      Gtk_New (Toggle, "Enable grouping");
      Vbox.Pack_Start (Toggle, Expand => False, Fill => False);
      Toggle.Set_Active (True);
      Toggle_Cb.Connect (Toggle, "toggled", Toggle_Grouping'Access, Group);

      Frame.Show_All;
   end Run;

end Create_Size_Groups;
