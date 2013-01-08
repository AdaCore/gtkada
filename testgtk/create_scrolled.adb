------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2013, AdaCore                     --
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

with Glib;                use Glib;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Table;           use Gtk.Table;
with Gtk.Toggle_Button;   use Gtk.Toggle_Button;
with Gtk;                 use Gtk;

package body Create_Scrolled is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo shows how a @bGtk_Scrolled_Window@B can be used to"
        & " provide some scrolling in any other widget. In this example, the"
        & " @bGtk_Scrolled_Window@B contains a Gtk_Table, that is"
        & " automatically scrolled when the scrollbars are moved."
        & ASCII.LF
        & "No explicit signal is required in this demo, everything is done"
        & " through @bGtk_Adjustment@Bs, that contain the current value of the"
        & " @bGtK_Scrollbar@Bs and also specify the area to display in the"
        & " @bGtk_Table@B.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Table     : Gtk_Table;
      Scrolled  : Gtk_Scrolled_Window;
      Toggle    : Gtk_Toggle_Button;

   begin
      Set_Label (Frame, "Scrolled Window");

      Gtk_New (Scrolled);
      Set_Border_Width (Scrolled, Border_Width => 10);
      Set_Policy (Scrolled,
                  Hscrollbar_Policy => Policy_Automatic,
                  Vscrollbar_Policy => Policy_Automatic);
      Add (Frame, Scrolled);

      Gtk_New (Table,
               Rows        => 20,
               Columns     => 20,
               Homogeneous => False);
      Set_Row_Spacings (Table, Spacing => 10);
      Set_Col_Spacings (Table, Spacing => 10);
      Add_With_Viewport (Scrolled, Table);
      Set_Focus_Hadjustment (Table, Get_Hadjustment (Scrolled));
      Set_Focus_Vadjustment (Table, Get_Vadjustment (Scrolled));

      for I in 0 .. 19 loop
         for J in 0 .. 19 loop
            Gtk_New (Toggle, "button (" & Integer'Image (I)
                     & "," & Integer'Image (J) & ")");
            Attach_Defaults (Table, Toggle, Guint (I), Guint (I + 1),
                             Guint (J), Guint (J + 1));
         end loop;
      end loop;

      Show_All (Frame);
   end Run;

end Create_Scrolled;
