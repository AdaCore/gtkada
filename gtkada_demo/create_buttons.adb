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
with Glib.Object;  use Glib.Object;
with Gtk;          use Gtk;
with Gtk.Box;      use Gtk.Box;
with Gtk.Button;   use Gtk.Button;
with Gtk.Enums;    use Gtk.Enums;
with Gtk.Frame;    use Gtk.Frame;
with Gtk.Grid;     use Gtk.Grid;
with Gtk.Widget;   use Gtk.Widget;

package body Create_Buttons is

   procedure Button_Window (Widget : access GObject_Record'Class);
   --  Toggles the visibility of Widget

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "a @bGtk_Button@B is the basic widget to which you can associate"
        & " a callback. Whenever the user presses the mouse on the button,"
        & " one or more functions specified by the user can be called.";
   end Help;

   -------------------
   -- Button_Window --
   -------------------

   procedure Button_Window (Widget : access GObject_Record'Class) is
      W : constant Gtk_Widget := Gtk_Widget (Widget);
   begin
      if W.Get_Visible then
         W.Hide;
      else
         W.Show;
      end if;
   end Button_Window;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1    : Gtk_Box;
      Table   : Gtk_Grid;
      Button  : array (0 .. 8) of Gtk_Button;
      Left_A  : constant array (0 .. 8) of Gint :=
        (0, 1, 2, 0, 2, 1, 1, 2, 0);
      Top_A  : constant array (0 .. 8) of Gint := (0, 1, 2, 2, 0, 2, 0, 1, 1);

   begin
      Gtk.Frame.Set_Label (Frame, "Buttons");

      Gtk_New_Vbox (Box1, Homogeneous => False, Spacing => 0);
      Frame.Add (Box1);

      Gtk_New (Table);
      Table.Set_Border_Width (Border_Width => 10);
      Box1.Pack_Start (Table, Expand => False, Fill => False, Padding => 0);

      for J in Button'Range loop
         Gtk_New (Button (J), Label => "Button" & Integer'Image (J));
      end loop;

      for J in Button'Range loop
         Button (J).On_Clicked
            (Button_Window'Access, Button ((J + 1) mod Button'Length));
         Table.Attach (Button (J), Left => Left_A (J), Top => Top_A (J));
      end loop;

      Show_All (Box1);
   end Run;

end Create_Buttons;
