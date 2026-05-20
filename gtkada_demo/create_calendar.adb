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
with Gtk.Box; use Gtk.Box;
with Gtk.Calendar; use Gtk.Calendar;
with Gtk.Frame; use Gtk.Frame;
with Gtk; use Gtk;

package body Create_Calendar is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Calendar@B is a simple way to interactively select"
        & " a date. A callback can be set for every change to the date,"
        & " for instance the date, the month,...";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1     : Gtk_Box;
      Calendar : Gtk_Calendar;
   begin
      Set_Label (Frame, "Calendar");

      Gtk_New_Vbox (Box1, Homogeneous => False, Spacing => 0);
      Add (Frame, Box1);
      Show (Box1);

      Gtk_New (Calendar);
      Pack_Start (Box1, Calendar, False, False, 0);
      Show (Calendar);
   end Run;

end Create_Calendar;
