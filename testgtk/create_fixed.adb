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

with Gtk.Button; use Gtk.Button;
with Gtk.Fixed; use Gtk.Fixed;
with Gtk; use Gtk;

package body Create_Fixed is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "@bGtk_Fixed@B is the only container that does not do any layout "
        & "for you. Instead, you can place the children at the exact "
        & "coordinates you want, as in this example."
        & ASCII.LF
        & "It is not recommended to use this container, since you have to "
        & "handle the resizing of the window yourself, and nothing is done "
        & "for you."
        & ASCII.LF
        & "If you need to use this container, it probably means that the "
        & "design of your application is not correct and should be rethought.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Fix     : Gtk_Fixed;
      Button  : Gtk_Button;

   begin
      Gtk.Frame.Set_Label (Frame, "Fixed");

      Gtk_New (Fix);
      Add (Frame, Fix);

      Gtk_New (Button, "(10, 10)");
      Put (Fix, Button, 10, 10);

      Gtk_New (Button, "(135, 123)");
      Put (Fix, Button, 135, 123);

      Gtk_New (Button, "(265, 243)");
      Put (Fix, Button, 265, 243);

      Gtk_New (Button, "(165, 243)");
      Put (Fix, Button, 165, 243);

      Show_All (Frame);
   end Run;

end Create_Fixed;
