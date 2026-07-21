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

with Glib;        use Glib;
with Glib.Main;   use Glib.Main;
with Glib.Object; use Glib.Object;
with Gtk.Box;     use Gtk.Box;
with Gtk.Button;  use Gtk.Button;
with Gtk.Enums;   use Gtk.Enums;
with Gtk.Label;   use Gtk.Label;
with Gtk.Widget;  use Gtk.Widget;

package body Create_Test_Timeout is

   package Label_Timeout is new Glib.Main.Generic_Sources (Gtk_Label);

   Timeout : G_Source_Id := 0;
   Count   : Integer := 0;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @btimeout@B function is a function that is run at specific"
        & " time intervals. This is different from an @bidle@B function, since"
        & " you know exactly when the next occurrence will be.";
   end Help;

   ------------------
   -- Timeout_Test --
   ------------------

   function Timeout_Test (Label : Gtk_Label) return Boolean is
   begin
      Count := Count + 1;
      Label.Set_Text ("count:" & Integer'Image (Count));
      return True;
   end Timeout_Test;

   ------------------
   -- Stop_Timeout --
   ------------------

   procedure Stop_Timeout (Object : access GObject_Record'Class) is
      pragma Unreferenced (Object);
   begin
      if Timeout /= 0 then
         Remove (Timeout);
         Timeout := 0;
         Count := 0;
      end if;
   end Stop_Timeout;

   -------------------
   -- Start_Timeout --
   -------------------

   procedure Start_Timeout (Object : access GObject_Record'Class) is
   begin
      if Timeout = 0 then
         Timeout := Label_Timeout.Timeout_Add
           (100, Timeout_Test'Access, Gtk_Label (Object));
      end if;
   end Start_Timeout;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Button : Gtk_Button;
      Label  : Gtk_Label;
      Box    : Gtk_Box;

   begin
      Set_Label (Frame, "Timeout Test");
      Gtk_New (Box, Orientation_Vertical, Spacing => 0);
      Box.Set_Homogeneous (False);
      Frame.Set_Child (Box);

      Gtk_New (Label, "count : 0");
      Label.Set_Margin_Start (10);
      Label.Set_Margin_End (10);
      Label.Set_Margin_Top (10);
      Label.Set_Margin_Bottom (10);
      Box.Append (Label);

      Gtk_New (Button, "start");
      Button.On_Clicked (Start_Timeout'Access, Slot => Label);
      Box.Append (Button);

      Gtk_New (Button, "stop");
      Button.On_Clicked (Stop_Timeout'Access, Slot => Frame);
      Box.Append (Button);

      --  Stop the timer when the demo is swapped out, so the periodic
      --  callback never references a destroyed label.
      Box.On_Destroy (Stop_Timeout'Access, Slot => Box);
   end Run;

end Create_Test_Timeout;
