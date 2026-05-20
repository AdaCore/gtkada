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

with Glib;       use Glib;
with Gtk.Box;    use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Label;  use Gtk.Label;
with Glib.Main;  use Glib.Main;
with Gtk.Widget; use Gtk.Widget;
with Gtk;        use Gtk;
with Common;     use Common;

package body Create_Test_Timeout is

   package Label_Timeout is new Glib.Main.Generic_Sources (Gtk_Label);

   Timeout : G_Source_Id;
   Count   : Integer := 0;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @btimeout@B function is a function that is run at specific"
        & " time intervals. This is different from a @bidle@B function, since"
        & " you know exactly when the next occurence will be.";
   end Help;

   ------------------
   -- Timeout_Test --
   ------------------

   function Timeout_Test (Label : Gtk_Label) return Boolean is
   begin
      Count := Count + 1;
      Set_Text (Label, "count:" & Integer'Image (Count));
      return True;
   end Timeout_Test;

   ------------------
   -- Stop_Timeout --
   ------------------

   procedure Stop_Timeout (Object : access Gtk_Widget_Record'Class) is
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

   procedure Start_Timeout (Label : access Gtk_Label_Record'Class) is
   begin
      if Timeout = 0 then
         Timeout := Label_Timeout.Timeout_Add
           (100, Timeout_Test'Access, Gtk_Label (Label));
      end if;
   end Start_Timeout;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Button   : Gtk_Button;
      Label    : Gtk_Label;
      Box      : Gtk_Box;

   begin
      Set_Label (Frame, "Timeout Test");
      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 0);
      Add (Frame, Box);

      Gtk_New (Label, "count : 0");
      Set_Padding (Label, 10, 10);
      Pack_Start (Box, Label, False, False, 0);

      Gtk_New (Button, "start");
      Label_Handler.Object_Connect
        (Button, "clicked", Start_Timeout'Access, Slot_Object => Label);
      Button.Set_Can_Default (True);
      Pack_Start (Box, Button, False, False, 0);

      Gtk_New (Button, "stop");
      Widget_Handler.Object_Connect
        (Button, "clicked", Stop_Timeout'Access, Slot_Object => Frame);
      Pack_Start (Box, Button, False, False, 0);

      Widget_Handler.Connect (Box, "destroy", Stop_Timeout'Access);

      Show_All (Frame);
   end Run;

end Create_Test_Timeout;
