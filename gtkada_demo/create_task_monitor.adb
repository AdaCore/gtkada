------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

with Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glib;       use Glib;
with Gtk.Box;    use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Label;  use Gtk.Label;
with Glib.Main;  use Glib.Main;
with Gtk.Widget; use Gtk.Widget;
with Gtk;        use Gtk;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Common;     use Common;

with Task_Worker;
with Gtk.Handlers;

package body Create_Task_Monitor is

   use type Ada.Containers.Count_Type;

   type GUI_Type is record
      Label : Gtk_Label;
      Progress_Bar : Gtk_Progress_Bar;
   end record;

   package GUI_Timeout is new Glib.Main.Generic_Sources (GUI_Type);
   Timeout : G_Source_Id;

   package GUI_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, GUI_Type);

   function Timeout_Test (GUI : GUI_Type) return Boolean is
   begin
      --  Pulse the progress bar
      GUI.Progress_Bar.Pulse;

      while Task_Worker.Queue.Current_Use > 0 loop
         --  There is data to display coming from the tasks!
         declare
            Data : Task_Worker.Work_Item;
         begin
            Task_Worker.Queue.Dequeue (Data);
            Set_Text (GUI.Label, To_String (Data.Some_Data));
         end;
      end loop;

      return True;
   end Timeout_Test;

   procedure On_Start_Clicked
     (Widget : access Gtk_Widget_Record'Class;
      GUI : GUI_Type) is
   begin
      --  If there is no timeout registered to monitor the tasks,
      --  start one now!
      if Timeout = 0 then
         Timeout := GUI_Timeout.Timeout_Add
           (
            --  This timeout will refresh every 50 ms
            50,
            --  This is the function to call in the timeout
            Timeout_Test'Access,
            --  This is the part of the GUI to refresh
            GUI);
      end if;

      Task_Worker.Run_Task;
   end On_Start_Clicked;

   procedure Stop_Timeout (Object : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Object);
   begin
      if Timeout /= 0 then
         Remove (Timeout);
         Timeout := 0;
      end if;
   end Stop_Timeout;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Button   : Gtk_Button;
      Box      : Gtk_Box;
      GUI      : GUI_Type;
   begin
      Set_Label (Frame, "Task_Monitor");
      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 0);
      Add (Frame, Box);

      --  Create a "start" button
      Gtk_New (Button, "start");
      Button.Set_Can_Default (True);
      Pack_Start (Box, Button, False, False, 0);

      --  Add a progress bar, to show that the UI is functioning when
      --  the tasks are working.
      Gtk_New (GUI.Progress_Bar);
      GUI.Progress_Bar.Set_Pulse_Step (0.01);
      Pack_Start (Box, GUI.Progress_Bar, False, False, 10);

      --  This is the label that will be refreshed to contain the data
      --  coming from the tasks.
      Gtk_New (GUI.Label, "press start to start tasks");
      Pack_Start (Box, GUI.Label, False, False, 10);

      --  Connect the start button
      GUI_Handler.Connect
        (Button, "clicked", On_Start_Clicked'Access, User_Data => GUI);

      --  Cleanup the timeout when the view is destroyed
      Widget_Handler.Connect (Box, "destroy", Stop_Timeout'Access);

      Show_All (Frame);
   end Run;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This shows how to use tasking in GtkAda, performing blocking"
        & " work in a task without blocking the UI.";
   end Help;

end Create_Task_Monitor;
