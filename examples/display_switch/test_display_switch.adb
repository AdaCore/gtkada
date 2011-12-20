------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

--  This program demonstrates how to switch between displays under X11.

with GNAT.OS_Lib;

with Gtk.Main;

with Gdk.Display; use Gdk.Display;
with Gdk.Display_Manager; use Gdk.Display_Manager;

with Gtk.Window; use Gtk.Window;
with Gtk.Button; use Gtk.Button;
with Gtkada.Handlers; use Gtkada.Handlers;

with Ada.Text_IO; use Ada.Text_IO;
with Gtk.Widget;

procedure Test_Display_Switch is
   Window : Gtk_Window;
   Button : Gtk_Button;

   procedure On_Click
     (Button : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Destroy (Window);
      Gtk.Main.Main_Quit;
   end On_Click;

begin
   Put_Line ("Enter the display used to initialize GtkAda: ");

   declare
      S : constant String := Get_Line;
   begin
      Put_Line ("Initializing with '" & S & "'");

      GNAT.OS_Lib.Setenv ("DISPLAY", S);

      --  Initialize GtkAda.
      Gtk.Main.Set_Locale;
      Gtk.Main.Init;

      Put_Line ("Initialization done.");
   end;

   New_Line;

   loop
      Put_Line ("Enter the display used to display the window: ");
      declare
         S    : constant String := Get_Line;
         Disp : Gdk.Display.Gdk_Display;
      begin
         Put_Line ("Displaying on '" & S & "'");

         --  Change the display
         Disp := Gdk.Display.Open (S);
         Gdk.Display_Manager.Set_Default_Display
           (Display_Manager_Get,  Disp);

         --  Create a window with a button
         Gtk_New (Window);
         Gtk_New (Button,
                  "hello on display '"
                  & S &  "' (click to select another display");
         Add (Window, Button);

         --  Connect the button to the callback
         Gtkada.Handlers.Widget_Callback.Connect
           (Button, "clicked", On_Click'Unrestricted_Access);

         --  Show the window
         Show_All (Window);

         --  Launch a main loop.
         Gtk.Main.Main; --  The program waits here until the button is pressed
      end;
   end loop;
end Test_Display_Switch;
