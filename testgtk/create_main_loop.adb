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
with Gtk.Main;   use Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Gtk;        use Gtk;
with Common;     use Common;

with Ada.Text_IO;

package body Create_Main_Loop is

   ------------------
   -- Loop_Destroy --
   ------------------

   procedure Loop_Destroy (Win : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Win);
   begin
      Main_Quit;
   end Loop_Destroy;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo creates a second event loop. All the events are now"
        & " processed in this second loop. You start this second loop by"
        & " calling again the @bGtk.Main.Main@B function. The interesting"
        & " side effect is that this procedure call is blocking until"
        & " the procedure @bMain_Quit@B is called. Thus, you can prevent"
        & " your program from exiting a given function until some condition"
        & " is met.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Label  : Gtk_Label;
      Button : Gtk_Button;
      Box    : Gtk_Box;

   begin
      Set_Label (Frame, "Test Main Loop");

      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 0);
      Add (Frame, Box);

      Gtk_New (Label, "In recursive main loop...");
      Set_Padding (Label, 20, 20);

      Pack_Start (Box, Label, False, False, 0);

      Gtk_New (Button, "Leave one instance of the main loop");
      Pack_Start (Box, Button, False, False, 0);
      Widget_Handler.Connect (Button, "clicked", Loop_Destroy'Access);
      Button.Set_Can_Default (True);
      Grab_Default (Button);

      Show_All (Frame);
      Ada.Text_IO.Put_Line
        ("Create_Mainloop: start (and block in the current function)");
      Gtk.Main.Main;
      Ada.Text_IO.Put_Line
        ("Create_Mainloop: done (leave the initial function");
      Ada.Text_IO.Put_Line
        ("Clicking again on the button might leave testgtk itself.");
   end Run;

end Create_Main_Loop;
