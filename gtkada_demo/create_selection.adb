------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

with Gtk.Frame;      use Gtk.Frame;
with Gtk.Button;     use Gtk.Button;
with Gtk.Clipboard;  use Gtk.Clipboard;
with Gtk.Text_Iter;   use Gtk.Text_Iter;
with Gtk.Text_View;   use Gtk.Text_View;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Box;        use Gtk.Box;
with Gtk.Handlers;   use Gtk.Handlers;
with Gtk.Label;      use Gtk.Label;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;

package body Create_Selection is

   type My_Button_Record is new Gtk_Button_Record with record
      Text      : Gtk_Text_Buffer;
      Text_View : Gtk_Text_View;
      Label     : Gtk_Label;
   end record;
   type My_Button is access all My_Button_Record'Class;

   package My_Button_Handler is new Gtk.Handlers.Callback
     (My_Button_Record);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This package demonstrates how to interact with the clipboard to"
        & " retrieve text copied from other applications" & ASCII.LF
        & "Try selecting some other text outside of this demo, and press"
        & " once again the button at the top.";
   end Help;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
      (Button : access My_Button_Record'Class)
   is
      Text : constant String := Wait_For_Text (Gtk.Clipboard.Get);
      Iter : Gtk_Text_Iter;
   begin
      --  For the text buffer, we query directly the clipboard
      Get_End_Iter (Button.Text, Iter);
      Insert (Button.Text, Iter, Text);
   end On_Button_Click;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box    : Gtk_Box;
      Button : My_Button;
      Scrolled : Gtk_Scrolled_Window;
   begin
      Set_Label (Frame, "Selection");

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      Button := new My_Button_Record;
      Initialize  (Button, "Paste the selection/clipboard");
      Pack_Start (Box, Button, Fill => False, Expand => False);
      My_Button_Handler.Connect (Button, "clicked", On_Button_Click'Access);

      --  The text

      Gtk_New (Scrolled);
      Pack_Start (Box, Scrolled, Fill => True, Expand => True);

      Gtk_New (Button.Text);
      Gtk_New (Button.Text_View, Button.Text);
      Add (Scrolled, Button.Text_View);

      Show_All (Frame);
   end Run;

end Create_Selection;
