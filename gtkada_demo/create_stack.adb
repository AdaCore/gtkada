------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
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
with Glib.Values;         use Glib.Values;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
--  with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Stack;           use Gtk.Stack;
with Gtk.Stack_Switcher;  use Gtk.Stack_Switcher;
with Gtk.Text_Buffer;     use Gtk.Text_Buffer;
with Gtk.Text_View;       use Gtk.Text_View;
with Gtk.Widget;          use Gtk.Widget;

package body Create_Stack is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Stack@B is a widget that only shows one of its children"
         & " at a time." & ASCII.LF
         & "It can be combined with a @bGtk_Stack_Switcher@B to allow"
         & " interactive switching.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box      : Gtk_Box;
      Switcher : Gtk_Stack_Switcher;
      Stack    : Gtk_Stack;
      W1       : Gtk_Text_View;
      W2       : Gtk_Button;
      --  Scrolled : Gtk_Scrolled_Window;
      V        : GValue;
   begin
      Gtk_New (Box, Orientation_Vertical, 0);
      Frame.Add (Box);

      Gtk_New (Switcher);
      Box.Pack_Start (Switcher, Expand => False, Fill => False);

      Gtk_New (Stack);
      Stack.Set_Transition_Duration (1500);  --  slower to better see them
      Stack.Set_Halign (Align_Start);
      Box.Add (Stack);

      Switcher.Set_Stack (Stack);

      Gtk_New (W1);
      W1.Get_Buffer.Set_Text ("This is a test");
      Stack.Add_Titled (W1, "Name:1", "Title:1");

      Gtk_New (W2, "A label");
      Stack.Add (W2);

      Init (V, GType_String);
      Set_String (V, "2");
      Stack.Child_Set_Property (W2, "name", V);
      Stack.Child_Set_Property (W2, "title", V);
      Unset (V);

      Init (V, GType_Boolean);
      Set_Boolean (V, True);
      Stack.Child_Set_Property (W2, "needs-attention", V);  --  See CSS file
      Unset (V);

      Frame.Show_All;
   end Run;
end Create_Stack;
