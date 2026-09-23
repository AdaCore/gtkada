------------------------------------------------------------------------------
--               GtkAda - Ada binding for the Gimp Toolkit                  --
--                                                                          --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

with Ada.Strings.Fixed;
with GNAT.Strings;

with Gtk.Box;           use Gtk.Box;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Frame;         use Gtk.Frame;
with Gtk.Label;         use Gtk.Label;
with Gtk.Stack;         use Gtk.Stack;
with Gtk.Stack_Page;    use Gtk.Stack_Page;
with Gtk.Stack_Sidebar; use Gtk.Stack_Sidebar;
with Gtk.Widget;        use Gtk.Widget;

package body Create_Stack_Sidebar is

   Pages : constant GNAT.Strings.String_List :=
     (new String'("Welcome to GtkAda"),
      new String'("A Gtk_Stack_Sidebar"),
      new String'("Consists of a list of the stack's pages"),
      new String'("Which is used to switch between them"),
      new String'("Without the need of a Gtk_Stack_Switcher"),
      new String'("Enjoy!"));
   --  The page bodies, after upstream's sidebar.c. The sidebar labels its
   --  rows from the pages' titles, not from these.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGtk_Stack_Sidebar@B is the second way of driving a"
        & " @bGtk_Stack@B: rather than a row of tabs it presents the pages"
        & " as a vertical list, which suits a stack with more pages than a"
        & " @bGtk_Stack_Switcher@B can comfortably show."
        & ASCII.LF
        & "Like the switcher, it needs nothing but @bSet_Stack@B: the titles"
        & " come from each page's @bGtk_Stack_Page@B, and the list follows"
        & " the stack as pages are added and removed.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box     : Gtk_Box;
      Sidebar : Gtk_Stack_Sidebar;
      Stack   : Gtk_Stack;
      Label   : Gtk_Label;
      Page    : Gtk_Stack_Page with Unreferenced;
   begin
      Set_Label (Frame, "Stack Sidebar");

      Gtk_New (Box, Orientation_Horizontal, Spacing => 0);
      Frame.Set_Child (Box);

      Gtk_New (Sidebar);
      Box.Append (Sidebar);

      Gtk_New (Stack);
      Stack.Set_Hexpand (True);
      Stack.Set_Vexpand (True);
      Stack.Set_Transition_Type (Stack_Transition_Type_Slide_Up_Down);
      Box.Append (Stack);

      Sidebar.Set_Stack (Stack);

      for J in Pages'Range loop
         declare
            N : constant String :=
              Ada.Strings.Fixed.Trim
                (Integer'Image (J - Pages'First + 1), Ada.Strings.Left);
         begin
            Gtk_New (Label, Pages (J).all);
            Label.Set_Halign (Align_Center);
            Label.Set_Valign (Align_Center);

            Page := Stack.Add_Titled (Label, "page" & N, "Page " & N);
         end;
      end loop;
   end Run;

end Create_Stack_Sidebar;
