------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtk.Box;                   use Gtk.Box;
with Gtk.File_Chooser;          use Gtk.File_Chooser;
with Gtk.File_Chooser_Button;   use Gtk.File_Chooser_Button;
with Gtk.File_Filter;           use Gtk.File_Filter;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Widget;                use Gtk.Widget;

package body Create_File_Chooser is

   -----------------
   -- Help_Button --
   -----------------

   function Help_Button return String is
   begin
      return "The Gtk_File_Chooser_Button is a widget that lets the user"
        & " select a file." & ASCII.LF
        & "It can exist in several modes, which influence its behavior.";
   end Help_Button;

   ----------------
   -- Run_Button --
   ----------------

   procedure Run_Button (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box    : Gtk_Box;
      Hbox   : Gtk_Box;
      File   : Gtk_File_Chooser_Button;
      Filter1, Filter2 : Gtk_File_Filter;
   begin
      Set_Label (Frame, "File Chooser Button");

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      --  File chooser

      Gtk_New (Filter1);
      Add_Pattern (Filter1, "*");
      Set_Name (Filter1, "All Files");

      Gtk_New (Filter2);
      Add_Pattern (Filter2, "*.ad[bs]");
      Set_Name (Filter2, "Ada Files");

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox, Fill => False, Expand => False);
      Gtk_New (File,
               Title   => "Select a file (Open mode)",
               Action  => Action_Open);
      Pack_Start (Hbox, File, Expand => True);

      File.Add_Filter (Filter1);
      File.Add_Filter (Filter2);

      --  Add a shortcut to the current directory

      if not File.Add_Shortcut_Folder (Get_Current_Dir) then
         Put_Line ("Got error when adding shortcut folder");
      end if;

      --  Directory chooser

      Gtk_New (Filter1);
      Add_Mime_Type (Filter1, "x-directory/normal");
      Set_Name (Filter1, "Directories only");

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox, Fill => False, Expand => False);
      Gtk_New (File,
               Title   => "Select a file (Open mode)",
               Action  => Action_Open);
      Pack_Start (Hbox, File, Expand => True);

      File.Set_Action (Action_Select_Folder);
      File.Add_Filter (Filter1);

      Show_All (Frame);
   end Run_Button;

end Create_File_Chooser;
