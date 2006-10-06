-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2006, AdaCore                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Ada.Text_IO;               use Ada.Text_IO;
with Glib.Error;                use Glib.Error;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtk.Box;                   use Gtk.Box;
with Gtk.File_Chooser;          use Gtk.File_Chooser;
with Gtk.File_Chooser_Button;   use Gtk.File_Chooser_Button;
with Gtk.Frame;                 use Gtk.Frame;

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
      Button : Gtk_File_Chooser_Button;
      Error  : GError;
   begin
      Set_Label (Frame, "File Chooser Button");

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      Gtk_New (Button,
               Title   => "Select a file (Open mode)",
               Action  => Action_Open);
      Pack_Start (Box, Button, Fill => False);
      Error := Add_Shortcut_Folder (+Button, Get_Current_Dir);
      if Error /= null then
         Put_Line (Get_Message (Error));
      end if;

      Show_All (Frame);
   end Run_Button;

end Create_File_Chooser;
