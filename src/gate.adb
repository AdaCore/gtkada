-----------------------------------------------------------------------
--                   GATE - GtkAda Components                        --
--                                                                   --
--                   Copyright (C) 1999-2000                         --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- GATE is free software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  GATE stands for Glade Ada Translator and Evaluator
--  Parse a Glade's XML project file and generate the corresponding Ada code

with Gtk.Main; use Gtk.Main;
with Gtk.Glade; use Gtk.Glade;
with Glib.Glade; use Glib; use Glib.Glade; use Glib.Glade.Glib_XML;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with Ada.Exceptions; use Ada.Exceptions;
with System.Assertions;

procedure Gate is
   N                      : Node_Ptr;
   S                      : String_Ptr;
   Arg                    : Natural;
   Flag_Project           : Boolean := False;
   Flag_Source_Directory  : Boolean := False;
   Flag_Pixmaps_Directory : Boolean := False;

   procedure Usage;

   procedure Usage is
   begin
      Put_Line ("Usage: " & Command_Name & " switches project-file");
      New_Line;
      Put_Line ("  -p    Output the program name and exit");
      Put_Line ("  -s    Output the name of the source directory and exit");
      Put_Line ("  -x    Output the name of the pixmaps directory and exit");
      Set_Exit_Status (1);
   end Usage;

begin
   if Argument_Count = 0 then
      Usage;
   else
      Arg := 1;

      if Argument (Arg) = "-p" then
         Flag_Project := True;
         Arg := Arg + 1;
      end if;

      if Argument (Arg) = "-s" then
         Flag_Source_Directory := True;
         Arg := Arg + 1;
      end if;

      if Argument (Arg) = "-x" then
         Flag_Pixmaps_Directory := True;
         Arg := Arg + 1;
      end if;

      if Arg > Argument_Count then
         Usage;
         return;
      end if;

      if not GNAT.OS_Lib.Is_Regular_File (Argument (Arg)) then
         Put_Line (Argument (Arg) & " is not a regular file");
         Set_Exit_Status (2);
         return;
      end if;

      N := Parse (Argument (Arg));

      if Flag_Project or else Flag_Source_Directory
        or else Flag_Pixmaps_Directory
      then
         if Flag_Project then
            S := Get_Field (Find_Tag (N.Child, "project"), "program_name");

            if S = null then
               Put_Line ("<no_name>");
            else
               Put_Line (S.all);
            end if;
         end if;

         if Flag_Source_Directory then
            S := Get_Field (Find_Tag (N.Child, "project"), "source_directory");

            if S = null then
               Put_Line ("<no_name>");
            else
               Put_Line (S.all);
            end if;
         end if;

         if Flag_Pixmaps_Directory then
            S := Get_Field (Find_Tag (N.Child, "project"),
              "pixmaps_directory");

            if S = null then
               Put_Line ("<no_name>");
            else
               Put_Line (S.all);
            end if;
         end if;

      else
         --  Instantiate (i.e create Gtk Widgets) N to ensure that the gtk+
         --  library will know about the internal structures of the generated
         --  widgets. This is needed in particular to retrieve the argument
         --  count and types for the various signals.

         Gtk.Main.Init;
         Instantiate (N, False);
         Reset_Tree (N);
         Generate (N);
      end if;
   end if;

exception
   when System.Assertions.Assert_Failure =>
      Put_Line ("GATE: the XML file seems to be corrupted. Please check it");
      Put_Line ("up manually, and try again");
      Set_Exit_Status (2);

   when E : others =>
      Put_Line ("Exception = " & Exception_Name (E));
      Put_Line ("GATE: Internal error. Please send a bug report with the XML");
      Put_Line ("file " & Argument (Arg) & " and the GtkAda version to " &
        "gtkada@ada.eu.org");
      Set_Exit_Status (2);
end Gate;
