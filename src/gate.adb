-----------------------------------------------------------------------
--                   Gate - GtkAda Components                        --
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
   Project_Node           : Node_Ptr;
   Interface_Node         : Node_Ptr;
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

      --  With Glade-2 there are two files. file.gladep is the project
      --  file while file.glade is the interface to be translated into
      --  Ada code. file.gladep will be removed in Glade-3

      --  Need to open both files.

      if not GNAT.OS_Lib.Is_Regular_File (Argument (Arg) & "p") then
         Put_Line (Argument (Arg) & "p is not a regular file");
         Set_Exit_Status (2);
         return;
      end if;

      if not GNAT.OS_Lib.Is_Regular_File (Argument (Arg)) then
         Put_Line (Argument (Arg) & " is not a regular file");
         Set_Exit_Status (2);
         return;
      end if;

      Interface_Node := Parse (Argument (Arg));
      Project_Node := Parse (Argument (Arg) & "p");

      if Flag_Project or else Flag_Source_Directory
        or else Flag_Pixmaps_Directory
      then
         if Flag_Project then
            S := Get_Field (Project_Node, "program_name");

            if S = null then
               Put_Line ("<no_name>");
            else
               Put_Line (S.all);
            end if;
         end if;

         if Flag_Source_Directory then
            S := Get_Field (Project_Node, "source_directory");

            if S = null then
               Put_Line ("src");
            else
               Put_Line (S.all);
            end if;
         end if;

         if Flag_Pixmaps_Directory then
            S := Get_Field (Project_Node,
              "pixmaps_directory");

            if S = null then
               Put_Line ("pixmaps");
            else
               Put_Line (S.all);
            end if;
         end if;

      else
         Gtk.Main.Init;
         Generate (Project_Node, Interface_Node);
      end if;
   end if;

exception
   when System.Assertions.Assert_Failure =>
      Put_Line ("GATE: the XML file seems to be corrupted. " &
        "Please check it");
      Put_Line ("up manually, and try again");
      Set_Exit_Status (2);

   when E : others =>
      Put_Line ("Exception = " & Exception_Name (E));
      Put_Line ("GATE: Internal error. " &
        "Please send a bug report with the XML");
      Put_Line ("file " & Argument (Arg) &
        " and the GtkAda version to " &
        "gtkada@lists.act-europe.fr");
      Set_Exit_Status (2);
end Gate;
