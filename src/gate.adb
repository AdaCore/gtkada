-----------------------------------------------------------------------
--                   GATE - GtkAda Components                        --
--                                                                   --
--                      Copyright (C) 1999                           --
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

with Gtk.Glade; use Gtk.Glade;
with Glib.Glade; use Glib; use Glib.Glade; use Glib.Glade.Glib_XML;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with Ada.Exceptions; use Ada.Exceptions;

procedure Gate is
   N                     : Node_Ptr;
   S                     : String_Ptr;
   Arg                   : Natural;
   Flag_Project          : Boolean := False;
   Flag_Source_Directory : Boolean := False;

   procedure Usage;

   procedure Usage is
   begin
      Put_Line ("Usage: " & Command_Name & " switches project-file");
      New_Line;
      Put_Line ("  -p    Output the name of the project and exit");
      Put_Line ("  -s    Output the name of the source directory and exit");
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

      if Flag_Project or else Flag_Source_Directory then
         if Flag_Project then
            S := Get_Field (Find_Tag (N.Child, "project"), "name");

            if S = null then
               New_Line;
            else
               Put_Line (S.all);
            end if;
         end if;

         if Flag_Source_Directory then
            S := Get_Field (Find_Tag (N.Child, "project"), "source_directory");

            if S = null then
               New_Line;
            else
               Put_Line (S.all);
            end if;
         end if;

      else
         Generate (N);
      end if;
   end if;

exception
   when E : others =>
      Put_Line ("Exception = " & Exception_Name (E));
      Put_Line ("GATE: Internal error. Please send a bug report with the XML");
      Put_Line ("file " & Argument (Arg) & " and the GtkAda version to " &
        "gtkada@ada.eu.org");
      Set_Exit_Status (2);
end Gate;
