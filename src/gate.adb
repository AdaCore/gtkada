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
with Glib.Glade; use Glib.Glade; use Glib.Glade.Glib_XML;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Gate is
   N            : Node_Ptr;
   Arg          : Natural;
   Flag_Project : Boolean := False;

   procedure Usage;

   procedure Usage is
   begin
      Put_Line ("Usage: gate switches project-file");
      New_Line;
      Put_Line ("  -p    Output the name of the project and exit");
   end Usage;

begin
   --  ??? This is still a very simple minded program.

   if Argument_Count = 0 then
      Usage;
   else
      Arg := 1;

      if Argument (Arg) = "-p" then
         Flag_Project := True;
         Arg := Arg + 1;
      end if;

      if Arg > Argument_Count then
         Usage;
         return;
      end if;

      N := Parse (Argument (Arg));

      if Flag_Project then
         Put_Line (Get_Field (Find_Tag (N.Child, "project"), "name").all);
      else
         Generate (N);
      end if;
   end if;
end Gate;
