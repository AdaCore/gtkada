-----------------------------------------------------------------------
--                   GATE - GtkAda Components                        --
--                                                                   --
--               Copyright (C) 1999 Arnaud Charlet                   --
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
with Ada.Command_Line; use Ada.Command_Line;

procedure Gate is
begin
   --  ??? This is still a very simple minded program.

   if Argument_Count > 0 then
      Generate (Argument (1));
   end if;
end Gate;
