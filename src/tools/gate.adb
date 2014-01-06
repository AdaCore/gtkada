------------------------------------------------------------------------------
--                         Gate - GtkAda Components                         --
--                                                                          --
--                     Copyright (C) 1999-2014, AdaCore                     --
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
   procedure Usage;

   procedure Usage is
   begin
      Put_Line ("Usage: " & Command_Name & " project-file");
      New_Line;
      Set_Exit_Status (1);
   end Usage;

begin
   if Argument_Count /= 1 then
      Usage;
      return;
   end if;

   if not GNAT.OS_Lib.Is_Regular_File (Argument (1)) then
      Put_Line (Argument (1) & " is not a regular file");
      Set_Exit_Status (2);
      return;
   end if;

   Gtk.Main.Init;
   Generate (Argument (1));

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
      Put_Line ("file " & Argument (1) &
        " and the GtkAda version to " & "report@adacore.com");
      Set_Exit_Status (2);
end Gate;
