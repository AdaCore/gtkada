-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                Copyright (C) 2000-2011, AdaCore                   --
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

with Gtk.Enums;          use Gtk.Enums;
with Gtkada.File_Selection; use Gtkada.File_Selection;
with Gtk.Widget;         use Gtk.Widget;
with Gtk;                use Gtk;

with Ada.Text_IO;

package body Create_File_Selection is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "the @Gtkada.File_Selection_Dialog@B is a dialog to ask the user"
        & " for a file name. It can also give access to basic file and"
        & " directory manipulation, such as create, rename, delete.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      pragma Unreferenced (Frame);
   begin
      Ada.Text_IO.Put_Line
        ("Selected: "
         & Gtkada.File_Selection.File_Selection_Dialog
           (Title       => "Select an existing file",
            Must_Exist  => True));
   end Run;

end Create_File_Selection;
