------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2018, AdaCore                     --
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
