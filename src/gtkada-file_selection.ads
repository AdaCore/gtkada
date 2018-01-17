------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
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

--  <description>
--  This package provides a high level support for creating file selection
--  dialogs by handling the signals internally.
--  </description>
--  <group>Selectors</group>

with Glib;

package Gtkada.File_Selection is

   function File_Selection_Dialog
     (Title       : Glib.UTF8_String := "Select File";
      Default_Dir : String := "";
      Dir_Only    : Boolean := False;
      Must_Exist  : Boolean := False) return String;
   --  Open a file selection dialog and make it modal.
   --  Return when either the Cancel button is clicked or when a file is
   --  selected.
   --  Default_Dir is the directory to display in dialog initially. Note that
   --  it must end with a directory separator ('/' or '\', depending on your
   --  system). You can use GNAT.Os_Lib.Directory_Separator to get the correct
   --  value for your system.
   --  If Must_Exist is True, then the file (or directory if Dir_Only is True)
   --  must exist.
   --  If Dir_Only is True, then the dialog is modified so that the user can
   --  only choose a directory name, but not a file name.
   --  The value returned is the name of the file selected, or "" if none.

end Gtkada.File_Selection;
