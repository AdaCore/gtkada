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

with Gtk.Notebook;
with Gtk.Window;

package Main_Windows is

   type Main_Window_Record is new Gtk.Window.Gtk_Window_Record with
      record
         Notebook       : Gtk.Notebook.Gtk_Notebook;
      end record;
   type Main_Window is access all Main_Window_Record'Class;

   procedure Gtk_New (Win : out Main_Window);
   procedure Initialize (Win : access Main_Window_Record'Class);

   type Help_Function is access function return String;
   procedure Set_Help (Func : Help_Function);
   --  Set a new help function to be displayed

end Main_Windows;
