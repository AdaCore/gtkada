------------------------------------------------------------------------------
--               GtkAda - Ada binding for the Gimp Toolkit                  --
--                                                                          --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

with Glib;
with Glib.List_Store;
with Gtk.Button;
with Gtk.Frame;
with Gtk.Grid;

package Create_List_Store is

   type Store_UI is tagged record
      Store : Glib.List_Store.Glist_Store;
      Grid  : Gtk.Grid.Gtk_Grid;
   end record;
   type Store_Ptr is access all Store_UI'Class;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class);
   function Help return String;

end Create_List_Store;
