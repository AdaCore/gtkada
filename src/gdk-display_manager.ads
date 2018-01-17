------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with Gdk.Display;  use Gdk.Display;

package Gdk.Display_Manager is

   type Display_Manager is new Glib.C_Proxy;

   function Display_Manager_Get return Display_Manager;
   --  Return the global Display Manager.
   --  This function must be called after the initial call to Gtk.Main.Init.

   procedure Set_Default_Display
     (Manager : Display_Manager;
      Display : Gdk_Display);
   --  Set Display as the default display.

private
   pragma Import (C, Display_Manager_Get, "gdk_display_manager_get");

end Gdk.Display_Manager;
