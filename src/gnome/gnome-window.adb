------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with System;
with Gtk; use Gtk;

package body Gnome.Window is

   use Gtk.Window;

   ------------------------
   -- Toplevel_Set_Title --
   ------------------------

   procedure Toplevel_Set_Title
     (W         : access Gtk_Window_Record'Class;
      Doc_Name  : String;
      App_Name  : String;
      Extension : String)
   is
      procedure Internal
        (W         : System.Address;
         Doc_Name  : String;
         App_Name  : String;
         Extension : String);
      pragma Import (C, Internal, "gnome_window_toplevel_set_title");
   begin
      Internal (Get_Object (W),
                Doc_Name & ASCII.NUL,
                App_Name & ASCII.NUL,
                Extension & ASCII.NUL);
   end Toplevel_Set_Title;

end Gnome.Window;
