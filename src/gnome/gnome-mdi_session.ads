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

with Gnome.MDI;
with Gnome.MDI_Child;

package Gnome.MDI_Session is

   type Gnome_MDI_Child_Creator is access
     function (Str : String) return Gnome.MDI_Child.Gnome_MDI_Child;
   --  This function should parse the config string and return a newly
   --  created GnomeMDIChild.

   function MDI_Restore_State
     (MDI               : access Gnome.MDI.Gnome_MDI_Record'Class;
      Section           : String;
      Child_Create_Func : Gnome_MDI_Child_Creator) return Boolean;

   procedure MDI_Save_State
     (MDI     : access Gnome.MDI.Gnome_MDI_Record'Class;
      Section : String);

end Gnome.MDI_Session;
