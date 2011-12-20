------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
--  This is the root of the Gnome hierarchy.
--  It provides initialization routines.
--  </description>

pragma Warnings (Off);
with Glib.Object; use Glib.Object;
pragma Warnings (On);

package Gnome is
   pragma Elaborate_Body;

   function Init (App_Id : String; App_Version : String) return Boolean;
   --  Initialize Gnome.
   --  You should call this function before anything other gnome related
   --  actions.
   --  Return True in case of success, False otherwise.

   type Gnome_Preferences_Type is
     (Preferences_Never, Preferences_User, Preferences_Always);
   --  Do something never, only when the user wants, or always.

end Gnome;
