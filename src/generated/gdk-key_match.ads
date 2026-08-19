------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
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


pragma Warnings (Off, "*is already use-visible*");
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Gdk.Key_Match is

   type Gdk_Key_Match is (
      Gdk_Key_Match_None,
      Gdk_Key_Match_Partial,
      Gdk_Key_Match_Exact);
   pragma Convention (C, Gdk_Key_Match);
   --  Describes how well an event matches a given keyval and modifiers.
   --
   --  `GdkKeyMatch` values are returned by [methodGdk.KeyEvent.matches].

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Key_Match_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Key_Match);
   type Property_Gdk_Key_Match is new Gdk_Key_Match_Properties.Property;

end Gdk.Key_Match;
