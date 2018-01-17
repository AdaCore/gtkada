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

--  <group>Glib, the general-purpose library</group>

package Glib.Enums is

   type Glib_Traverse_Flags is mod 2 ** 32;
   Traverse_Leafs     : constant Glib_Traverse_Flags := 2 ** 0;
   Traverse_Non_Leafs : constant Glib_Traverse_Flags := 2 ** 1;
   Traverse_All       : constant Glib_Traverse_Flags :=
     Traverse_Leafs or Traverse_Non_Leafs;
   Traverse_Flags     : constant Glib_Traverse_Flags := 16#03#;

   type Glib_Traverse_Type is (In_Order, Pre_Order, Post_Order, Level_Order);
   pragma Convention (C, Glib_Traverse_Type);

end Glib.Enums;
