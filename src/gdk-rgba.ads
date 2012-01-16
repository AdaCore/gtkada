------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
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
--  The #GdkRGBA struct is a convenient way to pass rgba colors around.
--  It's based on cairo's way to deal with colors and mirrors its behavior.
--  All values are in the range from 0.0 to 1.0 inclusive. So the color
--  (0.0, 0.0, 0.0, 0.0) represents transparent black and
--  (1.0, 1.0, 1.0, 1.0) is opaque white. Other values will be clamped
--  to this range when drawing.
--  </description>
--  <group>Gdk, the low-level API</group>

with Glib;

package Gdk.RGBA is

   type Gdk_RGBA is record
      Red   : Glib.Gdouble;
      Green : Glib.Gdouble;
      Blue  : Glib.Gdouble;
      Alpha : Glib.Gdouble;
   end record;
   pragma Convention (C, Gdk_RGBA);

end Gdk.RGBA;
