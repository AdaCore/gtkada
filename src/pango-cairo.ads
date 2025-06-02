------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2025, AdaCore                     --
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
--  Interfacing between Pango and Cairo.
--  </description>
--
--  <c_version>2.16.6</c_version>
--  <group>Cairo</group>

with Cairo; use Cairo;
with Pango.Layout; use Pango.Layout;

package Pango.Cairo is

   procedure Show_Layout (Cr : Cairo_Context; Layout : Pango_Layout);
   --  Draws a pango layout in the specified cairo context. The top-left corner
   --  of Layout will be drawn at the current point of the cairo context.

   procedure Set_Default_Font_Type (Font_Type : Standard.Cairo.Cairo_Font_Type);
   --  Sets the default fontmap used by default. This can be used to
   --  change the Cairo font backend that the default fontmap uses,
   --  for example. The old default font map is unreffed.
   --  Note that since Pango 1.32.6, the default fontmap is per-thread.
   --  This function only changes the default fontmap for the current thread.
   --  Default fontmaps of existing threads are not changed. Default fontmaps
   --  of any new threads will still be created using the default mechanism.

end Pango.Cairo;
