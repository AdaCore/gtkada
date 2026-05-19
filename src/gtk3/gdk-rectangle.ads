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

--  <group>Gdk, the low-level API</group>
with Glib; use Glib;
with Cairo.Region; use Cairo.Region;

package Gdk.Rectangle is

   subtype Gdk_Rectangle is Cairo_Rectangle_Int;

   type Gdk_Rectangle_Access is access all Gdk_Rectangle;
   pragma Convention (C, Gdk_Rectangle_Access);

   type Gdk_Rectangle_Array is array (Natural range <>) of Gdk_Rectangle;
   --  This type is used by Gdk.Region.

   Full_Area : constant Gdk_Rectangle;
   --  The constant above can be used in Gtk.Widget.Draw when you want to
   --  redraw the whole widget

   procedure Intersect
     (Src1      : Gdk_Rectangle;
      Src2      : Gdk_Rectangle;
      Dest      : out Gdk_Rectangle;
      Intersect : out Boolean);

   procedure Union
     (Src1 : Gdk_Rectangle;
      Src2 : Gdk_Rectangle;
      Dest : out Gdk_Rectangle);

private

   Full_Area : constant Gdk_Rectangle :=
     (Gint'First, Gint'First, Gint'Last, Gint'Last);

   pragma Import (C, Union, "gdk_rectangle_union");

end Gdk.Rectangle;
