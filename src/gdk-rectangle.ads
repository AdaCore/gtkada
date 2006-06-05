-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <group>Gdk, the low-level API</group>
with Glib; use Glib;

package Gdk.Rectangle is

   subtype GRectangle_Coord is Gint;
   subtype GRectangle_Length is Gint;

   type Gdk_Rectangle is record
      X      : GRectangle_Coord;
      Y      : GRectangle_Coord;
      Width  : GRectangle_Length;
      Height : GRectangle_Length;
   end record;
   pragma Convention (C, Gdk_Rectangle);

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
