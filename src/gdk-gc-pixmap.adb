-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

package body Gdk.GC.Pixmap is

   -------------------
   --  Set_Stipple  --
   -------------------

   procedure Set_Stipple (GC      : in out Gdk_GC;
                          Stipple : in     Gdk.Pixmap.Gdk_Pixmap) is
      procedure Internal (GC, Stipple : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_stipple");
   begin
      Internal (Get_Object (GC), Get_Object (Stipple));
   end Set_Stipple;

   ----------------
   --  Set_Tile  --
   ----------------

   procedure Set_Tile (GC   : in out Gdk_GC;
                       Tile : in     Gdk.Pixmap.Gdk_Pixmap) is
      procedure Internal (GC, Tile : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_tile");
   begin
      Internal (Get_Object (GC), Get_Object (Tile));
   end Set_Tile;

end Gdk.GC.Pixmap;
