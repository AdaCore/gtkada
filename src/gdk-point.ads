-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
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

with Glib; use Glib;

package Gdk.Point is

   type Gdk_Point is private;

   type Gdk_Points_Array is array (Positive range <>) of Gdk_Point;

   function Get_X (Point : in Gdk_Point) return Gint16;
   function Get_Y (Point : in Gdk_Point) return Gint16;
   procedure Set_X (Point : in out Gdk_Point; X : Gint16);
   procedure Set_Y (Point : in out Gdk_Point; Y : Gint16);

private

   type Gdk_Point is
      record
         X : Gint16;
         Y : Gint16;
      end record;
   pragma Pack (Gdk_Point);
   for Gdk_Point'Size use 32;

   pragma Inline (Get_X);
   pragma Inline (Get_Y);
   pragma Inline (Set_X);
   pragma Inline (Set_Y);

end Gdk.Point;
