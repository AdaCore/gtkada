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

package body Gdk.Point is

   -------------
   --  Get_X  --
   -------------

   function Get_X (Point : in Gdk_Point) return Gint16 is
   begin
      return Point.X;
   end Get_X;

   -------------
   --  Get_Y  --
   -------------

   function Get_Y (Point : in Gdk_Point) return Gint16 is
   begin
      return Point.Y;
   end Get_Y;

   -------------
   --  Set_X  --
   -------------

   procedure Set_X (Point : in out Gdk_Point; X : Gint16) is
   begin
      Point.X := X;
   end Set_X;

   -----------
   -- Set_Y --
   -----------

   procedure Set_Y (Point : in out Gdk_Point; Y : Gint16) is
   begin
      Point.Y := Y;
   end Set_Y;

   function Ada_Gdk_Point_Size return Guint;
   pragma Import (C, Ada_Gdk_Point_Size);
begin
   pragma Assert (Gdk_Point'Size / 8 = Ada_Gdk_Point_Size);
   null;
end Gdk.Point;
