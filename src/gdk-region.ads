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
with Gdk.Point;
with Gdk.Rectangle;
with Gdk.Types;

package Gdk.Region is

   type Gdk_Region is new Root_Type with private;


   procedure Gdk_New (Region : out Gdk_Region);

   procedure Destroy (Region : in out Gdk_Region);

   function Empty (Region : in Gdk_Region) return Boolean;

   function "=" (Left, Right : in Gdk_Region) return Boolean;


   function Point_In (Region : in Gdk_Region;
                      X, Y   : in Integer) return Boolean;

   function Rect_In (Region : in Gdk_Region;
                     Rect   : in Rectangle.Gdk_Rectangle)
                     return Types.Gdk_Overlap_Type;

   procedure Polygon (Region :    out Gdk_Region;
                      Points : in     Point.Gdk_Points_Array;
                      Fill_Rule : in     Types.Gdk_Fill_Rule);

   procedure Offset (Region : in out Gdk_Region;
                     Dx     : in     Gint;
                     Dy     : in     Gint);

   procedure Shrink (Region : in out Gdk_Region;
                     Dx     : in     Gint;
                     Dy     : in     Gint);

   procedure Union_With_Rect (Result :    out Gdk_Region;
                              Region : in     Gdk_Region;
                              Rect   : in     Rectangle.Gdk_Rectangle);

   procedure Intersect (Result  :    out Gdk_Region;
                        Source1 : in     Gdk_Region;
                        Source2 : in     Gdk_Region);

   procedure Union (Result  :     out Gdk_Region;
                    Source1 : in      Gdk_Region;
                    Source2 : in      Gdk_Region);

   procedure Substract (Result  :     out Gdk_Region;
                        Source1 : in      Gdk_Region;
                        Source2 : in      Gdk_Region);

   procedure Gdk_Xor (Result  :     out Gdk_Region;
                      Source1 : in      Gdk_Region;
                      Source2 : in      Gdk_Region);

private

   type Gdk_Region is new Root_Type with null record;

end Gdk.Region;
