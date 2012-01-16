------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
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

--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with Glib; use Glib;
with Gdk.Rectangle;
with Gdk.Types;

package Gdk.Region is

   subtype Gdk_Region is Gdk.Gdk_Region;
   Null_Region : constant Gdk_Region;

   type Gdk_Fill_Rule is (Even_Odd_Rule, Winding_Rule);
   --  GC fill rule for polygons.
   pragma Convention (C, Gdk_Fill_Rule);

   type Gdk_Overlap_Type is
     (Overlap_Rectangle_In,
      --  Rectangle is in region.

      Overlap_Rectangle_Out,
      --  Rectangle is not in region.

      Overlap_Rectangle_Part
      --  Rectangle is partially in region.
     );
   --  Types of overlapping between a rectangle and a region.
   pragma Convention (C, Gdk_Overlap_Type);

   procedure Gdk_New (Region : out Gdk_Region);
   --  Create a new region.

   procedure Polygon
     (Region    : out Gdk_Region;
      Points    : Gdk.Types.Gdk_Points_Array;
      Fill_Rule : Gdk_Fill_Rule);

   function Copy (Region : Gdk_Region) return Gdk_Region;

   function Rectangle
     (Rectangle : Gdk.Rectangle.Gdk_Rectangle) return Gdk_Region;

   procedure Destroy (Region : in out Gdk_Region);

   procedure Get_Clipbox
     (Region    : Gdk_Region;
      Rectangle : out Gdk.Rectangle.Gdk_Rectangle);

   procedure Get_Rectangles
     (Region       : Gdk_Region;
      Rectangle    : out Gdk.Rectangle.Gdk_Rectangle_Array;
      N_Rectangles : out Natural);

   function Empty (Region : Gdk_Region) return Boolean;

   function "=" (Left, Right : Gdk_Region) return Boolean;

   function Point_In (Region : Gdk_Region; X, Y : Integer) return Boolean;

   function Rect_In
     (Region : Gdk_Region;
      Rect   : Gdk.Rectangle.Gdk_Rectangle) return Gdk_Overlap_Type;

   procedure Offset
     (Region : Gdk_Region;
      Dx     : Gint;
      Dy     : Gint);

   procedure Shrink
     (Region : Gdk_Region;
      Dx     : Gint;
      Dy     : Gint);

   procedure Union_With_Rect
     (Region : in out Gdk_Region;
      Rect   : Gdk.Rectangle.Gdk_Rectangle);

   procedure Union_With_Rect
     (Result : in out Gdk_Region;
      Region : Gdk_Region;
      Rect   : Gdk.Rectangle.Gdk_Rectangle);
   --  Provided for backward compatibility.
   --  Region must be equal to Result.

   procedure Intersect
     (Source1 : in out Gdk_Region;
      Source2 : Gdk_Region);

   procedure Union
     (Source1 : in out Gdk_Region;
      Source2 : Gdk_Region);

   procedure Substract
     (Source1 : in out Gdk_Region;
      Source2 : Gdk_Region);

   procedure Gdk_Xor
     (Source1 : in out Gdk_Region;
      Source2 : Gdk_Region);

private
   Null_Region : constant Gdk_Region := null;

   pragma Import (C, Copy, "gdk_region_copy");
   pragma Import (C, Rectangle, "gdk_region_rectangle");
   pragma Import (C, Get_Clipbox, "gdk_region_get_clipbox");
   pragma Import (C, Rect_In, "gdk_region_rect_in");
   pragma Import (C, Offset, "gdk_region_offset");
   pragma Import (C, Shrink, "gdk_region_shrink");
   pragma Import (C, Intersect, "gdk_region_intersect");
   pragma Import (C, Union, "gdk_region_union");
   pragma Import (C, Substract, "gdk_region_subtract");
   pragma Import (C, Gdk_Xor, "gdk_region_xor");
end Gdk.Region;

--  missing:
--  gdk_region_spans_intersect_foreach
