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

with System;

package body Gdk.Region is

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : in Gdk_Region) return Boolean is
      function Internal (Region1, Region2 : in Gdk_Region) return Gboolean;
      pragma Import (C, Internal, "gdk_region_equal");

   begin
      return Boolean'Val (Internal (Left, Right));
   end "=";

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Region : in out Gdk_Region) is
      procedure Internal (Region : in Gdk_Region);
      pragma Import (C, Internal, "gdk_region_destroy");

   begin
      Internal (Region);
      Region := Null_Region;
   end Destroy;

   -----------
   -- Empty --
   -----------

   function Empty (Region : in Gdk_Region) return Boolean is
      function Internal (Region : in Gdk_Region) return Gboolean;
      pragma Import (C, Internal, "gdk_region_empty");

   begin
      return Boolean'Val (Internal (Region));
   end Empty;

   -----------------
   -- Get_Clipbox --
   -----------------

   procedure Get_Clipbox
     (Region    : in     Gdk_Region;
      Rectangle :    out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
        (Region    : in Gdk_Region;
         Rectangle : in System.Address);
      pragma Import (C, Internal, "gdk_region_get_clipbox");

      Rec : aliased Gdk.Rectangle.Gdk_Rectangle;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      Internal (Region, Rec'Address);
      Rectangle := Rec;
   end Get_Clipbox;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Region : out Gdk_Region) is
      function Internal return Gdk_Region;
      pragma Import (C, Internal, "gdk_region_new");

   begin
      Region := Internal;
   end Gdk_New;

   -------------
   -- Gdk_Xor --
   -------------

   procedure Gdk_Xor
     (Result  :     out Gdk_Region;
      Source1 : in      Gdk_Region;
      Source2 : in      Gdk_Region)
   is
      function Internal (Source1, Source2 : in Gdk_Region) return Gdk_Region;
      pragma Import (C, Internal, "gdk_regions_xor");

   begin
      Result := Internal (Source1, Source2);
   end Gdk_Xor;

   ---------------
   -- Intersect --
   ---------------

   procedure Intersect
     (Result  :    out Gdk_Region;
      Source1 : in     Gdk_Region;
      Source2 : in     Gdk_Region)
   is
      function Internal (Source1, Source2 : in Gdk_Region) return Gdk_Region;
      pragma Import (C, Internal, "gdk_regions_intersect");

   begin
      Result := Internal (Source1, Source2);
   end Intersect;

   --------------
   -- Point_In --
   --------------

   function Point_In
     (Region : in Gdk_Region;
      X, Y   : in Integer) return Boolean
   is
      function Internal
        (Region : in Gdk_Region; X, Y : in Integer) return Gboolean;
      pragma Import (C, Internal, "gdk_region_point_in");

   begin
      return Boolean'Val (Internal (Region, X, Y));
   end Point_In;

   -------------
   -- Polygon --
   -------------

   procedure Polygon
     (Region    :    out Gdk_Region;
      Points    : in     Gdk.Types.Gdk_Points_Array;
      Fill_Rule : in     Types.Gdk_Fill_Rule)
   is
      function Internal
        (Points  : in Gdk.Types.Gdk_Points_Array;
         Npoints : in Gint;
         Fill_Rule : in Types.Gdk_Fill_Rule) return Gdk_Region;
      pragma Import (C, Internal, "gdk_region_polygon");

   begin
      Region := Internal (Points, Points'Length, Fill_Rule);
   end Polygon;

   -------------
   -- Rect_In --
   -------------

   function Rect_In
     (Region : in Gdk_Region;
      Rect   : in Rectangle.Gdk_Rectangle) return Types.Gdk_Overlap_Type
   is
      function Internal
        (Region : in Gdk_Region;
         Rect   : in System.Address) return Types.Gdk_Overlap_Type;
      pragma Import (C, Internal, "gdk_region_rect_in");

      Rectangle : aliased Gdk.Rectangle.Gdk_Rectangle := Rect;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      return Internal (Region, Rectangle'Address);
   end Rect_In;

   ---------------
   -- Substract --
   ---------------

   procedure Substract
     (Result  :     out Gdk_Region;
      Source1 : in      Gdk_Region;
      Source2 : in      Gdk_Region)
   is
      function Internal (Source1, Source2 : in Gdk_Region) return Gdk_Region;
      pragma Import (C, Internal, "gdk_regions_subtract");

   begin
      Result := Internal (Source1, Source2);
   end Substract;

   -----------
   -- Union --
   -----------

   procedure Union
     (Result  :     out Gdk_Region;
      Source1 : in      Gdk_Region;
      Source2 : in      Gdk_Region)
   is
      function Internal (Source1, Source2 : in Gdk_Region) return Gdk_Region;
      pragma Import (C, Internal, "gdk_regions_union");

   begin
      Result := Internal (Source1, Source2);
   end Union;

   ---------------------
   -- Union_With_Rect --
   ---------------------

   procedure Union_With_Rect
     (Result :    out Gdk_Region;
      Region : in     Gdk_Region;
      Rect   : in     Rectangle.Gdk_Rectangle)
   is
      function Internal
        (Region : in Gdk_Region;
         Rect   : in System.Address) return Gdk_Region;
      pragma Import (C, Internal, "gdk_region_union_with_rect");

      Rectangle : aliased Gdk.Rectangle.Gdk_Rectangle := Rect;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

   begin
      Result := Internal (Region, Rectangle'Address);
   end Union_With_Rect;

end Gdk.Region;
