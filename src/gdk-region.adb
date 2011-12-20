------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       ----                     Copyright (C) 1998-2012, AdaCore                     --
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

with System;

package body Gdk.Region is

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Gdk_Region) return Boolean is
      function Internal (Region1, Region2 : Gdk_Region) return Gboolean;
      pragma Import (C, Internal, "gdk_region_equal");

   begin
      return Boolean'Val (Internal (Left, Right));
   end "=";

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Region : in out Gdk_Region) is
      procedure Internal (Region : Gdk_Region);
      pragma Import (C, Internal, "gdk_region_destroy");

   begin
      Internal (Region);
      Region := Null_Region;
   end Destroy;

   -----------
   -- Empty --
   -----------

   function Empty (Region : Gdk_Region) return Boolean is
      function Internal (Region : Gdk_Region) return Gboolean;
      pragma Import (C, Internal, "gdk_region_empty");

   begin
      return Boolean'Val (Internal (Region));
   end Empty;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Region : out Gdk_Region) is
      function Internal return Gdk_Region;
      pragma Import (C, Internal, "gdk_region_new");

   begin
      Region := Internal;
   end Gdk_New;

   --------------------
   -- Get_Rectangles --
   --------------------

   procedure Get_Rectangles
     (Region       : Gdk_Region;
      Rectangle    : out Gdk.Rectangle.Gdk_Rectangle_Array;
      N_Rectangles : out Natural)
   is
      procedure Internal
        (Region       : Gdk_Region;
         Rectangle    : System.Address;
         N_Rectangles : out Natural);
      pragma Import (C, Internal, "gdk_region_get_rectangles");

   begin
      Internal (Region, Rectangle'Address, N_Rectangles);
   end Get_Rectangles;

   --------------
   -- Point_In --
   --------------

   function Point_In
     (Region : Gdk_Region; X, Y : Integer) return Boolean
   is
      function Internal (Region : Gdk_Region; X, Y : Integer) return Gboolean;
      pragma Import (C, Internal, "gdk_region_point_in");

   begin
      return Boolean'Val (Internal (Region, X, Y));
   end Point_In;

   -------------
   -- Polygon --
   -------------

   procedure Polygon
     (Region    : out Gdk_Region;
      Points    : Gdk.Types.Gdk_Points_Array;
      Fill_Rule : Gdk_Fill_Rule)
   is
      function Internal
        (Points    : Gdk.Types.Gdk_Points_Array;
         Npoints   : Gint;
         Fill_Rule : Gdk_Fill_Rule) return Gdk_Region;
      pragma Import (C, Internal, "gdk_region_polygon");

   begin
      Region := Internal (Points, Points'Length, Fill_Rule);
   end Polygon;

   ---------------------
   -- Union_With_Rect --
   ---------------------

   procedure Union_With_Rect
     (Region : in out Gdk_Region;
      Rect   : Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
        (Region : Gdk_Region;
         Rect   : Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gdk_region_union_with_rect");
      pragma Unmodified (Region);
   begin
      Internal (Region, Rect);
   end Union_With_Rect;

   procedure Union_With_Rect
     (Result : in out Gdk_Region;
      Region : Gdk_Region;
      Rect   : Gdk.Rectangle.Gdk_Rectangle) is
   begin
      pragma Assert (Region = Result);
      Union_With_Rect (Result, Rect);
   end Union_With_Rect;

end Gdk.Region;
