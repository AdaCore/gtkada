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
-- Library General Public License for more details.                  --
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

package body Gdk.Region is


   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (Region : in out Gdk_Region) is
      procedure Internal (Region : in System.Address);
      pragma Import (C, Internal, "gdk_region_destroy");
   begin
      Internal (Get_Object (Region));
      Set_Object (Region, System.Null_Address);
   end Destroy;


   -------------
   --  Empty  --
   -------------

   function Empty (Region : in Gdk_Region) return Boolean is
      function Internal (Region : in System.Address) return Gboolean;
      pragma Import (C, Internal, "gdk_region_empty");
   begin
      return To_Boolean (Internal (Get_Object (Region)));
   end Empty;


   -------------
   --  Equal  --
   -------------

   function Equal (Region1, Region2 : in Gdk_Region) return Boolean is
      function Internal (Region1, Region2 : in System.Address) return Gboolean;
      pragma Import (C, Internal, "gdk_region_equal");
   begin
      return To_Boolean (Internal (Get_Object (Region1),
                                   Get_Object (Region2)));
   end Equal;


   ---------------
   --  Gdk_New  --
   ---------------

   procedure Gdk_New (Region : out Gdk_Region) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_region_new");
   begin
      Set_Object (Region, Internal);
   end Gdk_New;


   ---------------
   --  Gdk_Xor  --
   ---------------

   procedure Gdk_Xor (Result  :     out Gdk_Region;
                      Source1 : in      Gdk_Region;
                      Source2 : in      Gdk_Region) is
      function Internal (Source1, Source2 : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gdk_regions_xor");
   begin
      Set_Object (Result, Internal (Get_Object (Source1),
                                    Get_Object (Source2)));
   end Gdk_Xor;


   -----------------
   --  Intersect  --
   -----------------

   procedure Intersect (Result  :    out Gdk_Region;
                        Source1 : in     Gdk_Region;
                        Source2 : in     Gdk_Region) is
      function Internal (Source1, Source2 : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gdk_regions_intersect");
   begin
      Set_Object (Result, Internal (Get_Object (Source1),
                                    Get_Object (Source2)));
   end Intersect;


   --------------
   --  Offset  --
   --------------

   procedure Offset (Region : in out Gdk_Region;
                     Dx     : in     Gint;
                     Dy     : in     Gint) is
      procedure Internal (Region : in System.Address;
                          Dx, Dy : in Gint);
      pragma Import (C, Internal, "gdk_region_offset");
   begin
      Internal (Get_Object (Region), Dx, Dy);
   end Offset;


   ----------------
   --  Point_In  --
   ----------------

   function Point_In (Region : in Gdk_Region;
                      X, Y   : in Integer) return Boolean is
      function Internal (Region : in System.Address; X, Y : in Integer)
                         return Gboolean;
      pragma Import (C, Internal, "gdk_region_point_in");
   begin
      return To_Boolean (Internal (Get_Object (Region), X, Y));
   end Point_In;


   ---------------
   --  Polygon  --
   ---------------

   procedure Polygon (Region    :    out Gdk_Region;
                      Points    : in     Point.Gdk_Points_Array;
                      Fill_Rule : in     Types.Gdk_Fill_Rule) is
      function Internal (Points  : in Point.Gdk_Points_Array;
                         Npoints : in Gint;
                         Fill_Rule : in Types.Gdk_Fill_Rule)
                         return System.Address;
      pragma Import (C, Internal, "gdk_region_polygon");
   begin
      Set_Object (Region, Internal (Points, Points'Length, Fill_Rule));
   end Polygon;


   ---------------
   --  Rect_In  --
   ---------------

   function Rect_In (Region : in Gdk_Region;
                     Rect   : in Rectangle.Gdk_Rectangle)
                     return Types.Gdk_Overlap_Type is
      function Internal (Region : in System.Address;
                         Rect   : in System.Address)
                         return Types.Gdk_Overlap_Type;
      pragma Import (C, Internal, "gdk_region_rect_in");
   begin
      return Internal (Get_Object (Region), Get_Object (Rect));
   end Rect_In;


   --------------
   --  Shrink  --
   --------------

   procedure Shrink (Region : in out Gdk_Region;
                     Dx     : in     Gint;
                     Dy     : in     Gint) is
      procedure Internal (Region : in System.Address; Dx, Dy : in Gint);
      pragma Import (C, Internal, "gdk_region_shrink");
   begin
      Internal (Get_Object (Region), Dx, Dy);
   end Shrink;


   -----------------
   --  Substract  --
   -----------------

   procedure Substract (Result  :     out Gdk_Region;
                        Source1 : in      Gdk_Region;
                        Source2 : in      Gdk_Region) is
      function Internal (Source1, Source2 : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gdk_regions_subtract");
   begin
      Set_Object (Result, Internal (Get_Object (Source1),
                                    Get_Object (Source2)));
   end Substract;


   -------------
   --  Union  --
   -------------

   procedure Union (Result  :     out Gdk_Region;
                    Source1 : in      Gdk_Region;
                    Source2 : in      Gdk_Region) is
      function Internal (Source1, Source2 : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gdk_regions_union");
   begin
      Set_Object (Result, Internal (Get_Object (Source1),
                                    Get_Object (Source2)));
   end Union;


   -----------------------
   --  Union_With_Rect  --
   -----------------------

   procedure Union_With_Rect (Result :    out Gdk_Region;
                              Region : in     Gdk_Region;
                              Rect   : in     Rectangle.Gdk_Rectangle) is
      function Internal (Region, Rect : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gdk_region_union_with_rect");
   begin
      Set_Object (Result, Internal (Get_Object (Region), Get_Object (Rect)));
   end Union_With_Rect;


end Gdk.Region;
