with Glib; use Glib;
with Gdk.Point;
with Gdk.Rectangle;
with Gdk.Types;

package Gdk.Region is

   type Gdk_Region is new Root_Type with private;


   procedure Gdk_New (Region : out Gdk_Region);
   --  mapping: Gdk_New gdk.h gdk_region_new

   procedure Destroy (Region : in out Gdk_Region);
   --  mapping: Destroy gdk.h gdk_region_destroy

   function Empty (Region : in Gdk_Region) return Boolean;
   --  mapping: Empty gdk.h gdk_region_empty

   function Equal (Region1, Region2 : in Gdk_Region) return Boolean;
   --  mapping: Equal gdk.h gdk_region_equal

   function Point_In (Region : in Gdk_Region;
                      X, Y   : in Integer) return Boolean;
   --  mapping: Point_In gdk.h gdk_region_point_in

   function Rect_In (Region : in Gdk_Region;
                     Rect   : in Rectangle.Gdk_Rectangle)
                     return Types.Gdk_Overlap_Type;
   --  mapping: Rect_In gdk.h gdk_region_rect_in

   procedure Polygon (Region :    out Gdk_Region;
                      Points : in     Point.Gdk_Points_Array;
                      Fill_Rule : in     Types.Gdk_Fill_Rule);
   --  mapping: Polygon gdk.h gdk_region_polygon

   procedure Offset (Region : in out Gdk_Region;
                     Dx     : in     Gint;
                     Dy     : in     Gint);
   --  mapping: Offset gdk.h gdk_region_offset

   procedure Shrink (Region : in out Gdk_Region;
                     Dx     : in     Gint;
                     Dy     : in     Gint);
   --  mapping: Shrink gdk.h gdk_region_shrink

   procedure Union_With_Rect (Result :    out Gdk_Region;
                              Region : in     Gdk_Region;
                              Rect   : in     Rectangle.Gdk_Rectangle);
   --  mapping: Union_With_Rect gdk.h gdk_region_union_with_rect

   procedure Intersect (Result  :    out Gdk_Region;
                        Source1 : in     Gdk_Region;
                        Source2 : in     Gdk_Region);
   --  mapping: Intersect gdk.h gdk_regions_intersect

   procedure Union (Result  :     out Gdk_Region;
                    Source1 : in      Gdk_Region;
                    Source2 : in      Gdk_Region);
   --  mapping: Union gdk.h gdk_regions_union

   procedure Substract (Result  :     out Gdk_Region;
                        Source1 : in      Gdk_Region;
                        Source2 : in      Gdk_Region);
   --  mapping: Substract gdk.h gdk_regions_subtract

   procedure Gdk_Xor (Result  :     out Gdk_Region;
                      Source1 : in      Gdk_Region;
                      Source2 : in      Gdk_Region);
   --  mapping: Gdk_Xor gdk.h gdk_regions_xor

private

   type Gdk_Region is new Root_Type with null record;

end Gdk.Region;
