with Glib; use Glib;

package Gdk.Point is

   type Gdk_Point is new Root_Type with private;

   type Gdk_Points_Array is array (Positive range <>) of Gdk_Point;

   procedure Gdk_New (Point : out Gdk_Point);

   procedure Gdk_New (Point :    out Gdk_Point;
                      X     : in     Gint16;
                      Y     : in     Gint16);

   procedure Set_Coordinates (Point : in out Gdk_Point;
                              X     : in     Gint16;
                              Y     : in     Gint16);

   function Get_X (Point : in Gdk_Point) return Gint16;

   function Get_Y (Point : in Gdk_Point) return Gint16;

   procedure Destroy (Point : in out Gdk_Point);

private

   type Gdk_Point is new Root_Type with null record;

end Gdk.Point;
