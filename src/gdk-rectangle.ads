with Glib; use Glib;

package Gdk.Rectangle is

   type Gdk_Rectangle is new Root_Type with private;

   procedure Gdk_New (Rectangle : out Gdk_Rectangle);

   procedure Gdk_New (Rectangle :    out Gdk_Rectangle;
                      X         : in     Gint16;
                      Y         : in     Gint16;
                      Width     : in     Guint16;
                      Height    : in     Guint16);

   procedure Destroy (Rectangle : in out Gdk_Rectangle'Class);


   procedure Get_Values (Rectangle : in     Gdk_Rectangle;
                         X         :    out Gint16;
                         Y         :    out Gint16;
                         Width     :    out Guint16;
                         Height    :    out Guint16);

   function Get_X (Rectangle : in Gdk_Rectangle) return Gint16;

   function Get_Y (Rectangle : in Gdk_Rectangle) return Gint16;

   function Get_Width (Rectangle : in Gdk_Rectangle) return Guint16;

   function Get_Height (Rectangle : in Gdk_Rectangle) return Guint16;


   procedure Set_X (Rectangle : in out Gdk_Rectangle;
                    X         : in     Gint16);

   procedure Set_Y (Rectangle : in out Gdk_Rectangle;
                    Y         : in     Gint16);

   procedure Set_Width (Rectangle : in out Gdk_Rectangle;
                        Width     : in     Guint16);

   procedure Set_Height (Rectangle : in out Gdk_Rectangle;
                         Height    : in     Guint16);

   procedure Set_Values (Rectangle : in out Gdk_Rectangle;
                         X         : in     Gint16;
                         Y         : in     Gint16;
                         Width     : in     Guint16;
                         Height    : in     Guint16);

   procedure Intersect (Src1      : in     Gdk_Rectangle;
                        Src2      : in     Gdk_Rectangle;
                        Dest      : in out Gdk_Rectangle;
                        Intersect :    out Boolean);
   --  mapping: Intersect gdk.h gdk_rectangle_intersect
   --
   --  NOTE : Dest needs to be allocated first.
private

   type Gdk_Rectangle is new Root_Type with null record;

end Gdk.Rectangle;
