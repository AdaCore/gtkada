
with Gtk.Drawing_Area;

package Gtk.Curve is

   type Gtk_Curve is new Gtk.Drawing_Area.Gtk_Drawing_Area with private;
   type Gtk_Curve_Type is (Gtk_Curve_Type_Linear,
                           Gtk_Curve_Type_Spline,
                           Gtk_Curve_Type_Free);

   type Float_Array is array (Positive range <>) of Gfloat;
   type Curve_Vector (Vec_Len : Positive) is
      record
         Vector : Float_Array (1 .. Vec_Len);
      end record;

   procedure Get_Vector (Curve  : in Gtk_Curve'Class;
                         Vector : in out Curve_Vector);
   procedure Gtk_New (Widget : out Gtk_Curve);
   procedure Reset (Curve : in Gtk_Curve'Class);
   procedure Set_Curve_Type (Curve      : in Gtk_Curve'Class;
                             Curve_Type : in Gtk_Curve_Type);
   procedure Set_Gamma (Curve : in Gtk_Curve'Class; Gamma : in Gfloat);
   procedure Set_Range
     (Curve : in Gtk_Curve'Class;
      Min_X : in Gfloat;
      Max_X : in Gfloat;
      Min_Y : in Gfloat;
      Max_Y : in Gfloat);
   procedure Set_Vector (Curve  : in Gtk_Curve'Class;
                         Vector : in Curve_Vector);

private

   type Gtk_Curve is new Gtk.Drawing_Area.Gtk_Drawing_Area with null record;

   --  mapping: NOT_IMPLEMENTED gtkcurve.h gtk_curve_get_type
   --  mapping: Gtk_New  gtkcurve.h gtk_curve_new
   --  mapping: Reset gtkcurve.h gtk_curve_reset
   --  mapping: Set_Gamma gtkcurve.h gtk_curve_set_gamma
   --  mapping: Set_Range gtkcurve.h gtk_curve_set_range
   --  mapping: Get_Vector gtkcurve.h gtk_curve_get_vector
   --  mapping: Set_Vector gtkcurve.h gtk_curve_set_vector
   --  mapping: Set_Curve_Type gtkcurve.h gtk_curve_set_curve_type

end Gtk.Curve;
