
package body Gtk.Gamma_Curve is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Gamma_Curve) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_gamma_curve_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ---------------
   -- Get_Curve --
   ---------------

   function Get_Curve (Widget : in Gtk_Gamma_Curve'Class)
                       return Gtk.Curve.Gtk_Curve
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gamma_curve_get_curve");

      Curve : Gtk.Curve.Gtk_Curve;
   begin
      Set_Object (Curve, Internal (Get_Object (Widget)));
      return Curve;
   end Get_Curve;

   ---------------
   -- Get_Gamma --
   ---------------

   function Get_Gamma (Widget : in Gtk_Gamma_Curve'Class)
                       return Gfloat
   is
      function Internal (Widget : System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_gamma_curve_get_gamma");
   begin
      return Internal (Get_Object (Widget));
   end Get_Gamma;


end Gtk.Gamma_Curve;
