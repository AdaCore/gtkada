
package body Gtk.Curve is

   ----------------
   -- Get_Vector --
   ----------------

   procedure Get_Vector (Curve  : in Gtk_Curve'Class;
                         Vector : in out Gfloat_Array)
   is
      procedure Internal (Curve  : System.Address;
                          Veclen : Integer;
                          Vector : System.Address);
      pragma Import (C, Internal, "gtk_curve_get_vector");
   begin
      Internal (Get_Object (Curve), Vector'Length,
                Vector'Address);
   end Get_Vector;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Curve) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_curve_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   -----------
   -- Reset --
   -----------

   procedure Reset (Curve : in Gtk_Curve'Class) is
      procedure Internal (Curve : System.Address);
      pragma Import (C, Internal, "gtk_curve_reset");
   begin
      Internal (Get_Object (Curve));
   end Reset;

   --------------------
   -- Set_Curve_Type --
   --------------------

   procedure Set_Curve_Type (Curve      : in Gtk_Curve'Class;
                             Curve_Type : in Gtk_Curve_Type)
   is
      procedure Internal (Curve      : System.Address;
                          Curve_Type : Gint);
      pragma Import (C, Internal, "gtk_curve_set_curve_type");
   begin
      Internal (Get_Object (Curve), Gtk_Curve_Type'Pos (Curve_Type));
   end Set_Curve_Type;

   ---------------
   -- Set_Gamma --
   ---------------

   procedure Set_Gamma (Curve : in Gtk_Curve'Class; Gamma : in Gfloat) is
      procedure Internal (Curve : System.Address; Gamma : Gfloat);
      pragma Import (C, Internal, "gtk_curve_set_gamma");
   begin
      Internal (Get_Object (Curve), Gamma);
   end Set_Gamma;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range
     (Curve : in Gtk_Curve'Class;
      Min_X : in Gfloat;
      Max_X : in Gfloat;
      Min_Y : in Gfloat;
      Max_Y : in Gfloat)
   is
      procedure Internal
        (Curve : in System.Address;
         Min_X : in Gfloat;
         Max_X : in Gfloat;
         Min_Y : in Gfloat;
         Max_Y : in Gfloat);
      pragma Import (C, Internal, "gtk_curve_set_range");
   begin
      Internal (Get_Object (Curve), Min_X, Max_X, Min_Y, Max_Y);
   end Set_Range;

   ----------------
   -- Set_Vector --
   ----------------

   procedure Set_Vector (Curve  : in Gtk_Curve'Class;
                         Vector : in Gfloat_Array)
   is
      procedure Internal (Curve  : System.Address;
                          Veclen : Integer;
                          Vector : System.Address);
      pragma Import (C, Internal, "gtk_curve_set_vector");
   begin
      Internal (Get_Object (Curve), Vector'Length,
                Vector'Address);
   end Set_Vector;


end Gtk.Curve;
