package body Gtk.Adjustment is


   ------------------
   --  Clamp_Page  --
   ------------------

   procedure Clamp_Page (Adjustment : in out Gtk_Adjustment;
                         Lower      : in     Gfloat;
                         Upper      : in     Gfloat) is

      procedure Internal (Adjustment : in System.Address;
                          Lower      : in Gfloat;
                          Upper      : in Gfloat);
      pragma Import (C, Internal, "gtk_adjustment_clamp_page");

   begin
      Internal (Get_Object (Adjustment), Lower, Upper);
   end Clamp_Page;


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Adjustment : out Gtk_Adjustment;
                      Value          : in Gfloat;
                      Lower          : in Gfloat;
                      Upper          : in Gfloat;
                      Step_Increment : in Gfloat;
                      Page_Increment : in Gfloat;
                      Page_Size      : in Gfloat) is

      function Internal (Value          : in Gfloat;
                         Lower          : in Gfloat;
                         Upper          : in Gfloat;
                         Step_Increment : in Gfloat;
                         Page_Increment : in Gfloat;
                         Page_Size      : in Gfloat)
                         return System.Address;
      pragma Import (C, Internal, "gtk_adjustment_new");

   begin
      Set_Object (Adjustment,
                  Internal (Value, Lower, Upper, Step_Increment,
                            Page_Increment, Page_Size));
   end Gtk_New;


   -----------------
   --  Set_Value  --
   -----------------

   procedure Set_Value (Adjustment : in out Gtk_Adjustment;
                        Value      : in     Gfloat) is

      procedure Internal (Adjustment : in System.Address;
                          Value      : in Gfloat);
      pragma Import (C, Internal, "gtk_adjustment_set_value");

   begin
      Internal (Get_Object (Adjustment), Value);
   end Set_Value;


end Gtk.Adjustment;
