package body Gtk.Misc is


   ---------------------
   --  Set_Alignment  --
   ---------------------

   procedure Set_Alignment (Misc   : in out Gtk_Misc'Class;
                            Xalign : in     Gfloat;
                            Yalign : in     Gfloat) is
      procedure Internal (Misc           : in System.Address;
                          Xalign, Yalign : in Gfloat);
      pragma Import (C, Internal, "gtk_misc_set_alignment");
   begin
      Internal (Get_Object (Misc), Xalign, Yalign);
   end Set_Alignment;


   -------------------
   --  Set_Padding  --
   -------------------

   procedure Set_Padding (Misc : in out Gtk_Misc'Class;
                          Xpad : in     Gint;
                          Ypad : in     Gint) is
      procedure Internal (Misc       : in System.Address;
                          Xpad, Ypad : in Gint);
      pragma Import (C, Internal, "gtk_misc_set_padding");
   begin
      Internal (Get_Object (Misc), Xpad, Ypad);
   end Set_Padding;

end Gtk.Misc;
