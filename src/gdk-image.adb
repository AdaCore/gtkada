package body Gdk.Image is


   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (Image : in out Gdk_Image) is
      procedure Internal (Image : in System.Address);
      pragma Import (C, Internal, "gdk_image_destroy");
   begin
      Internal (Get_Object (Image));
   end Destroy;


   ---------------
   --  Gdk_New  --
   ---------------

   procedure Gdk_New (Image      :    out Gdk_Image;
                      Image_Type : in     Gdk_Image_Type;
                      Visual     : in     Gdk.Visual.Gdk_Visual;
                      Width      : in     Gint;
                      Height     : in     Gint) is
      function Internal (Image_Type : in Gdk_Image_Type;
                         Visual     : in System.Address;
                         Width, Height : in Gint) return System.Address;
      pragma Import (C, Internal, "gdk_image_new");
   begin
      Set_Object (Image, Internal (Image_Type, Get_Object (Visual),
                                   Width, Height));
   end Gdk_New;


   -----------
   --  Get  --
   -----------

   procedure Get (Image  :    out Gdk_Image;
                  Window : in     Gdk.Window.Gdk_Window;
                  X      : in     Gint;
                  Y      : in     Gint;
                  Width  : in     Gint;
                  Height : in     Gint) is
      function Internal (Window        : in System.Address;
                         X, Y          : in Gint;
                         Width, Height : in Gint) return System.Address;
      pragma Import (C, Internal, "gdk_image_get");
   begin
      Set_Object (Image, Internal (Get_Object (Window),
                                   X, Y, Width, Height));
   end Get;


   -----------------
   --  Get_Pixel  --
   -----------------

   function Get_Pixel (Image : in Gdk_Image;
                       X     : in Gint;
                       Y     : in Gint) return Guint32 is
      function Internal (Image : in System.Address;
                         X, Y  : in Gint) return Guint32;
      pragma Import (C, Internal, "gdk_image_get_pixel");
   begin
      return Internal (Get_Object (Image), X, Y);
   end Get_Pixel;


   -----------------
   --  Put_Pixel  --
   -----------------

   procedure Put_Pixel (Image : in out Gdk_Image;
                        X     : in     Gint;
                        Y     : in     Gint;
                        Pixel : in     Guint32) is
      procedure Internal (Image : in System.Address;
                          X, Y  : in Gint;
                          Pixel : in Guint32);
      pragma Import (C, Internal, "gdk_image_put_pixel");
   begin
      Internal (Get_Object (Image), X, Y, Pixel);
   end Put_Pixel;

end Gdk.Image;
