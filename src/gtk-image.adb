

package body Gtk.Image is

   ---------
   -- Get --
   ---------

   procedure Get
      (Image : in Gtk_Image'Class;
       Val   : in Gdk_Image'Class;
       Mask  : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      procedure Internal
         (Image : in System.Address;
          Val   : in System.Address;
          Mask  : in System.Address);
      pragma Import (C, Internal, "gtk_image_get");
   begin
      Internal (Get_Object (Image),
                Get_Object (Val),
                Get_Object (Mask));
   end Get;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget : out Gtk_Image;
       Val    : in Gdk_Image'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      function Internal
         (Val    : in System.Address;
          Mask   : in System.Address)
          return      System.Address;
      pragma Import (C, Internal, "gtk_image_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Val),
                                    Get_Object (Mask)));
   end Gtk_New;

   ---------
   -- Set --
   ---------

   procedure Set
      (Image : in Gtk_Image'Class;
       Val   : in Gdk_Image'Class;
       Mask  : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      procedure Internal
         (Image : in System.Address;
          Val   : in System.Address;
          Mask  : in System.Address);
      pragma Import (C, Internal, "gtk_image_set");
   begin
      Internal (Get_Object (Image),
                Get_Object (Val),
                Get_Object (Mask));
   end Set;

end Gtk.Image;
