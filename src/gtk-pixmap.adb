

package body Gtk.Pixmap is

   ---------
   -- Get --
   ---------

   procedure Get
      (Pixmap : in Gtk_Pixmap'Class;
       Val    : in Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      procedure Internal
         (Pixmap : in System.Address;
          Val    : in System.Address;
          Mask   : in System.Address);
      pragma Import (C, Internal, "gtk_pixmap_get");
   begin
      Internal (Get_Object (Pixmap),
                Get_Object (Val),
                Get_Object (Mask));
   end Get;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget : out Gtk_Pixmap;
       Pixmap : in Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      function Internal
         (Pixmap : in System.Address;
          Mask   : in System.Address)
          return      System.Address;
      pragma Import (C, Internal, "gtk_pixmap_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Pixmap),
                                    Get_Object (Mask)));
   end Gtk_New;

   ---------
   -- Set --
   ---------

   procedure Set
      (Pixmap : in Gtk_Pixmap'Class;
       Val    : in Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      procedure Internal
         (Pixmap : in System.Address;
          Val    : in System.Address;
          Mask   : in System.Address);
      pragma Import (C, Internal, "gtk_pixmap_set");
   begin
      Internal (Get_Object (Pixmap),
                Get_Object (Val),
                Get_Object (Mask));
   end Set;

end Gtk.Pixmap;
