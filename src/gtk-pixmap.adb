with Gdk; use Gdk;
with Gdk.Bitmap;
with Gdk.Pixmap;

package body Gtk.Pixmap is

   ---------
   -- Get --
   ---------

   procedure Get
      (Pixmap : in Gtk_Pixmap'Class;
       Val    : in Gdk.Pixmap.Gdk_Pixmap'Class;
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

   --------------
   -- Get_Mask --
   --------------

   function Get_Mask (Widget : in Gtk_Pixmap'Class)
                      return      Gdk.Bitmap.Gdk_Bitmap'Class
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_pixmap_get_mask");
      Tmp : Gdk.Bitmap.Gdk_Bitmap;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Mask;

   ----------------
   -- Get_Pixmap --
   ----------------

   function Get_Pixmap (Widget : in Gtk_Pixmap'Class)
                        return      Gdk.Pixmap.Gdk_Pixmap'Class
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_pixmap_get_pixmap");
      Tmp : Gdk.Pixmap.Gdk_Pixmap;
   begin
      Set_Object (Tmp, Internal (Get_Object (Widget)));
      return Tmp;
   end Get_Pixmap;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget : out Gtk_Pixmap;
       Pixmap : in Gdk.Pixmap.Gdk_Pixmap'Class;
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
       Val    : in Gdk.Pixmap.Gdk_Pixmap'Class;
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

