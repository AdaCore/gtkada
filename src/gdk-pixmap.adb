package body Gdk.Pixmap is


   ------------------------
   --  Create_From_Data  --
   ------------------------

   procedure Create_From_Data (Pixmap :    out Gdk_Pixmap;
                               Window : in     Gdk.Window.Gdk_Window'Class;
                               Data   : in     String;
                               Width  : in     Gint;
                               Height : in     Gint;
                               Depth  : in     Gint;
                               Fg     : in     Color.Gdk_Color'Class;
                               Bg     : in     Color.Gdk_Color'Class) is
      function Internal (Window : in System.Address;
                         Data   : in String;
                         Width  : in Gint;
                         Height : in Gint;
                         Depth  : in Gint;
                         Fg     : in System.Address;
                         Bg     : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_pixmap_create_from_data");
   begin
      Set_Object (Pixmap, Internal (Get_Object (Window), Data & ASCII.NUL,
                                    Width, Height, Depth,
                                    Get_Object (Fg), Get_Object (Bg)));
   end Create_From_Data;

   ---------------------
   -- Create_From_Xpm --
   ---------------------

   procedure Create_From_Xpm (Pixmap      : out Gdk_Pixmap;
                              Window      : in  Gdk.Window.Gdk_Window'Class;
                              Mask        : out Gdk.Bitmap.Gdk_Bitmap;
                              Transparent : in  Gdk.Color.Gdk_Color'Class;
                              Filename    : in  String)
   is
      function Internal (Window      : in System.Address;
                         Mask        : in System.Address;
                         Transparent : in System.Address;
                         Filename    : in String)
                         return           System.Address;
      pragma Import (C, Internal, "gdk_pixmap_create_from_xpm");
      Tmp : System.Address;
   begin
      Set_Object (Pixmap, Internal (Get_Object (Window), Tmp'Address,
                                    Get_Object (Transparent),
                                    Filename & Ascii.NUL));
      Set_Object (Mask, Tmp);
   end Create_From_Xpm;

   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Pixmap :    out Gdk_Pixmap;
                      Window : in     Gdk.Window.Gdk_Window;
                      Width  : in     Gint;
                      Height : in     Gint;
                      Depth  : in     Gint) is
      function Internal (Window : in System.Address;
                         Width  : in Gint;
                         Height : in Gint;
                         Depth  : in Gint) return System.Address;
      pragma Import (C, Internal, "gdk_pixmap_new");
   begin
      Set_Object (Pixmap, Internal (Get_Object (Window),
                                    Width, Height, Depth));
   end Gtk_New;

end Gdk.Pixmap;
