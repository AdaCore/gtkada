package body Gdk.Color is


   -----------
   --  "="  --
   -----------

   function "=" (Colora, Colorb : in Gdk_Color'Class) return Boolean is
      function Internal (Colora, Colorb : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_equal");
   begin
      return To_Boolean (Internal (Get_Object (Colora), Get_Object (Colorb)));
   end "=";


   -------------
   --  Alloc  --
   -------------

   procedure Alloc (Colormap   : in out Gdk_Colormap;
                    Contiguous : in     Boolean;
                    Planes     : in     Gulong_Array;
                    Pixels     : in     Gulong_Array;
                    Succeeded  :    out Boolean) is
      function Internal (Colormap   : in System.Address;
                         Contiguous : in Gint;
                         Planes     : in Gulong_Array;
                         Nplanes    : in Gint;
                         Pixels     : in Gulong_Array;
                         Npixels    : in Gint) return Gint;
      pragma Import (C, Internal, "gdk_colors_alloc");
   begin
      Succeeded := To_Boolean (Internal (Get_Object (Colormap),
                                         To_Gint (Contiguous),
                                         Planes, Planes'Length,
                                         Pixels, Pixels'Length));
   end Alloc;


   -------------
   --  Alloc  --
   -------------

   procedure Alloc (Colormap  : in out Gdk_Colormap;
                    Color     : in out Gdk_Color'Class;
                    Succeeded :    out Boolean) is
      function Internal (Colormap, Color : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_alloc");
   begin
      Succeeded := To_Boolean (Internal (Get_Object (Colormap),
                                         Get_Object (Color)));
   end Alloc;


   -------------
   --  Black  --
   -------------

   procedure Black (Colormap  : in out Gdk_Colormap;
                    Color     : in out Gdk_Color'Class;
                    Succeeded :    out Boolean) is
      function Internal (Colormap, Color : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_black");
   begin
      Succeeded := To_Boolean (Internal (Get_Object (Colormap),
                                         Get_Object (Color)));
   end Black;


   --------------
   --  Change  --
   --------------

   procedure Change (Colormap : in Gdk_Colormap;
                     Ncolors  : in Gint) is
      procedure Internal (Colormap : in System.Address;
                          Ncolors : in Gint);
      pragma Import (C, Internal, "gdk_colormap_change");
   begin
      Internal (Get_Object (Colormap), Ncolors);
   end Change;


   --------------
   --  Change  --
   --------------

   procedure Change (Colormap  : in out Gdk_Colormap;
                    Color     : in out Gdk_Color'Class;
                    Succeeded :    out Boolean) is
      function Internal (Colormap, Color : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_change");
   begin
      Succeeded := To_Boolean (Internal (Get_Object (Colormap),
                                         Get_Object (Color)));
   end Change;


   ------------
   --  Free  --
   ------------

   procedure Free (Colormap : in out Gdk_Colormap;
                   Pixels   : in     Gulong_Array;
                   Planes   : in     Gulong) is
      procedure Internal (Colormap : in System.Address;
                          Pixels   : in Gulong_Array;
                          NPixels  : in Gint;
                          Planes   : in Gulong);
      pragma Import (C, Internal, "gdk_colors_free");
   begin
      Internal (Get_Object (Colormap), Pixels, Pixels'Length, Planes);
   end Free;


   ---------------
   --  Gdk_New  --
   ---------------

   procedure Gdk_New (Colormap     :    out Gdk_Colormap;
                      Visual       : in     Gdk.Visual.Gdk_Visual;
                      Private_Cmap : in     Gint) is
      function Internal (Visual   : in System.Address;
                         Private_Cmap : in Gint) return System.Address;
      pragma Import (C, Internal, "gdk_colormap_new");
   begin
      Set_Object (Colormap, Internal (Get_Object (Visual), Private_Cmap));
   end Gdk_New;


   -------------
   --  Parse  --
   -------------

   procedure Parse (Spec      : in     String;
                    Color     : in out Gdk_Color'Class;
                    Succeeded :    out Boolean) is
      function Internal (Spec  : in String;
                         Color : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_parse");
   begin
      Succeeded := To_Boolean (Internal (Spec & ASCII.NUL,
                                         Get_Object (Color)));
   end Parse;


   -------------
   --  Store  --
   -------------

   procedure Store (Colormap : in out Gdk_Colormap;
                    Colors   : in     Gdk_Color'Class;
                    Ncolors  : in     Gint) is
      procedure Internal (Colormap : in System.Address;
                          Colors : in System.Address;
                          Ncolors : in Gint);
      pragma Import (C, Internal, "gdk_colors_store");
   begin
      Internal (Get_Object (Colormap), Get_Object (Colors), Ncolors);
   end Store;


   -------------
   --  White  --
   -------------

   procedure White (Colormap  : in out Gdk_Colormap;
                    Color     : in out Gdk_Color'Class;
                    Succeeded :    out Boolean) is
      function Internal (Colormap, Color : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_color_white");
   begin
      Succeeded := To_Boolean (Internal (Get_Object (Colormap),
                                         Get_Object (Color)));
   end White;

end Gdk.Color;
