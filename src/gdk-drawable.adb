package body Gdk.Drawable is

   --------------------
   -- Draw_Rectangle --
   --------------------

   procedure Draw_Rectangle (Drawable : in Gdk_Drawable'Class;
                             Gc       : in Gdk.GC.Gdk_GC'Class;
                             Filled   : in Boolean := False;
                             X, Y     : in Gint;
                             Width    : in Gint;
                             Height   : in Gint) is
      procedure Internal (Drawable : in System.Address;
                          Gc       : in System.Address;
                          Filled   : in Gint;
                          X, Y     : in Gint;
                          Width    : in Gint;
                          Height   : in Gint);
      pragma Import (C, Internal, "gdk_draw_rectangle");
   begin
      Internal (Get_Object (Drawable),
                Get_Object (Gc),
                To_Gint (Filled),
                X, Y, Width, Height);
   end Draw_Rectangle;


   -----------------
   -- Draw_Pixmap --
   -----------------

   procedure Draw_Pixmap (Drawable : in Gdk.Drawable.Gdk_Drawable'Class;
                          Gc       : in Gdk.GC.Gdk_GC'Class;
                          Src      : in Gdk.Drawable.Gdk_Drawable'Class;
                          Xsrc     : in Gint;
                          Ysrc     : in Gint;
                          Xdest    : in Gint;
                          Ydest    : in Gint;
                          Width    : in Gint;
                          Height   : in Gint) is
      procedure Internal (Drawable : in System.Address;
                          Gc       : in System.Address;
                          Src      : in System.Address;
                          Xsrc     : in Gint;
                          Ysrc     : in Gint;
                          Xdest    : in Gint;
                          Ydest    : in Gint;
                          Width    : in Gint;
                          Height   : in Gint);
      pragma Import (C, Internal, "gdk_draw_pixmap");
   begin
      Internal (Get_Object (Drawable), Get_Object (Gc), Get_Object (Src),
                Xsrc, Ysrc, Xdest, Ydest, Width, Height);
   end Draw_Pixmap;

end Gdk.Drawable;
