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

   ----------------
   -- Draw_Point --
   ----------------

   procedure Draw_Point (Drawable : in Gdk_Drawable'Class;
                         Gc       : in Gdk.GC.Gdk_GC'Class;
                         X        : in Gint;
                         Y        : in Gint) is
      procedure Internal (Drawable : in System.Address;
                          Gc       : in System.Address;
                          X, Y     : in Gint);
      pragma Import (C, Internal, "gdk_draw_point");
   begin
      Internal (Get_Object (Drawable),
                Get_Object (Gc),
                X, Y);
   end Draw_Point;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line (Drawable : in Gdk_Drawable'Class;
                             Gc       : in Gdk.GC.Gdk_GC'Class;
                             X1, Y1   : in Gint;
                             X2, Y2   : in Gint) is
      procedure Internal (Drawable : in System.Address;
                          Gc       : in System.Address;
                          X1, Y1   : in Gint;
                          X2, Y2   : in Gint);
      pragma Import (C, Internal, "gdk_draw_line");
   begin
      Internal (Get_Object (Drawable),
                Get_Object (Gc),
                X1, Y1, X2, Y2);
   end Draw_Line;

   --------------
   -- Draw_Arc --
   --------------

   procedure Draw_Arc (Drawable : in Gdk_Drawable'Class;
                       Gc       : in Gdk.GC.Gdk_GC'Class;
                       Filled   : in Boolean := False;
                       X        : in Gint;
                       Y        : in Gint;
                       Width    : in Gint;
                       Height   : in Gint;
                       Angle1   : in Gint;
                       Angle2   : in Gint) is
      procedure Internal (Drawable : in System.Address;
                          Gc       : in System.Address;
                          Filled   : in Gint;
                          X, Y     : in Gint;
                          Width    : in Gint;
                          Height   : in Gint;
                          Angle1   : in Gint;
                          Angle2   : in Gint);
      pragma Import (C, Internal, "gdk_draw_arc");
   begin
      Internal (Get_Object (Drawable),
                Get_Object (Gc),
                To_Gint (Filled),
                X, Y, Width, Height, Angle1, Angle2);
   end Draw_Arc;

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
