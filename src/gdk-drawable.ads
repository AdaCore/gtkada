with Glib; use Glib;
with Gdk.GC;
with Gdk.Window;

package Gdk.Drawable is

   subtype Gdk_Drawable is Window.Gdk_Window;

   procedure Draw_Rectangle (Drawable : in Gdk_Drawable'Class;
                             Gc       : in Gdk.GC.Gdk_GC'Class;
                             Filled   : in Boolean := False;
                             X        : in Gint;
                             Y        : in Gint;
                             Width    : in Gint;
                             Height   : in Gint);
   --  mapping: Draw_Rectangle gdk.h gdk_draw_rectangle

   procedure Draw_Point (Drawable : in Gdk_Drawable'Class;
                         Gc       : in Gdk.GC.Gdk_GC'Class;
                         X        : in Gint;
                         Y        : in Gint);
   --  mapping: Draw_Point gdk.h gdk_draw_point

   procedure Draw_Line (Drawable : in Gdk_Drawable'Class;
                        Gc       : in Gdk.GC.Gdk_GC'Class;
                        X1       : in Gint;
                        Y1       : in Gint;
                        X2       : in Gint;
                        Y2       : in Gint);
   --  mapping: Draw_Line gdk.h gdk_draw_line

   procedure Draw_Arc (Drawable : in Gdk_Drawable'Class;
                       Gc       : in Gdk.GC.Gdk_GC'Class;
                       Filled   : in Boolean := False;
                       X        : in Gint;
                       Y        : in Gint;
                       Width    : in Gint;
                       Height   : in Gint;
                       Angle1   : in Gint;
                       Angle2   : in Gint);
   --  mapping: Draw_Arc gdk.h gdk_draw_arc

   procedure Draw_Pixmap
      (Drawable : in Gdk_Drawable'Class;
       Gc       : in Gdk.GC.Gdk_GC'Class;
       Src      : in Gdk_Drawable'Class;
       Xsrc     : in Gint;
       Ysrc     : in Gint;
       Xdest    : in Gint;
       Ydest    : in Gint;
       Width    : in Gint;
       Height   : in Gint);
   --  mapping: Draw_Pixmap gdk.h gdk_draw_pixmap

end Gdk.Drawable;
